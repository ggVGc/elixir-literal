%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 The Elixir Team

% Holds the logic responsible for reader macro definitions and expansion.
-module(elixir_reader_macros).
-export([setup/1, store_reader_macro/5, fetch_reader_macros/1, 
         expand_reader_macros/3, match_pattern/2, format_error/1]).
-include("elixir.hrl").
-define(reader_macros, {elixir, reader_macros}).

setup(DataTables) ->
  reset_reader_macros(DataTables),
  ok.

reset_reader_macros({DataSet, _DataBag}) ->
  ets:insert(DataSet, {?reader_macros, []});

reset_reader_macros(Module) when is_atom(Module) ->
  reset_reader_macros(elixir_module:data_tables(Module)).

%% Store a reader macro definition
store_reader_macro(Meta, Name, Pattern, Body, E) ->
  Module = maps:get(module, E),
  io:format("Storing reader macro ~p in module ~p with pattern ~p~n", [Name, Module, Pattern]),
  {DataSet, _DataBag} = elixir_module:data_tables(Module),
  
  ReaderMacro = {Name, Pattern, Body, Meta, Module},
  
  case ets:lookup(DataSet, ?reader_macros) of
    [{?reader_macros, Existing}] ->
      io:format("Found existing reader macros: ~p~n", [Existing]),
      Updated = [ReaderMacro | lists:keydelete(Name, 1, Existing)],
      ets:insert(DataSet, {?reader_macros, Updated});
    [] ->
      io:format("No existing reader macros, creating new entry~n"),
      ets:insert(DataSet, {?reader_macros, [ReaderMacro]})
  end,
  
  ok.

%% Fetch all reader macros for a module
fetch_reader_macros(Module) ->
  io:format("Fetching reader macros for module: ~p~n", [Module]),
  try
    {DataSet, _DataBag} = elixir_module:data_tables(Module),
    io:format("DataSet: ~p~n", [DataSet]),
    Result = ets:lookup(DataSet, ?reader_macros),
    io:format("ETS lookup result: ~p~n", [Result]),
    case Result of
      [{?reader_macros, ReaderMacros}] -> 
        io:format("Found reader macros: ~p~n", [ReaderMacros]),
        ReaderMacros;
      [] -> 
        io:format("No reader macros found~n"),
        []
    end
  catch
    error:badarg -> 
      io:format("Error: badarg in fetch_reader_macros~n"),
      []
  end.

%% Expand reader macros in source text
expand_reader_macros(Source, Module, E) ->
  ReaderMacros = fetch_reader_macros(Module) ++ get_imported_reader_macros(E),
  expand_source_with_macros(Source, ReaderMacros, E).

%% Get imported reader macros from the environment
get_imported_reader_macros(E) ->
  Imports = maps:get(requires, E, []),
  lists:flatten([fetch_reader_macros(Mod) || Mod <- Imports]).

%% Apply reader macros to source text
expand_source_with_macros(Source, [], _E) ->
  Source;
expand_source_with_macros(Source, [ReaderMacro | Rest], E) ->
  NewSource = apply_reader_macro(Source, ReaderMacro, E),
  expand_source_with_macros(NewSource, Rest, E).

%% Apply a single reader macro to source text
apply_reader_macro(Source, {Name, Pattern, Body, Meta, Module}, E) ->
  case match_pattern(Source, Pattern) of
    {match, Before, Matched, After} ->
      try
        Expanded = expand_reader_macro_body(Body, Matched, Meta, Module, E),
        Before ++ Expanded ++ apply_reader_macro(After, {Name, Pattern, Body, Meta, Module}, E)
      catch
        _:_ ->
          Source
      end;
    nomatch ->
      Source
  end.

%% Match a pattern against source text
match_pattern(Source, Pattern) when is_binary(Pattern) ->
  match_binary_pattern(Source, Pattern);
match_pattern(Source, {regex, RegexStr}) ->
  match_regex_pattern(Source, RegexStr);
match_pattern(_Source, _Pattern) ->
  nomatch.

match_binary_pattern(Source, Pattern) ->
  case binary:match(Source, Pattern) of
    {Start, Length} ->
      Before = binary:part(Source, 0, Start),
      Matched = binary:part(Source, Start, Length),
      After = binary:part(Source, Start + Length, byte_size(Source) - Start - Length),
      {match, Before, Matched, After};
    nomatch ->
      nomatch
  end.

match_regex_pattern(Source, RegexStr) ->
  case re:run(Source, RegexStr, [{capture, first, binary}]) of
    {match, [Matched]} ->
      case binary:match(Source, Matched) of
        {Start, Length} ->
          Before = binary:part(Source, 0, Start),
          After = binary:part(Source, Start + Length, byte_size(Source) - Start - Length),
          {match, Before, Matched, After};
        nomatch ->
          nomatch
      end;
    nomatch ->
      nomatch
  end.

%% Expand the body of a reader macro
expand_reader_macro_body(Body, Matched, Meta, Module, E) ->
  case Body of
    {string, Result} ->
      Result;
    {function, Fun, Arity} when Arity =:= 1 ->
      apply(Module, Fun, [Matched]);
    {ast, AST} ->
      evaluate_ast_with_match(AST, Matched, Meta, E);
    _ ->
      Matched
  end.

%% Evaluate AST with matched text as context
evaluate_ast_with_match(AST, Matched, Meta, E) ->
  case AST of
    {'__block__', _, [Expr]} ->
      evaluate_expression(Expr, Matched, Meta, E);
    Expr ->
      evaluate_expression(Expr, Matched, Meta, E)
  end.

evaluate_expression({'binary_to_string', _, [Var]}, Matched, _Meta, _E) when Var =:= 'matched' ->
  binary_to_list(Matched);
evaluate_expression({'string_to_binary', _, [Var]}, Matched, _Meta, _E) when Var =:= 'matched' ->
  list_to_binary(binary_to_list(Matched));
evaluate_expression(Expr, Matched, _Meta, _E) ->
  case Expr of
    Var when Var =:= 'matched' ->
      binary_to_list(Matched);
    _ ->
      binary_to_list(Matched)
  end.

format_error({invalid_reader_macro_pattern, Pattern}) ->
  io_lib:format("invalid reader macro pattern: ~p", [Pattern]);
format_error({reader_macro_expansion_error, Name, Error}) ->
  io_lib:format("error expanding reader macro ~p: ~p", [Name, Error]).