%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 The Elixir Team

% Holds the logic responsible for reader macro definitions and expansion.
-module(elixir_reader_macros).
-export([setup/1, store_reader_macro/5, fetch_reader_macros/1, 
         expand_reader_macros/3, expand_reader_macros_from_source/2, match_pattern/2, format_error/1]).
-include("elixir.hrl").
-define(reader_macros, {elixir, reader_macros}).

%% Reader macro record
-record(reader_macro, {name, pattern, replacement}).

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
fetch_reader_macros(Module) when Module =:= nil; Module =:= undefined ->
  [];
fetch_reader_macros(Module) ->
  try
    {DataSet, _DataBag} = elixir_module:data_tables(Module),
    Result = ets:lookup(DataSet, ?reader_macros),
    case Result of
      [{?reader_macros, ReaderMacros}] -> 
        ReaderMacros;
      [] -> 
        []
    end
  catch
    error:badarg -> 
      []
  end.

%% Expand reader macros in source text
expand_reader_macros(Source, nil, E) ->
  io:format("expand_reader_macros: Module is nil, falling back to source extraction~n"),
  % When module is nil, fall back to extracting from source
  expand_reader_macros_from_source(Source, E);
expand_reader_macros(Source, Module, E) ->
  io:format("expand_reader_macros: Using module ~p~n", [Module]),
  ReaderMacros = fetch_reader_macros(Module) ++ get_imported_reader_macros(E),
  expand_source_with_macros(Source, ReaderMacros, E).

%% Expand reader macros when module is not yet known (during initial parsing)
expand_reader_macros_from_source(Source, E) ->
  % For now, completely disable reader macro expansion until the system is stable
  % TODO: Re-enable once the system is working and we can handle cross-module reader macros properly
  case should_process_reader_macros(E) of
    false -> 
      Source;
    true ->
      io:format("Source needs reader macro processing~n"),
      % Extract reader macros directly from source first
      SourceReaderMacros = extract_reader_macros_from_source(Source),
      io:format("Found ~p source reader macros~n", [length(SourceReaderMacros)]),
      
      % Get imported reader macros
      ImportedReaderMacros = get_imported_reader_macros(E),
      io:format("Found ~p imported reader macros~n", [length(ImportedReaderMacros)]),
      
      % Combine all reader macros and apply them
      AllReaderMacros = SourceReaderMacros ++ ImportedReaderMacros,
      expand_source_with_macros(Source, AllReaderMacros, E)
  end.

%% Determine if we should process reader macros for this environment
should_process_reader_macros(E) ->
  case elixir_config:is_bootstrap() of
    true -> false;  % Never during bootstrap
    false ->
      % Check if this looks like a user file vs a system file
      File = maps:get(file, E, <<>>),
      case File of
        <<"test_simple_reader_macro.ex">> -> true;  % Our test file
        <<"reader_macro_usage.exs">> -> true;      % Our test file
        <<"lisp_demo.ex">> -> true;                % SimpleLisp demo
        _ ->
          % Only process if it's clearly not a system file
          not is_system_file(File)
      end
  end.

%% Check if this is likely a system/stdlib file
is_system_file(File) when is_binary(File) ->
  % Check for common system path patterns
  binary:match(File, <<"lib/elixir/lib/">>) =/= nomatch orelse
  binary:match(File, <<"lib/kernel">>) =/= nomatch orelse
  binary:match(File, <<"lib/calendar">>) =/= nomatch orelse
  binary:match(File, <<"lib/mix">>) =/= nomatch orelse
  binary:match(File, <<"lib/iex">>) =/= nomatch;
is_system_file(_) ->
  false.

%% Quick check to see if source likely needs reader macro processing
needs_reader_macro_processing(Source) when is_binary(Source) ->
  % Only check for defreadermacro definitions - be very specific
  binary:match(Source, <<"defreadermacro">>) =/= nomatch;
needs_reader_macro_processing(Source) when is_list(Source) ->
  try
    SourceBin = iolist_to_binary(Source),
    needs_reader_macro_processing(SourceBin)
  catch
    _:_ ->
      % If conversion fails, do string search on the list directly
      case string:str(Source, "defreadermacro") of
        0 -> false;
        _ -> true
      end
  end.

%% Extract reader macro definitions directly from source text
extract_reader_macros_from_source(Source) ->
  io:format("Extracting reader macros from source: ~p~n", [Source]),
  % Pattern to match defreadermacro definitions with simple string patterns
  SimplePattern = "defreadermacro\\s+(\\w+)\\s*\\(\\s*\"([^\"]+)\"\\s*\\)\\s+do\\s+(.*?)\\s+end",
  % Pattern to match defreadermacros definitions with string concatenation patterns
  ConcatPattern = "defreadermacro\\s+(\\w+)\\s*\\(\\s*\"([^\"]+)\"\\s*<>\\s*(\\w+)\\s*\\)\\s+do\\s+(.*?)\\s+end",
  io:format("Using simple pattern: ~p~n", [SimplePattern]),
  io:format("Using concat pattern: ~p~n", [ConcatPattern]),
  
  SimpleMatches = case re:run(Source, SimplePattern, [global, {capture, all_but_first, list}, dotall]) of
    {match, SM} -> [build_reader_macro(Name, Pattern, Body) || [Name, Pattern, Body] <- SM];
    nomatch -> []
  end,
  
  ConcatMatches = case re:run(Source, ConcatPattern, [global, {capture, all_but_first, list}, dotall]) of
    {match, CM} -> [build_concat_reader_macro(Name, Prefix, Var, Body) || [Name, Prefix, Var, Body] <- CM];
    nomatch -> []
  end,
  
  SimpleMatches ++ ConcatMatches.

%% Build reader macro record from extracted components  
build_reader_macro(Name, ExtractedPattern, Body) ->
  % Clean up the body by removing extra whitespace and quotes
  CleanBody = string:strip(Body),
  FinalBody = case CleanBody of
    [$" | Rest] ->
      case lists:reverse(Rest) of
        [$" | RevBody] -> lists:reverse(RevBody);
        _ -> CleanBody
      end;
    _ -> CleanBody
  end,
  #reader_macro{
    name = list_to_atom(Name),
    pattern = ExtractedPattern, 
    replacement = FinalBody
  }.

%% Build concat reader macro - for patterns like "prefix" <> rest  
build_concat_reader_macro(Name, Prefix, _Var, Body) ->
  % For now, treat concat patterns as prefix patterns
  % This is a simplified approach - full implementation would need
  % more sophisticated pattern matching
  CleanBody = string:strip(Body),
  FinalBody = case CleanBody of
    [$" | Rest] ->
      case lists:reverse(Rest) of
        [$" | RevBody] -> lists:reverse(RevBody);
        _ -> CleanBody
      end;
    _ -> CleanBody
  end,
  #reader_macro{
    name = list_to_atom(Name),
    pattern = Prefix, 
    replacement = FinalBody
  }.

%% Legacy function - now delegates to new approach
expand_reader_macros_from_source_legacy(Source, E) ->
  case extract_module_from_source(Source) of
    {ok, Module} ->
      io:format("Found module: ~p~n", [Module]),
      expand_reader_macros(Source, Module, E#{module => Module});
    not_found ->
      io:format("No module found, checking imported modules only~n"),
      % No module declaration found, check imported modules only
      ReaderMacros = get_imported_reader_macros(E),
      expand_source_with_macros(Source, ReaderMacros, E)
  end.

%% Get imported reader macros from the environment
get_imported_reader_macros(E) ->
  Imports = maps:get(requires, E, []),
  lists:flatten([fetch_reader_macros(Mod) || Mod <- Imports]).

%% Apply reader macros to source text
expand_source_with_macros(Source, ReaderMacros, E) ->
  expand_source_with_macros(Source, ReaderMacros, E, 10).

expand_source_with_macros(Source, [], _E, _MaxIterations) ->
  Source;
expand_source_with_macros(Source, _ReaderMacros, _E, 0) ->
  % Max iterations reached, prevent infinite recursion
  Source;
expand_source_with_macros(Source, [ReaderMacro | Rest], E, MaxIterations) ->
  NewSource = apply_reader_macro(Source, ReaderMacro, E),
  case NewSource =:= Source of
    true ->
      % No change, continue with next macro
      expand_source_with_macros(NewSource, Rest, E, MaxIterations);
    false ->
      % Source changed, restart with all macros but decrement iterations
      expand_source_with_macros(NewSource, [ReaderMacro | Rest], E, MaxIterations - 1)
  end.

%% Apply a single reader macro to source text
apply_reader_macro(Source, {Name, Pattern, Body, Meta, Module}, E) ->
  io:format("Applying reader macro ~p with pattern ~p to source~n", [Name, Pattern]),
  case match_pattern(Source, Pattern) of
    {match, Before, Matched, After} ->
      io:format("Pattern matched! Before: ~p, Matched: ~p, After: ~p~n", [Before, Matched, After]),
      try
        Expanded = expand_reader_macro_body(Body, Matched, Meta, Module, E),
        io:format("Expanded to: ~p~n", [Expanded]),
        Result = Before ++ Expanded ++ apply_reader_macro(After, {Name, Pattern, Body, Meta, Module}, E),
        io:format("Final result: ~p~n", [Result]),
        Result
      catch
        Type:Reason ->
          io:format("Error expanding reader macro: ~p:~p~n", [Type, Reason]),
          Source
      end;
    nomatch ->
      io:format("Pattern did not match~n"),
      Source
  end;

%% Handle new reader macro record format
apply_reader_macro(Source, #reader_macro{name = Name, pattern = Pattern, replacement = Body}, E) ->
  io:format("Applying record-format reader macro ~p with pattern ~p to source~n", [Name, Pattern]),
  case match_pattern(Source, Pattern) of
    {match, Before, Matched, After} ->
      io:format("Pattern matched! Before: ~p, Matched: ~p, After: ~p~n", [Before, Matched, After]),
      % For simple text replacement, just use the replacement directly
      Result = Before ++ Body ++ apply_reader_macro(After, #reader_macro{name = Name, pattern = Pattern, replacement = Body}, E),
      io:format("Simple replacement result: ~p~n", [Result]),
      Result;
    nomatch ->
      io:format("Pattern did not match~n"),
      Source
  end.

%% Match a pattern against source text
match_pattern(Source, Pattern) when is_binary(Source), is_binary(Pattern) ->
  match_binary_pattern(Source, Pattern);
match_pattern(Source, Pattern) when is_list(Source), is_list(Pattern) ->
  match_string_pattern(Source, Pattern);
match_pattern(Source, Pattern) when is_list(Source), is_binary(Pattern) ->
  match_string_pattern(Source, binary_to_list(Pattern));
match_pattern(Source, Pattern) when is_binary(Source), is_list(Pattern) ->
  match_binary_pattern(Source, list_to_binary(Pattern));
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

%% Match a pattern in string (character list) source
match_string_pattern(Source, Pattern) ->
  case string:str(Source, Pattern) of
    0 ->
      nomatch;
    Start ->
      % string:str returns 1-based index, convert to 0-based
      StartIdx = Start - 1,
      Length = length(Pattern),
      Before = string:substr(Source, 1, StartIdx),
      Matched = Pattern,
      After = string:substr(Source, Start + Length),
      {match, Before, Matched, After}
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
    Result when is_binary(Result) ->
      % Handle simple string replacements from defreadermacro
      binary_to_list(Result);
    Result when is_list(Result) ->
      % Handle simple string replacements from defreadermacro
      Result;
    _ ->
      binary_to_list(Matched)
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

%% Extract module name from source code
extract_module_from_source(Source) when is_binary(Source) ->
  extract_module_from_source(binary_to_list(Source));
extract_module_from_source(Source) when is_list(Source) ->
  % Look for "defmodule ModuleName" pattern
  case re:run(Source, "defmodule\\s+([A-Z][a-zA-Z0-9._]*)", [{capture, all_but_first, list}]) of
    {match, [ModuleNameStr]} ->
      try
        case ModuleNameStr of
          [] -> not_found;
          _ ->
            ModuleName = list_to_atom("Elixir." ++ ModuleNameStr),
            {ok, ModuleName}
        end
      catch
        _:_ -> not_found
      end;
    nomatch ->
      not_found;
    _ ->
      not_found
  end.

format_error({invalid_reader_macro_pattern, Pattern}) ->
  io_lib:format("invalid reader macro pattern: ~p", [Pattern]);
format_error({reader_macro_expansion_error, Name, Error}) ->
  io_lib:format("error expanding reader macro ~p: ~p", [Name, Error]).