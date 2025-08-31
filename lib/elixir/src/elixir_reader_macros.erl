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
  % When module is nil, fall back to extracting from source
  expand_reader_macros_from_source(Source, E);
expand_reader_macros(Source, Module, E) ->
  ReaderMacros = fetch_reader_macros(Module) ++ get_imported_reader_macros(E),
  expand_source_with_macros(Source, ReaderMacros, E).

%% Expand reader macros when module is not yet known (during initial parsing)
expand_reader_macros_from_source(Source, E) ->
  case should_process_reader_macros(E) of
    false -> 
      Source;
    true ->
      % Quick pre-check: does source contain "defreadermacro"?
      case contains_reader_macro_keyword(Source) of
        false ->
          % No reader macros, skip expensive processing
          Source;
        true ->
          % Extract reader macros directly from source first
          SourceReaderMacros = extract_reader_macros_from_source(Source),
          
          % Get imported reader macros
          ImportedReaderMacros = get_imported_reader_macros(E),
          
          % Only do expensive expansion if we actually have macros
          case SourceReaderMacros ++ ImportedReaderMacros of
            [] -> 
              Source;
            AllReaderMacros ->
              expand_source_with_macros(Source, AllReaderMacros, E)
          end
      end
  end.

%% Quick check for reader macro keyword to avoid expensive processing
contains_reader_macro_keyword(Source) when is_binary(Source) ->
  binary:match(Source, <<"defreadermacro">>) =/= nomatch;
contains_reader_macro_keyword(Source) when is_list(Source) ->
  string:str(Source, "defreadermacro") =/= 0;
contains_reader_macro_keyword(_) ->
  false.

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
  % Check for all Elixir standard library and system paths
  binary:match(File, <<"lib/elixir/">>) =/= nomatch orelse
  binary:match(File, <<"lib/mix/">>) =/= nomatch orelse
  binary:match(File, <<"lib/eex/">>) =/= nomatch orelse
  binary:match(File, <<"lib/iex/">>) =/= nomatch orelse
  binary:match(File, <<"lib/logger/">>) =/= nomatch orelse
  binary:match(File, <<"lib/ex_unit/">>) =/= nomatch orelse
  % Also exclude any lib/ path to be safe
  binary:match(File, <<"lib/">>) =/= nomatch;
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
  % Safety check: don't process very large files to avoid regex performance issues
  SourceSize = case Source of
    Bin when is_binary(Bin) -> byte_size(Bin);
    List when is_list(List) -> length(List);
    _ -> 0
  end,
  
  % Skip files larger than 50KB to avoid regex performance issues
  case SourceSize > 50000 of
    true ->
      case elixir_config:get(debug, false) of
        true -> io:format("Skipping reader macro extraction: file too large (~p bytes)~n", [SourceSize]);
        false -> ok
      end,
      [];
    false ->
      extract_reader_macros_safe(Source)
  end.

%% Safe reader macro extraction with error handling
extract_reader_macros_safe(Source) ->
  % Pattern to match defreadermacro definitions with simple string patterns
  SimplePattern = "defreadermacro\\s+(\\w+)\\s*\\(\\s*\"([^\"]+)\"\\s*\\)\\s+do\\s+(.*?)\\s+end",
  % Pattern to match defreadermacros definitions with string concatenation patterns  
  ConcatPattern = "defreadermacro\\s+(\\w+)\\s*\\(\\s*\"([^\"]+)\"\\s*<>\\s*(\\w+)\\s*\\)\\s+do\\s+(.*?)\\s+end",
  
  SimpleMatches = safe_regex_run(Source, SimplePattern, fun([Name, Pattern, Body]) ->
    build_reader_macro(Name, Pattern, Body)
  end),
  
  ConcatMatches = safe_regex_run(Source, ConcatPattern, fun([Name, Prefix, Var, Body]) ->
    build_concat_reader_macro(Name, Prefix, Var, Body)
  end),
  
  SimpleMatches ++ ConcatMatches.

%% Safe regex execution with error handling
safe_regex_run(Source, Pattern, BuildFun) ->
  try
    case re:run(Source, Pattern, [global, {capture, all_but_first, list}, dotall]) of
      {match, Matches} -> [BuildFun(Match) || Match <- Matches];
      nomatch -> []
    end
  catch
    error:Reason ->
      % Only log errors in debug mode to avoid cluttering compilation
      case elixir_config:get(debug, false) of
        true -> io:format("Regex error in reader macro extraction: ~p~n", [Reason]);
        false -> ok
      end,
      [];
    _:_ ->
      []
  end.

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
  case match_pattern(Source, Pattern) of
    {match, Before, Matched, After} ->
      try
        Expanded = expand_reader_macro_body(Body, Matched, Meta, Module, E),
        Result = Before ++ Expanded ++ apply_reader_macro(After, {Name, Pattern, Body, Meta, Module}, E),
        Result
      catch
        Type:Reason ->
          case elixir_config:get(debug, false) of
            true -> io:format("Error expanding reader macro ~p: ~p:~p~n", [Name, Type, Reason]);
            false -> ok
          end,
          Source
      end;
    nomatch ->
      Source
  end;

%% Handle new reader macro record format
apply_reader_macro(Source, #reader_macro{name = Name, pattern = Pattern, replacement = Body}, E) ->
  case match_pattern(Source, Pattern) of
    {match, Before, Matched, After} ->
      % For simple text replacement, just use the replacement directly
      Result = Before ++ Body ++ apply_reader_macro(After, #reader_macro{name = Name, pattern = Pattern, replacement = Body}, E),
      Result;
    nomatch ->
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