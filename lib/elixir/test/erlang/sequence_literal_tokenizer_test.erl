%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 The Elixir Team

-module(sequence_literal_test).
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  tokenize(String, []).

tokenize(String, Opts) ->
  {ok, _Line, _Column, _Warnings, Result, []} = elixir_tokenizer:tokenize(String, 1, Opts),
  lists:reverse(Result).

tokenize_error(String) ->
  {error, Error, _, _, _} = elixir_tokenizer:tokenize(String, 1, []),
  Error.

%% Test basic sequence operator tokenization
sequence_op_test() ->
  [{sequence_op, {1, 1, false}, '~~'}] = tokenize("~~"),
  [{identifier, {1, 1, _}, a},
   {sequence_op, {1, 3, nil}, '~~'},
   {identifier, {1, 6, _}, b}] = tokenize("a ~~ b"),
  ok.

%% Test sequence literal with parentheses
sequence_literal_parens_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {')', {1, 4, false}}] = tokenize("~~()"),
  
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {identifier, {1, 4, _}, hello},
   {')', {1, 9, false}}] = tokenize("~~(hello)"),
  ok.

%% Test nested brackets in sequence literals
sequence_nested_brackets_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {'(', {1, 4, nil}},
   {identifier, {1, 5, _}, a},
   {')', {1, 6, false}},
   {')', {1, 7, false}}] = tokenize("~~((a))"),
  
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {'[', {1, 4, nil}},
   {'{', {1, 5, nil}},
   {identifier, {1, 6, _}, a},
   {'}', {1, 7, false}},
   {']', {1, 8, false}},
   {')', {1, 9, false}}] = tokenize("~~([{a}])"),
  ok.

%% Test identifiers with dots in sequence literals
sequence_dotted_identifiers_test() ->
  % Test if IO.puts becomes a single identifier or separate tokens
  Tokens = tokenize("~~(IO.puts)"),
  
  % Check if we get a single identifier with dots or separate tokens
  case Tokens of
    [{sequence_op, {1, 1, false}, '~~'},
     {'(', {1, 3, nil}},
     {identifier, {1, 4, _}, 'IO.puts'},
     {')', {1, 11, false}}] ->
      % Dots are included in identifier
      ok;
    [{sequence_op, {1, 1, false}, '~~'},
     {'(', {1, 3, nil}},
     {alias, {1, 4, _}, 'IO'},
     {'.', {1, 6, nil}},
     {identifier, {1, 7, _}, puts},
     {')', {1, 11, false}}] ->
      % Parsed as separate tokens (current behavior)
      ok;
    _ ->
      erlang:error({unexpected_tokens, Tokens})
  end.

%% Test operators in sequence literals
sequence_operators_test() ->
  % Test arithmetic operators
  ArithTokens = tokenize("~~(a + b)"),
  HasArithOp = lists:any(
    fun({dual_op, _, '+'}) -> true;
       ({sequence_atom, _, '+'}) -> true;
       (_) -> false
    end, ArithTokens),
  ?assert(HasArithOp),
  
  % Test comparison operators
  CompTokens = tokenize("~~(a == b)"),
  HasCompOp = lists:any(
    fun({comp_op, _, '=='}) -> true;
       ({sequence_atom, _, '=='}) -> true;
       (_) -> false
    end, CompTokens),
  ?assert(HasCompOp),
  ok.

%% Test literals in sequence literals
sequence_literals_test() ->
  % Test string literals
  StringTokens = tokenize("~~(\"hello\")"),
  HasString = lists:any(
    fun({bin_string, _, _}) -> true;
       ({sequence_atom, _, "hello"}) -> true;
       (_) -> false
    end, StringTokens),
  ?assert(HasString),
  
  % Test number literals
  NumTokens = tokenize("~~(42)"),
  HasNum = lists:any(
    fun({int, _, _}) -> true;
       ({sequence_atom, _, 42}) -> true;
       (_) -> false
    end, NumTokens),
  ?assert(HasNum),
  
  % Test atom literals
  AtomTokens = tokenize("~~(:foo)"),
  HasAtom = lists:any(
    fun({atom, _, foo}) -> true;
       ({sequence_atom, _, foo}) -> true;
       (_) -> false
    end, AtomTokens),
  ?assert(HasAtom),
  
  % Test float literals
  FloatTokens = tokenize("~~(3.14)"),
  HasFloat = lists:any(
    fun({flt, _, _}) -> true;
       ({sequence_atom, _, 3.14}) -> true;
       (_) -> false
    end, FloatTokens),
  ?assert(HasFloat),
  ok.

%% Test keywords in sequence literals
sequence_keywords_test() ->
  % Test if/do/end keywords
  KeywordTokens = tokenize("~~(if true do end)"),
  
  % Check that we can tokenize keywords within sequence literals
  HasTokens = length(KeywordTokens) > 0,
  ?assert(HasTokens),
  
  % Check for specific keyword tokens
  HasKeywords = lists:any(
    fun({'if', _}) -> true;
       ({'do', _}) -> true;
       ({'end', _}) -> true;
       ({identifier, _, 'if'}) -> true;
       ({identifier, _, 'do'}) -> true;
       ({identifier, _, 'end'}) -> true;
       ({sequence_atom, _, 'if'}) -> true;
       ({sequence_atom, _, 'do'}) -> true;
       ({sequence_atom, _, 'end'}) -> true;
       (_) -> false
    end, KeywordTokens),
  ?assert(HasKeywords),
  ok.

%% Test multiple sequence literals
sequence_multiple_test() ->
  Tokens = tokenize("~~(a) ++ ~~(b)"),
  
  % Count sequence operators
  SeqOpCount = length(lists:filter(
    fun({sequence_op, _, _}) -> true;
       (_) -> false
    end, Tokens)),
  ?assertEqual(2, SeqOpCount),
  
  % Check for ++ operator
  HasConcat = lists:any(
    fun({concat_op, _, '++'}) -> true;
       (_) -> false
    end, Tokens),
  ?assert(HasConcat),
  ok.

%% Test empty sequence literal
sequence_empty_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {')', {1, 4, false}}] = tokenize("~~()"),
  ok.

%% Test sequence operator without parentheses
sequence_no_parens_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {identifier, {1, 4, _}, a}] = tokenize("~~ a"),
  ok.

%% Test nested sequence literals
sequence_nested_test() ->
  Tokens = tokenize("~~(a ~~(b) c)"),
  
  % Count sequence operators
  SeqOpCount = length(lists:filter(
    fun({sequence_op, _, _}) -> true;
       (_) -> false
    end, Tokens)),
  ?assertEqual(2, SeqOpCount),
  
  % Count parentheses
  OpenParens = length(lists:filter(
    fun({'(', _}) -> true;
       (_) -> false
    end, Tokens)),
  CloseParens = length(lists:filter(
    fun({')', _}) -> true;
       (_) -> false
    end, Tokens)),
  ?assertEqual(2, OpenParens),
  ?assertEqual(2, CloseParens),
  ok.

%% Test error cases
sequence_error_test() ->
  % Test incomplete sequence literal
  case catch elixir_tokenizer:tokenize("~~(", 1, []) of
    {error, _, _, _, _} -> ok;
    {ok, _, _, _, _, Terminators} when Terminators =/= [] -> ok;
    _ -> erlang:error(should_error_on_incomplete)
  end,
  ok.