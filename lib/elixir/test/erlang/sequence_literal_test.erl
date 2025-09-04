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

%% Test sequence literal with parentheses - ISOLATED TOKENIZATION
sequence_literal_parens_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {')', {1, 4, false}}] = tokenize("~~()"),

  % Content within sequence literal should use sequence_* tokens
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, hello},
   {')', {1, 9, false}}] = tokenize("~~(hello)"),
  ok.

%% Test nested brackets in sequence literals
sequence_nested_brackets_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {'(', {1, 4, nil}},
   {sequence_identifier, {1, 5, _}, a},
   {')', {1, 6, false}},
   {')', {1, 7, false}}] = tokenize("~~((a))"),

  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {'[', {1, 4, nil}},
   {'{', {1, 5, nil}},
   {sequence_identifier, {1, 6, _}, a},
   {'}', {1, 7, false}},
   {']', {1, 8, false}},
   {')', {1, 9, false}}] = tokenize("~~([{a}])"),
  ok.

%% Test identifiers with dots in sequence literals - MUST BE SINGLE TOKENS
sequence_dotted_identifiers_test() ->
  % IO.puts MUST become a single identifier token
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, 'IO.puts'},
   {')', {1, 11, false}}] = tokenize("~~(IO.puts)"),

  % Complex dotted paths should also be single tokens
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, 'Enum.map.filter'},
   {')', {1, 19, false}}] = tokenize("~~(Enum.map.filter)"),
  ok.

%% Test operators in sequence literals - MUST BE sequence_operator
sequence_operators_test() ->
  % Test arithmetic operators
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, a},
   {sequence_operator, {1, 6, _}, '+'},
   {sequence_identifier, {1, 8, _}, b},
   {')', {1, 9, false}}] = tokenize("~~(a + b)"),

  % Test comparison operators
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, a},
   {sequence_operator, {1, 6, _}, '=='},
   {sequence_identifier, {1, 9, _}, b},
   {')', {1, 10, false}}] = tokenize("~~(a == b)"),

  % Test complex operators
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, a},
   {sequence_operator, {1, 6, _}, '++'},
   {sequence_identifier, {1, 9, _}, b},
   {')', {1, 10, false}}] = tokenize("~~(a ++ b)"),
  ok.

%% Test literals in sequence literals - MUST BE sequence_* tokens
sequence_literals_test() ->
  % Test string literals - MUST be sequence_string
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_string, {1, 4, _}, "hello"},
   {')', {1, 11, false}}] = tokenize("~~(\"hello\")"),

  % Test single quoted strings - ALSO sequence_string
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_string, {1, 4, _}, "world"},
   {')', {1, 11, false}}] = tokenize("~~('world')"),

  % Test number literals - MUST be sequence_number
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_number, {1, 4, _}, 42},
   {')', {1, 6, false}}] = tokenize("~~(42)"),

  % Test atom literals - MUST be sequence_atom
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_atom, {1, 4, _}, foo},
   {')', {1, 8, false}}] = tokenize("~~(:foo)"),

  % Test float literals - MUST be sequence_number
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_number, {1, 4, _}, 3.14},
   {')', {1, 8, false}}] = tokenize("~~(3.14)"),
  ok.

%% Test keywords in sequence literals - MUST BE sequence_keyword
sequence_keywords_test() ->
  % Test if/do/end keywords - MUST be sequence_keyword tokens
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_keyword, {1, 4, _}, 'if'},
   {sequence_keyword, {1, 7, _}, 'true'},
   {sequence_keyword, {1, 12, _}, 'do'},
   {sequence_keyword, {1, 15, _}, 'end'},
   {')', {1, 18, false}}] = tokenize("~~(if true do end)"),

  % Test boolean keywords
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_keyword, {1, 4, _}, 'true'},
   {')', {1, 8, false}}] = tokenize("~~(true)"),

  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_keyword, {1, 4, _}, 'false'},
   {')', {1, 9, false}}] = tokenize("~~(false)"),

  % Test nil keyword
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_keyword, {1, 4, _}, 'nil'},
   {')', {1, 7, false}}] = tokenize("~~(nil)"),
  ok.

%% Test multiple sequence literals - normal tokens between sequences
sequence_multiple_test() ->
  % Normal ++ operator between sequence literals
  Tokens = tokenize("~~(a) ++ ~~(b)"),

  % Expected: seq_op, (, seq_id, ), concat_op, seq_op, (, seq_id, )
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, a},
   {')', {1, 5, false}},
   {concat_op, {1, 7, _}, '++'},  % Normal token outside sequence
   {sequence_op, {1, 10, nil}, '~~'},
   {'(', {1, 12, nil}},
   {sequence_identifier, {1, 13, _}, b},
   {')', {1, 14, false}}] = Tokens,
  ok.

%% Test empty sequence literal
sequence_empty_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {')', {1, 4, false}}] = tokenize("~~()"),
  ok.

%% Test sequence operator without parentheses - normal tokenization
sequence_no_parens_test() ->
  [{sequence_op, {1, 1, false}, '~~'},
   {identifier, {1, 4, _}, a}] = tokenize("~~ a"),  % Normal identifier
  ok.

%% Test nested sequence literals
sequence_nested_test() ->
  % ~~(a ~~(b) c) - inner sequence should be fully isolated
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_identifier, {1, 4, _}, a},
   {sequence_op, {1, 6, nil}, '~~'},
   {'(', {1, 8, nil}},
   {sequence_identifier, {1, 9, _}, b},
   {')', {1, 10, false}},
   {sequence_identifier, {1, 12, _}, c},
   {')', {1, 13, false}}] = tokenize("~~(a ~~(b) c)"),
  ok.

%% Test mixed content in sequence literals - ALL must be sequence_* tokens
sequence_mixed_content_test() ->
  % ~~("hello", 42, :atom, IO.puts)
  [{sequence_op, {1, 1, false}, '~~'},
   {'(', {1, 3, nil}},
   {sequence_string, {1, 4, _}, "hello"},
   {',', {1, 11, _}},
   {sequence_number, {1, 13, _}, 42},
   {',', {1, 15, _}},
   {sequence_atom, {1, 17, _}, atom},
   {',', {1, 22, _}},
   {sequence_identifier, {1, 24, _}, 'IO.puts'},
   {')', {1, 31, false}}] = tokenize("~~(\"hello\", 42, :atom, IO.puts)"),
  ok.

%% Test isolation - NO normal tokens inside sequence literals
sequence_isolation_test() ->
  % Complex expression with all token types
  Tokens = tokenize("~~(\"str\", 123, 3.14, :atom, IO.puts, +, ==, if, true)"),

  % Verify NO normal Elixir tokens appear inside sequence literal
  InsideTokens = lists:filter(
    fun({sequence_op, _, _}) -> false;  % Skip sequence_op markers
       ({'(', _}) -> false;             % Skip parens
       ({')', _}) -> false;
       ({',', _}) -> false;             % Skip commas
       (_) -> true                      % Include all other tokens
    end, Tokens),

  % ALL tokens inside should be sequence_* types
  AllSequenceTokens = lists:all(
    fun({sequence_string, _, _}) -> true;
       ({sequence_number, _, _}) -> true;
       ({sequence_atom, _, _}) -> true;
       ({sequence_identifier, _, _}) -> true;
       ({sequence_operator, _, _}) -> true;
       ({sequence_keyword, _, _}) -> true;
       (_) -> false
    end, InsideTokens),

  ?assert(AllSequenceTokens),
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
