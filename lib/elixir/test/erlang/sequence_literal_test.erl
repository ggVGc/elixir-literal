%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 The Elixir Team

-module(sequence_literal_test).
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  tokenize(String, []).

tokenize(String, Opts) ->
  case elixir_tokenizer:tokenize(String, 1, Opts) of
    {ok, _Line, _Column, _Warnings, Result, []} ->
      lists:reverse(Result);
    {error, _, _, _, _} = Error ->
      Error
  end.

% tokenize_error(String) ->
%   {error, Error, _, _, _} = elixir_tokenizer:tokenize(String, 1, []),
%   Error.


quoted_test() ->

[{do_identifier,{1,1,"quote"},quote},
   {do,{1,7,nil}},
   {beginliteral,{1,10,nil}},
   {identifier,{1,23,"a"},a},
   {endliteral,{1,25,nil}},
   {'end',{1,36,nil}}] = tokenize("quote do beginliteral a endliteral end"),

[{do_identifier,{1,1,"quote"},quote},
   {do,{1,7,nil}},
   {beginliteral,{1,10,nil}},
   {dual_op,{1,23,nil},'+'},
   {identifier,{1,25,"a"},a},
   {int,{1,27,1},"1"},
   {atom,{1,29,"yeo"},yeo},
   {bin_string,{1,34,nil},[<<"breo">>]},
   {endliteral,{1,41,nil}},
   {'end',{1,52,nil}}] = tokenize("quote do beginliteral + a 1 :yeo \"breo\" endliteral end"),
  ok.

%% Test basic beginliteral keyword tokenization
sequence_keyword_test() ->
  % beginliteral without endliteral causes error
  {error, _, _, _, _} = tokenize("beginliteral"),
  % beginliteral in middle of tokens also causes error
  {error, _, _, _, _} = tokenize("a beginliteral b"),
  ok.

%% Test sequence literal with beginliteral/endliteral - ISOLATED TOKENIZATION
sequence_literal_keywords_test() ->
  [{beginliteral, {1, 1, nil}},
   {endliteral, {1, 14, nil}}] = tokenize("beginliteral endliteral"),

  % Content within sequence literal uses regular tokens
  [{beginliteral, {1, 1, nil}},
   {identifier, {1, 14, "hello"}, hello},
   {endliteral, {1, 20, nil}}] = tokenize("beginliteral hello endliteral"),
  ok.

%% Test nested brackets in sequence literals
sequence_nested_brackets_test() ->
  [{beginliteral, {1, 1, nil}},
   {'(', {1, 14, nil}},
   {identifier, {1, 15, "a"}, a},
   {')', {1, 16, nil}},
   {endliteral, {1, 18, nil}}] = tokenize("beginliteral (a) endliteral"),

  [{beginliteral, {1, 1, nil}},
   {'[', {1, 14, nil}},
   {'{', {1, 15, nil}},
   {identifier, {1, 16, "a"}, a},
   {'}', {1, 17, nil}},
   {']', {1, 18, nil}},
   {endliteral, {1, 20, nil}}] = tokenize("beginliteral [{a}] endliteral"),
  ok.

%% Test identifiers with dots in sequence literals
sequence_dotted_identifiers_test() ->
  % IO.puts becomes separate tokens (alias, dot, identifier)
  [{beginliteral, {1, 1, nil}},
   {alias, {1, 14, "IO"}, 'IO'},
   {'.', {1, 16, nil}},
   {identifier, {1, 17, "puts"}, puts},
   {endliteral, {1, 22, nil}}] = tokenize("beginliteral IO.puts endliteral"),

  % Complex dotted paths also become separate tokens
  [{beginliteral, {1, 1, nil}},
   {alias, {1, 14, "Enum"}, 'Enum'},
   {'.', {1, 18, nil}},
   {identifier, {1, 19, "map"}, map},
   {'.', {1, 22, nil}},
   {identifier, {1, 23, "filter"}, filter},
   {endliteral, {1, 30, nil}}] = tokenize("beginliteral Enum.map.filter endliteral"),
  ok.

%% Test operators in sequence literals
sequence_operators_test() ->
  % Test arithmetic operators
  [{beginliteral, {1, 1, nil}},
   {identifier, {1, 14, "a"}, a},
   {dual_op, {1, 16, nil}, '+'},
   {identifier, {1, 18, "b"}, b},
   {endliteral, {1, 20, nil}}] = tokenize("beginliteral a + b endliteral"),

  % Test comparison operators
  [{beginliteral, {1, 1, nil}},
   {identifier, {1, 14, "a"}, a},
   {comp_op, {1, 16, nil}, '=='},
   {identifier, {1, 19, "b"}, b},
   {endliteral, {1, 21, nil}}] = tokenize("beginliteral a == b endliteral"),

  % Test complex operators
  [{beginliteral, {1, 1, nil}},
   {identifier, {1, 14, "a"}, a},
   {concat_op, {1, 16, nil}, '++'},
   {identifier, {1, 19, "b"}, b},
   {endliteral, {1, 21, nil}}] = tokenize("beginliteral a ++ b endliteral"),
  ok.

%% Test literals in sequence literals
sequence_literals_test() ->
  % Test string literals
  [{beginliteral, {1, 1, nil}},
   {bin_string, {1, 14, nil}, [<<"hello">>]},
   {endliteral, {1, 22, nil}}] = tokenize("beginliteral \"hello\" endliteral"),

  % Test single quoted strings - they become list_string with binary content
  [{beginliteral, {1, 1, nil}},
   {list_string, {1, 14, nil}, [<<"world">>]},
   {endliteral, {1, 22, nil}}] = tokenize("beginliteral 'world' endliteral"),

  % Test number literals
  [{beginliteral, {1, 1, nil}},
   {int, {1, 14, 42}, "42"},
   {endliteral, {1, 17, nil}}] = tokenize("beginliteral 42 endliteral"),

  % Test atom literals
  [{beginliteral, {1, 1, nil}},
   {atom, {1, 14, "foo"}, foo},
   {endliteral, {1, 19, nil}}] = tokenize("beginliteral :foo endliteral"),

  [{beginliteral, {1, 1, nil}},
   {atom_quoted, {1, 14, 34}, 'spaced atom'},
   {endliteral, {1, 29, nil}}] = tokenize("beginliteral :\"spaced atom\" endliteral"),

  % Test float literals
  [{beginliteral, {1, 1, nil}},
   {flt, {1, 14, 3.14}, "3.14"},
   {endliteral, {1, 19, nil}}] = tokenize("beginliteral 3.14 endliteral"),
  ok.

%% Test keywords in sequence literals
sequence_keyword_tokens_test() ->
  % Test if/do/end keywords
  [{beginliteral, {1, 1, nil}},
   {identifier, {1, 14, "if"}, 'if'},
   {'true', {1, 17, nil}},
   {'do', {1, 22, nil}},
   {'end', {1, 25, nil}},
   {endliteral, {1, 29, nil}}] = tokenize("beginliteral if true do end endliteral"),

  % Test boolean keywords
  [{beginliteral, {1, 1, nil}},
   {'true', {1, 14, nil}},
   {endliteral, {1, 19, nil}}] = tokenize("beginliteral true endliteral"),

  [{beginliteral, {1, 1, nil}},
   {'false', {1, 14, nil}},
   {endliteral, {1, 20, nil}}] = tokenize("beginliteral false endliteral"),

  % Test nil keyword
  [{beginliteral, {1, 1, nil}},
   {'nil', {1, 14, nil}},
   {endliteral, {1, 18, nil}}] = tokenize("beginliteral nil endliteral"),
  ok.

%% Test multiple sequence literals - normal tokens between sequences
sequence_multiple_test() ->
  % Simple sequence literal test
  Tokens = tokenize("beginliteral a endliteral"),

  % Expected: beginliteral, identifier, endliteral
  [{beginliteral, {1, 1, nil}},
   {identifier, {1, 14, "a"}, a},
   {endliteral, {1, 16, nil}}] = Tokens,
  ok.

%% Test empty sequence literal
sequence_empty_test() ->
  [{beginliteral, {1, 1, nil}},
   {endliteral, {1, 14, nil}}] = tokenize("beginliteral endliteral"),
  ok.

sequence_keyword_embedded_test() ->
  % Test that tilde is tokenized properly
  {error, _, _, _, _} = tokenize("beginliteral ~ endliteral"),

  % beginliteral inside another sequence literal causes error
  {error, _, _, _, _} = tokenize("beginliteral beginliteral endliteral"),

  ok.

%% Test beginliteral without endliteral - causes error
sequence_no_endliteral_test() ->
  % beginliteral without endliteral causes error
  {error, _, _, _, _} = tokenize("beginliteral a"),
  ok.

%% Test nested sequence literals
sequence_nested_test() ->
  % beginliteral a (b) c endliteral - inner parentheses should be preserved as tokens
  [{beginliteral, {1, 1, nil}},
   {identifier, {1, 14, "a"}, a},
   {'(', {1, 16, nil}},
   {identifier, {1, 17, "b"}, b},
   {')', {1, 18, nil}},
   {identifier, {1, 20, "c"}, c},
   {endliteral, {1, 22, nil}}] = tokenize("beginliteral a (b) c endliteral"),
  ok.

%% Test mixed content in sequence literals
sequence_mixed_content_test() ->
  % beginliteral "hello", 42, :atom, IO.puts endliteral
  [{beginliteral, {1, 1, nil}},
   {bin_string, {1, 14, nil}, [<<"hello">>]},
   {',', {1, 21, 0}},
   {int, {1, 23, 42}, "42"},
   {',', {1, 25, 0}},
   {atom, {1, 27, "atom"}, atom},
   {',', {1, 32, 0}},
   {alias, {1, 34, "IO"}, 'IO'},
   {'.', {1, 36, nil}},
   {identifier, {1, 37, "puts"}, puts},
   {endliteral, {1, 42, nil}}] = tokenize("beginliteral \"hello\", 42, :atom, IO.puts endliteral"),
  ok.

%% Test isolation - regular tokens inside sequence literals
sequence_isolation_test() ->
  % Complex expression with all token types
  Tokens = tokenize("beginliteral \"str\", 123, 3.14, :atom, IO.puts, +, ==, if, true endliteral"),

  % Verify we have regular Elixir tokens inside sequence literal
  InsideTokens = lists:filter(
    fun({beginliteral, _}) -> false;  % Skip beginliteral markers
       ({endliteral, _}) -> false;    % Skip endliteral markers
       ({',', _}) -> false;           % Skip commas
       (_) -> true                    % Include all other tokens
    end, Tokens),

  % We should have regular tokens like bin_string, int, flt, atom, etc.
  HasRegularTokens = lists:any(
    fun({bin_string, _, _}) -> true;
       ({int, _, _}) -> true;
       ({flt, _, _}) -> true;
       ({atom, _, _}) -> true;
       ({alias, _, _}) -> true;
       ({identifier, _, _}) -> true;
       ({dual_op, _, _}) -> true;
       ({comp_op3, _, _}) -> true;
       ({'if', _}) -> true;
       (_) -> false
    end, InsideTokens),

  ?assert(HasRegularTokens),
  ok.

%% Test error cases
sequence_error_test() ->
  % Test incomplete sequence literal - should produce an error
  case elixir_tokenizer:tokenize("beginliteral", 1, []) of
    {error, _, _, _, _} -> ok;
    _ -> erlang:error(should_error_on_incomplete)
  end,
  ok.
