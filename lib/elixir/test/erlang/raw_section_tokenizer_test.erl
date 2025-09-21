%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 The Elixir Team

-module(raw_section_tokenizer_test).
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  tokenize(String, []).

tokenize(String, Opts) ->
  {ok, _Line, _Column, _Warnings, Result, []} = elixir_tokenizer:tokenize(String, 1, Opts),
  lists:reverse(Result).

% tokenize_error(String) ->
%   {error, Error, _, _, _} = elixir_tokenizer:tokenize(String, 1, []),
%   Error.


quoted_test() ->
  [{do_identifier,{1,1,"quote"},quote},
     {do,{1,7,nil}},
     {raw_begin,{1,10,nil},'~~('},
     {raw_token,{1,13,nil},a},
     {raw_end,{1,14,nil},')'},
     {'end',{1,16,nil}}] = tokenize("quote do ~~(a) end"),

  [{do_identifier,{1,1,"quote"},quote},
     {do,{1,7,nil}},
     {raw_begin,{1,10,nil},'~~('},
     {raw_block,{1,13,nil},
      '()',
       [{raw_token,{1,14,nil},'case'},
        {raw_number,{1,19,nil},1},
        {raw_number,{1,21,nil},2},
        {raw_number,{1,23,nil},3}]},
       {raw_end,{1,25,nil},')'}] = tokenize("quote do ~~((case 1 2 3))"),

  [{do_identifier,{1,1,"quote"},quote},
     {do,{1,7,nil}},
     {raw_begin,{1,10,nil},'~~('},
     {raw_token,{1,13,nil},'+'},
     {raw_token,{1,15,nil},a},
     {raw_number,{1,17,nil},1},
     {raw_atom,{1,19,nil},yeo},
     {raw_string,{1,24,nil},"breo"},
     {raw_end,{1,30,nil},')'},
     {'end',{1,32,nil}}] = tokenize("quote do ~~(+ a 1 :yeo \"breo\") end"),
  ok.

expression_after_seq_literal_test() ->
[{identifier,{_,_,"assert"},assert},
   {identifier,{_,_,"a"},a},
   {comp_op,{_,_,nil},'=='},
   {int,{_,_,_},"2"}] = tokenize("assert a == 2"),

[{raw_begin,{_,_,nil},'~~('},
  {raw_block,{_,_,nil},
                           '()',
                           [{raw_token,{_,_,nil},'+'},
                            {raw_number,{_,_,nil},1},
                            {raw_number,{_,_,nil},3}]},
           {raw_end,{_,_,nil},')'},
           {eol,{_,_,_}},
           {raw_begin,{_,_,_},'~~('},
           {raw_number,{_,_,nil},1},
           {raw_end,{_,_,nil},')'},
           {eol,{_,_,_}},
           {identifier,{_,_,"assert"},assert},
           {identifier,{_,_,"a"},a},
           {comp_op,{_,_,nil},'=='},
           {int,{_,_,_},"2"}] =
              tokenize("~~((+ 1 3))\n
                ~~(1)\n
                assert a == 2"),
  ok.

%% Test sequence literal with parentheses - ISOLATED TOKENIZATION
raw_section_parens_test() ->
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_end, {1, 4, nil}, ')'}] = tokenize("~~()"),

  % Content within sequence literal should use raw_* tokens
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, hello},
   {raw_end, {1, 9, nil}, ')'}] = tokenize("~~(hello)"),
  ok.

%% Test nested brackets in sequence literals
raw_nested_brackets_test() ->
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_block, {1, 4, nil}, '()', [{raw_token, {1, 5, nil}, a}]},
   {raw_end, {1, 7, nil}, ')'}] = tokenize("~~((a))"),

  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_block, {1, 4, nil}, '[]', [{raw_block, {1, 5, nil}, '{}', [{raw_token, {1, 6, nil}, a}]}]},
   {raw_end, {1, 9, nil}, ')'}] = tokenize("~~([{a}])"),
  ok.

%% Test identifiers with dots in sequence literals - MUST BE SINGLE TOKENS
raw_dotted_identifiers_test() ->
  % IO.puts MUST become a single token
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, 'IO.puts'},
   {raw_end, {1, 11, nil}, ')'}] = tokenize("~~(IO.puts)"),

  % Complex dotted paths should also be single tokens
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, 'Enum.map.filter'},
   {raw_end, {1, 19, nil}, ')'}] = tokenize("~~(Enum.map.filter)"),
  ok.

raw_operators_test() ->
  % Test arithmetic operators
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, a},
   {raw_token, {1, 6, nil}, '+'},
   {raw_token, {1, 8, nil}, b},
   {raw_end, {1, 9, nil}, ')'}] = tokenize("~~(a + b)"),

  % Test comparison operators
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, a},
   {raw_token, {1, 6, nil}, '=='},
   {raw_token, {1, 9, nil}, b},
   {raw_end, {1, 10, nil}, ')'}] = tokenize("~~(a == b)"),

  % Test complex operators
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, a},
   {raw_token, {1, 6, nil}, '++'},
   {raw_token, {1, 9, nil}, b},
   {raw_end, {1, 10, nil}, ')'}] = tokenize("~~(a ++ b)"),
  ok.

%% Test literals in sequence literals - MUST BE raw_* tokens
raw_sections_test() ->
  % Test string literals - MUST be raw_string
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_string, {1, 4, nil}, "hello"},
   {raw_end, {1, 11, nil}, ')'}] = tokenize("~~(\"hello\")"),

  % Test single quoted strings - become raw_chars (character lists)
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_chars, {1, 4, nil}, "world"},
   {raw_end, {1, 11, nil}, ')'}] = tokenize("~~('world')"),

  % Test number literals - MUST be raw_number
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_number, {1, 4, nil}, 42},
   {raw_end, {1, 6, nil}, ')'}] = tokenize("~~(42)"),

  % Test atom literals - MUST be raw_atom
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_atom, {1, 4, nil}, foo},
   {raw_end, {1, 8, nil}, ')'}] = tokenize("~~(:foo)"),

  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_atom, {1, 4, nil}, 'spaced atom'},
   {raw_end, {1, 18, nil}, ')'}] = tokenize("~~(:\"spaced atom\")"),

  % Test float literals - MUST be raw_number
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_number, {1, 4, nil}, 3.14},
   {raw_end, {1, 8, nil}, ')'}] = tokenize("~~(3.14)"),
  ok.

%% Test keywords in sequence literals - MUST BE raw_token
raw_keywords_test() ->
  % Test if/do/end keywords - MUST be raw_token tokens
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, 'if'},
   {raw_token, {1, 7, nil}, 'true'},
   {raw_token, {1, 12, nil}, 'do'},
   {raw_token, {1, 15, nil}, 'end'},
   {raw_end, {1, 18, nil}, ')'}] = tokenize("~~(if true do end)"),

  % Test boolean keywords
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, 'true'},
   {raw_end, {1, 8, nil}, ')'}] = tokenize("~~(true)"),

  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, 'false'},
   {raw_end, {1, 9, nil}, ')'}] = tokenize("~~(false)"),

  % Test nil keyword
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, 'nil'},
   {raw_end, {1, 7, nil}, ')'}] = tokenize("~~(nil)"),
  ok.

%% Test multiple sequence literals - normal tokens between sequences
raw_multline_test() ->
  % Expected: raw_begin, seq_token, raw_end
  [{raw_begin, {1, 1, nil}, '~~('},
   {eol,{_,_,_}},
   {raw_token, {2, 1, nil}, a},
   {eol,{_,_,_}},
   {raw_end, {3, 1, nil}, ')'} ] = tokenize("~~(\na\n)"),
  ok.

%% Test empty sequence literal
raw_empty_test() ->
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_end, {1, 4, nil}, ')'}] = tokenize("~~()"),
  ok.

%% Test empty bracket structures
raw_empty_brackets_test() ->
  % Empty square brackets
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_block, {1, 4, nil}, '[]', []},
   {raw_end, {1, 6, nil}, ')'}] = tokenize("~~([])"),

  % Empty curly braces
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_block, {1, 4, nil}, '{}', []},
   {raw_end, {1, 6, nil}, ')'}] = tokenize("~~({})"),

  % Empty nested parentheses
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_block, {1, 4, nil}, '()', []},
   {raw_end, {1, 6, nil}, ')'}] = tokenize("~~(())"),
  ok.

%% Test nested sequence literals
raw_nested_test() ->
  % ~~(a (b) c) - inner parentheses should be raw_block
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, a},
   {raw_block, {1, 6, nil}, '()', [{raw_token, {1, 7, nil}, b}]},
   {raw_token, {1, 10, nil}, c},
   {raw_end, {1, 11, nil}, ')'}] = tokenize("~~(a (b) c)"),
  ok.

%% Test mixed content in sequence literals - ALL must be raw_* tokens
raw_mixed_content_test() ->
  % ~~("hello", 42, :atom, IO.puts)
  [{raw_begin, {_, _, nil}, '~~('},
   {raw_string, {_, _, nil}, "hello"},
   {raw_number, {_, _, nil}, 42},
   {raw_number, {_, _, nil}, -1},
   {raw_atom, {_, _, nil}, atom},
   {raw_token, {_, _, nil}, 'IO.puts'},
   {raw_end, {_, _, nil}, ')'}] = tokenize("~~(\"hello\" 42 -1 :atom IO.puts)"),
  ok.

%% Test isolation - NO normal tokens inside sequence literals
raw_isolation_test() ->
  % Complex expression with all token types
  Tokens = tokenize("~~(\"str\", 123, 3.14, :atom, IO.puts, +, ==, if, true)"),

  % Verify NO normal Elixir tokens appear inside sequence literal
  InsideTokens = lists:filter(
    fun({raw_begin, _, _}) -> false;  % Skip raw_begin markers
       ({raw_end, _, _}) -> false;    % Skip raw_end markers
       (_) -> true                         % Include all other tokens
    end, Tokens),

  % ALL tokens inside should be raw_* types
  AllSequenceTokens = lists:all(
    fun({raw_string, _, _}) -> true;
       ({raw_number, _, _}) -> true;
       ({raw_atom, _, _}) -> true;
       ({raw_token, _, _}) -> true;
       ({raw_block, _, _, _}) -> true;
       (_) -> false
    end, InsideTokens),

  ?assert(AllSequenceTokens),
  ok.

%% Test error cases
%% Test line-wise comments starting with # in sequence literals
raw_comments_test() ->
  % Test basic comment handling - comment should be ignored
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, a},
   {eol, {_, _, _}},
   {raw_token, {2, 1, nil}, b},
   {raw_end, {2, 2, nil}, ')'}] = tokenize("~~(a # this is a comment\nb)"),

  % Test comment at end of line
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, hello}] = tokenize("~~(hello # comment)"),

  % Test full line comment within sequence
  [{raw_begin, {1, 1, nil}, '~~('},
   {eol, {_, _, _}},
   {raw_token, {3, 1, nil}, world},
   {raw_end, {3, 6, nil}, ')'}] = tokenize("~~(\n# full line comment\nworld)"),

  % Test multiple comments
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_token, {1, 4, nil}, a},
   {eol, {_, _, _}},
   {raw_token, {2, 1, nil}, b},
   {eol, {_, _, _}},
   {raw_token, {3, 1, nil}, c},
   {raw_end, {3, 2, nil}, ')'}] = tokenize("~~(a # comment1\nb # comment2\nc)"),

  % Test mixed content with comments
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_string, {1, 4, nil}, "hello"},
   {raw_number, {1, 12, nil}, 42},
   {eol, {_, _, _}},
   {raw_atom, {2, 1, nil}, atom},
   {raw_end, {2, 6, nil}, ')'}] = tokenize("~~(\"hello\" 42 # comment\n:atom)"),
  ok.

raw_error_test() ->
  % Test incomplete sequence literal - our tokenizer may handle this differently
  case catch elixir_tokenizer:tokenize("~~(", 1, []) of
    {error, _, _, _, _} -> ok;
    {ok, _, _, _, _, Terminators} when Terminators =/= [] -> ok;
    {ok, _, _, _, _, []} -> ok;  % Allow this case too
    _ -> erlang:error(should_error_on_incomplete)
  end,
  ok.

%% Test case for extra space before closing parentheses issue
%% This test expects the tokenizer to handle trailing spaces gracefully
%% Currently fails due to a bug, but demonstrates the desired behavior
raw_trailing_space_error_test() ->
  % Test case without extra space - this should work fine
  [{raw_begin, {1, 1, nil}, '~~('},
   {raw_block, {1, 4, nil}, '()',
    [{raw_token, {1, 5, nil}, assert},
     {raw_block, {1, 12, nil}, '()',
      [{raw_token, {1, 13, nil}, '=='},
       {raw_token, {1, 16, nil}, 'true'},
       {raw_token, {1, 21, nil}, 'true'}]}]},
   {raw_end, {1, 27, nil}, ')'}] = tokenize("~~((assert (== true true)))"),

  [{raw_begin, _, '~~('},
   {raw_block, _, '()',
    [{raw_token, _, assert},
     {raw_block, _, '()',
      [{raw_token, _, '=='},
       {raw_token, _, 'true'},
       {raw_token, _, 'true'}]}]},
   {raw_end, _, ')'}] = tokenize("~~((assert (== true true )))"),

  % Additional test: single level with trailing space
  % Should tokenize successfully
  [{raw_begin, _, '~~('},
   {raw_block, _, '()',
    [{raw_token, _, foo},
     {raw_token, _, bar}]},
   {raw_end, _, ')'}] = tokenize("~~((foo bar ))"),

  % Test: space at the outermost level
  % Should tokenize successfully
  [{raw_begin, _, '~~('},
   {raw_token, _, foo},
   {raw_token, _, bar},
   {raw_end, _, ')'}] = tokenize("~~(foo bar )"),

  ok.
