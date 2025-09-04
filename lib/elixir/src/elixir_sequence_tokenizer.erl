%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 The Elixir Team

-module(elixir_sequence_tokenizer).
-include("elixir.hrl").
-include("elixir_tokenizer.hrl").
-export([tokenize/5]).

%% Main sequence literal tokenizer entry point
%% This function handles ALL tokenization within sequence literals (~~(...))
tokenize(String, Line, Column, Scope, Tokens) ->
  case String of
    % Handle end of input
    [] ->
      {ok, Line, Column, [], Tokens, []};

    % Handle closing parenthesis (exit sequence literal)
    [$) | Rest] when Scope#elixir_tokenizer.sequence_depth > 0 ->
      NewScope = Scope#elixir_tokenizer{sequence_depth = Scope#elixir_tokenizer.sequence_depth - 1},
      Token = {')', {Line, Column, previous_was_eol(Tokens)}},
      case NewScope#elixir_tokenizer.sequence_depth of
        0 ->
          % Exiting sequence literal - return tokens and let main tokenizer continue
          {ok, Line, Column + 1, [], [Token | Tokens], []};
        _ ->
          % Still inside nested sequence literal
          tokenize(Rest, Line, Column + 1, NewScope, [Token | Tokens])
      end;

    % Handle opening parenthesis and brackets (stay in sequence mode)
    [H | Rest] when H =:= $(; H =:= ${; H =:= $[ ->
      Token = {list_to_atom([H]), {Line, Column, nil}},
      tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

    % Handle closing brackets (not parenthesis)
    [H | Rest] when H =:= $}; H =:= $] ->
      Token = {list_to_atom([H]), {Line, Column, previous_was_eol(Tokens)}},
      tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

    % Handle sequence operator ~~
    [$~, $~ | Rest] ->
      Token = {sequence_op, {Line, Column, previous_was_eol(Tokens)}, '~~'},
      % Check if next token is opening parenthesis for nested sequence
      case Rest of
        [$( | _] ->
          NewScope = Scope#elixir_tokenizer{sequence_depth = Scope#elixir_tokenizer.sequence_depth + 1},
          tokenize(Rest, Line, Column + 2, NewScope, [Token | Tokens]);
        _ ->
          tokenize(Rest, Line, Column + 2, Scope, [Token | Tokens])
      end;

    % Handle strings - both double and single quotes become sequence_string
    [$" | Rest] ->
      tokenize_string(Rest, Line, Column + 1, $", Scope, Tokens);

    [$' | Rest] ->
      tokenize_string(Rest, Line, Column + 1, $', Scope, Tokens);

    % Handle atoms
    [$: | Rest] ->
      tokenize_atom(Rest, Line, Column + 1, Scope, Tokens);

    % Handle numbers
    [H | _] when ?is_digit(H) ->
      tokenize_number(String, Line, Column, Scope, Tokens);

    % Handle identifiers and keywords (including those with dots)
    [H | _] when ?is_upcase(H); ?is_downcase(H); H =:= $_ ->
      tokenize_identifier(String, Line, Column, Scope, Tokens);

    % Handle operators
    [$+, $+, $+ | Rest] ->
      tokenize_operator("+++", Rest, Line, Column + 3, Scope, Tokens);
    [$+, $+ | Rest] ->
      tokenize_operator("++", Rest, Line, Column + 2, Scope, Tokens);
    [$+ | Rest] ->
      tokenize_operator("+", Rest, Line, Column + 1, Scope, Tokens);

    [$-, $-, $- | Rest] ->
      tokenize_operator("---", Rest, Line, Column + 3, Scope, Tokens);
    [$-, $- | Rest] ->
      tokenize_operator("--", Rest, Line, Column + 2, Scope, Tokens);
    [$-, $> | Rest] ->
      tokenize_operator("->", Rest, Line, Column + 2, Scope, Tokens);
    [$- | Rest] ->
      tokenize_operator("-", Rest, Line, Column + 1, Scope, Tokens);

    [$*, $* | Rest] ->
      tokenize_operator("**", Rest, Line, Column + 2, Scope, Tokens);
    [$* | Rest] ->
      tokenize_operator("*", Rest, Line, Column + 1, Scope, Tokens);

    [$/ | Rest] ->
      tokenize_operator("/", Rest, Line, Column + 1, Scope, Tokens);

    [$=, $=, $= | Rest] ->
      tokenize_operator("===", Rest, Line, Column + 3, Scope, Tokens);
    [$=, $= | Rest] ->
      tokenize_operator("==", Rest, Line, Column + 2, Scope, Tokens);
    [$=, $~ | Rest] ->
      tokenize_operator("=~", Rest, Line, Column + 2, Scope, Tokens);
    [$= | Rest] ->
      tokenize_operator("=", Rest, Line, Column + 1, Scope, Tokens);

    [$!, $=, $= | Rest] ->
      tokenize_operator("!==", Rest, Line, Column + 3, Scope, Tokens);
    [$!, $= | Rest] ->
      tokenize_operator("!=", Rest, Line, Column + 2, Scope, Tokens);
    [$! | Rest] ->
      tokenize_operator("!", Rest, Line, Column + 1, Scope, Tokens);

    [$<, $< | Rest] ->
      tokenize_operator("<<", Rest, Line, Column + 2, Scope, Tokens);
    [$<, $= | Rest] ->
      tokenize_operator("<=", Rest, Line, Column + 2, Scope, Tokens);
    [$<, $- | Rest] ->
      tokenize_operator("<-", Rest, Line, Column + 2, Scope, Tokens);
    [$< | Rest] ->
      tokenize_operator("<", Rest, Line, Column + 1, Scope, Tokens);

    [$>, $> | Rest] ->
      tokenize_operator(">>", Rest, Line, Column + 2, Scope, Tokens);
    [$>, $= | Rest] ->
      tokenize_operator(">=", Rest, Line, Column + 2, Scope, Tokens);
    [$> | Rest] ->
      tokenize_operator(">", Rest, Line, Column + 1, Scope, Tokens);

    [$&, $& | Rest] ->
      tokenize_operator("&&", Rest, Line, Column + 2, Scope, Tokens);
    [$& | Rest] ->
      tokenize_operator("&", Rest, Line, Column + 1, Scope, Tokens);

    [$|, $| | Rest] ->
      tokenize_operator("||", Rest, Line, Column + 2, Scope, Tokens);
    [$|, $> | Rest] ->
      tokenize_operator("|>", Rest, Line, Column + 2, Scope, Tokens);
    [$| | Rest] ->
      tokenize_operator("|", Rest, Line, Column + 1, Scope, Tokens);

    [$., $. | Rest] ->
      tokenize_operator("..", Rest, Line, Column + 2, Scope, Tokens);
    [$. | Rest] ->
      tokenize_operator(".", Rest, Line, Column + 1, Scope, Tokens);

    % Handle punctuation
    [$, | Rest] ->
      Token = {',', {Line, Column, previous_was_eol(Tokens)}},
      tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

    [$; | Rest] ->
      Token = {';', {Line, Column, previous_was_eol(Tokens)}},
      tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

    % Handle whitespace
    [H | Rest] when ?is_space(H) ->
      case H of
        $\n ->
          tokenize(Rest, Line + 1, 1, Scope, reset_eol(Tokens));
        $\r ->
          case Rest of
            [$\n | RestAfterCRLF] ->
              tokenize(RestAfterCRLF, Line + 1, 1, Scope, reset_eol(Tokens));
            _ ->
              tokenize(Rest, Line + 1, 1, Scope, reset_eol(Tokens))
          end;
        _ ->
          tokenize(Rest, Line, Column + 1, Scope, Tokens)
      end;

    % Handle unexpected characters
    [H | Rest] ->
      Reason = {?LOC(Line, Column), "unexpected character in sequence literal: ", [H]},
      {error, Reason, Rest, [], Tokens}
  end.

%% String tokenization - both "..." and '...' become sequence_string
tokenize_string(String, Line, Column, Quote, Scope, Tokens) ->
  case extract_string(String, Quote, Line, Column, []) of
    {ok, Value, Rest, NewLine, NewColumn} ->
      Token = {sequence_string, {Line, Column - 1, nil}, Value},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, String, [], Tokens}
  end.

%% Extract string content until closing quote
extract_string([], Quote, Line, Column, Acc) ->
  {error, {?LOC(Line, Column), "missing terminator: ", [Quote]}};
extract_string([Quote | Rest], Quote, Line, Column, Acc) ->
  {ok, lists:reverse(Acc), Rest, Line, Column + 1};
extract_string([$\\, C | Rest], Quote, Line, Column, Acc) ->
  % Simple escape handling - just include the escaped character
  extract_string(Rest, Quote, Line, Column + 2, [C | Acc]);
extract_string([$\n | Rest], Quote, Line, Column, Acc) ->
  extract_string(Rest, Quote, Line + 1, 1, [$\n | Acc]);
extract_string([C | Rest], Quote, Line, Column, Acc) ->
  extract_string(Rest, Quote, Line, Column + 1, [C | Acc]).

%% Atom tokenization - :atom becomes sequence_atom
tokenize_atom(String, Line, Column, Scope, Tokens) ->
  case extract_atom(String, Line, Column, []) of
    {ok, Value, Rest, Length} ->
      Token = {sequence_atom, {Line, Column - 1, nil}, list_to_atom(Value)},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, String, [], Tokens}
  end.

%% Extract atom name
extract_atom([], Line, Column, []) ->
  {error, {?LOC(Line, Column), "missing atom name after :", []}};
extract_atom([], _Line, _Column, Acc) ->
  {ok, lists:reverse(Acc), [], length(Acc)};
extract_atom([H | Rest], Line, Column, Acc) when ?is_upcase(H); ?is_downcase(H); ?is_digit(H); H =:= $_; H =:= $? ->
  extract_atom(Rest, Line, Column, [H | Acc]);
extract_atom(String, _Line, _Column, []) ->
  {error, {?LOC(_Line, _Column), "invalid atom name", []}};
extract_atom(String, _Line, _Column, Acc) ->
  {ok, lists:reverse(Acc), String, length(Acc)}.

%% Number tokenization - integers and floats become sequence_number
tokenize_number(String, Line, Column, Scope, Tokens) ->
  case extract_number(String, Line, Column) of
    {ok, Value, Rest, Length} ->
      Token = {sequence_number, {Line, Column, nil}, Value},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, String, [], Tokens}
  end.

%% Extract number (integer or float)
extract_number(String, Line, Column) ->
  case extract_integer(String, []) of
    {ok, IntStr, [$. | Rest]} ->
      % Potential float
      case extract_integer(Rest, []) of
        {ok, FracStr, FinalRest} ->
          FullStr = IntStr ++ "." ++ FracStr,
          case catch list_to_float(FullStr) of
            {'EXIT', _} ->
              {error, {?LOC(Line, Column), "invalid float: ", FullStr}};
            Float ->
              {ok, Float, FinalRest, length(FullStr)}
          end;
        _ ->
          % Not a valid float, treat as integer
          case catch list_to_integer(IntStr) of
            {'EXIT', _} ->
              {error, {?LOC(Line, Column), "invalid integer: ", IntStr}};
            Int ->
              {ok, Int, [$. | Rest], length(IntStr)}
          end
      end;
    {ok, IntStr, FinalRest} ->
      % Integer
      case catch list_to_integer(IntStr) of
        {'EXIT', _} ->
          {error, {?LOC(Line, Column), "invalid integer: ", IntStr}};
        Int ->
          {ok, Int, FinalRest, length(IntStr)}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% Extract integer digits
extract_integer([], Acc) when Acc =/= [] ->
  {ok, lists:reverse(Acc), []};
extract_integer([H | Rest], Acc) when ?is_digit(H) ->
  extract_integer(Rest, [H | Acc]);
extract_integer(String, []) ->
  {error, "no digits found"};
extract_integer(String, Acc) ->
  {ok, lists:reverse(Acc), String}.

%% Identifier tokenization - allows dots and becomes sequence_identifier or sequence_keyword
tokenize_identifier(String, Line, Column, Scope, Tokens) ->
  case extract_identifier(String, []) of
    {ok, Value, Rest, Length} ->
      Atom = list_to_atom(Value),
      TokenType = case is_keyword(Atom) of
        true -> sequence_keyword;
        false -> sequence_identifier
      end,
      Token = {TokenType, {Line, Column, nil}, Atom},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, String, [], Tokens}
  end.

%% Extract identifier allowing dots
extract_identifier([], Acc) when Acc =/= [] ->
  {ok, lists:reverse(Acc), [], length(Acc)};
extract_identifier([H | Rest], Acc) when ?is_upcase(H); ?is_downcase(H); ?is_digit(H); H =:= $_; H =:= $.; H =:= $? ->
  extract_identifier(Rest, [H | Acc]);
extract_identifier(String, []) ->
  {error, "no identifier found"};
extract_identifier(String, Acc) ->
  {ok, lists:reverse(Acc), String, length(Acc)}.

%% Check if an atom is a keyword
is_keyword('and') -> true;
is_keyword('or') -> true;
is_keyword('not') -> true;
is_keyword('if') -> true;
is_keyword('else') -> true;
is_keyword('elsif') -> true;
is_keyword('unless') -> true;
is_keyword('when') -> true;
is_keyword('do') -> true;
is_keyword('end') -> true;
is_keyword('def') -> true;
is_keyword('defp') -> true;
is_keyword('defmacro') -> true;
is_keyword('defstruct') -> true;
is_keyword('defmodule') -> true;
is_keyword('defprotocol') -> true;
is_keyword('defimpl') -> true;
is_keyword('true') -> true;
is_keyword('false') -> true;
is_keyword('nil') -> true;
is_keyword('try') -> true;
is_keyword('catch') -> true;
is_keyword('rescue') -> true;
is_keyword('after') -> true;
is_keyword('case') -> true;
is_keyword('cond') -> true;
is_keyword('receive') -> true;
is_keyword('for') -> true;
is_keyword('with') -> true;
is_keyword('fn') -> true;
is_keyword(_) -> false.

%% Operator tokenization - all operators become sequence_operator
tokenize_operator(OpStr, Rest, Line, Column, Scope, Tokens) ->
  OpAtom = list_to_atom(OpStr),
  Token = {sequence_operator, {Line, Column - length(OpStr), nil}, OpAtom},
  tokenize(Rest, Line, Column, Scope, [Token | Tokens]).

%% Helper functions
previous_was_eol([]) -> false;
previous_was_eol([{_, {_, _, EOL}, _} | _]) when is_boolean(EOL) -> EOL;
previous_was_eol([{_, {_, _, _}} | _]) -> false;
previous_was_eol([{_, _} | _]) -> false;
previous_was_eol(_) -> false.

reset_eol(Tokens) ->
  % This is a simplified version - in real implementation,
  % this would properly handle end-of-line tracking
  Tokens.
