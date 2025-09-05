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

    % Handle strings - double quotes become sequence_string
    [$" | Rest] ->
      tokenize_string(Rest, Line, Column + 1, $", Scope, Tokens);

    % Handle atoms with colon prefix
    [$: | Rest] ->
      tokenize_atom(Rest, Line, Column + 1, Scope, Tokens);

    % Handle numbers
    [H | _] when ?is_digit(H) ->
      tokenize_number(String, Line, Column, Scope, Tokens);

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

    % Handle any other non-whitespace sequence as sequence_token
    _ ->
      tokenize_sequence_token(String, Line, Column, Scope, Tokens)
  end.

%% String tokenization - "..." becomes sequence_string
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
tokenize_atom([$" | Rest], Line, Column, Scope, Tokens) ->
  % Handle quoted atom like :"spaced atom"
  extract_quoted_atom(Rest, Line, Column + 1, [], Scope, Tokens, Column - 1);
tokenize_atom(String, Line, Column, Scope, Tokens) ->
  case extract_atom(String, Line, Column, []) of
    {ok, Value, Rest, Length} ->
      Token = {sequence_atom, {Line, Column - 1, nil}, list_to_atom(Value)},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, String, [], Tokens}
  end.

%% Extract quoted atom - handles :"spaced atom"
extract_quoted_atom([], Line, Column, _Acc, _Scope, _Tokens, _StartColumn) ->
  {error, {?LOC(Line, Column), "missing closing quote for atom", []}, [], [], []};
extract_quoted_atom([$" | Rest], Line, Column, Acc, Scope, Tokens, StartColumn) ->
  AtomName = lists:reverse(Acc),
  Token = {sequence_atom, {Line, StartColumn, nil}, list_to_atom(AtomName)},
  tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);
extract_quoted_atom([$\\ | [C | Rest]], Line, Column, Acc, Scope, Tokens, StartColumn) ->
  % Handle escape sequences
  extract_quoted_atom(Rest, Line, Column + 2, [C | Acc], Scope, Tokens, StartColumn);
extract_quoted_atom([$\n | Rest], Line, Column, Acc, Scope, Tokens, StartColumn) ->
  extract_quoted_atom(Rest, Line + 1, 1, [$\n | Acc], Scope, Tokens, StartColumn);
extract_quoted_atom([C | Rest], Line, Column, Acc, Scope, Tokens, StartColumn) ->
  extract_quoted_atom(Rest, Line, Column + 1, [C | Acc], Scope, Tokens, StartColumn).

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

%% Sequence token tokenization - any non-structural element becomes sequence_token
tokenize_sequence_token(String, Line, Column, Scope, Tokens) ->
  case extract_sequence_token(String, []) of
    {ok, Value, Rest, Length} ->
      Atom = list_to_atom(Value),
      Token = {sequence_token, {Line, Column, nil}, Atom},
      tokenize(Rest, Line, Column + Length, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, String, [], Tokens}
  end.

%% Extract sequence token - anything until whitespace or structural character
extract_sequence_token([], Acc) when Acc =/= [] ->
  {ok, lists:reverse(Acc), [], length(Acc)};
extract_sequence_token([H | Rest], Acc) when ?is_space(H); H =:= $(; H =:= $); H =:= ${; 
                                              H =:= $}; H =:= $[; H =:= $]; H =:= $,; H =:= $; ->
  case Acc of
    [] -> {error, "no token found"};
    _ -> {ok, lists:reverse(Acc), [H | Rest], length(Acc)}
  end;
extract_sequence_token([H | Rest], Acc) ->
  extract_sequence_token(Rest, [H | Acc]);
extract_sequence_token([], []) ->
  {error, "no token found"}.

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