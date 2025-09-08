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
      Token = {sequence_end, {Line, Column, nil}, ')'},
      case NewScope#elixir_tokenizer.sequence_depth of
        0 ->
          % Exiting sequence literal - return remainder for main tokenizer to continue
          {ok, Line, Column + 1, Rest, [Token | Tokens], []};
        _ ->
          % Still inside nested sequence literal
          tokenize(Rest, Line, Column + 1, NewScope, [Token | Tokens])
      end;

    % Handle opening brackets - collect content until matching closing bracket
    [H | Rest] when H =:= $(; H =:= ${; H =:= $[ ->
      BracketType = case H of
        $( -> '()';
        ${ -> '{}';
        $[ -> '[]'
      end,
      ClosingChar = case H of
        $( -> $);
        ${ -> $};
        $[ -> $]
      end,
      NewScope = case H of
        $( -> Scope#elixir_tokenizer{sequence_depth = Scope#elixir_tokenizer.sequence_depth + 1};
        _ -> Scope
      end,
      % Extract content between brackets
      case extract_bracket_content(Rest, ClosingChar, Line, Column + 1, NewScope, []) of
        {ok, ContentTokens, RestAfterBracket, FinalLine, FinalColumn, FinalScope} ->
          Token = {sequence_block, {Line, Column, nil}, BracketType, lists:reverse(ContentTokens)},
          tokenize(RestAfterBracket, FinalLine, FinalColumn, FinalScope, [Token | Tokens]);
        {error, Reason} ->
          {error, Reason, Rest, [], Tokens}
      end;

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

    % Handle single-quoted strings - become sequence_chars
    [$' | Rest] ->
      tokenize_chars(Rest, Line, Column + 1, $', Scope, Tokens);

    % Handle atoms with colon prefix
    [$: | Rest] ->
      tokenize_atom(Rest, Line, Column + 1, Scope, Tokens);

    % Handle numbers (including negative numbers)
    [H | _] when ?is_digit(H) ->
      tokenize_number(String, Line, Column, Scope, Tokens);
    
    % Handle minus sign followed by digit (negative number)
    [$-, D | _] when ?is_digit(D) ->
      tokenize_number(String, Line, Column, Scope, Tokens);

    % Handle punctuation
    [$, | Rest] ->
      Token = {sequence_token, {Line, Column, nil}, ','},
      tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

    [$; | Rest] ->
      Token = {';', {Line, Column, previous_was_eol(Tokens)}},
      tokenize(Rest, Line, Column + 1, Scope, [Token | Tokens]);

    % Handle comments
    [$# | Rest] ->
      case tokenize_comment(Rest, [$#]) of
        {error, Char} ->
          error_comment(Char, [$# | Rest], Line, Column, Scope, Tokens);
        {CommentRest, Comment} ->
          preserve_comments(Line, Column, Tokens, Comment, CommentRest, Scope),
          tokenize(CommentRest, Line, Column, Scope, reset_eol(Tokens))
      end;

    % Handle whitespace
    [H | Rest] when ?is_space(H) ->
      case H of
        $\n ->
          EolTokens = eol(Line, Column, Tokens),
          tokenize(Rest, Line + 1, 1, Scope, reset_eol(EolTokens));
        $\r ->
          case Rest of
            [$\n | RestAfterCRLF] ->
              EolTokens = eol(Line, Column, Tokens),
              tokenize(RestAfterCRLF, Line + 1, 1, Scope, reset_eol(EolTokens));
            _ ->
              EolTokens = eol(Line, Column, Tokens),
              tokenize(Rest, Line + 1, 1, Scope, reset_eol(EolTokens))
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

%% Chars tokenization - '...' becomes sequence_chars
tokenize_chars(String, Line, Column, Quote, Scope, Tokens) ->
  case extract_string(String, Quote, Line, Column, []) of
    {ok, Value, Rest, NewLine, NewColumn} ->
      Token = {sequence_chars, {Line, Column - 1, nil}, Value},
      tokenize(Rest, NewLine, NewColumn, Scope, [Token | Tokens]);
    {error, Reason} ->
      {error, Reason, String, [], Tokens}
  end.

%% Extract string content until closing quote
extract_string([], Quote, Line, Column, _Acc) ->
  {error, {?LOC(Line, Column), "missing terminator: ", [Quote]}};
extract_string([Quote | Rest], Quote, Line, Column, Acc) ->
  {ok, lists:reverse(Acc), Rest, Line, Column + 1};
extract_string([$\\, C | Rest], Quote, Line, Column, Acc) ->
  % Simple escape handling - just include the escaped character
  extract_string(Rest, Quote, Line, Column + 2, [C | Acc]);
extract_string([$\n | Rest], Quote, Line, _Column, Acc) ->
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
extract_quoted_atom([$\n | Rest], Line, _Column, Acc, Scope, Tokens, StartColumn) ->
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
extract_atom(_String, Line, Column, []) ->
  {error, {?LOC(Line, Column), "invalid atom name", []}};
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

%% Extract number (integer or float, including negative)
extract_number([$- | Rest], Line, Column) ->
  % Handle negative numbers
  case extract_integer(Rest, []) of
    {ok, IntStr, RestAfterInt} ->
      case RestAfterInt of
        [$. | DecimalRest] ->
          % Potential negative float
          case extract_integer(DecimalRest, []) of
            {ok, FracStr, FinalRest} ->
              FullStr = "-" ++ IntStr ++ "." ++ FracStr,
              case catch list_to_float(FullStr) of
                {'EXIT', _} ->
                  {error, {?LOC(Line, Column), "invalid float: ", FullStr}};
                Float ->
                  {ok, Float, FinalRest, length(FullStr)}
              end;
            _ ->
              % Not a valid float, treat as negative integer
              NegIntStr = "-" ++ IntStr,
              case catch list_to_integer(NegIntStr) of
                {'EXIT', _} ->
                  {error, {?LOC(Line, Column), "invalid integer: ", NegIntStr}};
                Int ->
                  {ok, Int, [$. | DecimalRest], length(NegIntStr)}
              end
          end;
        _ ->
          % Negative integer
          NegIntStr = "-" ++ IntStr,
          case catch list_to_integer(NegIntStr) of
            {'EXIT', _} ->
              {error, {?LOC(Line, Column), "invalid integer: ", NegIntStr}};
            Int ->
              {ok, Int, RestAfterInt, length(NegIntStr)}
          end
      end;
    {error, Reason} ->
      {error, Reason}
  end;
extract_number(String, Line, Column) ->
  % Handle positive numbers
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
extract_integer(_String, []) ->
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
                                              H =:= $}; H =:= $[; H =:= $]; H =:= $,; H =:= $;; H =:= $"; H =:= $'; H =:= $# ->
  case Acc of
    [] -> {error, "no token found"};
    _ -> {ok, lists:reverse(Acc), [H | Rest], length(Acc)}
  end;
extract_sequence_token([H | Rest], Acc) ->
  extract_sequence_token(Rest, [H | Acc]);
extract_sequence_token([], []) ->
  {error, "no token found"}.

%% Extract content between matching brackets
extract_bracket_content(String, ClosingChar, Line, Column, Scope, Acc) ->
  % Handle empty brackets - if we immediately find the closing character
  case String of
    [ClosingChar | Rest] when ClosingChar =/= $) ->
      % Empty bracket pair (but not closing paren which ends sequence)
      {ok, Acc, Rest, Line, Column + 1, Scope};
    [ClosingChar | _] when ClosingChar =:= $), Scope#elixir_tokenizer.sequence_depth =:= 1 ->
      % This is the final closing paren of an empty sequence literal 
      {ok, Acc, String, Line, Column, Scope};
    [ClosingChar | Rest] when ClosingChar =:= $) ->
      % Empty nested parentheses
      FinalScope = Scope#elixir_tokenizer{sequence_depth = Scope#elixir_tokenizer.sequence_depth - 1},
      {ok, Acc, Rest, Line, Column + 1, FinalScope};
    _ ->
      % Not empty, proceed with normal tokenization
      case tokenize_single_item(String, Line, Column, Scope) of
    {ok, Token, Rest, NewLine, NewColumn, NewScope} ->
      case Rest of
        [ClosingChar | FinalRest] when ClosingChar =/= $) ->
          % Found matching closing bracket (but not closing paren which ends sequence)
          FinalScope = case ClosingChar of
            $) -> NewScope#elixir_tokenizer{sequence_depth = NewScope#elixir_tokenizer.sequence_depth - 1};
            _ -> NewScope
          end,
          {ok, [Token | Acc], FinalRest, NewLine, NewColumn + 1, FinalScope};
        [ClosingChar | _] when ClosingChar =:= $), NewScope#elixir_tokenizer.sequence_depth =:= 1 ->
          % This is the final closing paren of the sequence literal - don't consume it
          {ok, [Token | Acc], Rest, NewLine, NewColumn, NewScope};
        [ClosingChar | FinalRest] when ClosingChar =:= $) ->
          % This is a closing paren but we're still nested
          FinalScope = NewScope#elixir_tokenizer{sequence_depth = NewScope#elixir_tokenizer.sequence_depth - 1},
          {ok, [Token | Acc], FinalRest, NewLine, NewColumn + 1, FinalScope};
        _ ->
          % Continue collecting tokens
          extract_bracket_content(Rest, ClosingChar, NewLine, NewColumn, NewScope, [Token | Acc])
      end;
    {error, Reason} ->
      {error, Reason};
    {end_of_input, Rest, FinalLine, FinalColumn, FinalScope} ->
      % Reached end without finding closing bracket
      case ClosingChar of
        $) -> {ok, Acc, Rest, FinalLine, FinalColumn, FinalScope};
        _ -> {error, {?LOC(Line, Column), "missing closing bracket: ", [ClosingChar]}}
      end
      end
  end.

%% Tokenize a single item (used by extract_bracket_content)
tokenize_single_item(String, Line, Column, Scope) ->
  case String of
    [] ->
      {end_of_input, [], Line, Column, Scope};
    
    % Handle comments
    [$# | Rest] ->
      case tokenize_comment(Rest, [$#]) of
        {error, Char} ->
          {error, error_comment(Char, [$# | Rest], Line, Column, Scope, [])};
        {CommentRest, Comment} ->
          preserve_comments(Line, Column, [], Comment, CommentRest, Scope),
          % Skip the comment and continue tokenizing
          tokenize_single_item(CommentRest, Line, Column, Scope)
      end;
    
    % Handle strings
    [$" | Rest] ->
      case extract_string(Rest, $", Line, Column + 1, []) of
        {ok, Value, NewRest, NewLine, NewColumn} ->
          Token = {sequence_string, {Line, Column, nil}, Value},
          {ok, Token, NewRest, NewLine, NewColumn, Scope};
        {error, Reason} ->
          {error, Reason}
      end;
    
    % Handle single-quoted strings
    [$' | Rest] ->
      case extract_string(Rest, $', Line, Column + 1, []) of
        {ok, Value, NewRest, NewLine, NewColumn} ->
          Token = {sequence_chars, {Line, Column, nil}, Value},
          {ok, Token, NewRest, NewLine, NewColumn, Scope};
        {error, Reason} ->
          {error, Reason}
      end;
    
    % Handle atoms
    [$: | Rest] ->
      case Rest of
        [$" | AtomRest] ->
          case extract_quoted_atom_for_single(AtomRest, Line, Column + 2, [], Column) of
            {ok, Token, FinalRest, FinalLine, FinalColumn} ->
              {ok, Token, FinalRest, FinalLine, FinalColumn, Scope};
            {error, Reason} ->
              {error, Reason}
          end;
        _ ->
          case extract_atom(Rest, Line, Column + 1, []) of
            {ok, Value, NewRest, Length} ->
              Token = {sequence_atom, {Line, Column, nil}, list_to_atom(Value)},
              {ok, Token, NewRest, Line, Column + Length + 1, Scope};
            {error, Reason} ->
              {error, Reason}
          end
      end;
    
    % Handle numbers (including negative)
    [H | _] when ?is_digit(H) ->
      case extract_number(String, Line, Column) of
        {ok, Value, Rest, Length} ->
          Token = {sequence_number, {Line, Column, nil}, Value},
          {ok, Token, Rest, Line, Column + Length, Scope};
        {error, Reason} ->
          {error, Reason}
      end;
    
    % Handle minus sign followed by digit (negative number)
    [$-, D | _] when ?is_digit(D) ->
      case extract_number(String, Line, Column) of
        {ok, Value, Rest, Length} ->
          Token = {sequence_number, {Line, Column, nil}, Value},
          {ok, Token, Rest, Line, Column + Length, Scope};
        {error, Reason} ->
          {error, Reason}
      end;
    
    % Handle nested brackets
    [H | Rest] when H =:= $(; H =:= ${; H =:= $[ ->
      BracketType = case H of
        $( -> '()';
        ${ -> '{}';
        $[ -> '[]'
      end,
      ClosingChar = case H of
        $( -> $);
        ${ -> $};
        $[ -> $]
      end,
      NewScope = case H of
        $( -> Scope#elixir_tokenizer{sequence_depth = Scope#elixir_tokenizer.sequence_depth + 1};
        _ -> Scope
      end,
      case extract_bracket_content(Rest, ClosingChar, Line, Column + 1, NewScope, []) of
        {ok, ContentTokens, RestAfterBracket, FinalLine, FinalColumn, FinalScope} ->
          Token = {sequence_block, {Line, Column, nil}, BracketType, lists:reverse(ContentTokens)},
          {ok, Token, RestAfterBracket, FinalLine, FinalColumn, FinalScope};
        {error, Reason} ->
          {error, Reason}
      end;
    
    % Handle commas
    [$, | Rest] ->
      Token = {sequence_token, {Line, Column, nil}, ','},
      {ok, Token, Rest, Line, Column + 1, Scope};
    
    % Handle whitespace
    [H | Rest] when ?is_space(H) ->
      case H of
        $\n ->
          tokenize_single_item(Rest, Line + 1, 1, Scope);
        $\r ->
          case Rest of
            [$\n | RestAfterCRLF] ->
              tokenize_single_item(RestAfterCRLF, Line + 1, 1, Scope);
            _ ->
              tokenize_single_item(Rest, Line + 1, 1, Scope)
          end;
        _ ->
          tokenize_single_item(Rest, Line, Column + 1, Scope)
      end;
    
    % Handle sequence operator ~~
    [$~, $~ | Rest] ->
      Token = {sequence_op, {Line, Column, false}, '~~'},
      {ok, Token, Rest, Line, Column + 2, Scope};
    
    % Handle sequence tokens
    _ ->
      case extract_sequence_token(String, []) of
        {ok, Value, Rest, Length} ->
          Atom = list_to_atom(Value),
          Token = {sequence_token, {Line, Column, nil}, Atom},
          {ok, Token, Rest, Line, Column + Length, Scope};
        {error, Reason} ->
          {error, Reason}
      end
  end.

%% Helper function to extract quoted atom for single item tokenization
extract_quoted_atom_for_single(String, Line, Column, Acc, StartColumn) ->
  case String of
    [] ->
      {error, {?LOC(Line, Column), "missing closing quote for atom", []}};
    [$" | Rest] ->
      AtomName = lists:reverse(Acc),
      Token = {sequence_atom, {Line, StartColumn, nil}, list_to_atom(AtomName)},
      {ok, Token, Rest, Line, Column + 1};
    [$\\ | [C | Rest]] ->
      extract_quoted_atom_for_single(Rest, Line, Column + 2, [C | Acc], StartColumn);
    [$\n | Rest] ->
      extract_quoted_atom_for_single(Rest, Line + 1, 1, [$\n | Acc], StartColumn);
    [C | Rest] ->
      extract_quoted_atom_for_single(Rest, Line, Column + 1, [C | Acc], StartColumn)
  end.

%% Helper functions
previous_was_eol([]) -> false;
previous_was_eol([{_, {_, _, EOL}, _} | _]) when is_boolean(EOL) -> EOL;
previous_was_eol([{_, {_, _, _}} | _]) -> false;
previous_was_eol([{_, _} | _]) -> false;
previous_was_eol(_) -> false.

eol(_Line, _Column, [{',', {Line, Column, Count}} | Tokens]) ->
  [{',', {Line, Column, Count + 1}} | Tokens];
eol(_Line, _Column, [{';', {Line, Column, Count}} | Tokens]) ->
  [{';', {Line, Column, Count + 1}} | Tokens];
eol(_Line, _Column, [{eol, {Line, Column, Count}} | Tokens]) ->
  [{eol, {Line, Column, Count + 1}} | Tokens];
eol(Line, Column, Tokens) ->
  [{eol, {Line, Column, 1}} | Tokens].

reset_eol([{eol, {Line, Column, _}} | Rest]) -> [{eol, {Line, Column, 0}} | Rest];
reset_eol(Rest) -> Rest.

%% Comments handling - adapted from main tokenizer
tokenize_comment("\r\n" ++ _ = Rest, Acc) ->
  {Rest, lists:reverse(Acc)};
tokenize_comment("\n" ++ _ = Rest, Acc) ->
  {Rest, lists:reverse(Acc)};
tokenize_comment([H | _Rest], _) when ?bidi(H) ->
  {error, H};
tokenize_comment([H | Rest], Acc) ->
  tokenize_comment(Rest, [H | Acc]);
tokenize_comment([], Acc) ->
  {[], lists:reverse(Acc)}.

error_comment(H, Comment, Line, Column, Scope, Tokens) ->
  Token = io_lib:format("\\u~4.16.0B", [H]),
  Reason = {?LOC(Line, Column), "invalid bidirectional formatting character in comment: ", Token},
  {error, Reason, Comment, Scope, Tokens}.

preserve_comments(Line, Column, Tokens, Comment, Rest, Scope) ->
  case Scope#elixir_tokenizer.preserve_comments of
    Fun when is_function(Fun) ->
      Fun(Line, Column, Tokens, Comment, Rest);
    _ ->
      ok
  end.