# Sequence Literal Isolated Tokenization Implementation Plan

## Overview

This document outlines the complete implementation plan for truly isolated sequence literal tokenization, where ALL content within `~~(...)` uses separate tokenization logic that never calls normal Elixir tokenization functions.

## Core Architecture

### Current State
- Partial isolation: only identifiers and some operators get special treatment
- Most tokens (strings, numbers, atoms, etc.) fall through to normal Elixir tokenization
- Mixed token types within sequence literals

### Target State  
- Complete isolation: ALL tokens within `~~(...)` use sequence literal tokenization
- Consistent `sequence_*` token types for all content
- No fallback to normal Elixir tokenization when `sequence_depth > 0`

## Implementation Strategy

### Phase 1: Restructure Main Tokenization Entry Point

**File**: `lib/elixir/src/elixir_tokenizer.erl`

**Current Pattern** (around line 178+):
```erlang
tokenize(String, Line, Column, Scope, Tokens) ->
  % Large pattern matching with scattered sequence literal logic
  case String of
    [$# | _] -> % comments
    [$" | _] -> % strings  
    [H | _] when ?is_digit(H) -> % numbers
    % ... many more patterns
    % Only identifiers check sequence_depth
  end.
```

**New Pattern**:
```erlang
tokenize(String, Line, Column, Scope, Tokens) ->
  case Scope#elixir_tokenizer.sequence_depth > 0 of
    true ->
      sequence_literal_tokenize(String, Line, Column, Scope, Tokens);
    false ->
      normal_elixir_tokenize(String, Line, Column, Scope, Tokens)
  end.
```

### Phase 2: Extract Normal Tokenization

**Create**: `normal_elixir_tokenize/5`
- Move ALL existing tokenization patterns from main `tokenize/5` 
- Keep exact same behavior for backward compatibility
- This becomes the "normal Elixir" tokenization path

### Phase 3: Implement Sequence Literal Tokenizer

**Create**: `sequence_literal_tokenize/5`
- Handle ALL token types with sequence literal rules
- Never call normal tokenization functions
- Produce consistent `sequence_*` token types

#### Token Type Mapping

| Content | Normal Token | Sequence Token |
|---------|-------------|----------------|
| `"hello"` | `{bin_string, Meta, ["hello"]}` | `{sequence_string, Meta, "hello"}` |
| `'hello'` | `{list_string, Meta, [104,101,108,108,111]}` | `{sequence_string, Meta, "hello"}` |
| `42` | `{int, Meta, 42}` | `{sequence_number, Meta, 42}` |
| `3.14` | `{flt, Meta, 3.14}` | `{sequence_number, Meta, 3.14}` |
| `:atom` | `{atom, Meta, atom}` | `{sequence_atom, Meta, atom}` |
| `IO.puts` | `{alias, ...}, {'.', ...}, {identifier, ...}` | `{sequence_identifier, Meta, 'IO.puts'}` |
| `+` | `{dual_op, Meta, '+'}` | `{sequence_operator, Meta, '+'}` |
| `==` | `{comp_op, Meta, '=='}` | `{sequence_operator, Meta, '=='}` |
| `if` | `{'if', Meta}` | `{sequence_keyword, Meta, 'if'}` |
| `true` | `{'true', Meta}` | `{sequence_keyword, Meta, 'true'}` |

#### Sequence Literal Syntax Rules

1. **Identifiers**: Allow dots (`.`) as part of identifier
   - `IO.puts` → single `{sequence_identifier, Meta, 'IO.puts'}` token
   - `Enum.map.filter` → single `{sequence_identifier, Meta, 'Enum.map.filter'}` token

2. **Strings**: Simplified string handling
   - Both `"..."` and `'...'` → `{sequence_string, Meta, StringValue}`
   - Custom escape sequences allowed
   - No interpolation by default

3. **Numbers**: Unified number handling
   - Integers and floats → `{sequence_number, Meta, NumberValue}`

4. **Operators**: All operators become `sequence_operator`
   - Allows custom operator precedence/associativity in parser

5. **Keywords**: All keywords become `sequence_keyword`
   - Allows custom keyword handling in parser

### Phase 4: Sequence Depth Management

**Entry Detection**: `~~(`
- Tokenize `~~` as `{sequence_op, Meta, '~~'}`  
- When next token is `(`, increment `sequence_depth`
- Switch to sequence literal mode

**Exit Detection**: `)` at `sequence_depth > 0`
- Decrement `sequence_depth`
- If depth reaches 0, switch back to normal mode

**Nesting Support**: Handle nested `~~(~~())`
- Track depth correctly for multiple levels
- Each `~~(` increments, each matching `)` decrements

### Phase 5: Implementation Details

#### New Functions to Add

```erlang
%% Main dispatch
tokenize(String, Line, Column, Scope, Tokens) -> ...

%% Normal Elixir tokenization (existing logic)
normal_elixir_tokenize(String, Line, Column, Scope, Tokens) -> ...

%% Sequence literal tokenization (new)
sequence_literal_tokenize(String, Line, Column, Scope, Tokens) -> ...

%% Sequence literal token type handlers
sequence_tokenize_string(String, Line, Column, Scope, Tokens) -> ...
sequence_tokenize_number(String, Line, Column, Scope, Tokens) -> ...
sequence_tokenize_atom(String, Line, Column, Scope, Tokens) -> ...
sequence_tokenize_identifier(String, Line, Column, Scope, Tokens) -> ...
sequence_tokenize_operator(String, Line, Column, Scope, Tokens) -> ...
```

#### Pattern Matching Structure

```erlang
sequence_literal_tokenize(String, Line, Column, Scope, Tokens) ->
  case String of
    % Handle closing parenthesis first (exit condition)
    [$) | Rest] when Scope#elixir_tokenizer.sequence_depth > 0 ->
      handle_sequence_close(Rest, Line, Column, Scope, Tokens);
    
    % Sequence literal patterns
    [$" | Rest] -> sequence_tokenize_string(Rest, Line, Column + 1, $", Scope, Tokens);
    [$' | Rest] -> sequence_tokenize_string(Rest, Line, Column + 1, $', Scope, Tokens);
    [H | Rest] when ?is_digit(H) -> sequence_tokenize_number(String, Line, Column, Scope, Tokens);
    [$: | Rest] -> sequence_tokenize_atom(Rest, Line, Column + 1, Scope, Tokens);
    [H | Rest] when ?is_upcase(H); ?is_downcase(H); H =:= $_ -> 
      sequence_tokenize_identifier(String, Line, Column, Scope, Tokens);
    
    % Operators
    [$+, $+ | Rest] -> sequence_tokenize_operator("++", Rest, Line, Column + 2, Scope, Tokens);
    [$+ | Rest] -> sequence_tokenize_operator("+", Rest, Line, Column + 1, Scope, Tokens);
    % ... all other operators
    
    % Whitespace and other
    [H | Rest] when ?is_space(H) -> skip_whitespace_and_continue(...);
    
    % End of input
    [] -> {ok, Line, Column, [], Tokens, []}
  end.
```

## Testing Strategy

### Phase 1: Update Test Expectations
- Modify `lib/elixir/test/erlang/sequence_literal_test.erl`
- Expect `sequence_*` token types for all content within `~~(...)`
- Remove fallback checks for normal token types

### Phase 2: Comprehensive Test Coverage
- Test all token types in isolation
- Test mixed content within sequence literals  
- Test nested structures
- Test edge cases and error conditions
- Test sequence depth tracking

### Phase 3: Integration Tests
- Verify normal tokenization unchanged outside sequence literals
- Test mixed normal/sequence content in same source
- Test parser integration (if needed)

## Rollout Plan

1. **Implement Core Structure**: Phases 1-2 (dispatch + extract normal tokenization)
2. **Basic Sequence Tokenizer**: Phase 3 (minimal sequence literal support)
3. **Complete Token Support**: Phase 3 (all token types)  
4. **Depth Management**: Phase 4 (proper entry/exit detection)
5. **Testing & Polish**: Phase 5 (comprehensive tests + edge cases)

## Success Criteria

1. **Complete Isolation**: No normal tokenization functions called when `sequence_depth > 0`
2. **Consistent Token Types**: All sequence literal content produces `sequence_*` tokens
3. **Custom Syntax Support**: Identifiers with dots work correctly
4. **Backward Compatibility**: Normal tokenization unchanged outside sequence literals
5. **Robust Nesting**: Nested sequence literals and mixed content handled correctly

## Files Modified

- `lib/elixir/src/elixir_tokenizer.erl` - Main implementation
- `lib/elixir/src/elixir.hrl` - Token type definitions (if needed)
- `lib/elixir/unicode/tokenizer.ex` - Sequence literal identifier tokenization  
- `lib/elixir/test/erlang/sequence_literal_test.erl` - Updated tests