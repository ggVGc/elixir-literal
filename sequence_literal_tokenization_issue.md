# Sequence Literal Tokenization Issue: Incomplete Isolation

## The Problem

The sequence literal feature (`~~(...)`) was designed to provide a completely isolated lexing environment where content inside the sequence literal delimiters would be tokenized using entirely different rules from normal Elixir code. However, the current implementation only provides **partial isolation**, which fundamentally breaks the intended design.

## Current Implementation Analysis

### What IS Isolated (Working as Intended)

1. **Identifiers with Dots**: When `sequence_depth > 0`, identifiers use `tokenize_with_scope()` which allows dots within identifiers
   - Example: `~~(IO.puts)` → `IO.puts` becomes a single identifier token
   - Location: `elixir_tokenizer.erl:1404-1416`

2. **Some Operator Conversion**: Certain operators get converted to `sequence_atom` tokens
   - Handled by `create_token()` → `should_convert_to_sequence_atom()` → `is_convertible_token_type()`
   - Location: `elixir_tokenizer.erl:1532-1556`

### What is NOT Isolated (Still Uses Normal Elixir Tokenization)

1. **String Literals**: `~~("hello")` 
   - Still processed by `handle_strings()` 
   - Creates `{bin_string, ...}` tokens using normal Elixir string rules
   - Location: `elixir_tokenizer.erl:273-278, 815+`

2. **Number Literals**: `~~(42)`, `~~(3.14)`
   - Still processed by normal number tokenization
   - Creates `{int, ...}` and `{flt, ...}` tokens  
   - Location: `elixir_tokenizer.erl:185-198, 599+`

3. **Atom Literals**: `~~(:foo)`
   - Still processed by normal atom tokenization
   - Creates `{atom, ...}` tokens
   - Location: `elixir_tokenizer.erl:578+`

4. **Character Literals**: `~~(?a)`
   - Still processed by normal character tokenization  
   - Creates `{char, ...}` tokens
   - Location: `elixir_tokenizer.erl:224-249`

5. **Heredocs**: `~~(\"\"\"content\"\"\")`
   - Still processed by `handle_heredocs()`
   - Location: `elixir_tokenizer.erl:263-269`

6. **Comments**: `~~(# comment)`
   - Still processed by `tokenize_comment()`
   - Location: `elixir_tokenizer.erl:202-209`

7. **Sigils**: `~~(~s"hello")`
   - Still processed by `tokenize_sigil()`  
   - Location: `elixir_tokenizer.erl:213-214`

8. **All Other Token Types**: Base integers, version control markers, operator atoms, etc.
   - All use their normal tokenization paths

## The Architectural Problem

The sequence literal tokenization follows this flawed pattern:

```erlang
tokenize(Input, Line, Column, Scope, Tokens) when Scope#elixir_tokenizer.sequence_depth > 0 ->
  case Input of
    % Only a few specific cases are handled specially
    [H | T] when ?is_upcase(H); ?is_downcase(H); H =:= $_ ->
      % Use sequence literal tokenization
      tokenize_identifier(...);
    
    % Everything else falls through to normal Elixir tokenization
    _ ->
      % This calls ALL the normal tokenization rules:
      % - tokenize([$" | T], ...) for strings  
      % - tokenize([H | T], ...) when ?is_digit(H) for numbers
      % - tokenize([$: | String], ...) for atoms
      % - etc.
      normal_elixir_tokenization(Input, Line, Column, Scope, Tokens)
  end.
```

## What Should Happen

For true isolation, sequence literal content should NEVER call normal Elixir tokenization functions. The flow should be:

```erlang
tokenize(Input, Line, Column, Scope, Tokens) ->
  case Scope#elixir_tokenizer.sequence_depth > 0 of
    true ->
      % Use ONLY sequence literal tokenization - never call normal tokenization
      sequence_literal_tokenize(Input, Line, Column, Scope, Tokens);
    false ->
      % Use normal Elixir tokenization
      normal_elixir_tokenize(Input, Line, Column, Scope, Tokens)
  end.
```

## Consequences of the Current Issue

1. **Incomplete Feature**: Sequence literals can't define their own syntax for most token types
2. **Inconsistent Behavior**: Some tokens (identifiers) follow sequence rules, others don't  
3. **Leaky Abstraction**: Normal Elixir syntax constraints leak into sequence literals
4. **Maintenance Burden**: Adding new sequence literal token types requires modifying multiple places

## Example of the Problem

Consider: `~~("hello", 42, :atom, IO.puts)`

Current tokenization:
- `"hello"` → Uses normal string tokenization → `{bin_string, ...}`
- `42` → Uses normal number tokenization → `{int, ...}` 
- `:atom` → Uses normal atom tokenization → `{atom, ...}`
- `IO.puts` → Uses sequence literal tokenization → `{identifier, _, :'IO.puts'}`

This mixed behavior defeats the purpose of having isolated sequence literal syntax.

## The Fix Required

The tokenization entry point needs to be restructured so that when `sequence_depth > 0`, it routes ALL tokenization through a separate sequence literal lexer that never calls the existing Elixir tokenization functions.

This is a fundamental architectural change, not just a small bug fix.

## Location Summary

The main tokenization dispatch happens in the large pattern-matching function starting at `elixir_tokenizer.erl:178+`. Each pattern match (for strings, numbers, atoms, etc.) needs to check `sequence_depth` and route appropriately, or the entire dispatch needs to be restructured.

Current sequence literal logic is scattered across:
- `elixir_tokenizer.erl:407-420` (bracket depth tracking)
- `elixir_tokenizer.erl:1404-1416` (identifier tokenization)  
- `elixir_tokenizer.erl:1532-1556` (token conversion)
- `unicode/tokenizer.ex:410-436` (scope-aware identifier tokenization)