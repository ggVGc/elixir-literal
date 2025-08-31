# Plan: Adding Sequence Syntax Support to Elixir

## Problem Statement

Currently, Elixir supports function calls like `foo 123` but not `foo 123 567`. The goal is to enhance the parsing and tokenization so that sequences of identifiers like the second case become a new kind of syntax form that can be handled when writing macros, instead of erroring.

## Analysis of Current Behavior

### Why `foo 123` works but `foo 123 567` doesn't

From analyzing the parser grammar in `elixir_parser.yrl`:

1. **`foo 123` works** because it matches the `no_parens_one_expr` rule:
   - `dot_identifier call_args_no_parens_one` (line 258-259)
   - This allows exactly one argument without parentheses

2. **`foo 123 567` fails** because it would need to match `no_parens_many_expr`:
   - `dot_identifier call_args_no_parens_many_strict` (line 255-256)
   - But `call_args_no_parens_many` requires comma separation (line 520-522)
   - Space-separated arguments without commas are not currently supported

## Comprehensive Implementation Plan

### 1. Parser Grammar Changes (`lib/elixir/src/elixir_parser.yrl`)

#### A. Add New Nonterminals
```erlang
% Add to nonterminals section (around line 11-37)
sequence_expr sequence_args sequence_arg_list
```

#### B. Add New Grammar Rules
```erlang
% New sequence expression rules
sequence_expr -> dot_identifier sequence_args : build_sequence('$1', '$2').
sequence_expr -> dot_op_identifier sequence_args : build_sequence('$1', '$2').

% Sequence arguments (space-separated, no commas)
sequence_args -> sequence_arg_list : '$1'.

sequence_arg_list -> matched_expr matched_expr : ['$1', '$2'].
sequence_arg_list -> sequence_arg_list matched_expr : ['$2' | '$1'].
```

#### C. Integration with Expression Hierarchy
```erlang
% Add sequence_expr to existing expression rules
% Insert around line 113 where other expressions are defined
expr -> sequence_expr : '$1'.

% Add to no_parens expressions (around line 179)
no_parens_expr -> sequence_expr : '$1'.

% Add to matched expressions for precedence
matched_expr -> sequence_expr : '$1'.
```

#### D. Add Builder Function
```erlang
% Add around line 930 with other build functions
build_sequence(Expr, Args) ->
  % Create AST node: {:sequence_call, meta, [name | reversed_args]}
  {Name, Meta, _} = build_identifier(Expr),
  {sequence_call, Meta, [Name | lists:reverse(Args)]}.
```

### 2. AST Representation Changes

#### New AST Node Type
- **Format**: `{:sequence_call, metadata, [function_name | arguments]}`
- **Example**: `foo 123 567` becomes `{:sequence_call, [line: 1], [:foo, 123, 567]}`
- **Distinction**: Different from regular call `{:foo, [line: 1], [123, 567]}` to allow macro pattern matching

### 3. Expansion Phase Changes (`lib/elixir/src/elixir_expand.erl`)

#### Add New Expansion Clause
```erlang
% Add new clause to expand/3 function (around line 12-100)
expand({sequence_call, Meta, [Name | Args]}, S, E) ->
  % Expand arguments first
  {EArgs, SA, EA} = expand_args(Args, S, E),
  % Dispatch as sequence call
  elixir_dispatch:dispatch_sequence(Meta, Name, EArgs, SA, EA, 
    fun(Result) -> {Result, SA, EA} end);
```

### 4. Dispatch System Changes (`lib/elixir/src/elixir_dispatch.erl`)

#### Add Sequence Dispatch Function
```erlang
% Add new export
-export([dispatch_sequence/6]).

% Implement sequence dispatch
dispatch_sequence(Meta, Name, Args, S, E, Callback) ->
  Arity = length(Args),
  case expand_import(Meta, Name, Arity, E, [], false, true) of
    {macro, Receiver, Expander} ->
      % Convert to sequence call format for macro
      SequenceArgs = [sequence_call | Args],
      Caller = {?line(Meta), S, E},
      expand_quoted(Meta, Receiver, Name, Arity+1, 
                   Expander(SequenceArgs, Caller), S, E);
    {function, _Receiver, _NewName} ->
      % Sequences not supported for regular functions
      elixir_errors:file_error(Meta, E, ?MODULE, 
        {sequence_call_not_macro, Name, Arity});
    not_found ->
      elixir_errors:file_error(Meta, E, ?MODULE, 
        {undefined_sequence, Name, Arity});
    Error ->
      elixir_errors:file_error(Meta, E, ?MODULE, 
        {import, Error, Name, Arity})
  end.
```

### 5. Error Handling Enhancements

#### Update Error Messages (`lib/elixir/src/elixir_dispatch.erl`)
```erlang
% Add to format_error/1 function
format_error({sequence_call_not_macro, Name, Arity}) ->
  io_lib:format("sequence call ~ts/~B can only be used with macros", 
                [Name, Arity]);
                
format_error({undefined_sequence, Name, Arity}) ->
  io_lib:format("undefined sequence macro ~ts/~B", [Name, Arity]).
```

### 6. Macro Interface Changes

#### Update Macro Module (`lib/elixir/lib/macro.ex`)

Add documentation and helper functions for sequence syntax:

```elixir
@doc """
Checks if the given AST represents a sequence call.

## Examples

    iex> Macro.sequence_call?({:sequence_call, [], [:foo, 1, 2]})
    true
    
    iex> Macro.sequence_call?({:foo, [], [1, 2]})
    false
"""
def sequence_call?({:sequence_call, _, [_name | _args]}), do: true
def sequence_call?(_), do: false

@doc """
Extracts name and arguments from a sequence call.
"""
def sequence_call_name_args({:sequence_call, _, [name | args]}) do
  {name, args}
end
```

### 7. Precedence and Conflict Resolution

#### Grammar Precedence Rules
- **Ensure sequences have lower precedence** than operators to avoid conflicts
- **Higher precedence than block expressions** to allow `foo 123 567 do; end`
- **Careful ordering** in grammar rules to prefer existing syntax where applicable

#### Conflict Avoidance
```erlang
% Ensure sequence only triggers when regular parsing would fail
% Add guards to prevent conflicts with existing syntax
sequence_arg_list -> matched_expr matched_expr : 
  case is_ambiguous('$1', '$2') of
    true -> error_ambiguous_sequence('$1');
    false -> ['$1', '$2']
  end.
```

### 8. Tokenization Verification (`lib/elixir/src/elixir_tokenizer.erl`)

#### Confirm Current Tokenizer Handles Space-Separated Tokens
- **Review token stream generation** for `foo 123 567`
- **Verify no changes needed** to tokenization phase
- **Test edge cases** with different token types in sequences

### 9. Testing Strategy

#### Unit Tests for Parser
```elixir
# Test basic sequence parsing
assert Code.string_to_quoted("foo 123 567") == 
  {:ok, {:sequence_call, [line: 1], [:foo, 123, 567]}}

# Test precedence
assert Code.string_to_quoted("foo 123 567 + 1") == 
  {:ok, {:+, [line: 1], 
    [{:sequence_call, [line: 1], [:foo, 123, 567]}, 1]}}
```

#### Integration Tests for Macros
```elixir
defmacro test_sequence(sequence_call, arg1, arg2) do
  quote do
    {sequence_call, unquote(arg1), unquote(arg2)}
  end
end

# Should work: test_sequence 1 2
# Should expand to: {sequence_call, 1, 2}
```

#### Error Case Tests
```elixir
# Should error appropriately
test "sequence with regular function" do
  assert_raise CompileError, fn ->
    Code.eval_string("Enum.map 1 2 3")  # map is not a macro
  end
end
```

### 10. Documentation Updates

#### Language Reference
- **Add section on sequence syntax** to Elixir documentation
- **Explain macro pattern matching** with sequences
- **Provide examples** of when to use sequence calls

#### Migration Guide
- **Backward compatibility notes** (all existing code continues to work)
- **New syntax availability** and recommended usage patterns

## Implementation Phases

### Phase 1: Core Parser Changes
1. Modify `elixir_parser.yrl` grammar
2. Add builder functions
3. Test basic parsing functionality

### Phase 2: Expansion and Dispatch
1. Update `elixir_expand.erl`
2. Modify `elixir_dispatch.erl`
3. Add error handling

### Phase 3: Integration and Testing
1. Update `Macro` module helpers
2. Comprehensive test suite
3. Documentation updates

### Phase 4: Edge Cases and Polish
1. Handle complex precedence scenarios
2. Optimize performance
3. Final validation against existing codebase

## Risk Mitigation

### Backward Compatibility
- **No breaking changes** to existing syntax
- **Only additive** - new syntax where previously would error
- **Careful precedence** to avoid ambiguity

### Performance Considerations
- **Minimal parser overhead** for existing code
- **Efficient AST representation** for new syntax
- **No impact on compilation speed** for non-sequence code

### Error Recovery
- **Clear error messages** when sequence syntax used incorrectly
- **Helpful suggestions** for common mistakes
- **Graceful degradation** when sequence not applicable

## Expected Outcomes

After implementation:
1. **`foo 123 567` will parse successfully** as a sequence call
2. **Macros can pattern match** on sequence syntax differently than regular calls
3. **Error messages** will clearly indicate when sequences are used incorrectly
4. **Full backward compatibility** maintained for all existing Elixir code
5. **New metaprogramming capabilities** for DSL authors using sequence syntax

This enhancement will enable powerful new DSL patterns while maintaining Elixir's existing syntax and semantics.