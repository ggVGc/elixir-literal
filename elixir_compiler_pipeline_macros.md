# Elixir Compiler Pipeline: Deep Dive into Macros

## Overview

The Elixir compiler transforms source code through several well-defined stages, with macros playing a central role in the language's metaprogramming capabilities. This document provides an in-depth analysis of the compiler pipeline, focusing specifically on how macros are implemented and processed.

## Compiler Pipeline Architecture

The Elixir compiler consists of these main stages:

1. **Tokenization** (`elixir_tokenizer.erl`) - Converts source text into tokens
2. **Parsing** (`elixir_parser.yrl`) - Builds Abstract Syntax Tree (AST) from tokens  
3. **Expansion** (`elixir_expand.erl`) - **Critical macro processing phase**
4. **Translation** (`elixir_erl_pass.erl`) - Converts to Erlang AST
5. **Code Generation** - Produces BEAM bytecode via Erlang compiler

## Macro System Fundamentals

### What Makes Macros Special

Macros in Elixir are **compile-time constructs** that receive and return AST. The key distinction from functions:

- **Functions** receive evaluated arguments and return values at runtime
- **Macros** receive AST representations and return transformed AST at compile time

```elixir
# Function - receives evaluated result (3)
def fun_inspect(value), do: IO.inspect(value)
fun_inspect(1 + 2)  # receives: 3

# Macro - receives AST representation
defmacro macro_inspect(value) do
  IO.inspect(value)
  value
end
macro_inspect(1 + 2)  # receives: {:+, [line: 1], [1, 2]}
```

### AST Structure

Elixir AST follows a consistent three-tuple format: `{atom, metadata, args}`

- `atom` - Function/operator name or literal value
- `metadata` - Keyword list with compilation info (line numbers, context, etc.)  
- `args` - Arguments list or atom for variables

## Core Implementation Files

### elixir_expand.erl - The Heart of Macro Processing

Located at `lib/elixir/src/elixir_expand.erl`, this module handles the expansion phase where macros are processed. Key functions:

- `expand/3` - Main expansion entry point that dispatches based on AST node types
- `expand_args/3` - Recursively expands argument lists
- Handles all language constructs: assignments, blocks, aliases, imports, etc.

**Critical Macro Processing Logic**: When encountering function calls, `expand/3` delegates to `elixir_dispatch` to determine if it's a macro that needs expansion.

### elixir_dispatch.erl - Macro Resolution and Execution

Located at `lib/elixir/src/elixir_dispatch.erl`, this module:

**Macro Discovery Process** (lines 108-135):
```erlang
dispatch_import(Meta, Name, Args, S, E, Callback) ->
  case expand_import(Meta, Name, Arity, E, [], AllowLocals, true) of
    {macro, Receiver, Expander} ->
      % Execute macro and expand result
      expand_quoted(Meta, Receiver, Name, Arity, 
                   Expander(stop_generated(Args), Caller), S, E);
    {function, Receiver, NewName} ->
      % Handle as regular function call
      Callback({Receiver, NewName})
  end.
```

**Macro Execution Flow** (lines 261-283):
1. **Preparation**: Macro arguments are processed with `stop_generated/1` to prevent infinite expansion
2. **Invocation**: `expand_macro_fun/6` calls the macro with caller environment + arguments  
3. **Result Processing**: `expand_quoted/6` takes macro output and recursively expands it
4. **Context Management**: Line numbers and hygiene information are properly maintained

### elixir_quote.erl - AST Manipulation

Located at `lib/elixir/src/elixir_quote.erl`, handles:

**Quote/Unquote Processing**:
- `has_unquotes/1` - Detects unquote operations in AST
- `linify/3` - Applies line number information from call site
- Context counter management for hygiene

**Key Insight**: The `linify_with_context_counter/3` function (lines 93-100) ensures macro-generated code maintains proper source location information and variable scoping.

### elixir_def.erl - Function and Macro Definitions

Located at `lib/elixir/src/elixir_def.erl`, manages:

- Storage of `def`, `defp`, `defmacro`, `defmacrop` definitions
- Local macro resolution via `local_for/5` (lines 24-28)
- Macro vs function dispatch logic (line 44: `Kind == defmacrop`)

## Detailed Macro Processing Flow

### 1. Macro Definition Storage

When the compiler encounters `defmacro`, it:

1. **Parses** the definition into AST
2. **Stores** in module's ETS tables via `elixir_def:store_definition/9`
3. **Marks** as macro kind (`defmacro` or `defmacrop`) for later dispatch

### 2. Macro Call Recognition

During expansion (`elixir_expand.erl:expand/3`):

1. **Function call encountered**: `{name, meta, args}` 
2. **Dispatch to resolution**: `elixir_dispatch:dispatch_import/6`
3. **Lookup process**: Check imports, requires, and local definitions
4. **Kind determination**: Function vs macro based on definition type

### 3. Macro Execution

When a macro is identified (`elixir_dispatch.erl:expand_macro_fun/6`):

```erlang
expand_macro_fun(Meta, Fun, Receiver, Name, Args, Caller, E) ->
  try
    apply(Fun, [Caller | Args])  % Execute macro with caller context
  catch
    Kind:Reason:Stacktrace ->
      % Proper error reporting with macro context
      MFA = {Receiver, elixir_utils:macro_name(Name), Arity+1},
      Info = [{Receiver, Name, Arity, [{file, "expanding macro"}]}, 
              caller(?line(Meta), E)],
      erlang:raise(Kind, Reason, prune_stacktrace(Stacktrace, MFA, Info, {ok, Caller}))
  end.
```

**Critical Details**:
- Macro receives caller environment as first argument (the `__CALLER__` special form)
- Proper error context maintained for debugging
- Exception handling preserves macro call site information

### 4. Result Processing

The macro's returned AST goes through `expand_quoted/6`:

1. **Context application**: `elixir_quote:linify_with_context_counter/3` applies proper line numbers and hygiene
2. **Recursive expansion**: Result AST is fed back into `elixir_expand:expand/3`  
3. **Further macro processing**: If result contains more macro calls, process repeats

## Hygiene System

Elixir's macro hygiene prevents variable name collisions between macro-generated code and call site:

### Context Counter Mechanism

Each macro expansion gets a unique context identifier via `elixir_module:next_counter/1`:

```erlang
expand_quoted(Meta, Receiver, Name, Arity, Quoted, S, E) ->
  Next = elixir_module:next_counter(?key(E, module)),
  ToExpand = elixir_quote:linify_with_context_counter(Meta, {Receiver, Next}, Quoted),
  elixir_expand:expand(ToExpand, S, E)
```

This ensures variables in macro-generated code have unique contexts, preventing accidental capture.

### Variable Scoping

Variables in macros maintain their definition context:
- **Macro-defined variables**: Get macro's context, isolated from call site  
- **Unquoted variables**: Inherit call site context via `unquote/1`
- **Pin operator** (`^`): Explicitly references call-site variables

## Quote and Unquote Deep Dive

### Quote Mechanism

`quote` transforms code into its AST representation:

```elixir
quote do: 1 + 2
# Returns: {:+, [line: 1], [1, 2]}
```

**Implementation** (`elixir_quote.erl`):
- Recursively processes expressions
- Applies metadata (line numbers, context)
- Handles special forms and operators

### Unquote Operations

`unquote` and `unquote_splicing` inject runtime values into quoted expressions:

- `unquote(expr)` - Injects single expression
- `unquote_splicing(list)` - Injects list elements as separate arguments

**Detection Logic** (`has_unquotes/1`):
```erlang
has_unquotes({quote, _, [Child]}, QuoteLevel) ->
  has_unquotes(Child, QuoteLevel + 1);
has_unquotes({Unquote, _, [Child]}, QuoteLevel)
  when Unquote == unquote; Unquote == unquote_splicing ->
  case QuoteLevel of
    0 -> true;  % Active unquote
    _ -> has_unquotes(Child, QuoteLevel - 1)  % Nested quote
  end;
```

## Integration with Module System

### Module Compilation Process

Macros integrate deeply with Elixir's module system (`elixir_module.erl`):

1. **Module Definition**: `defmodule` creates ETS tables for storing definitions
2. **Definition Storage**: Each `def`/`defmacro` stored with metadata
3. **Compile-time Access**: Macros can inspect and modify module state
4. **Finalization**: Module compilation resolves all macro expansions

### Bootstrapping Considerations

The compiler itself uses macros (`elixir_compiler.erl:bootstrap/0`), requiring careful bootstrap order:

```erlang
bootstrap_files() ->
  {
    [ % First pass - core infrastructure
     <<"kernel/utils.ex">>,
     <<"macro/env.ex">>,
     <<"macro.ex">>,
     % ...
    ],
    [ % Second pass - full functionality
     <<"exception.ex">>,
     <<"module/types.ex">>,
     % ...
    ]
  }.
```

## Performance Implications

### Compile-time vs Runtime Costs

- **Macro expansion cost**: Paid during compilation
- **Generated code efficiency**: Can be optimized by macro authors
- **Compilation time**: Heavy macro usage increases compile times

### Caching and Optimization

- **Definition caching**: Module definitions cached in ETS tables
- **Code generation**: `elixir_compiler.erl` can fast-compile simple modules
- **Beam optimization**: Final Erlang AST benefits from BEAM compiler optimizations

## Error Handling and Debugging

### Macro Error Context

The system maintains detailed error context through macro expansions:

```erlang
Info = [{Receiver, Name, Arity, [{file, "expanding macro"}]}, 
        caller(?line(Meta), E)]
```

### Stack Trace Management

`prune_stacktrace/4` ensures error messages point to relevant code locations, not internal compiler details.

## Practical Examples

### Simple Macro Definition

```elixir
defmacro unless(condition, do: block) do
  quote do
    if not unquote(condition), do: unquote(block)
  end
end
```

**Expansion process**:
1. `unless(true, do: IO.puts("hello"))` encountered
2. Macro receives AST: `[true, [do: {{:., [], [{:__aliases__, [], [:IO]}, :puts]}, [], ["hello"]}]]`
3. Returns: `{:if, [], [{{:not, [], [true]}, [do: {{:., [], [...]], [], ["hello"]}]}`
4. Result expanded recursively

### Advanced: Code Generation

```elixir
defmacro create_functions(names) do
  for name <- names do
    quote do
      def unquote(name)(), do: unquote(to_string(name))
    end
  end
end
```

This demonstrates:
- **Compile-time iteration** over macro arguments
- **Dynamic function generation** using `unquote`
- **AST construction** with `quote`

## Conclusion

Elixir's macro system represents a sophisticated compile-time metaprogramming facility built on:

1. **AST-based processing** throughout the compilation pipeline
2. **Careful hygiene management** preventing variable capture
3. **Integrated dispatch system** seamlessly handling macros and functions  
4. **Robust error handling** maintaining developer debugging experience
5. **Performance-conscious design** moving complexity to compile time

The implementation in `elixir_expand.erl`, `elixir_dispatch.erl`, and `elixir_quote.erl` demonstrates how a relatively small amount of carefully designed code can enable powerful metaprogramming capabilities while maintaining language predictability and performance.

Understanding these internals enables developers to write more effective macros and debug macro-related issues more efficiently, while appreciating the elegant engineering behind one of Elixir's most distinctive features.