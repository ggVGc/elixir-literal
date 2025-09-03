# Elixir with Sequence Literal Support - Project Summary

When running lipex examples, use './bin/elixir', not the global elixir binary.

## Overview

This is a fork of the Elixir programming language that extends the tokenizer and parser to support **sequence literals** - a new syntax feature that enables Lisp-like s-expression programming within Elixir using the `~~(...)` syntax. This allows developers to write Lisp-style code that compiles to regular Elixir AST.

## Core Extension: Sequence Literals

### Syntax
The sequence literal syntax uses `~~(...)` to delimit s-expressions:
```elixir
# Instead of writing: deflisp "(+ 1 2 3)"
# You can write: deflisp quote do ~~((+ 1 2 3)) end
```

### Implementation Details

#### Tokenizer Changes (`lib/elixir/src/elixir_tokenizer.erl`)
- Added `sequence_op` token for `~~` operator
- Tracks sequence depth to properly handle nested parentheses within sequence literals
- Preserves all tokens inside sequence literals without special interpretation

#### Parser Changes (`lib/elixir/src/elixir_parser.yrl`)
- Added `sequence_literal` AST node type
- Transforms sequence contents into special AST nodes:
  - `sequence_prefix`: For prefix operations like `(+ 1 2)` → `{:sequence_prefix, meta, [:+, 1, 2]}`
  - `sequence_paren`: For parenthesized expressions like `(defun name ...)`
  - Distinguishes operators from regular identifiers

## Lisp Implementations

### 1. Basic Lisp (`lisp.ex`)
A minimal Lisp implementation providing core functionality:
- **Arithmetic**: `+`, `-`, `*`, `/`
- **Comparison**: `=`, `<`, `>`, `<=`, `>=`
- **Logic**: `and`, `or`, `not`
- **Control Flow**: `if`, `let`
- **Functions**: `defun` for defining functions
- **Lists**: `list`, `first`, `rest`, `cons`

Example usage:
```elixir
import Lisp

# Define a function
deflisp quote do ~~((defun factorial (n) 
  (if (= n 0) 1 (* n (factorial (- n 1)))))) end

# Use it
factorial(5) # Returns 120
```

### 2. Lipex - Extended Lisp (`lipex/`)
A more comprehensive Elixir-like Lisp that closely resembles Elixir's features:

#### Features
- **Data Structures**: Maps `(% :key value)`, Tuples, Structs, Keyword lists
- **Module System**: `defmodule`, `def`, `defp`, `defmacro`
- **Pattern Matching**: Function clauses, case expressions, guards with `:when`
- **Control Flow**: `if`, `case`, `cond`, `with`
- **Functional**: Pipes `(|> ...)`, Anonymous functions `(fn ...)`, Captures `(& ...)`
- **Comprehensions**: `(for x <- list ...)`
- **Concurrency**: `spawn`, `send`, `receive`
- **Error Handling**: `try`, `rescue`, `catch`, `throw`

#### Module Structure
```
lipex/
├── lipex.ex              # Main module and macro dispatcher
├── core/                 # Core language features
│   ├── arithmetic.ex     # Math operations
│   ├── control_flow.ex   # if, case, cond, with
│   ├── data_structures.ex # Maps, tuples, lists
│   └── logic.ex          # Boolean and type checking
├── functions/            # Function-related features
├── advanced/             # Pipes, comprehensions, pattern matching
├── concurrency/          # Process operations
├── error_handling/       # Exception handling
└── strings/              # String interpolation
```

## Usage Patterns

### Current Syntax (Working)
```elixir
# Direct usage (NEW - after parser modification)
deflisp ~~((+ 1 2 3))
deflisp ~~((defun add (x y) (+ x y)))
deflipex ~~((|> [1 2 3 4]
  (map (fn (x) (* x 2)))
  (filter (fn (x) (> x 4)))))

# Using quote do ... end wrapper (still supported for compatibility)
deflisp quote do ~~((+ 1 2 3)) end
```

### Design Goals (From Documentation)
The project aims to eventually support more natural Lisp-like DSLs within Elixir, allowing macro authors to create domain-specific languages with s-expression syntax while maintaining full integration with the Elixir ecosystem.

## Key Files

- **Parser/Tokenizer**: 
  - `lib/elixir/src/elixir_tokenizer.erl` - Tokenizes `~~` operator
  - `lib/elixir/src/elixir_parser.yrl` - Parses sequence literals into AST

- **Lisp Implementations**:
  - `lisp.ex` - Basic Lisp macro implementation
  - `lipex/lipex.ex` - Extended Elixir-like Lisp

- **Documentation**:
  - `elixir_lisp_syntax_design.md` - Design document for Elixir-like Lisp syntax
  - `sequence_syntax_enhancement_plan.md` - Original plan for sequence syntax support

- **Tests**:
  - `simple_lisp_test.exs` - Basic Lisp functionality tests
  - `test_direct_syntax.exs` - Tests for different syntax approaches
  - `lipex/test/` - Comprehensive Lipex tests

## Recent Development Progress

Based on commit history:
1. **Tokenizer improvements** - Fixed handling of operators, brackets, and braces within sequence literals
2. **Quote handling** - Resolved quoting issues for proper AST generation
3. **Operator support** - Extended support for various operators (`%`, `&`, etc.)
4. **Lipex implementation** - Built comprehensive Elixir-like Lisp on top of sequence literals
5. **Demo fixes** - Recent work on fixing the Lipex demo implementation
6. **Parser enhancement** - Modified parser to allow sequence literals as matched expressions, enabling direct `~~()` usage without `quote do ... end` wrapper

## Current Status

The sequence literal support is fully functional and allows writing Lisp-style code within Elixir. After the recent parser modification, sequence literals can now be used directly with macros like `deflisp ~~((+ 1 2))` without needing the `quote do ... end` wrapper. The Lipex library demonstrates the potential for creating sophisticated DSLs using this foundation.

The project extends the `extend-tokenization` branch and successfully integrates s-expression parsing into Elixir's compilation pipeline, enabling new metaprogramming capabilities while maintaining backward compatibility with existing Elixir code.
