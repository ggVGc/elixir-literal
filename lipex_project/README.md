# Lipex

**Lipex** - An Elixir-like Lisp syntax implementation using sequence literals.

Lipex provides a comprehensive Lisp-like syntax that closely resembles standard Elixir functionality, using the `~~(...)` sequence literal syntax. This design prioritizes familiarity for Elixir developers while maintaining s-expression clarity.

## Requirements

**Important**: This project requires a modified version of Elixir that includes sequence literal support. It will **not work** with the standard system Elixir installation.

You must use the modified Elixir binary located at:
```
/home/ggvgc/stuff/projects/elixir/bin/elixir
```

## Setup and Usage

### Compilation

To compile the project, use the modified Elixir binary:

```bash
cd lipex_project
/home/ggvgc/stuff/projects/elixir/bin/elixir -S mix compile
```

### Running Tests

To run tests with the modified Elixir:

```bash
/home/ggvgc/stuff/projects/elixir/bin/elixir -S mix test
```

### Interactive Shell

To start an interactive session with Lipex loaded:

```bash
/home/ggvgc/stuff/projects/elixir/bin/elixir -S iex -S mix
```

### Setting Up Aliases (Recommended)

For convenience, you can set up aliases to use the modified Elixir:

```bash
alias elixir="/home/ggvgc/stuff/projects/elixir/bin/elixir"
alias mix="/home/ggvgc/stuff/projects/elixir/bin/elixir -S mix"
```

After setting these aliases, you can use the standard commands:
```bash
mix compile
mix test
iex -S mix
```

## Sequence Literal Syntax

The sequence literal syntax uses `~~(...)` and supports the following patterns:

### Working Syntax

- **Empty sequences**: `~~()`
- **Simple identifiers**: `~~(hello world)`
- **Arithmetic with prefix notation**: `~~(+ 1 2 3)` 
- **Comparisons**: `~~(< 5 10)`
- **Brackets (lists)**: `~~([a b c])`
- **Braces (tuples/maps)**: `~~({x y})`

### Examples

```elixir
# Import Lipex in your module
import Lipex

# Basic arithmetic
result = deflipex ~~(+ 1 2 3)     # Returns 6

# Comparisons  
check = deflipex ~~(< 1 2)        # Returns true

# Data structures
my_map = deflipex ~~(% :name "John" :age 30)
my_list = deflipex ~~(list 1 2 3)

# Control flow
answer = deflipex ~~(if true :yes :no)  # Returns :yes
```

### Syntax Limitations

- **Nested parentheses don't work**: `~~((+ 1 2))` will cause a syntax error
- **Use prefix notation instead**: `~~(+ 1 2)` instead of `~~((+ 1 2))`

## Supported Features

- **Data Structures**: Maps `(% ...)`, Tuples `(tuple ...)`, Lists `(list ...)`
- **Arithmetic**: `(+ ...)`, `(- ...)`, `(* ...)`, `(/ ...)`
- **Comparisons**: `(< ...)`, `(> ...)`, `(<= ...)`, `(>= ...)`, `(== ...)`, `(!= ...)`
- **Control Flow**: `(if ...)`, `(case ...)`, `(cond ...)`
- **Logical Operations**: `(and ...)`, `(or ...)`, `(not ...)`

## Architecture

Lipex uses a modular evaluator architecture where each feature is implemented in separate modules:

- `Lipex.Core.Arithmetic` - Mathematical operations
- `Lipex.Core.Logic` - Boolean and logical operations  
- `Lipex.Core.DataStructures` - Maps, tuples, lists
- `Lipex.Core.ControlFlow` - If, case, cond expressions
- `Lipex.Functions.Calls` - Function calls and definitions

## Development

This project is part of the Elixir sequence literal implementation and serves as a demonstration of the macro capabilities enabled by the sequence literal syntax.

## License

Apache-2.0