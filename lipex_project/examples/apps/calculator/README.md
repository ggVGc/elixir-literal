# Lipex Calculator CLI

Interactive command-line calculator demonstrating practical Lipex usage in a complete application.

## Features

- **Interactive REPL**: Enter Lipex expressions and see results immediately
- **Variable Storage**: Define variables and reuse them in calculations
- **Built-in Functions**: Mathematical functions defined in Lipex syntax
- **Calculation History**: Review your last 50 calculations
- **Error Handling**: Helpful error messages for invalid expressions
- **Command Support**: Special commands for navigation and help

## Usage

### Starting the Calculator

From the `lipex_project` directory:
```bash
../bin/elixir examples/apps/calculator/calculator_app.exs
```

Or from the calculator directory:
```bash
cd examples/apps/calculator
../../../bin/elixir calculator_app.exs
```

### Basic Arithmetic

```bash
> (+ 2 3)
📊 5

> (* 4 5 6)
📊 120

> (/ 15 3)
📊 5.0

> (- 100 25)
📊 75
```

### Variables

```bash
> (= x 42)
📝 x = 42

> (= y (* 2 5))
📝 y = 10

> (+ x y)
📊 52

> vars
📦 ===== STORED VARIABLES =====
  x = 42
  y = 10
=============================
```

### Built-in Functions

The calculator includes several mathematical functions defined using Lipex:

```bash
> (square 7)
📊 49

> (cube 4)
📊 64

> (abs_val -15)
📊 15

> (power 2 8)
📊 256
```

### Complex Expressions

```bash
> (+ (* 2 3) (/ 12 4))
📊 9

> (square (+ 3 2))
📊 25

> (if (> 10 5) (* 2 3) 0)
📊 6
```

### Commands

- **`history`** - View calculation history
- **`vars`** - Show stored variables
- **`clear`** - Clear all variables and history
- **`help`** - Show detailed help
- **`quit`** or **`exit`** - Exit the calculator

### Example Session

```bash
🧮 ======= LIPEX CALCULATOR =======

> (= radius 5)
📝 radius = 5

> (= pi 3.14159)
📝 pi = 3.14159

> (* pi (square radius))
📊 78.53975

> history
📚 ===== CALCULATION HISTORY =====
1. [14:32:15] (= radius 5) = 5
2. [14:32:22] (= pi 3.14159) = 3.14159
3. [14:32:35] (* pi (square radius)) = 78.53975
===============================

> quit
👋 Thanks for using Lipex Calculator!
```

## Implementation Highlights

### Lipex Function Definitions

The calculator demonstrates defining reusable functions in Lipex:

```elixir
# Mathematical functions defined using Lipex syntax
deflipex ~~((def square (x) (* x x)))
deflipex ~~((def cube (x) (* x x x)))
deflipex ~~((def power (base exp) 
  (if (== exp 0) 
    1 
    (if (== exp 1) 
      base 
      (* base (power base (- exp 1)))))))
```

### Expression Evaluation

Shows how to evaluate Lipex expressions dynamically:

```elixir
# Wrap user input in deflipex ~~(...) and evaluate
lipex_expression = "deflipex ~~(#{expression})"
{result, _binding} = Code.eval_string(code, [], __ENV__)
```

### Error Handling

Demonstrates robust error handling for invalid expressions:

```elixir
try do
  {result, _binding} = Code.eval_string(code, [], __ENV__)
  # ... process result
rescue
  error ->
    IO.puts("❌ Error: #{Exception.message(error)}")
    IO.puts("💡 Try: (+ 2 3) or (= x 5) or type 'help' for examples")
end
```

## Learning Outcomes

This application demonstrates:

1. **Interactive Lipex Usage** - Building REPL-style applications
2. **Function Definition** - Defining reusable functions with `deflipex`
3. **Dynamic Evaluation** - Evaluating Lipex expressions at runtime
4. **State Management** - Managing variables and history
5. **Error Handling** - Graceful handling of syntax and runtime errors
6. **User Experience** - Building friendly, helpful CLI interfaces

## Extending the Calculator

Ideas for enhancements:

- Add trigonometric functions (sin, cos, tan)
- Support for complex numbers
- Unit conversion capabilities
- Save/load calculator sessions
- Graphing simple functions
- Scientific notation support

The modular design makes it easy to add new mathematical functions by defining them with `deflipex` syntax.