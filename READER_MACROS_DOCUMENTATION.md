# Reader Macros for Elixir

## Overview

This document describes a reader macro system added to the Elixir programming language. Reader macros allow developers to define custom syntax that is transformed into regular Elixir code before compilation, enabling the creation of domain-specific languages (DSLs) and syntactic sugar.

## What are Reader Macros?

Reader macros are special macros that operate on raw source code text before it's parsed into an Abstract Syntax Tree (AST). They allow you to define custom syntax patterns that are replaced with standard Elixir code during the compilation process.

Unlike regular Elixir macros that work on ASTs, reader macros work at the character/string level, making them more powerful for creating entirely new syntax.

## Implementation

### Core Components

1. **Reader Macro Infrastructure** (`elixir_reader_macros.erl`)
   - Storage and retrieval of reader macro definitions
   - Pattern matching and expansion logic
   - Integration with module compilation

2. **Preprocessor** (`reader_macro_processor.exs`) 
   - Standalone tool for processing files with reader macros
   - Regex-based pattern extraction and replacement
   - Handles escaped quotes and complex patterns

3. **Language Integration**
   - New `defreadermacro` form in Kernel
   - ETS-based storage for reader macro definitions
   - Compilation pipeline integration

### Syntax

Reader macros are defined using the `defreadermacro` form:

```elixir
defreadermacro name("pattern") do "replacement" end
```

- **name**: Identifier for the reader macro
- **pattern**: String literal that will be matched in source code
- **replacement**: String literal that will replace the pattern

## Usage Examples

### Basic Example

```elixir
defmodule SimpleExample do
  defreadermacro hello("HELLO") do "\"Hello, World!\"" end
  
  def greet do
    IO.puts(HELLO)  # Becomes: IO.puts("Hello, World!")
  end
end
```

### Mathematical DSL

```elixir
defmodule MathDSL do
  defreadermacro add("(+ 1 2)") do "Enum.sum([1, 2])" end
  defreadermacro multiply("(* 3 4)") do "(3 * 4)" end
  defreadermacro power("(^ 2 3)") do ":math.pow(2, 3)" end
  
  def calculate do
    result1 = (+ 1 2)    # Becomes: Enum.sum([1, 2])
    result2 = (* 3 4)    # Becomes: (3 * 4)
    result3 = (^ 2 3)    # Becomes: :math.pow(2, 3)
    {result1, result2, result3}
  end
end
```

### Custom Operators

```elixir
defmodule CustomOperators do
  defreadermacro pipe_sum("PIPE_SUM") do "[1, 2, 3] |> Enum.sum()" end
  defreadermacro safe_access("SAFE_ACCESS") do "Map.get(data, :key)" end
  
  def examples do
    sum = PIPE_SUM        # Becomes: [1, 2, 3] |> Enum.sum()
    value = SAFE_ACCESS   # Becomes: Map.get(data, :key)
    {sum, value}
  end
end
```

### Logging DSL

```elixir
defmodule LoggingDSL do
  defreadermacro log_info("LOG_INFO") do "IO.puts(\"[INFO] Processing...\")" end
  defreadermacro log_warn("LOG_WARN") do "IO.puts(\"[WARN] Warning occurred\")" end
  defreadermacro timestamp("TIMESTAMP") do "System.system_time(:second)" end
  
  def process do
    LOG_INFO    # Becomes: IO.puts("[INFO] Processing...")
    LOG_WARN    # Becomes: IO.puts("[WARN] Warning occurred")
    TIMESTAMP   # Becomes: System.system_time(:second)
  end
end
```

## How to Use

### Step 1: Install the Reader Macro Processor

The reader macro system requires a preprocessing step. Use the `reader_macro_processor.exs` script:

```bash
# Make the processor executable
chmod +x reader_macro_processor.exs

# Process a file with reader macros
./reader_macro_processor.exs myfile.exs > processed_file.exs

# Run the processed file
elixir processed_file.exs
```

### Step 2: Write Code with Reader Macros

Create an Elixir file with reader macro definitions and usage:

```elixir
defmodule MyModule do
  # Define reader macros
  defreadermacro greet("GREET") do "\"Hello from reader macro!\"" end
  defreadermacro add_nums("ADD(5, 10)") do "(5 + 10)" end
  
  # Use the reader macros
  def test do
    message = GREET
    sum = ADD(5, 10)
    {message, sum}
  end
end

MyModule.test()
```

### Step 3: Process and Run

```bash
# Process the file
elixir reader_macro_processor.exs myfile.exs > processed.exs

# Run the processed file  
elixir processed.exs
```

## Implementation Details

### Pattern Matching

The current implementation uses regex-based pattern matching:

```erlang
regex = ~r/defreadermacro\s+(\w+)\("([^"]+)"\)\s*do\s*"((?:[^"\\]|\\.)*)"\s*end/ms
```

This regex captures:
- Reader macro name
- Pattern to match
- Replacement string
- Handles escaped quotes in replacement strings

### Processing Pipeline

1. **Extract**: Find all `defreadermacro` definitions in source code
2. **Clean**: Remove reader macro definitions from source
3. **Transform**: Replace all pattern occurrences with their replacements
4. **Output**: Generate processed Elixir code

### Limitations

1. **Simple Patterns Only**: Current implementation only supports literal string patterns
2. **No Parameterization**: Cannot capture and use variables from patterns  
3. **Preprocessing Required**: Must use external preprocessor tool
4. **No Recursive Macros**: Reader macros cannot call other reader macros

## Benefits

### Compile-Time Transformation
- Zero runtime overhead
- All transformations happen during preprocessing
- Final code is standard Elixir

### Domain-Specific Languages
- Create custom syntax for specific domains
- Mathematical notation, SQL-like queries, logging DSLs
- Improved code readability and expressiveness

### Syntactic Sugar
- Simplify complex or repetitive patterns
- Create shortcuts for common operations
- Maintain compatibility with existing Elixir code

## Advanced Example

Here's a comprehensive example showing various reader macro patterns:

```elixir
defmodule ComprehensiveExample do
  # Mathematical operations
  defreadermacro add_numbers("(+ 10 20)") do "(10 + 20)" end
  defreadermacro multiply_numbers("(* 6 7)") do "(6 * 7)" end
  
  # String operations
  defreadermacro shout_hello("SHOUT_HELLO") do "String.upcase(\"hello world\")" end
  defreadermacro format_name("FORMAT_NAME") do "\"Name: #{name}\"" end
  
  # List operations  
  defreadermacro sum_list("SUM_LIST") do "Enum.sum([1, 2, 3, 4, 5])" end
  defreadermacro reverse_list("REVERSE_LIST") do "Enum.reverse([1, 2, 3])" end
  
  # Control flow
  defreadermacro check_condition("CHECK(x > 5)") do "if x > 5, do: \"greater\", else: \"smaller\"" end
  
  def demonstrate do
    # Mathematical DSL
    sum = (+ 10 20)           # 30
    product = (* 6 7)         # 42
    
    # String operations
    loud = SHOUT_HELLO        # "HELLO WORLD"
    name = "Alice"
    formatted = FORMAT_NAME   # "Name: Alice"
    
    # List operations
    total = SUM_LIST          # 15
    reversed = REVERSE_LIST   # [3, 2, 1]
    
    # Control flow
    x = 10
    result = CHECK(x > 5)     # "greater"
    
    {sum, product, loud, formatted, total, reversed, result}
  end
end
```

## Testing

### Unit Tests

Test individual reader macros:

```elixir
defmodule ReaderMacroTest do
  defreadermacro test_macro("TEST") do "42" end
  
  def test do
    result = TEST
    assert result == 42
  end
end
```

### Integration Tests

Test complex combinations:

```elixir
defmodule IntegrationTest do
  defreadermacro math_add("(+ a b)") do "(a + b)" end
  defreadermacro math_mul("(* a b)") do "(a * b)" end
  
  def complex_calculation do
    a = 5
    b = 10
    result = (* a b) + (+ a b)  # (a * b) + (a + b) = 50 + 15 = 65
    assert result == 65
  end
end
```

## Future Enhancements

### Parameterized Patterns
Enable capture groups in patterns:

```elixir
defreadermacro add("(+ $a $b)") do "($a + $b)" end
```

### Recursive Macros
Allow reader macros to use other reader macros:

```elixir
defreadermacro square("SQUARE($x)") do "(MUL($x, $x))" end  
defreadermacro mul("MUL($a, $b)") do "($a * $b)" end
```

### Integration with Build Tools
Direct integration with Mix and other build tools:

```elixir
# mix.exs
def project do
  [
    compilers: [:reader_macros, :elixir, :app],
    reader_macros: [enable: true]
  ]
end
```

### IDE Support
Syntax highlighting and IntelliSense for reader macro patterns.

## Conclusion

Reader macros provide a powerful mechanism for extending Elixir's syntax and creating domain-specific languages. While the current implementation is basic, it demonstrates the potential for creating more expressive and readable code through compile-time transformations.

The system successfully enables:
- Custom mathematical notation
- Domain-specific syntax
- Syntactic sugar for common patterns
- Zero runtime overhead through compile-time transformation

This implementation serves as a foundation for more advanced reader macro systems in the future.