# SimpleLisp Reader Macro

## Overview

The SimpleLisp reader macro demonstrates how reader macros can embed alternative programming languages directly into Elixir. This implementation provides a Lisp-like S-expression syntax that gets transformed into Elixir code at compile time.

## Features

### Supported Lisp Forms

#### Arithmetic Operations
```elixir
lisp!(+ 1 2 3 4)           # => Enum.sum([1, 2, 3, 4])
lisp!(- 10 3)              # => 10 - 3
lisp!(* 2 3 4)             # => Enum.reduce([2, 3, 4], &*/2)
lisp!(/ 10 2)              # => 10 / 2
```

#### Comparison Operations
```elixir
lisp!(> x 5)               # => x > 5
lisp!(< a b)               # => a < b
lisp!(= x 0)               # => x == 0
lisp!(>= score 90)         # => score >= 90
```

#### List Operations
```elixir
lisp!(list 1 2 3)          # => [1, 2, 3]
lisp!(cons 1 (list 2 3))   # => [1 | [2, 3]]
lisp!(car [1 2 3])         # => hd([1, 2, 3])
lisp!(cdr [1 2 3])         # => tl([1, 2, 3])
```

#### Control Flow
```elixir
lisp!(if (> x 0) 
       "positive" 
       "negative")          # => if x > 0, do: "positive", else: "negative"
```

#### Higher-Order Functions
```elixir
lisp!(map (fn [x] (* x 2)) [1 2 3])
# => Enum.map([1, 2, 3], fn x -> x * 2 end)

lisp!(filter (fn [x] (> x 5)) nums)
# => Enum.filter(nums, fn x -> x > 5 end)

lisp!(reduce (fn [a b] (+ a b)) 0 [1 2 3])
# => Enum.reduce([1, 2, 3], 0, fn a, b -> a + b end)
```

#### Function Definition
```elixir
lisp!(defn square [x] (* x x))
# => def square(x), do: x * x

lisp!(defn factorial [n]
       (if (<= n 1)
         1
         (* n (factorial (- n 1)))))
# => def factorial(n), do: if n <= 1, do: 1, else: n * factorial(n - 1)
```

#### Let Bindings
```elixir
lisp!(let [x 10 y 20] 
       (+ x y))
# => with x = 10, y = 20, do: x + y
```

## Implementation Details

### Architecture

1. **Tokenizer**: Breaks the S-expression into tokens
   - Parentheses: `(`, `)`
   - Brackets: `[`, `]`
   - Numbers: `123`, `45.6`
   - Strings: `"hello"`
   - Symbols: `+`, `map`, `defn`

2. **Parser**: Builds an AST from tokens
   - Lists: `{:list, [elements]}`
   - Arrays: `{:array, [elements]}`
   - Atoms: `{:symbol, name}`
   - Literals: `{:number, n}`, `{:string, s}`

3. **Transformer**: Converts Lisp AST to Elixir code
   - Pattern matches on operations
   - Generates appropriate Elixir syntax
   - Handles special forms and macros

### How It Works

1. The `lisp!` reader macro intercepts source code starting with `lisp!(`
2. It extracts the complete S-expression (balanced parentheses)
3. The expression is tokenized, parsed, and transformed
4. The resulting Elixir code string replaces the original Lisp expression
5. Normal Elixir compilation continues with the transformed code

## Usage Examples

### Basic Usage

```elixir
defmodule MyModule do
  import SimpleLisp
  
  def calculate do
    # Use Lisp syntax for calculations
    result = lisp!(+ (* 2 3) (/ 10 2))
    IO.puts("Result: #{result}")  # Result: 11
  end
end
```

### Defining Functions

```elixir
defmodule MathFunctions do
  import SimpleLisp
  
  # Define functions using Lisp syntax
  lisp!(defn square [x] (* x x))
  lisp!(defn cube [x] (* x x x))
  
  lisp!(defn hypotenuse [a b]
    (sqrt (+ (square a) (square b))))
end
```

### Mixed Syntax

```elixir
defmodule MixedExample do
  import SimpleLisp
  
  def process_data(data) do
    data
    |> lisp!(map (fn [x] (* x 2)))      # Double each element
    |> Enum.filter(&(&1 > 10))          # Regular Elixir
    |> lisp!(reduce (fn [a b] (+ a b)) 0) # Sum in Lisp
  end
end
```

### Complex Algorithm

```elixir
defmodule Algorithms do
  import SimpleLisp
  
  # Quicksort in Lisp syntax
  lisp!(defn quicksort [lst]
    (if (empty? lst)
      []
      (let [pivot (car lst)
            rest (cdr lst)
            smaller (filter (fn [x] (< x pivot)) rest)
            larger (filter (fn [x] (>= x pivot)) rest)]
        (++ (quicksort smaller) 
            (cons pivot (quicksort larger))))))
end
```

## Benefits

1. **Educational**: Demonstrates the power of reader macros
2. **Familiar Syntax**: Lisp programmers can write familiar code
3. **Compile-Time**: Zero runtime overhead - all transformation happens at compile time
4. **Interoperability**: Seamlessly mix Lisp and Elixir syntax
5. **Extensible**: Easy to add new Lisp forms and functions

## Limitations

1. **Basic Implementation**: This is a simplified Lisp - not all features are supported
2. **Error Messages**: Errors show transformed Elixir code, not original Lisp
3. **Macros**: Only supports predefined transformations, not user-defined Lisp macros
4. **Debugging**: Step-through debugging shows Elixir code, not Lisp

## Extending the Implementation

To add new Lisp forms, modify the `transform_operation/2` function in `SimpleLisp`:

```elixir
defp transform_operation("my-function", args) do
  # Transform arguments
  args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
  # Return Elixir code
  "MyModule.my_function(#{args_str})"
end
```

## Testing

Run the tests with:
```bash
mix test test/elixir/simple_lisp_test.exs
```

Run the demo:
```elixir
LispDemo.run_all()
```

## Conclusion

This SimpleLisp reader macro showcases how reader macros enable embedding domain-specific languages directly into Elixir. While this is a toy implementation, the same principles could be used to create reader macros for:

- SQL queries
- Mathematical notation
- Configuration DSLs
- Template languages
- Regular expressions
- Any domain-specific syntax

Reader macros provide a powerful metaprogramming tool for creating expressive, domain-appropriate syntax while maintaining the full power of the host language.