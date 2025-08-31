# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 The Elixir Team

defmodule LispDemoStandalone do
  @moduledoc """
  Self-contained demonstration of the SimpleLisp reader macro.
  
  This module includes all SimpleLisp functionality directly,
  eliminating the need for cross-module imports.
  """

  # Define a regular macro for lisp syntax (using string literals)
  defmacro lisp!(expr) when is_binary(expr) do
    elixir_code = parse_lisp_to_elixir(expr) 
    Code.string_to_quoted!(elixir_code)
  end

  # Pre-define functions using Lisp syntax at module level
  lisp!("(defn square [x] (* x x))")
  lisp!("(defn factorial [n] (if (<= n 1) 1 (* n (factorial (- n 1)))))")
  lisp!("(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")

  # ==== SimpleLisp Helper Functions ====

  @doc """
  Parse a Lisp S-expression and convert it to Elixir code.
  """
  def parse_lisp_to_elixir(expr) do
    expr
    |> String.trim()
    |> tokenize_lisp()
    |> parse_tokens()
    |> transform_to_elixir()
  end

  @doc """
  Tokenize a Lisp expression into a list of tokens.
  """
  def tokenize_lisp(str) do
    str
    |> String.replace("(", " ( ")
    |> String.replace(")", " ) ")
    |> String.replace("[", " [ ")
    |> String.replace("]", " ] ")
    |> String.split(~r/\s+/, trim: true)
    |> Enum.map(&parse_token/1)
  end

  defp parse_token("("), do: :open_paren
  defp parse_token(")"), do: :close_paren
  defp parse_token("["), do: :open_bracket
  defp parse_token("]"), do: :close_bracket
  defp parse_token("true"), do: {:bool, true}
  defp parse_token("false"), do: {:bool, false}
  defp parse_token("nil"), do: {:nil, nil}
  defp parse_token(<<"\"", _::binary>> = str) do
    # String literal
    {:string, String.trim(str, "\"")}
  end
  defp parse_token(str) do
    # Try to parse as number
    case Integer.parse(str) do
      {num, ""} -> {:number, num}
      _ ->
        case Float.parse(str) do
          {num, ""} -> {:number, num}
          _ -> {:symbol, str}
        end
    end
  end

  @doc """
  Parse tokens into an AST.
  """
  def parse_tokens(tokens) do
    {ast, _rest} = parse_expr(tokens)
    ast
  end

  defp parse_expr([:open_paren | rest]) do
    parse_list(rest, [])
  end
  defp parse_expr([:open_bracket | rest]) do
    {elements, rest} = parse_array(rest, [])
    {{:array, elements}, rest}
  end
  defp parse_expr([{:symbol, sym} | rest]) do
    {{:symbol, sym}, rest}
  end
  defp parse_expr([{:number, n} | rest]) do
    {{:number, n}, rest}
  end
  defp parse_expr([{:string, s} | rest]) do
    {{:string, s}, rest}
  end
  defp parse_expr([{:bool, b} | rest]) do
    {{:bool, b}, rest}
  end
  defp parse_expr([{:nil, _} | rest]) do
    {{:nil, nil}, rest}
  end

  defp parse_list([:close_paren | rest], acc) do
    {{:list, Enum.reverse(acc)}, rest}
  end
  defp parse_list(tokens, acc) do
    {expr, rest} = parse_expr(tokens)
    parse_list(rest, [expr | acc])
  end

  defp parse_array([:close_bracket | rest], acc) do
    {Enum.reverse(acc), rest}
  end
  defp parse_array(tokens, acc) do
    {expr, rest} = parse_expr(tokens)
    parse_array(rest, [expr | acc])
  end

  @doc """
  Transform Lisp AST to Elixir code string.
  """
  def transform_to_elixir({:list, [{:symbol, op} | args]}) do
    transform_operation(op, args)
  end
  def transform_to_elixir({:array, elements}) do
    # Array literal
    "[" <> Enum.map_join(elements, ", ", &transform_to_elixir/1) <> "]"
  end
  def transform_to_elixir({:symbol, sym}) do
    sym
  end
  def transform_to_elixir({:number, n}) do
    to_string(n)
  end
  def transform_to_elixir({:string, s}) do
    "\"#{s}\""
  end
  def transform_to_elixir({:bool, b}) do
    to_string(b)
  end
  def transform_to_elixir({:nil, _}) do
    "nil"
  end

  # ==== Operation Transformers ====

  defp transform_operation("+", args) do
    case args do
      [] -> "0"
      [single] -> transform_to_elixir(single)
      _ -> 
        # For multiple args, use Enum.sum
        args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
        "Enum.sum([#{args_str}])"
    end
  end

  defp transform_operation("-", [a, b]) do
    "#{transform_to_elixir(a)} - #{transform_to_elixir(b)}"
  end

  defp transform_operation("*", args) do
    case args do
      [] -> "1"
      [single] -> transform_to_elixir(single)
      _ -> 
        # Multiple args - use Enum.reduce
        args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
        "Enum.reduce([#{args_str}], &*/2)"
    end
  end

  defp transform_operation("/", [a, b]) do
    "#{transform_to_elixir(a)} / #{transform_to_elixir(b)}"
  end

  defp transform_operation("rem", [a, b]) do
    "rem(#{transform_to_elixir(a)}, #{transform_to_elixir(b)})"
  end

  # Comparison operators
  defp transform_operation(">", [a, b]) do
    "#{transform_to_elixir(a)} > #{transform_to_elixir(b)}"
  end

  defp transform_operation("<", [a, b]) do
    "#{transform_to_elixir(a)} < #{transform_to_elixir(b)}"
  end

  defp transform_operation(">=", [a, b]) do
    "#{transform_to_elixir(a)} >= #{transform_to_elixir(b)}"
  end

  defp transform_operation("<=", [a, b]) do
    "#{transform_to_elixir(a)} <= #{transform_to_elixir(b)}"
  end

  defp transform_operation("=", [a, b]) do
    "#{transform_to_elixir(a)} == #{transform_to_elixir(b)}"
  end

  # List operations
  defp transform_operation("list", args) do
    args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
    "[#{args_str}]"
  end

  defp transform_operation("cons", [head, tail]) do
    "[#{transform_to_elixir(head)} | #{transform_to_elixir(tail)}]"
  end

  defp transform_operation("car", [list]) do
    "hd(#{transform_to_elixir(list)})"
  end

  defp transform_operation("cdr", [list]) do
    "tl(#{transform_to_elixir(list)})"
  end

  defp transform_operation("empty?", [list]) do
    "Enum.empty?(#{transform_to_elixir(list)})"
  end

  defp transform_operation("++", args) do
    args_str = Enum.map_join(args, " ++ ", &transform_to_elixir/1)
    "(#{args_str})"
  end

  # Control flow
  defp transform_operation("if", [test, then_branch, else_branch]) do
    "if #{transform_to_elixir(test)}, do: #{transform_to_elixir(then_branch)}, else: #{transform_to_elixir(else_branch)}"
  end

  # Higher-order functions
  defp transform_operation("map", [func, list]) do
    "Enum.map(#{transform_to_elixir(list)}, #{transform_to_elixir(func)})"
  end

  defp transform_operation("filter", [func, list]) do
    "Enum.filter(#{transform_to_elixir(list)}, #{transform_to_elixir(func)})"
  end

  defp transform_operation("reduce", [func, init, list]) do
    "Enum.reduce(#{transform_to_elixir(list)}, #{transform_to_elixir(init)}, #{transform_to_elixir(func)})"
  end

  # Anonymous functions
  defp transform_operation("fn", [{:array, params}, body]) do
    params_str = Enum.map_join(params, ", ", fn {:symbol, p} -> p end)
    "fn #{params_str} -> #{transform_to_elixir(body)} end"
  end

  defp transform_operation("lambda", [{:array, params}, body]) do
    # Alias for fn
    transform_operation("fn", [{:array, params}, body])
  end

  # Function definition
  defp transform_operation("defn", [{:symbol, name}, {:array, params}, body]) do
    params_str = Enum.map_join(params, ", ", fn {:symbol, p} -> p end)
    "def #{name}(#{params_str}), do: #{transform_to_elixir(body)}"
  end

  # Let bindings
  defp transform_operation("let", [{:array, bindings}, body]) do
    # Transform let into a with expression
    bindings_str = bindings
    |> Enum.chunk_every(2)
    |> Enum.map(fn [{:symbol, var}, val] ->
      "#{var} = #{transform_to_elixir(val)}"
    end)
    |> Enum.join(", ")
    
    "with #{bindings_str}, do: #{transform_to_elixir(body)}"
  end

  # Default: function call
  defp transform_operation(func, args) do
    args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
    "#{func}(#{args_str})"
  end

  # ==== Demo Examples ====

  @doc """
  Demonstrate basic arithmetic operations in Lisp syntax.
  """
  def arithmetic_examples do
    IO.puts("=== Arithmetic Examples ===")
    
    # Simple addition
    result1 = lisp!("(+ 1 2 3 4 5)")
    IO.puts("(+ 1 2 3 4 5) = #{result1}")
    
    # Nested arithmetic
    result2 = lisp!("(* 2 (+ 3 4))")
    IO.puts("(* 2 (+ 3 4)) = #{result2}")
    
    # More complex expression
    result3 = lisp!("(/ (+ 10 20) (- 7 2))")
    IO.puts("(/ (+ 10 20) (- 7 2)) = #{result3}")
    
    result1 + result2 + result3
  end
  
  @doc """
  Demonstrate list operations in Lisp syntax.
  """
  def list_examples do
    IO.puts("\n=== List Examples ===")
    
    # Create a list
    list1 = lisp!("(list 1 2 3 4 5)")
    IO.puts("(list 1 2 3 4 5) = #{inspect(list1)}")
    
    # Cons operation
    list2 = lisp!("(cons 0 (list 1 2 3))")
    IO.puts("(cons 0 (list 1 2 3)) = #{inspect(list2)}")
    
    # Car (head) and cdr (tail)
    head = lisp!("(car [1 2 3 4])")
    IO.puts("(car [1 2 3 4]) = #{head}")
    
    tail = lisp!("(cdr [1 2 3 4])")
    IO.puts("(cdr [1 2 3 4]) = #{inspect(tail)}")
    
    {list1, list2, head, tail}
  end
  
  @doc """
  Demonstrate conditional expressions in Lisp syntax.
  """
  def conditional_examples(x) do
    IO.puts("\n=== Conditional Examples ===")
    IO.puts("x = #{x}")
    
    # Simple if expression
    result1 = lisp!("(if (> x 10) \"big\" \"small\")")
    IO.puts("(if (> x 10) \"big\" \"small\") = #{result1}")
    
    # Nested conditionals
    result2 = lisp!("(if (< x 0) \"negative\" (if (= x 0) \"zero\" \"positive\"))")
    IO.puts("Nested if for sign: #{result2}")
    
    {result1, result2}
  end
  
  @doc """
  Demonstrate higher-order functions in Lisp syntax.
  """
  def functional_examples do
    IO.puts("\n=== Functional Programming Examples ===")
    
    # Map with anonymous function
    nums = [1, 2, 3, 4, 5]
    doubled = lisp!("(map (fn [x] (* x 2)) [1 2 3 4 5])")
    IO.puts("(map (fn [x] (* x 2)) [1 2 3 4 5]) = #{inspect(doubled)}")
    
    # Filter
    evens = lisp!("(filter (fn [x] (= 0 (rem x 2))) [1 2 3 4 5 6 7 8])")
    IO.puts("Filter evens: #{inspect(evens)}")
    
    # Reduce (sum)
    sum = lisp!("(reduce (fn [a b] (+ a b)) 0 [1 2 3 4 5])")
    IO.puts("(reduce + 0 [1 2 3 4 5]) = #{sum}")
    
    {doubled, evens, sum}
  end
  
  @doc """
  Define functions using Lisp syntax.
  """
  def function_definition_examples do
    IO.puts("\n=== Function Definition Examples ===")
    
    # Functions were defined at module level using Lisp syntax
    IO.puts("square(5) = #{square(5)}")
    IO.puts("factorial(5) = #{factorial(5)}")
    IO.puts("fib(10) = #{fib(10)}")
    
    # Return results
    {square(5), factorial(5), fib(10)}
  end
  
  @doc """
  Demonstrate let bindings in Lisp syntax.
  """
  def let_binding_examples do
    IO.puts("\n=== Let Binding Examples ===")
    
    # Simple let binding
    result1 = lisp!("(let [x 10 y 20] (+ x y))")
    IO.puts("(let [x 10 y 20] (+ x y)) = #{result1}")
    
    # Let with computed values
    result2 = lisp!("(let [a (+ 1 2) b (* a 3)] (+ a b))")
    IO.puts("(let [a (+ 1 2) b (* a 3)] (+ a b)) = #{result2}")
    
    {result1, result2}
  end
  
  @doc """
  Example of a more complex Lisp program: quicksort
  """
  def quicksort_example do
    IO.puts("\n=== Quicksort in Lisp Syntax ===")
    
    # Define quicksort using Lisp syntax
    lisp!("(defn quicksort [lst] (if (empty? lst) [] (let [pivot (car lst) rest (cdr lst) smaller (filter (fn [x] (< x pivot)) rest) larger (filter (fn [x] (>= x pivot)) rest)] (++ (quicksort smaller) (cons pivot (quicksort larger))))))")
    
    # Test the quicksort
    unsorted = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
    sorted = quicksort(unsorted)
    
    IO.puts("Unsorted: #{inspect(unsorted)}")
    IO.puts("Sorted: #{inspect(sorted)}")
    
    sorted
  end
  
  @doc """
  Demonstrate how Lisp macros could compose with Elixir.
  """
  def mixed_syntax_example do
    IO.puts("\n=== Mixed Elixir and Lisp Syntax ===")
    
    # Mix Lisp expressions with regular Elixir
    elixir_list = [10, 20, 30, 40, 50]
    
    # Use Lisp syntax for the transformation
    transformed = lisp!("(map (fn [x] (* x x)) elixir_list)")
    
    IO.puts("Original Elixir list: #{inspect(elixir_list)}")
    IO.puts("Squared using Lisp map: #{inspect(transformed)}")
    
    # Combine with Elixir pipeline
    result = elixir_list
    |> lisp!("(map (fn [x] (+ x 1)))")
    |> Enum.filter(&(&1 > 25))
    |> lisp!("(reduce (fn [a b] (+ a b)) 0)")
    
    IO.puts("Pipeline result: #{result}")
    
    result
  end
  
  @doc """
  Run all demonstrations.
  """
  def run_all do
    IO.puts("\n🎭 SimpleLisp Reader Macro Demonstration 🎭")
    IO.puts("=" |> String.duplicate(50))
    
    arithmetic_examples()
    list_examples()
    conditional_examples(15)
    conditional_examples(5)
    conditional_examples(-3)
    functional_examples()
    function_definition_examples()
    let_binding_examples()
    quicksort_example()
    mixed_syntax_example()
    
    IO.puts("\n" <> String.duplicate("=", 50))
    IO.puts("✨ All Lisp examples completed successfully! ✨")
  end
end

# Run the demo
LispDemoStandalone.run_all()