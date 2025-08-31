# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 The Elixir Team

defmodule LispDemo do
  @moduledoc """
  Demonstration of the SimpleLisp reader macro.
  
  This module shows how you can write Lisp-style S-expressions
  that get transformed into Elixir code at compile time.
  """
  
  import SimpleLisp
  
  @doc """
  Demonstrate basic arithmetic operations in Lisp syntax.
  """
  def arithmetic_examples do
    IO.puts("=== Arithmetic Examples ===")
    
    # Simple addition
    result1 = lisp!(+ 1 2 3 4 5)
    IO.puts("(+ 1 2 3 4 5) = #{result1}")
    
    # Nested arithmetic
    result2 = lisp!(* 2 (+ 3 4))
    IO.puts("(* 2 (+ 3 4)) = #{result2}")
    
    # More complex expression
    result3 = lisp!(/ (+ 10 20) (- 7 2))
    IO.puts("(/ (+ 10 20) (- 7 2)) = #{result3}")
    
    result1 + result2 + result3
  end
  
  @doc """
  Demonstrate list operations in Lisp syntax.
  """
  def list_examples do
    IO.puts("\n=== List Examples ===")
    
    # Create a list
    list1 = lisp!(list 1 2 3 4 5)
    IO.puts("(list 1 2 3 4 5) = #{inspect(list1)}")
    
    # Cons operation
    list2 = lisp!(cons 0 (list 1 2 3))
    IO.puts("(cons 0 (list 1 2 3)) = #{inspect(list2)}")
    
    # Car (head) and cdr (tail)
    head = lisp!(car [1 2 3 4])
    IO.puts("(car [1 2 3 4]) = #{head}")
    
    tail = lisp!(cdr [1 2 3 4])
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
    result1 = lisp!(if (> x 10) "big" "small")
    IO.puts("(if (> x 10) \"big\" \"small\") = #{result1}")
    
    # Nested conditionals
    result2 = lisp!(if (< x 0) 
                      "negative" 
                      (if (= x 0) "zero" "positive"))
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
    doubled = lisp!(map (fn [x] (* x 2)) [1 2 3 4 5])
    IO.puts("(map (fn [x] (* x 2)) [1 2 3 4 5]) = #{inspect(doubled)}")
    
    # Filter
    evens = lisp!(filter (fn [x] (= 0 (rem x 2))) [1 2 3 4 5 6 7 8])
    IO.puts("Filter evens: #{inspect(evens)}")
    
    # Reduce (sum)
    sum = lisp!(reduce (fn [a b] (+ a b)) 0 [1 2 3 4 5])
    IO.puts("(reduce + 0 [1 2 3 4 5]) = #{sum}")
    
    {doubled, evens, sum}
  end
  
  @doc """
  Define functions using Lisp syntax.
  """
  def function_definition_examples do
    IO.puts("\n=== Function Definition Examples ===")
    
    # Define a square function
    lisp!(defn square [x] (* x x))
    
    # Define a factorial function
    lisp!(defn factorial [n]
      (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
    
    # Define a fibonacci function
    lisp!(defn fib [n]
      (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))
    
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
    result1 = lisp!(let [x 10 y 20] (+ x y))
    IO.puts("(let [x 10 y 20] (+ x y)) = #{result1}")
    
    # Let with computed values
    result2 = lisp!(let [a (+ 1 2) 
                         b (* a 3)] 
                      (+ a b))
    IO.puts("(let [a (+ 1 2) b (* a 3)] (+ a b)) = #{result2}")
    
    {result1, result2}
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
    
    IO.puts("\n" <> String.duplicate("=", 50))
    IO.puts("✨ All Lisp examples completed successfully! ✨")
  end
  
  @doc """
  Example of a more complex Lisp program: quicksort
  """
  def quicksort_example do
    IO.puts("\n=== Quicksort in Lisp Syntax ===")
    
    # Define quicksort using Lisp syntax
    lisp!(defn quicksort [lst]
      (if (empty? lst)
        []
        (let [pivot (car lst)
              rest (cdr lst)
              smaller (filter (fn [x] (< x pivot)) rest)
              larger (filter (fn [x] (>= x pivot)) rest)]
          (++ (quicksort smaller) 
              (cons pivot (quicksort larger))))))
    
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
    transformed = lisp!(map (fn [x] (* x x)) elixir_list)
    
    IO.puts("Original Elixir list: #{inspect(elixir_list)}")
    IO.puts("Squared using Lisp map: #{inspect(transformed)}")
    
    # Combine with Elixir pipeline
    result = elixir_list
    |> lisp!(map (fn [x] (+ x 1)))
    |> Enum.filter(&(&1 > 25))
    |> lisp!(reduce (fn [a b] (+ a b)) 0)
    
    IO.puts("Pipeline result: #{result}")
    
    result
  end
end