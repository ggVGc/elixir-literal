# Comprehensive test suite for the Lisp implementation
# Run with: ./bin/elixir lisp_test.exs

Code.require_file("lisp.ex")

defmodule LispTest do
  @moduledoc """
  Test module demonstrating the Lisp DSL implementation.
  
  This module shows how to:
  1. Define functions in Lisp syntax that become regular Elixir functions
  2. Use arithmetic, logic, and control flow operations
  3. Integrate Lisp-defined functions with regular Elixir code
  """
  
  import Lisp
  
  # Basic arithmetic function
  deflisp quote do ~~((defun add_two (x) (+ x 2))) end
  
  # More complex arithmetic with multiple operations
  deflisp quote do ~~((defun quadratic (a b c x) (+ (* a (* x x)) (* b x) c))) end
  
  # Recursive factorial function with negative number guard
  deflisp quote do ~~((defun factorial (n) (if (< n 0) 0 (if (= n 0) 1 (* n (factorial (- n 1))))))) end
  
  # Fibonacci with recursion
  deflisp quote do ~~((defun fibonacci (n) (if (<= n 1) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))) end
  
  # Function using comparisons and logic
  deflisp quote do ~~((defun classify_number (n) (if (and (>= n 0) (< n 10)) :single_digit (if (and (>= n 10) (< n 100)) :double_digit :large)))) end
  
  # Function with let bindings
  deflisp quote do ~~((defun circle_area (radius) (let ((pi 3.14159) (r_squared (* radius radius))) (* pi r_squared)))) end
  
  # List processing function
  deflisp quote do ~~((defun sum_list (lst) (if (= lst nil) 0 (+ (first lst) (sum_list (rest lst)))))) end
  
  # Function that creates and manipulates lists
  deflisp quote do ~~((defun make_range (n) (if (= n 0) (list) (cons n (make_range (- n 1)))))) end
  
  def run_tests do
    IO.puts("=== Lisp Implementation Test Suite ===\n")
    
    test_basic_arithmetic()
    test_recursive_functions()
    test_comparisons_and_logic()
    test_let_bindings()
    test_list_operations()
    test_complex_expressions()
    test_integration_with_elixir()
    
    IO.puts("\n=== All tests completed! ===")
  end
  
  def test_basic_arithmetic do
    IO.puts("--- Basic Arithmetic Tests ---")
    
    result1 = add_two(5)
    IO.puts("add_two(5) = #{result1}")  # Should be 7
    
    result2 = quadratic(1, -5, 6, 2)  # x^2 - 5x + 6 at x=2
    IO.puts("quadratic(1, -5, 6, 2) = #{result2}")  # Should be 0
    
    # Test direct expression evaluation
    direct_result = deflisp quote do ~~((+ 1 2 (* 3 4))) end
    IO.puts("Direct expression (+ 1 2 (* 3 4)) = #{direct_result}")  # Should be 15
    
    IO.puts("")
  end
  
  def test_recursive_functions do
    IO.puts("--- Recursive Function Tests ---")
    
    fact5 = factorial(5)
    IO.puts("factorial(5) = #{fact5}")  # Should be 120
    
    fib10 = fibonacci(10)  
    IO.puts("fibonacci(10) = #{fib10}")  # Should be 55
    
    IO.puts("")
  end
  
  def test_comparisons_and_logic do
    IO.puts("--- Comparison and Logic Tests ---")
    
    class1 = classify_number(5)
    IO.puts("classify_number(5) = #{class1}")  # Should be :single_digit
    
    class2 = classify_number(42)
    IO.puts("classify_number(42) = #{class2}")  # Should be :double_digit
    
    class3 = classify_number(150)
    IO.puts("classify_number(150) = #{class3}")  # Should be :large
    
    # Test direct logical expressions
    logic_result = deflisp quote do ~~((and (> 5 3) (< 10 20))) end
    IO.puts("(and (> 5 3) (< 10 20)) = #{logic_result}")  # Should be true
    
    IO.puts("")
  end
  
  def test_let_bindings do
    IO.puts("--- Let Binding Tests ---")
    
    area = circle_area(5)
    IO.puts("circle_area(5) = #{area}")  # Should be ~78.54
    
    # Test direct let expression
    let_result = deflisp quote do ~~((let ((x 10) (y 20)) (+ x y (* x y)))) end
    IO.puts("let expression result = #{let_result}")  # Should be 230
    
    IO.puts("")
  end
  
  def test_list_operations do
    IO.puts("--- List Operation Tests ---")
    
    # Test list creation and sum
    test_list = [1, 2, 3, 4, 5]
    sum_result = sum_list(test_list)
    IO.puts("sum_list([1,2,3,4,5]) = #{sum_result}")  # Should be 15
    
    # Test range creation (note: this creates reverse order)
    range_result = make_range(5)
    IO.puts("make_range(5) = #{inspect(range_result)}")  # Should be [5, 4, 3, 2, 1]
    
    # Test direct list operations
    list_result = deflisp quote do ~~((list 1 2 3 (+ 2 2) 5)) end
    IO.puts("(list 1 2 3 (+ 2 2) 5) = #{inspect(list_result)}")  # Should be [1, 2, 3, 4, 5]
    
    first_result = deflisp quote do ~~((first (list 10 20 30))) end
    IO.puts("(first (list 10 20 30)) = #{first_result}")  # Should be 10
    
    IO.puts("")
  end
  
  def test_complex_expressions do
    IO.puts("--- Complex Expression Tests ---")
    
    # Nested conditional with arithmetic
    complex_result = deflisp quote do ~~((if (> (+ 2 3) 4) (factorial 4) (fibonacci 6))) end
    IO.puts("Complex conditional result = #{complex_result}")  # Should be 24
    
    # Let with function calls
    let_with_calls = deflisp quote do ~~((let ((fact_5 (factorial 5)) (fib_7 (fibonacci 7))) (+ fact_5 fib_7))) end
    IO.puts("Let with function calls = #{let_with_calls}")  # Should be 120 + 13 = 133
    
    IO.puts("")
  end
  
  def test_integration_with_elixir do
    IO.puts("--- Integration with Elixir Tests ---")
    
    # Use Lisp functions in Elixir higher-order functions
    numbers = [1, 2, 3, 4, 5]
    incremented = Enum.map(numbers, &add_two/1)
    IO.puts("Enum.map([1,2,3,4,5], &add_two/1) = #{inspect(incremented)}")
    
    # Filter using Lisp-defined classification
    classifications = Enum.map([5, 15, 150], &classify_number/1)
    IO.puts("Classifications: #{inspect(classifications)}")
    
    # Use in Elixir control structures
    factorial_results = for n <- [3, 4, 5], do: {n, factorial(n)}
    IO.puts("Factorial results: #{inspect(factorial_results)}")
    
    # Demonstrate error handling
    try do
      bad_result = factorial(-1)  # This might cause issues depending on implementation
      IO.puts("factorial(-1) = #{bad_result}")
    rescue
      e -> IO.puts("Error with factorial(-1): #{inspect(e)}")
    end
    
    IO.puts("")
  end
  
  # Regular Elixir helper function to show integration
  def elixir_helper(x) do
    # Use Lisp-defined function from regular Elixir
    squared = quadratic(1, 0, 0, x)  # Just x^2
    "The square of #{x} is #{squared}"
  end
  
  def demonstration do
    IO.puts("=== Lisp-Elixir Integration Demo ===")
    IO.puts(elixir_helper(7))
    
    # Show that functions are just regular Elixir functions
    IO.puts("Function info for add_two: #{inspect(&add_two/1)}")
    IO.puts("Module functions: #{inspect(__MODULE__.__info__(:functions))}")
  end
end

# Run all the tests
LispTest.run_tests()
LispTest.demonstration()
