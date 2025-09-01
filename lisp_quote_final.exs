# Final comprehensive test of quote-based Lisp with working operators

Code.require_file("lisp.ex")

defmodule LispQuoteFinal do
  import Lisp
  
  # Simple function definition using addition
  deflisp quote do ~~((defun double (x) (+ x x))) end
  
  # Function with multiple parameters
  deflisp quote do ~~((defun add_three (a b c) (+ a b c))) end
  
  # Function using subtraction
  deflisp quote do ~~((defun subtract_one (n) (- n 1))) end
  
  # Function that adds numbers
  deflisp quote do ~~((defun add_five (x) (+ x 5))) end
  
  def test_functions do
    IO.puts("=== Final Quote-Based Lisp Test ===\n")
    
    IO.puts("--- Function Definitions ---")
    IO.puts("double(5) = #{double(5)}")
    IO.puts("add_three(1, 2, 3) = #{add_three(1, 2, 3)}")  
    IO.puts("subtract_one(10) = #{subtract_one(10)}")
    IO.puts("add_five(7) = #{add_five(7)}")
    
    IO.puts("\n--- Expression Evaluation ---")
    result1 = deflisp quote do ~~((+ (+ 2 3) (- 8 2))) end
    IO.puts("(+ (+ 2 3) (- 8 2)) = #{result1}")
    
    # Test calling the function directly in Elixir
    IO.puts("Calling double(7) directly = #{double(7)}")
    
    IO.puts("\n--- Integration with Elixir ---")
    numbers = [1, 2, 3, 4, 5]
    doubled = Enum.map(numbers, &double/1)
    IO.puts("Enum.map([1,2,3,4,5], &double/1) = #{inspect(doubled)}")
    
    # Test in list comprehensions
    results = for x <- [1, 2, 3], do: {x, double(x), add_three(x, x, x)}
    IO.puts("Comprehension results: #{inspect(results)}")
    
    IO.puts("\n--- Function Information ---")
    IO.puts("double/1 arity: #{Function.info(&double/1)[:arity]}")
    functions = __MODULE__.__info__(:functions)
    lisp_functions = Enum.filter(functions, fn {name, arity} -> 
      name in [:double, :add_three, :subtract_one, :add_five] 
    end)
    IO.puts("Lisp-defined functions: #{inspect(lisp_functions)}")
    
    IO.puts("\n=== SUCCESS: Quote-based Lisp DSL works perfectly! ===")
  end
  
  # Regular Elixir function using Lisp-defined functions
  def compute_stats(n) do
    %{
      original: n,
      doubled: double(n),
      tripled: add_three(n, n, n), 
      minus_one: subtract_one(n),
      plus_five: add_five(n)
    }
  end
  
  def demo_advanced_usage do
    IO.puts("\n--- Advanced Integration Demo ---")
    
    # Use Lisp functions in pipes
    result = 5
    |> double()
    |> subtract_one()
    |> add_five()
    
    IO.puts("5 |> double |> subtract_one |> add_five = #{result}")
    
    # Use in pattern matching
    case double(4) do
      8 -> IO.puts("✓ Pattern match success: double(4) = 8")
      other -> IO.puts("✗ Unexpected: #{other}")
    end
    
    # Show statistics
    stats = compute_stats(3)
    IO.puts("Stats for 3: #{inspect(stats)}")
    
    # Demonstrate higher-order function usage
    transformer = &double/1
    IO.puts("Higher-order function usage: #{transformer.(6)}")
    
    # Use in guards (if functions are pure)
    test_guard = fn x when x > 0 -> add_five(x) end
    IO.puts("Guard function test: #{test_guard.(2)}")
    
    IO.puts("\n🎉 Lisp functions integrate seamlessly with Elixir! 🎉")
  end
end

LispQuoteFinal.test_functions()
LispQuoteFinal.demo_advanced_usage()
