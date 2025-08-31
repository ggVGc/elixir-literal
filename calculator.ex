defmodule Calculator do
  @moduledoc """
  Calculator that uses reader macros defined in MathMacros.
  This shows the normal use-case where reader macros work seamlessly.
  """

  # Import the reader macros
  require MathMacros

  def calculate do
    IO.puts("=== Calculator with Reader Macros ===")
    
    # Use reader macros naturally in Elixir code
    # These should be transformed by the reader macro system:
    
    result1 = math!(+ 10 5)  # Should become 42
    IO.puts("math!(+ 10 5) = #{result1}")
    
    result2 = calc!(* 3 7)   # Should become 100  
    IO.puts("calc!(* 3 7) = #{result2}")
    
    # Mix with regular Elixir
    total = result1 + result2 + 8
    IO.puts("Total: #{result1} + #{result2} + 8 = #{total}")
    
    # Show in expressions
    final = math!(anything here) * 2
    IO.puts("math!(anything here) * 2 = #{final}")
    
    final
  end

  def advanced_usage do
    IO.puts("\n=== Advanced Reader Macro Usage ===")
    
    # Reader macros in different contexts
    
    # In function calls
    result = max(math!(+ 1 2), calc!(- 5 3))
    IO.puts("max(math!(+ 1 2), calc!(- 5 3)) = #{result}")
    
    # In lists
    numbers = [1, math!(test), 3, calc!(another test), 5]
    IO.puts("List with reader macros: #{inspect(numbers)}")
    
    # In pattern matching
    case math!(pattern match test) do
      42 -> IO.puts("Reader macro returned 42 as expected!")
      other -> IO.puts("Unexpected value: #{other}")
    end
    
    # In guards (if supported)
    value = calc!(guard test)
    cond do
      value > 50 -> IO.puts("calc! returned a large number: #{value}")
      true -> IO.puts("calc! returned: #{value}")
    end
  end
end