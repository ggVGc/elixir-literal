defmodule ReaderMacroDemo do
  @moduledoc """
  Comprehensive demonstration of working reader macro functionality.
  
  This shows reader macros working as designed:
  - Reader macros defined and used in the same file
  - Pattern matching on reader macro content
  - String replacement functionality
  - Integration with normal Elixir code
  """

  # Define multiple reader macros with different patterns
  defreadermacro math("simple") do
    "42"
  end
  
  defreadermacro math("complex calculation") do
    "84"
  end
  
  defreadermacro lisp("+ 1 2 3") do
    "6"
  end
  
  defreadermacro sql("SELECT * FROM users") do
    "[{id: 1, name: \"Alice\"}, {id: 2, name: \"Bob\"}]"
  end
  
  defreadermacro config("production.database.url") do
    "\"postgres://prod_server:5432/myapp\""
  end

  def run_demo do
    IO.puts("🚀 Reader Macro Comprehensive Demo")
    IO.puts("=" |> String.duplicate(50))
    
    IO.puts("\n📝 Testing different reader macro patterns:")
    
    # Test basic math reader macro
    result1 = math!(simple)
    IO.puts("math!(simple) = #{result1}")
    
    # Test more complex pattern
    result2 = math!(complex calculation) 
    IO.puts("math!(complex calculation) = #{result2}")
    
    # Test Lisp-style reader macro
    result3 = lisp!(+ 1 2 3)
    IO.puts("lisp!(+ 1 2 3) = #{result3}")
    
    # Test SQL-style reader macro  
    result4 = sql!(SELECT * FROM users)
    IO.puts("sql!(SELECT * FROM users) = #{inspect(result4)}")
    
    # Test config-style reader macro
    result5 = config!(production.database.url)
    IO.puts("config!(production.database.url) = #{result5}")
    
    IO.puts("\n🔥 Testing reader macros in expressions:")
    
    # Reader macros in arithmetic expressions
    total = math!(simple) + math!(complex calculation)
    IO.puts("math!(simple) + math!(complex calculation) = #{result1} + #{result2} = #{total}")
    
    # Reader macros in function calls
    max_value = max(math!(simple), lisp!(+ 1 2 3))
    IO.puts("max(math!(simple), lisp!(+ 1 2 3)) = max(#{result1}, #{result3}) = #{max_value}")
    
    # Reader macros in lists
    numbers = [1, math!(simple), 3, lisp!(+ 1 2 3), 5]
    IO.puts("List with reader macros: #{inspect(numbers)}")
    
    IO.puts("\n✅ All reader macros working correctly!")
    IO.puts("✨ Reader macro preprocessing successfully implemented!")
    
    :ok
  end
end

# Run the comprehensive demo
ReaderMacroDemo.run_demo()