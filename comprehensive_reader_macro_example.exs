defmodule ComprehensiveReaderMacroExample do
  @moduledoc """
  A comprehensive demonstration of reader macros in Elixir.
  
  This example shows how reader macros can be used to create domain-specific
  languages and syntactic sugar that gets transformed at compile time.
  """

  # Mathematical expressions using Lisp-like syntax
  defreadermacro add_example("(+ 5 10)") do "(5 + 10)" end
  defreadermacro multiply_example("(* 7 8)") do "(7 * 8)" end
  defreadermacro power_example("(^ 2 3)") do ":math.pow(2, 3)" end
  
  # String operations
  defreadermacro greet_world("GREET_WORLD") do "\"Hello, World!\"" end
  defreadermacro shout_important("SHOUT_IMPORTANT") do "String.upcase(\"important message\")" end
  
  # Control flow macros  
  defreadermacro check_large("CHECK_LARGE(x)") do "if x > 40, do: \"x is large\"" end
  defreadermacro check_not_small("CHECK_NOT_SMALL(x)") do "unless x < 10, do: \"x is not small\"" end
  
  # List operations using symbolic syntax
  defreadermacro sum_numbers("SUM_NUMBERS") do "Enum.sum([1, 2, 3, 4, 5])" end
  defreadermacro count_numbers("COUNT_NUMBERS") do "length([1, 2, 3, 4, 5])" end
  defreadermacro reverse_numbers("REVERSE_NUMBERS") do "Enum.reverse([1, 2, 3, 4, 5])" end
  
  # SQL-like syntax
  defreadermacro select_all_users("SELECT_ALL_USERS") do "Enum.to_list(users)" end
  defreadermacro count_users("COUNT_USERS") do "Enum.count(users)" end
  
  # Custom operators and syntax
  defreadermacro pipe_sum("PIPE_SUM") do "[1, 2, 3] |> Enum.sum()" end
  defreadermacro safe_name("SAFE_NAME") do "Map.get(user, :name)" end
  defreadermacro safe_email("SAFE_EMAIL") do "Map.get(user, :email)" end
  
  # Domain-specific syntax
  defreadermacro log_info("LOG_INFO") do "IO.puts(\"[INFO] Processing data...\")" end
  defreadermacro log_warn("LOG_WARN") do "IO.puts(\"[WARN] Something unusual happened\")" end
  defreadermacro timestamp("TIMESTAMP") do "System.system_time(:second)" end

  def math_examples do
    IO.puts("=== Mathematical Examples ===")
    
    # Using Lisp-like mathematical expressions  
    result1 = (+ 5 10)
    result2 = (* 7 8)  
    result3 = (^ 2 3)
    
    IO.puts("Addition: #{result1}")
    IO.puts("Multiplication: #{result2}")
    IO.puts("Power: #{result3}")
    
    {result1, result2, result3}
  end

  def string_examples do
    IO.puts("\n=== String Examples ===")
    
    # Custom string operations
    greeting = GREET_WORLD
    loud_message = SHOUT_IMPORTANT
    
    IO.puts("Greeting: #{greeting}")  
    IO.puts("Loud message: #{loud_message}")
    
    {greeting, loud_message}
  end

  def control_flow_examples do
    IO.puts("\n=== Control Flow Examples ===")
    
    x = 42
    
    # Custom control flow syntax
    result1 = CHECK_LARGE(x)
    result2 = CHECK_NOT_SMALL(x)
    
    IO.puts("Large check result: #{result1}")
    IO.puts("Not small check result: #{result2}")
    
    {result1, result2}
  end

  def list_examples do
    IO.puts("\n=== List Examples ===")
    
    # Symbolic list operations using reader macros
    total = SUM_NUMBERS
    count = COUNT_NUMBERS
    reversed = REVERSE_NUMBERS
    
    IO.puts("Sum: #{total}")
    IO.puts("Count: #{count}")
    IO.puts("Reversed: #{inspect(reversed)}")
    
    {total, count, reversed}
  end

  def data_examples do
    IO.puts("\n=== Data Query Examples ===")
    
    users = [
      %{name: "Alice", age: 30},
      %{name: "Bob", age: 25},
      %{name: "Charlie", age: 35}
    ]
    
    # SQL-like syntax using reader macros
    all_users = SELECT_ALL_USERS
    user_count = COUNT_USERS
    
    IO.puts("All users: #{inspect(all_users)}")
    IO.puts("User count: #{user_count}")
    
    {all_users, user_count}
  end

  def operator_examples do
    IO.puts("\n=== Custom Operator Examples ===")
    
    # Custom operations using reader macros
    result1 = PIPE_SUM
    
    # Safe map access
    user = %{name: "John", age: 30}
    name = SAFE_NAME
    email = SAFE_EMAIL  # Should be nil
    
    IO.puts("Pipe result: #{result1}")
    IO.puts("Name: #{name}")
    IO.puts("Email: #{inspect(email)}")
    
    {result1, name, email}
  end

  def logging_examples do
    IO.puts("\n=== Logging Examples ===")
    
    # Custom logging syntax
    LOG_INFO
    LOG_WARN
    
    # Timestamp macro
    timestamp = TIMESTAMP
    IO.puts("Current timestamp: #{timestamp}")
    
    timestamp
  end

  def run_all_examples do
    IO.puts("🎭 Comprehensive Reader Macro Demonstration 🎭")
    IO.puts(String.duplicate("=", 60))
    
    math_examples()
    string_examples() 
    control_flow_examples()
    list_examples()
    data_examples() 
    operator_examples()
    logging_examples()
    
    IO.puts("\n" <> String.duplicate("=", 60))
    IO.puts("✨ All reader macro examples completed successfully! ✨")
  end
end

ComprehensiveReaderMacroExample.run_all_examples()