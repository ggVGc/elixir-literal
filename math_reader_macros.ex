defmodule MathReaderMacros do
  @moduledoc """
  Simple reader macro definitions for mathematical operations.
  This demonstrates the traditional defreadermacro syntax.
  """

  # Define a simple reader macro for addition
  defreadermacro add("math") do
    quote do
      fn a, b -> a + b end
    end
  end

  # Define a reader macro for multiplication  
  defreadermacro multiply("math") do
    quote do
      fn a, b -> a * b end
    end
  end

  # Define a reader macro that returns a constant
  defreadermacro magic("number") do
    42
  end

  def test_internal do
    IO.puts("Testing reader macros internally...")
    
    # These would use the reader macros if the system supported them at parse time
    # For now, we'll show what the macros would expand to
    add_fn = (quote do: fn a, b -> a + b end) |> Code.eval_quoted() |> elem(0)
    result = add_fn.(10, 5)
    IO.puts("add_fn.(10, 5) = #{result}")
    
    magic_number = 42
    IO.puts("magic number = #{magic_number}")
  end
end