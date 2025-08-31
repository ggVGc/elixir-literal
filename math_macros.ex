defmodule MathMacros do
  @moduledoc """
  Simple reader macro definitions for mathematical operations.
  """

  # Define reader macros that match calculator.ex usage
  defreadermacro math("+ 10 5") do
    "42"
  end
  
  defreadermacro math("+ 1 2") do
    "42"
  end
  
  defreadermacro math("test") do
    "42"
  end
  
  defreadermacro math("anything here") do
    "42"
  end
  
  defreadermacro math("pattern match test") do
    "42"
  end

  defreadermacro calc("* 3 7") do
    "100"
  end
  
  defreadermacro calc("- 5 3") do
    "100"
  end
  
  defreadermacro calc("another test") do
    "100"
  end
  
  defreadermacro calc("guard test") do
    "100"
  end

  def test do
    IO.puts("MathMacros module loaded successfully!")
  end
end