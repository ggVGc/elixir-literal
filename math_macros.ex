defmodule MathMacros do
  @moduledoc """
  Simple reader macro definitions for mathematical operations.
  """

  # Define a reader macro for simple math operations
  defreadermacro math("simple") do
    "42"
  end

  # Define a reader macro for calculations  
  defreadermacro calc("helper") do
    "100"
  end

  def test do
    IO.puts("MathMacros module loaded successfully!")
  end
end