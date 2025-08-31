defmodule TokenReaderMacroTest do
  @moduledoc """
  Test the new token-level reader macro system.
  """

  # Define a reader macro using the new token-level approach
  defreadermacro lisp("test") do
    "42"
  end

  def test do
    IO.puts("=== Token-Level Reader Macro Test ===")
    
    # Try to use the reader macro
    result = lisp!(+ 1 2 3)
    IO.puts("lisp!(+ 1 2 3) = #{result}")
    
    result
  end
end

TokenReaderMacroTest.test()