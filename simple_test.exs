defmodule SimpleTest do
  defreadermacro math("+ 10 5") do
    "42"
  end

  def test do
    IO.puts("Testing simple reader macro")
    result = math!(+ 10 5)
    IO.puts("Result: #{result}")
  end
end

SimpleTest.test()
