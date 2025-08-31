defmodule MinimalTest do
  defreadermacro test_lisp("lisp!(" <> rest) do
    "Enum.sum([1, 2, 3])"
  end
  
  def test do
    IO.puts("About to use lisp")
    result = lisp!(+ 1 2 3)
    IO.puts("Result: #{result}")
  end
end

MinimalTest.test()