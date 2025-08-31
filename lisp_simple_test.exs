defmodule LispSimpleTest do
  # Simple Lisp-like reader macros
  defreadermacro add("(+ 1 2)") do "Enum.sum([1, 2])" end
  defreadermacro multiply("(* 3 4)") do "3 * 4" end
  defreadermacro hello_lisp("(hello)") do "\"Hello from Lisp!\"" end

  def test_arithmetic do
    result1 = (+ 1 2)
    result2 = (* 3 4)
    IO.puts("Addition: #{result1}")
    IO.puts("Multiplication: #{result2}")
    {result1, result2}
  end

  def test_hello do
    message = (hello)
    IO.puts("Message: #{message}")
    message
  end

  def run_all do
    IO.puts("=== Simple Lisp Reader Macro Test ===")
    test_arithmetic()
    test_hello()
    IO.puts("=== All tests completed ===")
  end
end

LispSimpleTest.run_all()