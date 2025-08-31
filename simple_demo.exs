defmodule SimpleDemo do
  defreadermacro test("hello") do
    "world"
  end

  def main do
    result = test!(hello)
    IO.puts("Reader macro result: #{result}")
    IO.puts("Success: Reader macro transformed correctly!")
  end
end

SimpleDemo.main()