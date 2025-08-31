defmodule SimpleReaderMacroTest do
  defreadermacro hello("HELLO") do
    "42"
  end
  
  def test do
    IO.puts("Testing HELLO expansion")
    result = HELLO
    IO.puts("HELLO expanded to: #{inspect(result)}")
    result
  end
end

SimpleReaderMacroTest.test()