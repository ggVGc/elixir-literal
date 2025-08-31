defmodule TestSimpleReaderMacro do
  defreadermacro hello("HELLO") do
    "42"
  end

  def test do
    IO.puts(HELLO)
  end
end

TestSimpleReaderMacro.test()