defmodule SimpleTest do  
  # Define a reader macro that replaces "HELLO" with "42"
  defreadermacro hello("HELLO") do
    "42"  
  end

  def test do
    IO.puts("Value: #{HELLO}")
  end
end

SimpleTest.test()