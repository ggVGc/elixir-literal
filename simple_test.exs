defmodule SimpleTest do
  def test do
    result = 42
    IO.puts("result = #{result}")
    result
  end
end

SimpleTest.test()
