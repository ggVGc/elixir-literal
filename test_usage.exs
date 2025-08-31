defmodule TestUsage do
  def test_simple do
    result = HELLO
    IO.puts("HELLO resolved to: #{inspect(result)}")
    result
  end
end

TestUsage.test_simple()