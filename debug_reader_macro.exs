defmodule DebugReaderMacro do
  defreadermacro debug("DEBUG") do
    "\"Debug worked!\""
  end

  def test do
    IO.puts("About to use reader macro:")
    result = DEBUG
    IO.puts("Reader macro result: #{inspect(result)}")
    result
  end
end

DebugReaderMacro.test()