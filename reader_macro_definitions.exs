defmodule ReaderMacroDefinitions do
  defreadermacro hello("HELLO") do
    "42"
  end
  
  defreadermacro greet("GREET_WORLD") do
    "\"Hello, World!\""
  end
  
  defreadermacro add_nums("(+ 1 2)") do
    "Enum.sum([1, 2])"
  end
end