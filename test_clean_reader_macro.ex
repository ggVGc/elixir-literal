defmodule CleanTest do
  defreadermacro num("NUM") do
    "42"
  end
  
  def get_number do
    NUM
  end
end

IO.puts("Result: #{CleanTest.get_number()}")