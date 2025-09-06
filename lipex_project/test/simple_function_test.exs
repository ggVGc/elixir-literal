defmodule SimpleFunctionTest do
  use ExUnit.Case, async: false
  import Lipex

  # Define test functions at module level using the same syntax as calculator examples
  deflipex ~~((def double (x) (* x 2)))

  test "simple function call" do
    result = deflipex ~~((double 5))
    assert result == 10
  end
end
