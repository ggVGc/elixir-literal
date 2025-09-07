defmodule SimpleFunctionTest do
  use ExUnit.Case, async: false
  import Lipex

  # Define test functions at module level using the same syntax as calculator examples
  deflipex ~~((def double (x) (* x 2)))
  deflipex ~~((def abs_val (x) (* x -1)))

  test "simple function call" do
    result = deflipex ~~((double 5))
    assert result == 10
  end

  test "nested function call" do
    result = deflipex ~~((abs_val (- 5 (double 5))))
    assert result == 5
  end

  deflipex ~~((def when_over_5 (a) when (> a 5) true))
  deflipex ~~((def when_over_5 (_) false))

  test "when clauses" do
    assert when_over_5(6) == true
    assert when_over_5(2) == false
  end
end
