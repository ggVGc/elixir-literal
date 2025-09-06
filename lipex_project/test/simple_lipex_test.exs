defmodule SimpleLipexTest do
  use ExUnit.Case
  import Lipex

  test "simple addition" do
    result = deflipex ~~((+ 1 2))
    assert result == 3
  end
end