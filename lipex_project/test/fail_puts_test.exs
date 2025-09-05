defmodule LipexFailPutsTest do
  use ExUnit.Case, async: true
  
  @moduledoc """
  Test for string interpolation functionality with Lipex expressions.
  """
  
  import Lipex
  import ExUnit.CaptureIO
  
  describe "string interpolation with lipex expressions" do
    test "variable assignment and string interpolation work" do
      # Capture the output from the lipex expression
      output = capture_io(fn ->
        deflipex beginliteral
          (= x 123)
          (IO.puts "x: #{(+ 3 4)}")
        endliteral
      end)
      
      # The output should show the interpolated arithmetic result
      assert output =~ "x: 7"
    end
    
    test "variable assignment works independently" do
      deflipex beginliteral (= y 456) endliteral
      assert y == 456
    end
    
    test "arithmetic in string interpolation works" do
      output = capture_io(fn ->
        deflipex beginliteral (IO.puts "result: #{(* 6 7)}") endliteral
      end)
      
      assert output =~ "result: 42"
    end
  end
end