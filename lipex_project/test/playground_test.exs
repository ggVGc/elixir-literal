defmodule LipexPlaygroundTest do
  use ExUnit.Case, async: true
  
  @moduledoc """
  Test for function definition with variable assignment and function calls.
  """
  
  import Lipex
  import ExUnit.CaptureIO
  
  describe "function definition with variable assignment" do
    test "function definition with variable works" do
      # Define a variable and then define a function using it
      pu = IO.puts
      
      # Define a function that uses the variable
      deflipex beginliteral (def test_run () (pu 132)) endliteral
      
      # Test that the function works by capturing its output
      output = capture_io(fn ->
        test_run()
      end)
      
      assert output =~ "132"
    end
    
    test "function definition with no parameters works" do
      # Define a simple function with no parameters
      deflipex beginliteral (def simple_func () 42) endliteral
      
      # Test that it returns the expected value
      assert simple_func() == 42
    end
  end
end