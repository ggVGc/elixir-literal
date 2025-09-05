defmodule LipexBasicTest do
  use ExUnit.Case, async: true
  
  @moduledoc """
  Basic tests for the Lipex implementation.
  
  Tests core functionality including:
  - Data structures (maps, tuples, lists)
  - Arithmetic operations
  - Logical operations
  - Control flow
  """
  
  import Lipex
  
  describe "arithmetic operations" do
    test "addition works correctly" do
      result = deflipex beginliteral (+ 1 2 3) endliteral
      assert result == 6
    end
    
    test "multiplication works correctly" do
      result = deflipex beginliteral (* 2 3 4) endliteral
      assert result == 24
    end
    
    test "subtraction works correctly" do
      result = deflipex beginliteral (- 10 3) endliteral
      assert result == 7
    end
    
    test "division works correctly" do
      result = deflipex beginliteral (/ 12 4) endliteral
      assert result == 3.0
    end
    
    test "less than comparison works correctly" do
      result = deflipex beginliteral (< 1 2) endliteral
      assert result == true
    end
    
    test "greater than or equal comparison works correctly" do
      result = deflipex beginliteral (>= 5 5) endliteral
      assert result == true
    end
  end
  
  describe "data structures" do
    test "map creation works correctly" do
      result = deflipex beginliteral (% :name "John" :age 30) endliteral
      assert result == %{name: "John", age: 30}
    end
    
    test "tuple creation works correctly" do
      result = deflipex beginliteral (tuple :ok :success) endliteral
      assert result == {:ok, :success}
    end
    
    test "list creation works correctly" do
      result = deflipex beginliteral (list 1 2 3) endliteral
      assert result == [1, 2, 3]
    end
  end
  
  describe "logical operations" do
    test "and operation works correctly" do
      result = deflipex beginliteral (and true true false) endliteral
      assert result == false
    end
    
    test "or operation works correctly" do
      result = deflipex beginliteral (or false true false) endliteral
      assert result == true
    end
    
    test "not operation works correctly" do
      result = deflipex beginliteral (not false) endliteral
      assert result == true
    end
    
    test "atom type checking works correctly" do
      result = deflipex beginliteral (atom? :hello) endliteral
      assert result == true
    end
    
    test "number type checking works correctly" do
      result = deflipex beginliteral (number? 42) endliteral
      assert result == true
    end
  end
  
  describe "control flow" do
    test "if expression with true condition works correctly" do
      result = deflipex beginliteral (if true :yes :no) endliteral
      assert result == :yes
    end
    
    test "if expression with false condition works correctly" do
      result = deflipex beginliteral (if false :yes :no) endliteral
      assert result == :no
    end
    
    test "nested expressions in if work correctly" do
      result = deflipex beginliteral (if (> (+ 2 3) 4) :big :small) endliteral
      assert result == :big
    end
  end
end