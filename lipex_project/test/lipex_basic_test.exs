defmodule LipexBasicTest do
  use ExUnit.Case, async: true
  import Lipex

  @moduledoc """
  Basic tests for the Lipex implementation.
  
  Tests core functionality including:
  - Data structures (maps, tuples, lists)  
  - Arithmetic operations
  - Logical operations
  - Control flow
  """

  describe "arithmetic operations" do
    test "addition works" do
      result = deflipex beginliteral (+ 1 2 3) endliteral
      assert result == 6
    end

    test "multiplication works" do
      result = deflipex beginliteral (* 2 3 4) endliteral
      assert result == 24
    end

    test "subtraction works" do
      result = deflipex beginliteral (- 10 3) endliteral
      assert result == 7
    end

    test "division works" do
      result = deflipex beginliteral (/ 12 4) endliteral
      assert result == 3.0
    end

    test "less than comparison works" do
      result = deflipex beginliteral (< 1 2) endliteral
      assert result == true
    end

    test "greater than or equal comparison works" do
      result = deflipex beginliteral (>= 5 5) endliteral
      assert result == true
    end
  end

  describe "data structures" do
    test "maps work" do
      map_result = deflipex beginliteral (% :name "John" :age 30) endliteral
      assert map_result == %{name: "John", age: 30}
    end

    test "tuples work" do
      tuple_result = deflipex beginliteral (tuple :ok :success) endliteral
      assert tuple_result == {:ok, :success}
    end

    test "lists work" do
      list_result = deflipex beginliteral (list 1 2 3) endliteral
      assert list_result == [1, 2, 3]
    end
  end

  describe "logical operations" do
    test "and operation works" do
      and_result = deflipex beginliteral (and true true false) endliteral
      assert and_result == false
    end

    test "or operation works" do
      or_result = deflipex beginliteral (or false true false) endliteral
      assert or_result == true
    end

    test "not operation works" do
      not_result = deflipex beginliteral (not false) endliteral
      assert not_result == true
    end

    test "atom? type check works" do
      atom_check = deflipex beginliteral (atom? :hello) endliteral
      assert atom_check == true
    end

    test "number? type check works" do
      number_check = deflipex beginliteral (number? 42) endliteral
      assert number_check == true
    end
  end

  describe "control flow" do
    test "if with true condition works" do
      if_result = deflipex beginliteral (if true :yes :no) endliteral
      assert if_result == :yes
    end

    test "if with false condition works" do
      if_result = deflipex beginliteral (if false :yes :no) endliteral
      assert if_result == :no
    end

    test "nested expressions work" do
      nested_result = deflipex beginliteral (if (> (+ 2 3) 4) :big :small) endliteral
      assert nested_result == :big
    end
  end
end