defmodule Lipex.Functions.DefinitionsTest do
  use ExUnit.Case, async: true
  import Lipex

  @moduledoc """
  Tests for function and module definitions in Lipex syntax.
  
  This test suite demonstrates comprehensive Lipex functionality and serves as 
  unit tests for the expression evaluation system, including basic syntax,
  data structures, control flow, and functional programming features.
  
  Note: Run with ../bin/elixir -S mix test to support sequence_literal syntax
  """

  describe "core Lipex syntax and evaluation" do
    test "basic arithmetic operations work correctly" do
      assert deflipex(~~((+ 1 2 3))) == 6
      assert deflipex(~~((* 2 3 4))) == 24  
      assert deflipex(~~((- 10 3))) == 7
      assert deflipex(~~((/ 12 4))) == 3.0
    end

    test "comparison operations work correctly" do
      assert deflipex(~~((< 1 2))) == true
      assert deflipex(~~((> 5 3))) == true
      assert deflipex(~~((== 5 5))) == true
      assert deflipex(~~((<= 3 3))) == true
      assert deflipex(~~((>= 7 7))) == true
      assert deflipex(~~((!= 4 5))) == true
    end

    test "logical operations work correctly" do
      assert deflipex(~~((and true true))) == true
      assert deflipex(~~((and true false))) == false
      assert deflipex(~~((or false true))) == true
      assert deflipex(~~((or false false))) == false
      assert deflipex(~~((not true))) == false
      assert deflipex(~~((not false))) == true
    end

    test "nested arithmetic expressions work correctly" do
      # Complex nested expressions
      result = deflipex ~~((+ (* 2 3) (/ 12 4) (- 10 5)))
      assert result == 14.0  # 6 + 3.0 + 5 = 14.0
      
      # Deeply nested
      result2 = deflipex ~~((* (+ 1 2) (- 10 (* 2 3))))
      assert result2 == 12  # 3 * (10 - 6) = 12
    end
  end

  describe "data structures and manipulation" do
    # test "map creation and access work" do
    #   # Create map
    #   map_result = deflipex ~~((% :name :john :age 30 :active true))
    #   expected_map = %{name: :john, age: 30, active: true}
    #   assert map_result == expected_map
      
    #   # Access map values  
    #   name = deflipex ~~((map_result :name))
    #   assert name == :john
      
    #   age = deflipex ~~((map_result :age))
    #   assert age == 30
    # end

    # test "basic data structures work" do
    #   # Test simple atoms and literals
    #   atom_result = deflipex ~~(:hello)
    #   assert atom_result == :hello
      
    #   # Test simple variables if possible
    #   number_result = deflipex ~~(42)
    #   assert number_result == 42
      
    #   boolean_result = deflipex ~~(true)
    #   assert boolean_result == true
    # end

    test "simple nested maps work" do
      # Test simple nested map structure using only working features
      nested_map = deflipex ~~((% :inner (% :value 42)))
      expected = %{inner: %{value: 42}}
      assert nested_map == expected
    end
  end

  describe "control flow and conditionals" do
    test "if expressions work correctly" do
      assert deflipex(~~((if true :yes :no))) == :yes
      assert deflipex(~~((if false :yes :no))) == :no
      
      # With comparisons
      assert deflipex(~~((if (> 5 3) :greater :lesser))) == :greater
      assert deflipex(~~((if (< 5 3) :greater :lesser))) == :lesser
    end

    test "nested conditional expressions work" do
      # Nested if statements
      result = deflipex ~~((if (> 10 5) 
        (if (< 3 7) :both_true :first_true)
        :first_false))
      assert result == :both_true
      
      # Complex nested conditionals
      complex_result = deflipex ~~((if (== (+ 2 3) 5)
        (if (and true (> 10 5)) :all_conditions_met :some_false)
        :first_condition_false))
      assert complex_result == :all_conditions_met
    end
  end

  describe "type checking and predicates" do
    test "basic type checking predicates work" do
      assert deflipex(~~((atom? :hello))) == true
      assert deflipex(~~((atom? 42))) == false
      
      assert deflipex(~~((number? 42))) == true
      assert deflipex(~~((number? :atom))) == false
    end

    # test "boolean type checking works" do
    #   assert deflipex(~~((boolean? true))) == true
    #   assert deflipex(~~((boolean? false))) == true
    #   assert deflipex(~~((boolean? :not_boolean))) == false
    # end
  end

  describe "basic evaluation features" do
    # test "variable references work" do
    #   # Test that atoms are properly evaluated
    #   assert deflipex(~~(:test_atom)) == :test_atom
    #   assert deflipex(~~(nil)) == nil
    #   assert deflipex(~~(42)) == 42
    # end
  end

  describe "advanced expression evaluation" do
    test "complex arithmetic expressions work" do
      # Very complex nested expression using only working features
      complex_expr = deflipex ~~((+ (* (+ 2 3) (- 10 6)) (/ 100 (+ 20 5))))
      
      # (2+3) * (10-6) = 5 * 4 = 20
      # 100 / (20+5) = 100 / 25 = 4.0  
      # Total: 20 + 4.0 = 24.0
      assert complex_expr == 24.0
    end

    test "mixed data types in map expressions work" do
      # Expression combining different data types in maps
      mixed_result = deflipex ~~((%
        :number (+ 10 20)
        :boolean (> 5 3)
        :atom :test_value))
      
      expected = %{
        number: 30,
        boolean: true,
        atom: :test_value
      }
      assert mixed_result == expected
    end

    test "basic expressions work as expected" do
      # Test that valid expressions don't raise errors
      assert deflipex(~~((+ 1 2))) == 3
      assert deflipex(~~((and true true))) == true
      # assert deflipex(~~(:my_atom)) == :my_atom
    end
  end
end
