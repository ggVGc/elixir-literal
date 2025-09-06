defmodule MinimalWorkingTest do
  use ExUnit.Case, async: true
  import Lipex

  @moduledoc """
  Minimal tests for confirmed working Lipex features only.
  
  This test suite includes only the most basic operations that are 
  confirmed to work without any syntax or parsing issues.
  
  Note: Run with ../bin/elixir -S mix test to support sequence_literal syntax
  """

  describe "basic arithmetic operations that work" do
    test "simple addition works" do
      assert deflipex(~~((+ 1 2))) == 3
    end

    test "simple subtraction works" do
      assert deflipex(~~((- 10 3))) == 7
    end

    test "simple multiplication works" do
      assert deflipex(~~((* 2 3))) == 6
    end

    test "simple division works" do
      assert deflipex(~~((/ 12 4))) == 3.0
    end

    test "multiple operands work" do
      assert deflipex(~~((+ 1 2 3 4))) == 10
      assert deflipex(~~((* 2 3 4))) == 24
    end
  end

  describe "comparison operations that work" do
    test "less than works" do
      assert deflipex(~~((< 1 2))) == true
      assert deflipex(~~((< 3 2))) == false
    end

    test "greater than works" do
      assert deflipex(~~((> 5 3))) == true
      assert deflipex(~~((> 2 5))) == false
    end

    test "equality works" do
      assert deflipex(~~((== 5 5))) == true
      assert deflipex(~~((== 3 4))) == false
    end

    test "less than or equal works" do
      assert deflipex(~~((<= 3 3))) == true
      assert deflipex(~~((<= 3 4))) == true
      assert deflipex(~~((<= 4 3))) == false
    end

    test "greater than or equal works" do
      assert deflipex(~~((>= 7 7))) == true
      assert deflipex(~~((>= 8 7))) == true
      assert deflipex(~~((>= 6 7))) == false
    end

    test "not equal works" do
      assert deflipex(~~((!= 4 5))) == true
      assert deflipex(~~((!= 5 5))) == false
    end
  end

  describe "logical operations that work" do
    test "logical and works" do
      assert deflipex(~~((and true true))) == true
      assert deflipex(~~((and true false))) == false
      assert deflipex(~~((and false true))) == false
      assert deflipex(~~((and false false))) == false
    end

    test "logical or works" do
      assert deflipex(~~((or false true))) == true
      assert deflipex(~~((or true false))) == true
      assert deflipex(~~((or true true))) == true
      assert deflipex(~~((or false false))) == false
    end

    test "logical not works" do
      assert deflipex(~~((not true))) == false
      assert deflipex(~~((not false))) == true
    end
  end

  describe "nested expressions that work" do
    test "arithmetic nesting works" do
      result = deflipex ~~((+ (* 2 3) (/ 12 4)))
      assert result == 9.0  # 6 + 3.0 = 9.0
    end

    test "complex nesting works" do
      result = deflipex ~~((* (+ 1 2) (- 10 (* 2 3))))
      assert result == 12  # 3 * (10 - 6) = 12
    end

    test "comparison with arithmetic works" do
      assert deflipex(~~((> (+ 2 3) 4))) == true
      assert deflipex(~~((< (* 2 2) 5))) == true
    end

    test "logical with comparisons works" do
      assert deflipex(~~((and (> 5 3) (< 2 4)))) == true
      assert deflipex(~~((or (< 5 3) (> 2 4)))) == false
    end
  end

  describe "very complex nested expressions" do
    test "deeply nested arithmetic works" do
      # Test complex expression from the original comprehensive test
      result = deflipex ~~((+ (* (+ 2 3) (- 10 6)) (/ 100 (+ 20 5))))
      
      # (2+3) * (10-6) = 5 * 4 = 20
      # 100 / (20+5) = 100 / 25 = 4.0  
      # Total: 20 + 4.0 = 24.0
      assert result == 24.0
    end

    test "mixed arithmetic and logic works" do
      result = deflipex ~~((and (> (* 3 4) 10) (< (+ 2 2) 5)))
      # (* 3 4) = 12 > 10 = true
      # (+ 2 2) = 4 < 5 = true  
      # true and true = true
      assert result == true
    end
  end
end