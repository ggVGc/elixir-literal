defmodule WorkingLipexTest do
  use ExUnit.Case, async: true
  import Lipex

  @moduledoc """
  Tests for the working features of Lipex syntax.
  
  This test suite focuses only on features that are confirmed to work,
  providing a solid foundation for understanding what Lipex currently supports.
  
  Note: Run with ../bin/elixir -S mix test to support sequence_literal syntax
  """

  describe "basic arithmetic operations" do
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

  describe "comparison operations" do
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

  describe "logical operations" do
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

  describe "nested expressions" do
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

  describe "map operations" do
    test "simple map creation works" do
      result = deflipex ~~((% name john age 30))
      expected = %{name: :john, age: 30}
      assert result == expected
    end

    test "map with different data types works" do
      result = deflipex ~~((% active true count 5))
      expected = %{active: true, count: 5}
      assert result == expected
    end

    test "nested map creation works" do
      result = deflipex ~~((% outer (% inner 42)))
      expected = %{outer: %{inner: 42}}
      assert result == expected
    end

    test "map with computed values works" do
      result = deflipex ~~((% sum (+ 10 20) product (* 3 4)))
      expected = %{sum: 30, product: 12}
      assert result == expected
    end
  end

  describe "conditional expressions" do
    test "if true condition works" do
      assert deflipex(~~((if true yes no))) == :yes
    end

    test "if false condition works" do
      assert deflipex(~~((if false yes no))) == :no
    end

    test "if with comparison works" do
      assert deflipex(~~((if (> 5 3) greater lesser))) == :greater
      assert deflipex(~~((if (< 5 3) greater lesser))) == :lesser
    end

    test "nested if expressions work" do
      result = deflipex ~~((if (> 10 5) 
        (if (< 3 7) both_true first_true)
        first_false))
      assert result == :both_true
    end
  end
end