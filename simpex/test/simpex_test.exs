defmodule SimpexTest do
  use ExUnit.Case
  import Simpex

  @moduledoc """
  Basic tests for the Simpex implementation.
  
  Tests core functionality including:
  - Data types (numbers, booleans, atoms)
  - Function definition
  - Function calls
  """

  # Define functions at module level
  defsimpex ~~(
    (def identity (x) x)
    (def second (_a b) b)
    (def get_number () 42)
    (def get_atom () :success)
    (def get_bool () true)
    (def echo (value) value)
    (def pick_first (a _b _c) a)
    (def pick_nth ({a _b} 0) a)
    (def pick_nth ({_a b} 1) b)
    (def pick_nth ([a _b] 0) a)
    (def pick_nth ([_ b] 1) b)
    (def get_map_value ((% :x val)) val)

    (def is_five (x) when (> x 5) true)
    (def is_five (_) false)
  )

  describe "data types" do
    test "numbers work" do
      result = defsimpex ~~(42)
      assert result == 42
    end

    test "floats work" do
      result = defsimpex ~~(3.14)
      assert result == 3.14
    end

    test "booleans work" do
      result_true = defsimpex ~~(true)
      assert result_true == true

      result_false = defsimpex ~~(false)
      assert result_false == false
    end
  end

  describe "function definitions and calls" do
    test "simple function works" do
      assert identity(123) == 123
    end

    test "function with multiple parameters works" do  
      assert second(1, 2) == 2
    end

    test "function with no parameters works" do
      assert get_number() == 42
      assert get_atom() == :success
      assert get_bool() == true
    end

    test "calling defined functions via simpex" do
      result = defsimpex ~~((identity 999))
      assert result == 999
    end

    test "calling with different data types" do
      assert (defsimpex ~~((echo 42))) == 42
      assert (defsimpex ~~((echo :atom))) == :atom
      assert (defsimpex ~~((echo true))) == true
    end

    test "calling with multiple arguments" do
      result = defsimpex ~~((pick_first 1 2 3))
      assert result == 1
    end

    test "calling with tuple pattern matching" do
      result = defsimpex ~~((pick_nth {5 10} 0))
      assert result == 5
    end

    test "calling with list pattern matching" do
      result1 = defsimpex ~~((pick_nth [7 14] 0))
      assert result1 == 7
      
      result2 = defsimpex ~~((pick_nth [7 14] 1))
      assert result2 == 14
    end

    test "tuple pattern matching works from multi-def block" do
      # Test the functions defined in the multi-expression block
      result1 = defsimpex ~~((pick_nth {3 6} 0))
      assert result1 == 3
      
      result2 = defsimpex ~~((pick_nth {3 6} 1))
      assert result2 == 6
    end

    test "assignment with pattern matching works" do
      defsimpex ~~((= a 123))
      assert a == 123

      defsimpex ~~((= [_fst x] [50 99]))
      assert x == 99

      defsimpex ~~((= {fst _} {50 99}))
      assert fst == 50
    end

    test "call global functions" do
      assert 3 == defsimpex ~~((abs -3))
      assert "YEO" == defsimpex ~~((String.upcase "yeo"))
    end

    test "assert inside simpex" do
      defsimpex ~~(
        (= a 1)
        (= b 1)
        (assert (== a b)))
    end

    test "elixir maps" do
      assert get_map_value(%{x: 123}) == 123

      assert (defsimpex ~~((% :x 12 :y "test"))) == %{x: 12, y: "test"}

      key = "yeo"
      value = :yep
      assert (defsimpex ~~((% key value))) == %{"yeo" => :yep}
    end

    test "case expression" do
      assert :yep  = defsimpex ~~(
        (= x 10)
        (case x
          (2 :nope)
          (10 :yep))
      )

        assert_raise CaseClauseError, fn ->
          defsimpex ~~(
            (case 123
              (0 :no_match)))
        end

        assert :original = defsimpex ~~(
          (case :original
            ("other" :not_this)
            (fallback fallback)))
    end

    test "if expression" do
      assert :yep == defsimpex ~~((if true :yep :nope))
      assert :nope == defsimpex ~~((if false :yep :nope))
      
      # Test with variable conditions
      assert :positive == defsimpex ~~(
        (= x 5)
        (if (> x 0) :positive :negative))
        
      # Test with nil (falsy in Elixir)
      assert :falsy == defsimpex ~~((if nil :truthy :falsy))
    end

    test "function with guard clauses" do
      # Test the is_five function which uses guards
      assert is_five(10) == true   # > 5, should match first clause
      assert is_five(3) == false   # <= 5, should match second clause
      assert is_five(5) == false   # == 5, should match second clause
      assert is_five(6) == true    # > 5, should match first clause
    end
  end
end
