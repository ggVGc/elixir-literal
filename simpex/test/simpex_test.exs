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
  defsimpex ~~((def identity (x) x))
  defsimpex ~~((def second (_a b) b))
  defsimpex ~~((def get_number () 42))
  defsimpex ~~((def get_atom () :success))
  defsimpex ~~((def get_bool () true))
  defsimpex ~~((def echo (value) value))
  defsimpex ~~((def pick_first (a _b _c) a))

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
  end
end