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
      ~~((+ 1 3))
      ~~(1)
      assert a   == 4
    end

    # test "multiplication works" do
    #   result = deflipex ~~((* 2 3 4))
    #   assert result == 24
    # end

    # test "subtraction works" do
    #   result = deflipex ~~((- 10 3))
    #   assert result == 7
    # end

    # test "division works" do
    #   result = deflipex ~~((/ 12 4))
    #   assert result == 3.0
    # end

    # test "less than comparison works" do
    #   result = deflipex ~~((< 1 2))
    #   assert result == true
    # end

    # test "greater than or equal comparison works" do
    #   result = deflipex ~~((>= 5 5))
    #   assert result == true
    # end
  end

  # describe "data structures" do
  #   test "maps work" do
  #     map_result = deflipex ~~((% :name "John" :age 30))
  #     assert map_result == %{name: "John", age: 30}
  #   end

  #   test "tuples work" do
  #     tuple_result = deflipex ~~((tuple :ok :success))
  #     assert tuple_result == {:ok, :success}
  #   end

  #   test "lists work" do
  #     list_result = deflipex ~~((list 1 2 3))
  #     assert list_result == [1, 2, 3]
  #   end
  # end

  # describe "logical operations" do
  #   test "and operation works" do
  #     and_result = deflipex ~~((and true true false))
  #     assert and_result == false
  #   end

  #   test "or operation works" do
  #     or_result = deflipex ~~((or false true false))
  #     assert or_result == true
  #   end

  #   test "not operation works" do
  #     not_result = deflipex ~~((not false))
  #     assert not_result == true
  #   end

  #   test "atom? type check works" do
  #     atom_check = deflipex ~~((atom? :hello))
  #     assert atom_check == true
  #   end

  #   test "number? type check works" do
  #     number_check = deflipex ~~((number? 42))
  #     assert number_check == true
  #   end
  # end

  # describe "control flow" do
  #   test "if with true condition works" do
  #     if_result = deflipex ~~((if true :yes :no))
  #     assert if_result == :yes
  #   end

  #   test "if with false condition works" do
  #     if_result = deflipex ~~((if false :yes :no))
  #     assert if_result == :no
  #   end

  #   test "nested expressions work" do
  #     nested_result = deflipex ~~((if (> (+ 2 3) 4) :big :small))
  #     assert nested_result == :big
  #   end
  # end
end
