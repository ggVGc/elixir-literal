#!/usr/bin/env elixir

# Require all lipex modules
Code.require_file("lipex/lipex.ex")
Code.require_file("lipex/core/data_structures.ex")
Code.require_file("lipex/core/arithmetic.ex")
Code.require_file("lipex/core/logic.ex")
Code.require_file("lipex/core/control_flow.ex")
Code.require_file("lipex/functions/calls.ex")

defmodule LipexBasicTest do
  @moduledoc """
  Basic tests for the Lipex implementation.
  
  Tests core functionality including:
  - Data structures (maps, tuples, lists)
  - Arithmetic operations
  - Logical operations
  - Control flow
  """
  
  import Lipex
  
  def run_all_tests do
    IO.puts("=== Lipex Basic Test Suite ===\n")
    
    test_arithmetic()
    test_data_structures()
    test_logic_operations()
    test_control_flow()
    
    IO.puts("\n=== All basic tests completed! ===")
  end
  
  def test_arithmetic do
    IO.puts("--- Arithmetic Operations ---")
    
    # Basic arithmetic
    result1 = deflipex ~~((+ 1 2 3))
    IO.puts("(+ 1 2 3) = #{result1}")  # Should be 6
    
    result2 = deflipex ~~((* 2 3 4))
    IO.puts("(* 2 3 4) = #{result2}")  # Should be 24
    
    result3 = deflipex ~~((- 10 3))
    IO.puts("(- 10 3) = #{result3}")  # Should be 7
    
    result4 = deflipex ~~((/ 12 4))
    IO.puts("(/ 12 4) = #{result4}")  # Should be 3.0
    
    # Comparisons
    result5 = deflipex ~~((< 1 2))
    IO.puts("(< 1 2) = #{result5}")  # Should be true
    
    result6 = deflipex ~~((>= 5 5))
    IO.puts("(>= 5 5) = #{result6}")  # Should be true
    
    IO.puts("")
  end
  
  def test_data_structures do
    IO.puts("--- Data Structures ---")
    
    # Maps
    map_result = deflipex ~~((% :name "John" :age 30))
    IO.puts("(% :name \"John\" :age 30) = #{inspect(map_result)}")
    
    # Tuples  
    tuple_result = deflipex ~~((tuple :ok :success))
    IO.puts("(tuple :ok :success) = #{inspect(tuple_result)}")
    
    # Lists
    list_result = deflipex ~~((list 1 2 3))
    IO.puts("(list 1 2 3) = #{inspect(list_result)}")
    
    IO.puts("")
  end
  
  def test_logic_operations do
    IO.puts("--- Logical Operations ---")
    
    # Boolean logic
    and_result = deflipex ~~((and true true false))
    IO.puts("(and true true false) = #{and_result}")  # Should be false
    
    or_result = deflipex ~~((or false true false))
    IO.puts("(or false true false) = #{or_result}")  # Should be true
    
    not_result = deflipex ~~((not false))
    IO.puts("(not false) = #{not_result}")  # Should be true
    
    # Type checking
    atom_check = deflipex ~~((atom? :hello))
    IO.puts("(atom? :hello) = #{atom_check}")  # Should be true
    
    number_check = deflipex ~~((number? 42))
    IO.puts("(number? 42) = #{number_check}")  # Should be true
    
    IO.puts("")
  end
  
  def test_control_flow do
    IO.puts("--- Control Flow ---")
    
    # If expressions
    if_result1 = deflipex ~~((if true :yes :no))
    IO.puts("(if true :yes :no) = #{if_result1}")  # Should be :yes
    
    if_result2 = deflipex ~~((if false :yes :no))
    IO.puts("(if false :yes :no) = #{if_result2}")  # Should be :no
    
    # Nested expressions
    nested_result = deflipex ~~((if (> (+ 2 3) 4) :big :small))
    IO.puts("(if (> (+ 2 3) 4) :big :small) = #{nested_result}")  # Should be :big
    
    IO.puts("")
  end
end

# Run the tests
LipexBasicTest.run_all_tests()