# Test file for simplified parenthesized sequence syntax parsing
# This tests the parsing of sequence expressions in parentheses

defmodule SequenceParseTest do
  @moduledoc """
  Tests for parenthesized sequence syntax parsing.
  This module contains tests to verify that sequence expressions 
  like `(a b c)` are parsed correctly into the new AST node format.
  """
  
  def test_basic_sequence_parsing do
    # Test case 1: Basic two-argument sequence in parentheses
    case Code.string_to_quoted("(foo 123)") do
      {:ok, {:sequence_literal, _meta, [:foo, 123]}} ->
        IO.puts("✓ Basic sequence parsing works: (foo 123)")
      {:ok, other_ast} ->
        IO.puts("✗ Basic sequence parsing failed. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Basic sequence parsing failed with error: #{inspect(reason)}")
    end
  end
  
  def test_three_argument_sequence do
    # Test case 2: Three-argument sequence in parentheses
    case Code.string_to_quoted("(test 1 2)") do
      {:ok, {:sequence_literal, _meta, [:test, 1, 2]}} ->
        IO.puts("✓ Three-argument sequence parsing works: (test 1 2)")
      {:ok, other_ast} ->
        IO.puts("✗ Three-argument sequence parsing failed. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Three-argument sequence parsing failed with error: #{inspect(reason)}")
    end
  end
  
  def test_sequence_with_different_types do
    # Test case 3: Mixed argument types in parentheses
    case Code.string_to_quoted("(process :atom \"string\")") do
      {:ok, {:sequence_literal, _meta, [:process, :atom, "string"]}} ->
        IO.puts("✓ Mixed-type sequence parsing works: (process :atom \"string\")")
      {:ok, other_ast} ->
        IO.puts("✗ Mixed-type sequence parsing failed. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Mixed-type sequence parsing failed with error: #{inspect(reason)}")
    end
  end
  
  def test_regular_call_still_works do
    # Test case 4: Ensure regular function calls still work
    case Code.string_to_quoted("foo(123, 567)") do
      {:ok, {:foo, _meta, [123, 567]}} ->
        IO.puts("✓ Regular function call still works: foo(123, 567)")
      {:ok, other_ast} ->
        IO.puts("✗ Regular function call broken. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Regular function call failed with error: #{inspect(reason)}")
    end
  end
  
  def test_single_argument_still_works do
    # Test case 5: Single argument no-parens call should still work as before
    case Code.string_to_quoted("foo 123") do
      {:ok, {:foo, _meta, [123]}} ->
        IO.puts("✓ Single argument no-parens call still works: foo 123")
      {:ok, other_ast} ->
        IO.puts("✗ Single argument no-parens call broken. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Single argument no-parens call failed with error: #{inspect(reason)}")
    end
  end
  
  def test_parenthesized_expression_still_works do
    # Test case 6: Regular parenthesized expressions should still work
    case Code.string_to_quoted("(1 + 2)") do
      {:ok, {:+, _meta, [1, 2]}} ->
        IO.puts("✓ Regular parenthesized expressions still work: (1 + 2)")
      {:ok, other_ast} ->
        IO.puts("✗ Regular parenthesized expressions broken. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Regular parenthesized expressions failed with error: #{inspect(reason)}")
    end
  end
  
  def run_all_tests do
    IO.puts("Running parenthesized sequence syntax parsing tests...")
    IO.puts("")
    
    test_basic_sequence_parsing()
    test_three_argument_sequence()
    test_sequence_with_different_types()
    test_regular_call_still_works()
    test_single_argument_still_works()
    test_parenthesized_expression_still_works()
    
    IO.puts("")
    IO.puts("Test run complete.")
  end
end

# Run the tests
SequenceParseTest.run_all_tests()