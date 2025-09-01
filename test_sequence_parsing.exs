# Test file for ~~(...) sequence syntax parsing
# This tests the parsing of sequence expressions with ~~ prefix

defmodule SequenceParseTest do
  @moduledoc """
  Tests for ~~(...) sequence syntax parsing.
  This module contains tests to verify that sequence expressions
  like `~~(a b c)` are parsed correctly into the new AST node format.
  """

  def test_basic_sequence_parsing do
    # Test case 1: Basic two-argument sequence with ~~
    case Code.string_to_quoted("~~(foo bar)") do
      {:ok, {:sequence_literal, _meta, [{:foo, _, nil}, {:bar, _, nil}]}} ->
        IO.puts("✓ Basic sequence parsing works: ~~(foo bar)")
      {:ok, other_ast} ->
        IO.puts("✗ Basic sequence parsing failed. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Basic sequence parsing failed with error: #{inspect(reason)}")
    end
  end

  def test_three_argument_sequence do
    # Test case 2: Three-argument sequence with ~~
    case Code.string_to_quoted("~~(test hello world)") do
      {:ok, {:sequence_literal, _meta, [{:test, _, nil}, {:hello, _, nil}, {:world, _, nil}]}} ->
        IO.puts("✓ Three-argument sequence parsing works: ~~(test hello world)")
      {:ok, other_ast} ->
        IO.puts("✗ Three-argument sequence parsing failed. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Three-argument sequence parsing failed with error: #{inspect(reason)}")
    end
  end

  def test_long_sequence do
    # Test case 3: Longer sequence with ~~
    case Code.string_to_quoted("~~(a b c d e)") do
      {:ok, {:sequence_literal, _meta, [{:a, _, nil}, {:b, _, nil}, {:c, _, nil}, {:d, _, nil}, {:e, _, nil}]}} ->
        IO.puts("✓ Long sequence parsing works: ~~(a b c d e)")
      {:ok, other_ast} ->
        IO.puts("✗ Long sequence parsing failed. Got AST: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Long sequence parsing failed with error: #{inspect(reason)}")
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
    IO.puts("Running ~~(...) sequence syntax parsing tests...")
    IO.puts("")

    test_basic_sequence_parsing()
    test_three_argument_sequence()
    test_long_sequence()
    test_regular_call_still_works()
    test_single_argument_still_works()
    test_parenthesized_expression_still_works()

    IO.puts("")
    IO.puts("Test run complete.")
    quote do ~~(def diddle = ("asd" y z) (+x y z) =) end
    |> IO.inspect(label: "yeo")
  end
end

# Run the tests
SequenceParseTest.run_all_tests()
