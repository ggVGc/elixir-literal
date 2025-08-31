# Test minimal sequence syntax implementation

defmodule MinimalSequenceTest do
  def test_simple_sequence do
    IO.puts("Testing ~~(a b c) with new sequence syntax...")

    case Code.string_to_quoted("~~(a b c)") do
      {:ok, {:sequence_literal, _meta, [{:a, _meta2, nil}, {:b, _meta3, nil}, {:c, _meta4, nil}]}} ->
        IO.puts("✓ New sequence parsing works!")
      {:ok, other_ast} ->
        IO.puts("✗ Expected sequence_literal pattern, got: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Parse error: #{inspect(reason)}")
    end
  end

  def test_regular_parens_still_work do
    IO.puts("\nTesting that (1 + 2) still works...")

    case Code.string_to_quoted("(1 + 2)") do
      {:ok, {:+, _meta, [1, 2]}} ->
        IO.puts("✓ Regular parenthesized expressions still work")
      {:ok, other_ast} ->
        IO.puts("✗ Regular parens broken: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Parse error: #{inspect(reason)}")
    end
  end

  def test_function_calls_still_work do
    IO.puts("\nTesting that foo(1) still works...")

    case Code.string_to_quoted("foo(1)") do
      {:ok, {:foo, _meta, [1]}} ->
        IO.puts("✓ Regular function calls still work")
      {:ok, other_ast} ->
        IO.puts("✗ Function calls broken: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Parse error: #{inspect(reason)}")
    end
  end

  def test_new_sequence_syntax do
    IO.puts("\nTesting that ~~(foo bar) works...")

    case Code.string_to_quoted("~~(foo bar)") do
      {:ok, {:sequence_literal, _meta, [{:foo, _meta2, nil}, {:bar, _meta3, nil}]}} ->
        IO.puts("✓ New sequence syntax works")
      {:ok, other_ast} ->
        IO.puts("✗ New sequence syntax broken: #{inspect(other_ast)}")
      {:error, reason} ->
        IO.puts("✗ Parse error: #{inspect(reason)}")
    end
  end

  def run_all_tests do
    IO.puts("Testing new ~~(...) sequence syntax implementation...")
    test_simple_sequence()
    test_new_sequence_syntax()
    test_regular_parens_still_work()
    test_function_calls_still_work()
    IO.puts("\nMinimal test complete.")
  end
end

MinimalSequenceTest.run_all_tests()
