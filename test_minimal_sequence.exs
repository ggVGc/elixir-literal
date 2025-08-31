# Test minimal sequence syntax implementation

defmodule MinimalSequenceTest do
  def test_simple_sequence do
    IO.puts("Testing (a b) with minimal parser support...")
    
    case Code.string_to_quoted("(a b)") do
      {:ok, {:a, _meta, [{:b, _meta2, nil}]}} ->
        IO.puts("✓ Minimal sequence parsing works!")
        IO.puts("  First identifier: a")
        IO.puts("  Second identifier: b") 
      {:ok, other_ast} ->
        IO.puts("✗ Expected function call pattern, got: #{inspect(other_ast)}")
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
  
  def run_all_tests do
    IO.puts("Testing minimal sequence syntax implementation...")
    test_simple_sequence()
    test_regular_parens_still_work() 
    test_function_calls_still_work()
    IO.puts("\nMinimal test complete.")
  end
end

MinimalSequenceTest.run_all_tests()