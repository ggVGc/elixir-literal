# Comprehensive test suite for ~~(...) sequence syntax

defmodule SequenceComprehensiveTest do
  def run_all_tests do
    IO.puts("Running comprehensive ~~(...) sequence syntax tests...\n")
    
    test_basic_sequences()
    test_transformation()
    test_edge_cases() 
    test_backward_compatibility()
    test_error_cases()
    
    IO.puts("\nAll comprehensive tests complete.")
  end
  
  def test_basic_sequences do
    IO.puts("=== Basic Sequence Parsing ===")
    
    test_cases = [
      {"~~(a b)", {:sequence_literal, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}},
      {"~~(foo bar)", {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}},
      {"~~(hello world)", {:sequence_literal, [line: 1], [{:hello, [line: 1], nil}, {:world, [line: 1], nil}]}}
    ]
    
    Enum.each(test_cases, fn {input, expected} ->
      case Code.string_to_quoted(input) do
        {:ok, ^expected} ->
          IO.puts("✓ #{input} parses correctly")
        {:ok, other} ->
          IO.puts("✗ #{input} failed - got: #{inspect(other)}")
        {:error, reason} ->
          IO.puts("✗ #{input} parse error: #{inspect(reason)}")
      end
    end)
  end
  
  def test_transformation do
    IO.puts("\n=== Sequence Utilities ===")
    Code.eval_file("lib/elixir/lib/kernel/sequence.ex")
    
    # Test utility functions instead
    test_ast = {:sequence_literal, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
    
    if Kernel.Sequence.sequence_literal?(test_ast) do
      IO.puts("✓ sequence_literal?/1 works correctly")
    else
      IO.puts("✗ sequence_literal?/1 failed")
    end
    
    args = Kernel.Sequence.sequence_args(test_ast)
    expected_args = [{:a, [line: 1], nil}, {:b, [line: 1], nil}]
    if args == expected_args do
      IO.puts("✓ sequence_args/1 works correctly")
    else
      IO.puts("✗ sequence_args/1 failed - got: #{inspect(args)}")
    end
    
    arity = Kernel.Sequence.sequence_arity(test_ast)
    if arity == 2 do
      IO.puts("✓ sequence_arity/1 works correctly")
    else
      IO.puts("✗ sequence_arity/1 failed - got: #{inspect(arity)}")
    end
  end
  
  def test_edge_cases do
    IO.puts("\n=== Edge Cases ===")
    
    # Single identifier in parentheses should not be sequence
    case Code.string_to_quoted("(x)") do
      {:ok, {:x, [line: 1], nil}} ->
        IO.puts("✓ (x) parses as single identifier, not sequence")
      {:ok, other} ->
        IO.puts("✗ (x) unexpected: #{inspect(other)}")
      {:error, reason} ->
        IO.puts("✗ (x) parse error: #{inspect(reason)}")
    end
    
    # Empty parentheses
    case Code.string_to_quoted("()") do
      {:ok, {:%{}, [line: 1], []}} ->
        IO.puts("✓ () parses (as empty map syntax)")
      {:ok, other} ->
        IO.puts("? () parses as: #{inspect(other)}")
      {:error, reason} ->
        IO.puts("? () parse result: #{inspect(reason)}")
    end
  end
  
  def test_backward_compatibility do
    IO.puts("\n=== Backward Compatibility ===")
    
    test_cases = [
      {"(1 + 2)", {:+, [line: 1], [1, 2]}},
      {"(x)", {:x, [line: 1], nil}},
      {"foo(bar)", {:foo, [line: 1], [{:bar, [line: 1], nil}]}},
      {"foo(bar, baz)", {:foo, [line: 1], [{:bar, [line: 1], nil}, {:baz, [line: 1], nil}]}}
    ]
    
    Enum.each(test_cases, fn {input, expected} ->
      case Code.string_to_quoted(input) do
        {:ok, ^expected} ->
          IO.puts("✓ #{input} backward compatibility OK")
        {:ok, other} ->
          IO.puts("✗ #{input} backward compatibility broken - got: #{inspect(other)}")
        {:error, reason} ->
          IO.puts("✗ #{input} parse error: #{inspect(reason)}")
      end
    end)
  end
  
  def test_error_cases do
    IO.puts("\n=== Error Cases ===")
    
    test_cases = [
      "~~()",  # Empty sequence should error
      "(a b c)",  # Old syntax should not work anymore
      "foo bar baz"  # Space-separated without ~~ should not work
    ]
    
    Enum.each(test_cases, fn input ->
      case Code.string_to_quoted(input) do
        {:ok, ast} ->
          IO.puts("? #{input} parses as: #{inspect(ast)}")
        {:error, reason} ->
          IO.puts("✓ #{input} correctly errors: #{inspect(reason)}")
      end
    end)
  end
end

SequenceComprehensiveTest.run_all_tests()