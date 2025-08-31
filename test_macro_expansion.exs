# Test macro expansion with sequence syntax

defmodule MacroExpansionTest do
  defmacro test_sequence({:sequence_literal, _meta, [name | args]}) do
    IO.puts("Sequence macro called with name: #{name}, args: #{inspect(args)}")
    quote do
      {unquote(name), unquote_splicing(args)}
    end
  end

  defmacro test_sequence(other) do
    IO.puts("Regular macro called with: #{inspect(other)}")
    other
  end

  def test_expansion do
    IO.puts("=== Testing Macro Expansion with Sequences ===")
    Code.eval_file("lib/elixir/lib/kernel/sequence.ex")

    # Test transforming and expanding a sequence
    ast = {:a, [line: 1], [{:b, [line: 1], nil}]}
    transformed = Kernel.Sequence.maybe_transform_sequence(ast)

    IO.puts("Original AST: #{inspect(ast)}")
    IO.puts("Transformed: #{inspect(transformed)}")

    # Test pattern matching against sequence_literal
    case transformed do
      {:sequence_literal, _meta, [name | args]} ->
        IO.puts("✓ Sequence detected - name: #{name}, args: #{inspect(args)}")
        IO.puts("✓ This could be handled by a macro that matches sequence_literal patterns")
      other ->
        IO.puts("✗ Not a sequence literal: #{inspect(other)}")
    end
  end

  def run_all_tests do
    IO.puts("Testing macro expansion with sequence syntax...\n")
    test_expansion()
    IO.puts("\nMacro expansion tests complete.")
  end
end

MacroExpansionTest.run_all_tests()
