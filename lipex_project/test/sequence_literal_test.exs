defmodule SequenceLiteralTest do
  use ExUnit.Case, async: true

  @moduledoc """
  Tests for sequence literal parsing functionality.
  These tests verify that the sequence literal syntax works correctly
  with the current tokenizer and parser implementation.
  """

  describe "basic sequence literals" do
    test "simple identifiers parse correctly" do
      ast = quote do
        beginliteral hello world endliteral
      end
      assert {:sequence_literal, _, _} = ast
    end

    test "single parentheses with identifiers" do
      ast = quote do
        beginliteral a b c endliteral
      end
      assert {:sequence_literal, _, _} = ast
    end
  end

  describe "nested parentheses" do
    test "nested parentheses parse correctly" do
      ast = quote do
        beginliteral (a b c) endliteral
      end
      assert {:sequence_literal, _, [{:sequence_paren, _, _}]} = ast
    end

    test "deeply nested parentheses work" do
      ast = quote do
        beginliteral (outer (inner val)) endliteral
      end
      assert {:sequence_literal, _, [{:sequence_paren, _, _}]} = ast
    end

    test "mixed nesting in brackets" do
      ast = quote do
        beginliteral [a (b c) d] endliteral
      end
      assert {:sequence_literal, _, [{:sequence_bracket, _, _}]} = ast
    end
  end

  describe "different delimiters" do
    test "brackets create sequence_bracket" do
      ast = quote do
        beginliteral [x y z] endliteral
      end
      assert {:sequence_literal, _, [{:sequence_bracket, _, _}]} = ast
    end

    test "braces create sequence_brace" do
      ast = quote do
        beginliteral {x y z} endliteral
      end
      assert {:sequence_literal, _, [{:sequence_brace, _, _}]} = ast
    end
  end

  describe "operators and special tokens" do
    test "operators as identifiers work" do
      ast = quote do
        beginliteral + a b endliteral
      end
      assert {:sequence_literal, _, _} = ast
    end

    test "comparison operators work" do
      ast = quote do
        beginliteral < x y endliteral
      end
      assert {:sequence_literal, _, _} = ast
    end
  end

  describe "complex nested structures" do
    test "multiple levels of nesting" do
      ast = quote do
        beginliteral (a (b (c d) e) f) endliteral
      end
      assert {:sequence_literal, _, [{:sequence_paren, _, _}]} = ast
    end

    test "mixed delimiter nesting" do
      ast = quote do
        beginliteral [{a (b c) d}] endliteral
      end
      assert {:sequence_literal, _, [{:sequence_bracket, _, _}]} = ast
    end
  end
end
