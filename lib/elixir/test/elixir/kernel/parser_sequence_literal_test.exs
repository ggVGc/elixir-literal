Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ParserSequenceLiteralTest do
  use ExUnit.Case, async: true

  defp parse!(string), do: Code.string_to_quoted!(string)

  defp assert_syntax_error(given_messages, source) do
    e =
      try do
        parse!(source)
        flunk("Expected an exception but parsing succeeded")
      rescue
        e in [SyntaxError, TokenMissingError] -> e
      end

    assert_exception_msg(e, given_messages)
  end

  defp assert_exception_msg(exception, messages) do
    error_msg = Exception.format(:error, exception, [])

    for msg <- messages do
      assert error_msg =~ msg
    end
  end

  test "sequence literals in quote blocks - parser working with beginliteral/endliteral" do
    # This works (direct parsing)
    direct_result = parse!("beginliteral + 1 2 3 endliteral")
    assert {:sequence_literal, _, _} = direct_result

    # This now works with beginliteral/endliteral (the fix!)
    quote_result = parse!("quote do beginliteral + 1 2 3 endliteral end")

    # Verify the structure is correct
    assert {:quote, [line: 1],
            [
              [
                do:
                  {:sequence_literal, [line: 1],
                   [{:sequence_prefix, {:+, [line: 1], nil}, [1, 2, 3]}]}
              ]
            ]} = quote_result

    # Test other cases too
    assert parse!("quote do beginliteral hello world endliteral end") ==
             {:quote, [line: 1],
              [
                [
                  do:
                    {:sequence_literal, [line: 1],
                     [{:hello, [line: 1], nil}, {:world, [line: 1], nil}]}
                ]
              ]}

    assert parse!("quote do beginliteral endliteral end") ==
             {:quote, [line: 1], [[do: {:sequence_literal, [line: 1], []}]]}
  end

  describe "sequence literals with simplified tokenizer" do
    # The simplified tokenizer produces sequence_token for identifiers, keywords, and operators
    # while preserving sequence_number, sequence_string, and sequence_atom.
    # The parser now works correctly with these simplified tokens.

    test "empty sequence works" do
      assert parse!("beginliteral endliteral") == {:sequence_literal, [line: 1], []}
    end

    test "basic identifiers work with simplified tokenizer" do
      # The tokenizer produces regular tokens which the parser now handles correctly
      assert parse!("beginliteral hello endliteral") ==
               {:sequence_literal, [line: 1], [{:hello, [line: 1], nil}]}

      assert parse!("beginliteral a b endliteral") ==
               {:sequence_literal, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}

      assert parse!("beginliteral hello_world foo_bar endliteral") ==
               {:sequence_literal, [line: 1],
                [{:hello_world, [line: 1], nil}, {:foo_bar, [line: 1], nil}]}
    end

    test "mixed case identifiers work with simplified tokenizer" do
      # CamelCase causes syntax error
      assert_syntax_error(
        ["syntax error before:"],
        "beginliteral CamelCase snake_case endliteral"
      )

      # But regular snake_case works
      assert parse!("beginliteral snake_case endliteral") ==
               {:sequence_literal, [line: 1], [{:snake_case, [line: 1], nil}]}
    end

    test "atoms work correctly" do
      # Atoms are supported in sequence literals
      assert parse!("beginliteral :atom endliteral") ==
               {:sequence_literal, [line: 1], [:atom]}

      assert parse!("beginliteral :hello :world endliteral") ==
               {:sequence_literal, [line: 1], [:hello, :world]}
    end

    test "normal Elixir dot syntax still works outside sequences" do
      # Outside sequence literals, dots should still be parsed as separate tokens
      # This verifies we didn't break normal Elixir syntax
      assert parse!("IO.puts()") ==
               {{:., [line: 1], [{:__aliases__, [line: 1], [:IO]}, :puts]}, [line: 1], []}

      # Regular module calls should be unaffected
      assert parse!("String.upcase(\"test\")") ==
               {{:., [line: 1], [{:__aliases__, [line: 1], [:String]}, :upcase]}, [line: 1],
                ["test"]}
    end

    test "keywords are not allowed in sequence literals" do
      # Keywords like true, false, nil cause syntax errors in sequence literals
      assert_syntax_error(
        ["reserved word"],
        "beginliteral true false nil endliteral"
      )

      # Control flow keywords also cause errors
      assert_syntax_error(
        ["syntax error before:"],
        "beginliteral if do end endliteral"
      )
    end

    test "operators work as simple tokens or prefixes" do
      # When operators appear first, they create sequence_prefix structures
      assert parse!("beginliteral + - * / endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:+, [line: 1], nil},
                   [{:-, [line: 1], nil}, {:*, [line: 1], nil}, {:/, [line: 1], nil}]}
                ]}

      # Even when operators are not first, they create sequence_prefix with empty args
      assert parse!("beginliteral a + endliteral") ==
               {:sequence_literal, [line: 1],
                [{:a, [line: 1], nil}, {:sequence_prefix, {:+, [line: 1], nil}, []}]}
    end

    test "sequence literal requires endliteral" do
      assert_syntax_error(["missing terminator: endliteral"], "beginliteral foo bar")
      assert_syntax_error(["missing terminator: endliteral"], "beginliteral a b c")
    end

    test "backward compatibility - regular parentheses unaffected" do
      # Regular parenthesized expressions should still parse as before
      assert parse!("(1 + 2)") == {:+, [line: 1], [1, 2]}
      assert parse!("(x)") == {:x, [line: 1], nil}

      # Space-separated identifiers in regular parens parse as nested function calls
      assert parse!("(a b)") == {:a, [line: 1], [{:b, [line: 1], nil}]}
      assert parse!("(a b c)") == {:a, [line: 1], [{:b, [line: 1], [{:c, [line: 1], nil}]}]}
    end

    test "regular function calls unaffected" do
      assert parse!("foo()") == {:foo, [line: 1], []}
      assert parse!("foo(1, 2)") == {:foo, [line: 1], [1, 2]}
      assert parse!("foo bar") == {:foo, [line: 1], [{:bar, [line: 1], nil}]}
    end

    test "sequence literals work in larger expressions" do
      # Sequence literals should work in larger expressions now
      assert parse!("x = beginliteral foo bar endliteral") ==
               {:=, [line: 1],
                [
                  {:x, [line: 1], nil},
                  {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
                ]}

      # Note: The tokenizer processes sequences independently, so complex expressions may not work as expected
      # This is because beginliteral a b endliteral is processed as one complete unit
      assert parse!("beginliteral a b endliteral") ==
               {:sequence_literal, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
    end
  end

  describe "sequence literals with structural elements" do
    test "brackets work correctly" do
      # Square brackets create sequence_bracket structures
      assert parse!("beginliteral [a b c] endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_bracket, [line: 1],
                   [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
                ]}

      # Empty brackets work
      assert parse!("beginliteral [] endliteral") ==
               {:sequence_literal, [line: 1], [{:sequence_bracket, [line: 1], []}]}
    end

    test "braces work correctly" do
      # Curly braces create sequence_brace structures
      assert parse!("beginliteral {x y z} endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_brace, [line: 1],
                   [{:x, [line: 1], nil}, {:y, [line: 1], nil}, {:z, [line: 1], nil}]}
                ]}

      # Empty braces work
      assert parse!("beginliteral {} endliteral") ==
               {:sequence_literal, [line: 1], [{:sequence_brace, [line: 1], []}]}
    end

    test "operator prefixes work correctly" do
      # When an operator appears first, it creates a sequence_prefix structure
      assert parse!("beginliteral + a b endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:+, [line: 1], nil},
                   [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
                ]}

      assert parse!("beginliteral + 1 2 endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:+, [line: 1], nil}, [1, 2]}
                ]}

      assert parse!("beginliteral - x y endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:-, [line: 1], nil},
                   [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}
                ]}

      assert parse!("beginliteral * foo bar endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:*, [line: 1], nil},
                   [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
                ]}
    end

    test "mixed structural elements work" do
      # Mix brackets, braces, and regular tokens
      assert parse!("beginliteral [a b] {c d} endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_bracket, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]},
                  {:sequence_brace, [line: 1], [{:c, [line: 1], nil}, {:d, [line: 1], nil}]}
                ]}
    end
  end

  describe "edge cases and special scenarios" do
    test "nested parentheses work correctly" do
      # Simple nested parentheses now work - they create sequence_paren structures
      assert parse!("beginliteral (a b) endliteral") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}]}

      # Single element in nested parentheses
      assert parse!("beginliteral (a) endliteral") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:a, [line: 1], nil}]}]}

      # Empty nested parentheses create empty sequence_paren
      assert parse!("beginliteral () endliteral") ==
               {:sequence_literal, [line: 1], [{:sequence_paren, [line: 1], []}]}

      # Nested sequence literals still don't work due to beginliteral being treated as token inside
      assert_syntax_error(
        ["syntax error before:"],
        "beginliteral a beginliteral b endliteral c endliteral"
      )
    end

    test "whitespace handling" do
      # Multiple spaces should be handled correctly
      assert parse!("beginliteral a    b     c endliteral") ==
               {:sequence_literal, [line: 1],
                [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
    end

    test "long sequences work" do
      # Test longer sequences
      assert parse!("beginliteral a b c d e f g h i j k l m n o p endliteral") ==
               {:sequence_literal, [line: 1],
                [
                  {:a, [line: 1], nil},
                  {:b, [line: 1], nil},
                  {:c, [line: 1], nil},
                  {:d, [line: 1], nil},
                  {:e, [line: 1], nil},
                  {:f, [line: 1], nil},
                  {:g, [line: 1], nil},
                  {:h, [line: 1], nil},
                  {:i, [line: 1], nil},
                  {:j, [line: 1], nil},
                  {:k, [line: 1], nil},
                  {:l, [line: 1], nil},
                  {:m, [line: 1], nil},
                  {:n, [line: 1], nil},
                  {:o, [line: 1], nil},
                  {:p, [line: 1], nil}
                ]}
    end
  end

  describe "tokenizer behavior verification" do
    # These tests verify that our tokenizer produces regular tokens inside beginliteral/endliteral

    defp tokenize(string) do
      {:ok, _line, _column, _warnings, tokens, []} =
        :elixir_tokenizer.tokenize(String.to_charlist(string), 1, [])

      Enum.reverse(tokens)
    end

    test "sequence literals produce regular tokens" do
      # Test that identifiers inside sequences are regular identifier tokens
      tokens = tokenize("beginliteral hello endliteral")

      assert Enum.any?(tokens, fn
               {:identifier, {_, _, _}, :hello} -> true
               _ -> false
             end)
    end

    test "sequence strings produce regular string tokens" do
      tokens = tokenize("beginliteral \"hello\" endliteral")

      assert Enum.any?(tokens, fn
               {:bin_string, {_, _, _}, _} -> true
               _ -> false
             end)
    end

    test "sequence numbers produce regular number tokens" do
      tokens = tokenize("beginliteral 42 endliteral")

      assert Enum.any?(tokens, fn
               {:int, {_, _, _}, _} -> true
               _ -> false
             end)
    end

    test "sequence atoms produce regular atom tokens" do
      tokens = tokenize("beginliteral :foo endliteral")

      assert Enum.any?(tokens, fn
               {:atom, {_, _, _}, :foo} -> true
               _ -> false
             end)
    end

    test "operators and keywords produce regular tokens" do
      tokens = tokenize("beginliteral + true endliteral")

      # Operator should be dual_op and true should be true token
      plus_token =
        Enum.any?(tokens, fn
          {:dual_op, {_, _, _}, :+} -> true
          _ -> false
        end)

      true_token =
        Enum.any?(tokens, fn
          {true, {_, _, _}} -> true
          _ -> false
        end)

      assert plus_token
      assert true_token
    end

    test "dotted identifiers become separate tokens" do
      tokens = tokenize("beginliteral IO.puts endliteral")

      # Should have alias, dot, and identifier
      assert Enum.any?(tokens, fn
               {:alias, {_, _, _}, :IO} -> true
               _ -> false
             end)

      assert Enum.any?(tokens, fn
               {:., _} -> true
               _ -> false
             end)

      assert Enum.any?(tokens, fn
               {:identifier, {_, _, _}, :puts} -> true
               _ -> false
             end)
    end

    test "tokenizer produces regular tokens inside sequences" do
      tokens = tokenize("beginliteral hello 123 :atom \"string\" + true endliteral")

      # Filter out structural tokens (beginliteral, endliteral)
      content_tokens =
        Enum.filter(tokens, fn
          {:beginliteral, _} -> false
          {:endliteral, _} -> false
          _ -> true
        end)

      # We should have regular token types
      has_regular_types =
        Enum.any?(content_tokens, fn
          {:identifier, _, _} -> true
          {:int, _, _} -> true
          {:atom, _, _} -> true
          {:bin_string, _, _} -> true
          {:dual_op, _, _} -> true
          {true, _} -> true
          _ -> false
        end)

      assert has_regular_types,
             "Expected regular tokens inside sequence literal: #{inspect(content_tokens)}"
    end
  end
end
