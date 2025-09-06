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

  test "sequence literals work in direct parsing" do
    # Direct parsing should work and produce sequence_literal AST
    result = parse!("~~(+ 1 2 3)")

    assert {:sequence_literal, [line: 1], [{:sequence_prefix, {:+, [line: 1], nil}, [1, 2, 3]}]} =
             result
  end

  test "sequence literals work within quote blocks" do
    # Quote block parsing should work and wrap sequence_literal in quote AST
    result = parse!("quote do ~~(+ 1 2 3) end")

    expected_inner =
      {:sequence_literal, [line: 1], [{:sequence_prefix, {:+, [line: 1], nil}, [1, 2, 3]}]}

    assert {:quote, [line: 1], [[do: ^expected_inner]]} = result
  end

  describe "sequence literals with simplified tokenizer" do
    # The simplified tokenizer produces sequence_token for identifiers, keywords, and operators
    # while preserving sequence_number, sequence_string, and sequence_atom.
    # The parser now works correctly with these simplified tokens.

    test "empty sequence works" do
      assert parse!("~~()") == {:sequence_literal, [line: 1], []}
    end

    test "basic identifiers work with simplified tokenizer" do
      # The tokenizer produces sequence_token which the parser now handles correctly
      assert parse!("~~(hello)") == {:sequence_literal, [line: 1], [{:hello, [line: 1], nil}]}

      assert parse!("~~(a b)") ==
               {:sequence_literal, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}

      assert parse!("~~(hello_world foo_bar)") ==
               {:sequence_literal, [line: 1],
                [{:hello_world, [line: 1], nil}, {:foo_bar, [line: 1], nil}]}
    end

    test "mixed case identifiers work with simplified tokenizer" do
      # Both produce sequence_token tokens that parser now handles correctly
      assert parse!("~~(CamelCase snake_case)") ==
               {:sequence_literal, [line: 1],
                [{:CamelCase, [line: 1], nil}, {:snake_case, [line: 1], nil}]}
    end

    test "dot notation identifiers work as single tokens" do
      # These now produce sequence_token tokens with dotted names like 'IO.puts'
      # and the parser handles them correctly as single atoms
      assert parse!("~~(IO.puts)") ==
               {:sequence_literal, [line: 1], [{:"IO.puts", [line: 1], nil}]}

      assert parse!("~~(String.upcase)") ==
               {:sequence_literal, [line: 1], [{:"String.upcase", [line: 1], nil}]}

      assert parse!("~~(GenServer.start_link)") ==
               {:sequence_literal, [line: 1], [{:"GenServer.start_link", [line: 1], nil}]}

      assert parse!("~~(io.puts data)") ==
               {:sequence_literal, [line: 1],
                [{:"io.puts", [line: 1], nil}, {:data, [line: 1], nil}]}
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

    test "keywords work as simple tokens" do
      # Keywords like true, false, nil are now treated as simple tokens
      assert parse!("~~(true false nil)") ==
               {:sequence_literal, [line: 1],
                [{true, [line: 1], nil}, {false, [line: 1], nil}, {nil, [line: 1], nil}]}

      # When if appears first, it creates a sequence_prefix
      assert parse!("~~(if do end)") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:if, [line: 1], nil},
                   [{:do, [line: 1], nil}, {:end, [line: 1], nil}]}
                ]}
    end

    test "operators work as simple tokens or prefixes" do
      # When operators appear first, they create sequence_prefix structures
      assert parse!("~~(+ - * /)") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:+, [line: 1], nil},
                   [{:-, [line: 1], nil}, {:*, [line: 1], nil}, {:/, [line: 1], nil}]}
                ]}

      # Even when operators are not first, they create sequence_prefix with empty args
      assert parse!("~~(a +)") ==
               {:sequence_literal, [line: 1],
                [{:a, [line: 1], nil}, {:sequence_prefix, {:+, [line: 1], nil}, []}]}
    end

    test "sequence operator still requires parentheses" do
      assert_syntax_error(["syntax error before: ", "foo"], "~~foo bar")
      assert_syntax_error(["syntax error before: ", "a"], "~~ a b c")
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
      assert parse!("x = ~~(foo bar)") ==
               {:=, [line: 1],
                [
                  {:x, [line: 1], nil},
                  {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
                ]}

      # Note: The tokenizer processes sequences independently, so complex expressions may not work as expected
      # This is because ~~(a b) is processed as one complete unit
      assert parse!("~~(a b)") ==
               {:sequence_literal, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
    end
  end

  describe "sequence literals with structural elements" do
    test "brackets work correctly" do
      # Square brackets create sequence_bracket structures
      assert parse!("~~([a b c])") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_bracket, [line: 1],
                   [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
                ]}

      # Empty brackets work
      assert parse!("~~([])") ==
               {:sequence_literal, [line: 1], [{:sequence_bracket, [line: 1], []}]}
    end

    test "braces work correctly" do
      # Curly braces create sequence_brace structures
      assert parse!("~~({x y z})") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_brace, [line: 1],
                   [{:x, [line: 1], nil}, {:y, [line: 1], nil}, {:z, [line: 1], nil}]}
                ]}

      # Empty braces work
      assert parse!("~~({})") ==
               {:sequence_literal, [line: 1], [{:sequence_brace, [line: 1], []}]}
    end

    test "operator prefixes work correctly" do
      # When an operator appears first, it creates a sequence_prefix structure
      assert parse!("~~(+ a b)") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:+, [line: 1], nil},
                   [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
                ]}

      assert parse!("~~(+ 1 2)") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:+, [line: 1], nil}, [1, 2]}
                ]}

      assert parse!("~~(- x y)") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:-, [line: 1], nil},
                   [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}
                ]}

      assert parse!("~~(* foo bar)") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_prefix, {:*, [line: 1], nil},
                   [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
                ]}
    end

    test "mixed structural elements work" do
      # Mix brackets, braces, and regular tokens
      assert parse!("~~([a b] {c d})") ==
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
      assert parse!("~~((a b))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}]}

      # Single element in nested parentheses
      assert parse!("~~((a))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:a, [line: 1], nil}]}]}

      # Empty nested parentheses create empty sequence_paren
      assert parse!("~~(())") ==
               {:sequence_literal, [line: 1], [{:sequence_paren, [line: 1], []}]}

      # Nested sequence literals still don't work due to ~~ being treated as token
      assert_syntax_error(["syntax error before:"], "~~(a ~~(b) c)")
    end

    test "whitespace handling" do
      # Multiple spaces should be handled correctly
      assert parse!("~~(a    b     c)") ==
               {:sequence_literal, [line: 1],
                [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
    end

    test "long sequences work" do
      # Test longer sequences
      assert parse!("~~(a b c d e f g h i j k l m n o p)") ==
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
    # These tests verify that our tokenizer produces the expected token types

    defp tokenize(string) do
      {:ok, _line, _column, _warnings, tokens, []} =
        :elixir_tokenizer.tokenize(String.to_charlist(string), 1, [])

      Enum.reverse(tokens)
    end

    test "sequence literals produce expected token types" do
      # Test that identifiers inside sequences become sequence_token
      tokens = tokenize("~~(hello)")

      assert Enum.any?(tokens, fn
               {:sequence_token, {_, _, _}, :hello} -> true
               _ -> false
             end)
    end

    test "sequence strings produce sequence_string tokens" do
      tokens = tokenize("~~(\"hello\")")

      assert Enum.any?(tokens, fn
               {:sequence_string, {_, _, _}, ~c"hello"} -> true
               _ -> false
             end)
    end

    test "sequence numbers produce sequence_number tokens" do
      tokens = tokenize("~~(42)")

      assert Enum.any?(tokens, fn
               {:sequence_number, {_, _, _}, 42} -> true
               _ -> false
             end)
    end

    test "sequence atoms produce sequence_atom tokens" do
      tokens = tokenize("~~(:foo)")

      assert Enum.any?(tokens, fn
               {:sequence_atom, {_, _, _}, :foo} -> true
               _ -> false
             end)
    end

    test "operators and keywords produce sequence_token tokens" do
      tokens = tokenize("~~(+ true)")

      # Both operator and keyword should be sequence_token
      plus_token =
        Enum.any?(tokens, fn
          {:sequence_token, {_, _, _}, :+} -> true
          _ -> false
        end)

      true_token =
        Enum.any?(tokens, fn
          {:sequence_token, {_, _, _}, true} -> true
          _ -> false
        end)

      assert plus_token
      assert true_token
    end

    test "dotted identifiers become single sequence_token tokens" do
      tokens = tokenize("~~(IO.puts)")

      assert Enum.any?(tokens, fn
               {:sequence_token, {_, _, _}, :"IO.puts"} -> true
               _ -> false
             end)
    end

    test "sequence literals produce sequence_begin and sequence_end tokens" do
      tokens = tokenize("~~(hello)")

      # Should have sequence_begin token
      has_begin =
        Enum.any?(tokens, fn
          {:sequence_begin, {_, _, _}, :"~~("} -> true
          _ -> false
        end)

      # Should have sequence_end token  
      has_end =
        Enum.any?(tokens, fn
          {:sequence_end, {_, _, _}, :")"} -> true
          _ -> false
        end)

      assert has_begin, "Expected sequence_begin token"
      assert has_end, "Expected sequence_end token"
    end

    test "bracket pairs produce sequence_block tokens" do
      # Test parentheses
      paren_tokens = tokenize("~~((a))")

      has_paren_block =
        Enum.any?(paren_tokens, fn
          {:sequence_block, {_, _, _}, :"()", _} -> true
          _ -> false
        end)

      assert has_paren_block, "Expected sequence_block token for parentheses"

      # Test square brackets
      bracket_tokens = tokenize("~~([a])")

      has_bracket_block =
        Enum.any?(bracket_tokens, fn
          {:sequence_block, {_, _, _}, :"[]", _} -> true
          _ -> false
        end)

      assert has_bracket_block, "Expected sequence_block token for brackets"

      # Test curly braces
      brace_tokens = tokenize("~~({a})")

      has_brace_block =
        Enum.any?(brace_tokens, fn
          {:sequence_block, {_, _, _}, :{}, _} -> true
          _ -> false
        end)

      assert has_brace_block, "Expected sequence_block token for braces"
    end

    test "tokenizer isolation - only sequence_* tokens inside sequences" do
      tokens = tokenize("~~(hello 123 :atom \"string\" + true)")

      # Filter out structural tokens (sequence_begin, sequence_end, etc.)
      content_tokens =
        Enum.filter(tokens, fn
          {:sequence_begin, {_, _, _}, _} -> false
          {:sequence_end, {_, _, _}, _} -> false
          {:sequence_block, {_, _, _}, _, _} -> false
          _ -> true
        end)

      # All content tokens should be sequence_* types
      all_sequence_types =
        Enum.all?(content_tokens, fn
          {:sequence_token, {_, _, _}, _} -> true
          {:sequence_number, {_, _, _}, _} -> true
          {:sequence_atom, {_, _, _}, _} -> true
          {:sequence_string, {_, _, _}, _} -> true
          _ -> false
        end)

      assert all_sequence_types,
             "Found non-sequence tokens inside sequence literal: #{inspect(content_tokens)}"
    end
  end
end
