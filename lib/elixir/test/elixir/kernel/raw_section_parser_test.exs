Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.RawSectionParserTest do
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
    # Direct parsing should work and produce raw_section AST
    result = parse!("~~(+ 1 2 3)")

    assert {:raw_section, [line: 1], [{:+, [line: 1], nil}, 1, 2, 3]} = result
  end

  describe "sequence literals work within quote blocks" do
    test "in a simple situation" do
      # Quote block parsing should work and wrap raw_section in quote AST
      result = parse!("quote do ~~(+ 1 2 3) end")

      expected_inner =
        {:raw_section, [line: 1], [{:+, [line: 1], nil}, 1, 2, 3]}

      assert {:quote, [line: 1], [[do: ^expected_inner]]} = result
    end

    test "in case expression" do
      expected_inner = {
        :raw_section,
        [line: 1],
        [{:case, [line: 1], nil}, 1, 2, 3]
      }

      assert {:quote, [line: 1], [[do: ^expected_inner]]} = parse!("quote do ~~(case 1 2 3) end")

      elixir_equivalent =
        quote do
          case 1 do
            2 -> 3
          end
        end

      assert {:case, [], [1, [do: [{:->, [], [[2], 3]}]]]} == elixir_equivalent
    end

    # TODO: This breaks formatter, currently.
    # test "in source-level quote block" do
    #   result = quote do ~~((+ 1 2)) end
    #   # Test that it returns a valid sequence literal AST structure
    #   assert {:raw_section, _, _} = result
    # end
  end

  describe "sequence literals with simplified tokenizer" do
    # The simplified tokenizer produces raw_token for identifiers, keywords, and operators
    # while preserving raw_number, raw_string, and raw_atom.
    # The parser now works correctly with these simplified tokens.

    test "empty sequence works" do
      assert parse!("~~()") == {:raw_section, [line: 1], []}
    end

    test "basic identifiers work with simplified tokenizer" do
      # The tokenizer produces raw_token which the parser now handles correctly
      assert parse!("~~(hello)") == {:raw_section, [line: 1], [{:hello, [line: 1], nil}]}

      assert parse!("~~(a b)") ==
               {:raw_section, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}

      assert parse!("~~(hello_world foo_bar)") ==
               {:raw_section, [line: 1],
                [{:hello_world, [line: 1], nil}, {:foo_bar, [line: 1], nil}]}
    end

    test "mixed case identifiers work with simplified tokenizer" do
      # Both produce raw_token tokens that parser now handles correctly
      assert parse!("~~(CamelCase snake_case)") ==
               {:raw_section, [line: 1],
                [{:CamelCase, [line: 1], nil}, {:snake_case, [line: 1], nil}]}
    end

    test "dot notation identifiers work as single tokens" do
      # These now produce raw_token tokens with dotted names like 'IO.puts'
      # and the parser handles them correctly as single atoms
      assert parse!("~~(IO.puts)") ==
               {:raw_section, [line: 1], [{:"IO.puts", [line: 1], nil}]}

      assert parse!("~~(String.upcase)") ==
               {:raw_section, [line: 1], [{:"String.upcase", [line: 1], nil}]}

      assert parse!("~~(GenServer.start_link)") ==
               {:raw_section, [line: 1], [{:"GenServer.start_link", [line: 1], nil}]}

      assert parse!("~~(io.puts data)") ==
               {:raw_section, [line: 1], [{:"io.puts", [line: 1], nil}, {:data, [line: 1], nil}]}
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
               {:raw_section, [line: 1],
                [{true, [line: 1], nil}, {false, [line: 1], nil}, {nil, [line: 1], nil}]}

      # When if appears first, it's now just a simple list
      assert parse!("~~(if do end)") ==
               {:raw_section, [line: 1],
                [{:if, [line: 1], nil}, {:do, [line: 1], nil}, {:end, [line: 1], nil}]}
    end

    test "operators work as simple tokens" do
      # When operators appear, they're now just simple tokens in the list
      assert parse!("~~(+ - * /)") ==
               {:raw_section, [line: 1],
                [
                  {:+, [line: 1], nil},
                  {:-, [line: 1], nil},
                  {:*, [line: 1], nil},
                  {:/, [line: 1], nil}
                ]}

      # Even when operators are not first, they're still simple tokens
      assert parse!("~~(a +)") ==
               {:raw_section, [line: 1], [{:a, [line: 1], nil}, {:+, [line: 1], nil}]}
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
                  {:raw_section, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
                ]}

      # Note: The tokenizer processes sequences independently, so complex expressions may not work as expected
      # This is because ~~(a b) is processed as one complete unit
      assert parse!("~~(a b)") ==
               {:raw_section, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
    end
  end

  describe "sequence literals with structural elements" do
    test "brackets work correctly" do
      assert parse!("~~([a b c])") ==
               {:raw_section, [line: 1],
                [
                  {:raw_block, [line: 1], :"[]",
                   [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
                ]}

      # Empty brackets work
      assert parse!("~~([])") ==
               {:raw_section, [line: 1], [{:raw_block, [line: 1], :"[]", []}]}
    end

    test "braces work correctly" do
      assert parse!("~~({x y z})") ==
               {:raw_section, [line: 1],
                [
                  {:raw_block, [line: 1], :{},
                   [{:x, [line: 1], nil}, {:y, [line: 1], nil}, {:z, [line: 1], nil}]}
                ]}

      # Empty braces work
      assert parse!("~~({})") ==
               {:raw_section, [line: 1], [{:raw_block, [line: 1], :{}, []}]}
    end

    test "operators work correctly as simple tokens" do
      # When an operator appears, it's now just a simple token in the list
      assert parse!("~~(+ a b)") ==
               {:raw_section, [line: 1],
                [{:+, [line: 1], nil}, {:a, [line: 1], nil}, {:b, [line: 1], nil}]}

      assert parse!("~~(+ 1 2)") ==
               {:raw_section, [line: 1], [{:+, [line: 1], nil}, 1, 2]}

      assert parse!("~~(- x y)") ==
               {:raw_section, [line: 1],
                [{:-, [line: 1], nil}, {:x, [line: 1], nil}, {:y, [line: 1], nil}]}

      assert parse!("~~(* foo bar)") ==
               {:raw_section, [line: 1],
                [{:*, [line: 1], nil}, {:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
    end

    test "mixed structural elements work" do
      # Mix brackets, braces, and regular tokens
      assert parse!("~~([a b] {c d})") ==
               {:raw_section, [line: 1],
                [
                  {:raw_block, [line: 1], :"[]", [{:a, [line: 1], nil}, {:b, [line: 1], nil}]},
                  {:raw_block, [line: 1], :{}, [{:c, [line: 1], nil}, {:d, [line: 1], nil}]}
                ]}
    end
  end

  describe "edge cases and special scenarios" do
    test "nested parentheses work correctly" do
      assert parse!("~~((a b))") ==
               {:raw_section, [line: 1],
                [{:raw_block, [line: 1], :"()", [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}]}

      # Single element in nested parentheses
      assert parse!("~~((a))") ==
               {:raw_section, [line: 1], [{:raw_block, [line: 1], :"()", [{:a, [line: 1], nil}]}]}

      assert parse!("~~(())") ==
               {:raw_section, [line: 1], [{:raw_block, [line: 1], :"()", []}]}

      # Nested sequence literals still don't work due to ~~ being treated as token
      assert_syntax_error(["syntax error before:"], "~~(a ~~(b) c)")
    end

    test "whitespace handling" do
      # Multiple spaces should be handled correctly
      assert parse!("~~(a    b     c)") ==
               {:raw_section, [line: 1],
                [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
    end

    test "multiline" do
      # Multiline sequence literals should work correctly
      assert parse!("~~(\na\n)") ==
               {:raw_section, [line: 1], [{:a, [line: 2], nil}]}
    end

    test "long sequences work" do
      # Test longer sequences
      assert parse!("~~(a b c d e f g h i j k l m n o p)") ==
               {:raw_section, [line: 1],
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

  describe "sequence literals with comments" do
    # Test that comments are properly handled during parsing and completely ignored
    # These tests verify that comments don't affect the resulting AST structure

    test "basic comment handling - inline comments are ignored" do
      # Comments should be completely ignored during parsing
      result_with_comment = parse!("~~(a # this is a comment\nb)")
      result_without_comment = parse!("~~(a\nb)")

      assert result_with_comment == result_without_comment

      assert result_with_comment ==
               {:raw_section, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 2], nil}]}
    end

    test "end-of-line comments are ignored" do
      result_with_comment = parse!("~~(hello # comment\n)")
      result_without_comment = parse!("~~(hello)")

      assert result_with_comment == result_without_comment
      assert result_with_comment == {:raw_section, [line: 1], [{:hello, [line: 1], nil}]}
    end

    test "full line comments within sequences" do
      # Full line comments should be completely ignored
      result_with_comment = parse!("~~(\n# full line comment\nworld)")
      result_without_comment = parse!("~~(\nworld)")

      # Line numbers will be different because the comment line affects counting
      # Just check that both are valid sequence literals with world token
      assert {:raw_section, [line: 1], [{:world, [line: _], nil}]} = result_with_comment
      assert {:raw_section, [line: 1], [{:world, [line: _], nil}]} = result_without_comment
    end

    test "multiple comments are ignored" do
      # Multiple comments should all be ignored
      result_with_comments = parse!("~~(a # comment1\nb # comment2\nc)")
      result_without_comments = parse!("~~(a\nb\nc)")

      assert result_with_comments == result_without_comments

      assert result_with_comments ==
               {:raw_section, [line: 1],
                [{:a, [line: 1], nil}, {:b, [line: 2], nil}, {:c, [line: 3], nil}]}
    end

    test "comments with different token types" do
      # Comments should work with numbers and atoms
      result_with_comment = parse!("~~(42 # comment\n:atom)")
      result_without_comment = parse!("~~(42\n:atom)")

      assert result_with_comment == result_without_comment

      expected =
        {:raw_section, [line: 1],
         [
           42,
           {:atom, [line: 2], nil}
         ]}

      assert result_with_comment == expected
    end

    test "comments within structural elements" do
      # Comments should work inside brackets
      result_with_comment = parse!("~~([a # comment\nb])")
      result_without_comment = parse!("~~([a\nb])")

      # Just verify both parse successfully and have similar structure
      assert {:raw_section, [line: 1], [bracket]} = result_with_comment
      assert {:raw_block, [line: 1], :"[]", _} = bracket

      assert {:raw_section, [line: 1], [^bracket]} = result_without_comment
    end

    test "comments in complex expressions with sequence literals" do
      # Comments should work in sequence literals within larger expressions
      result_with_comment = parse!("x = ~~(foo # comment\nbar)")
      result_without_comment = parse!("x = ~~(foo\nbar)")

      assert result_with_comment == result_without_comment

      expected =
        {:=, [line: 1],
         [
           {:x, [line: 1], nil},
           {:raw_section, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 2], nil}]}
         ]}

      assert result_with_comment == expected
    end

    test "comments with operators" do
      # Comments should work with operators
      result_with_comment = parse!("~~(+ a # comment\nb)")
      result_without_comment = parse!("~~(+ a\nb)")

      assert result_with_comment == result_without_comment

      # Verify the expected structure
      assert result_with_comment ==
               {:raw_section, [line: 1],
                [{:+, [line: 1], nil}, {:a, [line: 1], nil}, {:b, [line: 2], nil}]}
    end

    test "comments with parentheses in multiline sequence literals" do
      assert {:raw_section, _, _} =
               parse!("~~(\n# (def test (x) x)\n(valid code)\n)")
    end

    test "comments with various brackets - some work, some dont" do
      # Test various bracket types - only parentheses cause issues

      # These work fine (square brackets and braces in comments)
      result1 = parse!("~~(\n# [array syntax]\n42\n)")
      assert {:raw_section, [line: 1], [42]} = result1

      result2 = parse!("~~(\n# {map syntax}\n:atom\n)")
      assert {:raw_section, [line: 1], [{:atom, [line: 3], nil}]} = result2

      # But parentheses in comments cause parsing errors:
      assert_raise SyntaxError, fn ->
        parse!("~~(\n# ((nested parentheses))\n\"string\"\n)")
      end
    end

    test "comments at different positions in multiline sequence literals" do
      # Test comments at various positions to isolate the issue

      result1 = parse!("# comment before\n~~(valid code)")
      assert {:raw_section, [line: 2], _} = result1

      result2 = parse!("~~(valid code)\n# comment after")
      assert {:raw_section, [line: 1], _} = result2
    end
  end

  describe "sequence blocks in quote expressions" do
    test "raw_block nodes with tuple destructuring patterns compile correctly" do
      # This test reproduces the issue where raw_block nodes with tuple patterns
      # inside quoted expressions fail to compile

      # The issue occurs when we have a nested structure like:
      # {:raw_block, meta, :"()", [{:raw_block, meta2, :{}, [elements]}]}
      # This represents something like ({a b}) in the sequence literal

      ast =
        {:raw_section, [line: 1],
         [
           {:raw_paren, [line: 1],
            [
              {:def, [line: 1], nil},
              {:match_tuple, [line: 1], nil},
              {:raw_block, [line: 1, column: 24], :"()",
               [
                 {:raw_block, [line: 1, column: 25], :{},
                  [
                    {:a, [line: 1, column: 26], nil},
                    {:b, [line: 1, column: 28], nil}
                  ]}
               ]},
              {:a, [line: 1], nil}
            ]}
         ]}

      # This should not raise an error when used in a quote block
      result =
        quote do
          unquote(ast)
        end

      # Verify the AST is preserved correctly
      assert {:raw_section, _, _} = result
    end

    test "raw_block with parentheses type compiles in quote" do
      # Test that raw_block with :"()" type works in quote expressions
      ast = {:raw_block, [line: 1], :"()", [{:a, [line: 1], nil}]}

      # Should not raise an error
      result =
        quote do
          unquote(ast)
        end

      assert {:raw_block, _, :"()", _} = result
    end

    test "raw_block with braces type compiles in quote" do
      # Test that raw_block with :{} type works in quote expressions
      ast = {:raw_block, [line: 1], :{}, [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}

      # Should not raise an error
      result =
        quote do
          unquote(ast)
        end

      assert {:raw_block, _, :{}, _} = result
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
      # Test that identifiers inside sequences become raw_token
      tokens = tokenize("~~(hello)")

      assert Enum.any?(tokens, fn
               {:raw_token, {_, _, _}, :hello} -> true
               _ -> false
             end)
    end

    test "sequence strings produce raw_string tokens" do
      tokens = tokenize("~~(\"hello\")")

      assert Enum.any?(tokens, fn
               {:raw_string, {_, _, _}, ~c"hello"} -> true
               _ -> false
             end)
    end

    test "sequence numbers produce raw_number tokens" do
      tokens = tokenize("~~(42)")

      assert Enum.any?(tokens, fn
               {:raw_number, {_, _, _}, 42} -> true
               _ -> false
             end)
    end

    test "sequence atoms produce raw_atom tokens" do
      tokens = tokenize("~~(:foo)")

      assert Enum.any?(tokens, fn
               {:raw_atom, {_, _, _}, :foo} -> true
               _ -> false
             end)
    end

    test "operators and keywords produce raw_token tokens" do
      tokens = tokenize("~~(+ true)")

      # Both operator and keyword should be raw_token
      plus_token =
        Enum.any?(tokens, fn
          {:raw_token, {_, _, _}, :+} -> true
          _ -> false
        end)

      true_token =
        Enum.any?(tokens, fn
          {:raw_token, {_, _, _}, true} -> true
          _ -> false
        end)

      assert plus_token
      assert true_token
    end

    test "dotted identifiers become single raw_token tokens" do
      tokens = tokenize("~~(IO.puts)")

      assert Enum.any?(tokens, fn
               {:raw_token, {_, _, _}, :"IO.puts"} -> true
               _ -> false
             end)
    end

    test "sequence literals produce raw_begin and raw_end tokens" do
      tokens = tokenize("~~(hello)")

      # Should have raw_begin token
      has_begin =
        Enum.any?(tokens, fn
          {:raw_begin, {_, _, _}, :"~~("} -> true
          _ -> false
        end)

      has_end =
        Enum.any?(tokens, fn
          {:raw_end, {_, _, _}, :")"} -> true
          _ -> false
        end)

      assert has_begin, "Expected raw_begin token"
      assert has_end, "Expected raw_end token"
    end

    test "bracket pairs produce raw_block tokens" do
      # Test parentheses
      paren_tokens = tokenize("~~((a))")

      has_paren_block =
        Enum.any?(paren_tokens, fn
          {:raw_block, {_, _, _}, :"()", _} -> true
          _ -> false
        end)

      assert has_paren_block, "Expected raw_block token for parentheses"

      # Test square brackets
      bracket_tokens = tokenize("~~([a])")

      has_bracket_block =
        Enum.any?(bracket_tokens, fn
          {:raw_block, {_, _, _}, :"[]", _} -> true
          _ -> false
        end)

      assert has_bracket_block, "Expected raw_block token for brackets"

      # Test curly braces
      brace_tokens = tokenize("~~({a})")

      has_brace_block =
        Enum.any?(brace_tokens, fn
          {:raw_block, {_, _, _}, :{}, _} -> true
          _ -> false
        end)

      assert has_brace_block, "Expected raw_block token for braces"
    end

    test "tokenizer isolation - only raw_* tokens inside sequences" do
      tokens = tokenize("~~(hello 123 :atom \"string\" + true)")

      # Filter out structural tokens (raw_begin, raw_end, etc.)
      content_tokens =
        Enum.filter(tokens, fn
          {:raw_begin, {_, _, _}, _} -> false
          {:raw_end, {_, _, _}, _} -> false
          {:raw_block, {_, _, _}, _, _} -> false
          _ -> true
        end)

      # All content tokens should be raw_* types
      all_raw_types =
        Enum.all?(content_tokens, fn
          {:raw_token, {_, _, _}, _} -> true
          {:raw_number, {_, _, _}, _} -> true
          {:raw_atom, {_, _, _}, _} -> true
          {:raw_string, {_, _, _}, _} -> true
          _ -> false
        end)

      assert all_raw_types,
             "Found non-sequence tokens inside sequence literal: #{inspect(content_tokens)}"
    end
  end
end
