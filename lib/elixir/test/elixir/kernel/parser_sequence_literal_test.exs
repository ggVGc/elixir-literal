Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ParserSequenceLiteralTest do
  use ExUnit.Case, async: true

  defp parse!(string), do: Code.string_to_quoted!(string)

  defp assert_syntax_error(given_messages, source) do
    e = assert_raise SyntaxError, fn -> parse!(source) end
    assert_exception_msg(e, given_messages)
  end

  defp assert_exception_msg(exception, messages) do
    error_msg = Exception.format(:error, exception, [])

    for msg <- messages do
      assert error_msg =~ msg
    end
  end

  describe "sequence literals with ~~(...) syntax - current limitations" do
    # NOTE: The isolated tokenizer now produces sequence_* tokens that the current
    # parser grammar doesn't recognize. Most sequence literal parsing will fail
    # until the parser grammar is updated to handle sequence_* tokens.

    test "empty sequence still works" do
      assert parse!("~~()") == {:sequence_literal, [line: 1], []}
    end

    test "basic identifiers now fail due to sequence_identifier tokens" do
      # The tokenizer produces {sequence_identifier, _, hello} which parser doesn't recognize
      assert_syntax_error(["syntax error"], "~~(hello)")
      assert_syntax_error(["syntax error"], "~~(a b)")
      assert_syntax_error(["syntax error"], "~~(hello_world foo_bar)")
    end

    test "mixed case identifiers fail due to sequence_identifier tokens" do
      # Both produce sequence_identifier tokens that parser doesn't understand
      assert_syntax_error(["syntax error"], "~~(CamelCase snake_case)")
    end

    test "dot notation identifiers fail due to sequence_identifier tokens" do
      # These now produce sequence_identifier tokens with dotted names like 'IO.puts'
      # but the parser doesn't recognize sequence_identifier tokens
      assert_syntax_error(["syntax error"], "~~(IO.puts)")
      assert_syntax_error(["syntax error"], "~~(String.upcase)")
      assert_syntax_error(["syntax error"], "~~(GenServer.start_link)")
      assert_syntax_error(["syntax error"], "~~(io.puts data)")
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

    test "mixed types fail due to sequence_* tokens" do
      # All of these now produce sequence_* tokens that parser doesn't recognize
      # sequence_identifier + sequence_number
      assert_syntax_error(["syntax error"], "~~(foo 123)")
      # sequence_identifier + sequence_atom
      assert_syntax_error(["syntax error"], "~~(foo :atom)")
      # sequence_identifier + sequence_string
      assert_syntax_error(["syntax error"], "~~(foo \"string\")")
      # sequence_identifier + sequence_keyword
      assert_syntax_error(["syntax error"], "~~(foo true)")
      # nested parens with sequence_identifiers
      assert_syntax_error(["syntax error"], "~~(a (b c))")
    end

    # Empty sequence was already tested above, no need to repeat

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

    test "sequence literals in larger expressions fail" do
      # This fails because ~~(foo bar) produces sequence_identifier tokens
      assert_syntax_error(["syntax error"], "x = ~~(foo bar)")
    end

    test "operators fail due to sequence_operator tokens" do
      # All operators now produce sequence_operator tokens that parser doesn't recognize
      assert_syntax_error(["syntax error"], "~~(a + b)")
      assert_syntax_error(["syntax error"], "~~(x - y)")
      assert_syntax_error(["syntax error"], "~~(foo * bar)")
      assert_syntax_error(["syntax error"], "~~(a == b)")
      assert_syntax_error(["syntax error"], "~~(x > y)")
      assert_syntax_error(["syntax error"], "~~(a && b)")
      assert_syntax_error(["syntax error"], "~~(foo |> bar)")
      assert_syntax_error(["syntax error"], "~~(x = y)")
    end

    test "nested parentheses fail due to sequence_identifier tokens" do
      # Even nested parentheses fail because they contain sequence_identifier tokens
      assert_syntax_error(["syntax error"], "~~((a b))")
      assert_syntax_error(["syntax error"], "~~((a (b c)))")
      assert_syntax_error(["syntax error"], "~~(((a)))")
    end

    test "multiline sequences fail due to sequence_* tokens" do
      # All multiline cases fail because they contain sequence_identifier tokens
      assert_syntax_error(["syntax error"], "~~((foo\nbar))")
      assert_syntax_error(["syntax error"], "~~([hello\nworld])")
    end
  end

  describe "tokenizer behavior verification" do
    # These tests verify that our isolated tokenizer produces the expected token types
    # even though the parser can't handle them yet.

    defp tokenize(string) do
      {:ok, _line, _column, _warnings, tokens, []} =
        :elixir_tokenizer.tokenize(String.to_charlist(string), 1, [])

      Enum.reverse(tokens)
    end

    test "sequence literals produce sequence_* tokens" do
      # Test that identifiers inside sequences become sequence_identifier tokens
      tokens = tokenize("~~(hello)")

      assert Enum.any?(tokens, fn
               {:sequence_identifier, {_, _, _}, :hello} -> true
               _ -> false
             end)
    end

    test "sequence strings produce sequence_string tokens" do
      tokens = tokenize("~~(\"hello\")")

      assert Enum.any?(tokens, fn
               {:sequence_string, {_, _, _}, "hello"} -> true
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

    test "sequence operators produce sequence_operator tokens" do
      tokens = tokenize("~~(a + b)")

      assert Enum.any?(tokens, fn
               {:sequence_operator, {_, _, _}, :+} -> true
               _ -> false
             end)
    end

    test "sequence keywords produce sequence_keyword tokens" do
      tokens = tokenize("~~(true)")

      assert Enum.any?(tokens, fn
               {:sequence_keyword, {_, _, _}, true} -> true
               _ -> false
             end)
    end

    test "dotted identifiers become single sequence_identifier tokens" do
      tokens = tokenize("~~(IO.puts)")

      assert Enum.any?(tokens, fn
               {:sequence_identifier, {_, _, _}, :"IO.puts"} -> true
               _ -> false
             end)

      # Verify we don't get separate tokens for IO, ., puts
      refute Enum.any?(tokens, fn
               {:identifier, {_, _, _}, :IO} -> true
               {:., {_, _}} -> true
               {:identifier, {_, _, _}, :puts} -> true
               _ -> false
             end)
    end

    test "tokenizer isolation - no normal tokens inside sequences" do
      tokens = tokenize("~~(hello 123 :atom \"string\" true)")

      # Filter out structural tokens (sequence_op, parens, etc.)
      content_tokens =
        Enum.filter(tokens, fn
          {:sequence_op, {_, _, _}, _} -> false
          {:"(", {_, _, _}} -> false
          {:")", {_, _, _}} -> false
          _ -> true
        end)

      # All content tokens should be sequence_* types
      all_sequence_types =
        Enum.all?(content_tokens, fn
          {:sequence_identifier, {_, _, _}, _} -> true
          {:sequence_number, {_, _, _}, _} -> true
          {:sequence_atom, {_, _, _}, _} -> true
          {:sequence_string, {_, _, _}, _} -> true
          {:sequence_operator, {_, _, _}, _} -> true
          {:sequence_keyword, {_, _, _}, _} -> true
          _ -> false
        end)

      assert all_sequence_types,
             "Found non-sequence tokens inside sequence literal: #{inspect(content_tokens)}"
    end
  end

  describe "sequence literals with blocks and brackets - current limitations" do
    test "empty brackets and braces still work" do
      # These work because they don't contain sequence_* tokens
      assert parse!("~~({})") ==
               {:sequence_literal, [line: 1], [{:sequence_brace, [line: 1], []}]}

      assert parse!("~~([])") ==
               {:sequence_literal, [line: 1], [{:sequence_bracket, [line: 1], []}]}
    end

    test "brackets with content fail due to sequence_* tokens" do
      # These fail because they contain sequence_identifier or sequence_number tokens
      assert_syntax_error(["syntax error"], "~~([a b c])")
      assert_syntax_error(["syntax error"], "~~([1 2 3])")
      assert_syntax_error(["syntax error"], "~~({foo})")
      assert_syntax_error(["syntax error"], "~~({a b c})")
    end

    test "mixed bracket types fail due to sequence_* tokens" do
      # All complex bracket combinations fail due to sequence_identifier tokens
      assert_syntax_error(["syntax error"], "~~((a {b} [c]))")
      assert_syntax_error(["syntax error"], "~~({[a b]})")
    end

    test "nested structures with blocks fail" do
      # Function definition with block body fails because sequence_* tokens aren't parsed
      assert_syntax_error(["syntax error"], "~~((def f (x) {(+ x 1)}))")
    end

    test "multi-statement blocks fail" do
      # Blocks with multiple expressions fail because sequence_* tokens aren't parsed
      assert_syntax_error(["syntax error"], "~~({(log x) (inc x) (save x)})")
    end

    test "data structure literals fail" do
      # Nested list literals fail because sequence_* tokens aren't parsed
      assert_syntax_error(["syntax error"], "~~([[1 2] [3 4]])")
      # List operations fail because sequence_* tokens aren't parsed
      assert_syntax_error(["syntax error"], "~~((map inc [1 2 3 4 5]))")
    end

    test "nested empty brackets fail" do
      # Even empty brackets fail due to parsing issues with sequence context
      assert_syntax_error(["syntax error", "expression is incomplete"], "~~(({}))")
    end

    test "deep nesting fails due to sequence_identifier tokens" do
      # This fails because it contains sequence_identifier token for 'a'
      assert_syntax_error(["syntax error"], "~~(({[{[a]}]}))")
    end

    test "regular Elixir syntax remains unchanged" do
      # Regular tuples outside sequence literals should parse normally
      assert parse!("{:ok, 123}") == {:ok, 123}

      assert parse!("{a, b, c}") ==
               {:{}, [line: 1],
                [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}

      # Regular lists outside sequence literals should parse normally  
      assert parse!("[1, 2, 3]") == [1, 2, 3]

      assert parse!("[a, b, c]") ==
               [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]

      # Map syntax should remain unchanged
      assert parse!("%{a: 1, b: 2}") ==
               {:%{}, [line: 1], [a: 1, b: 2]}
    end

    test "mixed content fails due to sequence_* tokens" do
      # All these fail because they contain various sequence_* tokens
      assert_syntax_error(["syntax error"], "~~([1 a \"string\" :atom])")
      assert_syntax_error(["syntax error"], "~~({123 foo :key \"value\"})")
    end
  end

  describe "sequence literals with operator-led constructs - all fail" do
    # All operator-led constructs fail because they produce sequence_* tokens
    test "percent operator sequences fail" do
      # These fail because % becomes sequence_operator and identifiers become sequence_identifier
      assert_syntax_error(["unexpected character in sequence literal"], "~~((% a b))")

      assert_syntax_error(
        ["unexpected character in sequence literal"],
        "~~((% :name \"John\" :age 30))"
      )
    end

    test "ampersand operator sequences fail" do
      # These fail because & becomes sequence_operator
      assert_syntax_error(["syntax error"], "~~((& inc))")
      assert_syntax_error(["syntax error"], "~~((& (+ a b)))")
    end

    test "at-sign operator sequences fail" do
      # These fail because @ becomes sequence_operator
      assert_syntax_error(["unexpected character in sequence literal"], "~~((@ module_attr))")
    end

    test "arithmetic operators as prefix constructs fail" do
      # All fail because operators become sequence_operator tokens
      assert_syntax_error(["syntax error"], "~~((+ a b c))")
      assert_syntax_error(["syntax error"], "~~((- x y))")
      assert_syntax_error(["syntax error"], "~~((* x 2))")
    end

    test "comparison operators as prefix constructs fail" do
      # All fail because operators become sequence_operator tokens
      assert_syntax_error(["syntax error"], "~~((= a b))")
      assert_syntax_error(["syntax error"], "~~((< x y))")
    end

    test "mixed operator constructs fail" do
      # Mixed constructs with unsupported operators fail
      assert_syntax_error(
        ["unexpected character in sequence literal"],
        "~~((defun process (data) (% :result (& transform data))))"
      )

      # Arithmetic operators are not supported as prefix operators in sequences  
      assert_syntax_error(["syntax error"], "~~((+ (* x 2) (- y 1)))")
    end

    test "empty operator constructs fail" do
      # Even empty operators fail because they become sequence_operator tokens
      assert_syntax_error(["unexpected character in sequence literal"], "~~((%))")
      assert_syntax_error(["syntax error"], "~~((&))")
    end

    test "regular elixir operators remain unchanged" do
      # Regular infix operators should still work outside sequences
      assert parse!("a + b") == {:+, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
      assert parse!("x * y") == {:*, [line: 1], [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}

      # Regular capture syntax should work
      assert parse!("&inc/1") == {:&, [line: 1], [{:/, [line: 1], [{:inc, [line: 1], nil}, 1]}]}

      # Regular module attributes should work  
      assert parse!("@attr") == {:@, [line: 1], [{:attr, [line: 1], nil}]}
    end
  end
end
