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

  describe "sequence literals with ~~(...) syntax" do
    test "basic two-argument sequence" do
      assert parse!("~~(a b)") ==
               {:sequence_literal, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
    end

    test "longer sequences" do
      assert parse!("~~(a b c d e)") ==
               {:sequence_literal, [line: 1],
                [
                  {:a, [line: 1], nil},
                  {:b, [line: 1], nil},
                  {:c, [line: 1], nil},
                  {:d, [line: 1], nil},
                  {:e, [line: 1], nil}
                ]}
    end

    test "single argument sequence" do
      assert parse!("~~(foo)") == {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}]}
    end

    test "sequences with different identifier patterns" do
      assert parse!("~~(hello_world foo_bar)") ==
               {:sequence_literal, [line: 1],
                [{:hello_world, [line: 1], nil}, {:foo_bar, [line: 1], nil}]}

      # Both lowercase and uppercase identifiers are now supported
      assert parse!("~~(CamelCase snake_case)") ==
               {:sequence_literal, [line: 1],
                [{:CamelCase, [line: 1], nil}, {:snake_case, [line: 1], nil}]}
    end

    test "sequences with dot notation identifiers" do
      # Basic lowercase dot notation
      assert parse!("~~(io.puts)") ==
               {:sequence_literal, [line: 1], [{:"io.puts", [line: 1], nil}]}

      # Basic uppercase dot notation (modules)
      assert parse!("~~(IO.puts)") ==
               {:sequence_literal, [line: 1], [{:"IO.puts", [line: 1], nil}]}

      # Multiple dot-separated parts
      assert parse!("~~(String.upcase Enum.count)") ==
               {:sequence_literal, [line: 1],
                [{:"String.upcase", [line: 1], nil}, {:"Enum.count", [line: 1], nil}]}

      # Nested sequences with dot notation
      assert parse!("~~((IO.puts \"hello\"))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:"IO.puts", [line: 1], nil}, "hello"]}]}

      # Mixed dot notation with regular identifiers
      assert parse!("~~(process IO.puts data)") ==
               {:sequence_literal, [line: 1],
                [
                  {:process, [line: 1], nil},
                  {:"IO.puts", [line: 1], nil},
                  {:data, [line: 1], nil}
                ]}

      # Complex module paths
      assert parse!("~~(GenServer.start_link)") ==
               {:sequence_literal, [line: 1], [{:"GenServer.start_link", [line: 1], nil}]}

      # Function calls with dot notation
      assert parse!("~~((String.upcase \"test\"))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:"String.upcase", [line: 1], nil}, "test"]}]}

      # Multiple arguments with dot notation functions
      assert parse!("~~((Enum.map list func))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:"Enum.map", [line: 1], nil},
                     {:list, [line: 1], nil},
                     {:func, [line: 1], nil}
                   ]}
                ]}
    end

    test "dot notation only works inside sequence literals" do
      # Outside sequence literals, dots should still be parsed as separate tokens
      # This verifies we didn't break normal Elixir syntax
      assert parse!("IO.puts()") ==
               {{:., [line: 1], [{:__aliases__, [line: 1], [:IO]}, :puts]}, [line: 1], []}

      # Regular module calls should be unaffected
      assert parse!("String.upcase(\"test\")") ==
               {{:., [line: 1], [{:__aliases__, [line: 1], [:String]}, :upcase]}, [line: 1],
                ["test"]}
    end

    test "complex expressions and mixed types supported" do
      # Nested expressions are now supported
      assert parse!("~~(a (b c) d)") ==
               {:sequence_literal, [line: 1],
                [
                  {:a, [line: 1], nil},
                  {:sequence_paren, [line: 1], [{:b, [line: 1], nil}, {:c, [line: 1], nil}]},
                  {:d, [line: 1], nil}
                ]}

      # Mixed data types are supported
      assert parse!("~~(foo 123)") ==
               {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, 123]}

      assert parse!("~~(foo :atom)") ==
               {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, :atom]}

      assert parse!("~~(foo \"string\")") ==
               {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, "string"]}

      # Complex combinations
      assert parse!("~~(test 123 :atom \"string\" true false nil)") ==
               {:sequence_literal, [line: 1],
                [
                  {:test, [line: 1], nil},
                  123,
                  :atom,
                  "string",
                  {true, [line: 1], nil},
                  {false, [line: 1], nil},
                  {nil, [line: 1], nil}
                ]}

      # Nested mathematical expressions  
      assert parse!("~~(calc (1 + 2) result)") ==
               {:sequence_literal, [line: 1],
                [
                  {:calc, [line: 1], nil},
                  {:sequence_paren, [line: 1],
                   [1, {:sequence_prefix, {:+, [line: 1], nil}, [2]}]},
                  {:result, [line: 1], nil}
                ]}
    end

    test "empty sequence supported" do
      assert parse!("~~()") == {:sequence_literal, [line: 1], []}
    end

    test "sequence operator requires parentheses" do
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

    test "sequence literals in larger expressions" do
      # Sequence literals are now supported in assignment contexts
      assert parse!("x = ~~(foo bar)") ==
               {:=, [line: 1],
                [
                  {:x, [line: 1], nil},
                  {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
                ]}
    end

    test "operators in sequences - current limitation" do
      # These tests demonstrate that operators are not currently supported
      # in sequence literals. They should be parsed as individual tokens.

      # Arithmetic operators should be transformed to sequence_prefix when followed by arguments
      assert parse!("~~(a + b)") ==
               {:sequence_literal, [line: 1],
                [
                  {:a, [line: 1], nil},
                  {:sequence_prefix, {:+, [line: 1], nil}, [{:b, [line: 1], nil}]}
                ]}

      # assert_syntax_error(["syntax error before: ", ""], "~~(x - y)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(foo * bar)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(m / n)")

      # # Comparison operators should be preserved as atoms  
      # assert_syntax_error(["syntax error before: ", ""], "~~(a < b)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(x > y)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(foo == bar)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(a != b)")

      # # Logical operators should be preserved as atoms
      # assert_syntax_error(["syntax error before: ", ""], "~~(a && b)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(x || y)")

      # # Pipe operator should be preserved as atom
      # assert_syntax_error(["syntax error before: ", ""], "~~(foo |> bar)")

      # # Other operators should be preserved as atoms
      # assert_syntax_error(["syntax error before: ", ""], "~~(a <> b)")
      # # assert_syntax_error(["syntax error before: ", ""], "~~(start .. end)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(x = y)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(key => value)")

      # # Mixed operators and identifiers
      # assert_syntax_error(["syntax error before: ", ""], "~~(a + b * c)")
      # assert_syntax_error(["syntax error before: ", ""], "~~(foo |> bar |> baz)")
    end

    test "nested parentheses within sequences" do
      # Test that nested parentheses create sequence_paren nodes
      assert parse!("~~((a b))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}]}

      assert parse!("~~((a (b c)))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:a, [line: 1], nil},
                     {:sequence_paren, [line: 1], [{:b, [line: 1], nil}, {:c, [line: 1], nil}]}
                   ]}
                ]}

      assert parse!("~~(((a)))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [{:sequence_paren, [line: 1], [{:a, [line: 1], nil}]}]}
                ]}
    end

    test "multiline sequence expressions" do
      # Test basic multiline sequence with newlines
      assert parse!("~~((foo\nbar))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 2], nil}]}]}

      # Test multiline with nested expressions
      assert parse!("~~((def square (x)\n  (* x x)))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:def, [line: 1], nil},
                      [
                        {:square, [line: 1], nil},
                        {:sequence_paren, [line: 1], [{:x, [line: 1], nil}]},
                        {:sequence_paren, [line: 2],
                         [
                           {:sequence_prefix, {:*, [line: 2], nil},
                            [{:x, [line: 2], nil}, {:x, [line: 2], nil}]}
                         ]}
                      ]}
                   ]}
                ]}

      # Test multiline arithmetic with nested function calls
      assert parse!("~~((+\n  (* 2 3)\n  (* 4 5)))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:+, [line: 1], nil},
                      [
                        {:sequence_paren, [line: 2],
                         [{:sequence_prefix, {:*, [line: 2], nil}, [2, 3]}]},
                        {:sequence_paren, [line: 3],
                         [{:sequence_prefix, {:*, [line: 3], nil}, [4, 5]}]}
                      ]}
                   ]}
                ]}

      # Test multiline with mixed brackets
      assert parse!("~~([hello\nworld])") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_bracket, [line: 1],
                   [{:hello, [line: 1], nil}, {:world, [line: 2], nil}]}
                ]}

      # Test deeply nested multiline
      assert parse!("~~((outer\n  (inner\n    nested)))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:outer, [line: 1], nil},
                     {:sequence_paren, [line: 2],
                      [
                        {:inner, [line: 2], nil},
                        {:nested, [line: 3], nil}
                      ]}
                   ]}
                ]}
    end
  end

  describe "sequence literals with blocks and brackets" do
    test "basic brace sequences" do
      # Braces should create sequence_brace nodes
      assert parse!("~~({a b c})") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_brace, [line: 1],
                   [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
                ]}

      assert parse!("~~({foo})") ==
               {:sequence_literal, [line: 1],
                [{:sequence_brace, [line: 1], [{:foo, [line: 1], nil}]}]}
    end

    test "basic bracket sequences" do
      # Brackets should create sequence_bracket nodes
      assert parse!("~~([1 2 3])") ==
               {:sequence_literal, [line: 1], [{:sequence_bracket, [line: 1], [1, 2, 3]}]}

      assert parse!("~~([a b c])") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_bracket, [line: 1],
                   [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
                ]}
    end

    test "mixed bracket types in sequences" do
      # Test combinations of parentheses, braces, and brackets
      assert parse!("~~((a {b} [c]))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:a, [line: 1], nil},
                     {:sequence_brace, [line: 1], [{:b, [line: 1], nil}]},
                     {:sequence_bracket, [line: 1], [{:c, [line: 1], nil}]}
                   ]}
                ]}

      assert parse!("~~({[a b]})") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_brace, [line: 1],
                   [{:sequence_bracket, [line: 1], [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}]}
                ]}
    end

    test "nested structures with blocks" do
      # Function definition with block body
      assert parse!("~~((def f (x) {(+ x 1)}))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:def, [line: 1], nil},
                      [
                        {:f, [line: 1], nil},
                        {:sequence_paren, [line: 1], [{:x, [line: 1], nil}]},
                        {:sequence_brace, [line: 1],
                         [
                           {:sequence_paren, [line: 1],
                            [{:sequence_prefix, {:+, [line: 1], nil}, [{:x, [line: 1], nil}, 1]}]}
                         ]}
                      ]}
                   ]}
                ]}
    end

    test "multi-statement blocks" do
      # Blocks with multiple expressions
      assert parse!("~~({(log x) (inc x) (save x)})") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_brace, [line: 1],
                   [
                     {:sequence_paren, [line: 1], [{:log, [line: 1], nil}, {:x, [line: 1], nil}]},
                     {:sequence_paren, [line: 1], [{:inc, [line: 1], nil}, {:x, [line: 1], nil}]},
                     {:sequence_paren, [line: 1], [{:save, [line: 1], nil}, {:x, [line: 1], nil}]}
                   ]}
                ]}
    end

    test "data structure literals" do
      # Nested list literals
      assert parse!("~~([[1 2] [3 4]])") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_bracket, [line: 1],
                   [
                     {:sequence_bracket, [line: 1], [1, 2]},
                     {:sequence_bracket, [line: 1], [3, 4]}
                   ]}
                ]}

      # List operations
      assert parse!("~~((map inc [1 2 3 4 5]))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:map, [line: 1], nil},
                     {:inc, [line: 1], nil},
                     {:sequence_bracket, [line: 1], [1, 2, 3, 4, 5]}
                   ]}
                ]}
    end

    test "empty braces and brackets" do
      assert parse!("~~({})") ==
               {:sequence_literal, [line: 1], [{:sequence_brace, [line: 1], []}]}

      assert parse!("~~([])") ==
               {:sequence_literal, [line: 1], [{:sequence_bracket, [line: 1], []}]}

      assert parse!("~~(({}))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:sequence_brace, [line: 1], []}]}]}
    end

    test "deep nesting of different bracket types" do
      assert parse!("~~(({[{[a]}]}))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_brace, [line: 1],
                      [
                        {:sequence_bracket, [line: 1],
                         [
                           {:sequence_brace, [line: 1],
                            [{:sequence_bracket, [line: 1], [{:a, [line: 1], nil}]}]}
                         ]}
                      ]}
                   ]}
                ]}
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

    test "blocks and brackets with mixed content" do
      # Mix of literals and identifiers in brackets
      assert parse!("~~([1 a \"string\" :atom])") ==
               {:sequence_literal, [line: 1],
                [{:sequence_bracket, [line: 1], [1, {:a, [line: 1], nil}, "string", :atom]}]}

      # Complex block with various expression types
      assert parse!("~~({123 foo :key \"value\"})") ==
               {:sequence_literal, [line: 1],
                [{:sequence_brace, [line: 1], [123, {:foo, [line: 1], nil}, :key, "value"]}]}
    end
  end

  describe "sequence literals with operator-led constructs" do
    test "percent operator sequences" do
      # Percent should create sequence_prefix nodes
      assert parse!("~~((% a b))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:%, [line: 1], nil},
                      [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
                   ]}
                ]}

      assert parse!("~~((% :name \"John\" :age 30))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [{:sequence_prefix, {:%, [line: 1], nil}, [:name, "John", :age, 30]}]}
                ]}
    end

    test "ampersand operator sequences" do
      # Ampersand should create sequence_prefix nodes
      assert parse!("~~((& inc))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [{:sequence_prefix, {:&, [line: 1], nil}, [{:inc, [line: 1], nil}]}]}
                ]}

      assert parse!("~~((& (+ a b)))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:&, [line: 1], nil},
                      [
                        {:sequence_paren, [line: 1],
                         [
                           {:sequence_prefix, {:+, [line: 1], nil},
                            [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
                         ]}
                      ]}
                   ]}
                ]}
    end

    test "at-sign operator sequences" do
      # At-sign should create sequence_prefix nodes
      assert parse!("~~((@ module_attr))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [{:sequence_prefix, {:@, [line: 1], nil}, [{:module_attr, [line: 1], nil}]}]}
                ]}
    end

    test "arithmetic operators as prefix constructs" do
      # Plus operator
      assert parse!("~~((+ a b c))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:+, [line: 1], nil},
                      [{:a, [line: 1], nil}, {:b, [line: 1], nil}, {:c, [line: 1], nil}]}
                   ]}
                ]}

      # Minus operator  
      assert parse!("~~((- x y))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:-, [line: 1], nil},
                      [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}
                   ]}
                ]}

      # Multiplication operator
      assert parse!("~~((* x 2))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [{:sequence_prefix, {:*, [line: 1], nil}, [{:x, [line: 1], nil}, 2]}]}
                ]}
    end

    test "comparison operators as prefix constructs" do
      assert parse!("~~((= a b))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:=, [line: 1], nil},
                      [{:a, [line: 1], nil}, {:b, [line: 1], nil}]}
                   ]}
                ]}

      assert parse!("~~((< x y))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:<, [line: 1], nil},
                      [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}
                   ]}
                ]}
    end

    test "mixed operator constructs" do
      # Function with percent and ampersand (defun is not an operator, so remains as regular atom)
      assert parse!("~~((defun process (data) (% :result (& transform data))))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:defun, [line: 1], nil},
                     {:process, [line: 1], nil},
                     {:sequence_paren, [line: 1], [{:data, [line: 1], nil}]},
                     {:sequence_paren, [line: 1],
                      [
                        {:sequence_prefix, {:%, [line: 1], nil},
                         [
                           :result,
                           {:sequence_paren, [line: 1],
                            [
                              {:sequence_prefix, {:&, [line: 1], nil},
                               [{:transform, [line: 1], nil}, {:data, [line: 1], nil}]}
                            ]}
                         ]}
                      ]}
                   ]}
                ]}

      # Nested arithmetic with different operators
      assert parse!("~~((+ (* x 2) (- y 1)))") ==
               {:sequence_literal, [line: 1],
                [
                  {:sequence_paren, [line: 1],
                   [
                     {:sequence_prefix, {:+, [line: 1], nil},
                      [
                        {:sequence_paren, [line: 1],
                         [{:sequence_prefix, {:*, [line: 1], nil}, [{:x, [line: 1], nil}, 2]}]},
                        {:sequence_paren, [line: 1],
                         [{:sequence_prefix, {:-, [line: 1], nil}, [{:y, [line: 1], nil}, 1]}]}
                      ]}
                   ]}
                ]}
    end

    test "empty operator constructs" do
      # Empty percent
      assert parse!("~~((%))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:sequence_prefix, {:%, [line: 1], nil}, []}]}]}

      # Empty ampersand
      assert parse!("~~((&))") ==
               {:sequence_literal, [line: 1],
                [{:sequence_paren, [line: 1], [{:sequence_prefix, {:&, [line: 1], nil}, []}]}]}
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
