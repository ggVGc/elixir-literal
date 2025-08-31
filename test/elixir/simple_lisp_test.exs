# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 The Elixir Team

Code.require_file("test_helper.exs", __DIR__)

defmodule SimpleLispTest do
  use ExUnit.Case, async: true
  
  describe "tokenizer" do
    test "tokenizes simple expressions" do
      tokens = SimpleLisp.tokenize_lisp("(+ 1 2)")
      assert tokens == [:open_paren, {:symbol, "+"}, {:number, 1}, {:number, 2}, :close_paren]
    end
    
    test "tokenizes nested expressions" do
      tokens = SimpleLisp.tokenize_lisp("(* 2 (+ 3 4))")
      assert tokens == [
        :open_paren, 
        {:symbol, "*"}, 
        {:number, 2}, 
        :open_paren, 
        {:symbol, "+"}, 
        {:number, 3}, 
        {:number, 4}, 
        :close_paren, 
        :close_paren
      ]
    end
    
    test "tokenizes strings and symbols" do
      tokens = SimpleLisp.tokenize_lisp("(hello \"world\" 42)")
      assert tokens == [
        :open_paren,
        {:symbol, "hello"},
        {:string, "world"},
        {:number, 42},
        :close_paren
      ]
    end
    
    test "tokenizes arrays" do
      tokens = SimpleLisp.tokenize_lisp("[1 2 3]")
      assert tokens == [
        :open_bracket,
        {:number, 1},
        {:number, 2},
        {:number, 3},
        :close_bracket
      ]
    end
  end
  
  describe "parser" do
    test "parses simple expression" do
      ast = SimpleLisp.parse_tokens([:open_paren, {:symbol, "+"}, {:number, 1}, {:number, 2}, :close_paren])
      assert ast == {:list, [{:symbol, "+"}, {:number, 1}, {:number, 2}]}
    end
    
    test "parses nested expression" do
      tokens = SimpleLisp.tokenize_lisp("(* 2 (+ 3 4))")
      ast = SimpleLisp.parse_tokens(tokens)
      assert ast == {:list, [
        {:symbol, "*"},
        {:number, 2},
        {:list, [{:symbol, "+"}, {:number, 3}, {:number, 4}]}
      ]}
    end
    
    test "parses array literals" do
      tokens = SimpleLisp.tokenize_lisp("[1 2 3]")
      ast = SimpleLisp.parse_tokens(tokens)
      assert ast == {:array, [{:number, 1}, {:number, 2}, {:number, 3}]}
    end
  end
  
  describe "transformer" do
    test "transforms addition" do
      ast = {:list, [{:symbol, "+"}, {:number, 1}, {:number, 2}, {:number, 3}]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "Enum.sum([1, 2, 3])"
    end
    
    test "transforms subtraction" do
      ast = {:list, [{:symbol, "-"}, {:number, 5}, {:number, 3}]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "5 - 3"
    end
    
    test "transforms multiplication" do
      ast = {:list, [{:symbol, "*"}, {:number, 2}, {:number, 3}, {:number, 4}]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "Enum.reduce([2, 3, 4], &*/2)"
    end
    
    test "transforms comparison" do
      ast = {:list, [{:symbol, ">"}, {:symbol, "x"}, {:number, 5}]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "x > 5"
    end
    
    test "transforms if expression" do
      ast = {:list, [
        {:symbol, "if"},
        {:list, [{:symbol, ">"}, {:symbol, "x"}, {:number, 0}]},
        {:string, "positive"},
        {:string, "negative"}
      ]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "if x > 0, do: \"positive\", else: \"negative\""
    end
    
    test "transforms list creation" do
      ast = {:list, [{:symbol, "list"}, {:number, 1}, {:number, 2}, {:number, 3}]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "[1, 2, 3]"
    end
    
    test "transforms cons" do
      ast = {:list, [
        {:symbol, "cons"},
        {:number, 1},
        {:list, [{:symbol, "list"}, {:number, 2}, {:number, 3}]}
      ]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "[1 | [2, 3]]"
    end
    
    test "transforms anonymous function" do
      ast = {:list, [
        {:symbol, "fn"},
        {:array, [{:symbol, "x"}]},
        {:list, [{:symbol, "*"}, {:symbol, "x"}, {:number, 2}]}
      ]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "fn x -> Enum.reduce([x, 2], &*/2) end"
    end
    
    test "transforms map" do
      ast = {:list, [
        {:symbol, "map"},
        {:list, [
          {:symbol, "fn"},
          {:array, [{:symbol, "x"}]},
          {:list, [{:symbol, "*"}, {:symbol, "x"}, {:number, 2}]}
        ]},
        {:array, [{:number, 1}, {:number, 2}, {:number, 3}]}
      ]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "Enum.map([1, 2, 3], fn x -> Enum.reduce([x, 2], &*/2) end)"
    end
    
    test "transforms function definition" do
      ast = {:list, [
        {:symbol, "defn"},
        {:symbol, "square"},
        {:array, [{:symbol, "x"}]},
        {:list, [{:symbol, "*"}, {:symbol, "x"}, {:symbol, "x"}]}
      ]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "def square(x), do: Enum.reduce([x, x], &*/2)"
    end
    
    test "transforms let binding" do
      ast = {:list, [
        {:symbol, "let"},
        {:array, [{:symbol, "x"}, {:number, 10}, {:symbol, "y"}, {:number, 20}]},
        {:list, [{:symbol, "+"}, {:symbol, "x"}, {:symbol, "y"}]}
      ]}
      code = SimpleLisp.transform_to_elixir(ast)
      assert code == "with x = 10, y = 20, do: Enum.sum([x, y])"
    end
  end
  
  describe "extract_balanced_expr" do
    test "extracts simple expression" do
      result = SimpleLisp.extract_balanced_expr("+ 1 2)")
      assert result == "+ 1 2"
    end
    
    test "extracts nested expression" do
      result = SimpleLisp.extract_balanced_expr("* 2 (+ 3 4))")
      assert result == "* 2 (+ 3 4)"
    end
    
    test "handles multiple levels of nesting" do
      result = SimpleLisp.extract_balanced_expr("+ 1 (* 2 (- 3 1)))")
      assert result == "+ 1 (* 2 (- 3 1))"
    end
  end
  
  describe "end-to-end transformation" do
    test "simple arithmetic" do
      code = SimpleLisp.parse_lisp_to_elixir("(+ 1 2 3)")
      assert code == "Enum.sum([1, 2, 3])"
    end
    
    test "nested arithmetic" do
      code = SimpleLisp.parse_lisp_to_elixir("(* 2 (+ 3 4))")
      assert code == "Enum.reduce([2, Enum.sum([3, 4])], &*/2)"
    end
    
    test "conditional expression" do
      code = SimpleLisp.parse_lisp_to_elixir("(if (> x 5) \"big\" \"small\")")
      assert code == "if x > 5, do: \"big\", else: \"small\""
    end
    
    test "list operations" do
      code = SimpleLisp.parse_lisp_to_elixir("(cons 1 (list 2 3 4))")
      assert code == "[1 | [2, 3, 4]]"
    end
    
    test "map with lambda" do
      code = SimpleLisp.parse_lisp_to_elixir("(map (fn [x] (* x 2)) [1 2 3])")
      assert code == "Enum.map([1, 2, 3], fn x -> Enum.reduce([x, 2], &*/2) end)"
    end
  end
end