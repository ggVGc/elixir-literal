# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 The Elixir Team

defmodule SimpleLisp do
  @moduledoc """
  A simple Lisp-like language reader macro for Elixir.
  
  This module demonstrates how reader macros can be used to embed
  alternative syntax (S-expressions) directly in Elixir code.
  
  ## Examples
  
      import SimpleLisp
      
      # Arithmetic
      lisp!(+ 1 2 3)              # => 6
      lisp!(* 2 (+ 3 4))          # => 14
      
      # Lists
      lisp!(list 1 2 3)           # => [1, 2, 3]
      lisp!(cons 1 (list 2 3))    # => [1, 2, 3]
      
      # Conditionals
      lisp!(if (> x 5) "big" "small")
      
      # Functions
      lisp!(defn square [x] (* x x))
      lisp!(map (fn [x] (* x 2)) [1 2 3])
  """

  # Define the reader macro for lisp syntax
  defreadermacro lisp("lisp!(" <> rest) do
    # Extract the complete S-expression
    s_expr = extract_balanced_expr(rest)
    # Parse and transform to Elixir code
    elixir_code = parse_lisp_to_elixir(s_expr)
    elixir_code
  end

  @doc """
  Parse a Lisp S-expression and convert it to Elixir code.
  """
  def parse_lisp_to_elixir(expr) do
    expr
    |> String.trim()
    |> tokenize_lisp()
    |> parse_tokens()
    |> transform_to_elixir()
  end

  @doc """
  Extract a balanced S-expression (matching parentheses).
  """
  def extract_balanced_expr(str) do
    extract_balanced_expr(str, 1, [])
  end

  defp extract_balanced_expr(<<char, rest::binary>>, depth, acc) do
    case char do
      ?( ->
        extract_balanced_expr(rest, depth + 1, [char | acc])
      ?) ->
        if depth == 1 do
          # Found matching closing paren
          acc |> Enum.reverse() |> List.to_string()
        else
          extract_balanced_expr(rest, depth - 1, [char | acc])
        end
      _ ->
        extract_balanced_expr(rest, depth, [char | acc])
    end
  end

  defp extract_balanced_expr(<<>>, _depth, acc) do
    # Unmatched parentheses - return what we have
    acc |> Enum.reverse() |> List.to_string()
  end

  @doc """
  Tokenize a Lisp expression into a list of tokens.
  """
  def tokenize_lisp(str) do
    str
    |> String.replace("(", " ( ")
    |> String.replace(")", " ) ")
    |> String.replace("[", " [ ")
    |> String.replace("]", " ] ")
    |> String.split(~r/\s+/, trim: true)
    |> Enum.map(&parse_token/1)
  end

  defp parse_token("("), do: :open_paren
  defp parse_token(")"), do: :close_paren
  defp parse_token("["), do: :open_bracket
  defp parse_token("]"), do: :close_bracket
  defp parse_token("true"), do: {:bool, true}
  defp parse_token("false"), do: {:bool, false}
  defp parse_token("nil"), do: {:nil, nil}
  defp parse_token(<<"\"", _::binary>> = str) do
    # String literal
    {:string, String.trim(str, "\"")}
  end
  defp parse_token(str) do
    # Try to parse as number
    case Integer.parse(str) do
      {num, ""} -> {:number, num}
      _ ->
        case Float.parse(str) do
          {num, ""} -> {:number, num}
          _ -> {:symbol, str}
        end
    end
  end

  @doc """
  Parse tokens into an AST.
  """
  def parse_tokens(tokens) do
    {ast, _rest} = parse_expr(tokens)
    ast
  end

  defp parse_expr([:open_paren | rest]) do
    parse_list(rest, [])
  end
  defp parse_expr([:open_bracket | rest]) do
    {elements, rest} = parse_array(rest, [])
    {{:array, elements}, rest}
  end
  defp parse_expr([{:symbol, sym} | rest]) do
    {{:symbol, sym}, rest}
  end
  defp parse_expr([{:number, n} | rest]) do
    {{:number, n}, rest}
  end
  defp parse_expr([{:string, s} | rest]) do
    {{:string, s}, rest}
  end
  defp parse_expr([{:bool, b} | rest]) do
    {{:bool, b}, rest}
  end
  defp parse_expr([{:nil, _} | rest]) do
    {{:nil, nil}, rest}
  end

  defp parse_list([:close_paren | rest], acc) do
    {{:list, Enum.reverse(acc)}, rest}
  end
  defp parse_list(tokens, acc) do
    {expr, rest} = parse_expr(tokens)
    parse_list(rest, [expr | acc])
  end

  defp parse_array([:close_bracket | rest], acc) do
    {Enum.reverse(acc), rest}
  end
  defp parse_array(tokens, acc) do
    {expr, rest} = parse_expr(tokens)
    parse_array(rest, [expr | acc])
  end

  @doc """
  Transform Lisp AST to Elixir code string.
  """
  def transform_to_elixir({:list, [{:symbol, op} | args]}) do
    transform_operation(op, args)
  end
  def transform_to_elixir({:array, elements}) do
    # Array literal
    "[" <> Enum.map_join(elements, ", ", &transform_to_elixir/1) <> "]"
  end
  def transform_to_elixir({:symbol, sym}) do
    sym
  end
  def transform_to_elixir({:number, n}) do
    to_string(n)
  end
  def transform_to_elixir({:string, s}) do
    "\"#{s}\""
  end
  def transform_to_elixir({:bool, b}) do
    to_string(b)
  end
  def transform_to_elixir({:nil, _}) do
    "nil"
  end

  defp transform_operation("+", args) do
    case args do
      [] -> "0"
      [single] -> transform_to_elixir(single)
      _ -> 
        # For multiple args, use Enum.sum
        args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
        "Enum.sum([#{args_str}])"
    end
  end

  defp transform_operation("-", [a, b]) do
    "#{transform_to_elixir(a)} - #{transform_to_elixir(b)}"
  end

  defp transform_operation("*", args) do
    case args do
      [] -> "1"
      [single] -> transform_to_elixir(single)
      _ -> 
        # Multiple args - use Enum.reduce
        args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
        "Enum.reduce([#{args_str}], &*/2)"
    end
  end

  defp transform_operation("/", [a, b]) do
    "#{transform_to_elixir(a)} / #{transform_to_elixir(b)}"
  end

  # Comparison operators
  defp transform_operation(">", [a, b]) do
    "#{transform_to_elixir(a)} > #{transform_to_elixir(b)}"
  end

  defp transform_operation("<", [a, b]) do
    "#{transform_to_elixir(a)} < #{transform_to_elixir(b)}"
  end

  defp transform_operation(">=", [a, b]) do
    "#{transform_to_elixir(a)} >= #{transform_to_elixir(b)}"
  end

  defp transform_operation("<=", [a, b]) do
    "#{transform_to_elixir(a)} <= #{transform_to_elixir(b)}"
  end

  defp transform_operation("=", [a, b]) do
    "#{transform_to_elixir(a)} == #{transform_to_elixir(b)}"
  end

  # List operations
  defp transform_operation("list", args) do
    args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
    "[#{args_str}]"
  end

  defp transform_operation("cons", [head, tail]) do
    "[#{transform_to_elixir(head)} | #{transform_to_elixir(tail)}]"
  end

  defp transform_operation("car", [list]) do
    "hd(#{transform_to_elixir(list)})"
  end

  defp transform_operation("cdr", [list]) do
    "tl(#{transform_to_elixir(list)})"
  end

  # Control flow
  defp transform_operation("if", [test, then_branch, else_branch]) do
    "if #{transform_to_elixir(test)}, do: #{transform_to_elixir(then_branch)}, else: #{transform_to_elixir(else_branch)}"
  end

  # Higher-order functions
  defp transform_operation("map", [func, list]) do
    "Enum.map(#{transform_to_elixir(list)}, #{transform_to_elixir(func)})"
  end

  defp transform_operation("filter", [func, list]) do
    "Enum.filter(#{transform_to_elixir(list)}, #{transform_to_elixir(func)})"
  end

  defp transform_operation("reduce", [func, init, list]) do
    "Enum.reduce(#{transform_to_elixir(list)}, #{transform_to_elixir(init)}, #{transform_to_elixir(func)})"
  end

  # Anonymous functions
  defp transform_operation("fn", [{:array, params}, body]) do
    params_str = Enum.map_join(params, ", ", fn {:symbol, p} -> p end)
    "fn #{params_str} -> #{transform_to_elixir(body)} end"
  end

  defp transform_operation("lambda", [{:array, params}, body]) do
    # Alias for fn
    transform_operation("fn", [{:array, params}, body])
  end

  # Function definition
  defp transform_operation("defn", [{:symbol, name}, {:array, params}, body]) do
    params_str = Enum.map_join(params, ", ", fn {:symbol, p} -> p end)
    "def #{name}(#{params_str}), do: #{transform_to_elixir(body)}"
  end

  # Let bindings
  defp transform_operation("let", [{:array, bindings}, body]) do
    # Transform let into a with expression
    bindings_str = bindings
    |> Enum.chunk_every(2)
    |> Enum.map(fn [{:symbol, var}, val] ->
      "#{var} = #{transform_to_elixir(val)}"
    end)
    |> Enum.join(", ")
    
    "with #{bindings_str}, do: #{transform_to_elixir(body)}"
  end

  # Default: function call
  defp transform_operation(func, args) do
    args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
    "#{func}(#{args_str})"
  end
end