defmodule SimpleLispDemo do
  @moduledoc """
  A simplified demonstration of Lisp-style syntax in Elixir using regular macros.
  
  This shows how Lisp-style expressions can be embedded in Elixir code
  while avoiding the complexity of reader macros.
  """

  # Define a macro for Lisp-style syntax that takes string literals
  defmacro lisp(expr) when is_binary(expr) do
    elixir_code = parse_lisp_to_elixir(expr) 
    Code.string_to_quoted!(elixir_code)
  end

  def parse_lisp_to_elixir(expr) do
    # Add parentheses if not present to make it a valid S-expression
    s_expr = if String.starts_with?(expr, "(") do
      expr
    else
      "(#{expr})"
    end
    
    s_expr
    |> String.trim()
    |> tokenize_lisp()
    |> parse_tokens() 
    |> transform_to_elixir()
  end

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
    {:string, String.trim(str, "\"")}
  end
  defp parse_token(str) do
    case Integer.parse(str) do
      {num, ""} -> {:number, num}
      _ ->
        case Float.parse(str) do
          {num, ""} -> {:number, num}
          _ -> {:symbol, str}
        end
    end
  end

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

  def transform_to_elixir({:list, [{:symbol, op} | args]}) do
    transform_operation(op, args)
  end
  def transform_to_elixir({:array, elements}) do
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
        args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
        "Enum.sum([#{args_str}])"
    end
  end

  defp transform_operation("*", args) do
    case args do
      [] -> "1"
      [single] -> transform_to_elixir(single)
      _ -> 
        args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
        "Enum.reduce([#{args_str}], &*/2)"
    end
  end

  defp transform_operation("-", [a, b]) do
    "#{transform_to_elixir(a)} - #{transform_to_elixir(b)}"
  end

  defp transform_operation("/", [a, b]) do
    "#{transform_to_elixir(a)} / #{transform_to_elixir(b)}"
  end

  defp transform_operation("list", args) do
    args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
    "[#{args_str}]"
  end

  # Simple function test
  def test do
    IO.puts("=== Simple Lisp Demo ===")
    
    # Arithmetic
    result1 = lisp("+ 1 2 3 4 5")
    IO.puts("(+ 1 2 3 4 5) = #{result1}")
    
    # Nested arithmetic  
    result2 = lisp("* 2 (+ 3 4)")
    IO.puts("(* 2 (+ 3 4)) = #{result2}")
    
    # Lists
    list1 = lisp("list 1 2 3 4 5")
    IO.puts("(list 1 2 3 4 5) = #{inspect(list1)}")
    
    IO.puts("✨ Simple Lisp demo completed! ✨")
    {result1, result2, list1}
  end
end

SimpleLispDemo.test()