defmodule DebugLisp do
  defreadermacro lisp("lisp!(" <> rest) do
    s_expr = extract_balanced_expr(rest)
    IO.puts("DEBUG: Extracted S-expression: #{inspect(s_expr)}")
    elixir_code = parse_lisp_to_elixir(s_expr)
    IO.puts("DEBUG: Generated Elixir code: #{inspect(elixir_code)}")
    elixir_code
  end

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

  def parse_lisp_to_elixir(expr) do
    expr
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

  defp transform_operation("*", args) do
    case args do
      [] -> "1"
      [single] -> transform_to_elixir(single)
      _ -> 
        args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
        "Enum.reduce([#{args_str}], &*/2)"
    end
  end

  defp transform_operation("map", [func, list]) do
    "Enum.map(#{transform_to_elixir(list)}, #{transform_to_elixir(func)})"
  end

  defp transform_operation("fn", [{:array, params}, body]) do
    params_str = Enum.map_join(params, ", ", fn {:symbol, p} -> p end)
    "fn #{params_str} -> #{transform_to_elixir(body)} end"
  end

  defp transform_operation(func, args) do
    args_str = Enum.map_join(args, ", ", &transform_to_elixir/1)
    "#{func}(#{args_str})"
  end

  def test do
    result = lisp!(map (fn [x] (* x 2)) [1 2 3 4 5])
    IO.puts("Result: #{inspect(result)}")
  end
end

DebugLisp.test()