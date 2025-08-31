# Debug parser directly

defmodule ParserDebug do
  def test_parse do
    IO.puts("Testing parser output for '(a b)'...")
    
    tokens = [{:"(", {1, 1, nil}}, {:identifier, {1, 2, ~c"a"}, :a}, {:identifier, {1, 4, ~c"b"}, :b}, {:")", {1, 5, nil}}]
    
    case :elixir_parser.parse(tokens) do
      {:ok, ast} ->
        IO.puts("Parsed AST: #{inspect(ast)}")
      {:error, reason} ->
        IO.puts("Parser error: #{inspect(reason)}")
    end
    
    IO.puts("")
    IO.puts("Testing parser output for simple '(a)'...")
    tokens2 = [{:"(", {1, 1, nil}}, {:identifier, {1, 2, ~c"a"}, :a}, {:")", {1, 3, nil}}]
    
    case :elixir_parser.parse(tokens2) do
      {:ok, ast} ->
        IO.puts("Parsed AST: #{inspect(ast)}")
      {:error, reason} ->
        IO.puts("Parser error: #{inspect(reason)}")
    end
  end
end

ParserDebug.test_parse()