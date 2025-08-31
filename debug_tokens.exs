# Debug the actual token stream for (a b)

defmodule TokenDebugger do
  def debug_tokens do
    IO.puts("Debugging tokens for '(a b)'...")
    
    case :elixir.string_to_tokens(~c"(a b)", 1, 1, "debug.exs", []) do
      {:ok, tokens} ->
        IO.puts("Tokens:")
        Enum.each(tokens, fn token ->
          IO.puts("  #{inspect(token)}")
        end)
      {:error, reason} ->
        IO.puts("Tokenizer error: #{inspect(reason)}")
    end
    
    IO.puts("\nDebugging parsing...")
    case Code.string_to_quoted("(a b)") do
      {:ok, ast} ->
        IO.puts("Final AST: #{inspect(ast)}")
      {:error, reason} ->
        IO.puts("Parse error: #{inspect(reason)}")
    end
  end
end

TokenDebugger.debug_tokens()