# Debug tokenizer output for sequence expressions

defmodule TokenDebug do
  def test_tokens do
    IO.puts("Testing tokenizer output for '(foo 123)'...")
    
    case :elixir.string_to_tokens(~c"(foo 123)", 1, 1, "debug.exs", []) do
      {:ok, tokens} ->
        IO.puts("Tokens: #{inspect(tokens)}")
      {:error, reason} ->
        IO.puts("Tokenizer error: #{inspect(reason)}")
    end
    
    IO.puts("")
    IO.puts("Testing tokenizer output for '(a b)'...")
    case :elixir.string_to_tokens(~c"(a b)", 1, 1, "debug.exs", []) do
      {:ok, tokens} ->
        IO.puts("Tokens: #{inspect(tokens)}")
      {:error, reason} ->
        IO.puts("Tokenizer error: #{inspect(reason)}")
    end
  end
end

TokenDebug.test_tokens()