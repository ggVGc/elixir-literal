defmodule ReaderMacroDemo do
  def test do
    IO.puts("=== Reader Macro System Demo ===")
    
    # Test 1: Direct tokenizer call with reader macro
    IO.puts("\\nTest 1: Direct tokenizer call")
    {:ok, _line, _col, _warnings, tokens, _terminators} = 
      :elixir_tokenizer.tokenize_with_reader_macros("lisp!(+ 1 2 3)", 1, 1, [], %{})
    IO.puts("Tokens: #{inspect(tokens)}")
    
    # Test 2: Show what the tokenizer produces
    case tokens do
      [{:int, _, value}] ->
        IO.puts("Successfully converted lisp!(+ 1 2 3) to integer: #{value}")
      _ ->
        IO.puts("Unexpected token format: #{inspect(tokens)}")
    end
    
    # Test 3: Show that regular expressions work
    IO.puts("\\nTest 3: Regular Elixir expression")
    {:ok, _line, _col, _warnings, normal_tokens, _terminators} = 
      :elixir_tokenizer.tokenize_with_reader_macros("42", 1, 1, [], %{})
    IO.puts("Normal tokens: #{inspect(normal_tokens)}")
    
    IO.puts("\\n=== Demo Complete ===")
  end
end

ReaderMacroDemo.test()