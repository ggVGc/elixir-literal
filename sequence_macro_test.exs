# Test implementing sequence syntax using macros

defmodule SequenceMacro do
  @doc """
  A simple sequence macro that can handle the (a b c) syntax.
  
  Since (expr) is already handled by the parser, we can create a macro
  that detects when it's being called with space-separated arguments.
  """
  
  defmacro sequence(args) do
    IO.puts("Sequence macro called with: #{inspect(args)}")
    
    # For now, just return the arguments as a sequence_call tuple
    case args do
      {name, _meta, call_args} when is_atom(name) and is_list(call_args) ->
        quote do: {:sequence_call, [], [unquote(name) | unquote(call_args)]}
      
      _ ->
        quote do: {:sequence_call, [], [unquote(args)]}
    end
  end
  
  def test_parsing do
    IO.puts("Testing what (foo 123) parses to...")
    
    case Code.string_to_quoted("(foo 123)") do
      {:ok, ast} ->
        IO.puts("AST: #{inspect(ast)}")
      {:error, reason} ->
        IO.puts("Parse error: #{inspect(reason)}")
    end
    
    IO.puts("\nTesting what foo(123) parses to...")
    case Code.string_to_quoted("foo(123)") do
      {:ok, ast} ->
        IO.puts("Regular call AST: #{inspect(ast)}")
      {:error, reason} ->
        IO.puts("Parse error: #{inspect(reason)}")
    end
    
    IO.puts("\nTesting what foo 123 parses to...")
    case Code.string_to_quoted("foo 123") do
      {:ok, ast} ->
        IO.puts("No-parens call AST: #{inspect(ast)}")
      {:error, reason} ->
        IO.puts("Parse error: #{inspect(reason)}")
    end
    
    IO.puts("\nTesting what foo 123 456 parses to...")
    case Code.string_to_quoted("foo 123 456") do
      {:ok, ast} ->
        IO.puts("Multi-arg no-parens call AST: #{inspect(ast)}")
      {:error, reason} ->
        IO.puts("Parse error: #{inspect(reason)}")
    end
  end
end

SequenceMacro.test_parsing()