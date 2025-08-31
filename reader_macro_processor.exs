#!/usr/bin/env elixir

defmodule ReaderMacroProcessor do
  @moduledoc """
  A simple reader macro preprocessor for Elixir.
  
  This script processes .exs files that contain reader macros,
  applies the transformations, and outputs the processed Elixir code.
  """

  def process_file(filename) do
    content = File.read!(filename)
    process_content(content)
  end

  def process_content(content) do
    {reader_macros, cleaned_content} = extract_reader_macros(content)
    apply_reader_macros(cleaned_content, reader_macros)
  end

  defp extract_reader_macros(content) do
    # Extract simple defreadermacro definitions
    # Pattern: defreadermacro name("pattern") do "replacement" end
    # Handle escaped quotes in replacement strings
    regex = ~r/defreadermacro\s+(\w+)\("([^"]+)"\)\s*do\s*"((?:[^"\\]|\\.)*)"\s*end/ms
    
    matches = Regex.scan(regex, content)
    reader_macros = for [_, name, pattern, replacement] <- matches do
      # Unescape quotes in replacement string - handle both \" and \\"
      unescaped_replacement = replacement
      |> String.replace(~s(\\\"), ~s("))  # Replace \\\" with "
      |> String.replace(~s(\"), ~s("))    # Replace \" with " 
      {name, pattern, unescaped_replacement}
    end
    
    # Remove reader macro definitions from content - remove all matches
    cleaned_content = Regex.replace(regex, content, "", [global: true])
    
    {reader_macros, cleaned_content}
  end

  defp apply_reader_macros(content, reader_macros) do
    Enum.reduce(reader_macros, content, fn {_name, pattern, replacement}, acc ->
      String.replace(acc, pattern, replacement)
    end)
  end
end

# If run as script
if System.argv() |> length() > 0 do
  filename = System.argv() |> hd()
  processed = ReaderMacroProcessor.process_file(filename)
  IO.puts(processed)
else
  IO.puts("Usage: elixir reader_macro_processor.exs filename.exs")
end