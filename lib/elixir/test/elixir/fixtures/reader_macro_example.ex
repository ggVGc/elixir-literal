# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 The Elixir Team

defmodule ReaderMacroExample do
  @doc """
  Example usage of reader macros in Elixir.

  This module demonstrates how to define and use reader macros
  that work on raw source code during tokenization.
  """

  # Define a reader macro for environment variables
  defreadermacro env("@@" <> var_name) do
    "System.get_env(\"#{String.trim(var_name)}\")"
  end

  # Define a reader macro for regex literals
  defreadermacro regex("r/" <> pattern) do
    [clean_pattern, _] = String.split(pattern, "/", parts: 2)
    "~r/#{clean_pattern}/"
  end

  # Define a reader macro for SQL-like syntax
  defreadermacro sql("SQL:" <> query) do
    "MyApp.Database.query(\"#{String.trim(query)}\")"
  end

  def example_usage do
    # These would be transformed by reader macros:
    # @@HOME -> System.get_env("HOME")
    # r/\d+/ -> ~r/\d+/
    # SQL:SELECT * FROM users -> MyApp.Database.query("SELECT * FROM users")

    IO.puts("Reader macro examples would be expanded here")
  end
end
