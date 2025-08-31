# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 The Elixir Team

Code.require_file("test_helper.exs", __DIR__)

defmodule ReaderMacroTestHelpers do
  defreadermacro env_var("@@" <> rest) do
    "System.get_env(\"#{String.trim(rest)}\")"
  end

  defreadermacro json_literal("json!" <> json_content) do
    "Jason.decode!(\"#{String.trim(json_content)}\")"
  end

  defreadermacro time_literal("time!" <> time_str) do
    "Time.from_iso8601!(\"#{String.trim(time_str)}\")"
  end
end

defmodule ReaderMacroTest do
  use ExUnit.Case, async: true

  import ReaderMacroTestHelpers

  describe "basic reader macro functionality" do
    test "defreadermacro can be defined" do
      # This test passes if the module compiles successfully
      # with defreadermacro definitions
      assert true
    end

    test "reader macros are stored in module data" do
      reader_macros = :elixir_reader_macros.fetch_reader_macros(ReaderMacroTestHelpers)
      assert is_list(reader_macros)
      
      # Check that our reader macros are stored
      names = Enum.map(reader_macros, fn {name, _, _, _, _} -> name end)
      assert :env_var in names
      assert :json_literal in names  
      assert :time_literal in names
    end
  end

  describe "reader macro expansion" do
    test "environment variable reader macro" do
      # Test source code before expansion
      source = "@@HOME"
      module = ReaderMacroTestHelpers
      env = %{module: module}
      
      expanded = :elixir_reader_macros.expand_reader_macros(source, module, env)
      assert expanded =~ "System.get_env(\"HOME\")"
    end

    test "JSON literal reader macro" do
      source = "json!{\"key\": \"value\"}"
      module = ReaderMacroTestHelpers  
      env = %{module: module}
      
      expanded = :elixir_reader_macros.expand_reader_macros(source, module, env)
      assert expanded =~ "Jason.decode!"
      assert expanded =~ "{\"key\": \"value\"}"
    end

    test "time literal reader macro" do
      source = "time!10:30:00"
      module = ReaderMacroTestHelpers
      env = %{module: module}
      
      expanded = :elixir_reader_macros.expand_reader_macros(source, module, env)
      assert expanded =~ "Time.from_iso8601!"
      assert expanded =~ "10:30:00"
    end
  end

  describe "pattern matching" do
    test "binary pattern matching" do
      pattern = "@@"
      source = "@@USER"
      
      result = :elixir_reader_macros.match_pattern(source, pattern)
      assert {:match, "", "@@", "USER"} = result
    end

    test "no match returns nomatch" do
      pattern = "##"
      source = "@@USER"
      
      result = :elixir_reader_macros.match_pattern(source, pattern)
      assert result == :nomatch
    end
  end

  describe "integration with tokenizer" do
    test "tokenizer calls reader macro expansion" do
      # Test that the tokenizer integration exists
      source = "test code"
      line = 1
      column = 1
      env = %{module: ReaderMacroTestHelpers}
      
      # Should not raise an error
      result = :elixir_tokenizer.tokenize_with_reader_macros(source, line, column, env)
      assert {:ok, _, _, _, _, _} = result
    end
  end
end