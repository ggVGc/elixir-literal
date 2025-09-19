# Sequence syntax support for Elixir
# This provides utilities for working with sequence literals

defmodule Kernel.Sequence do
  @moduledoc """
  Provides utilities for working with sequence literals in Elixir.

  Sequence literals are created using the `~~(...)` syntax and are represented
  as `{:raw_section, metadata, arguments}` AST nodes.

  ## Examples

      iex> Code.string_to_quoted("~~(foo bar baz)")
      {:ok, {:raw_section, [line: 1], 
       [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}, {:baz, [line: 1], nil}]}}

  ## Supported Features

  Sequence literals support a wide variety of content:

  - **Identifiers**: `~~(foo bar baz)` 
  - **Numbers**: `~~(item 123 45.67)`
  - **Atoms**: `~~(config :debug :verbose)`
  - **Strings**: `~~(message "hello world")`  
  - **Booleans and nil**: `~~(flags true false nil)`
  - **Nested expressions**: `~~(calc (1 + 2) (foo bar))`
  - **Mixed types**: `~~(data 123 :atom "string" true)`

  ## Current Limitations

  - CamelCase aliases are not supported (use lowercase identifiers)
  - Sequences can only be used at the top-level expression context  
  - Sequences cannot be used inside function calls or assignments
  - Empty sequences `~~()` are not supported

  """

  @doc """
  Checks if the given AST represents a sequence literal.

  ## Examples

      iex> Kernel.Sequence.raw_section?({:raw_section, [], [:foo, :bar]})
      true
      
      iex> Kernel.Sequence.raw_section?({:foo, [], [:bar]})
      false
  """
  def raw_section?({:raw_section, _, _}), do: true
  def raw_section?(_), do: false

  @doc """
  Extracts the arguments from a sequence literal.

  ## Examples

      iex> ast = {:raw_section, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
      iex> Kernel.Sequence.raw_args(ast)
      [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]
  """
  def raw_args({:raw_section, _, args}), do: args

  @doc """
  Returns the number of arguments in a sequence literal.

  ## Examples

      iex> ast = {:raw_section, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
      iex> Kernel.Sequence.raw_arity(ast)
      2
  """
  def raw_arity({:raw_section, _, args}), do: length(args)
end
