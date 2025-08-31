# Sequence syntax support for Elixir
# This provides utilities for working with sequence literals

defmodule Kernel.Sequence do
  @moduledoc """
  Provides utilities for working with sequence literals in Elixir.

  Sequence literals are created using the `~~(...)` syntax and are represented
  as `{:sequence_literal, metadata, arguments}` AST nodes.

  ## Examples

      iex> Code.string_to_quoted("~~(foo bar baz)")
      {:ok, {:sequence_literal, [line: 1], 
       [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}, {:baz, [line: 1], nil}]}}

  ## Current Limitations
  
  The sequence syntax currently has the following limitations:
  
  - Only simple identifiers are supported (no numbers, strings, or complex expressions)
  - Only lowercase identifiers work (CamelCase aliases are not supported)
  - Sequences can only be used at the top-level expression context
  - Sequences cannot be used inside function calls or assignments
  - Empty sequences `~~()` are not supported
  
  These limitations may be addressed in future versions.

  """

  @doc """
  Checks if the given AST represents a sequence literal.

  ## Examples

      iex> Kernel.Sequence.sequence_literal?({:sequence_literal, [], [:foo, :bar]})
      true
      
      iex> Kernel.Sequence.sequence_literal?({:foo, [], [:bar]})
      false
  """
  def sequence_literal?({:sequence_literal, _, _}), do: true
  def sequence_literal?(_), do: false

  @doc """
  Extracts the arguments from a sequence literal.

  ## Examples

      iex> ast = {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
      iex> Kernel.Sequence.sequence_args(ast)
      [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]
  """
  def sequence_args({:sequence_literal, _, args}), do: args

  @doc """
  Returns the number of arguments in a sequence literal.

  ## Examples

      iex> ast = {:sequence_literal, [line: 1], [{:foo, [line: 1], nil}, {:bar, [line: 1], nil}]}
      iex> Kernel.Sequence.sequence_arity(ast)
      2
  """
  def sequence_arity({:sequence_literal, _, args}), do: length(args)
end
