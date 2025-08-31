# Sequence syntax support for Elixir
# This provides a simple macro-based implementation of sequence syntax

defmodule Kernel.Sequence do
  @moduledoc """
  Provides sequence syntax support for Elixir.

  This module implements parenthesized sequence syntax like `(a b c)` 
  as a compile-time transformation.
  """

  @doc """
  Transforms parenthesized sequences into sequence_literal AST nodes.

  This is meant to be called during macro expansion to detect and
  transform sequences.
  """
  def maybe_transform_sequence({:__block__, meta, [expr]}) do
    # Single expression in parentheses - check if it looks like a sequence
    maybe_transform_sequence_expr(expr)
  end

  def maybe_transform_sequence({func, func_meta, args})
      when is_atom(func) and is_list(args) and length(args) >= 1 do
    # Direct function call pattern - check if all args are simple literals (sequence pattern)
    if all_simple_literals?(args) do
      {:sequence_literal, func_meta, [func | args]}
    else
      {func, func_meta, args}
    end
  end

  defp maybe_transform_sequence_expr({func, func_meta, args})
       when is_atom(func) and is_list(args) and length(args) >= 1 do
    # This looks like a function call - check if all args are simple literals
    if all_simple_literals?(args) do
      {:sequence_literal, func_meta, [func | args]}
    else
      {func, func_meta, args}
    end
  end

  defp maybe_transform_sequence_expr(expr), do: expr

  def maybe_transform_sequence(expr), do: expr

  defp all_simple_literals?([]), do: true

  defp all_simple_literals?([arg | rest]) do
    simple_literal?(arg) and all_simple_literals?(rest)
  end

  defp simple_literal?(arg) when is_atom(arg), do: true
  defp simple_literal?(arg) when is_integer(arg), do: true
  defp simple_literal?(arg) when is_float(arg), do: true
  defp simple_literal?(arg) when is_binary(arg), do: true
  defp simple_literal?({name, _meta, nil}) when is_atom(name), do: true
  defp simple_literal?(_), do: false
end
