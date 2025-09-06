defmodule Lipex.Strings.Interpolation do
  @moduledoc """
  Handles string interpolation in Lipex expressions.

  Supports Elixir's standard interpolated string syntax like "text \#{variable}" 
  when used within Lipex sequence literals.
  """

  @behaviour Lipex.Evaluator

  @doc """
  Tries to evaluate string interpolation expressions.

  Returns `{:ok, result}` for interpolated strings, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # Handle Elixir's interpolated string AST: {:<<>>, meta, parts}
      {:<<>>, meta, parts} ->
        {:ok, handle_interpolated_string(meta, parts)}

      # Not a string interpolation expression
      _ ->
        :pass
    end
  end

  defp handle_interpolated_string(meta, parts) do
    processed_parts = Enum.map(parts, &process_string_part/1)
    {:<<>>, meta, processed_parts}
  end

  defp process_string_part(part) do
    case part do
      # Literal string part - pass through unchanged
      string when is_binary(string) ->
        string

      # Interpolated expression: {:"::", meta, [expr, {:binary, meta, nil}]}
      {:"::", meta, [expr, {:binary, _, nil}]} ->
        # Process the expression through Lipex evaluation
        processed_expr = process_interpolated_expr(expr)
        {:"::", meta, [processed_expr, {:binary, meta, nil}]}

      # Other parts - pass through unchanged
      other ->
        other
    end
  end

  defp process_interpolated_expr(expr) do
    case expr do
      # Kernel.to_string call with expression inside
      {{:., kernel_meta, [Kernel, :to_string]}, call_meta, [inner_expr]} ->
        # Process the inner expression through Lipex evaluation first
        processed_expr = process_interpolated_expr(inner_expr)
        {{:., kernel_meta, [Kernel, :to_string]}, call_meta, [processed_expr]}

      # Lipex sequence expressions - these need to be evaluated through Lipex pipeline
      {:sequence_prefix, meta, args} ->
        # This is a Lipex expression like (+ 3 4) - evaluate it
        Lipex.eval_lipex_expr({:sequence_prefix, meta, args})

      {:sequence_paren, meta, args} ->
        # This is a Lipex expression like (+ 3 4) in paren form - evaluate it
        Lipex.eval_lipex_expr({:sequence_paren, meta, args})

      # Direct variable reference
      {var, meta, nil} when is_atom(var) ->
        # Convert to Elixir variable format
        Lipex.eval_lipex_expr({var, meta, nil})

      # Other expressions - evaluate through Lipex
      other ->
        Lipex.eval_lipex_expr(other)
    end
  end
end
