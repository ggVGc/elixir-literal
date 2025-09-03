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
  
  @doc """
  Handles interpolated string expressions by processing each part.
  
  Elixir represents interpolated strings as `{:<<>>, meta, parts}` where
  parts can be literal strings or interpolated expressions.
  """
  defp handle_interpolated_string(meta, parts) do
    processed_parts = Enum.map(parts, &process_string_part/1)
    {:<<>>, meta, processed_parts}
  end
  
  @doc """
  Processes individual parts of an interpolated string.
  
  - Literal strings are passed through unchanged
  - Interpolated expressions are processed through Lipex evaluation
  """
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
  
  @doc """
  Processes expressions within string interpolation.
  """
  defp process_interpolated_expr(expr) do
    case expr do
      # Kernel.to_string call with Lipex variable
      {{:., kernel_meta, [Kernel, :to_string]}, call_meta, [var_expr]} ->
        # Process the variable through Lipex evaluation
        processed_var = Lipex.eval_lipex_expr(var_expr)
        {{:., kernel_meta, [Kernel, :to_string]}, call_meta, [processed_var]}
      
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