defmodule Lipex.Evaluator do
  @moduledoc """
  Behavior for Lipex expression evaluation modules.

  Each evaluation module (e.g., Arithmetic, Logic, DataStructures) implements this
  behavior to provide a standard interface for trying to evaluate expressions.

  This allows lipex.ex to remain simple by just trying each module in sequence
  until one successfully handles the expression.
  """

  @doc """
  Attempts to evaluate a Lipex expression.

  ## Returns
  - `{:ok, ast}` - Successfully handled the expression, returns Elixir AST
  - `:pass` - This module doesn't handle this type of expression
  - `{:error, reason}` - Error occurred while processing the expression

  ## Examples

      # A module that handles arithmetic might:
      iex> ArithmeticModule.try_eval({:sequence_prefix, [], [:+, 1, 2]})
      {:ok, {:+, [], [1, 2]}}
      
      iex> ArithmeticModule.try_eval({:sequence_prefix, [], [:if, true, 1]})
      :pass  # Doesn't handle control flow
      
      iex> ArithmeticModule.try_eval({:sequence_prefix, [], [:+]})
      {:error, "Addition requires at least one argument"}
  """
  @callback try_eval(expr :: any()) :: {:ok, any()} | :pass | {:error, String.t()}
end
