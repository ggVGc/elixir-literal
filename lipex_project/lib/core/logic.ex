defmodule Lipex.Core.Logic do
  @moduledoc """
  Handles logical operations in Lipex expressions.

  Supports:
  - Logical operations: `(and ...)`, `(or ...)`, `(not ...)`
  - Type checking: `(atom? ...)`, `(number? ...)`, etc.
  - Boolean operations with short-circuiting
  """

  @behaviour Lipex.Evaluator

  # List of supported logical and type checking operators
  @operators [
    :and,
    :or,
    :not,
    :truthy?,
    :falsy?,
    :nil?,
    :some?,
    :atom?,
    :number?,
    :integer?,
    :float?,
    :string?,
    :list?,
    :tuple?,
    :map?,
    :function?,
    :pid?
  ]

  @doc """
  Tries to evaluate logical and type checking expressions.

  Returns `{:ok, result}` for logical patterns, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # sequence_block format: (and arg1 arg2)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, op} | _args]} when op in @operators ->
        {:ok, eval_logic(expr)}

      # sequence_prefix format with atom operator: (and arg1 arg2)
      {:sequence_prefix, _meta, [op | _args]} when op in @operators ->
        {:ok, eval_logic(expr)}

      # sequence_prefix format with AST node operator: (and arg1 arg2) where and is {:and, _, nil}
      {:sequence_prefix, {op, _, nil}, args} when op in @operators ->
        {:ok, eval_logic({:sequence_prefix, [], [op | args]})}

      # sequence_paren format for type checks: (atom? arg)
      {:sequence_paren, _meta, [{op, _, nil} | args]} when op in @operators ->
        # Convert to sequence_prefix format
        {:ok, eval_logic({:sequence_prefix, [], [op | args]})}

      _ ->
        :pass
    end
  end

  @doc """
  Evaluates logical AND expressions.

  ## Examples

      (and true false)      -> false
      (and true true true)  -> true
      (and)                 -> true
  """
  def eval_logic({:sequence_block, _meta, :"()", [{:sequence_token, _, op} | args]}) when op in @operators do
    # Convert sequence_block to sequence_prefix format and delegate
    eval_logic({:sequence_prefix, [], [op | args]})
  end

  def eval_logic({:sequence_prefix, _meta, [:and | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)

    case elixir_args do
      [] ->
        true

      [single] ->
        single

      [first, second] ->
        # Binary and with short-circuiting
        quote do: unquote(first) and unquote(second)

      multiple ->
        # Multi-argument and - fold from left with short-circuiting
        Enum.reduce(multiple, quote(do: true), fn arg, acc ->
          quote do: unquote(acc) and unquote(arg)
        end)
    end
  end

  # Evaluates logical OR expressions.
  def eval_logic({:sequence_prefix, _meta, [:or | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)

    case elixir_args do
      [] ->
        false

      [single] ->
        single

      [first, second] ->
        # Binary or with short-circuiting
        quote do: unquote(first) or unquote(second)

      multiple ->
        # Multi-argument or - fold from left with short-circuiting
        Enum.reduce(multiple, quote(do: false), fn arg, acc ->
          quote do: unquote(acc) or unquote(arg)
        end)
    end
  end

  # Evaluates logical NOT expressions.
  def eval_logic({:sequence_prefix, _meta, [:not, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: not unquote(elixir_arg)
  end

  # Evaluates truthiness testing.
  def eval_logic({:sequence_prefix, _meta, [:truthy?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)

    quote do
      case unquote(elixir_arg) do
        nil -> false
        false -> false
        _ -> true
      end
    end
  end

  def eval_logic({:sequence_prefix, _meta, [:falsy?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)

    quote do
      case unquote(elixir_arg) do
        nil -> true
        false -> true
        _ -> false
      end
    end
  end

  # Evaluates nil checking operations.
  def eval_logic({:sequence_prefix, _meta, [:nil?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_nil(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:some?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: not is_nil(unquote(elixir_arg))
  end

  # Evaluates type checking operations.
  def eval_logic({:sequence_prefix, _meta, [:atom?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_atom(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:number?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_number(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:integer?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_integer(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:float?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_float(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:string?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_binary(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:list?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_list(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:tuple?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_tuple(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:map?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_map(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:function?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_function(unquote(elixir_arg))
  end

  def eval_logic({:sequence_prefix, _meta, [:pid?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_pid(unquote(elixir_arg))
  end
end
