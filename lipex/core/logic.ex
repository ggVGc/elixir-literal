defmodule Lipex.Core.Logic do
  @moduledoc """
  Handles logical operations in Lipex expressions.
  
  Supports:
  - Logical operations: `(and ...)`, `(or ...)`, `(not ...)`
  - Boolean operations with short-circuiting
  """
  
  @doc """
  Evaluates logical AND expressions.
  
  ## Examples
  
      (and true false)      -> false
      (and true true true)  -> true
      (and)                 -> true
  """
  def eval_logic({:sequence_prefix, _meta, [:and | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> true
      [single] -> single
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
  
  @doc """
  Evaluates logical OR expressions.
  
  ## Examples
  
      (or false true)       -> true
      (or false false false) -> false
      (or)                  -> false
  """
  def eval_logic({:sequence_prefix, _meta, [:or | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> false
      [single] -> single
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
  
  @doc """
  Evaluates logical NOT expressions.
  
  ## Examples
  
      (not true)            -> false
      (not false)           -> true
      (not nil)             -> true
  """
  def eval_logic({:sequence_prefix, _meta, [:not, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: not unquote(elixir_arg)
  end
  
  @doc """
  Evaluates truthiness testing.
  
  ## Examples
  
      (truthy? nil)         -> false
      (truthy? false)       -> false  
      (truthy? 0)           -> true
      (falsy? nil)          -> true
  """
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
  
  @doc """
  Evaluates nil checking operations.
  
  ## Examples
  
      (nil? nil)            -> true
      (nil? false)          -> false
      (some? nil)           -> false
      (some? 42)            -> true
  """
  def eval_logic({:sequence_prefix, _meta, [:nil?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: is_nil(unquote(elixir_arg))
  end
  
  def eval_logic({:sequence_prefix, _meta, [:some?, arg]}) do
    elixir_arg = Lipex.eval_lipex_expr(arg)
    quote do: not is_nil(unquote(elixir_arg))
  end
  
  @doc """
  Evaluates type checking operations.
  
  ## Examples
  
      (atom? :hello)        -> true
      (number? 42)          -> true
      (string? "hello")     -> true
      (list? [1 2 3])       -> true
  """
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