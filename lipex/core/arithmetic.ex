defmodule Lipex.Core.Arithmetic do
  @moduledoc """
  Handles arithmetic and comparison operations in Lipex expressions.
  
  Supports:
  - Arithmetic: `(+ ...)`, `(- ...)`, `(* ...)`, `(/ ...)`
  - Comparison: `(< ...)`, `(> ...)`, `(<= ...)`, `(>= ...)`, `(== ...)`, `(!= ...)`
  """
  
  @doc """
  Evaluates arithmetic expressions.
  
  ## Examples
  
      (+ 1 2 3)         -> 6
      (- 10 3)          -> 7 
      (* 2 3 4)         -> 24
      (/ 12 4)          -> 3.0
  """
  def eval_arithmetic({:sequence_prefix, _meta, [:+ | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> 0
      [single] -> single
      multiple -> 
        quote do: Enum.reduce(unquote(multiple), 0, &+/2)
    end
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:- | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> raise "Subtraction requires at least one argument"
      [single] -> 
        # Unary negation: (- x) -> -x
        quote do: -unquote(single)
      [first, second] ->
        # Binary subtraction: (- a b) -> a - b  
        quote do: unquote(first) - unquote(second)
      [first | rest] ->
        # Multi-argument subtraction: (- a b c d) -> a - b - c - d
        quote do: Enum.reduce(unquote(rest), unquote(first), fn b, a -> a - b end)
    end
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:* | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> 1
      [single] -> single
      multiple ->
        quote do: Enum.reduce(unquote(multiple), 1, &*/2)
    end
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:/ | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> raise "Division requires at least one argument"
      [single] ->
        # Reciprocal: (/ x) -> 1/x
        quote do: 1 / unquote(single)
      [dividend, divisor] ->
        # Binary division: (/ a b) -> a / b
        quote do: unquote(dividend) / unquote(divisor)
      [first | rest] ->
        # Multi-argument division: (/ a b c d) -> a / b / c / d
        quote do: Enum.reduce(unquote(rest), unquote(first), fn b, a -> a / b end)
    end
  end
  
  @doc """
  Evaluates comparison expressions.
  
  ## Examples
  
      (< 1 2)           -> true
      (>= 5 5)          -> true
      (== a b)          -> true if a equals b
      (!= a b)          -> true if a doesn't equal b
  """
  def eval_arithmetic({:sequence_prefix, _meta, [:<, left, right]}) do
    elixir_left = Lipex.eval_lipex_expr(left)
    elixir_right = Lipex.eval_lipex_expr(right)
    quote do: unquote(elixir_left) < unquote(elixir_right)
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:>, left, right]}) do
    elixir_left = Lipex.eval_lipex_expr(left)
    elixir_right = Lipex.eval_lipex_expr(right)
    quote do: unquote(elixir_left) > unquote(elixir_right)
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:<=, left, right]}) do
    elixir_left = Lipex.eval_lipex_expr(left)
    elixir_right = Lipex.eval_lipex_expr(right)
    quote do: unquote(elixir_left) <= unquote(elixir_right)
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:>=, left, right]}) do
    elixir_left = Lipex.eval_lipex_expr(left)
    elixir_right = Lipex.eval_lipex_expr(right)
    quote do: unquote(elixir_left) >= unquote(elixir_right)
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:==, left, right]}) do
    elixir_left = Lipex.eval_lipex_expr(left)
    elixir_right = Lipex.eval_lipex_expr(right)
    quote do: unquote(elixir_left) == unquote(elixir_right)
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:!=, left, right]}) do
    elixir_left = Lipex.eval_lipex_expr(left)
    elixir_right = Lipex.eval_lipex_expr(right)
    quote do: unquote(elixir_left) != unquote(elixir_right)
  end
  
  # Support for Lisp-style equality with special nil/empty list handling
  def eval_arithmetic({:sequence_prefix, _meta, [:=, left, right]}) do
    elixir_left = Lipex.eval_lipex_expr(left)
    elixir_right = Lipex.eval_lipex_expr(right)
    quote do
      # Handle Lisp-style list comparison where nil and empty list are equivalent
      case {unquote(elixir_left), unquote(elixir_right)} do
        {[], nil} -> true
        {nil, []} -> true
        {a, b} -> a == b
      end
    end
  end
  
  @doc """
  Evaluates modulo and power operations.
  
  ## Examples
  
      (rem 10 3)        -> 1
      (pow 2 8)         -> 256
  """
  def eval_arithmetic({:sequence_prefix, _meta, [:rem, dividend, divisor]}) do
    elixir_dividend = Lipex.eval_lipex_expr(dividend)
    elixir_divisor = Lipex.eval_lipex_expr(divisor)
    quote do: rem(unquote(elixir_dividend), unquote(elixir_divisor))
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:pow, base, exponent]}) do
    elixir_base = Lipex.eval_lipex_expr(base)
    elixir_exponent = Lipex.eval_lipex_expr(exponent)
    quote do: :math.pow(unquote(elixir_base), unquote(elixir_exponent))
  end
  
  @doc """
  Evaluates mathematical functions.
  
  ## Examples
  
      (abs -5)          -> 5
      (min 1 2 3)       -> 1
      (max 1 2 3)       -> 3
  """
  def eval_arithmetic({:sequence_prefix, _meta, [:abs, value]}) do
    elixir_value = Lipex.eval_lipex_expr(value)
    quote do: abs(unquote(elixir_value))
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:min | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> raise "min requires at least one argument"
      [single] -> single
      multiple ->
        quote do: Enum.min(unquote(multiple))
    end
  end
  
  def eval_arithmetic({:sequence_prefix, _meta, [:max | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case elixir_args do
      [] -> raise "max requires at least one argument" 
      [single] -> single
      multiple ->
        quote do: Enum.max(unquote(multiple))
    end
  end
end