defmodule Lipex.Functions.Anonymous do
  @moduledoc """
  Handles anonymous functions and function captures in Lipex syntax.

  Supports:
  - Anonymous functions: (fn (args) body)
  - Function captures: (& function/arity) or (& expr)
  """

  @doc """
  Evaluates an anonymous function expression.

  ## Examples

      (fn (x) (* x 2))
      (fn (x y) (+ x y))
      (fn () (IO.puts "Hello"))
  """
  def eval_fn({:sequence_paren, _meta, [{:fn, _, nil} | clauses]}) do
    # Process function clauses
    clauses_ast = Enum.map(clauses, &process_fn_clause/1)

    # Build the fn AST
    {:fn, [], clauses_ast}
  end

  @doc """
  Evaluates a function capture expression.

  ## Examples

      (& Enum.map/2)
      (& (+ &1 &2))
      (& &1)
  """
  def eval_capture({:sequence_paren, _meta, [{:&, _, nil}, expr]}) do
    case expr do
      # Handle function/arity captures like (& Module.function/2)
      {:/, _, [func, arity]} ->
        {:&, [], [{:/, [], [eval_capture_function(func), arity]}]}

      # Handle expressions with placeholder arguments
      _ ->
        {:&, [], [Lipex.eval_lipex_expr(expr)]}
    end
  end

  # Process a single function clause
  defp process_fn_clause({:sequence_paren, _, [args_list | body]})
       when is_list(args_list) or
              tuple_size(args_list) == 3 do
    # Parse arguments
    args_ast = parse_fn_args(args_list)

    # Parse body
    body_ast =
      case body do
        [single] -> Lipex.eval_lipex_expr(single)
        multiple -> {:__block__, [], Enum.map(multiple, &Lipex.eval_lipex_expr/1)}
      end

    {:->, [], [args_ast, body_ast]}
  end

  # Handle single expression functions (zero arity)
  defp process_fn_clause(expr) do
    {:->, [], [[], Lipex.eval_lipex_expr(expr)]}
  end

  # Parse function arguments
  defp parse_fn_args({:sequence_paren, _, args}) when is_list(args) do
    Enum.map(args, &convert_fn_arg/1)
  end

  defp parse_fn_args(args) when is_list(args) do
    Enum.map(args, &convert_fn_arg/1)
  end

  defp parse_fn_args(arg) do
    [convert_fn_arg(arg)]
  end

  # Convert individual function argument
  defp convert_fn_arg(arg) when is_atom(arg) do
    Lipex.lipex_to_elixir_var(arg)
  end

  defp convert_fn_arg(arg) when is_number(arg) do
    arg
  end

  defp convert_fn_arg(arg) when is_binary(arg) do
    arg
  end

  defp convert_fn_arg({:sequence_paren, _, pattern}) do
    # Handle pattern matching in arguments
    pattern
  end

  defp convert_fn_arg(other) do
    Lipex.eval_lipex_expr(other)
  end

  # Evaluate captured function reference
  defp eval_capture_function({:., _, [module, func]}) do
    {:., [], [module, func]}
  end

  defp eval_capture_function(func) when is_atom(func) do
    func
  end

  defp eval_capture_function(other) do
    Lipex.eval_lipex_expr(other)
  end
end
