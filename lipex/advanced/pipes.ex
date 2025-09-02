defmodule Lipex.Advanced.Pipes do
  @moduledoc """
  Handles pipe operator expressions in Lipex syntax.
  
  Supports:
  - Pipe operator: (|> initial_value func1 func2 ...)
  """
  
  @doc """
  Evaluates a pipe expression.
  
  ## Examples
  
      (|> [1 2 3]
        (Enum.map (fn (x) (* x 2)))
        (Enum.filter (fn (x) (> x 2)))
        (Enum.sum))
  """
  def eval_pipe({:sequence_paren, _meta, [{:|>, _, nil}, initial | operations]}) do
    # Evaluate the initial value
    initial_ast = Lipex.eval_lipex_expr(initial)
    
    # Build the pipe chain
    Enum.reduce(operations, initial_ast, fn op, acc ->
      {:|>, [], [acc, eval_pipe_operation(op)]}
    end)
  end
  
  # Evaluate a single operation in the pipe
  defp eval_pipe_operation({:sequence_paren, _, [func | args]}) do
    # Function call with arguments
    func_ast = Lipex.eval_lipex_expr(func)
    args_ast = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    # Build partial application
    case func_ast do
      {:., _, _} = dot_call ->
        # Module.function call
        {dot_call, [], args_ast}
      func_name when is_atom(func_name) ->
        # Simple function name
        {func_name, [], args_ast}
      other ->
        # Complex expression
        {other, args_ast}
    end
  end
  
  defp eval_pipe_operation(func) when is_atom(func) do
    # Simple function reference
    func
  end
  
  defp eval_pipe_operation(other) do
    # Evaluate as expression
    Lipex.eval_lipex_expr(other)
  end
end