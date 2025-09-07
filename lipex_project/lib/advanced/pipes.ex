defmodule Lipex.Advanced.Pipes do
  @moduledoc """
  Handles pipe operator expressions in Lipex syntax.

  Supports:
  - Pipe operator: (|> initial_value func1 func2 ...)
  """

  @behaviour Lipex.Evaluator

  @doc """
  Tries to evaluate pipe expressions.

  Returns `{:ok, result}` for pipe patterns, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # Handle direct AST nodes from new tokenizer: (|> value func)
      {:sequence_prefix, {:|>, _, nil}, args} ->
        {:ok, eval_pipe({:sequence_prefix, {:|>, [], nil}, args})}

      # Handle sequence_paren patterns: (|> value func)
      {:sequence_paren, _meta, [{:|>, _, nil} | _args]} ->
        {:ok, eval_pipe(expr)}

      # Handle sequence_block patterns: (|> value func)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :|>} | _args]} ->
        {:ok, eval_pipe(expr)}

      _ ->
        :pass
    end
  end

  @doc """
  Evaluates a pipe expression.

  ## Examples

      (|> [1 2 3]
        (Enum.map (fn (x) (* x 2)))
        (Enum.filter (fn (x) (> x 2)))
        (Enum.sum))
  """
  def eval_pipe({:sequence_prefix, {:|>, _, nil}, [initial | operations]}) do
    # Evaluate the initial value
    initial_ast = Lipex.eval_lipex_expr(initial)

    # Build the pipe chain
    Enum.reduce(operations, initial_ast, fn op, acc ->
      {:|>, [], [acc, eval_pipe_operation(op)]}
    end)
  end

  def eval_pipe({:sequence_paren, _meta, [{:|>, _, nil}, initial | operations]}) do
    # Evaluate the initial value
    initial_ast = Lipex.eval_lipex_expr(initial)

    # Build the pipe chain
    Enum.reduce(operations, initial_ast, fn op, acc ->
      {:|>, [], [acc, eval_pipe_operation(op)]}
    end)
  end

  def eval_pipe({:sequence_block, _meta, :"()", [{:sequence_token, _, :|>}, initial | operations]}) do
    # Evaluate the initial value
    initial_ast = Lipex.eval_lipex_expr(initial)

    # Build the pipe chain
    Enum.reduce(operations, initial_ast, fn op, acc ->
      {:|>, [], [acc, eval_pipe_operation(op)]}
    end)
  end

  # Evaluate a single operation in the pipe
  defp eval_pipe_operation({:sequence_paren, _, [func | args]}) do
    # Function call with arguments - handle both module and local functions
    args_ast = Enum.map(args, &Lipex.eval_lipex_expr/1)

    case func do
      # Handle atom function names (including dot notation like :"String.upcase")
      {module_func, _, nil} when is_atom(module_func) ->
        string = Atom.to_string(module_func)

        if String.contains?(string, ".") do
          # This is a module.function call - create proper module alias
          parts = String.split(string, ".")
          {module_parts, [func_name]} = Enum.split(parts, -1)

          # Convert module parts to proper module alias
          module_atoms = Enum.map(module_parts, &String.to_atom/1)
          module_alias = {:__aliases__, [], module_atoms}
          func_atom = String.to_atom(func_name)

          # Generate proper module call using dot notation
          {{:., [], [module_alias, func_atom]}, [], args_ast}
        else
          # Regular local function call
          {module_func, [], args_ast}
        end

      # Handle sequence tokens
      {:sequence_token, _, module_func} when is_atom(module_func) ->
        string = Atom.to_string(module_func)

        if String.contains?(string, ".") do
          # This is a module.function call - create proper module alias
          parts = String.split(string, ".")
          {module_parts, [func_name]} = Enum.split(parts, -1)

          # Convert module parts to proper module alias
          module_atoms = Enum.map(module_parts, &String.to_atom/1)
          module_alias = {:__aliases__, [], module_atoms}
          func_atom = String.to_atom(func_name)

          # Generate proper module call using dot notation
          {{:., [], [module_alias, func_atom]}, [], args_ast}
        else
          # Regular local function call
          {module_func, [], args_ast}
        end

      # Handle direct atoms (like String.upcase as single atom)
      module_func when is_atom(module_func) ->
        string = Atom.to_string(module_func)

        if String.contains?(string, ".") do
          # This is a module.function call - create proper module alias
          parts = String.split(string, ".")
          {module_parts, [func_name]} = Enum.split(parts, -1)

          # Convert module parts to proper module alias
          module_atoms = Enum.map(module_parts, &String.to_atom/1)
          module_alias = {:__aliases__, [], module_atoms}
          func_atom = String.to_atom(func_name)

          # Generate proper module call using dot notation
          {{:., [], [module_alias, func_atom]}, [], args_ast}
        else
          # Regular local function call
          {module_func, [], args_ast}
        end

      other ->
        # Complex expression
        func_ast = Lipex.eval_lipex_expr(other)
        {func_ast, [], args_ast}
    end
  end

  defp eval_pipe_operation({:sequence_token, _, func}) when is_atom(func) do
    # Simple function reference from sequence token - handle module functions
    string = Atom.to_string(func)

    if String.contains?(string, ".") do
      # This is a module.function call - create proper module alias
      parts = String.split(string, ".")
      {module_parts, [func_name]} = Enum.split(parts, -1)

      # Convert module parts to proper module alias
      module_atoms = Enum.map(module_parts, &String.to_atom/1)
      module_alias = {:__aliases__, [], module_atoms}
      func_atom = String.to_atom(func_name)

      # Generate proper module call using dot notation (no arguments, pipe will provide them)
      {{:., [], [module_alias, func_atom]}, [], []}
    else
      # Regular function reference
      {func, [], []}
    end
  end

  defp eval_pipe_operation(func) when is_atom(func) do
    # Simple function reference - handle module functions
    string = Atom.to_string(func)

    if String.contains?(string, ".") do
      # This is a module.function call - create proper module alias
      parts = String.split(string, ".")
      {module_parts, [func_name]} = Enum.split(parts, -1)

      # Convert module parts to proper module alias
      module_atoms = Enum.map(module_parts, &String.to_atom/1)
      module_alias = {:__aliases__, [], module_atoms}
      func_atom = String.to_atom(func_name)

      # Generate proper module call using dot notation (no arguments, pipe will provide them)
      {{:., [], [module_alias, func_atom]}, [], []}
    else
      # Regular function reference
      {func, [], []}
    end
  end

  defp eval_pipe_operation(other) do
    # Evaluate as expression
    Lipex.eval_lipex_expr(other)
  end
end
