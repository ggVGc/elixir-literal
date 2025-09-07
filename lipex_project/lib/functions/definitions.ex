defmodule Lipex.Functions.Definitions do
  @moduledoc """
  Handles function and module definitions in Lipex syntax.

  Supports:
  - Module definitions: (defmodule ModuleName ...)
  - Public functions: (def function_name (args) body)
  - Private functions: (defp function_name (args) body)
  - Macros: (defmacro macro_name (args) body)
  """

  @behaviour Lipex.Evaluator

  @doc """
  Tries to evaluate function and module definition expressions.

  Returns `{:ok, result}` for definition patterns, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # defmodule patterns in sequence_block format
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :defmodule} | args]} ->
        {:ok, eval_defmodule({:sequence_paren, [], [{:defmodule, [], nil} | args]})}

      # defmodule patterns
      {:sequence_paren, _meta, [{:defmodule, _, nil} | _args]} ->
        {:ok, eval_defmodule(expr)}

      # def patterns in sequence_block format
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :def} | args]} ->
        {:ok, eval_def({:sequence_paren, [], [{:def, [], nil} | args]})}

      # defp patterns in sequence_block format
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :defp} | args]} ->
        {:ok, eval_defp({:sequence_paren, [], [{:defp, [], nil} | args]})}

      # defmacro patterns in sequence_block format
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :defmacro} | args]} ->
        {:ok, eval_defmacro({:sequence_paren, [], [{:defmacro, [], nil} | args]})}

      # def patterns - multiple formats due to AST variations
      # Handle sequence_prefix where def is the operator (e.g., (def square (x) ...))
      {:sequence_prefix, {:def, _, nil}, args} ->
        {:ok, eval_def({:sequence_paren, [], [{:def, [], nil} | args]})}

      {:sequence_paren, _meta, [{:sequence_prefix, _, [:def | args]}]} ->
        {:ok, eval_def({:sequence_paren, [], [:def | args]})}

      {:sequence_paren, _meta, [{:def, _, nil} | _args]} ->
        {:ok, eval_def(expr)}

      {:sequence_prefix, _meta, [:def | args]} ->
        {:ok, eval_def({:sequence_paren, [], [:def | args]})}

      # defp patterns
      # Handle sequence_prefix where defp is the operator
      {:sequence_prefix, {:defp, _, nil}, args} ->
        {:ok, eval_defp({:sequence_paren, [], [{:defp, [], nil} | args]})}

      {:sequence_paren, _meta, [{:sequence_prefix, _, [:defp | args]}]} ->
        {:ok, eval_defp({:sequence_paren, [], [:defp | args]})}

      {:sequence_paren, _meta, [{:defp, _, nil} | _args]} ->
        {:ok, eval_defp(expr)}

      {:sequence_prefix, _meta, [:defp | args]} ->
        {:ok, eval_defp({:sequence_paren, [], [:defp | args]})}

      # defmacro patterns
      # Handle sequence_prefix where defmacro is the operator
      {:sequence_prefix, {:defmacro, _, nil}, args} ->
        {:ok, eval_defmacro({:sequence_paren, [], [{:defmacro, [], nil} | args]})}

      {:sequence_paren, _meta, [{:sequence_prefix, _, [:defmacro | args]}]} ->
        {:ok, eval_defmacro({:sequence_paren, [], [:defmacro | args]})}

      {:sequence_paren, _meta, [{:defmacro, _, nil} | _args]} ->
        {:ok, eval_defmacro(expr)}

      {:sequence_prefix, _meta, [:defmacro | args]} ->
        {:ok, eval_defmacro({:sequence_paren, [], [:defmacro | args]})}

      _ ->
        :pass
    end
  end

  @doc """
  Evaluates a defmodule expression.

  ## Examples

      (defmodule MyModule
        (def add (x y) (+ x y))
        (defp validate (x) (> x 0)))
  """
  def eval_defmodule({:sequence_paren, _meta, [{:defmodule, _, nil}, module_name | body]}) do
    module_ast =
      case module_name do
        # Handle aliased module names
        {:__aliases__, meta, parts} -> {:__aliases__, meta, parts}
        # Handle single atom module names
        atom when is_atom(atom) -> {:__aliases__, [], [atom]}
        # Handle other cases
        other -> other
      end

    # Process body expressions
    body_ast = Enum.map(body, &Lipex.eval_lipex_expr/1)

    # Build the defmodule AST
    {:defmodule, [], [module_ast, [do: {:__block__, [], body_ast}]]}
  end

  @doc """
  Evaluates a def (public function) expression.

  ## Examples

      (def add (x y) (+ x y))
      (def factorial (0) 1)
      (def factorial (n) :when (> n 0) (* n (factorial (- n 1))))
  """
  def eval_def({:sequence_paren, _meta, [:def, name | rest]}) do
    eval_function_def(:def, name, rest)
  end

  def eval_def({:sequence_paren, _meta, [{:def, _, nil}, name | rest]}) do
    eval_function_def(:def, name, rest)
  end

  @doc """
  Evaluates a defp (private function) expression.
  """
  def eval_defp({:sequence_paren, _meta, [:defp, name | rest]}) do
    eval_function_def(:defp, name, rest)
  end

  def eval_defp({:sequence_paren, _meta, [{:defp, _, nil}, name | rest]}) do
    eval_function_def(:defp, name, rest)
  end

  @doc """
  Evaluates a defmacro expression.
  """
  def eval_defmacro({:sequence_paren, _meta, [:defmacro, name | rest]}) do
    eval_function_def(:defmacro, name, rest)
  end

  def eval_defmacro({:sequence_paren, _meta, [{:defmacro, _, nil}, name | rest]}) do
    eval_function_def(:defmacro, name, rest)
  end

  # Helper function to handle all types of function definitions
  defp eval_function_def(def_type, name, rest) do
    {args, body_parts} = parse_function_parts(rest)

    # Check for guard clause
    {guard, actual_body} =
      case body_parts do
        [:when, guard_expr | body] ->
          {convert_to_ast(guard_expr), body}

        body ->
          {nil, body}
      end

    # Convert arguments
    args_ast = convert_args(args)

    # Process body - convert directly to AST without evaluation
    body_ast =
      case actual_body do
        [single] -> convert_to_ast(single)
        multiple -> {:__block__, [], Enum.map(multiple, &convert_to_ast/1)}
      end

    # Extract function name atom from AST
    function_name =
      case name do
        {atom, _meta, nil} when is_atom(atom) -> atom
        atom when is_atom(atom) -> atom
        _ -> name
      end

    # Build the function definition
    if guard do
      # Function with guard
      {def_type, [],
       [
         {:when, [],
          [
            {function_name, [], args_ast},
            guard
          ]},
         [do: body_ast]
       ]}
    else
      # Regular function
      {def_type, [],
       [
         {function_name, [], args_ast},
         [do: body_ast]
       ]}
    end
  end

  # Parse function arguments and body
  defp parse_function_parts([{:sequence_paren, _, args} | body]) do
    # Check for inline when syntax: (args) when guard_expr body
    case body do
      [:when, guard_expr | actual_body] ->
        {args, [:when, guard_expr | actual_body]}
      
      [{:when, _, nil} | rest] ->
        # Handle when as AST node
        case rest do
          [guard_expr | actual_body] ->
            {args, [:when, guard_expr | actual_body]}
          _ ->
            {args, body}
        end
      
      _ ->
        {args, body}
    end
  end

  defp parse_function_parts([{:sequence_block, _, :"()", args} | body]) do
    # Check for inline when syntax: (args) when guard_expr body
    case body do
      [:when, guard_expr | actual_body] ->
        {args, [:when, guard_expr | actual_body]}
      
      [{:when, _, nil} | rest] ->
        # Handle when as AST node
        case rest do
          [guard_expr | actual_body] ->
            {args, [:when, guard_expr | actual_body]}
          _ ->
            {args, body}
        end
      
      _ ->
        {args, body}
    end
  end

  defp parse_function_parts([args | body]) when is_list(args) do
    # Check for inline when syntax when args is a plain list
    case body do
      [:when, guard_expr | actual_body] ->
        {args, [:when, guard_expr | actual_body]}
      
      [{:when, _, nil} | rest] ->
        # Handle when as AST node
        case rest do
          [guard_expr | actual_body] ->
            {args, [:when, guard_expr | actual_body]}
          _ ->
            {args, body}
        end
      
      _ ->
        {args, body}
    end
  end

  defp parse_function_parts(parts) do
    # If no explicit args list, treat as zero-arity function
    # Check if first element is 'when' for zero-arity functions with guards
    case parts do
      [:when, guard_expr | actual_body] ->
        {[], [:when, guard_expr | actual_body]}
      
      [{:when, _, nil} | rest] ->
        # Handle when as AST node
        case rest do
          [guard_expr | actual_body] ->
            {[], [:when, guard_expr | actual_body]}
          _ ->
            {[], parts}
        end
      
      _ ->
        {[], parts}
    end
  end

  # Convert argument list to AST
  defp convert_args(args) when is_list(args) do
    Enum.map(args, &convert_arg/1)
  end

  defp convert_args(args), do: [convert_arg(args)]

  defp convert_arg(arg) when is_atom(arg) do
    # Validate that the atom is a valid Elixir variable name
    arg_string = Atom.to_string(arg)
    
    unless valid_elixir_variable_name?(arg_string) do
      raise ArgumentError, "Invalid parameter name: '#{arg}' is not a valid Elixir variable name"
    end
    
    Lipex.lipex_to_elixir_var(arg)
  end

  defp convert_arg(arg) when is_number(arg) do
    arg
  end

  defp convert_arg({:sequence_paren, _, args}) do
    # Handle parenthesized arguments like (x) -> x
    # Extract the arguments from within the parentheses
    case args do
      [single_arg] -> convert_arg(single_arg)  # Recursive call for the inner arg
      multiple_args -> Enum.map(multiple_args, &convert_arg/1)
    end
  end

  defp convert_arg({:sequence_token, meta, arg_name}) when is_atom(arg_name) do
    # Handle sequence_token arguments from sequence_block
    # This is how variables appear within (...) in sequence literals
    # Validate that the atom is a valid Elixir variable name
    arg_string = Atom.to_string(arg_name)
    
    unless valid_elixir_variable_name?(arg_string) do
      raise ArgumentError, "Invalid parameter name: '#{arg_name}' is not a valid Elixir variable name"
    end
    
    {arg_name, normalize_meta(meta), nil}
  end

  defp convert_arg({:sequence_prefix, {arg_name, meta, nil}, []}) when is_atom(arg_name) do
    # Handle parenthesized single arguments like (x) -> x
    # This occurs when sequence literals parse (x) as {:sequence_prefix, {:x, meta, nil}, []}
    {arg_name, meta, nil}
  end

  defp convert_arg(other) do
    Lipex.eval_lipex_expr(other)
  end

  # Convert sequence structures directly to Elixir AST without evaluation
  defp convert_to_ast({:sequence_block, _meta, :"()", [{:sequence_token, op_meta, op} | args]}) do
    # Convert (op arg1 arg2) directly to {op, [], [arg1_ast, arg2_ast]}
    ast_args = Enum.map(args, &convert_to_ast/1)
    {op, normalize_meta(op_meta), ast_args}
  end

  defp convert_to_ast({:sequence_block, _meta, :"()", []}) do
    # Empty parentheses
    nil
  end

  defp convert_to_ast({:sequence_token, meta, value}) do
    # Convert sequence tokens to AST variables or literals
    case value do
      nil -> nil
      true -> true
      false -> false
      atom when is_atom(atom) -> {atom, normalize_meta(meta), nil}
    end
  end

  defp convert_to_ast({:sequence_number, _meta, value}) do
    # Numbers are literals
    value
  end

  defp convert_to_ast({:sequence_atom, _meta, value}) do
    # Atoms are literals
    value
  end

  defp convert_to_ast({:sequence_string, _meta, value}) do
    # Strings are literals
    if is_list(value) do
      List.to_string(value)
    else
      value
    end
  end

  defp convert_to_ast(other) do
    # For anything else, fall back to the main evaluator
    Lipex.eval_lipex_expr(other)
  end

  # Convert patterns for pattern matching
  defp convert_pattern(pattern) when is_list(pattern) do
    pattern
  end

  defp convert_pattern(pattern) do
    Lipex.eval_lipex_expr(pattern)
  end

  # Convert sequence tokenizer metadata to Elixir AST format
  defp normalize_meta({line, _column, _}) when is_integer(line) do
    [line: line]
  end

  defp normalize_meta(meta) when is_list(meta) do
    meta
  end

  defp normalize_meta(_) do
    []
  end
  
  # Helper function to validate Elixir variable names
  defp valid_elixir_variable_name?(name) do
    # Elixir variable names must:
    # - Start with lowercase letter or underscore
    # - Contain only letters, digits, and underscores  
    # - Can optionally end with ? or !
    case name do
      "_" -> true  # Single underscore is valid
      _ ->
        Regex.match?(~r/^[a-z_][a-zA-Z0-9_]*[?!]?$/, name)
    end
  end
end
