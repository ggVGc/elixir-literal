defmodule Lipex do
  @moduledoc """
  Lipex - An Elixir-like Lisp syntax implementation using sequence literals.

  Lipex provides a comprehensive Lisp-like syntax that closely resembles standard
  Elixir functionality, using the `~~(...)` sequence literal syntax. This design
  prioritizes familiarity for Elixir developers while maintaining s-expression clarity.

  ## Examples

      Code.require_file("lipex/lipex.ex")

      defmodule Calculator do
        import Lipex

        # Define a module in Lipex syntax
        deflipex ~~((defmodule MyMath
          (def add (x y) (+ x y))
          (def multiply (x y) (* x y))
          (defp validate (x) (> x 0))))

        # Create maps using % syntax
        user_map = deflipex ~~((% :name "John" :age 30))

        # Use pipes for data transformation
        result = deflipex ~~((|> [1 2 3 4]
          (map (fn (x) (* x 2)))
          (filter (fn (x) (> x 4)))
          (reduce +)))
      end

  ## Supported Features

  - **Data Structures**: Maps `(% ...)`, Tuples `(tuple ...)`, Structs `(struct ...)`
  - **Functions**: `(defmodule ...)`, `(def ...)`, `(defp ...)`, `(defmacro ...)`
  - **Pattern Matching**: Function clauses, case expressions, guards with `:when`
  - **Control Flow**: `(if ...)`, `(case ...)`, `(cond ...)`, `(with ...)`
  - **Functional**: Pipes `(|> ...)`, Anonymous functions `(fn ...)`, Captures `(& ...)`
  - **Comprehensions**: `(for ...)` with filters and generators
  - **Concurrency**: `(spawn ...)`, `(send ...)`, `(receive ...)`
  - **Error Handling**: `(try ...)`, `(rescue ...)`, `(catch ...)`, `(throw ...)`
  """

  # Evaluation modules are referenced directly in @evaluator_modules list below

  # Module evaluation order (specific to general)
  # Each module implements the Lipex.Evaluator behavior
  # NOTE: Only including migrated modules for now
  @evaluator_modules [
    # String interpolation - handle early since strings are literals
    # Lipex.Strings.Interpolation,
    # Core data structures - most specific patterns
    Lipex.Core.DataStructures,
    # Function definitions - need early resolution
    Lipex.Functions.Definitions,
    # Pattern matching - handle before arithmetic since = is pattern matching, not equality
    Lipex.Core.PatternMatching,
    # Mathematical operations
    Lipex.Core.Arithmetic,
    # Logical operations and type checks
    Lipex.Core.Logic,
    # Function calls - handle as fallback for arbitrary function calls
    Lipex.Functions.Calls

    # TODO: Add these modules as they get migrated:
    # Lipex.Functions.Anonymous,
    # Lipex.Core.ControlFlow,
    # Lipex.Advanced.Pipes,
    # Lipex.Advanced.Comprehensions,
    # Lipex.Concurrency.Processes,
    # Lipex.ErrorHandling.TryRescue
  ]

  @doc """
  Main macro for defining and evaluating Lipex expressions.

  Similar to `deflisp` but with enhanced Elixir-like syntax support.

  ## Examples

      # Define a function with pattern matching
      deflipex ~~((def factorial (0) 1))
      deflipex ~~((def factorial (n) :when (> n 0) (* n (factorial (- n 1)))))

      # Evaluate expressions with maps and pipes
      result = deflipex ~~((|> (% :x 10 :y 20)
                                     (Map.values)
                                     (Enum.sum)))
  """
  # Handle sequence literals with multiple expressions
  defmacro deflipex({:sequence_literal, _meta, exprs}) when length(exprs) > 1 do
    elixir_exprs = Enum.map(exprs, &eval_lipex_expr/1)
    {:__block__, [], elixir_exprs}
  end

  # Handle direct sequence literals
  defmacro deflipex({:quote, _, nil}, do: {:sequence_literal, _meta, [expr]}) do
    eval_lipex_expr(expr)
  end

  # Handle quoted sequence literals (for backwards compatibility)
  defmacro deflipex({:sequence_literal, _meta, [expr]}) do
    eval_lipex_expr(expr)
  end

  @doc """
  Evaluates a single Lipex expression and returns the appropriate Elixir AST.

  This function uses a modular approach where each evaluation module implements
  the Lipex.Evaluator behavior. We try each module in order until one succeeds.
  """
  def eval_lipex_expr(expr) do
    case expr do
      # Handle literals directly (no module needed)
      number when is_number(number) ->
        number

      string when is_binary(string) ->
        string

      # Handle bracket list syntax [a b c] as sugar for (list a b c)
      {:sequence_bracket, meta, items} ->
        eval_lipex_expr({:sequence_paren, meta, [{:list, meta, nil} | items]})

      # Handle sequence_paren wrapping a single sequence_prefix (unwrap it)
      {:sequence_paren, _meta, [{:sequence_prefix, prefix_meta, args}]} ->
        eval_lipex_expr({:sequence_prefix, prefix_meta, args})

      # Handle boolean literals that look like variables (must come BEFORE generic variable pattern)
      {true, _meta, nil} ->
        true

      {false, _meta, nil} ->
        false

      {nil, _meta, nil} ->
        nil

      # Variables - convert to Elixir variables
      {var, meta, nil} when is_atom(var) ->
        {var, meta, nil}

      # Special atoms with : prefix
      {nil, _meta, nil} ->
        nil

      {true, _meta, nil} ->
        true

      {false, _meta, nil} ->
        false

      # Handle bare atoms
      true ->
        true

      false ->
        false

      nil ->
        nil

      atom when is_atom(atom) ->
        atom

      # Delegate to evaluation modules
      _ ->
        try_modules(expr)
    end
  end

  # Tries each evaluation module in order until one handles the expression.
  defp try_modules(expr) do
    Enum.reduce_while(@evaluator_modules, nil, fn module, _acc ->
      case module.try_eval(expr) do
        {:ok, result} -> {:halt, result}
        :pass -> {:cont, nil}
        {:error, reason} -> {:halt, raise_evaluation_error(reason, expr, module)}
      end
    end) || raise_unsupported_expression(expr)
  end

  defp raise_evaluation_error(reason, expr, module) do
    raise "Evaluation error in #{inspect(module)}: #{reason}. Expression: #{inspect(expr)}"
  end

  defp raise_unsupported_expression(expr) do
    raise "Unsupported Lipex expression: #{inspect(expr)}"
  end

  @doc """
  Converts a Lipex variable/function name to an Elixir variable.
  Handles hyphenated names by converting them to underscored names.
  """
  def lipex_to_elixir_var({name, meta, nil}) when is_atom(name) do
    elixir_name =
      name
      |> Atom.to_string()
      |> String.replace("-", "_")
      |> String.to_atom()

    {elixir_name, meta, nil}
  end

  def lipex_to_elixir_var(name) when is_atom(name) do
    name
    |> Atom.to_string()
    |> String.replace("-", "_")
    |> String.to_atom()
  end

  def lipex_to_elixir_var(other), do: other
end
