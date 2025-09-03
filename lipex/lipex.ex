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
  
  # Import all evaluation modules
  alias Lipex.Core.DataStructures
  alias Lipex.Core.Arithmetic
  alias Lipex.Core.Logic
  alias Lipex.Core.ControlFlow
  alias Lipex.Functions.Definitions
  alias Lipex.Functions.Anonymous
  alias Lipex.Functions.Calls
  alias Lipex.Advanced.Pipes
  alias Lipex.Advanced.Comprehensions
  alias Lipex.Advanced.PatternMatching
  alias Lipex.Concurrency.Processes
  alias Lipex.ErrorHandling.TryRescue
  alias Lipex.Strings.Interpolation
  
  # Module evaluation order (specific to general)
  # Each module implements the Lipex.Evaluator behavior
  # NOTE: Only including migrated modules for now
  @evaluator_modules [
    # Core data structures - most specific patterns
    Lipex.Core.DataStructures,
    # Function definitions - need early resolution  
    Lipex.Functions.Definitions,
    # Mathematical operations
    Lipex.Core.Arithmetic,
    # Logical operations and type checks
    Lipex.Core.Logic
    
    # TODO: Add these modules as they get migrated:
    # Lipex.Functions.Anonymous,
    # Lipex.Core.ControlFlow,
    # Lipex.Advanced.Pipes,
    # Lipex.Advanced.Comprehensions,
    # Lipex.Concurrency.Processes,
    # Lipex.ErrorHandling.TryRescue,
    # Lipex.Strings.Interpolation,
    # Lipex.Functions.Calls
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
  # Handle direct sequence literals
  defmacro deflipex({:quote, _, nil}, [do: {:sequence_literal, _meta, [expr]}]) do
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
      number when is_number(number) -> number  
      string when is_binary(string) -> string
      
      # Handle sequence_paren wrapping a single sequence_prefix (unwrap it)
      {:sequence_paren, _meta, [{:sequence_prefix, prefix_meta, args}]} ->
        eval_lipex_expr({:sequence_prefix, prefix_meta, args})
      
      # Handle boolean literals that look like variables (must come BEFORE generic variable pattern)
      {true, _meta, nil} -> true
      {false, _meta, nil} -> false
      {nil, _meta, nil} -> nil
      
      # Variables - convert to Elixir variables
      {var, meta, nil} when is_atom(var) -> {var, meta, nil}
      
      # Special atoms with : prefix
      {:nil, _meta, nil} -> nil
      {:true, _meta, nil} -> true
      {:false, _meta, nil} -> false
      
      # Handle bare atoms
      true -> true
      false -> false
      nil -> nil
      atom when is_atom(atom) -> atom
      
      # Delegate to evaluation modules
      _ -> try_modules(expr)
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
  
  # BACKUP: Old evaluation function (will be removed after migration is complete)
  @doc false
  def eval_lipex_expr_old(expr) do
    case expr do
      # Handle sequence_paren wrapping a single sequence_prefix (unwrap it)
      {:sequence_paren, _meta, [{:sequence_prefix, prefix_meta, args}]} ->
        eval_lipex_expr({:sequence_prefix, prefix_meta, args})
      
      # Delegate to specialized modules based on expression patterns
      
      # Data Structure Construction
      {:sequence_prefix, _meta, [:% | _args]} ->
        DataStructures.eval_map(expr)
      
      {:sequence_paren, _meta, [{:tuple, _, nil} | _args]} ->
        DataStructures.eval_tuple(expr)
      
      {:sequence_paren, _meta, [{:struct, _, nil} | _args]} ->
        DataStructures.eval_struct(expr)
      
      {:sequence_paren, _meta, [{:kwlist, _, nil} | _args]} ->
        DataStructures.eval_kwlist(expr)
      
      # Arithmetic Operations (sequence_prefix format)
      {:sequence_prefix, _meta, [op | _args]} when op in [:+, :-, :*, :/, :<, :>, :<=, :>=, :==, :!=] ->
        Arithmetic.eval_arithmetic(expr)
      
      # Logical Operations (sequence_prefix format)  
      {:sequence_prefix, _meta, [op | _args]} when op in [:and, :or, :not, :truthy?, :falsy?, :nil?, :some?, :atom?, :number?, :integer?, :float?, :string?, :list?, :tuple?, :map?, :function?, :pid?] ->
        Logic.eval_logic(expr)
      
      # Control Flow
      {:sequence_prefix, _meta, [:if | _args]} ->
        ControlFlow.eval_if(expr)
      
      {:sequence_paren, _meta, [{:case, _, nil} | _args]} ->
        ControlFlow.eval_case(expr)
      
      {:sequence_paren, _meta, [{:cond, _, nil} | _args]} ->
        ControlFlow.eval_cond(expr)
      
      {:sequence_paren, _meta, [{:with, _, nil} | _args]} ->
        ControlFlow.eval_with(expr)
      
      # Function Definitions
      {:sequence_paren, _meta, [{:defmodule, _, nil} | _args]} ->
        Definitions.eval_defmodule(expr)
      
      # Handle def with sequence_prefix format (the actual AST structure)
      {:sequence_paren, _meta, [{:sequence_prefix, _, [:def | args]}]} ->
        Definitions.eval_def({:sequence_paren, [], [:def | args]})
      
      {:sequence_paren, _meta, [{:def, _, nil} | _args]} ->
        Definitions.eval_def(expr)
      
      # Handle defp with sequence_prefix format
      {:sequence_paren, _meta, [{:sequence_prefix, _, [:defp | args]}]} ->
        Definitions.eval_defp({:sequence_paren, [], [:defp | args]})
      
      {:sequence_paren, _meta, [{:defp, _, nil} | _args]} ->
        Definitions.eval_defp(expr)
      
      # Handle defmacro with sequence_prefix format
      {:sequence_paren, _meta, [{:sequence_prefix, _, [:defmacro | args]}]} ->
        Definitions.eval_defmacro({:sequence_paren, [], [:defmacro | args]})
      
      {:sequence_paren, _meta, [{:defmacro, _, nil} | _args]} ->
        Definitions.eval_defmacro(expr)
      
      # Anonymous Functions
      {:sequence_paren, _meta, [{:fn, _, nil} | _args]} ->
        Anonymous.eval_fn(expr)
      
      {:sequence_paren, _meta, [{:&, _, nil} | _args]} ->
        Anonymous.eval_capture(expr)
      
      # Pipes
      {:sequence_paren, _meta, [{:|>, _, nil} | _args]} ->
        Pipes.eval_pipe(expr)
      
      # Comprehensions
      {:sequence_paren, _meta, [{:for, _, nil} | _args]} ->
        Comprehensions.eval_for(expr)
      
      # Process Operations
      {:sequence_paren, _meta, [{:spawn, _, nil} | _args]} ->
        Processes.eval_spawn(expr)
      
      {:sequence_paren, _meta, [{:send, _, nil} | _args]} ->
        Processes.eval_send(expr)
      
      {:sequence_paren, _meta, [{:receive, _, nil} | _args]} ->
        Processes.eval_receive(expr)
      
      # Error Handling
      {:sequence_paren, _meta, [{:try, _, nil} | _args]} ->
        TryRescue.eval_try(expr)
      
      {:sequence_paren, _meta, [{:throw, _, nil} | _args]} ->
        TryRescue.eval_throw(expr)
      
      {:sequence_paren, _meta, [{:catch, _, nil} | _args]} ->
        TryRescue.eval_catch(expr)
      
      # String Operations
      {:sequence_paren, _meta, [{:str, _, nil} | _args]} ->
        Interpolation.eval_str(expr)
      
      {:sequence_paren, _meta, [{:istr, _, nil} | _args]} ->
        Interpolation.eval_istr(expr)
      
      # List Operations (basic ones - delegate to data structures)
      {:sequence_paren, _meta, [{:list, _, nil} | _args]} ->
        DataStructures.eval_list(expr)
        
      # Type checking functions (sequence_paren format)
      {:sequence_paren, _meta, [{op, _, nil} | args]} when op in [:atom?, :number?, :integer?, :float?, :string?, :list?, :tuple?, :map?, :function?, :pid?, :truthy?, :falsy?, :nil?, :some?] ->
        # Convert to sequence_prefix format for Logic module
        Logic.eval_logic({:sequence_prefix, [], [op | args]})
      
      # Handle def/defp/defmacro in sequence_prefix format directly  
      {:sequence_prefix, _meta, [:def | args]} ->
        Definitions.eval_def({:sequence_paren, [], [:def | args]})
        
      {:sequence_prefix, _meta, [:defp | args]} ->
        Definitions.eval_defp({:sequence_paren, [], [:defp | args]})
        
      {:sequence_prefix, _meta, [:defmacro | args]} ->
        Definitions.eval_defmacro({:sequence_paren, [], [:defmacro | args]})
      
      # Function Calls - delegate to calls module (exclude def/defp/defmacro)
      {:sequence_prefix, _meta, [function | _args]} when is_atom(function) and function not in [:def, :defp, :defmacro] ->
        Calls.eval_function_call(expr)
      
      {:sequence_paren, _meta, [function | _args]} ->
        Calls.eval_function_call(expr)
      
      # Atoms, numbers, strings, variables - pass through or handle basic cases
      atom when is_atom(atom) -> atom
      number when is_number(number) -> number
      string when is_binary(string) -> string
      
      # Special atoms
      {:nil, _meta, nil} -> nil
      {:true, _meta, nil} -> true
      {:false, _meta, nil} -> false
      
      # Variables - convert to Elixir variables
      {var, meta, nil} when is_atom(var) -> {var, meta, nil}
      
      # Fallback for expressions not handled by any module
      other -> 
        quote do
          raise "Unsupported Lipex expression: #{inspect(unquote(other))}"
        end
    end
  end
  
  @doc """
  Converts a Lipex variable/function name to an Elixir variable.
  Handles hyphenated names by converting them to underscored names.
  """
  def lipex_to_elixir_var({name, meta, nil}) when is_atom(name) do
    elixir_name = name
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