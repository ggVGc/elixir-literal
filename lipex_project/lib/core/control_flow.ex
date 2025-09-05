defmodule Lipex.Core.ControlFlow do
  @moduledoc """
  Handles control flow expressions in Lipex.
  
  Supports:
  - Conditionals: `(if ...)`, `(cond ...)`, `(case ...)`  
  - Pattern matching: `(match ...)`, `(with ...)`
  """
  
  @behaviour Lipex.Evaluator
  
  @doc """
  Tries to evaluate control flow expressions.
  
  Returns `{:ok, result}` for control flow patterns, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # if patterns with AST node operator
      {:sequence_prefix, {:if, _, nil}, args} ->
        {:ok, eval_if({:sequence_paren, [], [{:if, [], nil} | args]})}
        
      # if patterns
      {:sequence_paren, _meta, [{:if, _, nil} | args]} ->
        {:ok, eval_if({:sequence_paren, [], [{:if, [], nil} | args]})}
        
      {:sequence_prefix, _meta, [:if | args]} ->
        {:ok, eval_if({:sequence_prefix, [], [:if | args]})}
        
      # cond patterns  
      {:sequence_paren, _meta, [{:cond, _, nil} | _clauses]} ->
        {:ok, eval_cond(expr)}
        
      # case patterns
      {:sequence_paren, _meta, [{:case, _, nil} | _args]} ->
        {:ok, eval_case(expr)}
        
      # with patterns
      {:sequence_paren, _meta, [{:with, _, nil} | _args]} ->
        {:ok, eval_with(expr)}
        
      # match patterns
      {:sequence_paren, _meta, [{:match, _, nil} | _args]} ->
        {:ok, eval_match(expr)}
        
      _ ->
        :pass
    end
  end
  
  @doc """
  Evaluates if expressions.
  
  ## Examples
  
      (if true :yes :no)            -> :yes
      (if false :yes :no)           -> :no
      (if (> x 0) :positive :not-positive)
  """
  def eval_if({:sequence_paren, _meta, [{:if, _, nil}, condition, then_expr, else_expr]}) do
    elixir_condition = Lipex.eval_lipex_expr(condition)
    elixir_then = Lipex.eval_lipex_expr(then_expr)
    elixir_else = Lipex.eval_lipex_expr(else_expr)
    
    {:if, [],
     [elixir_condition,
      [do: elixir_then,
       else: elixir_else]]}
  end
  
  def eval_if({:sequence_paren, _meta, [{:if, _, nil}, condition, then_expr]}) do
    elixir_condition = Lipex.eval_lipex_expr(condition)
    elixir_then = Lipex.eval_lipex_expr(then_expr)
    
    {:if, [],
     [elixir_condition,
      [do: elixir_then]]}
  end
  
  def eval_if({:sequence_prefix, _meta, [:if, condition, then_expr, else_expr]}) do
    elixir_condition = Lipex.eval_lipex_expr(condition)
    elixir_then = Lipex.eval_lipex_expr(then_expr)
    elixir_else = Lipex.eval_lipex_expr(else_expr)
    
    {:if, [],
     [elixir_condition,
      [do: elixir_then,
       else: elixir_else]]}
  end
  
  # Handle if with only condition and then clause (else defaults to nil)
  def eval_if({:sequence_prefix, _meta, [:if, condition, then_expr]}) do
    elixir_condition = Lipex.eval_lipex_expr(condition)
    elixir_then = Lipex.eval_lipex_expr(then_expr)
    
    {:if, [],
     [elixir_condition,
      [do: elixir_then]]}
  end
  
  @doc """
  Evaluates cond expressions.
  
  ## Examples
  
      (cond 
        (< x 0) :negative
        (> x 0) :positive  
        true :zero)
  """
  def eval_cond({:sequence_paren, _meta, [{:cond, _, nil} | clauses]}) do
    # Convert clauses to cond format
    elixir_clauses = Enum.map(clauses, fn
      {:sequence_paren, _, [condition, result]} ->
        elixir_condition = Lipex.eval_lipex_expr(condition)
        elixir_result = Lipex.eval_lipex_expr(result)
        {:->, [], [[elixir_condition], elixir_result]}
      other ->
        raise "Invalid cond clause format: #{inspect(other)}"
    end)
    
    {:cond, [], [[do: elixir_clauses]]}
  end
  
  @doc """
  Evaluates case expressions.
  
  ## Examples
  
      (case value
        :ok :success
        {:error reason} reason
        _ :unknown)
  """
  def eval_case({:sequence_paren, _meta, [{:case, _, nil}, value | clauses]}) do
    elixir_value = Lipex.eval_lipex_expr(value)
    
    # Convert clauses to case format
    elixir_clauses = Enum.map(clauses, fn
      {:sequence_paren, _, [pattern, result]} ->
        elixir_pattern = case pattern do
          # Handle underscore as catch-all
          {:_, _, nil} -> {:_, [], nil}
          # Handle other patterns - evaluate them as patterns, not expressions
          other -> convert_pattern(other)
        end
        elixir_result = Lipex.eval_lipex_expr(result)
        {:->, [], [[elixir_pattern], elixir_result]}
      other ->
        raise "Invalid case clause format: #{inspect(other)}"
    end)
    
    {:case, [], [elixir_value, [do: elixir_clauses]]}
  end
  
  @doc """
  Evaluates with expressions for sequential pattern matching.
  
  ## Examples
  
      (with 
        (:ok x) (func1)
        (:ok y) (func2 x)
        (do (+ x y)))
  """
  def eval_with({:sequence_paren, _meta, [{:with, _, nil} | clauses_and_do]}) do
    # Split clauses and do block
    {clauses, do_block} = split_with_clauses(clauses_and_do)
    
    # Convert clauses to with format
    elixir_clauses = Enum.map(clauses, fn
      {:sequence_paren, _, [pattern, expr]} ->
        elixir_pattern = convert_pattern(pattern)
        elixir_expr = Lipex.eval_lipex_expr(expr)
        {:<-, [], [elixir_pattern, elixir_expr]}
      other ->
        raise "Invalid with clause format: #{inspect(other)}"
    end)
    
    elixir_do = case do_block do
      {:sequence_paren, _, [{:do, _, nil}, expr]} ->
        Lipex.eval_lipex_expr(expr)
      other ->
        Lipex.eval_lipex_expr(other)
    end
    
    {:with, [], elixir_clauses ++ [[do: elixir_do]]}
  end
  
  @doc """
  Evaluates match expressions for destructuring.
  
  ## Examples
  
      (match {a b} (tuple 1 2))    ; Binds a=1, b=2
      (match [h | t] [1 2 3])      ; Binds h=1, t=[2,3]
  """
  def eval_match({:sequence_paren, _meta, [{:match, _, nil}, pattern, value]}) do
    elixir_pattern = convert_pattern(pattern)
    elixir_value = Lipex.eval_lipex_expr(value)
    
    {:=, [], [elixir_pattern, elixir_value]}
  end
  
  # Private helper functions
  
  defp split_with_clauses(clauses_and_do) do
    # Find the do block (last clause that starts with 'do')
    {clauses, [do_block]} = Enum.split_while(clauses_and_do, fn
      {:sequence_paren, _, [{:do, _, nil} | _]} -> false
      _ -> true
    end)
    
    {clauses, do_block}
  end
  
  defp convert_pattern(expr) do
    case expr do
      # Atoms, numbers, strings - literal patterns
      atom when is_atom(atom) -> atom
      number when is_number(number) -> number
      string when is_binary(string) -> string
      
      # Variables
      {var, meta, nil} when is_atom(var) -> {var, meta, nil}
      
      # Tuples - convert sequence_paren to tuple pattern
      {:sequence_paren, _, elements} ->
        pattern_elements = Enum.map(elements, &convert_pattern/1)
        case length(pattern_elements) do
          2 -> {:{}, [], pattern_elements}
          n when n != 2 -> {:{}, [], pattern_elements}
        end
      
      # Lists - handle list patterns
      list when is_list(list) ->
        Enum.map(list, &convert_pattern/1)
      
      # Special patterns
      {:_, _meta, nil} -> {:_, [], nil}
      
      # Map patterns - TODO: implement
      # For now, pass through
      other -> other
    end
  end
end