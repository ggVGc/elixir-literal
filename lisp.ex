defmodule Lisp do
  @moduledoc """
  A minimal Lisp-like language implementation using Elixir's sequence literals.
  
  This module provides a `deflisp` macro that allows you to write Lisp-style code
  using the `~~(...)` sequence literal syntax and have it compiled into regular
  Elixir functions or evaluated at runtime.
  
  ## Examples
  
      Code.require_file("lisp.ex")
      
      defmodule Calculator do
        import Lisp
        
        # Define a function in Lisp that becomes an Elixir function
        deflisp ~~((defun add-two (x) (+ x 2)))
        
        # Use it from regular Elixir code
        def test do
          add_two(5) # Returns 7
        end
        
        # Evaluate Lisp expressions directly
        result = deflisp ~~((+ 1 2 (* 3 4)))  # Returns 15
      end
  
  ## Supported Features
  
  - Arithmetic: `(+ 1 2 3)`, `(- 10 5)`, `(* 2 3)`, `(/ 10 2)`
  - Comparison: `(= a b)`, `(< a b)`, `(> a b)`, `(<= a b)`, `(>= a b)`
  - Logic: `(and true false)`, `(or true false)`, `(not true)`
  - Conditionals: `(if condition then-expr else-expr)`
  - Let bindings: `(let ((x 5) (y 10)) (+ x y))`
  - Function definitions: `(defun name (args...) body)`
  - Lists: `(list 1 2 3)`, `(first list)`, `(rest list)`, `(cons item list)`
  """
  
  @doc """
  Main macro for defining and evaluating Lisp expressions.
  
  Can be used to:
  1. Define functions that become regular Elixir functions
  2. Evaluate expressions and return their result
  
  ## Examples
  
      # Define a function
      deflisp ~~((defun factorial (n) 
                  (if (= n 0) 1 (* n (factorial (- n 1))))))
      
      # Evaluate an expression  
      result = deflisp ~~((+ 1 2 3))
  """
  # Handle direct sequence literals
  defmacro deflisp({:sequence_literal, _meta, [expr]}) do
    eval_lisp_expr(expr)
  end
  
  # Handle quoted sequence literals (for backwards compatibility)
  defmacro deflisp({:quote, _, nil}, [do: {:sequence_literal, _meta, [expr]}]) do
    eval_lisp_expr(expr)
  end
  
  @doc """
  Evaluates a single Lisp expression and returns the appropriate Elixir AST.
  """
  def eval_lisp_expr(expr) do
    case expr do
      # Handle sequence_paren wrapping a single sequence_prefix (unwrap it)
      {:sequence_paren, _meta, [{:sequence_prefix, prefix_meta, args}]} ->
        eval_lisp_expr({:sequence_prefix, prefix_meta, args})
        
      # Function definition - creates an Elixir function
      {:sequence_paren, _meta, [{:defun, _, nil}, name, {:sequence_paren, _, args}, body]} ->
        elixir_name = case name do
          {atom, _, nil} -> atom
          atom when is_atom(atom) -> atom
        end
        elixir_args = Enum.map(args, fn
          {var, _, nil} -> {var, [], nil}
          var when is_atom(var) -> {var, [], nil}
        end)
        elixir_body = eval_lisp_expr(body)
        
        quote do
          def unquote(elixir_name)(unquote_splicing(elixir_args)) do
            unquote(elixir_body)
          end
        end
      
      # Arithmetic operations
      {:sequence_prefix, _meta, [:+ | args]} ->
        elixir_args = Enum.map(args, &eval_lisp_expr/1)
        quote do: Enum.reduce(unquote(elixir_args), 0, &+/2)
        
      {:sequence_prefix, _meta, [:-, arg]} ->
        elixir_arg = eval_lisp_expr(arg)
        quote do: -unquote(elixir_arg)
        
      {:sequence_prefix, _meta, [:-, first, second]} ->
        # Binary subtraction: (- a b) -> a - b
        elixir_first = eval_lisp_expr(first)
        elixir_second = eval_lisp_expr(second)
        quote do: unquote(elixir_first) - unquote(elixir_second)
        
      {:sequence_prefix, _meta, [:-, first | rest]} ->
        # Multi-argument subtraction: (- a b c d) -> a - b - c - d
        elixir_first = eval_lisp_expr(first)
        elixir_rest = Enum.map(rest, &eval_lisp_expr/1)
        quote do: Enum.reduce(unquote(elixir_rest), unquote(elixir_first), fn b, a -> a - b end)
        
      {:sequence_prefix, _meta, [:*, first, second]} ->
        # Binary multiplication: (* a b) -> a * b
        elixir_first = eval_lisp_expr(first)
        elixir_second = eval_lisp_expr(second)
        quote do: unquote(elixir_first) * unquote(elixir_second)
        
      {:sequence_prefix, _meta, [:* | args]} ->
        # Multi-argument multiplication: (* a b c d) -> a * b * c * d
        elixir_args = Enum.map(args, &eval_lisp_expr/1)
        quote do: Enum.reduce(unquote(elixir_args), 1, &*/2)
        
      {:sequence_prefix, _meta, [:/, dividend, divisor]} ->
        elixir_dividend = eval_lisp_expr(dividend)
        elixir_divisor = eval_lisp_expr(divisor)
        quote do: unquote(elixir_dividend) / unquote(elixir_divisor)
      
      # Comparison operations
      {:sequence_prefix, _meta, [:=, left, right]} ->
        elixir_left = eval_lisp_expr(left)
        elixir_right = eval_lisp_expr(right)
        quote do
          # Handle Lisp-style list comparison where nil and empty list are equivalent
          case {unquote(elixir_left), unquote(elixir_right)} do
            {[], nil} -> true
            {nil, []} -> true
            {a, b} -> a == b
          end
        end
        
      {:sequence_prefix, _meta, [:<, left, right]} ->
        elixir_left = eval_lisp_expr(left)
        elixir_right = eval_lisp_expr(right)
        quote do: unquote(elixir_left) < unquote(elixir_right)
        
      {:sequence_prefix, _meta, [:>, left, right]} ->
        elixir_left = eval_lisp_expr(left)
        elixir_right = eval_lisp_expr(right)
        quote do: unquote(elixir_left) > unquote(elixir_right)
        
      {:sequence_prefix, _meta, [:<=, left, right]} ->
        elixir_left = eval_lisp_expr(left)
        elixir_right = eval_lisp_expr(right)
        quote do: unquote(elixir_left) <= unquote(elixir_right)
        
      {:sequence_prefix, _meta, [:>=, left, right]} ->
        elixir_left = eval_lisp_expr(left)
        elixir_right = eval_lisp_expr(right)
        quote do: unquote(elixir_left) >= unquote(elixir_right)
      
      # Logical operations
      {:sequence_prefix, _meta, [:and | args]} ->
        elixir_args = Enum.map(args, &eval_lisp_expr/1)
        Enum.reduce(elixir_args, quote(do: true), fn arg, acc ->
          quote do: unquote(acc) and unquote(arg)
        end)
        
      {:sequence_prefix, _meta, [:or | args]} ->
        elixir_args = Enum.map(args, &eval_lisp_expr/1)
        Enum.reduce(elixir_args, quote(do: false), fn arg, acc ->
          quote do: unquote(acc) or unquote(arg)
        end)
        
      {:sequence_prefix, _meta, [:not, arg]} ->
        elixir_arg = eval_lisp_expr(arg)
        quote do: not unquote(elixir_arg)
      
      # Conditional expressions
      {:sequence_prefix, _meta, [:if, condition, then_expr, else_expr]} ->
        elixir_condition = eval_lisp_expr(condition)
        elixir_then = eval_lisp_expr(then_expr)
        elixir_else = eval_lisp_expr(else_expr)
        
        quote do
          if unquote(elixir_condition) do
            unquote(elixir_then)
          else
            unquote(elixir_else)
          end
        end
      
      # Let bindings
      {:sequence_paren, _meta, [{:let, _, nil}, {:sequence_paren, _, bindings}, body]} ->
        elixir_bindings = Enum.map(bindings, fn
          {:sequence_paren, _, [var, value]} ->
            {lisp_to_elixir_var(var), eval_lisp_expr(value)}
        end)
        elixir_body = eval_lisp_expr(body)
        
        # Create simple variable assignments using let-style with case
        assignments = Enum.map(elixir_bindings, fn {var, value} ->
          {var, value}
        end)
        
        quote do
          (fn ->
            unquote_splicing(for {var, value} <- assignments do
              quote do: unquote(var) = unquote(value)
            end)
            unquote(elixir_body)
          end).()
        end
      
      
      # Function calls (sequence_prefix format) - use module-scoped calls for recursive support  
      {:sequence_prefix, _meta, [function | args]} ->
        elixir_function_name = function
        elixir_args = Enum.map(args, &eval_lisp_expr/1)
        quote do: __MODULE__.unquote(elixir_function_name)(unquote_splicing(elixir_args))
      
      # List operations (still use sequence_paren format)
      {:sequence_paren, _meta, [{:list, _, nil} | args]} ->
        elixir_args = Enum.map(args, &eval_lisp_expr/1)
        quote do: unquote(elixir_args)
        
      {:sequence_paren, _meta, [{:first, _, nil}, list]} ->
        elixir_list = eval_lisp_expr(list)
        quote do: List.first(unquote(elixir_list))
        
      {:sequence_paren, _meta, [{:rest, _, nil}, list]} ->
        elixir_list = eval_lisp_expr(list)
        quote do
          case unquote(elixir_list) do
            [_ | tail] -> tail
            [] -> []
          end
        end
        
      {:sequence_paren, _meta, [{:cons, _, nil}, item, list]} ->
        elixir_item = eval_lisp_expr(item)
        elixir_list = eval_lisp_expr(list)
        quote do: [unquote(elixir_item) | unquote(elixir_list)]
      
      # Function calls (sequence_paren format) - use module-scoped calls for recursive support
      {:sequence_paren, _meta, [function | args]} ->
        elixir_function_name = case function do
          {name, _, nil} -> name
          name when is_atom(name) -> name
        end
        elixir_args = Enum.map(args, &eval_lisp_expr/1)
        quote do: __MODULE__.unquote(elixir_function_name)(unquote_splicing(elixir_args))
      
      # Atoms, numbers, strings - pass through
      atom when is_atom(atom) -> atom
      number when is_number(number) -> number
      string when is_binary(string) -> string
      
      # Special atoms that should be values, not variables
      {:nil, _meta, nil} -> nil
      {:true, _meta, nil} -> true
      {:false, _meta, nil} -> false
      
      # Variables - convert to Elixir variables
      {var, meta, nil} when is_atom(var) -> {var, meta, nil}
      
      # Fallback for other expressions
      other -> other
    end
  end
  
  @doc """
  Converts a Lisp variable/function name to an Elixir variable.
  Handles hyphenated names by converting them to underscored names.
  """
  def lisp_to_elixir_var({name, meta, nil}) when is_atom(name) do
    elixir_name = name
      |> Atom.to_string()
      |> String.replace("-", "_")
      |> String.to_atom()
    
    {elixir_name, meta, nil}
  end
  
  def lisp_to_elixir_var(name) when is_atom(name) do
    name
      |> Atom.to_string()
      |> String.replace("-", "_")
      |> String.to_atom()
  end
  
  def lisp_to_elixir_var(other), do: other
end