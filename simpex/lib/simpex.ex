defmodule Simpex do
  @moduledoc """
  Simpex - A minimal Lisp implementation using Elixir's sequence literal syntax.
  
  Supports only basic functionality:
  - Function definition with `def`
  - Function calls
  - Basic data types: numbers, strings, booleans, atoms
  
  ## Examples
  
      # Define a function
      defsimpex ~~((def greet (name) name))
      
      # Call a function  
      defsimpex ~~((greet "world"))
  """

  @doc """
  Main macro for defining and evaluating Simpex expressions.
  """
  defmacro defsimpex({:sequence_literal, _meta, [expr]}) do
    eval_simpex_expr(expr)
  end

  @doc """
  Evaluates a single Simpex expression and returns the appropriate Elixir AST.
  """
  def eval_simpex_expr(expr) do
    case expr do
      # Handle literals directly - let Elixir handle basic types
      literal when is_number(literal) or is_binary(literal) or literal in [true, false, nil] ->
        literal

      atom when is_atom(atom) -> atom

      # Handle boolean and nil literals in AST form  
      {true, _meta, nil} -> true
      {false, _meta, nil} -> false
      {nil, _meta, nil} -> nil
      
      # Handle Elixir AST variables (excluding special literals)
      {var, meta, nil} when is_atom(var) and var not in [true, false, nil] ->
        {var, meta, nil}

      # Handle structured expressions - sequence_prefix for def, sequence_paren for calls
      {:sequence_prefix, op_node, args} ->
        eval_function_expr(op_node, args)

      # Handle function calls: (func arg1 arg2...)
      {:sequence_paren, _meta, [func_node | args]} ->
        case func_node do
          # Handle nested sequence_prefix in paren (like function definitions)
          {:sequence_prefix, op_node, nested_args} ->
            eval_function_expr(op_node, nested_args)
          # Handle direct function calls
          _ ->
            func_name = extract_atom(func_node)
            elixir_args = Enum.map(args, &eval_simpex_expr/1)
            
            quote do
              unquote(func_name)(unquote_splicing(elixir_args))
            end
        end

      # Handle parameter blocks - return as-is for parameter parsing
      {:sequence_block, _meta, :"()", contents} -> contents

      _ ->
        raise "Unsupported Simpex expression: #{inspect(expr)}"
    end
  end

  # Simplified function expression handling
  defp eval_function_expr({:def, _meta, nil}, [name_node, params_node, body_node]) do
    name = extract_atom(name_node)
    params = extract_params(params_node)
    body = eval_simpex_expr(body_node)
    
    quote do
      def unquote(name)(unquote_splicing(params)) do
        unquote(body)
      end
    end
  end

  defp eval_function_expr(func_name_node, args) do
    func_name = extract_atom(func_name_node)
    elixir_args = Enum.map(args, &eval_simpex_expr/1)
    
    quote do
      unquote(func_name)(unquote_splicing(elixir_args))
    end
  end

  # Simplified atom extraction - let Elixir handle most cases
  defp extract_atom({atom, _meta, nil}) when is_atom(atom), do: atom
  defp extract_atom(atom) when is_atom(atom), do: atom
  defp extract_atom(expr), do: raise("Expected atom, got: #{inspect(expr)}")

  # Parameter extraction with minimal sequence token handling
  defp extract_params({:sequence_block, _meta, :"()", params}) do
    Enum.map(params, &to_elixir_param/1)
  end
  defp extract_params([]), do: []
  defp extract_params(params) when is_list(params) do
    Enum.map(params, &to_elixir_param/1)
  end
  defp extract_params(_), do: []

  # Convert sequence tokens to proper Elixir AST for parameters
  defp to_elixir_param({:sequence_token, {line, column, _}, name}) when is_atom(name) do
    {name, [line: line, column: column], nil}
  end
  defp to_elixir_param({name, meta, nil}) when is_atom(name) do
    {name, meta, nil}
  end
  defp to_elixir_param(name) when is_atom(name) do
    {name, [], nil}
  end
end