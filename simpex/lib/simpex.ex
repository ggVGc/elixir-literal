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
      # Handle literals directly
      number when is_number(number) -> number
      string when is_binary(string) -> string
      true -> true
      false -> false
      nil -> nil
      atom when is_atom(atom) -> atom

      # Handle sequence tokens from tokenizer
      {:sequence_number, _meta, value} -> value
      {:sequence_atom, _meta, value} -> value
      {:sequence_string, _meta, value} -> 
        if is_list(value), do: List.to_string(value), else: value
      {:sequence_chars, _meta, value} -> value
      {:sequence_token, meta, value} ->
        case value do
          nil -> nil
          true -> true
          false -> false
          _ -> {value, meta, nil}
        end

      # Handle boolean literals first (before generic variables)
      {true, _meta, nil} -> true
      {false, _meta, nil} -> false
      {nil, _meta, nil} -> nil

      # Handle variables  
      {var, meta, nil} when is_atom(var) and var not in [nil, true, false] ->
        {var, meta, nil}

      # Handle sequence_prefix - the main expression form
      {:sequence_prefix, op_node, args} ->
        eval_function_expr(op_node, args)

      # Handle sequence_paren
      {:sequence_paren, _meta, [inner_expr]} ->
        eval_simpex_expr(inner_expr)
      
      # Handle sequence_paren with function calls like (func arg1 arg2)
      {:sequence_paren, _meta, [func_node | args]} when args != [] ->
        func_name = extract_atom(func_node)
        elixir_args = Enum.map(args, &eval_simpex_expr/1)
        
        quote do
          unquote(func_name)(unquote_splicing(elixir_args))
        end

      # Handle sequence_block for parameter lists
      {:sequence_block, _meta, :"()", contents} ->
        # This is likely a parameter list or grouped expression
        case contents do
          [single_expr] -> eval_simpex_expr(single_expr)
          _ -> contents  # Return as-is for parameter parsing
        end

      _ ->
        raise "Unsupported Simpex expression: #{inspect(expr)}"
    end
  end

  defp eval_function_expr(op_node, args) do
    case op_node do
      # Function definition: (def name (params) body) - def parsed as bare atom
      {:def, _meta, nil} ->
        case args do
          [name_node, params_node, body_node] ->
            name = extract_atom(name_node)
            params = extract_params(params_node)
            body = eval_simpex_expr(body_node)
            
            quote do
              def unquote(name)(unquote_splicing(params)) do
                unquote(body)
              end
            end

          _ ->
            raise "Invalid def syntax. Expected: (def name (params) body). Got: #{inspect(args)}"
        end

      # Function call: (function-name arg1 arg2 ...)
      func_name_node ->
        func_name = extract_atom(func_name_node)
        elixir_args = Enum.map(args, &eval_simpex_expr/1)
        
        quote do
          unquote(func_name)(unquote_splicing(elixir_args))
        end
    end
  end

  defp extract_atom({:sequence_token, _meta, atom}) when is_atom(atom), do: atom
  defp extract_atom({:sequence_atom, _meta, atom}) when is_atom(atom), do: atom
  defp extract_atom({atom, _meta, nil}) when is_atom(atom), do: atom
  defp extract_atom(atom) when is_atom(atom), do: atom
  defp extract_atom(expr), do: raise("Expected atom, got: #{inspect(expr)}")

  defp extract_params({:sequence_paren, _meta, [inner]}) do
    extract_params(inner)
  end
  defp extract_params({:sequence_block, _meta, :"()", params}) do
    Enum.map(params, &extract_param/1)
  end
  defp extract_params({:sequence_prefix, _op, params}) do
    Enum.map(params, &extract_param/1)
  end
  defp extract_params([]), do: []
  defp extract_params(params) when is_list(params) do
    Enum.map(params, &extract_param/1)
  end
  defp extract_params(_), do: []

  defp extract_param({:sequence_token, meta, name}) when is_atom(name) do
    {name, normalize_meta(meta), nil}
  end
  defp extract_param({:sequence_atom, meta, name}) when is_atom(name) do
    {name, normalize_meta(meta), nil}
  end
  defp extract_param({name, meta, nil}) when is_atom(name) do
    {name, normalize_meta(meta), nil}
  end
  defp extract_param(name) when is_atom(name) do
    {name, [], nil}
  end
  defp extract_param(expr) do
    raise "Expected parameter name, got: #{inspect(expr)}"
  end

  # Convert tuple metadata format to keyword list format
  defp normalize_meta({line, column, _}) when is_integer(line) and is_integer(column) do
    [line: line, column: column]
  end
  defp normalize_meta(meta) when is_list(meta), do: meta
  defp normalize_meta(_), do: []
end