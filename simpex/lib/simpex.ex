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
  # Handle empty sequence literal
  defmacro defsimpex({:sequence_literal, _meta, []}) do
    nil
  end

  # Handle single expression
  defmacro defsimpex({:sequence_literal, _meta, [expr]}) do
    eval_simpex_expr(expr)
  end

  # Handle multiple expressions
  defmacro defsimpex({:sequence_literal, _meta, exprs}) when is_list(exprs) do
    elixir_exprs = Enum.map(exprs, &eval_simpex_expr/1)
    {:__block__, [], elixir_exprs}
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

      # Handle sequence numbers  
      {:sequence_number, _meta, value} -> value

      # Handle sequence atoms (like :x, :key, etc.)
      {:sequence_atom, _meta, atom} -> atom

      # Handle sequence string tokens (double quotes)
      {:sequence_string, _meta, value} ->
        # Convert character list to binary string if needed
        if is_list(value) do
          List.to_string(value)
        else
          value
        end

      # Handle sequence character list tokens (single quotes)
      {:sequence_chars, _meta, value} ->
        # Keep as character list
        value

      # Handle boolean and nil literals in AST form  
      {true, _meta, nil} -> true
      {false, _meta, nil} -> false
      {nil, _meta, nil} -> nil
      
      # Handle sequence tokens that represent variables
      {:sequence_token, meta, name} when is_atom(name) ->
        # Convert to proper Elixir AST variable
        case name do
          n when n in [true, false, nil] -> n
          _ -> 
            # Normalize metadata format
            normalized_meta = case meta do
              {line, column, _} when is_integer(line) and is_integer(column) ->
                [line: line, column: column]
              m when is_list(m) -> m
              _ -> []
            end
            {name, normalized_meta, nil}
        end

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

      # Handle parenthesized expressions that look like function calls
      {:sequence_block, _meta, :"()", [{:sequence_token, _, func_atom} | args]} when func_atom == :% ->
        # Handle map syntax
        pairs = build_map_pairs(args)
        {:%{}, [], pairs}

      {:sequence_block, _meta, :"()", [{:sequence_token, _, _} = func | args]} ->
        func_name = extract_atom(func)
        elixir_args = Enum.map(args, &eval_simpex_expr/1)
        quote do
          unquote(func_name)(unquote_splicing(elixir_args))
        end

      # Handle parameter blocks and other parenthesized content
      {:sequence_block, _meta, :"()", contents} -> contents
      
      # Handle tuple syntax {a b c} -> {a, b, c}
      {:sequence_block, _meta, :{}, items} ->
        elixir_items = Enum.map(items, &eval_simpex_expr/1)
        {:{}, [], elixir_items}

      # Handle list syntax [a b c] -> [a, b, c] or keyword list [key: value] -> [{:key, value}]
      {:sequence_block, _meta, :"[]", items} ->
        case detect_keyword_syntax(items) do
          {:keywords, _} ->
            # Convert to keyword list using general expression evaluation
            build_keyword_pairs_general(items)
          :no_keywords ->
            # Regular list handling
            Enum.map(items, &eval_simpex_expr/1)
        end

      # Also handle sequence_brace if it appears
      {:sequence_brace, _meta, items} ->
        elixir_items = Enum.map(items, &eval_simpex_expr/1)
        {:{}, [], elixir_items}

      _ ->
        raise "Unsupported Simpex expression: #{inspect(expr)}"
    end
  end

  # Simplified function expression handling
  defp eval_function_expr({:def, _meta, nil}, [name_node, params_node, body_node]) do
    name = extract_atom(name_node)
    
    case params_node do
      # Keyword-only function: (def name [keywords] body)  
      {:sequence_block, _meta, :"[]", _items} ->
        keyword_params = extract_keyword_list_params(params_node)
        body = eval_simpex_expr(body_node)
        
        quote do
          def unquote(name)(unquote(keyword_params)) do
            unquote(body)
          end
        end
        
      # Normal function: (def name (params) body)
      _ ->
        params = extract_params(params_node)
        body = eval_simpex_expr(body_node)
        
        quote do
          def unquote(name)(unquote_splicing(params)) do
            unquote(body)
          end
        end
    end
  end

  # Handle function definition with guard clause (def name (params) when guard body)
  defp eval_function_expr({:def, _meta, nil}, [name_node, params_node, {:when, _, nil}, guard_node, body_node]) do
    name = extract_atom(name_node)
    params = extract_params(params_node)
    guard = eval_simpex_expr(guard_node)
    body = eval_simpex_expr(body_node)
    
    # Create the when clause AST: def name(params) when guard do body end
    when_clause = {:when, [], [{name, [], params}, guard]}
    
    quote do
      def unquote(when_clause) do
        unquote(body)
      end
    end
  end

  # Handle function definition with guard clause using sequence tokens
  defp eval_function_expr({:def, _meta, nil}, [name_node, params_node, {:sequence_token, _, :when}, guard_node, body_node]) do
    name = extract_atom(name_node)
    params = extract_params(params_node)
    guard = eval_simpex_expr(guard_node)
    body = eval_simpex_expr(body_node)
    
    # Create the when clause AST: def name(params) when guard do body end
    when_clause = {:when, [], [{name, [], params}, guard]}
    
    quote do
      def unquote(when_clause) do
        unquote(body)
      end
    end
  end

  # Handle function definition with both normal params and keyword list: (def name (params) [keywords] body)
  defp eval_function_expr({:def, _meta, nil}, [name_node, normal_params_node, keyword_params_node, body_node]) do
    name = extract_atom(name_node)
    normal_params = extract_params(normal_params_node)
    keyword_list_pattern = extract_keyword_list_params(keyword_params_node)
    body = eval_simpex_expr(body_node)
    
    # Create a function that matches: name(normal_params, keyword_list_pattern)
    # where keyword_list_pattern is the expected keyword list structure
    all_params = normal_params ++ [keyword_list_pattern]
    
    quote do
      def unquote(name)(unquote_splicing(all_params)) do
        unquote(body)
      end
    end
  end

  # Handle map syntax (% key1 val1 key2 val2) -> %{key1: val1, key2: val2}
  defp eval_function_expr({:%, _meta, nil}, args) do
    pairs = build_map_pairs(args)
    {:%{}, [], pairs}
  end

  # Handle case expression (case expr (pattern1 result1) (pattern2 result2))
  defp eval_function_expr({:case, _meta, nil}, [expr | clauses]) do
    elixir_expr = eval_simpex_expr(expr)
    elixir_clauses = Enum.map(clauses, &build_case_clause/1)
    
    {:case, [], [elixir_expr, [do: elixir_clauses]]}
  end

  # Handle if expression (if condition true_branch false_branch)
  defp eval_function_expr({:if, _meta, nil}, [condition, true_branch, false_branch]) do
    condition_ast = eval_simpex_expr(condition)
    true_ast = eval_simpex_expr(true_branch)
    false_ast = eval_simpex_expr(false_branch)
    
    {:if, [], [condition_ast, [do: true_ast, else: false_ast]]}
  end

  defp eval_function_expr(func_name_node, args) do
    func_name = extract_atom(func_name_node)
    elixir_args = Enum.map(args, &eval_simpex_expr/1)
    
    quote do
      unquote(func_name)(unquote_splicing(elixir_args))
    end
  end

  # Simplified atom extraction - let Elixir handle most cases
  defp extract_atom({:sequence_token, _meta, atom}) when is_atom(atom) do
    handle_atom(atom)
  end
  defp extract_atom({atom, _meta, nil}) when is_atom(atom) do
    handle_atom(atom)
  end
  defp extract_atom(atom) when is_atom(atom) do
    handle_atom(atom)
  end
  defp extract_atom(expr), do: raise("Expected atom, got: #{inspect(expr)}")

  # Handle module calls like String.upcase
  defp handle_atom(atom) do
    atom_str = Atom.to_string(atom)
    if String.contains?(atom_str, ".") do
      # Split "String.upcase" into module and function
      case String.split(atom_str, ".") do
        [module_str, func_str] ->
          # Build proper module call AST
          module = String.to_atom("Elixir." <> module_str)
          func = String.to_atom(func_str)
          {:., [], [module, func]}
        _ ->
          # Multiple dots or other cases - just return the atom
          atom
      end
    else
      atom
    end
  end

  # Parameter extraction with minimal sequence token handling
  defp extract_params({:sequence_block, _meta, :"()", params}) do
    # Check if we have keyword syntax and convert to proper keyword list parameter
    case detect_keyword_syntax(params) do
      {:keywords, keyword_pairs} ->
        # Convert to a single keyword list parameter
        [keyword_pairs]
      :no_keywords ->
        # Normal parameter handling
        Enum.map(params, &to_elixir_param/1)
    end
  end
  defp extract_params([]), do: []
  defp extract_params(params) when is_list(params) do
    Enum.map(params, &to_elixir_param/1)
  end
  defp extract_params(_), do: []

  # Extract keyword list parameters from bracket syntax [key: value, ...]
  defp extract_keyword_list_params({:sequence_block, _meta, :"[]", params}) do
    # Convert keyword syntax to proper keyword list
    build_keyword_pairs_from_list(params)
  end
  defp extract_keyword_list_params(other) do
    raise "Expected keyword list in brackets, got: #{inspect(other)}"
  end

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
  # For complex patterns (tuples, etc.), use the main expression evaluator
  defp to_elixir_param(expr) do
    eval_simpex_expr(expr)
  end

  # Detect if parameters contain keyword syntax (key: value patterns)
  defp detect_keyword_syntax(params) do
    if has_keyword_syntax?(params) do
      keyword_pairs = build_keyword_pairs(params)
      {:keywords, keyword_pairs}
    else
      :no_keywords
    end
  end

  # Check if any parameter looks like keyword syntax
  defp has_keyword_syntax?(params) do
    Enum.any?(params, fn param ->
      case param do
        {:sequence_atom, _, atom} ->
          atom_str = Atom.to_string(atom)
          String.ends_with?(atom_str, ":")
        {:sequence_token, _, atom} ->
          atom_str = Atom.to_string(atom)
          String.ends_with?(atom_str, ":")
        _ -> false
      end
    end)
  end

  # Build keyword list from parameters containing key: value pairs
  defp build_keyword_pairs(params) do
    pairs = params
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [{:sequence_atom, _, key_atom}, value] ->
        # Remove trailing colon from key
        key_str = Atom.to_string(key_atom)
        key = if String.ends_with?(key_str, ":") do
          key_str |> String.trim_trailing(":") |> String.to_atom()
        else
          key_atom
        end
        {key, to_elixir_param(value)}
      [{:sequence_token, _, key_atom}, value] ->
        # Remove trailing colon from key
        key_str = Atom.to_string(key_atom)
        key = if String.ends_with?(key_str, ":") do
          key_str |> String.trim_trailing(":") |> String.to_atom()
        else
          key_atom
        end
        {key, to_elixir_param(value)}
      other -> 
        raise "Invalid keyword syntax in parameters: #{inspect(other)}"
    end)
    
    pairs
  end

  # Build keyword list from bracket syntax [key: value, key2: value2]
  defp build_keyword_pairs_from_list(params) do
    pairs = params
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [{:sequence_atom, _, key_atom}, value] ->
        # Remove trailing colon from key
        key_str = Atom.to_string(key_atom)
        key = if String.ends_with?(key_str, ":") do
          key_str |> String.trim_trailing(":") |> String.to_atom()
        else
          key_atom
        end
        {key, to_elixir_param(value)}
      [{:sequence_token, _, key_atom}, value] ->
        # Remove trailing colon from key
        key_str = Atom.to_string(key_atom)
        key = if String.ends_with?(key_str, ":") do
          key_str |> String.trim_trailing(":") |> String.to_atom()
        else
          key_atom
        end
        {key, to_elixir_param(value)}
      other -> 
        raise "Invalid keyword syntax in bracket list: #{inspect(other)}"
    end)
    
    pairs
  end

  # Build keyword list from general expressions (not function parameters)
  defp build_keyword_pairs_general(items) do
    pairs = items
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [{:sequence_atom, _, key_atom}, value] ->
        # Remove trailing colon from key
        key_str = Atom.to_string(key_atom)
        key = if String.ends_with?(key_str, ":") do
          key_str |> String.trim_trailing(":") |> String.to_atom()
        else
          key_atom
        end
        {key, eval_simpex_expr(value)}
      [{:sequence_token, _, key_atom}, value] ->
        # Remove trailing colon from key
        key_str = Atom.to_string(key_atom)
        key = if String.ends_with?(key_str, ":") do
          key_str |> String.trim_trailing(":") |> String.to_atom()
        else
          key_atom
        end
        {key, eval_simpex_expr(value)}
      other -> 
        raise "Invalid keyword syntax in list: #{inspect(other)}"
    end)
    
    pairs
  end

  # Build map key-value pairs
  defp build_map_pairs([]), do: []
  defp build_map_pairs([key, value | rest]) do
    key_ast = eval_simpex_expr(key)
    value_ast = eval_simpex_expr(value)
    [{key_ast, value_ast} | build_map_pairs(rest)]
  end
  defp build_map_pairs([_single]) do
    raise "Map requires even number of arguments (key-value pairs)"
  end

  # Build case clause from (pattern result) -> pattern -> result
  defp build_case_clause({:sequence_paren, _meta, [pattern, result]}) do
    pattern_ast = eval_simpex_expr(pattern)
    result_ast = eval_simpex_expr(result)
    {:->, [], [[pattern_ast], result_ast]}
  end
  # Handle sequence_block format for case clauses
  defp build_case_clause({:sequence_block, _meta, :"()", [pattern, result]}) do
    pattern_ast = eval_simpex_expr(pattern)
    result_ast = eval_simpex_expr(result)
    {:->, [], [[pattern_ast], result_ast]}
  end
  defp build_case_clause(clause) do
    raise "Invalid case clause, expected (pattern result), got: #{inspect(clause)}"
  end
end
