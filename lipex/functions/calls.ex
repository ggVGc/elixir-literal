defmodule Lipex.Functions.Calls do
  @moduledoc """
  Handles function call evaluation in Lipex expressions.
  
  Supports:
  - Local function calls: `(function-name args...)`
  - Module function calls: `("Module.function" args...)`
  - Recursive calls with proper scoping
  """
  
  @behaviour Lipex.Evaluator
  
  @doc """
  Tries to evaluate function call expressions.
  
  Returns `{:ok, result}` for function call patterns, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # Function calls with sequence_prefix: (func arg1 arg2)
      {:sequence_prefix, _meta, [function | _args]} when is_atom(function) ->
        {:ok, eval_function_call(expr)}
      
      # Function calls with sequence_paren: ("Module.func" arg1 arg2) or (func arg1 arg2)
      {:sequence_paren, _meta, [function | _args]} ->
        {:ok, eval_function_call(expr)}
      
      # Not a function call expression
      _ ->
        :pass
    end
  end
  
  @doc """
  Evaluates function call expressions.
  
  ## Examples
  
      (add 1 2)                    -> Local function call
      ("String.upcase" "hello")    -> Module function call  
      (factorial 5)                -> Recursive call
  """
  def eval_function_call({:sequence_prefix, _meta, [function | args]}) when is_atom(function) do
    elixir_function_name = function
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    # For local calls, use module-scoped calls to support recursion
    quote do: __MODULE__.unquote(elixir_function_name)(unquote_splicing(elixir_args))
  end
  
  def eval_function_call({:sequence_paren, _meta, [function | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    
    case function do
      # Handle module calls like (String.upcase ...)
      {{:., _, [module, func_name]}, _, []} ->
        elixir_module = Lipex.eval_lipex_expr(module)
        quote do: unquote(elixir_module).unquote(func_name)(unquote_splicing(elixir_args))
      
      # Handle string function names like "IO.puts" or "String.upcase"
      module_func when is_binary(module_func) ->
        if String.contains?(module_func, ".") do
          # This is a "Module.function" call
          parts = String.split(module_func, ".")
          {module_parts, [func_name]} = Enum.split(parts, -1)
          
          # Convert module parts to proper module alias
          module_atoms = Enum.map(module_parts, &String.to_atom/1)
          module_alias = {:__aliases__, [], module_atoms}
          
          func_atom = String.to_atom(func_name)
          
          # Generate proper module call using dot notation
          quote do
            unquote(module_alias).unquote(func_atom)(unquote_splicing(elixir_args))
          end
        else
          # Regular local function call as string
          func_atom = String.to_atom(module_func)
          quote do: __MODULE__.unquote(func_atom)(unquote_splicing(elixir_args))
        end
      
      # Handle atom function names (including dot notation like :"IO.puts")
      {module_func, _, nil} when is_atom(module_func) ->
        string = Atom.to_string(module_func)
        if String.contains?(string, ".") do
          # This is a module.function call - create proper module alias
          parts = String.split(string, ".")
          {module_parts, [func_name]} = Enum.split(parts, -1)
          
          # Convert module parts to proper module alias (like the string version)
          module_atoms = Enum.map(module_parts, &String.to_atom/1)
          module_alias = {:__aliases__, [], module_atoms}
          func_atom = String.to_atom(func_name)
          
          # Generate proper module call using dot notation
          quote do
            unquote(module_alias).unquote(func_atom)(unquote_splicing(elixir_args))
          end
        else
          # Regular local function call
          quote do: __MODULE__.unquote(module_func)(unquote_splicing(elixir_args))
        end
      
      # Function variable or complex expression
      other ->
        elixir_function = Lipex.eval_lipex_expr(other)
        quote do: unquote(elixir_function).(unquote_splicing(elixir_args))
    end
  end
end