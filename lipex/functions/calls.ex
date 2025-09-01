defmodule Lipex.Functions.Calls do
  @moduledoc """
  Handles function call evaluation in Lipex expressions.
  
  Supports:
  - Local function calls: `(function-name args...)`
  - Module function calls: `(Module.function args...)`
  - Recursive calls with proper scoping
  """
  
  @doc """
  Evaluates function call expressions.
  
  ## Examples
  
      (add 1 2)                    -> Local function call
      (String.upcase "hello")      -> Module function call  
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
      
      # Handle atom function names  
      {module_func, _, nil} when is_atom(module_func) ->
        string = Atom.to_string(module_func)
        if String.contains?(string, ".") do
          # This is a module.function call
          parts = String.split(string, ".")
          {module_parts, [func_name]} = Enum.split(parts, -1)
          module_name = module_parts |> Enum.join(".") |> String.to_atom()
          func_atom = String.to_atom(func_name)
          
          quote do: unquote(module_name).unquote(func_atom)(unquote_splicing(elixir_args))
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