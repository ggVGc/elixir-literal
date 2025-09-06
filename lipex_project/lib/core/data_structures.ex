defmodule Lipex.Core.DataStructures do
  @moduledoc """
  Handles data structure construction in Lipex expressions.

  Supports:
  - Maps: `(% :key1 value1 :key2 value2)`
  - Tuples: `(tuple a b c)`
  - Structs: `(struct User :name "John" :age 30)`
  - Keyword Lists: `(kwlist :timeout 5000 :retries 3)`
  - Lists: `(list 1 2 3)`
  """

  @behaviour Lipex.Evaluator

  @doc """
  Tries to evaluate data structure expressions.

  Returns `{:ok, result}` for data structure patterns, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # Maps in sequence_block format: (% key1 value1 key2 value2)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :%} | args]} ->
        {:ok, eval_map({:sequence_prefix, [], [:% | args]})}

      # Maps with atom operator: (% key1 value1 key2 value2)
      {:sequence_prefix, _meta, [:% | _args]} ->
        {:ok, eval_map(expr)}

      # Maps with AST node operator: (% key1 value1) where % is {:%, _, nil}
      {:sequence_prefix, {:%, _, nil}, args} ->
        {:ok, eval_map({:sequence_prefix, [], [:% | args]})}

      # Tuples in sequence_block format: (tuple a b c)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :tuple} | args]} ->
        {:ok, eval_tuple({:sequence_paren, [], [{:tuple, [], nil} | args]})}

      # Structs in sequence_block format: (struct ModuleName :field1 val1 :field2 val2)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :struct} | args]} ->
        {:ok, eval_struct({:sequence_paren, [], [{:struct, [], nil} | args]})}

      # Keyword Lists in sequence_block format: (kwlist :key1 val1 :key2 val2)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :kwlist} | args]} ->
        {:ok, eval_kwlist({:sequence_paren, [], [{:kwlist, [], nil} | args]})}

      # Lists in sequence_block format: (list 1 2 3)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :list} | args]} ->
        {:ok, eval_list({:sequence_paren, [], [{:list, [], nil} | args]})}

      # Tuples: (tuple a b c)
      {:sequence_paren, _meta, [{:tuple, _, nil} | _args]} ->
        {:ok, eval_tuple(expr)}

      # Structs: (struct ModuleName :field1 val1 :field2 val2)
      {:sequence_paren, _meta, [{:struct, _, nil} | _args]} ->
        {:ok, eval_struct(expr)}

      # Keyword Lists: (kwlist :key1 val1 :key2 val2)
      {:sequence_paren, _meta, [{:kwlist, _, nil} | _args]} ->
        {:ok, eval_kwlist(expr)}

      # Lists: (list 1 2 3) - sequence_paren format
      {:sequence_paren, _meta, [{:list, _, nil} | _args]} ->
        {:ok, eval_list(expr)}

      # Lists: (list 1 2 3) - sequence_prefix format
      {:sequence_prefix, {:list, _, nil}, args} ->
        {:ok, eval_list_prefix({:sequence_prefix, {:list, [], nil}, args})}

      # Not a data structure expression
      _ ->
        :pass
    end
  end

  @doc """
  Evaluates map construction expressions.

  ## Examples

      (% :name "John" :age 30)  -> %{name: "John", age: 30}
      (% a b c d)               -> %{a => b, c => d}
  """
  def eval_map({:sequence_prefix, _meta, [:% | args]}) do
    # Convert args to key-value pairs
    pairs =
      args
      |> Enum.chunk_every(2)
      |> Enum.map(fn
        [key, value] ->
          elixir_key = Lipex.eval_lipex_expr(key)
          elixir_value = Lipex.eval_lipex_expr(value)
          {elixir_key, elixir_value}

        [key] ->
          raise "Map construction requires even number of arguments, got odd key: #{inspect(key)}"
      end)

    # Generate map construction AST
    quote do
      %{unquote_splicing(pairs)}
    end
  end

  @doc """
  Evaluates tuple construction expressions.

  ## Examples

      (tuple :ok result)     -> {:ok, result}
      (tuple a b c)          -> {a, b, c}
  """
  def eval_tuple({:sequence_paren, _meta, [{:tuple, _, nil} | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)

    case length(elixir_args) do
      0 ->
        quote do: {}

      1 ->
        [arg] = elixir_args
        quote do: {unquote(arg)}

      2 ->
        [a, b] = elixir_args
        quote do: {unquote(a), unquote(b)}

      3 ->
        [a, b, c] = elixir_args
        quote do: {unquote(a), unquote(b), unquote(c)}

      4 ->
        [a, b, c, d] = elixir_args
        quote do: {unquote(a), unquote(b), unquote(c), unquote(d)}

      n when n <= 10 ->
        # For tuples up to 10 elements, generate the tuple directly
        tuple_ast = List.to_tuple(elixir_args)
        quote do: unquote(tuple_ast)

      _ ->
        # For larger tuples, use List.to_tuple/1
        quote do: List.to_tuple(unquote(elixir_args))
    end
  end

  @doc """
  Evaluates struct construction expressions.

  ## Examples

      (struct User :name "John")              -> %User{name: "John"}
      (struct MyApp.User :name "John" :age 30) -> %MyApp.User{name: "John", age: 30}
  """
  def eval_struct({:sequence_paren, _meta, [{:struct, _, nil}, struct_name | field_args]}) do
    # Evaluate the struct name
    elixir_struct_name = Lipex.eval_lipex_expr(struct_name)

    # Convert field arguments to keyword pairs
    field_pairs =
      field_args
      |> Enum.chunk_every(2)
      |> Enum.map(fn
        [key, value] ->
          elixir_key =
            case Lipex.eval_lipex_expr(key) do
              atom when is_atom(atom) -> atom
              other -> raise "Struct field keys must be atoms, got: #{inspect(other)}"
            end

          elixir_value = Lipex.eval_lipex_expr(value)
          {elixir_key, elixir_value}

        [key] ->
          raise "Struct construction requires even number of field arguments, got odd key: #{inspect(key)}"
      end)

    quote do
      struct(unquote(elixir_struct_name), unquote(field_pairs))
    end
  end

  @doc """
  Evaluates keyword list construction expressions.

  ## Examples

      (kwlist :timeout 5000 :retries 3)  -> [timeout: 5000, retries: 3]
  """
  def eval_kwlist({:sequence_paren, _meta, [{:kwlist, _, nil} | args]}) do
    # Convert args to keyword pairs
    pairs =
      args
      |> Enum.chunk_every(2)
      |> Enum.map(fn
        [key, value] ->
          elixir_key =
            case Lipex.eval_lipex_expr(key) do
              atom when is_atom(atom) -> atom
              other -> raise "Keyword list keys must be atoms, got: #{inspect(other)}"
            end

          elixir_value = Lipex.eval_lipex_expr(value)
          {elixir_key, elixir_value}

        [key] ->
          raise "Keyword list construction requires even number of arguments, got odd key: #{inspect(key)}"
      end)

    quote do: unquote(pairs)
  end

  @doc """
  Evaluates list construction expressions.

  ## Examples

      (list 1 2 3)         -> [1, 2, 3]
      (list (+ 1 2) (* 3 4)) -> [3, 12]
  """
  def eval_list({:sequence_paren, _meta, [{:list, _, nil} | args]}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    quote do: unquote(elixir_args)
  end

  @doc """
  Evaluates list construction expressions in sequence_prefix format.

  ## Examples

      (list 1 2 3)         -> [1, 2, 3]
      (list)               -> []
  """
  def eval_list_prefix({:sequence_prefix, {:list, _, nil}, args}) do
    elixir_args = Enum.map(args, &Lipex.eval_lipex_expr/1)
    quote do: unquote(elixir_args)
  end

  @doc """
  Evaluates list operation expressions (first, rest, cons).

  ## Examples

      (first [1 2 3])       -> 1
      (rest [1 2 3])        -> [2, 3]  
      (cons 0 [1 2 3])      -> [0, 1, 2, 3]
  """
  def eval_list_op({:sequence_paren, _meta, [{:first, _, nil}, list]}) do
    elixir_list = Lipex.eval_lipex_expr(list)
    quote do: List.first(unquote(elixir_list))
  end

  def eval_list_op({:sequence_paren, _meta, [{:rest, _, nil}, list]}) do
    elixir_list = Lipex.eval_lipex_expr(list)

    quote do
      case unquote(elixir_list) do
        [_ | tail] -> tail
        [] -> []
        nil -> []
      end
    end
  end

  def eval_list_op({:sequence_paren, _meta, [{:cons, _, nil}, item, list]}) do
    elixir_item = Lipex.eval_lipex_expr(item)
    elixir_list = Lipex.eval_lipex_expr(list)
    quote do: [unquote(elixir_item) | unquote(elixir_list)]
  end

  @doc """
  Evaluates range construction expressions.

  ## Examples

      (range 1 10)          -> 1..10
      (range 1 10 2)        -> 1..10//2
  """
  def eval_range({:sequence_paren, _meta, [{:range, _, nil}, start, stop]}) do
    elixir_start = Lipex.eval_lipex_expr(start)
    elixir_stop = Lipex.eval_lipex_expr(stop)
    quote do: unquote(elixir_start)..unquote(elixir_stop)
  end

  def eval_range({:sequence_paren, _meta, [{:range, _, nil}, start, stop, step]}) do
    elixir_start = Lipex.eval_lipex_expr(start)
    elixir_stop = Lipex.eval_lipex_expr(stop)
    elixir_step = Lipex.eval_lipex_expr(step)
    quote do: unquote(elixir_start)..unquote(elixir_stop)//unquote(elixir_step)
  end
end
