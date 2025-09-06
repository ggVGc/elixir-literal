defmodule Lipex.Core.PatternMatching do
  @moduledoc """
  Handles pattern matching expressions in Lipex.

  Supports:
  - Variable assignment: `(= x 42)`
  - Tuple destructuring: `(= {a b} (tuple 1 2))`
  - Map destructuring: `(= (% :name n :age a) user_map)`
  - List destructuring: `(= [h | t] [1 2 3])`
  - Nested patterns: `(= {x (% :y z)} complex_data)`

  This follows Elixir conventions where `=` is the match operator.
  """

  @behaviour Lipex.Evaluator

  @doc """
  Tries to evaluate pattern matching expressions.

  Returns `{:ok, result}` for pattern matching expressions, `:pass` otherwise.
  """
  def try_eval(expr) do
    case expr do
      # Pattern matching in sequence_block format: (= pattern value)
      {:sequence_block, _meta, :"()", [{:sequence_token, _, :=} | args]} ->
        case args do
          [pattern, value] -> {:ok, eval_pattern_match(pattern, value)}
          _ -> raise "Pattern match (=) requires exactly 2 arguments"
        end

      # Pattern matching with atom operator: (= pattern value)
      {:sequence_prefix, _meta, [:=, pattern, value]} ->
        {:ok, eval_pattern_match(pattern, value)}

      # Pattern matching with AST node operator: (= pattern value) where = is {:=, _, nil}
      {:sequence_prefix, {:=, _, nil}, [pattern, value]} ->
        {:ok, eval_pattern_match(pattern, value)}

      # Not a pattern matching expression
      _ ->
        :pass
    end
  end

  @doc """
  Evaluates pattern matching expressions.

  ## Examples

      (= x 42)                    -> x = 42
      (= {a b} (tuple 1 2))       -> {a, b} = {1, 2}
      (= [h | t] [1 2 3])         -> [h | t] = [1, 2, 3]
      (= (% :name n) user)        -> %{name: n} = user
  """
  def eval_pattern_match(pattern, value) do
    elixir_pattern = convert_lipex_pattern_to_elixir(pattern)
    elixir_value = Lipex.eval_lipex_expr(value)

    quote do
      unquote(elixir_pattern) = unquote(elixir_value)
    end
  end

  @doc """
  Converts Lipex patterns to Elixir AST patterns.

  This handles the translation from Lipex syntax to equivalent Elixir patterns.
  """
  def convert_lipex_pattern_to_elixir(pattern) do
    case pattern do
      # Literals - atoms, numbers, strings pass through as-is
      atom when is_atom(atom) ->
        atom

      number when is_number(number) ->
        number

      string when is_binary(string) ->
        string

      # Variables - convert AST node to Elixir variable
      {var, meta, nil} when is_atom(var) ->
        # Handle special cases
        case var do
          # Wildcard pattern
          :_ -> {:_, [], nil}
          _ -> {var, meta, nil}
        end

      # Tuple patterns with braces: {a b c} -> {a, b, c}
      {:sequence_brace, _meta, elements} ->
        pattern_elements = Enum.map(elements, &convert_lipex_pattern_to_elixir/1)

        case length(pattern_elements) do
          0 ->
            quote do: {}

          1 ->
            [elem] = pattern_elements
            quote do: {unquote(elem)}

          2 ->
            [a, b] = pattern_elements
            quote do: {unquote(a), unquote(b)}

          _ ->
            {:{}, [], pattern_elements}
        end

      # Tuple patterns with parens: (tuple a b c) -> {a, b, c}
      {:sequence_paren, _meta, [{:tuple, _, nil} | elements]} ->
        pattern_elements = Enum.map(elements, &convert_lipex_pattern_to_elixir/1)

        case length(pattern_elements) do
          0 ->
            quote do: {}

          1 ->
            [elem] = pattern_elements
            quote do: {unquote(elem)}

          2 ->
            [a, b] = pattern_elements
            quote do: {unquote(a), unquote(b)}

          _ ->
            {:{}, [], pattern_elements}
        end

      # Map patterns: (% :key1 var1 :key2 var2) -> %{key1: var1, key2: var2}
      {:sequence_prefix, _meta, [:% | args]} ->
        pairs = convert_map_pattern_args(args)
        quote do: %{unquote_splicing(pairs)}

      # List patterns: [a b c] -> [a, b, c], [a | b] -> [a | b]
      {:sequence_bracket, _meta, elements} ->
        convert_list_pattern(elements)

      # List patterns with list constructor: (list a b c) -> [a, b, c]
      {:sequence_paren, _meta, [{:list, _, nil} | elements]} ->
        pattern_elements = Enum.map(elements, &convert_lipex_pattern_to_elixir/1)
        quote do: unquote(pattern_elements)

      # Cons patterns: (cons h t) -> [h | t] (head-tail destructuring)
      {:sequence_paren, _meta, [{:cons, _, nil}, head, tail]} ->
        elixir_head = convert_lipex_pattern_to_elixir(head)
        elixir_tail = convert_lipex_pattern_to_elixir(tail)
        quote do: [unquote(elixir_head) | unquote(elixir_tail)]

      # Struct patterns: (struct ModuleName :field1 var1) -> %ModuleName{field1: var1}
      {:sequence_paren, _meta, [{:struct, _, nil}, module_name | field_args]} ->
        elixir_module = convert_lipex_pattern_to_elixir(module_name)
        field_pairs = convert_struct_pattern_args(field_args)
        quote do: %unquote(elixir_module){unquote_splicing(field_pairs)}

      # String interpolation patterns (if supported later)
      # Binary patterns (if supported later)

      # Pass through other patterns as-is for now
      other ->
        other
    end
  end

  # Private helper functions

  defp convert_map_pattern_args(args) do
    args
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [key, value_pattern] ->
        elixir_key =
          case convert_lipex_pattern_to_elixir(key) do
            atom when is_atom(atom) -> atom
            other -> other
          end

        elixir_value_pattern = convert_lipex_pattern_to_elixir(value_pattern)
        {elixir_key, elixir_value_pattern}

      [key] ->
        raise "Map pattern requires even number of arguments, got odd key: #{inspect(key)}"
    end)
  end

  defp convert_struct_pattern_args(field_args) do
    field_args
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [key, value_pattern] ->
        elixir_key =
          case convert_lipex_pattern_to_elixir(key) do
            atom when is_atom(atom) -> atom
            other -> raise "Struct field keys must be atoms, got: #{inspect(other)}"
          end

        elixir_value_pattern = convert_lipex_pattern_to_elixir(value_pattern)
        {elixir_key, elixir_value_pattern}

      [key] ->
        raise "Struct pattern requires even number of field arguments, got odd key: #{inspect(key)}"
    end)
  end

  defp convert_list_pattern(elements) do
    case find_pipe_operator(elements) do
      nil ->
        # Regular list pattern: [a b c] -> [a, b, c]
        pattern_elements = Enum.map(elements, &convert_lipex_pattern_to_elixir/1)
        quote do: unquote(pattern_elements)

      pipe_index ->
        # Cons pattern: [a b | tail] -> [a, b | tail]
        {head_elements, [{:|, _, nil} | tail_elements]} = Enum.split(elements, pipe_index)

        case tail_elements do
          [tail] ->
            head_patterns = Enum.map(head_elements, &convert_lipex_pattern_to_elixir/1)
            tail_pattern = convert_lipex_pattern_to_elixir(tail)

            # Build the cons pattern [h1, h2, ... | tail]
            case head_patterns do
              # Just the tail
              [] -> tail_pattern
              [single] -> quote do: [unquote(single) | unquote(tail_pattern)]
              multiple -> quote do: [unquote_splicing(multiple) | unquote(tail_pattern)]
            end

          [] ->
            raise "Cons pattern requires a tail after |"

          _ ->
            raise "Cons pattern can only have one element after |"
        end
    end
  end

  defp find_pipe_operator(elements) do
    elements
    |> Enum.with_index()
    |> Enum.find(fn
      {{:|, _, nil}, _index} -> true
      _ -> false
    end)
    |> case do
      nil -> nil
      {_, index} -> index
    end
  end
end
