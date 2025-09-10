# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Code.Formatter.SequenceLiteral do
  @moduledoc false
  import Inspect.Algebra, except: [format: 2, surround: 3, surround: 4]

  @min_line 0
  @max_line 9_999_999
  @empty empty()

  def quoted_to_algebra({:sequence_literal, meta, args}, _context, state) do
    case args do
      [] ->
        {string("~~()"), state}

      [single_arg] ->
        {args_doc, state} = sequence_literal_element_to_algebra(single_arg, state)
        doc = string("~~(") |> concat(args_doc) |> concat(string(")"))
        {doc, state}

      _ ->
        # Check if this was originally multi-line by looking at line spans
        min_line = meta[:line] || @min_line

        max_line =
          case args do
            [] ->
              min_line

            _ ->
              args
              |> Enum.reduce(min_line, fn arg, max ->
                {_arg_min, arg_max} = traverse_line(arg, {@max_line, @min_line})

                case arg_max do
                  # No line info found, keep current max
                  @min_line -> max
                  line -> max(line, max)
                end
              end)
          end

        is_multiline = max_line > min_line

        {args_docs, state} =
          Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
            {doc, state} = sequence_literal_element_to_algebra(arg, state)
            {[doc | acc], state}
          end)

        if is_multiline do
          # Original was multi-line: preserve newlines
          args_doc =
            args_docs
            |> Enum.reverse()
            |> Enum.reduce(&line(&2, &1))

          doc =
            string("~~(")
            |> concat(line())
            |> concat(args_doc)
            |> concat(line())
            |> concat(string(")"))

          {doc, state}
        else
          # Original was single-line: keep compact
          args_doc =
            args_docs
            |> Enum.reverse()
            |> Enum.reduce(&concat(&2, concat(" ", &1)))

          doc = string("~~(") |> concat(args_doc) |> concat(string(")"))
          {doc, state}
        end
    end
  end

  def sequence_literal_args_to_algebra(args, state) do
    case args do
      [] ->
        {@empty, state}

      [first | rest] ->
        {first_doc, state} = sequence_literal_element_to_algebra(first, state)

        {rest_docs, state} =
          Enum.reduce(rest, {[], state}, fn arg, {acc, state} ->
            {doc, state} = sequence_literal_element_to_algebra(arg, state)
            {[doc | acc], state}
          end)

        docs = [first_doc | Enum.reverse(rest_docs)]

        final_doc =
          Enum.reduce(docs, fn doc, acc ->
            concat([acc, " ", doc])
          end)

        {final_doc, state}
    end
  end

  def sequence_literal_element_to_algebra({:sequence_prefix, op, args}, state) do
    # Handle operator node specially - just extract the atom name
    op_meta =
      case op do
        {_atom, meta, _} -> meta
        _ -> []
      end

    {op_doc, state} =
      case op do
        {atom, _meta, nil} when is_atom(atom) ->
          {Atom.to_string(atom) |> string(), state}

        _ ->
          sequence_literal_element_to_algebra(op, state)
      end

    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = sequence_literal_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    if Enum.empty?(args_docs) do
      # For bare atoms/operators, don't add extra parentheses
      {op_doc, state}
    else
      # Check if arguments span multiple lines
      min_line = op_meta[:line] || @min_line

      max_line =
        args
        |> Enum.reduce(min_line, fn arg, max ->
          {_arg_min, arg_max} = traverse_line(arg, {@max_line, @min_line})

          case arg_max do
            @min_line -> max
            line -> max(line, max)
          end
        end)

      is_multiline = max_line > min_line

      # Format with intelligent line breaks for multi-line expressions
      if is_multiline do
        reversed_args = Enum.reverse(args_docs)

        # For function definitions (def name args body...), keep header together
        # and put body expressions on separate lines
        case {op, reversed_args} do
          {{:def, _, _}, [name_doc, args_doc | body_docs]} when body_docs != [] ->
            # Keep 'def name args' together, separate body expressions
            header_doc = concat([name_doc, " ", args_doc])
            body_doc = body_docs |> Enum.reduce(&line(&2, &1))
            indented_body = line("", body_doc) |> nest(2)
            {concat([op_doc, " ", header_doc, indented_body]), state}

          _ ->
            # For other cases, apply indentation to multi-line arguments
            case reversed_args do
              [single_arg] ->
                # Single argument - no special indentation needed
                {concat([op_doc, " ", single_arg]), state}

              [first_arg | rest_args] ->
                # Multiple arguments - first stays on same line, rest get indented
                rest_doc = rest_args |> Enum.reduce(&line(&2, &1))
                indented_rest = line("", rest_doc) |> nest(2)
                {concat([op_doc, " ", first_arg, indented_rest]), state}

              [] ->
                # No arguments
                {op_doc, state}
            end
        end
      else
        args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
        {concat([op_doc, " ", args_doc]), state}
      end
    end
  end

  def sequence_literal_element_to_algebra({:sequence_paren, meta, args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = sequence_literal_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    # Check if arguments span multiple lines
    min_line = meta[:line] || @min_line

    max_line =
      args
      |> Enum.reduce(min_line, fn arg, max ->
        {_arg_min, arg_max} = traverse_line(arg, {@max_line, @min_line})

        case arg_max do
          @min_line -> max
          line -> max(line, max)
        end
      end)

    is_multiline = max_line > min_line

    # Format with newlines if multi-line, spaces if single-line
    if is_multiline do
      # Check if we have a single sequence_prefix that handles its own formatting
      case {args, args_docs} do
        {[{:sequence_prefix, _, _}], [single_doc]} ->
          # Single sequence_prefix - let it handle its own multi-line formatting
          {concat(["(", single_doc, ")"]), state}

        _ ->
          # Multiple args or non-prefix - add our own line breaks
          args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&line(&2, &1))
          {concat(["(", line(), args_doc, line(), ")"]), state}
      end
    else
      args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
      {concat(["(", args_doc, ")"]), state}
    end
  end

  def sequence_literal_element_to_algebra({:sequence_block, meta, :"()", args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = sequence_literal_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    # Check if arguments span multiple lines
    min_line = meta[:line] || @min_line

    max_line =
      args
      |> Enum.reduce(min_line, fn arg, max ->
        {_arg_min, arg_max} = traverse_line(arg, {@max_line, @min_line})

        case arg_max do
          @min_line -> max
          line -> max(line, max)
        end
      end)

    is_multiline = max_line > min_line

    args_doc =
      case Enum.reverse(args_docs) do
        [] -> @empty
        [single] -> single
        docs when is_multiline -> Enum.reduce(docs, &line(&2, &1))
        docs -> Enum.reduce(docs, &concat(&2, concat(" ", &1)))
      end

    if is_multiline && length(args) > 1 do
      {concat(["(", line(), args_doc, line(), ")"]), state}
    else
      {concat(["(", args_doc, ")"]), state}
    end
  end

  def sequence_literal_element_to_algebra({:sequence_block, meta, :"[]", args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = sequence_literal_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    # Check if arguments span multiple lines
    min_line = meta[:line] || @min_line

    max_line =
      args
      |> Enum.reduce(min_line, fn arg, max ->
        {_arg_min, arg_max} = traverse_line(arg, {@max_line, @min_line})

        case arg_max do
          @min_line -> max
          line -> max(line, max)
        end
      end)

    is_multiline = max_line > min_line

    if is_multiline && length(args) > 1 do
      args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&line(&2, &1))
      {concat(["[", line(), args_doc, line(), "]"]), state}
    else
      args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
      {concat(["[", args_doc, "]"]), state}
    end
  end

  def sequence_literal_element_to_algebra({:sequence_block, meta, :{}, args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = sequence_literal_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    # Check if arguments span multiple lines
    min_line = meta[:line] || @min_line

    max_line =
      args
      |> Enum.reduce(min_line, fn arg, max ->
        {_arg_min, arg_max} = traverse_line(arg, {@max_line, @min_line})

        case arg_max do
          @min_line -> max
          line -> max(line, max)
        end
      end)

    is_multiline = max_line > min_line

    if is_multiline && length(args) > 1 do
      args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&line(&2, &1))
      {concat(["{", line(), args_doc, line(), "}"]), state}
    else
      args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
      {concat(["{", args_doc, "}"]), state}
    end
  end

  def sequence_literal_element_to_algebra({:sequence_brace, _meta, args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = sequence_literal_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
    {concat(["{", args_doc, "}"]), state}
  end

  def sequence_literal_element_to_algebra({:sequence_token, _meta, atom}, state)
      when is_atom(atom) do
    {Atom.to_string(atom) |> string(), state}
  end

  def sequence_literal_element_to_algebra({:sequence_atom, _meta, atom}, state) do
    {(":" <> Atom.to_string(atom)) |> string(), state}
  end

  def sequence_literal_element_to_algebra({:sequence_string, _meta, value}, state) do
    str = if is_list(value), do: List.to_string(value), else: value
    {inspect(str) |> string(), state}
  end

  def sequence_literal_element_to_algebra({:sequence_chars, _meta, value}, state) do
    {inspect(value, as_charlists: :as_charlists) |> string(), state}
  end

  def sequence_literal_element_to_algebra({:sequence_number, _meta, value}, state) do
    {to_string(value) |> string(), state}
  end

  # Handle regular AST nodes within sequence literals
  def sequence_literal_element_to_algebra({atom, _meta, nil}, state) when is_atom(atom) do
    {Atom.to_string(atom) |> string(), state}
  end

  def sequence_literal_element_to_algebra(number, state) when is_number(number) do
    {to_string(number) |> string(), state}
  end

  def sequence_literal_element_to_algebra(string, state) when is_binary(string) do
    {inspect(string) |> string(), state}
  end

  def sequence_literal_element_to_algebra(atom, state) when is_atom(atom) do
    {(":" <> Atom.to_string(atom)) |> string(), state}
  end

  def sequence_literal_element_to_algebra(list, state) when is_list(list) do
    {docs, state} =
      Enum.reduce(list, {[], state}, fn arg, {acc, state} ->
        {doc, state} = sequence_literal_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    args_doc = docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
    {concat(["[", args_doc, "]"]), state}
  end

  def sequence_literal_element_to_algebra(other, state) do
    # Fallback to regular quoted_to_algebra for complex expressions
    Code.Formatter.delegate_to_sequence_literal(
      :quoted_to_algebra,
      {other, :sequence_element, state},
      nil
    )
  end

  ## Line traversal helpers

  def traverse_line({:sequence_literal, meta, args}, {min, max}) do
    acc = extract_line_from_meta(meta, {min, max})
    traverse_line(args, acc)
  end

  def traverse_line({:sequence_prefix, op, args}, {min, max}) do
    acc = traverse_line(op, {min, max})
    traverse_line(args, acc)
  end

  def traverse_line({:sequence_paren, meta, args}, {min, max}) do
    acc = extract_line_from_meta(meta, {min, max})
    traverse_line(args, acc)
  end

  def traverse_line({:sequence_block, meta, _bracket_type, args}, {min, max}) do
    acc = extract_line_from_meta(meta, {min, max})
    traverse_line(args, acc)
  end

  def traverse_line(other, acc) do
    Code.Formatter.delegate_to_sequence_literal(:traverse_line, {other, acc}, nil)
  end

  def extract_line_from_meta(meta, {min, max}) when is_list(meta) do
    case :lists.keyfind(:line, 1, meta) do
      {:line, line} -> {min(line, min), max(line, max)}
      false -> {min, max}
    end
  end

  def extract_line_from_meta(_, acc), do: acc
end
