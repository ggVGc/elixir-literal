# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Code.Formatter.SequenceLiteral do
  @moduledoc false
  import Inspect.Algebra, except: [format: 2, surround: 3, surround: 4]

  @min_line 0
  @max_line 9_999_999
  @empty empty()

  def quoted_to_algebra({:raw_section, meta, args}, _context, state) do
    case args do
      [] ->
        {string("~~()"), state}

      [single_arg] ->
        {args_doc, state} = raw_section_element_to_algebra(single_arg, state)
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
            {doc, state} = raw_section_element_to_algebra(arg, state)
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

  def raw_section_args_to_algebra(args, state) do
    case args do
      [] ->
        {@empty, state}

      [first | rest] ->
        {first_doc, state} = raw_section_element_to_algebra(first, state)

        {rest_docs, state} =
          Enum.reduce(rest, {[], state}, fn arg, {acc, state} ->
            {doc, state} = raw_section_element_to_algebra(arg, state)
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

  def raw_section_element_to_algebra({:sequence_paren, meta, args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = raw_section_element_to_algebra(arg, state)
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
      # Check for function-like pattern: (operator name args body...)
      case args do
        [{operator, _, nil}, {name, _, nil} | rest]
        when is_atom(operator) and is_atom(name) ->
          # This is a function-like pattern with operator and name
          # Format: (operator name args on one line, then body on separate lines)
          [operator_doc, name_doc | rest_docs] = Enum.reverse(args_docs)

          case rest do
            [{:sequence_block, _, :"()", _} | body] when body != [] ->
              # Has args and body - format specially
              [args_doc | body_docs] = rest_docs
              header = concat([operator_doc, " ", name_doc, " ", args_doc])
              body_doc = body_docs |> Enum.reduce(&line(&2, &1))
              {concat(["(", header, line(), body_doc, ")"]), state}

            _ ->
              # No special pattern - use default formatting
              args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&line(&2, &1))
              {concat(["(", line(), args_doc, line(), ")"]), state}
          end

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

  def raw_section_element_to_algebra({:sequence_block, meta, :"()", args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = raw_section_element_to_algebra(arg, state)
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

  def raw_section_element_to_algebra({:sequence_block, meta, :"[]", args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = raw_section_element_to_algebra(arg, state)
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

  def raw_section_element_to_algebra({:sequence_block, meta, :{}, args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = raw_section_element_to_algebra(arg, state)
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

  def raw_section_element_to_algebra({:sequence_brace, _meta, args}, state) do
    {args_docs, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {doc, state} = raw_section_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    args_doc = args_docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
    {concat(["{", args_doc, "}"]), state}
  end

  def raw_section_element_to_algebra({:raw_token, _meta, atom}, state)
      when is_atom(atom) do
    {Atom.to_string(atom) |> string(), state}
  end

  def raw_section_element_to_algebra({:sequence_atom, _meta, atom}, state) do
    {(":" <> Atom.to_string(atom)) |> string(), state}
  end

  def raw_section_element_to_algebra({:sequence_string, _meta, value}, state) do
    str = if is_list(value), do: List.to_string(value), else: value
    {inspect(str) |> string(), state}
  end

  def raw_section_element_to_algebra({:sequence_chars, _meta, value}, state) do
    {inspect(value, as_charlists: :as_charlists) |> string(), state}
  end

  def raw_section_element_to_algebra({:sequence_number, _meta, value}, state) do
    {to_string(value) |> string(), state}
  end

  # Handle regular AST nodes within sequence literals
  def raw_section_element_to_algebra({atom, _meta, nil}, state) when is_atom(atom) do
    {Atom.to_string(atom) |> string(), state}
  end

  def raw_section_element_to_algebra(number, state) when is_number(number) do
    {to_string(number) |> string(), state}
  end

  def raw_section_element_to_algebra(string, state) when is_binary(string) do
    {inspect(string) |> string(), state}
  end

  def raw_section_element_to_algebra(atom, state) when is_atom(atom) do
    {(":" <> Atom.to_string(atom)) |> string(), state}
  end

  def raw_section_element_to_algebra(list, state) when is_list(list) do
    {docs, state} =
      Enum.reduce(list, {[], state}, fn arg, {acc, state} ->
        {doc, state} = raw_section_element_to_algebra(arg, state)
        {[doc | acc], state}
      end)

    args_doc = docs |> Enum.reverse() |> Enum.reduce(&concat(&2, concat(" ", &1)))
    {concat(["[", args_doc, "]"]), state}
  end

  def raw_section_element_to_algebra(other, state) do
    # Fallback to regular quoted_to_algebra for complex expressions
    Code.Formatter.delegate_to_raw_section(
      :quoted_to_algebra,
      {other, :sequence_element, state},
      nil
    )
  end

  ## Line traversal helpers

  def traverse_line({:raw_section, meta, args}, {min, max}) do
    acc = extract_line_from_meta(meta, {min, max})
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
    Code.Formatter.delegate_to_raw_section(:traverse_line, {other, acc}, nil)
  end

  def extract_line_from_meta(meta, {min, max}) when is_list(meta) do
    case :lists.keyfind(:line, 1, meta) do
      {:line, line} -> {min(line, min), max(line, max)}
      false -> {min, max}
    end
  end

  def extract_line_from_meta(_, acc), do: acc
end
