defmodule Module.Types.Literal do
  import Module.Types.Descr

  def of_expr({tag, _meta, _bracket_type, _args}, _expected, _expr, _stack, context)
      when tag in [
             :sequence_block,
             :sequence_literal,
             :sequence_paren,
             :sequence_brace,
             :sequence_bracket
           ] do
    {dynamic(), context}
  end
end
