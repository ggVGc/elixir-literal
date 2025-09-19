defmodule Module.Types.Literal do
  import Module.Types.Descr

  def of_expr({tag, _meta, _bracket_type, _args}, _expected, _expr, _stack, context)
      when tag in [
             :raw_block,
             :raw_section,
             :raw_paren,
             :raw_brace,
             :raw_bracket
           ] do
    {dynamic(), context}
  end
end
