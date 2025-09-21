defmodule Module.Types.Literal do
  import Module.Types.Descr

  # Handle 4-tuple raw_block
  def of_expr({:raw_block, _meta, _bracket_type, _args}, _expected, _expr, _stack, context) do
    {dynamic(), context}
  end

  # Handle 3-tuple raw_section
  def of_expr({:raw_section, _meta, _args}, _expected, _expr, _stack, context) do
    {dynamic(), context}
  end
end
