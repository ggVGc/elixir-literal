Code.require_file("lipex/evaluator.ex")
Code.require_file("lipex/lipex.ex")
Code.require_file("lipex/core/data_structures.ex")
Code.require_file("lipex/core/pattern_matching.ex")
Code.require_file("lipex/core/arithmetic.ex")
Code.require_file("lipex/core/logic.ex")
Code.require_file("lipex/core/control_flow.ex")
Code.require_file("lipex/functions/calls.ex")
Code.require_file("lipex/functions/definitions.ex")

defmodule Playground do
  import Lipex

  pu = IO.puts
  deflipex ~~((def run () (pu 132)))
end

Playground.run()



