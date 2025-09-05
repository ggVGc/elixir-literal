#!/usr/bin/env elixir

# Require all lipex modules
Code.require_file("lipex/evaluator.ex")
Code.require_file("lipex/lipex.ex")
Code.require_file("lipex/strings/interpolation.ex")
Code.require_file("lipex/core/data_structures.ex")
Code.require_file("lipex/core/pattern_matching.ex")
Code.require_file("lipex/core/arithmetic.ex")
Code.require_file("lipex/core/logic.ex")
Code.require_file("lipex/core/control_flow.ex")
Code.require_file("lipex/functions/calls.ex")
Code.require_file("lipex/functions/definitions.ex")

defmodule LipexDemo do
  import Lipex

  def run do
    deflipex ~~(
      (= x 123)
      (IO.puts "x: #{(+ 3 4)}"))
  end
end

LipexDemo.run()
