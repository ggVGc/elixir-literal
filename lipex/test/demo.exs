#!/usr/bin/env elixir

# Require all lipex modules
Code.require_file("lipex/evaluator.ex")
Code.require_file("lipex/lipex.ex")
Code.require_file("lipex/core/data_structures.ex")
Code.require_file("lipex/core/pattern_matching.ex")
Code.require_file("lipex/core/arithmetic.ex")
Code.require_file("lipex/core/logic.ex")
Code.require_file("lipex/core/control_flow.ex")
Code.require_file("lipex/functions/calls.ex")
Code.require_file("lipex/functions/definitions.ex")

defmodule LipexDemo do
  import Lipex
  
  # Define functions using Lipex syntax
  deflipex ~~(
    (def square (x) (* x x)))

  deflipex ~~((def cube (x) (* x x x)))
  deflipex ~~((def add (a b) (+ a b)))
  
  def run_demo do
    deflipex ~~(
      (= y (square 12))
      (IO.puts "y"))
  end
end

LipexDemo.run_demo()
