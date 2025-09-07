defmodule Mix.Tasks.Lipex.Calculator do
  @moduledoc """
  Runs the Lipex Calculator demo application.

  This interactive calculator demonstrates practical Lipex usage, including:
  - Arithmetic operations with prefix notation
  - Variable assignment and reuse
  - Built-in mathematical functions
  - Expression history and error handling

  ## Usage

      mix lipex.calculator

  ## Examples

  Once started, you can use expressions like:

      > (+ 2 3)                    # Basic arithmetic
      > (= x 42)                   # Variable assignment  
      > (* x 2)                    # Using variables
      > (square 5)                 # Built-in functions
      > (+ (* 2 3) (/ 8 4))        # Complex expressions

  Use 'help' for detailed instructions, 'quit' to exit.
  """

  @shortdoc "Runs the interactive Lipex calculator demo"

  use Mix.Task

  @impl Mix.Task
  def run(_args) do
    Mix.Task.run("app.start")
    Lipex.Examples.CalculatorApp.start()
  end
end
