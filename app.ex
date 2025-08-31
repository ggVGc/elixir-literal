defmodule App do
  @moduledoc """
  Main application demonstrating reader macro usage across modules.
  """

  def main do
    IO.puts("🚀 Reader Macro Application Demo")
    IO.puts("=" |> String.duplicate(40))
    
    # Load and test the math macros module
    MathMacros.test()
    
    # Run the calculator with reader macros
    Calculator.calculate()
    Calculator.advanced_usage()
    
    IO.puts("\n✅ Reader macro demo complete!")
  end
end

# Run the application
App.main()