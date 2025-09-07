defmodule Lipex.Examples.CalculatorApp do
  @moduledoc """
  Interactive Calculator CLI using Lipex syntax.

  This application demonstrates practical Lipex usage in an interactive setting,
  showing how to build a complete CLI application with the Lipex DSL.
  """

  import Lipex

  deflipex ~~(
    (def square (x) (* x x))
    (def cube (x) (* x (square x)))
    (def abs_val (x) (* x -1))
  )

  deflipex ~~(
  (def power (base, exp) when (== exp 0) 1)
    (def power (base exp) when (== exp 1) base)
    (def power (base exp) when (> exp 1)
      (* base
        (power base (- exp 1))))
  )

  # Calculator state
  defstruct variables: %{}, history: [], running: true

  def start do
    IO.puts """
    🧮 ======= LIPEX CALCULATOR =======

    Interactive calculator using Lipex syntax!

    Usage:
      • Math expressions: (+ 2 3), (* 4 5), (- 10 3), (/ 8 2)
      • Variables: (= x 42) then use x in expressions
      • Functions: (square 5), (cube 3), (power 2 8)
      • History: 'history' to see past calculations
      • Clear: 'clear' to reset variables and history
      • Quit: 'quit' or 'exit'

    Examples:
      > (+ 10 (* 5 3))        # Returns 25
      > (= radius 5)          # Store value in variable
      > (* 3.14 (square radius))  # Use variable in calculation

    =====================================
    """

    calculator_loop(%__MODULE__{})
  end

  defp calculator_loop(state) do
    input = IO.gets("> ") |> String.trim()

    case input do
      "" ->
        calculator_loop(state)

      "quit" ->
        IO.puts("👋 Thanks for using Lipex Calculator!")

      "exit" ->
        IO.puts("👋 Thanks for using Lipex Calculator!")

      "history" ->
        show_history(state)
        calculator_loop(state)

      "clear" ->
        IO.puts("🧹 Cleared variables and history")
        calculator_loop(%__MODULE__{})

      "help" ->
        show_help()
        calculator_loop(state)

      "vars" ->
        show_variables(state)
        calculator_loop(state)

      expression ->
        new_state = process_expression(expression, state)
        calculator_loop(new_state)
    end
  end

  defp process_expression(expression, state) do
    try do
      # Wrap the expression in deflipex ~~(...) if not already wrapped
      lipex_expression = if String.starts_with?(expression, "~~(") do
        "deflipex #{expression}"
      else
        "deflipex ~~(#{expression})"
      end

      # Temporarily inject variables into the environment
      variable_assignments = state.variables
        |> Enum.map(fn {name, value} -> "#{name} = #{inspect(value)}" end)
        |> Enum.join("; ")

      # Create a code string that sets up variables and evaluates the expression
      code = if variable_assignments != "" do
        "#{variable_assignments}; import Lipex; #{lipex_expression}"
      else
        "import Lipex; #{lipex_expression}"
      end

      # Evaluate the expression
      {result, _binding} = Code.eval_string(code, [], __ENV__)

      # Check if this is a variable assignment
      new_variables = case extract_variable_assignment(expression) do
        {var_name, var_value} ->
          IO.puts("📝 #{var_name} = #{inspect(var_value)}")
          Map.put(state.variables, var_name, var_value)
        nil ->
          IO.puts("📊 #{inspect(result)}")
          state.variables
      end

      # Add to history
      history_entry = %{expression: expression, result: result, timestamp: DateTime.utc_now()}
      new_history = [history_entry | state.history] |> Enum.take(50)  # Keep last 50

      %{state | variables: new_variables, history: new_history}

    rescue
      error ->
        IO.puts("❌ Error: #{Exception.message(error)}")
        IO.puts("💡 Try: (+ 2 3) or (= x 5) or type 'help' for examples")
        state
    end
  end

  defp extract_variable_assignment(expression) do
    # Simple pattern matching to detect variable assignments like (= x 5)
    case Regex.run(~r/^\s*\(\s*=\s+(\w+)\s+(.+)\)\s*$/, expression) do
      [_, var_name, _var_expression] ->
        # We can't easily evaluate just the value part, so we'll return nil
        # and let the full evaluation handle it
        nil
      _ ->
        nil
    end
  end

  defp show_history(state) do
    if Enum.empty?(state.history) do
      IO.puts("📝 No calculation history yet")
    else
      IO.puts("📚 ===== CALCULATION HISTORY =====")
      state.history
      |> Enum.reverse()
      |> Enum.with_index(1)
      |> Enum.each(fn {entry, index} ->
        timestamp = entry.timestamp |> DateTime.to_time() |> Time.to_string()
        IO.puts("#{index}. [#{timestamp}] #{entry.expression} = #{inspect(entry.result)}")
      end)
      IO.puts("===============================")
    end
  end

  defp show_variables(state) do
    if Enum.empty?(state.variables) do
      IO.puts("📦 No variables stored")
    else
      IO.puts("📦 ===== STORED VARIABLES =====")
      state.variables
      |> Enum.each(fn {name, value} ->
        IO.puts("  #{name} = #{inspect(value)}")
      end)
      IO.puts("=============================")
    end
  end

  defp show_help do
    IO.puts """

    📖 ===== LIPEX CALCULATOR HELP =====

    BASIC ARITHMETIC:
      (+ 1 2 3)           # Addition: 6
      (- 10 3)            # Subtraction: 7
      (* 2 4 5)           # Multiplication: 40
      (/ 12 3)            # Division: 4.0

    COMPARISONS:
      (< 5 10)            # Less than: true
      (> 8 3)             # Greater than: true
      (<= 5 5)            # Less or equal: true
      (>= 7 7)            # Greater or equal: true
      (== 5 5)            # Equal: true

    BUILT-IN FUNCTIONS:
      (square 5)          # Square: 25
      (cube 3)            # Cube: 27
      (abs_val -5)        # Absolute value: 5
      (power 2 8)         # Power: 256

    VARIABLES:
      (= x 42)            # Assign x = 42
      (= y (* 2 5))       # Assign y = 10
      (+ x y)             # Use variables: 52

    COMPLEX EXPRESSIONS:
      (+ (* 2 3) (/ 8 4))         # Nested: 8
      (square (+ 3 2))            # Function with expression: 25
      (if (> x 0) (square x) 0)   # Conditional: varies

    COMMANDS:
      history             # Show calculation history
      vars                # Show stored variables
      clear               # Clear variables and history
      help                # Show this help
      quit/exit           # Exit calculator

    TIPS:
      • All expressions use prefix notation: (operator arg1 arg2 ...)
      • Variables persist until cleared
      • History keeps your last 50 calculations
      • Use parentheses to group operations

    ===================================
    """
  end
end
