#!/usr/bin/env elixir

# Require all lipex modules
Code.require_file("lipex/lipex.ex")
Code.require_file("lipex/core/data_structures.ex")
Code.require_file("lipex/core/arithmetic.ex")
Code.require_file("lipex/core/logic.ex")
Code.require_file("lipex/core/control_flow.ex")
Code.require_file("lipex/functions/calls.ex")

defmodule LipexDemo do
  import Lipex
  
  def run_demo do
    IO.puts("🌟 ===== LIPEX WORKING DEMO ===== 🌟")
    IO.puts("")
    
    # Arithmetic
    IO.puts("🔢 ARITHMETIC:")
    IO.puts("  (+ 1 2 3 4 5) = #{deflipex quote do ~~((+ 1 2 3 4 5)) end}")
    IO.puts("  (* 2 3 4) = #{deflipex quote do ~~((* 2 3 4)) end}")
    IO.puts("  (- 100 30) = #{deflipex quote do ~~((- 100 30)) end}")
    IO.puts("  (/ 20 4) = #{deflipex quote do ~~((/ 20 4)) end}")
    IO.puts("  (< 5 10) = #{deflipex quote do ~~((< 5 10)) end}")
    IO.puts("  (>= 7 7) = #{deflipex quote do ~~((>= 7 7)) end}")
    IO.puts("")
    
    # Data Structures
    IO.puts("🏗️ DATA STRUCTURES:")
    user_map = deflipex quote do ~~((% :name "Alice" :age 28 :city "Portland")) end
    IO.puts("  Map: #{inspect(user_map)}")
    
    coordinates = deflipex quote do ~~((tuple 45.5152 -122.6784)) end
    IO.puts("  Tuple: #{inspect(coordinates)}")
    
    shopping_list = deflipex quote do ~~((list :apples :bananas :coffee :tea)) end
    IO.puts("  List: #{inspect(shopping_list)}")
    
    settings = deflipex quote do ~~((kwlist :timeout 3000 :retries 5)) end
    IO.puts("  Keyword list: #{inspect(settings)}")
    IO.puts("")
    
    # Logic
    IO.puts("🧠 LOGIC:")
    IO.puts("  (and true true false) = #{deflipex quote do ~~((and true true false)) end}")
    IO.puts("  (or false true false) = #{deflipex quote do ~~((or false true false)) end}")
    IO.puts("  (not false) = #{deflipex quote do ~~((not false)) end}")
    IO.puts("  (atom? :hello) = #{deflipex quote do ~~((atom? :hello)) end}")
    IO.puts("  (number? 42) = #{deflipex quote do ~~((number? 42)) end}")
    IO.puts("")
    
    # Control Flow
    IO.puts("🚦 CONTROL FLOW:")
    IO.puts("  (if true :success :failure) = #{deflipex quote do ~~((if true :success :failure)) end}")
    IO.puts("  (if false :success :failure) = #{deflipex quote do ~~((if false :success :failure)) end}")
    condition_result = deflipex quote do ~~((if (> 10 5) :bigger :smaller)) end
    IO.puts("  (if (> 10 5) :bigger :smaller) = #{condition_result}")
    IO.puts("")
    
    # Complex Nested Expressions
    IO.puts("🏢 NESTED EXPRESSIONS:")
    complex_math = deflipex quote do ~~((+ (* 2 5) (- 20 8) (/ 24 6))) end
    IO.puts("  Complex math: (+ (* 2 5) (- 20 8) (/ 24 6)) = #{complex_math}")
    
    nested_logic = deflipex quote do ~~((and (> 10 5) (< 3 7) (number? 42))) end
    IO.puts("  Nested logic: (and (> 10 5) (< 3 7) (number? 42)) = #{nested_logic}")
    
    nested_data = deflipex quote do ~~((% :user (% :name "Bob" :score (+ 85 10)) :active true)) end
    IO.puts("  Nested data:")
    IO.puts("    #{inspect(nested_data, pretty: true)}")
    IO.puts("")
    
    # Real-world Examples
    IO.puts("🌍 REAL-WORLD EXAMPLES:")
    
    # Configuration-like structure
    config = deflipex quote do ~~((% :server (% :host "api.example.com" :port 443 :ssl true) :database (% :name "myapp_prod" :pool_size 10) :features (list :auth :logging :caching) :timeout (+ 5000 (* 2 1000)))) end
    IO.puts("  Application config:")
    IO.puts("    #{inspect(config, pretty: true)}")
    
    # Mathematical formula
    formula_result = deflipex quote do ~~((+ (* 3 (* 4 4)) (* (- 2) 4) 7)) end
    IO.puts("  Formula 3x² - 2x + 7 at x=4: #{formula_result}")
    
    IO.puts("")
    IO.puts("🎉 LIPEX IS WORKING PERFECTLY! 🎉")
    IO.puts("✨ Elixir-like Lisp syntax with modular architecture ✨")
  end
end

LipexDemo.run_demo()