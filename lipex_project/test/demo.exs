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
  deflipex ~~((def square (x)
    (* x x)))

  deflipex ~~((def cube (x) (* x x x)))
  deflipex ~~((def add (a b) (+ a b)))
  
  def run_demo do
    IO.puts("🌟 ===== LIPEX WORKING DEMO ===== 🌟")
    IO.puts("")
    
    # Function Definitions
    IO.puts("📝 FUNCTION DEFINITIONS:")
    IO.puts("  Defined: (def square (x) (* x x))")
    IO.puts("  Defined: (def cube (x) (* x x x))")
    IO.puts("  Defined: (def add (a b) (+ a b))")
    IO.puts("  square(5) = #{square(5)}")
    IO.puts("  cube(3) = #{cube(3)}")
    IO.puts("  add(10, 15) = #{add(10, 15)}")
    IO.puts("")
    
    # Arithmetic
    IO.puts("🔢 ARITHMETIC:")
    IO.puts("  (+ 1 2 3 4 5) = #{deflipex ~~((+ 1 2 3 4 5))}")
    IO.puts("  (* 2 3 4) = #{deflipex ~~((* 2 3 4))}")
    IO.puts("  (- 100 30) = #{deflipex ~~((- 100 30))}")
    IO.puts("  (/ 20 4) = #{deflipex ~~((/ 20 4))}")
    IO.puts("  (< 5 10) = #{deflipex ~~((< 5 10))}")
    IO.puts("  (>= 7 7) = #{deflipex ~~((>= 7 7))}")
    IO.puts("")
    
    # Data Structures
    IO.puts("🏗️ DATA STRUCTURES:")
    user_map = deflipex ~~(% :name "Alice" :age 28 :city "Portland")
    IO.puts("  Map: #{inspect(user_map)}")
    
    coordinates = deflipex ~~((tuple 45.5152 -122.6784))
    IO.puts("  Tuple: #{inspect(coordinates)}")
    
    shopping_list = deflipex ~~((list :apples :bananas :coffee :tea))
    IO.puts("  List: #{inspect(shopping_list)}")
    
    # Bracket syntax for lists (syntax sugar for (list ...))
    bracket_list = deflipex ~~([1 2 3 4 5])
    IO.puts("  List with brackets: #{inspect(bracket_list)}")
    
    nested_list = deflipex ~~([[:a :b] [:c :d] [:e :f]])
    IO.puts("  Nested lists: #{inspect(nested_list)}")
    
    settings = deflipex ~~((kwlist :timeout 3000 :retries 5))
    IO.puts("  Keyword list: #{inspect(settings)}")
    IO.puts("")
    
    # Logic
    IO.puts("🧠 LOGIC:")
    IO.puts("  (and true true false) = #{deflipex ~~((and true true false))}")
    IO.puts("  (or false true false) = #{deflipex ~~((or false true false))}")
    IO.puts("  (not false) = #{deflipex ~~((not false))}")
    IO.puts("  (atom? :hello) = #{deflipex ~~((atom? :hello))}")
    IO.puts("  (number? 42) = #{deflipex ~~((number? 42))}")
    IO.puts("  (string? \"world\") = #{deflipex ~~((string? "world"))}")
    IO.puts("")
    
    # Pattern Matching
    IO.puts("🎯 PATTERN MATCHING:")
    
    # Simple variable assignment
    deflipex ~~((= x 42))
    IO.puts("  (= x 42) assigns x = #{x}")
    
    # Tuple destructuring
    deflipex ~~((= {a b} (tuple 10 20)))
    IO.puts("  (= {a b} (tuple 10 20)) assigns a = #{a}, b = #{b}")
    
    # List destructuring with brackets
    deflipex ~~((= [first second | rest] [1 2 3 4 5]))
    IO.puts("  (= [first second | rest] [1 2 3 4 5]) gives first = #{first}, second = #{second}, rest = #{inspect(rest)}")
    
    # Note: Map and nested patterns need simpler syntax due to AST parsing
    IO.puts("  [Map destructuring examples temporarily simplified for initial demo]")
    
    IO.puts("")
    
    # Function Calls
    IO.puts("🔧 FUNCTION CALLS:")
    
    # Simple module function calls
    IO.puts("  Simple function calls:")
    deflipex ~~((IO.puts "  -> Hello from Lipex function call!"))
    upcase_result = deflipex ~~((String.upcase "hello world"))
    IO.puts("  (String.upcase \"hello world\") = #{upcase_result}")
    
    count_result = deflipex ~~((Enum.count [1 2 3 4 5]))
    IO.puts("  (Enum.count [1 2 3 4 5]) = #{count_result}")
    
    # Nested function calls
    IO.puts("  Nested function calls:")
    deflipex ~~((IO.puts (String.upcase "  -> nested calls work perfectly!")))
    
    # More function calls with different modules
    int_to_string = deflipex ~~((Integer.to_string 42))
    IO.puts("  (Integer.to_string 42) = #{int_to_string}")
    
    # String manipulation
    join_result = deflipex ~~((Enum.join ["hello" "lipex" "world"] " "))
    IO.puts("  (Enum.join [\"hello\" \"lipex\" \"world\"] \" \") = #{join_result}")
    
    IO.puts("")
    
    # Control Flow
    IO.puts("🚦 CONTROL FLOW:")
    IO.puts("  (if true :success :failure) = #{deflipex ~~((if true :success :failure))}")
    IO.puts("  (if false :success :failure) = #{deflipex ~~((if false :success :failure))}")
    condition_result = deflipex ~~((if (> 10 5) :bigger :smaller))
    IO.puts("  (if (> 10 5) :bigger :smaller) = #{condition_result}")
    IO.puts("")
    
    # Complex Nested Expressions
    IO.puts("🏢 NESTED EXPRESSIONS:")
    complex_math = deflipex ~~((+ (* 2 5) (- 20 8) (/ 24 6)))
    IO.puts("  Complex math: (+ (* 2 5) (- 20 8) (/ 24 6)) = #{complex_math}")
    
    nested_logic = deflipex ~~((and (> 10 5) (< 3 7) (number? 42)))
    IO.puts("  Nested logic: (and (> 10 5) (< 3 7) (number? 42)) = #{nested_logic}")
    
    nested_data = deflipex ~~((% :user (% :name "Bob" :score (+ 85 10)) :active true))
    IO.puts("  Nested data:")
    IO.puts("    #{inspect(nested_data, pretty: true)}")
    IO.puts("")
    
    # Real-world Examples
    IO.puts("🌍 REAL-WORLD EXAMPLES:")
    
    # Configuration-like structure
    server_config = deflipex ~~((% :host "api.example.com" :port 443 :ssl true))
    features = deflipex ~~((list :auth :logging :caching))
    timeout_calc = deflipex ~~((+ 5000 (* 2 1000)))
    IO.puts("  Server config: #{inspect(server_config)}")
    IO.puts("  Features: #{inspect(features)}")
    IO.puts("  Calculated timeout: #{timeout_calc}ms")
    
    # Mathematical formula
    formula_result = deflipex ~~((+ (* 3 (* 4 4)) (* (- 2) 4) 7))
    IO.puts("  Formula 3x² - 2x + 7 at x=4: #{formula_result}")
    
    IO.puts("")
    IO.puts("🎉 LIPEX IS WORKING PERFECTLY! 🎉")
    IO.puts("✨ Elixir-like Lisp syntax with modular architecture ✨")
  end
end

LipexDemo.run_demo()
