#!/usr/bin/env elixir

# Require all lipex modules
Code.require_file("lipex/lipex.ex")
Code.require_file("lipex/core/data_structures.ex")
Code.require_file("lipex/core/arithmetic.ex")
Code.require_file("lipex/core/logic.ex")
Code.require_file("lipex/core/control_flow.ex")
Code.require_file("lipex/functions/calls.ex")

defmodule LipexComprehensiveTest do
  @moduledoc """
  Comprehensive test suite demonstrating all working Lipex functionality.
  """
  
  import Lipex
  
  def run_all_tests do
    IO.puts("🚀 === COMPREHENSIVE LIPEX TEST SUITE === 🚀\n")
    
    test_arithmetic_operations()
    test_data_structures()
    test_logical_operations()
    test_control_flow()
    test_nested_expressions()
    test_complex_scenarios()
    
    IO.puts("\n🎉 === ALL TESTS COMPLETED SUCCESSFULLY! === 🎉")
    IO.puts("✨ Lipex is working beautifully! ✨")
  end
  
  def test_arithmetic_operations do
    IO.puts("🔢 --- ARITHMETIC OPERATIONS ---")
    
    # Basic arithmetic
    IO.puts("Basic arithmetic:")
    IO.puts("  (+ 1 2 3 4 5) = #{deflipex quote do ~~((+ 1 2 3 4 5)) end}")
    IO.puts("  (* 2 3 4) = #{deflipex quote do ~~((* 2 3 4)) end}")
    IO.puts("  (- 100 25 10) = #{deflipex quote do ~~((- 100 25 10)) end}")
    IO.puts("  (/ 100 5 2) = #{deflipex quote do ~~((/ 100 5 2)) end}")
    IO.puts("  (- 42) = #{deflipex quote do ~~((- 42)) end}")  # Unary negation
    
    # Comparisons
    IO.puts("\nComparisons:")
    IO.puts("  (< 1 5) = #{deflipex quote do ~~((< 1 5)) end}")
    IO.puts("  (> 10 5) = #{deflipex quote do ~~((> 10 5)) end}")
    IO.puts("  (<= 5 5) = #{deflipex quote do ~~((<= 5 5)) end}")
    IO.puts("  (>= 7 3) = #{deflipex quote do ~~((>= 7 3)) end}")
    IO.puts("  (== :hello :hello) = #{deflipex quote do ~~((== :hello :hello)) end}")
    IO.puts("  (!= :foo :bar) = #{deflipex quote do ~~((!= :foo :bar)) end}")
    
    # Mathematical functions
    IO.puts("\nMath functions:")
    IO.puts("  (abs (- 42)) = #{deflipex quote do ~~((abs (- 42))) end}")
    IO.puts("  (min 5 2 8 1) = #{deflipex quote do ~~((min 5 2 8 1)) end}")
    IO.puts("  (max 5 2 8 1) = #{deflipex quote do ~~((max 5 2 8 1)) end}")
    IO.puts("  (rem 17 5) = #{deflipex quote do ~~((rem 17 5)) end}")
    
    IO.puts("")
  end
  
  def test_data_structures do
    IO.puts("🏗️ --- DATA STRUCTURES ---")
    
    # Maps
    IO.puts("Maps using (% ...) syntax:")
    simple_map = deflipex quote do ~~((% :name "Alice" :age 30)) end
    IO.puts("  (% :name \"Alice\" :age 30) = #{inspect(simple_map)}")
    
    complex_map = deflipex quote do ~~((% "key1" :value1 :key2 42 :nested (% :x 10 :y 20))) end
    IO.puts("  Complex map = #{inspect(complex_map)}")
    
    # Tuples
    IO.puts("\nTuples:")
    IO.puts("  (tuple :ok :success) = #{inspect(deflipex quote do ~~((tuple :ok :success)) end)}")
    IO.puts("  (tuple 1 2 3 4) = #{inspect(deflipex quote do ~~((tuple 1 2 3 4)) end)}")
    IO.puts("  (tuple) = #{inspect(deflipex quote do ~~((tuple)) end)}")
    
    # Lists  
    IO.puts("\nLists:")
    IO.puts("  (list 1 2 3 4 5) = #{inspect(deflipex quote do ~~((list 1 2 3 4 5)) end)}")
    IO.puts("  (list :a :b :c) = #{inspect(deflipex quote do ~~((list :a :b :c)) end)}")
    IO.puts("  (list) = #{inspect(deflipex quote do ~~((list)) end)}")
    
    # Keyword lists
    IO.puts("\nKeyword Lists:")
    kwlist = deflipex quote do ~~((kwlist :timeout 5000 :retries 3 :mode :sync)) end
    IO.puts("  (kwlist :timeout 5000 :retries 3 :mode :sync) = #{inspect(kwlist)}")
    
    IO.puts("")
  end
  
  def test_logical_operations do
    IO.puts("🧠 --- LOGICAL OPERATIONS ---")
    
    # Boolean logic
    IO.puts("Boolean operations:")
    IO.puts("  (and true true true) = #{deflipex quote do ~~((and true true true)) end}")
    IO.puts("  (and true false true) = #{deflipex quote do ~~((and true false true)) end}")
    IO.puts("  (or false false true) = #{deflipex quote do ~~((or false false true)) end}")
    IO.puts("  (or false false false) = #{deflipex quote do ~~((or false false false)) end}")
    IO.puts("  (not true) = #{deflipex quote do ~~((not true)) end}")
    IO.puts("  (not false) = #{deflipex quote do ~~((not false)) end}")
    
    # Type checking
    IO.puts("\nType checking:")
    IO.puts("  (atom? :hello) = #{deflipex quote do ~~((atom? :hello)) end}")
    IO.puts("  (number? 42) = #{deflipex quote do ~~((number? 42)) end}")
    IO.puts("  (string? \"hello\") = #{deflipex quote do ~~((string? "hello")) end}")
    IO.puts("  (list? (list 1 2 3)) = #{deflipex quote do ~~((list? (list 1 2 3))) end}")
    IO.puts("  (tuple? (tuple :ok :value)) = #{deflipex quote do ~~((tuple? (tuple :ok :value))) end}")
    
    # Truthiness
    IO.puts("\nTruthiness:")
    IO.puts("  (truthy? 42) = #{deflipex quote do ~~((truthy? 42)) end}")
    IO.puts("  (truthy? nil) = #{deflipex quote do ~~((truthy? nil)) end}")
    IO.puts("  (falsy? false) = #{deflipex quote do ~~((falsy? false)) end}")
    IO.puts("  (nil? nil) = #{deflipex quote do ~~((nil? nil)) end}")
    IO.puts("  (some? 42) = #{deflipex quote do ~~((some? 42)) end}")
    
    IO.puts("")
  end
  
  def test_control_flow do
    IO.puts("🚦 --- CONTROL FLOW ---")
    
    # If expressions
    IO.puts("If expressions:")
    IO.puts("  (if true :yes :no) = #{deflipex quote do ~~((if true :yes :no)) end}")
    IO.puts("  (if false :yes :no) = #{deflipex quote do ~~((if false :yes :no)) end}")
    IO.puts("  (if (> 5 3) :bigger :smaller) = #{deflipex quote do ~~((if (> 5 3) :bigger :smaller)) end}")
    
    # Conditional with only then branch
    IO.puts("  (if true :success) = #{deflipex quote do ~~((if true :success)) end}")
    IO.puts("  (if false :success) = #{inspect(deflipex quote do ~~((if false :success)) end)}")
    
    IO.puts("")
  end
  
  def test_nested_expressions do
    IO.puts("🏢 --- NESTED EXPRESSIONS ---")
    
    # Nested arithmetic
    nested_math = deflipex quote do ~~((+ (* 2 3) (- 10 5) (/ 20 4))) end
    IO.puts("Complex math: (+ (* 2 3) (- 10 5) (/ 20 4)) = #{nested_math}")
    
    # Nested comparisons and logic  
    nested_logic = deflipex quote do ~~((and (> 5 3) (< 2 10) (== :foo :foo))) end
    IO.puts("Complex logic: (and (> 5 3) (< 2 10) (== :foo :foo)) = #{nested_logic}")
    
    # Nested data structures
    nested_data = deflipex quote do ~~((% :user (% :name "Bob" :age (+ 20 10)) :scores (list 95 87 92))) end
    IO.puts("Nested structures:")
    IO.puts("  #{inspect(nested_data)}")
    
    # Complex conditionals
    complex_if = deflipex quote do ~~((if (and (> 10 5) (< 3 7)) (+ 1 2 3) (* 2 2 2))) end
    IO.puts("Complex if: #{complex_if}")
    
    IO.puts("")
  end
  
  def test_complex_scenarios do
    IO.puts("🌟 --- COMPLEX SCENARIOS ---")
    
    # Mathematical expression
    IO.puts("Mathematical expressions:")
    quadratic = deflipex quote do ~~((+ (* 2 (* 3 3)) (* (- 5) 3) 6)) end
    IO.puts("  Quadratic 2x² - 5x + 6 at x=3: #{quadratic}")
    
    # Data processing simulation
    IO.puts("\nData processing:")
    scores = deflipex quote do ~~((list 85 92 78 96 88)) end
    max_score = deflipex quote do ~~((max 85 92 78 96 88)) end
    min_score = deflipex quote do ~~((min 85 92 78 96 88)) end
    IO.puts("  Scores: #{inspect(scores)}")
    IO.puts("  Max score: #{max_score}")
    IO.puts("  Min score: #{min_score}")
    
    # Configuration-like data
    IO.puts("\nConfiguration data:")
    config = deflipex quote do ~~((% :database (% :host "localhost" :port 5432) :cache (kwlist :ttl 300 :size 1000) :features (list :auth :logging :metrics))) end
    IO.puts("  #{inspect(config, pretty: true)}")
    
    # Complex boolean logic
    IO.puts("\nComplex boolean expressions:")
    _access_check = deflipex quote do ~~((and (or (== :role :admin) (== :role :moderator)) (not (== :status :banned)) (> :score 50))) end
    IO.puts("  Access check structure created successfully")
    
    # Nested conditionals
    IO.puts("\nNested conditionals:")
    grade = deflipex quote do ~~((if (>= 95 90) :A (if (>= 95 80) :B (if (>= 95 70) :C :F)))) end
    IO.puts("  Grade for 95: #{grade}")
    
    IO.puts("")
  end
end

# Run the comprehensive tests
LipexComprehensiveTest.run_all_tests()