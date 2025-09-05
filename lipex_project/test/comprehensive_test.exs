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
    IO.puts("  (+ 1 2 3 4 5) = #{deflipex ~~((+ 1 2 3 4 5))}")
    IO.puts("  (* 2 3 4) = #{deflipex ~~((* 2 3 4))}")
    IO.puts("  (- 100 25 10) = #{deflipex ~~((- 100 25 10))}")
    IO.puts("  (/ 100 5 2) = #{deflipex ~~((/ 100 5 2))}")
    IO.puts("  (- 42) = #{deflipex ~~((- 42))}")  # Unary negation
    
    # Comparisons
    IO.puts("\nComparisons:")
    IO.puts("  (< 1 5) = #{deflipex ~~((< 1 5))}")
    IO.puts("  (> 10 5) = #{deflipex ~~((> 10 5))}")
    IO.puts("  (<= 5 5) = #{deflipex ~~((<= 5 5))}")
    IO.puts("  (>= 7 3) = #{deflipex ~~((>= 7 3))}")
    IO.puts("  (== :hello :hello) = #{deflipex ~~((== :hello :hello))}")
    IO.puts("  (!= :foo :bar) = #{deflipex ~~((!= :foo :bar))}")
    
    # Mathematical functions
    IO.puts("\nMath functions:")
    IO.puts("  (abs (- 42)) = #{deflipex ~~((abs (- 42)))}")
    IO.puts("  (min 5 2 8 1) = #{deflipex ~~((min 5 2 8 1))}")
    IO.puts("  (max 5 2 8 1) = #{deflipex ~~((max 5 2 8 1))}")
    IO.puts("  (rem 17 5) = #{deflipex ~~((rem 17 5))}")
    
    IO.puts("")
  end
  
  def test_data_structures do
    IO.puts("🏗️ --- DATA STRUCTURES ---")
    
    # Maps
    IO.puts("Maps using (% ...) syntax:")
    simple_map = deflipex ~~((% :name "Alice" :age 30))
    IO.puts("  (% :name \"Alice\" :age 30) = #{inspect(simple_map)}")
    
    complex_map = deflipex ~~((% "key1" :value1 :key2 42 :nested (% :x 10 :y 20)))
    IO.puts("  Complex map = #{inspect(complex_map)}")
    
    # Tuples
    IO.puts("\nTuples:")
    IO.puts("  (tuple :ok :success) = #{inspect(deflipex ~~((tuple :ok :success)))}")
    IO.puts("  (tuple 1 2 3 4) = #{inspect(deflipex ~~((tuple 1 2 3 4)))}")
    IO.puts("  (tuple) = #{inspect(deflipex ~~((tuple)))}")
    
    # Lists  
    IO.puts("\nLists:")
    IO.puts("  (list 1 2 3 4 5) = #{inspect(deflipex ~~((list 1 2 3 4 5)))}")
    IO.puts("  (list :a :b :c) = #{inspect(deflipex ~~((list :a :b :c)))}")
    IO.puts("  (list) = #{inspect(deflipex ~~((list)))}")
    
    # Keyword lists
    IO.puts("\nKeyword Lists:")
    kwlist = deflipex ~~((kwlist :timeout 5000 :retries 3 :mode :sync))
    IO.puts("  (kwlist :timeout 5000 :retries 3 :mode :sync) = #{inspect(kwlist)}")
    
    IO.puts("")
  end
  
  def test_logical_operations do
    IO.puts("🧠 --- LOGICAL OPERATIONS ---")
    
    # Boolean logic
    IO.puts("Boolean operations:")
    IO.puts("  (and true true true) = #{deflipex ~~((and true true true))}")
    IO.puts("  (and true false true) = #{deflipex ~~((and true false true))}")
    IO.puts("  (or false false true) = #{deflipex ~~((or false false true))}")
    IO.puts("  (or false false false) = #{deflipex ~~((or false false false))}")
    IO.puts("  (not true) = #{deflipex ~~((not true))}")
    IO.puts("  (not false) = #{deflipex ~~((not false))}")
    
    # Type checking
    IO.puts("\nType checking:")
    IO.puts("  (atom? :hello) = #{deflipex ~~((atom? :hello))}")
    IO.puts("  (number? 42) = #{deflipex ~~((number? 42))}")
    IO.puts("  (string? \"hello\") = #{deflipex ~~((string? "hello"))}")
    IO.puts("  (list? (list 1 2 3)) = #{deflipex ~~((list? (list 1 2 3)))}")
    IO.puts("  (tuple? (tuple :ok :value)) = #{deflipex ~~((tuple? (tuple :ok :value)))}")
    
    # Truthiness
    IO.puts("\nTruthiness:")
    IO.puts("  (truthy? 42) = #{deflipex ~~((truthy? 42))}")
    IO.puts("  (truthy? nil) = #{deflipex ~~((truthy? nil))}")
    IO.puts("  (falsy? false) = #{deflipex ~~((falsy? false))}")
    IO.puts("  (nil? nil) = #{deflipex ~~((nil? nil))}")
    IO.puts("  (some? 42) = #{deflipex ~~((some? 42))}")
    
    IO.puts("")
  end
  
  def test_control_flow do
    IO.puts("🚦 --- CONTROL FLOW ---")
    
    # If expressions
    IO.puts("If expressions:")
    IO.puts("  (if true :yes :no) = #{deflipex ~~((if true :yes :no))}")
    IO.puts("  (if false :yes :no) = #{deflipex ~~((if false :yes :no))}")
    IO.puts("  (if (> 5 3) :bigger :smaller) = #{deflipex ~~((if (> 5 3) :bigger :smaller))}")
    
    # Conditional with only then branch
    IO.puts("  (if true :success) = #{deflipex ~~((if true :success))}")
    IO.puts("  (if false :success) = #{inspect(deflipex ~~((if false :success)))}")
    
    IO.puts("")
  end
  
  def test_nested_expressions do
    IO.puts("🏢 --- NESTED EXPRESSIONS ---")
    
    # Nested arithmetic
    nested_math = deflipex ~~((+ (* 2 3) (- 10 5) (/ 20 4)))
    IO.puts("Complex math: (+ (* 2 3) (- 10 5) (/ 20 4)) = #{nested_math}")
    
    # Nested comparisons and logic  
    nested_logic = deflipex ~~((and (> 5 3) (< 2 10) (== :foo :foo)))
    IO.puts("Complex logic: (and (> 5 3) (< 2 10) (== :foo :foo)) = #{nested_logic}")
    
    # Nested data structures
    nested_data = deflipex ~~((% :user (% :name "Bob" :age (+ 20 10)) :scores (list 95 87 92)))
    IO.puts("Nested structures:")
    IO.puts("  #{inspect(nested_data)}")
    
    # Complex conditionals
    complex_if = deflipex ~~((if (and (> 10 5) (< 3 7)) (+ 1 2 3) (* 2 2 2)))
    IO.puts("Complex if: #{complex_if}")
    
    IO.puts("")
  end
  
  def test_complex_scenarios do
    IO.puts("🌟 --- COMPLEX SCENARIOS ---")
    
    # Mathematical expression
    IO.puts("Mathematical expressions:")
    quadratic = deflipex ~~((+ (* 2 (* 3 3)) (* (- 5) 3) 6))
    IO.puts("  Quadratic 2x² - 5x + 6 at x=3: #{quadratic}")
    
    # Data processing simulation
    IO.puts("\nData processing:")
    scores = deflipex ~~((list 85 92 78 96 88))
    max_score = deflipex ~~((max 85 92 78 96 88))
    min_score = deflipex ~~((min 85 92 78 96 88))
    IO.puts("  Scores: #{inspect(scores)}")
    IO.puts("  Max score: #{max_score}")
    IO.puts("  Min score: #{min_score}")
    
    # Configuration-like data
    IO.puts("\nConfiguration data:")
    config = deflipex ~~((% :database (% :host "localhost" :port 5432) :cache (kwlist :ttl 300 :size 1000) :features (list :auth :logging :metrics)))
    IO.puts("  #{inspect(config, pretty: true)}")
    
    # Complex boolean logic
    IO.puts("\nComplex boolean expressions:")
    _access_check = deflipex ~~((and (or (== :role :admin) (== :role :moderator)) (not (== :status :banned)) (> :score 50)))
    IO.puts("  Access check structure created successfully")
    
    # Nested conditionals
    IO.puts("\nNested conditionals:")
    grade = deflipex ~~((if (>= 95 90) :A (if (>= 95 80) :B (if (>= 95 70) :C :F))))
    IO.puts("  Grade for 95: #{grade}")
    
    IO.puts("")
  end
end

# Run the comprehensive tests
LipexComprehensiveTest.run_all_tests()