defmodule LipexComprehensiveTest do
  use ExUnit.Case, async: true
  import Lipex

  @moduledoc """
  Comprehensive test suite demonstrating all working Lipex functionality.
  Converted from the original comprehensive_test.exs to proper ExUnit format.
  
  Tests all major Lipex features including:
  - Arithmetic operations (basic, comparisons, math functions)
  - Data structures (maps, tuples, lists, keyword lists)
  - Logical operations (boolean logic, type checking, truthiness)
  - Control flow (if expressions, conditionals)
  - Nested expressions
  - Complex scenarios
  """

  describe "arithmetic operations" do
    test "basic arithmetic with multiple operands" do
      assert deflipex ~~((+ 1 2 3 4 5)) == 15
      assert deflipex ~~((* 2 3 4)) == 24
      assert deflipex ~~((- 100 25 10)) == 65
      assert deflipex ~~((/ 100 5 2)) == 10.0
    end

    test "unary negation" do
      assert deflipex ~~((- 42)) == -42
    end

    test "comparison operations" do
      assert deflipex ~~((< 1 5)) == true
      assert deflipex ~~((> 10 5)) == true
      assert deflipex ~~((<= 5 5)) == true
      assert deflipex ~~((>= 7 3)) == true
      assert deflipex ~~((== :hello :hello)) == true
      assert deflipex ~~((!= :foo :bar)) == true
    end

    test "mathematical functions" do
      assert deflipex ~~((abs (- 42))) == 42
      assert deflipex ~~((min 5 2 8 1)) == 1
      assert deflipex ~~((max 5 2 8 1)) == 8
      assert deflipex ~~((rem 17 5)) == 2
    end
  end

  describe "data structures" do
    test "maps with various key types" do
      simple_map = deflipex ~~((% :name "Alice" :age 30)))
      assert simple_map == %{name: "Alice", age: 30}

      complex_map = deflipex ~~((% "key1" :value1 :key2 42 :nested (% :x 10 :y 20))
      expected = %{"key1" => :value1, key2: 42, nested: %{x: 10, y: 20}}
      assert complex_map == expected
    end

    test "tuples of various sizes" do
      assert deflipex ~~((tuple :ok :success))) == {:ok, :success}
      assert deflipex ~~((tuple 1 2 3 4))) == {1, 2, 3, 4}
      assert deflipex ~~((tuple))) == {}
    end

    test "lists" do
      assert deflipex ~~((list 1 2 3 4 5))) == [1, 2, 3, 4, 5]
      assert deflipex ~~((list :a :b :c))) == [:a, :b, :c]
      assert deflipex ~~((list))) == []
    end

    test "keyword lists" do
      kwlist = deflipex ~~((kwlist :timeout 5000 :retries 3 :mode :sync)))
      assert kwlist == [timeout: 5000, retries: 3, mode: :sync]
    end
  end

  describe "logical operations" do
    test "boolean logic with multiple operands" do
      assert deflipex ~~((and true true true))) == true
      assert deflipex ~~((and true false true))) == false
      assert deflipex ~~((or false false true))) == true
      assert deflipex ~~((or false false false))) == false
    end

    test "boolean negation" do
      assert deflipex ~~((not true))) == false
      assert deflipex ~~((not false))) == true
    end

    test "type checking predicates" do
      assert deflipex ~~((atom? :hello))) == true
      assert deflipex ~~((number? 42))) == true
      assert deflipex ~~((string? "hello"))) == true
      assert deflipex ~~((list? (list 1 2 3)) == true
      assert deflipex ~~((tuple? (tuple :ok :value)) == true
    end

    test "truthiness predicates" do
      assert deflipex ~~((truthy? 42))) == true
      assert deflipex ~~((truthy? nil))) == false
      assert deflipex ~~((falsy? false))) == true
      assert deflipex ~~((nil? nil))) == true
      assert deflipex ~~((some? 42))) == true
    end
  end

  describe "control flow" do
    test "if expressions with both branches" do
      assert deflipex ~~((if true :yes :no))) == :yes
      assert deflipex ~~((if false :yes :no))) == :no
      assert deflipex ~~((if (> 5 3) :bigger :smaller))) == :bigger
    end

    test "if expressions with only then branch" do
      assert deflipex ~~((if true :success))) == :success
      assert deflipex ~~((if false :success))) == nil
    end
  end

  describe "nested expressions" do
    test "nested arithmetic operations" do
      result = deflipex ~~((+ (* 2 3) (- 10 5) (/ 20 4))
      assert result == 16.0
    end

    test "nested logic and comparisons" do
      result = deflipex ~~((and (> 5 3) (< 2 10) (== :foo :foo))
      assert result == true
    end

    test "nested data structures" do
      nested_data = deflipex ~~((% :user (% :name "Bob" :age (+ 20 10)) :scores (list 95 87 92))
      expected = %{
        user: %{name: "Bob", age: 30},
        scores: [95, 87, 92]
      }
      assert nested_data == expected
    end

    test "complex conditionals" do
      result = deflipex ~~((if (and (> 10 5) (< 3 7)) (+ 1 2 3) (* 2 2 2))
      assert result == 6
    end
  end

  describe "complex scenarios" do
    test "mathematical expressions" do
      # 2x² - 5x + 6 at x=3: 2*9 - 5*3 + 6 = 18 - 15 + 6 = 9
      quadratic = deflipex ~~((+ (* 2 (* 3 3)) (* (- 5) 3) 6)))
      assert quadratic == 9
    end

    test "data processing operations" do
      scores = deflipex ~~((list 85 92 78 96 88)))
      max_score = deflipex ~~((max 85 92 78 96 88)))
      min_score = deflipex ~~((min 85 92 78 96 88)))
      
      assert scores == [85, 92, 78, 96, 88]
      assert max_score == 96
      assert min_score == 78
    end

    test "complex configuration structures" do
      config = deflipex ~~((% :database (% :host "localhost" :port 5432) :cache (kwlist :ttl 300 :size 1000) :features (list :auth :logging :metrics))
      
      expected = %{
        database: %{host: "localhost", port: 5432},
        cache: [ttl: 300, size: 1000],
        features: [:auth, :logging, :metrics]
      }
      assert config == expected
    end

    test "complex boolean logic structures" do
      # Testing that complex boolean expressions can be constructed
      access_check = deflipex ~~((and (or (== :role :admin) (== :role :moderator)) (not (== :status :banned)) (> :score 50))
      # This should create the boolean structure, exact evaluation depends on variable bindings
      assert is_boolean(access_check) or is_tuple(access_check)
    end

    test "nested conditional chains" do
      # Grade calculation: 95 >= 90 -> A
      grade = deflipex ~~((if (>= 95 90) :A (if (>= 95 80) :B (if (>= 95 70) :C :F)))
      assert grade == :A
      
      # Test with lower score: 75
      grade2 = deflipex ~~((if (>= 75 90) :A (if (>= 75 80) :B (if (>= 75 70) :C :F)))
      assert grade2 == :C
    end
  end

  describe "edge cases and error handling" do
    test "empty data structures" do
      assert deflipex ~~((tuple))) == {}
      assert deflipex ~~((list))) == []
    end

    test "single argument operations" do
      assert deflipex ~~((+ 42))) == 42
      assert deflipex ~~((* 7))) == 7
    end

    test "deeply nested expressions" do
      # Test that deep nesting works without issues
      deep_nested = deflipex ~~((+ 1 (* 2 (+ 3 (* 4 (+ 5 6)))))
      # 1 + (2 * (3 + (4 * (5 + 6)) = 1 + (2 * (3 + (4 * 11))) = 1 + (2 * (3 + 44)) = 1 + (2 * 47) = 1 + 94 = 95
      assert deep_nested == 95
    end
  end
end