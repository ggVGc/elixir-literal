defmodule LipexDemoTest do
  use ExUnit.Case, async: true
  
  @moduledoc """
  Test suite demonstrating advanced Lipex functionality including function definitions,
  pattern matching, function calls, and complex data structures.
  """
  
  import Lipex
  
  describe "function definitions" do
    # Define functions using Lipex syntax
    deflipex beginliteral (def square (x) (* x x)) endliteral
    deflipex beginliteral (def cube (x) (* x x x)) endliteral
    deflipex beginliteral (def add (a b) (+ a b)) endliteral
    
    test "function definitions work" do
      assert square(5) == 25
      assert cube(3) == 27
      assert add(10, 15) == 25
    end
  end
  
  describe "advanced arithmetic" do
    test "complex arithmetic operations work" do
      assert deflipex(beginliteral (+ 1 2 3 4 5) endliteral) == 15
      assert deflipex(beginliteral (* 2 3 4) endliteral) == 24
      assert deflipex(beginliteral (- 100 30) endliteral) == 70
      assert deflipex(beginliteral (/ 20 4) endliteral) == 5.0
      assert deflipex(beginliteral (< 5 10) endliteral) == true
      assert deflipex(beginliteral (>= 7 7) endliteral) == true
    end
  end
  
  describe "advanced data structures" do
    test "complex maps work" do
      user_map = deflipex beginliteral (% :name "Alice" :age 28 :city "Portland") endliteral
      assert user_map == %{name: "Alice", age: 28, city: "Portland"}
    end
    
    test "tuples with coordinates work" do
      coordinates = deflipex beginliteral (tuple 45.5152 -122.6784) endliteral
      assert coordinates == {45.5152, -122.6784}
    end
    
    test "shopping lists work" do
      shopping_list = deflipex beginliteral (list :apples :bananas :coffee :tea) endliteral
      assert shopping_list == [:apples, :bananas, :coffee, :tea]
    end
    
    test "bracket syntax for lists works" do
      bracket_list = deflipex beginliteral [1 2 3 4 5] endliteral
      assert bracket_list == [1, 2, 3, 4, 5]
    end
    
    test "nested lists work" do
      nested_list = deflipex beginliteral [[:a :b] [:c :d] [:e :f]] endliteral
      assert nested_list == [[:a, :b], [:c, :d], [:e, :f]]
    end
    
    test "keyword lists work" do
      settings = deflipex beginliteral (kwlist :timeout 3000 :retries 5) endliteral
      assert settings == [timeout: 3000, retries: 5]
    end
  end
  
  describe "pattern matching" do
    test "simple variable assignment works" do
      deflipex beginliteral (= x 42) endliteral
      assert x == 42
    end
    
    test "tuple destructuring works" do
      deflipex beginliteral (= {a b} (tuple 10 20)) endliteral
      assert a == 10
      assert b == 20
    end
    
    test "list destructuring with brackets works" do
      deflipex beginliteral (= [first second | rest] [1 2 3 4 5]) endliteral
      assert first == 1
      assert second == 2
      assert rest == [3, 4, 5]
    end
  end
  
  describe "function calls" do
    test "string module functions work" do
      upcase_result = deflipex beginliteral (String.upcase "hello world") endliteral
      assert upcase_result == "HELLO WORLD"
      
      int_to_string = deflipex beginliteral (Integer.to_string 42) endliteral
      assert int_to_string == "42"
    end
    
    test "enum module functions work" do
      count_result = deflipex beginliteral (Enum.count [1 2 3 4 5]) endliteral
      assert count_result == 5
      
      join_result = deflipex beginliteral (Enum.join ["hello" "lipex" "world"] " ") endliteral
      assert join_result == "hello lipex world"
    end
    
    test "nested function calls work" do
      # This tests that function calls can be nested and work correctly
      result = deflipex beginliteral (String.upcase (String.reverse "hello")) endliteral
      assert result == "OLLEH"
    end
  end
  
  describe "complex nested examples" do
    test "nested arithmetic expressions work" do
      complex_math = deflipex beginliteral (+ (* 2 5) (- 20 8) (/ 24 6)) endliteral
      assert complex_math == 26.0  # 10 + 12 + 4 = 26
    end
    
    test "nested logical expressions work" do
      nested_logic = deflipex beginliteral (and (> 10 5) (< 3 7) (number? 42)) endliteral
      assert nested_logic == true
    end
    
    test "nested data structures work" do
      nested_data = deflipex beginliteral (% :user (% :name "Bob" :score (+ 85 10)) :active true) endliteral
      expected = %{user: %{name: "Bob", score: 95}, active: true}
      assert nested_data == expected
    end
  end
  
  describe "real-world examples" do
    test "server configuration structures work" do
      server_config = deflipex beginliteral (% :host "api.example.com" :port 443 :ssl true) endliteral
      features = deflipex beginliteral (list :auth :logging :caching) endliteral
      timeout_calc = deflipex beginliteral (+ 5000 (* 2 1000)) endliteral
      
      assert server_config == %{host: "api.example.com", port: 443, ssl: true}
      assert features == [:auth, :logging, :caching]
      assert timeout_calc == 7000
    end
    
    test "mathematical formulas work" do
      # Formula 3x² - 2x + 7 at x=4
      formula_result = deflipex beginliteral (+ (* 3 (* 4 4)) (* (- 2) 4) 7) endliteral
      expected = 3 * 16 + (-2) * 4 + 7  # 48 - 8 + 7 = 47
      assert formula_result == expected
    end
  end
end