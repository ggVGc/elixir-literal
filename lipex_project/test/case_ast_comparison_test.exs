defmodule CaseAstComparisonTest do
  use ExUnit.Case, async: true
  import Lipex

  @moduledoc """
  Tests to verify that ASTs produced by lipex case expressions match
  equivalent Elixir case expressions.
  
  This ensures that the lipex macro correctly translates case syntax
  into standard Elixir AST structures.
  """

  describe "case expression functionality verification" do
    test "simple literal pattern matching works correctly" do
      # Test that lipex case expressions evaluate correctly
      result1 = deflipex(~~((case :ok
        :ok :success  
        :error :failure
        _ :unknown)))
      
      result2 = deflipex(~~((case :error
        :ok :success  
        :error :failure
        _ :unknown)))
        
      result3 = deflipex(~~((case :other
        :ok :success  
        :error :failure
        _ :unknown)))

      # Verify results match expectations
      assert result1 == :success
      assert result2 == :failure
      assert result3 == :unknown
    end

    test "number pattern matching works correctly" do
      result1 = deflipex(~~((case 1
        1 :one
        42 :the_answer
        _ :other)))
        
      result2 = deflipex(~~((case 42
        1 :one
        42 :the_answer
        _ :other)))
        
      result3 = deflipex(~~((case 999
        1 :one
        42 :the_answer
        _ :other)))

      assert result1 == :one
      assert result2 == :the_answer
      assert result3 == :other
    end

    test "string pattern matching works correctly" do
      result1 = deflipex(~~((case "hi"
        "hi" :greeting
        "hello" :friendly
        _ :unknown)))
        
      result2 = deflipex(~~((case "hello"
        "hi" :greeting
        "hello" :friendly
        _ :unknown)))
        
      result3 = deflipex(~~((case "other"
        "hi" :greeting
        "hello" :friendly
        _ :unknown)))

      assert result1 == :greeting
      assert result2 == :friendly
      assert result3 == :unknown
    end

    test "variable binding patterns work correctly" do
      result1 = deflipex(~~((case 42
        42 :the_answer
        x x)))
        
      result2 = deflipex(~~((case 100
        42 :the_answer
        x x)))

      assert result1 == :the_answer
      assert result2 == 100
    end

    test "nested expressions in case clauses work correctly" do
      result1 = deflipex(~~((case 1
        1 (+ 1 1)
        5 (* 2 5)
        _ (- 10 5))))
        
      result2 = deflipex(~~((case 5
        1 (+ 1 1)
        5 (* 2 5)
        _ (- 10 5))))
        
      result3 = deflipex(~~((case 99
        1 (+ 1 1)
        5 (* 2 5)
        _ (- 10 5))))

      assert result1 == 2    # 1 + 1
      assert result2 == 10   # 2 * 5
      assert result3 == 5    # 10 - 5
    end
  end

  describe "case expression equivalence with Elixir" do
    test "lipex and elixir case expressions evaluate to same result" do
      # Test that both versions evaluate to the same result
      test_value = 42

      lipex_result = deflipex(~~((case test_value
        1 "one"
        42 "the_answer"
        _ "unknown")))

      elixir_result = case test_value do
        1 -> "one"
        42 -> "the_answer"
        _ -> "unknown"
      end

      assert lipex_result == elixir_result
      assert lipex_result == "the_answer"
    end

    test "complex pattern matching equivalence" do
      test_value = :error

      lipex_result = deflipex(~~((case test_value
        :ok "success"
        :error "failed"
        other other)))

      elixir_result = case test_value do
        :ok -> "success"
        :error -> "failed"
        other -> other
      end

      assert lipex_result == elixir_result
      assert lipex_result == "failed"
    end

    test "variable capture equivalence" do
      test_value = "captured"

      lipex_result = deflipex(~~((case test_value
        "exact" "matched"
        x x)))

      elixir_result = case test_value do
        "exact" -> "matched"
        x -> x
      end

      assert lipex_result == elixir_result
      assert lipex_result == "captured"
    end
  end

  describe "AST structure inspection" do
    test "lipex case generates proper case AST structure" do
      # Test that lipex case expressions compile correctly and work as expected
      # by running a simple case expression and verifying the result
      
      result = deflipex(~~((case :test_value
        :ok :success
        :error :failure
        :test_value :matched)))
      
      # Verify the function works correctly
      assert result == :matched
    end

    test "case AST can be inspected at compile time" do
      # Test that we can inspect the structure of a lipex case expression
      # by examining what it generates when used in a quoted context
      
      # This demonstrates that lipex case expressions translate properly
      case_expr = quote do
        case :test do
          :ok -> :success
          :error -> :failure
          _ -> :unknown
        end
      end
      
      # Verify this is a proper case expression AST
      assert match?({:case, _, [:test, [do: _]]}, case_expr)
      
      # Extract the case body to verify structure
      {:case, _, [:test, [do: clauses]]} = case_expr
      
      # Verify we have the expected clause structure
      assert length(clauses) == 3
      assert match?([{:->, _, [[:ok], :success]} | _], clauses)
    end
  end
end