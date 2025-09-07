defmodule SimpleFunctionTest do
  use ExUnit.Case, async: false
  import Lipex

  # Define test functions at module level using the same syntax as calculator examples
  deflipex ~~((def double (x) (* x 2)))
  deflipex ~~((def abs_val (x) (* x -1)))

  test "simple function call" do
    result = deflipex ~~((double 5))
    assert result == 10
  end

  test "nested function call" do
    result = deflipex ~~((abs_val (- 5 (double 5))))
    assert result == 5
  end

  deflipex ~~((def when_over_5 (a) when (> a 5) true))
  deflipex ~~((def when_over_5 (_) false))

  test "when clauses" do
    assert when_over_5(6) == true
    assert when_over_5(2) == false
  end

  # Define recursive factorial function
  deflipex ~~(
    (def factorial (0) 1)
    (def factorial (n) (* n (factorial (- n 1))))
  )

  test "recursive function" do
    assert factorial(0) == 1
    assert factorial(1) == 1
    assert factorial(5) == 120
    assert factorial(6) == 720
  end

  test "only valid elixir variable names accepted in def parameter list" do
    # Test case 1: comma should be rejected
    lipex_expr_comma = {:sequence_block, [], :"()", [
      {:sequence_token, [], :def},
      {:sequence_token, [], :test_func}, 
      {:sequence_paren, [], [
        {:sequence_token, [], :_a},
        {:sequence_token, [], :","},  # Invalid: comma
        {:sequence_token, [], :_b}
      ]},
      {:sequence_token, [], :body}
    ]}
    
    assert_raise ArgumentError, ~r/Invalid parameter name/, fn ->
      Lipex.eval_lipex_expr(lipex_expr_comma)
    end
    
    # Test case 2: operators should be rejected  
    lipex_expr_operator = {:sequence_block, [], :"()", [
      {:sequence_token, [], :def},
      {:sequence_token, [], :test_func2}, 
      {:sequence_paren, [], [
        {:sequence_token, [], :x},
        {:sequence_token, [], :+}  # Invalid: operator
      ]},
      {:sequence_token, [], :body}
    ]}
    
    assert_raise ArgumentError, ~r/Invalid parameter name/, fn ->
      Lipex.eval_lipex_expr(lipex_expr_operator)
    end
    
    # Test case 3: valid variable names should work (including reserved keywords)
    lipex_expr_valid = {:sequence_block, [], :"()", [
      {:sequence_token, [], :def},
      {:sequence_token, [], :test_func3}, 
      {:sequence_paren, [], [
        {:sequence_token, [], :x},
        {:sequence_token, [], :_valid_var},
        {:sequence_token, [], :and},  # Reserved keywords are OK as variable names
        {:sequence_token, [], :when}
      ]},
      {:sequence_token, [], :body}
    ]}
    
    # This should work - no assertion, just shouldn't raise
    result = Lipex.eval_lipex_expr(lipex_expr_valid)
    # Verify it produces a function definition AST
    assert match?({:def, _, [{{:sequence_token, _, :test_func3}, _, [_, _, _, _]}, _]}, result)
  end
end
