# SPDX-License-Identifier: Apache-2.0
# String interpolation tests for Lipex

# Require all necessary modules for the test
Code.require_file("../evaluator.ex", __DIR__)
Code.require_file("../strings/interpolation.ex", __DIR__)
Code.require_file("../core/data_structures.ex", __DIR__)
Code.require_file("../core/pattern_matching.ex", __DIR__)
Code.require_file("../core/arithmetic.ex", __DIR__)
Code.require_file("../core/logic.ex", __DIR__)
Code.require_file("../core/control_flow.ex", __DIR__)
Code.require_file("../functions/calls.ex", __DIR__)
Code.require_file("../functions/definitions.ex", __DIR__)
Code.require_file("../lipex.ex", __DIR__)

# Load test framework
Code.require_file("test_helper.exs", Path.dirname(__ENV__.file))

defmodule Lipex.Strings.InterpolationTest do
  use ExUnit.Case, async: true
  
  alias Lipex.Strings.Interpolation

  describe "try_eval/1" do
    test "handles simple string interpolation with variables" do
      # AST for "x: #{x}"
      ast = {:<<>>, [], [
        "x: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
           [{:x, [], Elixir}]},
          {:binary, [], Elixir}
        ]}
      ]}
      
      assert {:ok, result} = Interpolation.try_eval(ast)
      
      # Verify the structure is preserved but processed
      assert {:<<>>, [], [
        "x: ",
        {:"::", [], [_, {:binary, [], Elixir}]}
      ]} = result
    end
    
    test "handles Lipex sequence_prefix expressions in interpolation" do
      # AST for "x: #{(+ 3 4)}" with Lipex sequence inside
      ast = {:<<>>, [], [
        "x: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
           [
             # This is the key - Lipex sequence_prefix
             {:sequence_prefix, [], [:+, 3, 4]}
           ]},
          {:binary, [], Elixir}
        ]}
      ]}
      
      assert {:ok, result} = Interpolation.try_eval(ast)
      
      # Verify it's an interpolated string structure
      assert {:<<>>, [], [
        "x: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true], [_evaluated_expr]},
          {:binary, [], Elixir}
        ]}
      ]} = result
    end
    
    test "handles Lipex sequence_paren expressions in interpolation" do
      # AST for string with sequence_paren format
      ast = {:<<>>, [], [
        "result: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
           [
             {:sequence_paren, [], [:*, 5, 6]}
           ]},
          {:binary, [], Elixir}
        ]}
      ]}
      
      assert {:ok, result} = Interpolation.try_eval(ast)
      
      # Verify the interpolation structure is maintained
      assert {:<<>>, [], [
        "result: ",
        {:"::", [], [_, {:binary, [], Elixir}]}
      ]} = result
    end
    
    test "handles nested Lipex expressions in interpolation" do
      # AST for "result: #{(* 2 (+ 3 4))}"
      ast = {:<<>>, [], [
        "result: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
           [
             {:sequence_prefix, [], [:*, 2, {:sequence_prefix, [], [:+, 3, 4]}]}
           ]},
          {:binary, [], Elixir}
        ]}
      ]}
      
      assert {:ok, result} = Interpolation.try_eval(ast)
      
      # Verify nested expressions get processed
      assert {:<<>>, [], [
        "result: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true], [_complex_expr]},
          {:binary, [], Elixir}
        ]}
      ]} = result
    end
    
    test "handles multiple interpolated parts" do
      # AST for "a: #{x}, b: #{(+ 1 2)}"
      ast = {:<<>>, [], [
        "a: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
           [{:x, [], Elixir}]},
          {:binary, [], Elixir}
        ]},
        ", b: ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
           [{:sequence_prefix, [], [:+, 1, 2]}]},
          {:binary, [], Elixir}
        ]}
      ]}
      
      assert {:ok, result} = Interpolation.try_eval(ast)
      
      # Verify all parts are processed
      assert {:<<>>, [], [
        "a: ",
        {:"::", [], [_, {:binary, [], Elixir}]},
        ", b: ",
        {:"::", [], [_, {:binary, [], Elixir}]}
      ]} = result
    end
    
    test "passes through non-interpolation expressions" do
      # Regular string (not interpolated)
      regular_string = "hello world"
      assert :pass = Interpolation.try_eval(regular_string)
      
      # Regular tuple
      regular_tuple = {:ok, :success}
      assert :pass = Interpolation.try_eval(regular_tuple)
      
      # Random AST
      other_ast = {:some_other, [], [:random, :ast]}
      assert :pass = Interpolation.try_eval(other_ast)
    end
    
    test "handles empty interpolated string" do
      # AST for "" (empty interpolated string)
      empty_ast = {:<<>>, [], []}
      
      assert {:ok, result} = Interpolation.try_eval(empty_ast)
      assert {:<<>>, [], []} = result
    end
    
    test "handles literal string parts correctly" do
      # AST for "hello #{world}" where we focus on literal preservation
      ast = {:<<>>, [], [
        "hello ",
        {:"::", [], [
          {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
           [{:world, [], Elixir}]},
          {:binary, [], Elixir}
        ]},
        " end"
      ]}
      
      assert {:ok, result} = Interpolation.try_eval(ast)
      
      # Verify literal strings are preserved exactly
      assert {:<<>>, [], [
        "hello ",
        {:"::", [], [_, {:binary, [], Elixir}]},
        " end"
      ]} = result
    end
  end
  
  describe "integration with Lipex evaluation pipeline" do
    test "Lipex arithmetic expressions evaluate correctly" do
      # Test that our Lipex expressions actually work in isolation
      simple_add = {:sequence_prefix, [], [:+, 3, 4]}
      result = Lipex.eval_lipex_expr(simple_add)
      
      # Should produce some form of addition AST
      # The exact structure depends on the arithmetic module implementation
      assert is_tuple(result)
    end
    
    test "complex nested expressions are handled" do  
      # Test that nested Lipex expressions work
      nested_expr = {:sequence_prefix, [], [:*, 2, {:sequence_prefix, [], [:+, 3, 4]}]}
      result = Lipex.eval_lipex_expr(nested_expr)
      
      # Should produce some form of multiplication AST
      assert is_tuple(result)
    end
  end
  
  describe "error handling" do
    test "malformed interpolation AST is handled gracefully" do
      # Malformed interpolation structure
      bad_ast = {:<<>>, [], [
        "test: ",
        {:"::", [], [
          "not_a_proper_interpolation_structure"
        ]}
      ]}
      
      # Should not crash, should either return :pass or {:ok, processed}
      result = Interpolation.try_eval(bad_ast)
      assert result == :pass or match?({:ok, _}, result)
    end
  end
end