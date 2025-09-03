# SPDX-License-Identifier: Apache-2.0
# Test helper for Lipex tests

# Start ExUnit
ExUnit.start()

# Configure ExUnit
ExUnit.configure(
  exclude: [:integration],  # Exclude integration tests by default
  formatters: [ExUnit.CLIFormatter]
)

# Helper functions for Lipex tests
defmodule LipexTestHelpers do
  @moduledoc """
  Helper functions for Lipex testing.
  """
  
  @doc """
  Creates an AST structure representing an interpolated string.
  
  ## Examples
  
      iex> LipexTestHelpers.interpolated_string_ast("x: ", {:x, [], Elixir})
      {:<<>>, [], ["x: ", {:"::", [], [...]}]}
  """
  def interpolated_string_ast(prefix, expr) do
    {:<<>>, [], [
      prefix,
      {:"::", [], [
        {{:., [], [Kernel, :to_string]}, [from_interpolation: true], [expr]},
        {:binary, [], Elixir}
      ]}
    ]}
  end
  
  @doc """
  Creates a Lipex sequence_prefix AST for testing.
  
  ## Examples
  
      iex> LipexTestHelpers.lipex_sequence(:+, [3, 4])
      {:sequence_prefix, [], [:+, 3, 4]}
  """
  def lipex_sequence(operator, args) when is_list(args) do
    {:sequence_prefix, [], [operator | args]}
  end
  
  @doc """
  Creates a Lipex sequence_paren AST for testing.
  """
  def lipex_paren_sequence(operator, args) when is_list(args) do
    {:sequence_paren, [], [operator | args]}
  end
  
  @doc """
  Asserts that a result is a valid interpolated string AST.
  """
  def assert_interpolated_string(result) do
    case result do
      {:<<>>, [], parts} when is_list(parts) ->
        # Verify that interpolated parts have the right structure
        Enum.each(parts, fn
          binary when is_binary(binary) -> :ok
          {:"::", [], [_expr, {:binary, [], _context}]} -> :ok
          other -> 
            ExUnit.Assertions.flunk("Invalid interpolation part: #{inspect(other)}")
        end)
        result
        
      other ->
        ExUnit.Assertions.flunk("Expected interpolated string AST, got: #{inspect(other)}")
    end
  end
end