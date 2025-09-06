defmodule LipexDataTypesTest do
  use ExUnit.Case, async: true
  import Lipex

  # Identity function for testing bare data types
  def id(x), do: x

  describe "comprehensive data type testing" do
    test "all basic data types work with identity function" do
      # Integer
      assert deflipex(~~((id 42))) == 42
      assert deflipex(~~((id 0))) == 0
      assert deflipex(~~((id 123456))) == 123456

      # Float
      assert deflipex(~~((id 3.14))) == 3.14
      assert deflipex(~~((id 0.0))) == 0.0
      assert deflipex(~~((id 999.999))) == 999.999

      # String
      assert deflipex(~~((id "hello"))) == "hello"
      assert deflipex(~~((id ""))) == ""
      assert deflipex(~~((id "with spaces"))) == "with spaces"

      # Atom
      assert deflipex(~~((id :atom))) == :atom
      assert deflipex(~~((id :underscore_atom))) == :underscore_atom

      # Booleans
      assert deflipex(~~((id true))) == true
      assert deflipex(~~((id false))) == false

      # Nil
      assert deflipex(~~((id nil))) == nil
    end

    test "integers work in arithmetic" do
      result = deflipex ~~((+ 42 8))
      assert result == 50

      result = deflipex ~~((- 0 15))
      assert result == -15
    end

    test "floats work in arithmetic" do
      result = deflipex ~~((+ 3.14 0.86))
      assert result == 4.0

      result = deflipex ~~((/ 10.0 2.5))
      assert result == 4.0
    end

    test "strings work as map values" do
      result = deflipex ~~((% :greeting "Hello World" :farewell "Goodbye"))
      assert result == %{greeting: "Hello World", farewell: "Goodbye"}
      assert is_binary(result.greeting)
    end

    test "atoms work in various contexts" do
      result = deflipex ~~((atom? :test_atom))
      assert result == true

      result = deflipex ~~((% :status :active :type :user))
      assert result == %{status: :active, type: :user}
    end

    test "booleans work in logical operations" do
      result = deflipex ~~((and true true))
      assert result == true

      result = deflipex ~~((or false true))
      assert result == true
    end

    test "nil handling works" do
      result = deflipex ~~((% :value nil :present 42))
      assert result == %{value: nil, present: 42}
      assert is_nil(result.value)
    end

    test "lists work" do
      result = deflipex ~~((list 1 2 3 4))
      assert result == [1, 2, 3, 4]

      result = deflipex ~~((list 10 20 30))
      assert result == [10, 20, 30]
    end

    test "tuples work with brace syntax" do
      result = deflipex ~~({:ok "success"})
      assert result == {:ok, "success"}

      result = deflipex ~~({1 2 3 4 5})
      assert result == {1, 2, 3, 4, 5}
    end

    test "maps work with various key-value combinations" do
      result = deflipex ~~((% :name "John" :age 30 :active true))
      assert result == %{name: "John", age: 30, active: true}
    end

    test "nested data structures work" do
      result = deflipex ~~((list (% :id 1 :name "First") (% :id 2 :name "Second")))
      assert result == [%{id: 1, name: "First"}, %{id: 2, name: "Second"}]
    end

    test "mixed type collections work" do
      result = deflipex ~~((list 42 :atom "string" true nil))
      assert result == [42, :atom, "string", true, nil]
    end

    test "complex nested structures work" do
      result = deflipex ~~((% :user (% :name "Alice" :contacts (list)) :settings (% :theme :dark)))
      expected = %{
        user: %{name: "Alice", contacts: []},
        settings: %{theme: :dark}
      }
      assert result == expected
    end
  end
end
