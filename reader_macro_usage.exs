defmodule ReaderMacroUsage do
  import ReaderMacroDefinitions
  
  def test_hello do
    IO.puts("Testing HELLO: #{HELLO}")
  end
  
  def test_greet do  
    IO.puts("Testing GREET_WORLD: #{GREET_WORLD}")
  end
  
  def test_add do
    IO.puts("Testing (+ 1 2): #{(+ 1 2)}")
  end
  
  def run_tests do
    test_hello()
    test_greet()  
    test_add()
  end
end

ReaderMacroUsage.run_tests()