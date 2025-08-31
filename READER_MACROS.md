# Reader Macros Implementation for Elixir

## Overview

This implementation adds reader macros to Elixir, allowing developers to define macros that operate on raw source code during tokenization, before the source is parsed into an Abstract Syntax Tree (AST).

## Key Features

- **Raw Source Processing**: Reader macros work on the actual source text before tokenization
- **Pattern Matching**: Support for binary patterns and regex patterns  
- **Integration**: Seamlessly integrated into the existing Elixir compilation pipeline
- **Familiar Syntax**: Uses `defreadermacro/2` form similar to `defmacro/2`

## Implementation Details

### Files Modified/Created

1. **`lib/elixir/src/elixir_reader_macros.erl`** (NEW)
   - Core reader macro engine
   - Pattern matching and expansion logic
   - Reader macro storage and retrieval

2. **`lib/elixir/src/elixir_tokenizer.erl`** (MODIFIED)
   - Added `tokenize_with_reader_macros/4` function
   - Integration hook for reader macro expansion

3. **`lib/elixir/src/elixir_def.erl`** (MODIFIED)
   - Added `defreadermacro` support to definition handling
   - Special processing for reader macro definitions
   - Reader macro storage during compilation

4. **`lib/elixir/lib/kernel.ex`** (MODIFIED)
   - Added `defreadermacro/2` macro form
   - Documentation and examples

5. **`lib/elixir/test/elixir/reader_macro_test.exs`** (NEW)
   - Comprehensive test suite
   - Pattern matching tests
   - Integration tests

6. **`lib/elixir/test/elixir/fixtures/reader_macro_example.ex`** (NEW)
   - Example usage patterns
   - Documentation examples

## Usage Examples

### Basic Environment Variable Reader Macro

```elixir
defmodule MyMacros do
  defreadermacro env("@@" <> var_name) do
    "System.get_env(\"#{String.trim(var_name)}\")"
  end
end

# Usage: @@HOME expands to System.get_env("HOME")
```

### JSON Literal Reader Macro

```elixir
defreadermacro json_literal("json!" <> json_content) do
  "Jason.decode!(\"#{String.trim(json_content)}\")"
end

# Usage: json!{"key": "value"} expands to Jason.decode!("{\"key\": \"value\"}")
```

### SQL Query Reader Macro

```elixir
defreadermacro sql("SQL:" <> query) do
  "MyApp.Database.query(\"#{String.trim(query)}\")"
end

# Usage: SQL:SELECT * FROM users expands to MyApp.Database.query("SELECT * FROM users")
```

## Architecture

### Reader Macro Processing Pipeline

1. **Definition Phase**: `defreadermacro` calls are processed during module compilation
2. **Storage Phase**: Reader macros are stored in module data tables
3. **Expansion Phase**: During tokenization, source code is checked for reader macro patterns
4. **Transformation Phase**: Matching patterns are replaced with expanded code
5. **Normal Processing**: Expanded code continues through normal tokenization/parsing

### Pattern Matching

Reader macros support two types of patterns:

1. **Binary Patterns**: Direct string matching (e.g., `"@@" <> rest`)
2. **Regex Patterns**: Regular expression matching (e.g., `{:regex, "\\d+"}`)

### Integration Points

- **Tokenizer**: `elixir_tokenizer:tokenize_with_reader_macros/4`
- **Definition Handler**: `elixir_def:store_reader_macro_definition/5`
- **Storage Engine**: `elixir_reader_macros:store_reader_macro/5`
- **Expansion Engine**: `elixir_reader_macros:expand_reader_macros/3`

## Technical Considerations

### Performance

- Reader macros are processed only once during tokenization
- Pattern matching is optimized for common cases
- No runtime overhead for expanded code

### Error Handling

- Invalid patterns are caught during compilation
- Expansion errors are handled gracefully
- Clear error messages for debugging

### Limitations

- Reader macros operate on raw text, not AST structures
- Pattern matching is limited to string/regex patterns
- No access to compile-time environment during expansion

## Testing

The implementation includes comprehensive tests covering:

- Reader macro definition and storage
- Pattern matching functionality  
- Source code expansion
- Integration with tokenizer
- Error cases and edge conditions

Run tests with:
```bash
mix test test/elixir/reader_macro_test.exs
```

## Future Enhancements

Possible future improvements:

1. **Advanced Pattern Matching**: Support for more complex pattern types
2. **Nested Expansion**: Support for reader macros that expand to other reader macros
3. **Performance Optimization**: Caching and optimization for frequently used patterns
4. **IDE Integration**: Syntax highlighting and completion for reader macro patterns
5. **Debugging Tools**: Enhanced debugging support for reader macro expansion

## Compatibility

- **Elixir Version**: Compatible with Elixir 1.20+
- **OTP Version**: Compatible with OTP 25+
- **Breaking Changes**: None - fully backward compatible

## Contributing

When modifying reader macro functionality:

1. Update tests in `reader_macro_test.exs`
2. Update documentation in `kernel.ex`  
3. Consider performance implications
4. Ensure error handling is comprehensive
5. Update this documentation as needed