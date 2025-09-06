# Lipex Examples

This directory contains examples demonstrating Lipex functionality, from basic syntax showcases to complete applications.

## Quick Start

All examples require the modified Elixir binary. From the `lipex_project` directory:

```bash
# Run any example
../bin/elixir examples/demo.exs

# Or from within the examples directory
cd examples
../../bin/elixir demo.exs
```

## Feature Demonstrations

### 📚 Core Language Features

- **`demo.exs`** - Comprehensive feature showcase covering all major Lipex capabilities
- **`playground.exs`** - Simple test environment for experimenting
- **`fail_puts.exs`** - String interpolation testing

Run the main demo to see all features:
```bash
../bin/elixir examples/demo.exs
```

## Example Applications

### 🧮 Calculator CLI (`apps/calculator/`)
Interactive command-line calculator with variable storage and mathematical functions.

**Features:**
- Complex expression evaluation: `(+ (* 2 3) (/ 10 2))`
- Variable assignment: `(= x 42)` then `(+ x 8)`
- Mathematical functions: `sqrt`, `pow`, `abs`
- Calculation history
- Error handling

**Run:**
```bash
../bin/elixir examples/apps/calculator/calculator_app.exs
```

### ⚙️ Configuration Manager (`apps/config_manager/`)
Application configuration management using native Lipex syntax.

**Features:**
- Load/save configurations in Lipex format
- Environment-specific configs (dev/test/prod)
- Nested configuration structures
- Validation and defaults
- Dynamic updates

**Run:**
```bash
../bin/elixir examples/apps/config_manager/config_app.exs
```

### 🎮 Tic-Tac-Toe Game (`apps/tictactoe/`)
Interactive console game showcasing game logic and user interaction.

**Features:**
- Game state management in Lipex
- Win condition checking
- Player input validation
- Simple AI opponent
- Interactive game loop

**Run:**
```bash
../bin/elixir examples/apps/tictactoe/game_app.exs
```

### 📋 Task Manager CLI (`apps/todo_manager/`)
Command-line todo list manager with persistent storage.

**Features:**
- Add/remove/update tasks
- Task categorization and prioritization
- Due date handling
- File-based persistence
- Filtering and search

**Run:**
```bash
../bin/elixir examples/apps/todo_manager/todo_app.exs
```

### 🔄 Data Processing Pipeline (`apps/data_pipeline/`)
ETL-style data transformation using functional Lipex patterns.

**Features:**
- CSV/JSON parsing and generation
- Data filtering and mapping
- Aggregation and statistics
- Pipeline composition
- Error handling and validation

**Run:**
```bash
../bin/elixir examples/apps/data_pipeline/pipeline_app.exs
```

### 🌐 Simple HTTP Server (`apps/http_server/`)
Basic HTTP server with Lipex-defined routing.

**Features:**
- Route definitions in Lipex syntax
- JSON API endpoints
- Request/response handling
- Basic middleware support
- RESTful patterns

**Run:**
```bash
../bin/elixir examples/apps/http_server/server_app.exs
```

## Learning Path

1. **Start with `demo.exs`** to see all language features
2. **Try the Calculator** for interactive Lipex experience
3. **Explore Configuration Manager** for practical data structures
4. **Play Tic-Tac-Toe** to understand game logic patterns
5. **Build with other apps** for specific use cases

## Application Structure

Each application follows this pattern:
- **Main application file** (e.g., `calculator_app.exs`)
- **README.md** with detailed usage instructions
- **Sample data files** where applicable
- **Standalone execution** with clear error messages

## Requirements

- Modified Elixir binary: `/home/ggvgc/stuff/projects/elixir/bin/elixir`
- Lipex project compiled: `../bin/elixir -S mix compile`
- Some applications may require additional dependencies (noted in their READMEs)

## Contributing Examples

When adding new examples:
1. Create a focused application that demonstrates specific Lipex features
2. Include comprehensive error handling and user feedback
3. Provide sample data or configuration files where helpful
4. Document the Lipex patterns and syntax used
5. Test with the modified Elixir binary

## Troubleshooting

**"Module not found" errors**: Ensure you're using the modified Elixir binary and have compiled the project with `../bin/elixir -S mix compile`.

**"Invalid syntax" errors**: Check that you're using proper `~~(...)` sequence literal syntax.

**Import errors**: Applications use `import Lipex` instead of manual module loading for cleaner code.

## Next Steps

These examples demonstrate current Lipex capabilities. As new language features are added (pipes, comprehensions, error handling), new examples will showcase those features.

Happy coding with Lipex! 🚀