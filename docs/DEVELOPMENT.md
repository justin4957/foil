# Development Guide

## Prerequisites

### Required Software

1. **Erlang/OTP** (>= 26.0)
   ```bash
   # macOS
   brew install erlang

   # Or use asdf for version management
   asdf plugin add erlang
   asdf install erlang latest
   asdf global erlang latest
   ```

2. **Gleam** (>= 0.34.0)
   ```bash
   # macOS
   brew install gleam

   # Or use asdf
   asdf plugin add gleam
   asdf install gleam latest
   asdf global gleam latest
   ```

3. **Z3** (for z3_gleam development)
   ```bash
   # macOS
   brew install z3
   ```

4. **PostgreSQL** (for analyst persistence)
   ```bash
   # macOS
   brew install postgresql@16
   brew services start postgresql@16
   ```

5. **Redis** (for analyst caching)
   ```bash
   # macOS
   brew install redis
   brew services start redis
   ```

## Building the Project

This is a monorepo with multiple Gleam packages. You can build from the root or from individual packages.

### Build from Root
```bash
# Build the root workspace package
gleam build
```

### Build Individual Packages
```bash
# Build anthropic_gleam
cd packages/anthropic_gleam
gleam build
gleam test

# Build z3_gleam
cd packages/z3_gleam
gleam build
gleam test

# Build modal_logic
cd packages/modal_logic
gleam build
gleam test

# Build analyst application
cd apps/analyst
gleam build
gleam test
gleam run
```

## Development Workflow

### Working on Track A (anthropic_gleam)
```bash
cd packages/anthropic_gleam

# Add dependencies
gleam add package_name

# Run tests
gleam test

# Run tests with API key for integration tests
ANTHROPIC_API_KEY=your_key gleam test

# Format code
gleam format
```

### Working on Track B (z3_gleam)
```bash
cd packages/z3_gleam

# Ensure Z3 is installed
which z3

# Run tests
gleam test

# Build and test
gleam build && gleam test
```

### Working on Track C (analyst)
```bash
cd apps/analyst

# The analyst app depends on the other packages via path dependencies
# Make sure to build dependencies first if needed

# Run the application
gleam run

# Run tests
gleam test
```

## Package Dependencies

The dependency graph:
```
analyst
├── anthropic_gleam (path dependency)
├── z3_gleam (path dependency)
└── modal_logic (path dependency)

anthropic_gleam (independent, publishable)
z3_gleam (independent, publishable)
modal_logic (shared types)
```

## Testing

Each package has its own test suite:

```bash
# Test all packages (run from each directory)
cd packages/anthropic_gleam && gleam test
cd packages/z3_gleam && gleam test
cd packages/modal_logic && gleam test
cd apps/analyst && gleam test
```

For more testing details, see [TESTING.md](TESTING.md).

## Code Quality

Before creating a PR:

1. **Format code:**
   ```bash
   gleam format
   ```

2. **Run tests:**
   ```bash
   gleam test
   ```

3. **Check for warnings:**
   ```bash
   gleam build
   ```

## Environment Variables

Create a `.env` file in the root (already gitignored):

```bash
# For anthropic_gleam integration tests
ANTHROPIC_API_KEY=your_api_key_here

# For analyst application
DATABASE_URL=postgresql://localhost/modal_logic_engine
REDIS_URL=redis://localhost:6379
```

## Common Issues

### `escript` not found
Install Erlang: `brew install erlang`

### `gleam: command not found`
Install Gleam: `brew install gleam`

### Z3 binding errors
Install Z3: `brew install z3`

### Database connection errors
Ensure PostgreSQL is running: `brew services start postgresql@16`

## Project Structure

```
foil/
├── packages/          # Publishable libraries
│   ├── anthropic_gleam/
│   ├── z3_gleam/
│   └── modal_logic/
├── apps/             # Applications
│   └── analyst/
├── docs/             # Documentation
└── scripts/          # Development scripts
```

Each package is independent and can be developed, tested, and published separately.
