# Testing Strategy

## Overview

This document outlines testing strategies for the Modal Logic Engine project.

## Per-Package Testing

### anthropic_gleam

**Unit Tests:**
- JSON encoding/decoding for all message types
- Configuration management
- Error type conversions

**Integration Tests:**
- Actual API calls (requires `ANTHROPIC_API_KEY`)
- Streaming response handling
- Tool use cycles

**Running Tests:**
```bash
cd packages/anthropic_gleam
gleam test                    # Unit tests only
ANTHROPIC_API_KEY=xxx gleam test  # With integration tests
```

### z3_gleam

**Unit Tests:**
- Expression building
- Type inference
- Model extraction logic

**Integration Tests:**
- Z3 solver integration
- Modal logic encoding
- Countermodel extraction

**Running Tests:**
```bash
cd packages/z3_gleam
gleam test
```

### modal_logic

**Unit Tests:**
- Proposition construction
- Type conversions
- JSON serialization

**Running Tests:**
```bash
cd packages/modal_logic
gleam test
```

### analyst

**Unit Tests:**
- Domain logic
- Service layer functions

**Integration Tests:**
- Full analysis pipeline
- Database operations (requires PostgreSQL)
- Cache operations (requires Redis)

**Running Tests:**
```bash
cd apps/analyst
gleam test
```

## Testing Best Practices

1. **Use descriptive test names** - Clearly state what is being tested
2. **Test edge cases** - Empty lists, invalid inputs, boundary conditions
3. **Mock external dependencies** - Use test fixtures for API responses
4. **Property-based testing** - For logical properties (e.g., if valid in S5, valid in S4)
5. **Golden tests** - Known valid/invalid arguments should produce expected results

## Continuous Integration

Tests will run automatically on:
- Pull request creation
- Push to main branch
- Manual workflow dispatch

See `.github/workflows/` for CI configuration.

## Test Data

Test fixtures and sample arguments are located in:
- `packages/*/test/fixtures/`
- `apps/analyst/test/fixtures/`

## Coverage Goals

- Aim for >80% code coverage
- 100% coverage for critical paths (validation logic)
- Document known gaps in coverage
