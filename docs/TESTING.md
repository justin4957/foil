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
- Proof tree construction and traversal
- Debugger session management
- Visualization configuration

**Proof Visualization Tests:**
```gleam
// Test proof tree creation
pub fn proof_tree_valid_construction_test() {
  let premises = [Atom("p"), Implies(Atom("p"), Atom("q"))]
  let conclusion = Atom("q")
  let tree = proof_tree.valid_proof(K, premises, conclusion, [])
  tree.metadata.is_valid |> should.equal(True)
}

// Test debugger stepping
pub fn debugger_step_forward_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)
  let result = debugger.step_forward(session)
  result.session.current_step |> should.equal(1)
}

// Test D3.js visualization generation
pub fn proof_visualization_to_d3_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let js = proof_visualization.proof_tree_to_d3(tree, default_config())
  string.contains(js, "d3.select") |> should.equal(True)
}
```

**Plugin System Tests:**
```gleam
// Test plugin registration
pub fn registry_register_plugin_test() {
  let config = plugin.default_registry_config()
  let registry = plugin.new_registry(config)
  let test_plugin = create_test_plugin("test_1")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      plugin.list_plugins(updated_registry) |> list.length |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

// Test plugin loading
pub fn loader_load_plugin_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let test_plugin = create_test_plugin("loadable")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      let loader = plugin.new_loader(updated_registry, plugin.default_loader_config())
      case plugin.load_plugin(loader, "loadable") {
        Ok(updated_loader) -> {
          // Plugin should now be active
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test temporal logic plugins
pub fn ltl_plugin_creation_test() {
  let ltl = temporal.ltl_plugin()
  ltl.metadata.id |> should.equal("temporal_ltl")
  ltl.logic.operators |> list.length |> should.equal(6)
}

// Test version management
pub fn version_satisfies_caret_test() {
  plugin.version_satisfies("1.2.3", "^1.2.3") |> should.be_true
  plugin.version_satisfies("1.9.0", "^1.2.3") |> should.be_true
  plugin.version_satisfies("2.0.0", "^1.2.3") |> should.be_false
}

// Test security sandbox
pub fn sandbox_operation_allowed_test() {
  let config = plugin.default_sandbox_config()
  let sandbox = plugin.new_sandbox(config)
  plugin.is_operation_allowed(sandbox, OpEvaluate) |> should.be_true
}
```

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
