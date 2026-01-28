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

## Validation Accuracy and Confusion Matrix

The `accuracy_tests` module computes validation accuracy using a proper confusion
matrix from actual heuristic validation results:

- **True Positive (TP)**: Argument is valid, heuristic classifies as Valid
- **True Negative (TN)**: Argument is invalid, heuristic classifies as Invalid
- **False Positive (FP)**: Argument is invalid, heuristic classifies as Valid
- **False Negative (FN)**: Argument is valid, heuristic classifies as Invalid
- **Indeterminate**: Heuristic returns Unknown/Timeout/Error, or expected validity is Unknown/Either

Metrics derived from confusion matrix:
- **Precision** = TP / (TP + FP) — of arguments classified as valid, how many truly are
- **Recall** = TP / (TP + FN) — of truly valid arguments, how many were classified as valid
- **F1 Score** = 2 * Precision * Recall / (Precision + Recall) — harmonic mean

Each `AccuracyTestResult` includes a `ValidationClassification` field tracking
the predicted vs expected validity. The classification runs actual heuristic
validation (`heuristics.try_heuristic_validation`) on the translated formalization
rather than using confidence thresholds as a proxy.

**Dialogue test:** `test/validation_correctness_dialogue_test.gleam` verifies
confusion matrix computation with known valid and invalid arguments.

## Timing and Performance Measurement

The `modal_logic/timing` module provides real BEAM monotonic time instrumentation
via `erlang:monotonic_time/0` FFI. All benchmark and validation metrics use
measured durations instead of simulated/hardcoded values.

**Key functions:**
- `timing.measure_ms(fn)` — Measure a function's elapsed time in milliseconds
- `timing.measure_microseconds(fn)` — Sub-millisecond precision for fast operations
- `timing.monotonic_time_native()` — Raw monotonic clock reading
- `timing.native_to_ms(duration)` — Convert native units to milliseconds
- `timing.native_to_microseconds(duration)` — Convert native units to microseconds

**Where timing is used:**
- `benchmark_runner.gleam` — Suite-level total duration and per-case durations
- `epic_validation.gleam` — Tier 1 P80 latency and Tier 2 average latency metrics
- `accuracy_tests.gleam` — Per-fixture pipeline duration

**Dialogue test:** `test/timing_instrumentation_dialogue_test.gleam` verifies
that monotonic time returns real values, throughput is non-zero, and the
`benchmark_performance` metric passes at 100%.

```gleam
// Example: measure Tier 1 heuristic latency
let #(result, elapsed_us) = timing.measure_microseconds(fn() {
  heuristics.try_heuristic_validation(formalization)
})
// elapsed_us is real microseconds from BEAM monotonic clock
```

## Coverage Goals

- Aim for >80% code coverage
- 100% coverage for critical paths (validation logic)
- Document known gaps in coverage
