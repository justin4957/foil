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

### Confidence Calibration

`TranslationMetrics.confidence_calibration` contains a `ConfidenceCalibration`
record with proper statistical metrics replacing the previous simple
mean-difference proxy:

- **Brier Score** = 1/N * Σ(confidence_i - correct_i)² — mean squared error
  between predicted confidence and actual correctness (lower is better, 0.0 =
  perfect calibration)
- **Expected Calibration Error (ECE)** — weighted average of |mean_confidence -
  observed_accuracy| across 5 confidence buckets ([0.0-0.2), [0.2-0.4),
  [0.4-0.6), [0.6-0.8), [0.8-1.0])
- **Overconfidence Rate** — fraction of high-confidence (>0.8) results that are
  incorrect (correctness < 0.5)
- **Underconfidence Rate** — fraction of low-confidence (<0.5) results that are
  correct (correctness >= 0.5)
- **Calibration Curve** — per-bucket predicted confidence vs observed accuracy
  data for plotting calibration diagrams

Translation match levels are scored continuously rather than binary:
- ExactTranslation → 1.0
- PartialTranslation(matched, total) → matched / total
- ConclusionOnly → 0.25
- NoTranslation → 0.0

```gleam
// Example: access calibration metrics
let results = accuracy_tests.run_accuracy_tests(fixtures)
let cal = results.translation.confidence_calibration
// cal.brier_score — lower is better
// cal.expected_calibration_error — lower is better
// cal.overconfidence_rate — fraction of overconfident results
// cal.calibration_curve — list of CalibrationBucket records
```

**Dialogue test:** `test/confidence_calibration_dialogue_test.gleam` verifies
Brier score computation, ECE, over/underconfidence rates, calibration curve
buckets, translation match scoring, and empty-input safety.

### Per-System Validation Breakdown

`AccuracyResults.validation_by_system` contains a `List(#(String, ValidationMetrics))`
with a full confusion matrix (TP/TN/FP/FN/precision/recall/F1) for each modal
logic system that has fixtures:

- **K**: Base modal logic
- **T**: Reflexive frames
- **K4**: Transitive frames
- **S4**: Reflexive + transitive
- **S5**: Equivalence relation
- **KD**: Deontic (serial)
- **KD45**: Doxastic (serial + transitive + euclidean)

Use `find_lowest_f1_systems(by_system)` to identify systems with the weakest
validation accuracy, sorted by F1 score ascending.

### Per-Complexity Validation Breakdown

`AccuracyResults.validation_by_complexity` groups results by formula complexity
based on modal operator count:

- **Simple**: 0-2 modal operators
- **Medium**: 3-5 modal operators
- **Complex**: 6+ modal operators

The `count_modal_operators(proposition)` function recursively counts Necessary,
Possible, Obligatory, Permitted, Knows, and Believes operators in a formula.

### System Boundary Fixtures

`fixtures.system_boundary_fixtures()` provides test cases that exercise
system-specific boundary behavior:

- K4: Valid (4-axiom) and invalid (T-axiom fails without reflexivity)
- KD45: Valid (belief distribution) and invalid (belief not veridical)
- T: Invalid (4-axiom fails without transitivity)
- S4: Invalid (5-axiom fails without euclidean property)
- S5: Invalid (possibility does not imply necessity)
- KD: Invalid (permission does not imply obligation)

**Dialogue test:** `test/per_system_accuracy_dialogue_test.gleam` verifies
per-system confusion matrices, complexity bucketing, lowest F1 identification,
and system boundary fixture coverage.

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

## Logic System Detection

The `accuracy_tests` module detects the logic system from formula structure
and compares against the expected system in each `TestFixture`. Detection
analyzes modal operators and axiom patterns:

**Detection priority:**
1. Epistemic operators (`Knows`) → S5
2. Doxastic operators (`Believes`) → KD45
3. Deontic operators (`Obligatory`, `Permitted`) → KD
4. Axiom patterns requiring specific frame properties:
   - T-axiom (`□p → p` or `□p ⊢ p`) → requires reflexivity → T
   - 4-axiom (`□p → □□p` or `□p ⊢ □□p`) → requires transitivity → K4
   - 5-axiom (`◇p → □◇p` or `◇p ⊢ □◇p`) → requires euclidean → S5
   - D-axiom (`□p → ◇p`) → requires seriality → KD
   - Combined reflexivity + transitivity → S4
5. Pure alethic modal operators with no special patterns → K

**Per-system breakdown:** `LogicDetectionMetrics.by_system` contains tuples of
`(system_name, correct_count, total_count)` for each system that has fixtures,
enabling identification of which modal systems have weaker detection accuracy.

**Dialogue test:** `test/logic_system_detection_dialogue_test.gleam` verifies
detection for each supported system (K, T, S4, S5, K4, KD, KD45) and confirms
the per-system breakdown is populated with real data.

```gleam
// Example: detect logic system from formula structure
let detected = accuracy_tests.detect_logic_system(premises, conclusion)
// Returns the LogicSystem (K, T, S4, S5, K4, KD, KD45) based on
// operator analysis and axiom pattern matching
```

## Curated Ground-Truth Fixtures

The `fixtures/ground_truth.gleam` module provides 52 independently verified test
cases for measuring accuracy against real logic problems rather than synthetic
generated data.

### Categories (10)

- **Propositional** (8): Modus ponens, modus tollens, hypothetical syllogism,
  disjunctive syllogism, affirming consequent, denying antecedent, constructive
  dilemma, ex falso quodlibet
- **Modal K** (5): Distribution axiom, necessitation, invalid possibility
  distribution, necessity-conjunction, invalid necessity-to-truth
- **Modal T** (4): T-axiom, truth implies possibility, invalid 4-axiom, K+T combined
- **Modal S4** (4): 4-axiom, possible collapse, invalid 5-axiom, T-axiom holds
- **Modal S5** (5): 5-axiom, possible-necessary collapse, invalid possible-to-necessary,
  iteration collapse, possibility iteration
- **Deontic** (4): D-axiom, distribution, invalid permission-to-obligation,
  invalid obligation-to-truth
- **Epistemic** (5): Knowledge distribution, veridicality, positive introspection,
  belief not veridical, belief consistency
- **Cross-system** (5): T-not-K reflexivity, S4-not-T transitivity, S5-not-S4
  euclidean, K4-not-K transitivity, non-sequitur
- **Deep nesting** (5): Triple necessity S4, mixed modality S5, alternating S5,
  triple necessity T, different atoms
- **Tier boundary** (7): Identity, contraposition, complex propositional,
  modal-propositional mix, countermodel needed, conjunction intro, modal invalid

### Source Documentation

Every fixture includes a `source` field documenting its justification (axiom
schema, standard textbook result, well-known fallacy, etc.). This ensures each
expected validity is independently verified rather than assumed.

### Usage

```gleam
import modal_logic/testing/fixtures/ground_truth

// Get all curated fixtures
let fixtures = ground_truth.all_ground_truth_fixtures()
// Returns 52 TestFixture values

// Run accuracy against curated fixtures
let results = accuracy_tests.run_accuracy_tests(fixtures)
// Results include per-system and per-complexity breakdowns
```

### Phase D Integration

The `validate_curated_accuracy()` metric in `epic_validation.gleam` runs the
full accuracy pipeline against curated fixtures and reports F1 score with
per-system and per-complexity breakdown. Target: 50% F1 (curated cases are
harder than generated ones).

**Dialogue test:** `test/ground_truth_dialogue_test.gleam` verifies fixture
count (>= 50), category coverage, source documentation, accuracy pipeline
integration, per-system/per-complexity breakdowns, and Phase D metric wiring.

## Coverage Goals

- Aim for >80% code coverage
- 100% coverage for critical paths (validation logic)
- Document known gaps in coverage
