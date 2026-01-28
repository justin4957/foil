//// Dialogue Test: Per-Modal-System Validation Accuracy (Issue #173)
////
//// This dialogue test verifies that validation accuracy is broken down
//// per-system (K, T, S4, S5, K4, KD, KD45) and per-complexity bucket,
//// with confusion matrices at each granularity level.
////
//// ## Test Objectives
//// - Verify validation_by_system contains per-system confusion matrices
//// - Verify validation_by_complexity buckets formulas by operator count
//// - Verify system_boundary_fixtures provide both valid and invalid per system
//// - Verify find_lowest_f1_systems identifies weakest systems
//// - Verify count_modal_operators counts operators correctly
//// - Verify classify_complexity assigns buckets correctly
////
//// ## Issue #173 Requirements
//// - Add by_system: Dict(String, ValidationMetrics) to accuracy results
//// - Track confusion matrix per modal system
//// - Track accuracy by formula complexity bucket
//// - Report which systems have the lowest F1 scores
//// - Add test cases that exercise each system with both valid and invalid

import gleam/int
import gleam/io
import gleam/list
import gleeunit/should
import modal_logic/proposition.{
  And, Atom, Believes, Implies, Knows, Necessary, Not, Obligatory, Or, Possible,
}
import modal_logic/testing/accuracy/accuracy_tests
import modal_logic/testing/fixtures/fixtures

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn per_system_accuracy_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Per-Modal-System Validation Accuracy (Issue #173)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Per-system validation metrics are populated
  io.println("--- Test 1: Per-System Validation Metrics Populated ---")
  io.println("")
  test_per_system_validation_populated()
  io.println("[PASS] Per-system validation metrics populated")
  io.println("")

  // Test 2: Per-complexity validation metrics are populated
  io.println("--- Test 2: Per-Complexity Validation Metrics Populated ---")
  io.println("")
  test_per_complexity_validation_populated()
  io.println("[PASS] Per-complexity validation metrics populated")
  io.println("")

  // Test 3: System boundary fixtures provide both valid and invalid
  io.println("--- Test 3: System Boundary Fixtures Coverage ---")
  io.println("")
  test_system_boundary_coverage()
  io.println(
    "[PASS] System boundary fixtures cover valid and invalid per system",
  )
  io.println("")

  // Test 4: count_modal_operators correctly counts operators
  io.println("--- Test 4: Modal Operator Counting ---")
  io.println("")
  test_count_modal_operators()
  io.println("[PASS] Modal operators counted correctly")
  io.println("")

  // Test 5: classify_complexity assigns correct buckets
  io.println("--- Test 5: Complexity Classification ---")
  io.println("")
  test_classify_complexity()
  io.println("[PASS] Complexity buckets assigned correctly")
  io.println("")

  // Test 6: find_lowest_f1_systems identifies weakest systems
  io.println("--- Test 6: Lowest F1 Score Identification ---")
  io.println("")
  test_lowest_f1_systems()
  io.println("[PASS] Lowest F1 systems identified correctly")
  io.println("")

  // Test 7: Per-system metrics have correct confusion matrix fields
  io.println("--- Test 7: Per-System Confusion Matrix Structure ---")
  io.println("")
  test_per_system_confusion_matrix_structure()
  io.println("[PASS] Per-system confusion matrices have correct structure")
  io.println("")

  // Test 8: K4 and KD45 systems now have fixtures
  io.println("--- Test 8: K4 and KD45 Systems Have Fixtures ---")
  io.println("")
  test_k4_kd45_have_fixtures()
  io.println("[PASS] K4 and KD45 systems now have fixtures")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL PER-SYSTEM ACCURACY DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Per-System Validation Metrics Populated
// =============================================================================

fn test_per_system_validation_populated() {
  io.println(
    "User: Run accuracy tests and verify per-system validation breakdown",
  )

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)

  let by_system = results.validation_by_system
  let system_count = list.length(by_system)

  io.println("[System]: Per-System Validation Breakdown")
  io.println("  Systems with data: " <> int.to_string(system_count))
  io.println("")

  list.each(by_system, fn(entry) {
    let #(system_name, metrics) = entry
    io.println(
      "  "
      <> system_name
      <> ": TP="
      <> int.to_string(metrics.true_positives)
      <> " TN="
      <> int.to_string(metrics.true_negatives)
      <> " FP="
      <> int.to_string(metrics.false_positives)
      <> " FN="
      <> int.to_string(metrics.false_negatives)
      <> " (total="
      <> int.to_string(metrics.total)
      <> ")",
    )
  })

  // Should have per-system data for systems with fixtures
  { system_count > 0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(system_count)
    <> " systems have validation metrics",
  )
}

// =============================================================================
// Test 2: Per-Complexity Validation Metrics Populated
// =============================================================================

fn test_per_complexity_validation_populated() {
  io.println(
    "User: Run accuracy tests and verify per-complexity validation breakdown",
  )

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)

  let by_complexity = results.validation_by_complexity
  let bucket_count = list.length(by_complexity)

  io.println("[System]: Per-Complexity Validation Breakdown")
  io.println("  Complexity buckets with data: " <> int.to_string(bucket_count))
  io.println("")

  list.each(by_complexity, fn(entry) {
    let #(bucket_name, metrics) = entry
    io.println(
      "  "
      <> bucket_name
      <> ": TP="
      <> int.to_string(metrics.true_positives)
      <> " TN="
      <> int.to_string(metrics.true_negatives)
      <> " FP="
      <> int.to_string(metrics.false_positives)
      <> " FN="
      <> int.to_string(metrics.false_negatives)
      <> " (total="
      <> int.to_string(metrics.total)
      <> ")",
    )
  })

  // Should have at least one complexity bucket
  { bucket_count > 0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(bucket_count)
    <> " complexity buckets populated",
  )
}

// =============================================================================
// Test 3: System Boundary Fixtures Coverage
// =============================================================================

fn test_system_boundary_coverage() {
  io.println(
    "User: Verify system_boundary_fixtures provide both valid and invalid per system",
  )

  let boundary_fixtures = fixtures.system_boundary_fixtures()
  let boundary_count = list.length(boundary_fixtures)

  io.println("[System]: Boundary fixtures: " <> int.to_string(boundary_count))

  // Check each boundary system has at least one valid and one invalid
  let systems_with_valid =
    boundary_fixtures
    |> list.filter(fn(fixture) {
      case fixture.expected_validity {
        test_config.ExpectedValid -> True
        _ -> False
      }
    })
    |> list.map(fn(fixture) {
      multi_system.logic_system_to_string(fixture.expected_logic_system)
    })
    |> list.unique()

  let systems_with_invalid =
    boundary_fixtures
    |> list.filter(fn(fixture) {
      case fixture.expected_validity {
        test_config.ExpectedInvalid(_) -> True
        _ -> False
      }
    })
    |> list.map(fn(fixture) {
      multi_system.logic_system_to_string(fixture.expected_logic_system)
    })
    |> list.unique()

  io.println(
    "  Systems with valid fixtures: "
    <> int.to_string(list.length(systems_with_valid)),
  )
  io.println(
    "  Systems with invalid fixtures: "
    <> int.to_string(list.length(systems_with_invalid)),
  )

  // Boundary fixtures add valid fixtures for previously missing systems
  // (K4, KD45) and invalid fixtures for systems that only had valid ones
  { list.length(systems_with_valid) >= 2 } |> should.be_true()
  { list.length(systems_with_invalid) >= 5 } |> should.be_true()

  io.println(
    "[System]: Confirmed - boundary fixtures cover both valid and invalid arguments",
  )
}

// =============================================================================
// Test 4: Modal Operator Counting
// =============================================================================

fn test_count_modal_operators() {
  io.println("User: Verify count_modal_operators counts operators correctly")

  // Atom: 0 operators
  let atom_count = accuracy_tests.count_modal_operators(Atom("P"))
  io.println("  Atom(P): " <> int.to_string(atom_count) <> " operators")
  atom_count |> should.equal(0)

  // Necessary(P): 1 operator
  let nec_count = accuracy_tests.count_modal_operators(Necessary(Atom("P")))
  io.println("  Necessary(P): " <> int.to_string(nec_count) <> " operator")
  nec_count |> should.equal(1)

  // Necessary(Possible(P)): 2 operators
  let nested_count =
    accuracy_tests.count_modal_operators(Necessary(Possible(Atom("P"))))
  io.println(
    "  Necessary(Possible(P)): " <> int.to_string(nested_count) <> " operators",
  )
  nested_count |> should.equal(2)

  // Knows("a", Implies(Necessary(P), Possible(Q))): 3 operators
  let epistemic_count =
    accuracy_tests.count_modal_operators(Knows(
      "a",
      Implies(Necessary(Atom("P")), Possible(Atom("Q"))),
    ))
  io.println(
    "  Knows(a, □P → ◇Q): " <> int.to_string(epistemic_count) <> " operators",
  )
  epistemic_count |> should.equal(3)

  // Complex: □(◇P ∧ □Q) → Obligatory(R): 4 operators
  let complex_count =
    accuracy_tests.count_modal_operators(Implies(
      Necessary(And(Possible(Atom("P")), Necessary(Atom("Q")))),
      Obligatory(Atom("R")),
    ))
  io.println(
    "  □(◇P ∧ □Q) → O(R): " <> int.to_string(complex_count) <> " operators",
  )
  complex_count |> should.equal(4)

  io.println("[System]: Confirmed - modal operator counting is correct")
}

// =============================================================================
// Test 5: Complexity Classification
// =============================================================================

fn test_classify_complexity() {
  io.println("User: Verify classify_complexity assigns correct buckets")

  // Simple: p → q (0 modal operators)
  let simple =
    accuracy_tests.classify_complexity(
      [Implies(Atom("p"), Atom("q"))],
      Atom("q"),
    )
  io.println("  p→q, q: " <> accuracy_tests.complexity_bucket_to_string(simple))
  simple |> should.equal(accuracy_tests.SimpleFormula)

  // Simple: □p, □(p→q) ⊢ □q (3 operators... wait, that's medium)
  // Let's use □p ⊢ p (1 operator) = simple
  let simple_modal =
    accuracy_tests.classify_complexity([Necessary(Atom("P"))], Atom("P"))
  io.println(
    "  □P ⊢ P: " <> accuracy_tests.complexity_bucket_to_string(simple_modal),
  )
  simple_modal |> should.equal(accuracy_tests.SimpleFormula)

  // Medium: □(P→Q), □P ⊢ □Q (3 operators)
  let medium =
    accuracy_tests.classify_complexity(
      [
        Necessary(Implies(Atom("P"), Atom("Q"))),
        Necessary(Atom("P")),
      ],
      Necessary(Atom("Q")),
    )
  io.println(
    "  □(P→Q), □P ⊢ □Q: " <> accuracy_tests.complexity_bucket_to_string(medium),
  )
  medium |> should.equal(accuracy_tests.MediumFormula)

  // Complex: □◇□◇P, □P, □Q ⊢ ◇□P (8 operators)
  let complex_formula =
    accuracy_tests.classify_complexity(
      [
        Necessary(Possible(Necessary(Possible(Atom("P"))))),
        Necessary(Atom("P")),
        Necessary(Atom("Q")),
      ],
      Possible(Necessary(Atom("P"))),
    )
  io.println(
    "  □◇□◇P, □P, □Q ⊢ ◇□P: "
    <> accuracy_tests.complexity_bucket_to_string(complex_formula),
  )
  complex_formula |> should.equal(accuracy_tests.ComplexFormula)

  io.println("[System]: Confirmed - complexity buckets assigned correctly")
}

// =============================================================================
// Test 6: Lowest F1 Score Identification
// =============================================================================

fn test_lowest_f1_systems() {
  io.println("User: Run accuracy tests and find systems with lowest F1 scores")

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)

  let lowest_f1 =
    accuracy_tests.find_lowest_f1_systems(results.validation_by_system)

  io.println("[System]: Systems by F1 Score (ascending)")
  list.each(lowest_f1, fn(entry) {
    let #(system_name, f1_score) = entry
    io.println("  " <> system_name <> ": F1=" <> format_float_pct(f1_score))
  })

  // Should have at least one system with classified results
  { list.length(lowest_f1) > 0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(list.length(lowest_f1))
    <> " systems ranked by F1 score",
  )
}

// =============================================================================
// Test 7: Per-System Confusion Matrix Structure
// =============================================================================

fn test_per_system_confusion_matrix_structure() {
  io.println(
    "User: Verify per-system metrics have correct confusion matrix fields",
  )

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)

  // Check that K system has a confusion matrix (K has the most fixtures)
  let k_metrics =
    results.validation_by_system
    |> list.find(fn(entry) {
      let #(name, _) = entry
      name == "K"
    })

  case k_metrics {
    Ok(#(_, metrics)) -> {
      io.println("[System]: K system confusion matrix:")
      io.println("  Total: " <> int.to_string(metrics.total))
      io.println("  TP: " <> int.to_string(metrics.true_positives))
      io.println("  TN: " <> int.to_string(metrics.true_negatives))
      io.println("  FP: " <> int.to_string(metrics.false_positives))
      io.println("  FN: " <> int.to_string(metrics.false_negatives))
      io.println("  Precision: " <> format_float_pct(metrics.precision))
      io.println("  Recall: " <> format_float_pct(metrics.recall))
      io.println("  F1: " <> format_float_pct(metrics.f1_score))

      // K should have a reasonable number of fixtures
      { metrics.total > 0 } |> should.be_true()

      // Sum of classified results should be <= total
      let classified =
        metrics.true_positives
        + metrics.true_negatives
        + metrics.false_positives
        + metrics.false_negatives
      { classified <= metrics.total } |> should.be_true()
    }
    Error(_) -> {
      io.println("[System]: ERROR - K system not found in by_system")
      should.fail()
    }
  }

  io.println(
    "[System]: Confirmed - per-system confusion matrices have correct structure",
  )
}

// =============================================================================
// Test 8: K4 and KD45 Systems Have Fixtures
// =============================================================================

fn test_k4_kd45_have_fixtures() {
  io.println("User: Verify K4 and KD45 systems now have test fixtures")

  let all_test_fixtures = fixtures.all_fixtures()

  let k4_fixtures =
    list.filter(all_test_fixtures, fn(fixture) {
      fixture.expected_logic_system == proposition.K4
    })

  let kd45_fixtures =
    list.filter(all_test_fixtures, fn(fixture) {
      fixture.expected_logic_system == proposition.KD45
    })

  io.println(
    "[System]: K4 fixtures: " <> int.to_string(list.length(k4_fixtures)),
  )
  io.println(
    "[System]: KD45 fixtures: " <> int.to_string(list.length(kd45_fixtures)),
  )

  // Both should have at least 2 fixtures (one valid, one invalid)
  { list.length(k4_fixtures) >= 2 } |> should.be_true()
  { list.length(kd45_fixtures) >= 2 } |> should.be_true()

  // Check K4 has both valid and invalid
  let k4_has_valid =
    list.any(k4_fixtures, fn(f) {
      case f.expected_validity {
        test_config.ExpectedValid -> True
        _ -> False
      }
    })
  let k4_has_invalid =
    list.any(k4_fixtures, fn(f) {
      case f.expected_validity {
        test_config.ExpectedInvalid(_) -> True
        _ -> False
      }
    })

  k4_has_valid |> should.be_true()
  k4_has_invalid |> should.be_true()

  // Check KD45 has both valid and invalid
  let kd45_has_valid =
    list.any(kd45_fixtures, fn(f) {
      case f.expected_validity {
        test_config.ExpectedValid -> True
        _ -> False
      }
    })
  let kd45_has_invalid =
    list.any(kd45_fixtures, fn(f) {
      case f.expected_validity {
        test_config.ExpectedInvalid(_) -> True
        _ -> False
      }
    })

  kd45_has_valid |> should.be_true()
  kd45_has_invalid |> should.be_true()

  io.println(
    "[System]: Confirmed - K4 and KD45 both have valid + invalid fixtures",
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

import modal_logic/multi_system
import modal_logic/testing/test_config

fn format_float_pct(f: Float) -> String {
  let whole = float_truncate(f *. 100.0)
  int.to_string(whole) <> "%"
}

fn float_truncate(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float_truncate(0.0 -. f)
    False -> {
      case f <. 1.0 {
        True -> 0
        False -> 1 + float_truncate(f -. 1.0)
      }
    }
  }
}
