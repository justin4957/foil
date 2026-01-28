//// Dialogue Test: Curated Ground-Truth Fixtures (Issue #174)
////
//// This dialogue test verifies that curated ground-truth test fixtures
//// replace synthetic generated data for measuring accuracy against real
//// logic problems.
////
//// ## Test Objectives
//// - Verify at least 50 curated ground-truth fixtures exist
//// - Verify all 10 categories are covered (propositional, modal K/T/S4/S5,
////   deontic, epistemic, cross-system, deep nesting, tier boundary)
//// - Verify each fixture has a documented source
//// - Verify curated fixtures run through the accuracy pipeline
//// - Verify per-system and per-complexity breakdown from curated cases
//// - Verify curated accuracy metric integrates into Phase D validation
//// - Verify both valid and invalid fixtures are present per system
////
//// ## Issue #174 Requirements
//// - Create 50+ independently verified test cases
//// - Wire curated fixtures into epic validation
//// - Report accuracy separately for curated vs generated cases

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import modal_logic/testing/accuracy/accuracy_tests
import modal_logic/testing/epic_validation
import modal_logic/testing/fixtures/ground_truth
import modal_logic/testing/test_config.{
  ExpectedEither, ExpectedInvalid, ExpectedValid, Unknown,
}

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn ground_truth_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Curated Ground-Truth Fixtures (Issue #174)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: At least 50 curated ground-truth fixtures
  io.println("--- Test 1: Ground-Truth Fixture Count ---")
  io.println("")
  test_fixture_count()
  io.println("[PASS] At least 50 curated ground-truth fixtures exist")
  io.println("")

  // Test 2: All 10 categories are covered
  io.println("--- Test 2: Category Coverage ---")
  io.println("")
  test_category_coverage()
  io.println("[PASS] All 10 fixture categories are covered")
  io.println("")

  // Test 3: Each fixture has a documented source
  io.println("--- Test 3: Source Documentation ---")
  io.println("")
  test_source_documentation()
  io.println("[PASS] All fixtures have documented sources")
  io.println("")

  // Test 4: Curated fixtures produce accuracy results
  io.println("--- Test 4: Curated Accuracy Pipeline ---")
  io.println("")
  test_curated_accuracy_pipeline()
  io.println("[PASS] Curated fixtures run through accuracy pipeline")
  io.println("")

  // Test 5: Per-system breakdown from curated fixtures
  io.println("--- Test 5: Curated Per-System Breakdown ---")
  io.println("")
  test_curated_per_system_breakdown()
  io.println("[PASS] Per-system breakdown from curated fixtures populated")
  io.println("")

  // Test 6: Per-complexity breakdown from curated fixtures
  io.println("--- Test 6: Curated Per-Complexity Breakdown ---")
  io.println("")
  test_curated_per_complexity_breakdown()
  io.println("[PASS] Per-complexity breakdown from curated fixtures populated")
  io.println("")

  // Test 7: Both valid and invalid fixtures per system
  io.println("--- Test 7: Valid and Invalid Per System ---")
  io.println("")
  test_valid_and_invalid_per_system()
  io.println("[PASS] Valid and invalid fixtures present across systems")
  io.println("")

  // Test 8: Phase D curated accuracy metric runs
  io.println("--- Test 8: Phase D Curated Accuracy Metric ---")
  io.println("")
  test_phase_d_curated_metric()
  io.println("[PASS] Phase D curated accuracy metric runs successfully")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL GROUND-TRUTH FIXTURE DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Ground-Truth Fixture Count
// =============================================================================

fn test_fixture_count() {
  io.println("User: How many curated ground-truth fixtures are available?")

  let fixtures = ground_truth.all_ground_truth_fixtures()
  let fixture_count = list.length(fixtures)

  io.println(
    "[System]: Ground-truth fixture count: " <> int.to_string(fixture_count),
  )

  // Must have at least 50 curated fixtures
  { fixture_count >= 50 } |> should.be_true()

  io.println(
    "[System]: Confirmed - "
    <> int.to_string(fixture_count)
    <> " curated fixtures (>= 50 required)",
  )
}

// =============================================================================
// Test 2: Category Coverage
// =============================================================================

fn test_category_coverage() {
  io.println("User: Do the curated fixtures cover all expected categories?")

  let fixtures = ground_truth.all_ground_truth_fixtures()

  // Check that we have fixtures with tags from each category
  let expected_tag_groups = [
    "propositional",
    "k",
    "t",
    "s4",
    "s5",
    "deontic",
    "epistemic",
    "cross-system",
    "deep-nesting",
    "tier-boundary",
  ]

  let covered_groups =
    expected_tag_groups
    |> list.filter(fn(tag_group) {
      list.any(fixtures, fn(fixture) {
        list.any(fixture.tags, fn(tag) { tag == tag_group })
      })
    })

  let coverage_count = list.length(covered_groups)
  let expected_count = list.length(expected_tag_groups)

  io.println("[System]: Category coverage:")
  list.each(expected_tag_groups, fn(tag_group) {
    let present = list.any(covered_groups, fn(covered) { covered == tag_group })
    let status = case present {
      True -> "PRESENT"
      False -> "MISSING"
    }
    io.println("  " <> tag_group <> ": " <> status)
  })

  // All categories must be covered
  coverage_count |> should.equal(expected_count)

  io.println("")
  io.println(
    "[System]: Confirmed - all "
    <> int.to_string(coverage_count)
    <> " categories covered",
  )
}

// =============================================================================
// Test 3: Source Documentation
// =============================================================================

fn test_source_documentation() {
  io.println("User: Does every fixture have a documented source/justification?")

  let fixtures = ground_truth.all_ground_truth_fixtures()

  let fixtures_with_source =
    list.filter(fixtures, fn(fixture) {
      case fixture.source {
        Some(source) -> string.length(source) > 0
        None -> False
      }
    })

  let sourced_count = list.length(fixtures_with_source)
  let total_count = list.length(fixtures)

  io.println(
    "[System]: Fixtures with source: "
    <> int.to_string(sourced_count)
    <> " / "
    <> int.to_string(total_count),
  )

  // All fixtures must have a source
  sourced_count |> should.equal(total_count)

  // Show a few example sources
  let sample_sources =
    fixtures
    |> list.take(3)
    |> list.map(fn(fixture) {
      case fixture.source {
        Some(source) -> "  " <> fixture.id <> ": \"" <> source <> "\""
        None -> "  " <> fixture.id <> ": (none)"
      }
    })

  io.println("[System]: Example sources:")
  list.each(sample_sources, fn(source_line) { io.println(source_line) })

  io.println("")
  io.println(
    "[System]: Confirmed - all "
    <> int.to_string(total_count)
    <> " fixtures have documented sources",
  )
}

// =============================================================================
// Test 4: Curated Accuracy Pipeline
// =============================================================================

fn test_curated_accuracy_pipeline() {
  io.println("User: Run the accuracy pipeline on curated ground-truth fixtures")

  let fixtures = ground_truth.all_ground_truth_fixtures()
  let results = accuracy_tests.run_accuracy_tests(fixtures)

  io.println("[System]: Curated accuracy results:")
  io.println("  Total tests: " <> int.to_string(results.overall.total_tests))
  io.println("  TP: " <> int.to_string(results.validation.true_positives))
  io.println("  TN: " <> int.to_string(results.validation.true_negatives))
  io.println("  FP: " <> int.to_string(results.validation.false_positives))
  io.println("  FN: " <> int.to_string(results.validation.false_negatives))
  io.println(
    "  F1: "
    <> int.to_string(float.round(results.validation.f1_score *. 100.0))
    <> "%",
  )
  io.println("  Grade: " <> results.overall.grade)

  // Results should have processed all fixtures
  { results.overall.total_tests >= 50 } |> should.be_true()

  // Confusion matrix counts should be non-negative
  { results.validation.true_positives >= 0 } |> should.be_true()
  { results.validation.true_negatives >= 0 } |> should.be_true()
  { results.validation.false_positives >= 0 } |> should.be_true()
  { results.validation.false_negatives >= 0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - accuracy pipeline processes curated fixtures",
  )
}

// =============================================================================
// Test 5: Curated Per-System Breakdown
// =============================================================================

fn test_curated_per_system_breakdown() {
  io.println("User: Show per-system validation breakdown from curated fixtures")

  let fixtures = ground_truth.all_ground_truth_fixtures()
  let results = accuracy_tests.run_accuracy_tests(fixtures)

  let by_system = results.validation_by_system
  let system_count = list.length(by_system)

  io.println("[System]: Per-System Validation from Curated Fixtures")
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
      <> " F1="
      <> int.to_string(float.round(metrics.f1_score *. 100.0))
      <> "%",
    )
  })

  // Curated fixtures span multiple systems (K, T, S4, S5, KD, KD45 at minimum)
  { system_count >= 4 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(system_count)
    <> " systems have curated fixture data",
  )
}

// =============================================================================
// Test 6: Curated Per-Complexity Breakdown
// =============================================================================

fn test_curated_per_complexity_breakdown() {
  io.println(
    "User: Show per-complexity validation breakdown from curated fixtures",
  )

  let fixtures = ground_truth.all_ground_truth_fixtures()
  let results = accuracy_tests.run_accuracy_tests(fixtures)

  let by_complexity = results.validation_by_complexity
  let bucket_count = list.length(by_complexity)

  io.println("[System]: Per-Complexity Validation from Curated Fixtures")
  io.println("  Complexity buckets: " <> int.to_string(bucket_count))
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
      <> " F1="
      <> int.to_string(float.round(metrics.f1_score *. 100.0))
      <> "%",
    )
  })

  // Should have at least simple and medium buckets from curated fixtures
  { bucket_count >= 2 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(bucket_count)
    <> " complexity buckets from curated fixtures",
  )
}

// =============================================================================
// Test 7: Valid and Invalid Per System
// =============================================================================

fn test_valid_and_invalid_per_system() {
  io.println(
    "User: Verify curated fixtures include both valid and invalid per system",
  )

  let fixtures = ground_truth.all_ground_truth_fixtures()

  // Count valid and invalid per logic system tag
  let system_tags = ["k", "t", "s4", "s5", "kd", "kd45"]

  io.println("[System]: Valid/Invalid fixture counts per system:")

  let systems_with_both =
    system_tags
    |> list.filter(fn(system_tag) {
      let system_fixtures =
        list.filter(fixtures, fn(fixture) {
          list.any(fixture.tags, fn(tag) { tag == system_tag })
        })

      let valid_count =
        list.count(system_fixtures, fn(fixture) {
          case fixture.expected_validity {
            ExpectedValid -> True
            _ -> False
          }
        })

      let invalid_count =
        list.count(system_fixtures, fn(fixture) {
          case fixture.expected_validity {
            ExpectedInvalid(_) -> True
            _ -> False
          }
        })

      io.println(
        "  "
        <> system_tag
        <> ": valid="
        <> int.to_string(valid_count)
        <> ", invalid="
        <> int.to_string(invalid_count)
        <> ", total="
        <> int.to_string(list.length(system_fixtures)),
      )

      valid_count > 0 && invalid_count > 0
    })

  let both_count = list.length(systems_with_both)

  // At least 4 systems should have both valid and invalid curated fixtures
  { both_count >= 4 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(both_count)
    <> " systems have both valid and invalid curated fixtures",
  )
}

// =============================================================================
// Test 8: Phase D Curated Accuracy Metric
// =============================================================================

fn test_phase_d_curated_metric() {
  io.println(
    "User: Run the Phase D curated accuracy metric and verify it produces results",
  )

  let metric_result = epic_validation.validate_curated_accuracy()

  io.println("[System]: Phase D Curated Accuracy Metric:")
  io.println("  Name: " <> metric_result.name)
  io.println(
    "  Target: " <> int.to_string(float.round(metric_result.target)) <> "%",
  )
  io.println(
    "  Actual: " <> int.to_string(float.round(metric_result.actual)) <> "%",
  )
  io.println(
    "  Passed: "
    <> case metric_result.passed {
      True -> "yes"
      False -> "no"
    },
  )
  io.println("  Samples: " <> int.to_string(metric_result.samples))

  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> io.println("  Details: (none)")
  }

  // Metric should have the correct name
  metric_result.name |> should.equal("curated_ground_truth_accuracy")

  // Samples should match ground-truth count
  { metric_result.samples >= 50 } |> should.be_true()

  // Actual should be a valid percentage (0-100)
  { metric_result.actual >=. 0.0 } |> should.be_true()
  { metric_result.actual <=. 100.0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - curated accuracy metric runs with "
    <> int.to_string(metric_result.samples)
    <> " fixtures",
  )
}
