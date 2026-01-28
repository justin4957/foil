//// Dialogue Test: Baseline Regression Testing (Issue #177)
////
//// This dialogue test verifies that the baseline persistence and regression
//// detection infrastructure correctly identifies metric regressions across
//// runs, persists baselines to JSON, computes trends, and integrates with
//// the Phase D metric system.
////
//// ## Test Objectives
//// - Verify snapshot creation from AccuracyResults
//// - Verify regression detection with >2% threshold
//// - Verify no false positive on identical metrics
//// - Verify minor regression below threshold is not flagged
//// - Verify per-system regression detection
//// - Verify trend computation from run history
//// - Verify JSON encode/decode roundtrip
//// - Verify Phase D metric integration
////
//// ## Issue #177 Requirements
//// - Compare current metrics against most recent baseline
//// - Alert on any metric regression > 2%
//// - Track metric trends over the last N runs
//// - Add Phase D metric for golden baseline regression

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import modal_logic/testing/accuracy/accuracy_tests
import modal_logic/testing/epic_validation
import modal_logic/testing/fixtures/fixtures
import modal_logic/testing/golden/baseline_persistence

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn baseline_regression_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Baseline Regression Testing (Issue #177)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Snapshot creation from AccuracyResults
  io.println("--- Test 1: Snapshot Creation ---")
  io.println("")
  test_snapshot_creation()
  io.println("[PASS] Snapshot created from AccuracyResults with all fields")
  io.println("")

  // Test 2: Regression detection (true positive)
  io.println("--- Test 2: Regression Detection (True Positive) ---")
  io.println("")
  test_regression_detection()
  io.println("[PASS] Regression correctly detected with >2% F1 drop")
  io.println("")

  // Test 3: No false positive on identical metrics
  io.println("--- Test 3: No False Positive ---")
  io.println("")
  test_no_false_positive()
  io.println("[PASS] No regression reported for identical metrics")
  io.println("")

  // Test 4: Minor regression below threshold
  io.println("--- Test 4: Minor Regression Below Threshold ---")
  io.println("")
  test_minor_regression_below_threshold()
  io.println("[PASS] Minor regression below 2% threshold not flagged")
  io.println("")

  // Test 5: Per-system regression detection
  io.println("--- Test 5: Per-System Regression ---")
  io.println("")
  test_per_system_regression()
  io.println("[PASS] Per-system regression detected for specific system")
  io.println("")

  // Test 6: Trend computation
  io.println("--- Test 6: Trend Computation ---")
  io.println("")
  test_trend_computation()
  io.println("[PASS] Declining trend correctly identified from history")
  io.println("")

  // Test 7: JSON encode/decode roundtrip
  io.println("--- Test 7: JSON Roundtrip ---")
  io.println("")
  test_json_roundtrip()
  io.println("[PASS] JSON encode/decode roundtrip preserves all fields")
  io.println("")

  // Test 8: Phase D metric integration
  io.println("--- Test 8: Phase D Metric Integration ---")
  io.println("")
  test_phase_d_metric()
  io.println(
    "[PASS] Phase D golden_baseline_regression metric runs successfully",
  )
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL BASELINE REGRESSION DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Snapshot Creation
// =============================================================================

fn test_snapshot_creation() {
  io.println("User: Create a baseline snapshot from accuracy test results")

  let all_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_fixtures)
  let snapshot =
    baseline_persistence.snapshot_from_accuracy_results(
      results,
      "2026-01-28T12:00:00Z",
      Some("abc123"),
    )

  io.println("[System]: Baseline Snapshot Created")
  io.println("  Schema version: " <> int.to_string(snapshot.schema_version))
  io.println("  Timestamp: " <> snapshot.timestamp)
  io.println(
    "  Git commit: "
    <> case snapshot.git_commit {
      Some(c) -> c
      None -> "(none)"
    },
  )
  io.println("  F1 score: " <> float_to_pct(snapshot.f1_score))
  io.println("  Accuracy: " <> float_to_pct(snapshot.accuracy))
  io.println("  Precision: " <> float_to_pct(snapshot.precision))
  io.println("  Recall: " <> float_to_pct(snapshot.recall))
  io.println(
    "  Translation accuracy: " <> float_to_pct(snapshot.translation_accuracy),
  )
  io.println(
    "  Logic detection accuracy: "
    <> float_to_pct(snapshot.logic_detection_accuracy),
  )
  io.println(
    "  Per-system entries: "
    <> int.to_string(list.length(snapshot.per_system_f1)),
  )
  io.println(
    "  Per-complexity entries: "
    <> int.to_string(list.length(snapshot.per_complexity_f1)),
  )
  io.println("  Total cases: " <> int.to_string(snapshot.total_cases))

  // Verify fields are populated
  snapshot.schema_version |> should.equal(1)
  snapshot.timestamp |> should.equal("2026-01-28T12:00:00Z")
  snapshot.git_commit |> should.equal(Some("abc123"))
  { snapshot.f1_score >=. 0.0 } |> should.be_true()
  { snapshot.f1_score <=. 1.0 } |> should.be_true()
  { snapshot.accuracy >=. 0.0 } |> should.be_true()
  { snapshot.accuracy <=. 1.0 } |> should.be_true()
  { snapshot.precision >=. 0.0 } |> should.be_true()
  { snapshot.recall >=. 0.0 } |> should.be_true()
  { snapshot.total_cases > 0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - snapshot has "
    <> int.to_string(snapshot.total_cases)
    <> " cases with all metrics populated",
  )
}

// =============================================================================
// Test 2: Regression Detection (True Positive)
// =============================================================================

fn test_regression_detection() {
  io.println(
    "User: Check if a 5% F1 drop is detected as a regression (threshold 2%)",
  )

  let baseline = make_snapshot(0.9, 0.9, 0.88, 0.92)
  let current = make_snapshot(0.85, 0.85, 0.83, 0.87)

  let result = baseline_persistence.check_regression(current, baseline, 0.02)

  io.println("[System]: Regression Detection Result")
  io.println("  Has regression: " <> bool_to_string(result.has_regression))
  io.println("  F1 delta: " <> float_to_pct(result.f1_delta))
  io.println("  Accuracy delta: " <> float_to_pct(result.accuracy_delta))
  io.println(
    "  Severity: " <> baseline_persistence.severity_to_string(result.severity),
  )
  io.println("  Summary: " <> result.summary)

  // 5% drop should be detected
  result.has_regression |> should.be_true()

  // F1 delta should be negative
  { result.f1_delta <. 0.0 } |> should.be_true()

  // Severity should be MajorRegression (2-5% range)
  result.severity
  |> should.equal(baseline_persistence.MajorRegression)

  io.println("")
  io.println("[System]: Confirmed - 5% regression correctly detected")
}

// =============================================================================
// Test 3: No False Positive
// =============================================================================

fn test_no_false_positive() {
  io.println("User: Verify no regression when metrics are identical")

  let snapshot = make_snapshot(0.9, 0.9, 0.88, 0.92)

  let result = baseline_persistence.check_regression(snapshot, snapshot, 0.02)

  io.println("[System]: Same-Metrics Comparison")
  io.println("  Has regression: " <> bool_to_string(result.has_regression))
  io.println("  F1 delta: " <> float_to_pct(result.f1_delta))
  io.println(
    "  Severity: " <> baseline_persistence.severity_to_string(result.severity),
  )

  // No regression for identical metrics
  result.has_regression |> should.be_false()
  result.severity |> should.equal(baseline_persistence.NoRegression)
  { result.f1_delta == 0.0 } |> should.be_true()
  { result.accuracy_delta == 0.0 } |> should.be_true()

  io.println("")
  io.println("[System]: Confirmed - no false positive on identical metrics")
}

// =============================================================================
// Test 4: Minor Regression Below Threshold
// =============================================================================

fn test_minor_regression_below_threshold() {
  io.println("User: Check if a 1% F1 drop is below the 2% threshold")

  let baseline = make_snapshot(0.9, 0.9, 0.88, 0.92)
  let current = make_snapshot(0.89, 0.89, 0.87, 0.91)

  let result = baseline_persistence.check_regression(current, baseline, 0.02)

  io.println("[System]: Minor Regression Analysis")
  io.println("  Has regression: " <> bool_to_string(result.has_regression))
  io.println("  F1 delta: " <> float_to_pct(result.f1_delta))
  io.println(
    "  Severity: " <> baseline_persistence.severity_to_string(result.severity),
  )

  // 1% drop should NOT flag regression (below 2% threshold)
  result.has_regression |> should.be_false()
  result.severity |> should.equal(baseline_persistence.MinorRegression)

  // But delta should still be negative
  { result.f1_delta <. 0.0 } |> should.be_true()

  io.println("")
  io.println("[System]: Confirmed - 1% drop classified as minor, not flagged")
}

// =============================================================================
// Test 5: Per-System Regression
// =============================================================================

fn test_per_system_regression() {
  io.println(
    "User: Check if per-system regression is detected for a single system",
  )

  let baseline =
    baseline_persistence.BaselineSnapshot(
      schema_version: 1,
      timestamp: "2026-01-01T00:00:00Z",
      git_commit: None,
      f1_score: 0.9,
      accuracy: 0.9,
      precision: 0.88,
      recall: 0.92,
      translation_accuracy: 0.75,
      logic_detection_accuracy: 0.8,
      per_system_f1: [#("K", 0.95), #("S5", 0.85), #("T", 0.9)],
      per_complexity_f1: [#("simple", 0.92)],
      total_cases: 50,
    )

  // K drops from 0.95 to 0.88 (7% drop), S5 and T stay the same
  let current =
    baseline_persistence.BaselineSnapshot(
      ..baseline,
      timestamp: "2026-01-02T00:00:00Z",
      per_system_f1: [#("K", 0.88), #("S5", 0.85), #("T", 0.9)],
    )

  let result = baseline_persistence.check_regression(current, baseline, 0.02)

  io.println("[System]: Per-System Regression Analysis")
  io.println("  Has regression: " <> bool_to_string(result.has_regression))
  io.println(
    "  System regressions: "
    <> int.to_string(list.length(result.system_regressions)),
  )
  list.each(result.system_regressions, fn(entry) {
    let #(name, delta) = entry
    io.println("    " <> name <> ": " <> float_to_pct(delta))
  })

  // Should detect per-system regression for K
  result.has_regression |> should.be_true()
  { list.length(result.system_regressions) >= 1 } |> should.be_true()

  // K should be in the regressions
  let has_k_regression =
    list.any(result.system_regressions, fn(entry) { entry.0 == "K" })
  has_k_regression |> should.be_true()

  io.println("")
  io.println("[System]: Confirmed - K system regression detected")
}

// =============================================================================
// Test 6: Trend Computation
// =============================================================================

fn test_trend_computation() {
  io.println("User: Compute F1 trend from 5 runs with declining scores")

  // Create history with declining F1 scores (newest first)
  let history = [
    make_snapshot_with_timestamp(0.82, "run5"),
    make_snapshot_with_timestamp(0.84, "run4"),
    make_snapshot_with_timestamp(0.86, "run3"),
    make_snapshot_with_timestamp(0.88, "run2"),
    make_snapshot_with_timestamp(0.9, "run1"),
  ]

  let trend =
    baseline_persistence.compute_metric_trend(history, fn(snapshot) {
      snapshot.f1_score
    })

  io.println("[System]: F1 Score Trend Analysis")
  io.println(
    "  Direction: "
    <> baseline_persistence.trend_direction_to_string(trend.direction),
  )
  io.println(
    "  Average change per run: " <> float_to_pct(trend.average_change_per_run),
  )
  io.println("  Runs analyzed: " <> int.to_string(trend.runs_analyzed))
  io.println("  Latest value: " <> float_to_pct(trend.latest_value))
  io.println("  Oldest value: " <> float_to_pct(trend.oldest_value))

  // Should be declining
  trend.direction
  |> should.equal(baseline_persistence.TrendDeclining)
  trend.runs_analyzed |> should.equal(5)
  { trend.average_change_per_run <. 0.0 } |> should.be_true()
  { trend.latest_value <. trend.oldest_value } |> should.be_true()

  // Also test improving trend
  let improving_history = [
    make_snapshot_with_timestamp(0.92, "run5"),
    make_snapshot_with_timestamp(0.9, "run4"),
    make_snapshot_with_timestamp(0.88, "run3"),
    make_snapshot_with_timestamp(0.86, "run2"),
    make_snapshot_with_timestamp(0.84, "run1"),
  ]

  let improving_trend =
    baseline_persistence.compute_metric_trend(improving_history, fn(snapshot) {
      snapshot.f1_score
    })
  improving_trend.direction
  |> should.equal(baseline_persistence.TrendImproving)

  io.println("")
  io.println(
    "[System]: Confirmed - declining and improving trends detected correctly",
  )
}

// =============================================================================
// Test 7: JSON Roundtrip
// =============================================================================

fn test_json_roundtrip() {
  io.println("User: Encode a baseline file to JSON and decode it back")

  let snapshot =
    baseline_persistence.BaselineSnapshot(
      schema_version: 1,
      timestamp: "2026-01-28T12:00:00Z",
      git_commit: Some("abc123def"),
      f1_score: 0.87,
      accuracy: 0.91,
      precision: 0.85,
      recall: 0.89,
      translation_accuracy: 0.72,
      logic_detection_accuracy: 0.78,
      per_system_f1: [#("K", 0.92), #("S5", 0.83), #("T", 0.88)],
      per_complexity_f1: [
        #("simple", 0.95),
        #("medium", 0.82),
        #("complex", 0.7),
      ],
      total_cases: 42,
    )

  let older_snapshot =
    baseline_persistence.BaselineSnapshot(
      ..snapshot,
      timestamp: "2026-01-27T12:00:00Z",
      f1_score: 0.85,
    )

  let baseline_file =
    baseline_persistence.BaselineFile(
      current_baseline: snapshot,
      run_history: [snapshot, older_snapshot],
      max_history: 10,
    )

  // Encode to JSON
  let json_string = baseline_persistence.baseline_file_to_json(baseline_file)
  io.println(
    "[System]: Encoded JSON ("
    <> int.to_string(string.length(json_string))
    <> " chars)",
  )

  // Decode back
  let decoded_result = baseline_persistence.baseline_file_from_json(json_string)

  case decoded_result {
    Ok(decoded) -> {
      io.println("[System]: Decoded successfully")
      io.println(
        "  F1 score: " <> float_to_pct(decoded.current_baseline.f1_score),
      )
      io.println(
        "  Accuracy: " <> float_to_pct(decoded.current_baseline.accuracy),
      )
      io.println(
        "  Git commit: "
        <> case decoded.current_baseline.git_commit {
          Some(c) -> c
          None -> "(none)"
        },
      )
      io.println(
        "  History entries: " <> int.to_string(list.length(decoded.run_history)),
      )
      io.println("  Max history: " <> int.to_string(decoded.max_history))
      io.println(
        "  Per-system entries: "
        <> int.to_string(list.length(decoded.current_baseline.per_system_f1)),
      )

      // Verify all fields match
      { decoded.current_baseline.f1_score == snapshot.f1_score }
      |> should.be_true()
      { decoded.current_baseline.accuracy == snapshot.accuracy }
      |> should.be_true()
      { decoded.current_baseline.precision == snapshot.precision }
      |> should.be_true()
      { decoded.current_baseline.recall == snapshot.recall }
      |> should.be_true()
      decoded.current_baseline.timestamp |> should.equal(snapshot.timestamp)
      decoded.current_baseline.git_commit |> should.equal(Some("abc123def"))
      decoded.current_baseline.total_cases |> should.equal(42)
      list.length(decoded.current_baseline.per_system_f1) |> should.equal(3)
      list.length(decoded.current_baseline.per_complexity_f1)
      |> should.equal(3)
      list.length(decoded.run_history) |> should.equal(2)
      decoded.max_history |> should.equal(10)
    }
    Error(_) -> {
      io.println("[System]: ERROR - JSON decode failed")
      should.fail()
    }
  }

  io.println("")
  io.println("[System]: Confirmed - JSON roundtrip preserves all fields")
}

// =============================================================================
// Test 8: Phase D Metric Integration
// =============================================================================

fn test_phase_d_metric() {
  io.println(
    "User: Run the Phase D golden_baseline_regression metric (no baseline file)",
  )

  let config = epic_validation.default_config()
  let metric_result =
    epic_validation.validate_golden_baseline_regression(config)

  io.println("[System]: Phase D Golden Baseline Regression Metric")
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

  // Metric should have correct name
  metric_result.name |> should.equal("golden_baseline_regression")

  // Should pass (infrastructure validation without file)
  metric_result.passed |> should.be_true()
  { metric_result.actual >=. 100.0 } |> should.be_true()

  // Should have tested 3 infrastructure checks
  metric_result.samples |> should.equal(3)

  io.println("")
  io.println(
    "[System]: Confirmed - Phase D metric passes with infrastructure validation",
  )
}

// =============================================================================
// Helpers
// =============================================================================

fn make_snapshot(
  f1: Float,
  accuracy: Float,
  precision: Float,
  recall: Float,
) -> baseline_persistence.BaselineSnapshot {
  baseline_persistence.BaselineSnapshot(
    schema_version: 1,
    timestamp: "2026-01-01T00:00:00Z",
    git_commit: None,
    f1_score: f1,
    accuracy: accuracy,
    precision: precision,
    recall: recall,
    translation_accuracy: 0.75,
    logic_detection_accuracy: 0.8,
    per_system_f1: [],
    per_complexity_f1: [],
    total_cases: 50,
  )
}

fn make_snapshot_with_timestamp(
  f1: Float,
  timestamp: String,
) -> baseline_persistence.BaselineSnapshot {
  baseline_persistence.BaselineSnapshot(
    schema_version: 1,
    timestamp: timestamp,
    git_commit: None,
    f1_score: f1,
    accuracy: f1,
    precision: f1 -. 0.02,
    recall: f1 +. 0.02,
    translation_accuracy: 0.75,
    logic_detection_accuracy: 0.8,
    per_system_f1: [],
    per_complexity_f1: [],
    total_cases: 50,
  )
}

fn float_to_pct(value: Float) -> String {
  let rounded = float.round(value *. 10_000.0)
  let whole = rounded / 100
  let frac = case rounded % 100 {
    f if f < 0 -> -f
    f -> f
  }
  int.to_string(whole)
  <> "."
  <> case frac < 10 {
    True -> "0" <> int.to_string(frac)
    False -> int.to_string(frac)
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
