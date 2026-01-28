//// Dialogue Test: Phase D Benchmark Metrics (Issue #165)
////
//// This dialogue test verifies that benchmark metrics are properly computed
//// and meet the target thresholds defined in Epic #144.
////
//// ## Test Objectives
//// - Verify FOLIO F1 score reaches 80%+ target
//// - Verify LogiQA accuracy reaches 75%+ target
//// - Verify InPhO coverage reaches 70%+ target
//// - Verify benchmark performance metrics are computed correctly
////
//// ## Issue #165 Requirements
//// Phase D metrics must connect benchmark suites to validation infrastructure:
//// - folio_f1_score: 80%+ on FOLIO-style syllogistic reasoning
//// - logiqa_accuracy: 75%+ on LogiQA-style conditional reasoning
//// - inpho_coverage: 70%+ on InPhO-style modal/philosophy patterns
//// - benchmark_performance: P95 latency under 3000ms

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/benchmark_runner
import modal_logic/heuristics
import modal_logic/testing/epic_validation

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn benchmark_metrics_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Phase D Benchmark Metrics (Issue #165)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: FOLIO F1 Score
  io.println("--- Test 1: FOLIO F1 Score Validation ---")
  io.println("")
  test_folio_f1_score()
  io.println("[PASS] FOLIO F1 score meets 80% target")
  io.println("")

  // Test 2: LogiQA Accuracy
  io.println("--- Test 2: LogiQA Accuracy Validation ---")
  io.println("")
  test_logiqa_accuracy()
  io.println("[PASS] LogiQA accuracy meets 75% target")
  io.println("")

  // Test 3: InPhO Coverage
  io.println("--- Test 3: InPhO Coverage Validation ---")
  io.println("")
  test_inpho_coverage()
  io.println("[PASS] InPhO coverage meets 70% target")
  io.println("")

  // Test 4: Benchmark Performance
  io.println("--- Test 4: Benchmark Performance Validation ---")
  io.println("")
  test_benchmark_performance()
  io.println("[PASS] Benchmark performance meets targets")
  io.println("")

  // Test 5: Full Phase D Validation
  io.println("--- Test 5: Full Phase D Validation ---")
  io.println("")
  test_phase_d_validation()
  io.println("[PASS] Phase D validation complete")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL BENCHMARK METRICS DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: FOLIO F1 Score
// =============================================================================

fn test_folio_f1_score() {
  io.println("User: Validate FOLIO-style F1 score with 50 test cases")

  let metric_result = epic_validation.validate_folio_f1_score(50)

  io.println("[System]: FOLIO F1 Score Validation Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Verify F1 score meets 80% target
  io.println("User: Verify F1 score is at least 80%")
  { metric_result.actual >=. 80.0 } |> should.be_true()
  io.println(
    "[System]: Confirmed - F1 score of "
    <> float_to_string(metric_result.actual)
    <> "% meets 80% target",
  )
}

// =============================================================================
// Test 2: LogiQA Accuracy
// =============================================================================

fn test_logiqa_accuracy() {
  io.println("User: Validate LogiQA-style accuracy with 50 test cases")

  let metric_result = epic_validation.validate_logiqa_accuracy(50)

  io.println("[System]: LogiQA Accuracy Validation Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Verify accuracy meets 75% target
  io.println("User: Verify accuracy is at least 75%")
  { metric_result.actual >=. 75.0 } |> should.be_true()
  io.println(
    "[System]: Confirmed - Accuracy of "
    <> float_to_string(metric_result.actual)
    <> "% meets 75% target",
  )
}

// =============================================================================
// Test 3: InPhO Coverage
// =============================================================================

fn test_inpho_coverage() {
  io.println("User: Validate InPhO-style coverage with 50 test cases")

  let metric_result = epic_validation.validate_inpho_coverage(50)

  io.println("[System]: InPhO Coverage Validation Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Verify coverage meets 70% target
  io.println("User: Verify coverage is at least 70%")
  { metric_result.actual >=. 70.0 } |> should.be_true()
  io.println(
    "[System]: Confirmed - Coverage of "
    <> float_to_string(metric_result.actual)
    <> "% meets 70% target",
  )
}

// =============================================================================
// Test 4: Benchmark Performance
// =============================================================================

fn test_benchmark_performance() {
  io.println("User: Validate benchmark performance metrics with 25 test cases")

  let metric_result = epic_validation.validate_benchmark_performance(25)

  io.println("[System]: Benchmark Performance Validation Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // With real timing instrumentation (issue #170), both P95 latency and
  // throughput are measured via BEAM monotonic clock. The benchmark_performance
  // metric should now pass at 100% since real throughput is computed.
  io.println("User: Verify P95 latency and throughput meet targets")
  metric_result.passed |> should.be_true()
  io.println(
    "[System]: Confirmed - P95 latency under 3000ms and throughput >= 1 case/sec",
  )
}

// =============================================================================
// Test 5: Full Phase D Validation
// =============================================================================

fn test_phase_d_validation() {
  io.println("User: Run full Phase D validation")

  let config = epic_validation.default_config()
  let phase_result =
    epic_validation.validate_phase(epic_validation.PhaseD, config)

  io.println("[System]: Phase D Validation Results")
  io.println("  Phase: " <> phase_result.name)
  io.println(
    "  Overall Status: "
    <> case phase_result.passed {
      True -> "PASS"
      False -> "FAIL (some metrics below target)"
    },
  )
  io.println("  Duration: " <> int.to_string(phase_result.duration_ms) <> "ms")
  io.println("")

  // Show individual metrics
  io.println("  Individual Metrics:")
  phase_result.metrics
  |> list.each(fn(m) {
    let status = case m.passed {
      True -> "[PASS]"
      False -> "[FAIL]"
    }
    io.println(
      "    "
      <> status
      <> " "
      <> m.name
      <> ": "
      <> float_to_string(m.actual)
      <> m.unit
      <> " (target: "
      <> float_to_string(m.target)
      <> m.unit
      <> ")",
    )
  })
  io.println("")

  // Count passing metrics
  let passing_count = list.count(phase_result.metrics, fn(m) { m.passed })
  let total_count = list.length(phase_result.metrics)

  io.println(
    "  Summary: "
    <> int.to_string(passing_count)
    <> "/"
    <> int.to_string(total_count)
    <> " metrics passing",
  )

  // With real timing instrumentation (issue #170), all 5 Phase D metrics
  // should now pass including benchmark_performance (previously at 50%).
  io.println("")
  io.println("User: Verify all 5 Phase D metrics are passing")
  { passing_count >= 5 } |> should.be_true()
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(passing_count)
    <> "/"
    <> int.to_string(total_count)
    <> " metrics passing, Phase D fully complete",
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let decimal = float_truncate({ f -. int_to_float(whole) } *. 100.0)
  int.to_string(whole)
  <> "."
  <> case decimal < 10 {
    True -> "0"
    False -> ""
  }
  <> int.to_string(int_abs(decimal))
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

fn int_to_float(n: Int) -> Float {
  case n {
    0 -> 0.0
    _ -> 1.0 +. int_to_float(n - 1)
  }
}

fn int_abs(n: Int) -> Int {
  case n < 0 {
    True -> 0 - n
    False -> n
  }
}
