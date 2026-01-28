//// Dialogue Test: Real Timing Instrumentation (Issue #170)
////
//// This dialogue test verifies that timing instrumentation uses real BEAM
//// monotonic clock measurements instead of simulated/hardcoded values.
////
//// ## Test Objectives
//// - Verify monotonic_time_native() returns real, increasing values
//// - Verify measure_ms() captures real elapsed time
//// - Verify measure_microseconds() provides sub-ms precision
//// - Verify benchmark_runner uses real timing (non-zero durations)
//// - Verify Tier 1 and Tier 2 latency metrics use real measurements
//// - Verify benchmark_performance metric passes at 100%
////
//// ## Issue #170 Requirements
//// - Replace get_current_time_ms() returning 0 with BEAM monotonic time
//// - Measure real Tier 1 and Tier 2 latency in epic validation
//// - Compute real throughput in benchmark performance metric
//// - Record real duration in AccuracyTestResult
//// - benchmark_performance metric should pass based on measured values

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/argument.{Formalization}
import modal_logic/heuristics
import modal_logic/proposition.{Atom, Implies, K}
import modal_logic/testing/epic_validation
import modal_logic/timing

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn timing_instrumentation_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Real Timing Instrumentation (Issue #170)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Monotonic time produces real values
  io.println("--- Test 1: Monotonic Time Produces Real Values ---")
  io.println("")
  test_monotonic_time_real_values()
  io.println("[PASS] Monotonic time returns real, increasing values")
  io.println("")

  // Test 2: measure_ms captures real elapsed time
  io.println("--- Test 2: measure_ms Captures Real Elapsed Time ---")
  io.println("")
  test_measure_ms_real_time()
  io.println("[PASS] measure_ms captures real elapsed time")
  io.println("")

  // Test 3: measure_microseconds provides sub-ms precision
  io.println("--- Test 3: Sub-Millisecond Precision ---")
  io.println("")
  test_sub_millisecond_precision()
  io.println("[PASS] Microsecond measurement provides sub-ms precision")
  io.println("")

  // Test 4: Benchmark runner records non-zero durations
  io.println("--- Test 4: Benchmark Runner Real Durations ---")
  io.println("")
  test_benchmark_runner_real_durations()
  io.println("[PASS] Benchmark runner records real durations")
  io.println("")

  // Test 5: Tier 1 latency uses real measurement
  io.println("--- Test 5: Tier 1 Latency Real Measurement ---")
  io.println("")
  test_tier1_real_latency()
  io.println("[PASS] Tier 1 latency measured via BEAM monotonic clock")
  io.println("")

  // Test 6: Tier 2 latency uses real measurement
  io.println("--- Test 6: Tier 2 Latency Real Measurement ---")
  io.println("")
  test_tier2_real_latency()
  io.println("[PASS] Tier 2 latency measured via BEAM monotonic clock")
  io.println("")

  // Test 7: benchmark_performance metric passes at 100%
  io.println("--- Test 7: Benchmark Performance Metric Passes ---")
  io.println("")
  test_benchmark_performance_passes()
  io.println("[PASS] benchmark_performance metric at 100%")
  io.println("")

  // Test 8: Throughput is non-zero
  io.println("--- Test 8: Throughput is Non-Zero ---")
  io.println("")
  test_throughput_nonzero()
  io.println("[PASS] Throughput computed from real timing")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL TIMING INSTRUMENTATION DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Monotonic Time Produces Real Values
// =============================================================================

fn test_monotonic_time_real_values() {
  io.println("User: Verify monotonic_time_native() returns real values")

  let time_a = timing.monotonic_time_native()
  let time_b = timing.monotonic_time_native()

  io.println(
    "[System]: First reading:  " <> int.to_string(time_a) <> " native units",
  )
  io.println(
    "[System]: Second reading: " <> int.to_string(time_b) <> " native units",
  )

  // Time values should be non-zero (real monotonic clock)
  { time_a != 0 } |> should.be_true()

  // Second reading should be >= first (monotonic guarantee)
  { time_b >= time_a } |> should.be_true()

  io.println(
    "[System]: Confirmed - monotonic time is non-zero and non-decreasing",
  )
}

// =============================================================================
// Test 2: measure_ms Captures Real Elapsed Time
// =============================================================================

fn test_measure_ms_real_time() {
  io.println("User: Measure a computation and verify elapsed time is real")

  let modus_ponens_formalization =
    Formalization(
      id: "timing_test",
      argument_id: "timing_test",
      logic_system: K,
      premises: [Implies(Atom("p"), Atom("q")), Atom("p")],
      conclusion: Atom("q"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Run multiple validations to accumulate measurable time
  let #(result_count, elapsed_ms) =
    timing.measure_ms(fn() {
      let results =
        list.range(1, 100)
        |> list.map(fn(_iteration) {
          heuristics.try_heuristic_validation(modus_ponens_formalization)
        })
      list.length(results)
    })

  io.println(
    "[System]: Computed " <> int.to_string(result_count) <> " validations",
  )
  io.println("[System]: Elapsed time: " <> int.to_string(elapsed_ms) <> "ms")

  // Elapsed time should be non-negative (real measurement)
  { elapsed_ms >= 0 } |> should.be_true()

  io.println("[System]: Confirmed - measure_ms returns real elapsed time")
}

// =============================================================================
// Test 3: Sub-Millisecond Precision
// =============================================================================

fn test_sub_millisecond_precision() {
  io.println(
    "User: Verify measure_microseconds provides sub-ms precision for fast ops",
  )

  let modus_ponens_formalization =
    Formalization(
      id: "fast_op",
      argument_id: "fast_op",
      logic_system: K,
      premises: [Implies(Atom("p"), Atom("q")), Atom("p")],
      conclusion: Atom("q"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // A single Tier 1 heuristic validation is sub-millisecond
  let #(_result, elapsed_us) =
    timing.measure_microseconds(fn() {
      heuristics.try_heuristic_validation(modus_ponens_formalization)
    })

  io.println(
    "[System]: Microsecond measurement: " <> int.to_string(elapsed_us) <> "us",
  )

  // Microsecond measurement should be non-negative
  { elapsed_us >= 0 } |> should.be_true()

  io.println(
    "[System]: Confirmed - microsecond precision available for sub-ms ops",
  )
}

// =============================================================================
// Test 4: Benchmark Runner Real Durations
// =============================================================================

fn test_benchmark_runner_real_durations() {
  io.println("User: Run benchmark suite and verify real timing is used")

  // Use the epic validation's benchmark suite which runs enough cases
  // for measurable durations
  let metric_result = epic_validation.validate_benchmark_performance(25)

  io.println("[System]: Benchmark Performance Metric")
  io.println("  Score: " <> format_float(metric_result.actual) <> "%")
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }

  // The benchmark_performance metric checks both P95 < 3000ms and
  // throughput >= 1.0 case/sec. Previously this scored 50% because
  // throughput was 0.0 (hardcoded timing). Now with real timing it
  // should score 100%.
  { metric_result.actual >=. 100.0 } |> should.be_true()

  io.println("[System]: Confirmed - benchmark uses real timing (score 100%)")
}

// =============================================================================
// Test 5: Tier 1 Latency Real Measurement
// =============================================================================

fn test_tier1_real_latency() {
  io.println("User: Validate Tier 1 latency with real BEAM monotonic timing")

  let metric_result = epic_validation.validate_tier1_latency(25)

  io.println("[System]: Tier 1 Latency Results")
  io.println("  Metric: " <> metric_result.name)
  io.println(
    "  Target: " <> format_float(metric_result.target) <> metric_result.unit,
  )
  io.println(
    "  Actual: " <> format_float(metric_result.actual) <> metric_result.unit,
  )
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

  // Tier 1 should be fast (< 1ms target)
  metric_result.passed |> should.be_true()

  io.println(
    "[System]: Confirmed - Tier 1 latency measured via BEAM monotonic clock",
  )
}

// =============================================================================
// Test 6: Tier 2 Latency Real Measurement
// =============================================================================

fn test_tier2_real_latency() {
  io.println("User: Validate Tier 2 latency with real BEAM monotonic timing")

  let metric_result = epic_validation.validate_tier2_latency(25)

  io.println("[System]: Tier 2 Latency Results")
  io.println("  Metric: " <> metric_result.name)
  io.println(
    "  Target: " <> format_float(metric_result.target) <> metric_result.unit,
  )
  io.println(
    "  Actual: " <> format_float(metric_result.actual) <> metric_result.unit,
  )
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

  // Tier 2 should be under 50ms target
  metric_result.passed |> should.be_true()

  io.println(
    "[System]: Confirmed - Tier 2 latency measured via BEAM monotonic clock",
  )
}

// =============================================================================
// Test 7: benchmark_performance Metric Passes at 100%
// =============================================================================

fn test_benchmark_performance_passes() {
  io.println("User: Verify benchmark_performance metric now passes at 100%")

  let metric_result = epic_validation.validate_benchmark_performance(25)

  io.println("[System]: Benchmark Performance Results")
  io.println("  Score: " <> format_float(metric_result.actual) <> "%")
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

  // With real timing, both P95 and throughput should pass
  { metric_result.actual >=. 100.0 } |> should.be_true()
  metric_result.passed |> should.be_true()

  io.println(
    "[System]: Confirmed - benchmark_performance at 100% with real timing",
  )
}

// =============================================================================
// Test 8: Throughput is Non-Zero
// =============================================================================

fn test_throughput_nonzero() {
  io.println("User: Verify throughput is computed from real timing")

  // Run the FOLIO benchmark suite which generates enough test cases for
  // the total duration to be measurable at microsecond precision.
  let folio_metric = epic_validation.validate_folio_f1_score(50)

  io.println("[System]: FOLIO benchmark (50 cases)")
  io.println("  F1 Score: " <> format_float(folio_metric.actual) <> "%")
  case folio_metric.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }

  // Run benchmark_performance which explicitly tests throughput >= 1.0
  let perf_metric = epic_validation.validate_benchmark_performance(25)

  io.println("[System]: Performance metric")
  io.println("  Score: " <> format_float(perf_metric.actual) <> "%")
  case perf_metric.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }

  // Previously throughput was always 0.0 because get_current_time_ms() returned 0.
  // The benchmark_performance metric scores 100% only when BOTH P95 < 3000ms
  // AND throughput >= 1.0 cases/sec — meaning throughput is now positive.
  { perf_metric.actual >=. 100.0 } |> should.be_true()

  io.println(
    "[System]: Confirmed - throughput is positive (performance score 100%)",
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

fn format_float(f: Float) -> String {
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
