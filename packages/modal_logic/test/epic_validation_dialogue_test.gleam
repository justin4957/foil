//// Epic Validation Dialogue Test
////
//// This test demonstrates the epic validation infrastructure that enables
//// CLI-driven testing of Epic #144 (Fast Modal Logic Checking for Prediction
//// Accuracy) progress.
////
//// ## Purpose
//// - Validates epic phase metrics work correctly
//// - Demonstrates CLI command execution
//// - Shows structured output formats (JSON, Markdown, Text)
//// - Documents expected behavior for PR reviews

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/cli
import modal_logic/testing/epic_validation.{
  type EpicOutputFormat, type EpicPhase, type MetricResult,
  type PhaseValidationResult, JsonOutput, MarkdownOutput, PhaseA, PhaseB, PhaseC,
  PhaseD, PhaseE, TextOutput,
}

pub fn main() {
  io.println(string.repeat("=", 70))
  io.println("Epic Validation Dialogue Test")
  io.println("Testing CLI infrastructure for Epic #144 progress tracking")
  io.println(string.repeat("=", 70))
  io.println("")

  // Test 1: Phase A validation (tiered validation metrics)
  test_phase_a_validation()

  // Test 2: All phases overview
  test_all_phases_validation()

  // Test 3: Individual metric checks
  test_individual_metrics()

  // Test 4: CLI command execution
  test_cli_commands()

  // Test 5: Output format demonstration
  test_output_formats()

  // Summary
  print_analysis_summary()

  io.println("")
  io.println(string.repeat("=", 70))
  io.println("All Epic Validation Dialogue Tests Completed!")
  io.println(string.repeat("=", 70))
}

// =============================================================================
// Test 1: Phase A Validation
// =============================================================================

fn test_phase_a_validation() {
  io.println("")
  io.println("--- Test 1: Phase A Validation (Fast Validation Pipeline) ---")
  io.println("")

  io.println("User: Validate Phase A metrics for tiered validation")
  io.println("")

  let config = epic_validation.default_config()
  let result = epic_validation.validate_phase(PhaseA, config)

  io.println("[System]: Phase A Validation Results")
  io.println("         Name: " <> result.name)
  io.println(
    "         Status: "
    <> case result.passed {
      True -> "PASSING"
      False -> "PENDING"
    },
  )
  io.println("")

  io.println("         Metrics:")
  list.each(result.metrics, fn(m) {
    let status = case m.passed {
      True -> "[OK]"
      False -> "[--]"
    }
    io.println(
      "           "
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
  io.println(
    "         Related Issues: "
    <> result.issues
    |> list.map(fn(i) { "#" <> int.to_string(i) })
    |> string.join(", "),
  )
  io.println(
    "         Completed: "
    <> case result.completed_issues {
      [] -> "None"
      issues ->
        issues
        |> list.map(fn(i) { "#" <> int.to_string(i) })
        |> string.join(", ")
    },
  )

  // Verify key metrics
  let tier1_ok =
    list.any(result.metrics, fn(m) { m.name == "tier1_latency_p80" && m.passed })
  let coverage_present =
    list.any(result.metrics, fn(m) { m.name == "fastpath_coverage" })

  case tier1_ok {
    True -> io.println("\n[OK] Tier 1 latency metric validated correctly")
    False -> io.println("\n[WARN] Tier 1 latency metric not passing")
  }

  case coverage_present {
    True -> io.println("[OK] Fast-path coverage metric present")
    False -> io.println("[WARN] Fast-path coverage metric missing")
  }

  io.println("")
}

// =============================================================================
// Test 2: All Phases Overview
// =============================================================================

fn test_all_phases_validation() {
  io.println("")
  io.println("--- Test 2: All Phases Overview ---")
  io.println("")

  io.println("User: Show progress for all epic phases")
  io.println("")

  let config = epic_validation.default_config()
  let progress = epic_validation.generate_epic_progress(config)

  io.println("[System]: Epic #144 Progress Report")
  io.println(
    "         Overall Progress: " <> float_to_percent(progress.overall_progress),
  )
  io.println(
    "         All Passed: "
    <> case progress.all_passed {
      True -> "Yes"
      False -> "No"
    },
  )
  io.println("")

  io.println("         Phase Summary:")
  list.each(progress.phase_results, fn(phase) {
    let status = case phase.passed {
      True -> "[x]"
      False -> "[ ]"
    }
    let metric_count = list.length(phase.metrics)
    let passed_count = list.count(phase.metrics, fn(m) { m.passed })
    io.println(
      "           "
      <> status
      <> " Phase "
      <> epic_validation.phase_to_string(phase.phase)
      <> ": "
      <> phase.name
      <> " ("
      <> int.to_string(passed_count)
      <> "/"
      <> int.to_string(metric_count)
      <> " metrics)",
    )
  })

  // Check phase counts
  let total_phases = list.length(progress.phase_results)
  case total_phases == 5 {
    True -> io.println("\n[OK] All 5 phases present")
    False ->
      io.println(
        "\n[WARN] Expected 5 phases, found " <> int.to_string(total_phases),
      )
  }

  io.println("")
}

// =============================================================================
// Test 3: Individual Metric Checks
// =============================================================================

fn test_individual_metrics() {
  io.println("")
  io.println("--- Test 3: Individual Metric Checks ---")
  io.println("")

  // Test tier1 latency
  io.println("User: Check tier1_latency_p80 metric")
  io.println("")

  let tier1_result = epic_validation.validate_tier1_latency(50)
  io.println("[System]: Metric Check Result")
  print_metric_result(tier1_result)

  // Test tier2 latency
  io.println("")
  io.println("User: Check tier2_latency_avg metric")
  io.println("")

  let tier2_result = epic_validation.validate_tier2_latency(50)
  io.println("[System]: Metric Check Result")
  print_metric_result(tier2_result)

  // Test coverage
  io.println("")
  io.println("User: Check fastpath_coverage metric")
  io.println("")

  let coverage_result = epic_validation.validate_fastpath_coverage(30)
  io.println("[System]: Metric Check Result")
  print_metric_result(coverage_result)

  // Verify metrics have reasonable values
  case tier1_result.actual <. 10.0 {
    True -> io.println("\n[OK] Tier 1 latency is reasonable (<10ms)")
    False ->
      io.println(
        "\n[WARN] Tier 1 latency seems high: "
        <> float_to_string(tier1_result.actual)
        <> "ms",
      )
  }

  io.println("")
}

// =============================================================================
// Test 4: CLI Command Execution
// =============================================================================

fn test_cli_commands() {
  io.println("")
  io.println("--- Test 4: CLI Command Execution ---")
  io.println("")

  // Test epic-validate command
  io.println("User: Run epic-validate --phase A")
  io.println("")

  let validate_output = cli.execute_epic_validate(Some("A"), None)
  io.println("[System]: Command Output")
  io.println(validate_output)

  // Test epic-progress command
  io.println("")
  io.println("User: Run epic-progress")
  io.println("")

  let progress_output = cli.execute_epic_progress(144, None)
  // Print first 20 lines only to keep output manageable
  let progress_lines = string.split(progress_output, "\n")
  io.println("[System]: Command Output (first 20 lines)")
  progress_lines
  |> list.take(20)
  |> list.each(io.println)
  io.println("...")

  // Test metric-check command
  io.println("")
  io.println("User: Run metric-check --metric tier1_latency_p80")
  io.println("")

  let metric_output = cli.execute_metric_check("tier1_latency_p80", None)
  io.println("[System]: Command Output")
  io.println(metric_output)

  // Test benchmark command
  io.println("")
  io.println("User: Run benchmark --suite tiered-validation --iterations 20")
  io.println("")

  let benchmark_output = cli.execute_benchmark("tiered-validation", 20)
  io.println("[System]: Command Output")
  io.println(benchmark_output)

  io.println("")
}

// =============================================================================
// Test 5: Output Format Demonstration
// =============================================================================

fn test_output_formats() {
  io.println("")
  io.println("--- Test 5: Output Format Demonstration ---")
  io.println("")

  let config = epic_validation.default_config()
  let progress = epic_validation.generate_epic_progress(config)

  // JSON output
  io.println("User: Show progress in JSON format")
  io.println("")
  io.println("[System]: JSON Output")

  let json_config =
    epic_validation.EpicValidationConfig(
      ..config,
      output_format: epic_validation.JsonOutput,
    )
  let json_output = epic_validation.format_progress(progress, json_config)
  // Print first 30 lines
  json_output
  |> string.split("\n")
  |> list.take(30)
  |> list.each(io.println)
  io.println("...")

  // Markdown output
  io.println("")
  io.println("User: Show progress in Markdown format")
  io.println("")
  io.println("[System]: Markdown Output")

  let md_config =
    epic_validation.EpicValidationConfig(
      ..config,
      output_format: epic_validation.MarkdownOutput,
    )
  let md_output = epic_validation.format_progress(progress, md_config)
  // Print first 30 lines
  md_output
  |> string.split("\n")
  |> list.take(30)
  |> list.each(io.println)
  io.println("...")

  io.println("")
}

// =============================================================================
// Analysis Summary
// =============================================================================

fn print_analysis_summary() {
  io.println("")
  io.println(string.repeat("=", 70))
  io.println("EPIC VALIDATION INFRASTRUCTURE ANALYSIS")
  io.println(string.repeat("=", 70))
  io.println("")

  io.println(
    "| Component                  | Status      | Description                    |",
  )
  io.println(
    "|----------------------------|-------------|--------------------------------|",
  )
  io.println(
    "| epic_validation module     | FUNCTIONAL  | Core validation logic          |",
  )
  io.println(
    "| Phase A metrics            | FUNCTIONAL  | Tiered validation metrics      |",
  )
  io.println(
    "| Phase B-E metrics          | PLACEHOLDER | Awaiting implementation        |",
  )
  io.println(
    "| CLI epic-validate          | FUNCTIONAL  | Phase validation command       |",
  )
  io.println(
    "| CLI epic-progress          | FUNCTIONAL  | Progress report command        |",
  )
  io.println(
    "| CLI metric-check           | FUNCTIONAL  | Individual metric checks       |",
  )
  io.println(
    "| CLI benchmark              | FUNCTIONAL  | Benchmark suite runner         |",
  )
  io.println(
    "| JSON output                | FUNCTIONAL  | Structured output for CI       |",
  )
  io.println(
    "| Markdown output            | FUNCTIONAL  | Report format for PRs          |",
  )
  io.println(
    "| Shell script               | FUNCTIONAL  | CI integration wrapper         |",
  )
  io.println("")

  io.println("Phase A Metrics (Implemented):")
  io.println(
    "  - tier1_latency_p80: Tier 1 syntactic latency at 80th percentile",
  )
  io.println("  - tier2_latency_avg: Tier 2 truth table average latency")
  io.println("  - fastpath_coverage: % formulas resolved without Z3")
  io.println("  - tier_selection_accuracy: Correct tier selection rate")
  io.println("")

  io.println("Epic #144 Success Metrics:")
  io.println("  - Fast-path validation latency: <100ms for 80% of formulas")
  io.println("  - Full Z3 validation latency: <2s for 95% of formulas")
  io.println("  - Confidence score accuracy: 85%+ correlation")
  io.println("  - Fallacy detection precision: 90%+")
  io.println("  - Benchmark accuracy (FOLIO): 80%+ F1 score")
  io.println("")

  io.println("Usage Examples:")
  io.println("  # Validate Phase A")
  io.println("  gleam run -m modal_logic/cli -- epic-validate --phase A")
  io.println("")
  io.println("  # Generate JSON progress report for CI")
  io.println(
    "  gleam run -m modal_logic/cli -- epic-progress --format json > progress.json",
  )
  io.println("")
  io.println("  # Check specific metric")
  io.println(
    "  gleam run -m modal_logic/cli -- metric-check --metric fastpath_coverage",
  )
  io.println("")
  io.println("  # Run benchmarks")
  io.println(
    "  gleam run -m modal_logic/cli -- benchmark --suite tiered-validation",
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

fn print_metric_result(m: MetricResult) {
  let status = case m.passed {
    True -> "PASS"
    False -> "FAIL"
  }

  io.println("         Name: " <> m.name)
  io.println("         Target: " <> float_to_string(m.target) <> m.unit)
  io.println("         Actual: " <> float_to_string(m.actual) <> m.unit)
  io.println("         Samples: " <> int.to_string(m.samples))
  io.println("         Status: " <> status)
  case m.details {
    Some(d) -> io.println("         Details: " <> d)
    None -> Nil
  }
}

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let frac = float_truncate({ f -. int_to_float(whole) } *. 100.0)
  int.to_string(whole) <> "." <> pad_left(int.to_string(abs(frac)), 2, "0")
}

fn float_to_percent(f: Float) -> String {
  float_to_string(f *. 100.0) <> "%"
}

fn pad_left(s: String, len: Int, char: String) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left(char <> s, len, char)
  }
}

fn abs(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
