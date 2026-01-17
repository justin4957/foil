//// Epic #144 Full Validation Test
////
//// Comprehensive validation of all phases for Epic #144
//// "Fast Modal Logic Checking for Prediction Accuracy Assessment"
////
//// This test produces a complete validation report suitable for
//// PR reviews and progress tracking.

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import modal_logic/testing/epic_validation.{
  type EpicPhase, type EpicProgress, type MetricResult,
  type PhaseValidationResult, PhaseA, PhaseB, PhaseC, PhaseD, PhaseE,
}

// =============================================================================
// Main Test Entry Point
// =============================================================================

pub fn main() {
  run_full_epic_validation()
}

pub fn epic_144_full_validation_test() {
  run_full_epic_validation()
}

fn run_full_epic_validation() {
  print_header()

  // Run validation for all phases
  let config = epic_validation.default_config()
  let progress = epic_validation.generate_epic_progress(config)

  // Print detailed results
  print_epic_summary(progress)
  print_phase_details(progress.phase_results)
  print_metrics_table(progress.phase_results)
  print_issues_status(progress.phase_results)
  print_validation_summary(progress)

  // Assert that we have all phases
  list.length(progress.phase_results) |> should.equal(5)

  // Assert progress calculation is reasonable
  { progress.overall_progress >=. 0.0 } |> should.be_true()
  { progress.overall_progress <=. 1.0 } |> should.be_true()

  io.println("")
}

// =============================================================================
// Header
// =============================================================================

fn print_header() {
  io.println("")
  io.println(string.repeat("=", 80))
  io.println("EPIC #144 FULL VALIDATION REPORT")
  io.println("Fast Modal Logic Checking for Prediction Accuracy Assessment")
  io.println(string.repeat("=", 80))
  io.println("")
}

// =============================================================================
// Epic Summary
// =============================================================================

fn print_epic_summary(progress: EpicProgress) {
  io.println("## Epic Summary")
  io.println("")

  let passed_phases =
    progress.phase_results
    |> list.count(fn(p) { p.passed })

  let total_phases = list.length(progress.phase_results)

  let total_metrics =
    progress.phase_results
    |> list.flat_map(fn(p) { p.metrics })
    |> list.length()

  let passed_metrics =
    progress.phase_results
    |> list.flat_map(fn(p) { p.metrics })
    |> list.count(fn(m) { m.passed })

  io.println(
    "| Metric            | Value                                                |",
  )
  io.println(
    "|-------------------|------------------------------------------------------|",
  )
  io.println(
    "| Epic Number       | #144                                                 |",
  )
  io.println(
    "| Overall Progress  | "
    <> pad_right(float_to_percent(progress.overall_progress), 52)
    <> " |",
  )
  io.println(
    "| Phases Passed     | "
    <> pad_right(
      int.to_string(passed_phases)
        <> "/"
        <> int.to_string(total_phases)
        <> " ("
        <> float_to_percent(
        int.to_float(passed_phases) /. int.to_float(total_phases),
      )
        <> ")",
      52,
    )
    <> " |",
  )
  io.println(
    "| Metrics Passed    | "
    <> pad_right(
      int.to_string(passed_metrics)
        <> "/"
        <> int.to_string(total_metrics)
        <> " ("
        <> float_to_percent(
        int.to_float(passed_metrics) /. int.to_float(total_metrics),
      )
        <> ")",
      52,
    )
    <> " |",
  )
  io.println(
    "| All Passed        | "
    <> pad_right(
      case progress.all_passed {
        True -> "YES ✓"
        False -> "NO (in progress)"
      },
      52,
    )
    <> " |",
  )
  io.println("")
}

// =============================================================================
// Phase Details
// =============================================================================

fn print_phase_details(phases: List(PhaseValidationResult)) {
  io.println("## Phase Status")
  io.println("")

  io.println(
    "| Phase | Name                       | Status  | Metrics | Issues       |",
  )
  io.println(
    "|-------|----------------------------|---------|---------|--------------|",
  )

  list.each(phases, fn(phase) {
    let phase_letter = epic_validation.phase_to_string(phase.phase)
    let status = case phase.passed {
      True -> "PASS ✓"
      False -> "PENDING"
    }
    let passed_count = list.count(phase.metrics, fn(m) { m.passed })
    let total_count = list.length(phase.metrics)
    let metrics_str =
      int.to_string(passed_count) <> "/" <> int.to_string(total_count)

    let issues_str = case phase.completed_issues {
      [] -> "none"
      completed ->
        completed
        |> list.map(fn(i) { "#" <> int.to_string(i) })
        |> string.join(",")
    }

    io.println(
      "| "
      <> pad_right(phase_letter, 5)
      <> " | "
      <> pad_right(phase.name, 26)
      <> " | "
      <> pad_right(status, 7)
      <> " | "
      <> pad_right(metrics_str, 7)
      <> " | "
      <> pad_right(issues_str, 12)
      <> " |",
    )
  })

  io.println("")
}

// =============================================================================
// Metrics Table
// =============================================================================

fn print_metrics_table(phases: List(PhaseValidationResult)) {
  io.println("## All Metrics")
  io.println("")

  list.each(phases, fn(phase) {
    let phase_letter = epic_validation.phase_to_string(phase.phase)
    io.println("### Phase " <> phase_letter <> ": " <> phase.name)
    io.println("")

    io.println(
      "| Metric                              | Target    | Actual    | Status |",
    )
    io.println(
      "|-------------------------------------|-----------|-----------|--------|",
    )

    list.each(phase.metrics, fn(m) {
      let status = case m.passed {
        True -> "PASS ✓"
        False -> "FAIL"
      }
      let target_str = float_to_string(m.target) <> m.unit
      let actual_str = float_to_string(m.actual) <> m.unit

      io.println(
        "| "
        <> pad_right(m.name, 35)
        <> " | "
        <> pad_right(target_str, 9)
        <> " | "
        <> pad_right(actual_str, 9)
        <> " | "
        <> pad_right(status, 6)
        <> " |",
      )
    })

    io.println("")
  })
}

// =============================================================================
// Issues Status
// =============================================================================

fn print_issues_status(phases: List(PhaseValidationResult)) {
  io.println("## Issue Tracking")
  io.println("")

  let all_issues =
    phases
    |> list.flat_map(fn(p) { p.issues })
    |> list.unique()

  let completed_issues =
    phases
    |> list.flat_map(fn(p) { p.completed_issues })
    |> list.unique()

  let pending_issues =
    all_issues
    |> list.filter(fn(i) { !list.contains(completed_issues, i) })

  io.println(
    "| Status    | Issues                                                       |",
  )
  io.println(
    "|-----------|--------------------------------------------------------------|",
  )

  let completed_str = case completed_issues {
    [] -> "none"
    issues ->
      issues
      |> list.map(fn(i) { "#" <> int.to_string(i) })
      |> string.join(", ")
  }

  let pending_str = case pending_issues {
    [] -> "none"
    issues ->
      issues
      |> list.map(fn(i) { "#" <> int.to_string(i) })
      |> string.join(", ")
  }

  io.println("| Completed | " <> pad_right(completed_str, 60) <> " |")
  io.println("| Pending   | " <> pad_right(pending_str, 60) <> " |")
  io.println("")

  io.println(
    "Total: "
    <> int.to_string(list.length(completed_issues))
    <> " completed, "
    <> int.to_string(list.length(pending_issues))
    <> " pending, "
    <> int.to_string(list.length(all_issues))
    <> " total",
  )
  io.println("")
}

// =============================================================================
// Validation Summary
// =============================================================================

fn print_validation_summary(progress: EpicProgress) {
  io.println("## Validation Summary")
  io.println("")

  // Phase-by-phase summary
  list.each(progress.phase_results, fn(phase) {
    let phase_letter = epic_validation.phase_to_string(phase.phase)
    let status_icon = case phase.passed {
      True -> "✓"
      False -> "○"
    }

    io.println(
      status_icon <> " Phase " <> phase_letter <> " (" <> phase.name <> ")",
    )

    // Show passing and failing metrics
    let passing =
      phase.metrics
      |> list.filter(fn(m) { m.passed })
    let failing =
      phase.metrics
      |> list.filter(fn(m) { !m.passed })

    case failing {
      [] -> io.println("    All metrics passing")
      _ -> {
        list.each(failing, fn(m) {
          io.println(
            "    ✗ "
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
        list.each(passing, fn(m) {
          io.println(
            "    ✓ " <> m.name <> ": " <> float_to_string(m.actual) <> m.unit,
          )
        })
      }
    }
    io.println("")
  })

  // Final verdict
  io.println(string.repeat("-", 80))
  case progress.all_passed {
    True -> {
      io.println("RESULT: ALL PHASES PASSING ✓")
      io.println("Epic #144 validation: COMPLETE")
    }
    False -> {
      let passed_count =
        progress.phase_results
        |> list.count(fn(p) { p.passed })
      let total_count = list.length(progress.phase_results)
      io.println(
        "RESULT: "
        <> int.to_string(passed_count)
        <> "/"
        <> int.to_string(total_count)
        <> " phases passing",
      )
      io.println(
        "Epic #144 validation: IN PROGRESS ("
        <> float_to_percent(progress.overall_progress)
        <> " complete)",
      )
    }
  }
  io.println(string.repeat("-", 80))
}

// =============================================================================
// Helper Functions
// =============================================================================

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let frac_val = { f -. int.to_float(whole) } *. 100.0
  let frac = float_truncate(float.absolute_value(frac_val))
  int.to_string(whole) <> "." <> pad_left(int.to_string(frac), 2, "0")
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

fn pad_right(s: String, len: Int) -> String {
  let current_len = string.length(s)
  case current_len >= len {
    True -> string.slice(s, 0, len)
    False -> s <> string.repeat(" ", len - current_len)
  }
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int
