//// Test Report Generation
////
//// This module generates comprehensive test reports in multiple formats
//// for CI/CD integration and documentation.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/testing/test_config.{
  type FixtureCategory, ClassicArgument, EdgeCase, ExternalDataset, ModalTheorem,
  RegressionCase,
}

/// A complete test report
pub type TestReport {
  TestReport(
    /// Unique identifier for this run
    run_id: String,
    /// When the test run started
    started_at: String,
    /// When the test run completed
    completed_at: String,
    /// Total duration in milliseconds
    duration_ms: Int,
    /// Summary metrics
    summary: ReportSummary,
    /// Detailed results by category
    by_category: List(CategoryReport),
    /// Comparison with baseline (if any)
    baseline_comparison: Option(BaselineComparison),
    /// Environment information
    environment: EnvironmentInfo,
  )
}

/// Summary metrics for the report
pub type ReportSummary {
  ReportSummary(
    total_tests: Int,
    passed: Int,
    failed: Int,
    skipped: Int,
    errored: Int,
    pass_rate: Float,
    avg_confidence: Float,
  )
}

/// Report for a specific category
pub type CategoryReport {
  CategoryReport(
    category: FixtureCategory,
    total: Int,
    passed: Int,
    failed: Int,
    pass_rate: Float,
    details: List(TestDetail),
  )
}

/// Details of an individual test
pub type TestDetail {
  TestDetail(
    fixture_id: String,
    fixture_name: String,
    status: TestStatus,
    duration_ms: Int,
    message: Option(String),
  )
}

/// Test status
pub type TestStatus {
  StatusPassed
  StatusFailed
  StatusSkipped
  StatusErrored
}

/// Comparison with a baseline run
pub type BaselineComparison {
  BaselineComparison(
    baseline_id: String,
    baseline_date: String,
    /// Tests that regressed (passed before, failed now)
    regressions: List(String),
    /// Tests that improved (failed before, passed now)
    improvements: List(String),
    /// New tests (not in baseline)
    new_tests: List(String),
    /// Pass rate delta
    pass_rate_delta: Float,
  )
}

/// Environment information
pub type EnvironmentInfo {
  EnvironmentInfo(
    gleam_version: String,
    platform: String,
    git_commit: Option(String),
    git_branch: Option(String),
  )
}

/// Create a new test report
pub fn new_report(
  run_id: String,
  started_at: String,
  completed_at: String,
  duration_ms: Int,
) -> TestReport {
  TestReport(
    run_id: run_id,
    started_at: started_at,
    completed_at: completed_at,
    duration_ms: duration_ms,
    summary: ReportSummary(
      total_tests: 0,
      passed: 0,
      failed: 0,
      skipped: 0,
      errored: 0,
      pass_rate: 0.0,
      avg_confidence: 0.0,
    ),
    by_category: [],
    baseline_comparison: None,
    environment: EnvironmentInfo(
      gleam_version: "1.13.0",
      platform: "darwin",
      git_commit: None,
      git_branch: None,
    ),
  )
}

/// Set summary metrics
pub fn with_summary(report: TestReport, summary: ReportSummary) -> TestReport {
  TestReport(..report, summary: summary)
}

/// Add category report
pub fn add_category(report: TestReport, category: CategoryReport) -> TestReport {
  TestReport(..report, by_category: [category, ..report.by_category])
}

/// Set baseline comparison
pub fn with_baseline(
  report: TestReport,
  comparison: BaselineComparison,
) -> TestReport {
  TestReport(..report, baseline_comparison: Some(comparison))
}

/// Set environment info
pub fn with_environment(report: TestReport, env: EnvironmentInfo) -> TestReport {
  TestReport(..report, environment: env)
}

/// Format report as JSON
pub fn to_json(report: TestReport) -> String {
  string.concat([
    "{\n",
    "  \"run_id\": \"",
    report.run_id,
    "\",\n",
    "  \"started_at\": \"",
    report.started_at,
    "\",\n",
    "  \"completed_at\": \"",
    report.completed_at,
    "\",\n",
    "  \"duration_ms\": ",
    int.to_string(report.duration_ms),
    ",\n",
    "  \"summary\": ",
    summary_to_json(report.summary),
    ",\n",
    "  \"by_category\": [\n",
    report.by_category
      |> list.map(category_to_json)
      |> string.join(",\n"),
    "\n  ],\n",
    "  \"baseline_comparison\": ",
    case report.baseline_comparison {
      Some(bc) -> baseline_to_json(bc)
      None -> "null"
    },
    ",\n",
    "  \"environment\": ",
    environment_to_json(report.environment),
    "\n",
    "}\n",
  ])
}

/// Format summary as JSON
fn summary_to_json(summary: ReportSummary) -> String {
  string.concat([
    "{\n",
    "    \"total_tests\": ",
    int.to_string(summary.total_tests),
    ",\n",
    "    \"passed\": ",
    int.to_string(summary.passed),
    ",\n",
    "    \"failed\": ",
    int.to_string(summary.failed),
    ",\n",
    "    \"skipped\": ",
    int.to_string(summary.skipped),
    ",\n",
    "    \"errored\": ",
    int.to_string(summary.errored),
    ",\n",
    "    \"pass_rate\": ",
    float_to_json(summary.pass_rate),
    ",\n",
    "    \"avg_confidence\": ",
    float_to_json(summary.avg_confidence),
    "\n",
    "  }",
  ])
}

/// Format category as JSON
fn category_to_json(category: CategoryReport) -> String {
  string.concat([
    "    {\n",
    "      \"category\": \"",
    category_to_string(category.category),
    "\",\n",
    "      \"total\": ",
    int.to_string(category.total),
    ",\n",
    "      \"passed\": ",
    int.to_string(category.passed),
    ",\n",
    "      \"failed\": ",
    int.to_string(category.failed),
    ",\n",
    "      \"pass_rate\": ",
    float_to_json(category.pass_rate),
    "\n",
    "    }",
  ])
}

/// Format baseline comparison as JSON
fn baseline_to_json(bc: BaselineComparison) -> String {
  string.concat([
    "{\n",
    "    \"baseline_id\": \"",
    bc.baseline_id,
    "\",\n",
    "    \"baseline_date\": \"",
    bc.baseline_date,
    "\",\n",
    "    \"regressions\": ",
    list_to_json_array(bc.regressions),
    ",\n",
    "    \"improvements\": ",
    list_to_json_array(bc.improvements),
    ",\n",
    "    \"new_tests\": ",
    list_to_json_array(bc.new_tests),
    ",\n",
    "    \"pass_rate_delta\": ",
    float_to_json(bc.pass_rate_delta),
    "\n",
    "  }",
  ])
}

/// Format environment as JSON
fn environment_to_json(env: EnvironmentInfo) -> String {
  string.concat([
    "{\n",
    "    \"gleam_version\": \"",
    env.gleam_version,
    "\",\n",
    "    \"platform\": \"",
    env.platform,
    "\",\n",
    "    \"git_commit\": ",
    option_to_json_string(env.git_commit),
    ",\n",
    "    \"git_branch\": ",
    option_to_json_string(env.git_branch),
    "\n",
    "  }",
  ])
}

/// Format report as Markdown
pub fn to_markdown(report: TestReport) -> String {
  let status_badge = case report.summary.pass_rate >=. 100.0 {
    True -> "![Status](https://img.shields.io/badge/status-passing-brightgreen)"
    False -> "![Status](https://img.shields.io/badge/status-failing-red)"
  }

  string.concat([
    "# Test Report\n\n",
    status_badge,
    "\n\n",
    "**Run ID:** `",
    report.run_id,
    "`\n",
    "**Date:** ",
    report.started_at,
    "\n",
    "**Duration:** ",
    int.to_string(report.duration_ms),
    "ms\n\n",
    "## Summary\n\n",
    "| Metric | Value |\n",
    "|--------|-------|\n",
    "| Total Tests | ",
    int.to_string(report.summary.total_tests),
    " |\n",
    "| Passed | ",
    int.to_string(report.summary.passed),
    " |\n",
    "| Failed | ",
    int.to_string(report.summary.failed),
    " |\n",
    "| Skipped | ",
    int.to_string(report.summary.skipped),
    " |\n",
    "| Errored | ",
    int.to_string(report.summary.errored),
    " |\n",
    "| Pass Rate | ",
    format_percent(report.summary.pass_rate),
    " |\n\n",
    categories_to_markdown(report.by_category),
    baseline_to_markdown(report.baseline_comparison),
    environment_to_markdown(report.environment),
  ])
}

/// Format categories as Markdown
fn categories_to_markdown(categories: List(CategoryReport)) -> String {
  case categories {
    [] -> ""
    _ ->
      string.concat([
        "## Results by Category\n\n",
        "| Category | Total | Passed | Failed | Rate |\n",
        "|----------|-------|--------|--------|------|\n",
        categories
          |> list.map(fn(c) {
            "| "
            <> category_to_string(c.category)
            <> " | "
            <> int.to_string(c.total)
            <> " | "
            <> int.to_string(c.passed)
            <> " | "
            <> int.to_string(c.failed)
            <> " | "
            <> format_percent(c.pass_rate)
            <> " |\n"
          })
          |> string.concat,
        "\n",
      ])
  }
}

/// Format baseline comparison as Markdown
fn baseline_to_markdown(baseline: Option(BaselineComparison)) -> String {
  case baseline {
    None -> ""
    Some(bc) ->
      string.concat([
        "## Baseline Comparison\n\n",
        "**Baseline:** `",
        bc.baseline_id,
        "` (",
        bc.baseline_date,
        ")\n",
        "**Pass Rate Change:** ",
        format_delta(bc.pass_rate_delta),
        "\n\n",
        case bc.regressions {
          [] -> ""
          _ ->
            "### Regressions\n"
            <> list.map(bc.regressions, fn(r) { "- " <> r <> "\n" })
            |> string.concat
        },
        case bc.improvements {
          [] -> ""
          _ ->
            "### Improvements\n"
            <> list.map(bc.improvements, fn(i) { "- " <> i <> "\n" })
            |> string.concat
        },
        "\n",
      ])
  }
}

/// Format environment as Markdown
fn environment_to_markdown(env: EnvironmentInfo) -> String {
  string.concat([
    "## Environment\n\n",
    "- **Gleam:** ",
    env.gleam_version,
    "\n",
    "- **Platform:** ",
    env.platform,
    "\n",
    case env.git_commit {
      Some(c) -> "- **Commit:** `" <> c <> "`\n"
      None -> ""
    },
    case env.git_branch {
      Some(b) -> "- **Branch:** " <> b <> "\n"
      None -> ""
    },
  ])
}

/// Format report as console output
pub fn to_console(report: TestReport) -> String {
  let separator = string.repeat("=", 70)
  let thin_sep = string.repeat("-", 70)

  string.concat([
    "\n",
    separator,
    "\n",
    "TEST REPORT: ",
    report.run_id,
    "\n",
    separator,
    "\n\n",
    "Started:  ",
    report.started_at,
    "\n",
    "Finished: ",
    report.completed_at,
    "\n",
    "Duration: ",
    int.to_string(report.duration_ms),
    "ms\n\n",
    thin_sep,
    "\n",
    "SUMMARY\n",
    thin_sep,
    "\n\n",
    "  Total:   ",
    int.to_string(report.summary.total_tests),
    "\n",
    "  Passed:  ",
    int.to_string(report.summary.passed),
    "\n",
    "  Failed:  ",
    int.to_string(report.summary.failed),
    "\n",
    "  Skipped: ",
    int.to_string(report.summary.skipped),
    "\n",
    "  Errored: ",
    int.to_string(report.summary.errored),
    "\n",
    "  Rate:    ",
    format_percent(report.summary.pass_rate),
    "\n\n",
    case report.baseline_comparison {
      Some(bc) ->
        thin_sep
        <> "\n"
        <> "BASELINE COMPARISON\n"
        <> thin_sep
        <> "\n\n"
        <> "  Baseline: "
        <> bc.baseline_id
        <> "\n"
        <> "  Delta:    "
        <> format_delta(bc.pass_rate_delta)
        <> "\n"
        <> case bc.regressions {
          [] -> ""
          regs ->
            "\n  Regressions:\n"
            <> list.map(regs, fn(r) { "    - " <> r <> "\n" })
            |> string.concat
        }
        <> "\n"
      None -> ""
    },
    separator,
    "\n",
  ])
}

/// Helper: category to string
fn category_to_string(cat: FixtureCategory) -> String {
  case cat {
    ClassicArgument -> "Classic Arguments"
    ModalTheorem -> "Modal Theorems"
    EdgeCase -> "Edge Cases"
    RegressionCase -> "Regressions"
    ExternalDataset(source) -> "External: " <> source
  }
}

/// Helper: format percentage
fn format_percent(rate: Float) -> String {
  int.to_string(float_to_int(rate *. 100.0)) <> "%"
}

/// Helper: format delta (with sign)
fn format_delta(delta: Float) -> String {
  let pct = float_to_int(delta *. 100.0)
  case pct >= 0 {
    True -> "+" <> int.to_string(pct) <> "%"
    False -> int.to_string(pct) <> "%"
  }
}

/// Helper: float to int
fn float_to_int(f: Float) -> Int {
  case f >=. 0.0 {
    True -> {
      let shifted = f +. 0.5
      truncate_positive(shifted)
    }
    False -> {
      let shifted = f -. 0.5
      truncate_negative(shifted)
    }
  }
}

fn truncate_positive(f: Float) -> Int {
  // Simple truncation for positive floats
  case f <. 1.0 {
    True -> 0
    False -> 1 + truncate_positive(f -. 1.0)
  }
}

fn truncate_negative(f: Float) -> Int {
  case f >. -1.0 {
    True -> 0
    False -> -1 + truncate_negative(f +. 1.0)
  }
}

/// Helper: float to JSON string
fn float_to_json(f: Float) -> String {
  let int_part = float_to_int(f)
  int.to_string(int_part) <> ".0"
}

/// Helper: list to JSON array
fn list_to_json_array(items: List(String)) -> String {
  "["
  <> list.map(items, fn(i) { "\"" <> i <> "\"" })
  |> string.join(", ")
  <> "]"
}

/// Helper: option to JSON string
fn option_to_json_string(opt: Option(String)) -> String {
  case opt {
    Some(s) -> "\"" <> s <> "\""
    None -> "null"
  }
}
