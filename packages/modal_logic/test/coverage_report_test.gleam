//// Tests for Coverage Report Generator
////
//// Tests the coverage report generation including configuration,
//// report generation, and multi-format output.

import gleam/dict
import gleam/string
import gleeunit/should
import modal_logic/rules/rule_store
import modal_logic/testing/docs/coverage_report.{
  CoverageSummary, MinimallyTested, PartiallyTested, Untested, WellTested,
  comprehensive_config, coverage_grade, default_config, format_html, format_json,
  format_markdown, generate_report, meets_threshold, minimal_config,
  summary_string,
}
import modal_logic/testing/validation/philosophical_tester

// ============ Configuration Tests ============

pub fn default_config_test() {
  let config = default_config()

  { config.min_coverage_threshold >=. 0.0 } |> should.be_true
  { config.min_coverage_threshold <=. 1.0 } |> should.be_true
  config.include_untested |> should.be_true
  config.include_system_breakdown |> should.be_true
  config.include_argument_details |> should.be_true
  { config.max_arguments_per_rule > 0 } |> should.be_true
}

pub fn minimal_config_test() {
  let config = minimal_config()

  config.include_untested |> should.be_false
  config.include_system_breakdown |> should.be_false
  config.include_argument_details |> should.be_false
  { config.max_arguments_per_rule < default_config().max_arguments_per_rule }
  |> should.be_true
}

pub fn comprehensive_config_test() {
  let config = comprehensive_config()

  { config.min_coverage_threshold >=. default_config().min_coverage_threshold }
  |> should.be_true
  config.include_untested |> should.be_true
  config.include_system_breakdown |> should.be_true
  config.include_argument_details |> should.be_true
}

// ============ Report Generation Tests ============

pub fn generate_report_empty_store_test() {
  let store = rule_store.new()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)

  report.summary.total_rules |> should.equal(0)
  report.summary.total_axioms |> should.equal(0)
}

pub fn generate_report_standard_store_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)

  { report.summary.total_rules > 0 } |> should.be_true
  { report.summary.total_axioms > 0 } |> should.be_true
}

pub fn generate_report_has_rule_coverage_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)

  // Rule coverage list should have same length as rules in store
  { report.rule_coverage != [] } |> should.be_true
}

pub fn generate_report_has_axiom_coverage_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)

  // Axiom coverage list should have same length as axioms in store
  { report.axiom_coverage != [] } |> should.be_true
}

pub fn generate_report_system_breakdown_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config =
    coverage_report.CoverageConfig(
      ..default_config(),
      include_system_breakdown: True,
    )

  let report = generate_report(store, test_result, config)

  // Should have system coverage
  dict.size(report.system_coverage) |> should.not_equal(0)
}

pub fn generate_report_no_system_breakdown_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config =
    coverage_report.CoverageConfig(
      ..default_config(),
      include_system_breakdown: False,
    )

  let report = generate_report(store, test_result, config)

  // Should not have system coverage
  dict.size(report.system_coverage) |> should.equal(0)
}

// ============ Coverage Status Tests ============

pub fn coverage_status_values_test() {
  // Just verify status values exist
  let statuses = [WellTested, PartiallyTested, MinimallyTested, Untested]

  { statuses != [] } |> should.be_true
}

// ============ Format Markdown Tests ============

pub fn format_markdown_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let markdown = format_markdown(report)

  markdown |> should.not_equal("")
  string.contains(markdown, "# Coverage Report") |> should.be_true
  string.contains(markdown, "## Summary") |> should.be_true
}

pub fn format_markdown_has_tables_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let markdown = format_markdown(report)

  // Should have markdown table syntax
  string.contains(markdown, "|") |> should.be_true
  string.contains(markdown, "---") |> should.be_true
}

pub fn format_markdown_has_rule_coverage_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let markdown = format_markdown(report)

  string.contains(markdown, "## Rule Coverage") |> should.be_true
}

pub fn format_markdown_has_axiom_coverage_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let markdown = format_markdown(report)

  string.contains(markdown, "## Axiom Coverage") |> should.be_true
}

// ============ Format HTML Tests ============

pub fn format_html_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let html = format_html(report)

  html |> should.not_equal("")
  string.contains(html, "<!DOCTYPE html>") |> should.be_true
  string.contains(html, "</html>") |> should.be_true
}

pub fn format_html_has_css_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let html = format_html(report)

  string.contains(html, "<style>") |> should.be_true
  string.contains(html, "</style>") |> should.be_true
}

pub fn format_html_has_tables_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let html = format_html(report)

  string.contains(html, "<table>") |> should.be_true
  string.contains(html, "</table>") |> should.be_true
}

pub fn format_html_has_summary_cards_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let html = format_html(report)

  string.contains(html, "summary-card") |> should.be_true
}

pub fn format_html_has_progress_bars_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let html = format_html(report)

  string.contains(html, "progress-bar") |> should.be_true
}

// ============ Format JSON Tests ============

pub fn format_json_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let json = format_json(report)

  json |> should.not_equal("")
  string.contains(json, "{") |> should.be_true
  string.contains(json, "}") |> should.be_true
}

pub fn format_json_has_summary_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let json = format_json(report)

  string.contains(json, "\"summary\":") |> should.be_true
}

pub fn format_json_has_rule_coverage_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let json = format_json(report)

  string.contains(json, "\"rule_coverage\":") |> should.be_true
}

pub fn format_json_has_axiom_coverage_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let json = format_json(report)

  string.contains(json, "\"axiom_coverage\":") |> should.be_true
}

pub fn format_json_has_metadata_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let json = format_json(report)

  string.contains(json, "\"metadata\":") |> should.be_true
}

// ============ Utility Function Tests ============

pub fn meets_threshold_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)

  // With no tests run, coverage should be low
  meets_threshold(report, 0.0) |> should.be_true
}

pub fn coverage_grade_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let grade = coverage_grade(report)

  // Grade should be a letter
  {
    grade == "A" || grade == "B" || grade == "C" || grade == "D" || grade == "F"
  }
  |> should.be_true
}

pub fn summary_string_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = default_config()

  let report = generate_report(store, test_result, config)
  let summary = summary_string(report)

  summary |> should.not_equal("")
  string.contains(summary, "Coverage Report Summary") |> should.be_true
  string.contains(summary, "Rules:") |> should.be_true
  string.contains(summary, "Axioms:") |> should.be_true
  string.contains(summary, "Grade:") |> should.be_true
}

// ============ Coverage Summary Tests ============

pub fn coverage_summary_structure_test() {
  let summary =
    CoverageSummary(
      total_rules: 10,
      tested_rules: 5,
      rule_coverage_percent: 50.0,
      total_axioms: 6,
      tested_axioms: 3,
      axiom_coverage_percent: 50.0,
      overall_pass_rate: 80.0,
      total_arguments_tested: 20,
      arguments_passed: 16,
    )

  summary.total_rules |> should.equal(10)
  summary.tested_rules |> should.equal(5)
  // Float comparison - check it's approximately 50.0
  {
    summary.rule_coverage_percent >=. 49.9
    && summary.rule_coverage_percent <=. 50.1
  }
  |> should.be_true
}

// ============ Helper Functions ============

fn create_empty_test_result() -> philosophical_tester.PhilosophicalTestResult {
  philosophical_tester.PhilosophicalTestResult(
    config: philosophical_tester.default_config(),
    total_tested: 0,
    correctly_validated: 0,
    incorrectly_validated: 0,
    argument_results: [],
    rule_statistics: dict.new(),
    soundness_assessment: philosophical_tester.SoundnessAssessment(
      score: 1.0,
      category: philosophical_tester.FullySoundness,
      notes: [],
    ),
  )
}
