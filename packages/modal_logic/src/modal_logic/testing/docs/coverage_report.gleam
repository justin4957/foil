//// Coverage Report Generator
////
//// This module generates comprehensive coverage reports showing which rules
//// and axioms have been tested, with what arguments, and their pass rates.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{type LogicSystem}
import modal_logic/rules/axiom.{type Axiom}
import modal_logic/rules/inference_rule.{type InferenceRule}
import modal_logic/rules/rule_store.{type RuleStore}
import modal_logic/testing/validation/philosophical_tester.{
  type PhilosophicalTestResult,
}

// ============ Core Types ============

/// Coverage report configuration
pub type CoverageConfig {
  CoverageConfig(
    /// Minimum coverage threshold (0.0 - 1.0)
    min_coverage_threshold: Float,
    /// Include untested rules
    include_untested: Bool,
    /// Include per-system breakdown
    include_system_breakdown: Bool,
    /// Include argument-level details
    include_argument_details: Bool,
    /// Maximum arguments to show per rule
    max_arguments_per_rule: Int,
  )
}

/// Complete coverage report
pub type CoverageReport {
  CoverageReport(
    /// Overall coverage statistics
    summary: CoverageSummary,
    /// Rule coverage details
    rule_coverage: List(RuleCoverage),
    /// Axiom coverage details
    axiom_coverage: List(AxiomCoverage),
    /// Coverage by logic system
    system_coverage: Dict(String, SystemCoverage),
    /// Untested rules
    untested_rules: List(String),
    /// Untested axioms
    untested_axioms: List(String),
    /// Report metadata
    metadata: ReportMetadata,
  )
}

/// Coverage summary statistics
pub type CoverageSummary {
  CoverageSummary(
    /// Total rules in store
    total_rules: Int,
    /// Rules that have been tested
    tested_rules: Int,
    /// Rule coverage percentage
    rule_coverage_percent: Float,
    /// Total axioms in store
    total_axioms: Int,
    /// Axioms that have been referenced
    tested_axioms: Int,
    /// Axiom coverage percentage
    axiom_coverage_percent: Float,
    /// Overall pass rate
    overall_pass_rate: Float,
    /// Total arguments tested
    total_arguments_tested: Int,
    /// Arguments that passed
    arguments_passed: Int,
  )
}

/// Coverage details for a single rule
pub type RuleCoverage {
  RuleCoverage(
    /// Rule ID
    rule_id: String,
    /// Rule name
    rule_name: String,
    /// Times the rule was applied
    times_applied: Int,
    /// Times application succeeded
    times_succeeded: Int,
    /// Pass rate (0.0 - 1.0)
    pass_rate: Float,
    /// Logic systems where tested
    tested_in_systems: List(LogicSystem),
    /// Arguments that used this rule
    arguments: List(String),
    /// Coverage status
    status: CoverageStatus,
  )
}

/// Coverage details for a single axiom
pub type AxiomCoverage {
  AxiomCoverage(
    /// Axiom ID
    axiom_id: String,
    /// Axiom name
    axiom_name: String,
    /// Times referenced
    times_referenced: Int,
    /// Valid in systems
    valid_in_systems: List(LogicSystem),
    /// Coverage status
    status: CoverageStatus,
  )
}

/// Coverage for a logic system
pub type SystemCoverage {
  SystemCoverage(
    /// Logic system name
    system_name: String,
    /// Rules tested in this system
    rules_tested: Int,
    /// Total rules valid in this system
    total_rules: Int,
    /// Coverage percentage
    coverage_percent: Float,
    /// Pass rate in this system
    pass_rate: Float,
  )
}

/// Coverage status classification
pub type CoverageStatus {
  /// Well tested with high coverage
  WellTested
  /// Partially tested
  PartiallyTested
  /// Minimal testing
  MinimallyTested
  /// No testing
  Untested
}

/// Report metadata
pub type ReportMetadata {
  ReportMetadata(
    /// When report was generated
    generated_at: String,
    /// Report version
    version: String,
    /// Configuration used
    config: CoverageConfig,
  )
}

// ============ Configuration ============

/// Default coverage configuration
pub fn default_config() -> CoverageConfig {
  CoverageConfig(
    min_coverage_threshold: 0.8,
    include_untested: True,
    include_system_breakdown: True,
    include_argument_details: True,
    max_arguments_per_rule: 10,
  )
}

/// Minimal configuration for quick reports
pub fn minimal_config() -> CoverageConfig {
  CoverageConfig(
    min_coverage_threshold: 0.5,
    include_untested: False,
    include_system_breakdown: False,
    include_argument_details: False,
    max_arguments_per_rule: 3,
  )
}

/// Comprehensive configuration for detailed analysis
pub fn comprehensive_config() -> CoverageConfig {
  CoverageConfig(
    min_coverage_threshold: 0.9,
    include_untested: True,
    include_system_breakdown: True,
    include_argument_details: True,
    max_arguments_per_rule: 20,
  )
}

// ============ Report Generation ============

/// Generate coverage report from store and test results
pub fn generate_report(
  store: RuleStore,
  test_result: PhilosophicalTestResult,
  config: CoverageConfig,
) -> CoverageReport {
  let rules = rule_store.list_rules(store)
  let axioms = rule_store.list_axioms(store)

  // Calculate rule coverage
  let rule_coverage =
    rules
    |> list.map(fn(rule) { calculate_rule_coverage(rule, test_result, config) })

  // Calculate axiom coverage
  let axiom_coverage =
    axioms
    |> list.map(fn(axiom) { calculate_axiom_coverage(axiom, test_result) })

  // Find untested items
  let untested_rules =
    rule_coverage
    |> list.filter(fn(rc) { rc.status == Untested })
    |> list.map(fn(rc) { rc.rule_id })

  let untested_axioms =
    axiom_coverage
    |> list.filter(fn(ac) { ac.status == Untested })
    |> list.map(fn(ac) { ac.axiom_id })

  // Calculate system coverage
  let system_coverage = case config.include_system_breakdown {
    True -> calculate_system_coverage(rules, rule_coverage)
    False -> dict.new()
  }

  // Calculate summary
  let summary =
    calculate_summary(rules, axioms, rule_coverage, axiom_coverage, test_result)

  CoverageReport(
    summary: summary,
    rule_coverage: rule_coverage,
    axiom_coverage: axiom_coverage,
    system_coverage: system_coverage,
    untested_rules: untested_rules,
    untested_axioms: untested_axioms,
    metadata: ReportMetadata(
      generated_at: "now",
      version: "1.0.0",
      config: config,
    ),
  )
}

/// Calculate coverage for a single rule
fn calculate_rule_coverage(
  rule: InferenceRule,
  test_result: PhilosophicalTestResult,
  _config: CoverageConfig,
) -> RuleCoverage {
  // Find statistics for this rule from dict
  let stats = dict.get(test_result.rule_statistics, rule.id)

  let #(times_applied, times_succeeded, pass_rate, args_contributed) = case
    stats
  {
    Ok(s) -> {
      let total_apps = s.successful_applications + s.failed_applications
      let rate = case total_apps {
        0 -> 0.0
        n -> int.to_float(s.successful_applications) /. int.to_float(n)
      }
      #(total_apps, s.successful_applications, rate, s.contributed_to)
    }
    Error(_) -> #(0, 0, 0.0, 0)
  }

  let status = classify_coverage_status(times_applied, pass_rate)

  // Build argument list from test results that used this rule
  let arguments =
    test_result.argument_results
    |> list.filter(fn(ar) { ar.passed })
    |> list.take(args_contributed)
    |> list.map(fn(ar) { ar.argument.id })

  RuleCoverage(
    rule_id: rule.id,
    rule_name: rule.name,
    times_applied: times_applied,
    times_succeeded: times_succeeded,
    pass_rate: pass_rate,
    tested_in_systems: rule.valid_in,
    arguments: arguments,
    status: status,
  )
}

/// Calculate coverage for a single axiom
fn calculate_axiom_coverage(
  axiom: Axiom,
  test_result: PhilosophicalTestResult,
) -> AxiomCoverage {
  // Count arguments that might use this axiom (by checking logic system match)
  let times_referenced =
    test_result.argument_results
    |> list.count(fn(ar) {
      // Check if any of axiom's valid systems overlap with argument's valid systems
      list.any(axiom.included_in, fn(sys) {
        list.contains(ar.argument.valid_in, sys)
      })
    })

  let status = case times_referenced {
    0 -> Untested
    n if n < 3 -> MinimallyTested
    n if n < 10 -> PartiallyTested
    _ -> WellTested
  }

  AxiomCoverage(
    axiom_id: axiom.id,
    axiom_name: axiom.name,
    times_referenced: times_referenced,
    valid_in_systems: axiom.included_in,
    status: status,
  )
}

/// Calculate system-level coverage
fn calculate_system_coverage(
  rules: List(InferenceRule),
  rule_coverage: List(RuleCoverage),
) -> Dict(String, SystemCoverage) {
  let systems = [
    proposition.K,
    proposition.T,
    proposition.S4,
    proposition.S5,
    proposition.KD,
  ]

  systems
  |> list.map(fn(sys) {
    let sys_name = logic_system_to_string(sys)

    // Rules valid in this system
    let rules_in_system =
      rules
      |> list.filter(fn(r) { list.contains(r.valid_in, sys) })

    let total = list.length(rules_in_system)

    // Rules tested in this system
    let tested =
      rule_coverage
      |> list.filter(fn(rc) {
        rc.status != Untested && list.contains(rc.tested_in_systems, sys)
      })
      |> list.length

    let coverage_pct = case total {
      0 -> 0.0
      n -> int.to_float(tested) /. int.to_float(n) *. 100.0
    }

    // Average pass rate for rules in this system
    let pass_rates =
      rule_coverage
      |> list.filter(fn(rc) {
        rc.times_applied > 0 && list.contains(rc.tested_in_systems, sys)
      })
      |> list.map(fn(rc) { rc.pass_rate })

    let avg_pass_rate = case pass_rates {
      [] -> 0.0
      rates -> {
        let sum = list.fold(rates, 0.0, fn(acc, r) { acc +. r })
        sum /. int.to_float(list.length(rates))
      }
    }

    #(
      sys_name,
      SystemCoverage(
        system_name: sys_name,
        rules_tested: tested,
        total_rules: total,
        coverage_percent: coverage_pct,
        pass_rate: avg_pass_rate,
      ),
    )
  })
  |> dict.from_list
}

/// Calculate overall summary
fn calculate_summary(
  rules: List(InferenceRule),
  axioms: List(Axiom),
  rule_coverage: List(RuleCoverage),
  axiom_coverage: List(AxiomCoverage),
  test_result: PhilosophicalTestResult,
) -> CoverageSummary {
  let total_rules = list.length(rules)
  let tested_rules =
    rule_coverage
    |> list.count(fn(rc) { rc.status != Untested })

  let rule_coverage_pct = case total_rules {
    0 -> 0.0
    n -> int.to_float(tested_rules) /. int.to_float(n) *. 100.0
  }

  let total_axioms = list.length(axioms)
  let tested_axioms =
    axiom_coverage
    |> list.count(fn(ac) { ac.status != Untested })

  let axiom_coverage_pct = case total_axioms {
    0 -> 0.0
    n -> int.to_float(tested_axioms) /. int.to_float(n) *. 100.0
  }

  let overall_pass_rate = test_result.soundness_assessment.score *. 100.0

  CoverageSummary(
    total_rules: total_rules,
    tested_rules: tested_rules,
    rule_coverage_percent: rule_coverage_pct,
    total_axioms: total_axioms,
    tested_axioms: tested_axioms,
    axiom_coverage_percent: axiom_coverage_pct,
    overall_pass_rate: overall_pass_rate,
    total_arguments_tested: test_result.total_tested,
    arguments_passed: test_result.correctly_validated,
  )
}

/// Classify coverage status based on metrics
fn classify_coverage_status(
  times_applied: Int,
  pass_rate: Float,
) -> CoverageStatus {
  case times_applied, pass_rate {
    0, _ -> Untested
    n, r if n >= 10 && r >=. 0.8 -> WellTested
    n, r if n >= 3 && r >=. 0.5 -> PartiallyTested
    _, _ -> MinimallyTested
  }
}

// ============ Formatting ============

/// Format coverage report as Markdown
pub fn format_markdown(report: CoverageReport) -> String {
  let sections = [
    format_summary_markdown(report.summary),
    format_rule_coverage_markdown(report.rule_coverage),
    format_axiom_coverage_markdown(report.axiom_coverage),
    format_system_coverage_markdown(report.system_coverage),
    format_untested_markdown(report.untested_rules, report.untested_axioms),
  ]

  string.join(["# Coverage Report", "", ..sections], "\n\n")
}

/// Format summary section
fn format_summary_markdown(summary: CoverageSummary) -> String {
  string.join(
    [
      "## Summary",
      "",
      "| Metric | Value |",
      "|--------|-------|",
      "| Total Rules | " <> int.to_string(summary.total_rules) <> " |",
      "| Tested Rules | " <> int.to_string(summary.tested_rules) <> " |",
      "| Rule Coverage | "
        <> float_to_percent(summary.rule_coverage_percent)
        <> " |",
      "| Total Axioms | " <> int.to_string(summary.total_axioms) <> " |",
      "| Tested Axioms | " <> int.to_string(summary.tested_axioms) <> " |",
      "| Axiom Coverage | "
        <> float_to_percent(summary.axiom_coverage_percent)
        <> " |",
      "| Overall Pass Rate | "
        <> float_to_percent(summary.overall_pass_rate)
        <> " |",
      "| Arguments Tested | "
        <> int.to_string(summary.total_arguments_tested)
        <> " |",
      "| Arguments Passed | " <> int.to_string(summary.arguments_passed) <> " |",
    ],
    "\n",
  )
}

/// Format rule coverage section
fn format_rule_coverage_markdown(coverage: List(RuleCoverage)) -> String {
  let rows =
    coverage
    |> list.map(fn(rc) {
      "| "
      <> rc.rule_name
      <> " | "
      <> int.to_string(rc.times_applied)
      <> " | "
      <> float_to_percent(rc.pass_rate *. 100.0)
      <> " | "
      <> status_to_emoji(rc.status)
      <> " |"
    })
    |> string.join("\n")

  string.join(
    [
      "## Rule Coverage",
      "",
      "| Rule | Times Applied | Pass Rate | Status |",
      "|------|---------------|-----------|--------|",
      rows,
    ],
    "\n",
  )
}

/// Format axiom coverage section
fn format_axiom_coverage_markdown(coverage: List(AxiomCoverage)) -> String {
  let rows =
    coverage
    |> list.map(fn(ac) {
      "| "
      <> ac.axiom_name
      <> " | "
      <> int.to_string(ac.times_referenced)
      <> " | "
      <> status_to_emoji(ac.status)
      <> " |"
    })
    |> string.join("\n")

  string.join(
    [
      "## Axiom Coverage",
      "",
      "| Axiom | Times Referenced | Status |",
      "|-------|------------------|--------|",
      rows,
    ],
    "\n",
  )
}

/// Format system coverage section
fn format_system_coverage_markdown(
  coverage: Dict(String, SystemCoverage),
) -> String {
  let rows =
    coverage
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(_name, sc) = pair
      "| "
      <> sc.system_name
      <> " | "
      <> int.to_string(sc.rules_tested)
      <> "/"
      <> int.to_string(sc.total_rules)
      <> " | "
      <> float_to_percent(sc.coverage_percent)
      <> " | "
      <> float_to_percent(sc.pass_rate *. 100.0)
      <> " |"
    })
    |> string.join("\n")

  string.join(
    [
      "## Coverage by Logic System",
      "",
      "| System | Rules Tested | Coverage | Pass Rate |",
      "|--------|--------------|----------|-----------|",
      rows,
    ],
    "\n",
  )
}

/// Format untested items section
fn format_untested_markdown(
  untested_rules: List(String),
  untested_axioms: List(String),
) -> String {
  let rules_section = case untested_rules {
    [] -> "No untested rules."
    rules ->
      "### Untested Rules\n\n"
      <> {
        rules
        |> list.map(fn(r) { "- " <> r })
        |> string.join("\n")
      }
  }

  let axioms_section = case untested_axioms {
    [] -> "No untested axioms."
    axioms ->
      "### Untested Axioms\n\n"
      <> {
        axioms
        |> list.map(fn(a) { "- " <> a })
        |> string.join("\n")
      }
  }

  string.join(
    ["## Untested Items", "", rules_section, "", axioms_section],
    "\n",
  )
}

/// Format coverage report as HTML with styling
pub fn format_html(report: CoverageReport) -> String {
  let css = get_coverage_css()

  string.join(
    [
      "<!DOCTYPE html>",
      "<html>",
      "<head>",
      "<meta charset=\"UTF-8\">",
      "<title>Coverage Report</title>",
      "<style>",
      css,
      "</style>",
      "</head>",
      "<body>",
      "<div class=\"container\">",
      "<h1>Coverage Report</h1>",
      format_summary_html(report.summary),
      format_rule_coverage_html(report.rule_coverage),
      format_axiom_coverage_html(report.axiom_coverage),
      format_system_coverage_html(report.system_coverage),
      format_untested_html(report.untested_rules, report.untested_axioms),
      "</div>",
      "</body>",
      "</html>",
    ],
    "\n",
  )
}

/// Get CSS for coverage report
fn get_coverage_css() -> String {
  "
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
  line-height: 1.6;
  color: #333;
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
  background: #f5f5f5;
}
.container {
  background: white;
  padding: 30px;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
h2 { color: #34495e; margin-top: 30px; }
table { width: 100%; border-collapse: collapse; margin: 20px 0; }
th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
th { background: #3498db; color: white; }
tr:hover { background: #f5f5f5; }
.status-well-tested { color: #27ae60; font-weight: bold; }
.status-partially-tested { color: #f39c12; }
.status-minimally-tested { color: #e74c3c; }
.status-untested { color: #95a5a6; }
.summary-card {
  display: inline-block;
  background: #3498db;
  color: white;
  padding: 20px;
  border-radius: 8px;
  margin: 10px;
  text-align: center;
}
.summary-card h3 { margin: 0; font-size: 2em; }
.summary-card p { margin: 5px 0 0 0; opacity: 0.9; }
.progress-bar {
  background: #ecf0f1;
  border-radius: 4px;
  height: 20px;
  overflow: hidden;
}
.progress-fill {
  background: #27ae60;
  height: 100%;
  transition: width 0.3s ease;
}
.warning { background: #fff3cd; border: 1px solid #ffc107; padding: 15px; border-radius: 4px; }
"
}

/// Format summary as HTML
fn format_summary_html(summary: CoverageSummary) -> String {
  string.join(
    [
      "<h2>Summary</h2>",
      "<div class=\"summary-cards\">",
      "<div class=\"summary-card\">",
      "<h3>" <> float_to_percent(summary.rule_coverage_percent) <> "</h3>",
      "<p>Rule Coverage</p>",
      "</div>",
      "<div class=\"summary-card\">",
      "<h3>" <> float_to_percent(summary.axiom_coverage_percent) <> "</h3>",
      "<p>Axiom Coverage</p>",
      "</div>",
      "<div class=\"summary-card\">",
      "<h3>" <> float_to_percent(summary.overall_pass_rate) <> "</h3>",
      "<p>Pass Rate</p>",
      "</div>",
      "<div class=\"summary-card\">",
      "<h3>" <> int.to_string(summary.total_arguments_tested) <> "</h3>",
      "<p>Tests Run</p>",
      "</div>",
      "</div>",
    ],
    "\n",
  )
}

/// Format rule coverage as HTML
fn format_rule_coverage_html(coverage: List(RuleCoverage)) -> String {
  let rows =
    coverage
    |> list.map(fn(rc) {
      let status_class = status_to_class(rc.status)
      "<tr>"
      <> "<td>"
      <> rc.rule_name
      <> "</td>"
      <> "<td>"
      <> int.to_string(rc.times_applied)
      <> "</td>"
      <> "<td>"
      <> format_progress_bar(rc.pass_rate *. 100.0)
      <> "</td>"
      <> "<td class=\""
      <> status_class
      <> "\">"
      <> status_to_text(rc.status)
      <> "</td>"
      <> "</tr>"
    })
    |> string.join("\n")

  string.join(
    [
      "<h2>Rule Coverage</h2>",
      "<table>",
      "<tr><th>Rule</th><th>Times Applied</th><th>Pass Rate</th><th>Status</th></tr>",
      rows,
      "</table>",
    ],
    "\n",
  )
}

/// Format progress bar HTML
fn format_progress_bar(percent: Float) -> String {
  let width = float.round(percent) |> int.to_string
  "<div class=\"progress-bar\"><div class=\"progress-fill\" style=\"width: "
  <> width
  <> "%\"></div></div>"
  <> "<span>"
  <> float_to_percent(percent)
  <> "</span>"
}

/// Format axiom coverage as HTML
fn format_axiom_coverage_html(coverage: List(AxiomCoverage)) -> String {
  let rows =
    coverage
    |> list.map(fn(ac) {
      let status_class = status_to_class(ac.status)
      "<tr>"
      <> "<td>"
      <> ac.axiom_name
      <> "</td>"
      <> "<td>"
      <> int.to_string(ac.times_referenced)
      <> "</td>"
      <> "<td class=\""
      <> status_class
      <> "\">"
      <> status_to_text(ac.status)
      <> "</td>"
      <> "</tr>"
    })
    |> string.join("\n")

  string.join(
    [
      "<h2>Axiom Coverage</h2>",
      "<table>",
      "<tr><th>Axiom</th><th>Times Referenced</th><th>Status</th></tr>",
      rows,
      "</table>",
    ],
    "\n",
  )
}

/// Format system coverage as HTML
fn format_system_coverage_html(coverage: Dict(String, SystemCoverage)) -> String {
  let rows =
    coverage
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(_name, sc) = pair
      "<tr>"
      <> "<td>"
      <> sc.system_name
      <> "</td>"
      <> "<td>"
      <> int.to_string(sc.rules_tested)
      <> "/"
      <> int.to_string(sc.total_rules)
      <> "</td>"
      <> "<td>"
      <> format_progress_bar(sc.coverage_percent)
      <> "</td>"
      <> "<td>"
      <> float_to_percent(sc.pass_rate *. 100.0)
      <> "</td>"
      <> "</tr>"
    })
    |> string.join("\n")

  string.join(
    [
      "<h2>Coverage by Logic System</h2>",
      "<table>",
      "<tr><th>System</th><th>Rules Tested</th><th>Coverage</th><th>Pass Rate</th></tr>",
      rows,
      "</table>",
    ],
    "\n",
  )
}

/// Format untested items as HTML
fn format_untested_html(
  untested_rules: List(String),
  untested_axioms: List(String),
) -> String {
  let rules_html = case untested_rules {
    [] -> "<p>No untested rules.</p>"
    rules ->
      "<ul>"
      <> {
        rules
        |> list.map(fn(r) { "<li>" <> r <> "</li>" })
        |> string.join("")
      }
      <> "</ul>"
  }

  let axioms_html = case untested_axioms {
    [] -> "<p>No untested axioms.</p>"
    axioms ->
      "<ul>"
      <> {
        axioms
        |> list.map(fn(a) { "<li>" <> a <> "</li>" })
        |> string.join("")
      }
      <> "</ul>"
  }

  string.join(
    [
      "<h2>Untested Items</h2>",
      "<h3>Untested Rules</h3>",
      rules_html,
      "<h3>Untested Axioms</h3>",
      axioms_html,
    ],
    "\n",
  )
}

/// Format coverage report as JSON
pub fn format_json(report: CoverageReport) -> String {
  let summary_json = format_summary_json(report.summary)
  let rules_json = format_rule_coverage_json(report.rule_coverage)
  let axioms_json = format_axiom_coverage_json(report.axiom_coverage)
  let systems_json = format_system_coverage_json(report.system_coverage)

  string.join(
    [
      "{",
      "  \"summary\": " <> summary_json <> ",",
      "  \"rule_coverage\": " <> rules_json <> ",",
      "  \"axiom_coverage\": " <> axioms_json <> ",",
      "  \"system_coverage\": " <> systems_json <> ",",
      "  \"untested_rules\": "
        <> format_string_list_json(report.untested_rules)
        <> ",",
      "  \"untested_axioms\": "
        <> format_string_list_json(report.untested_axioms)
        <> ",",
      "  \"metadata\": {",
      "    \"generated_at\": \"" <> report.metadata.generated_at <> "\",",
      "    \"version\": \"" <> report.metadata.version <> "\"",
      "  }",
      "}",
    ],
    "\n",
  )
}

/// Format summary as JSON
fn format_summary_json(summary: CoverageSummary) -> String {
  string.join(
    [
      "{",
      "    \"total_rules\": " <> int.to_string(summary.total_rules) <> ",",
      "    \"tested_rules\": " <> int.to_string(summary.tested_rules) <> ",",
      "    \"rule_coverage_percent\": "
        <> float.to_string(summary.rule_coverage_percent)
        <> ",",
      "    \"total_axioms\": " <> int.to_string(summary.total_axioms) <> ",",
      "    \"tested_axioms\": " <> int.to_string(summary.tested_axioms) <> ",",
      "    \"axiom_coverage_percent\": "
        <> float.to_string(summary.axiom_coverage_percent)
        <> ",",
      "    \"overall_pass_rate\": "
        <> float.to_string(summary.overall_pass_rate)
        <> ",",
      "    \"total_arguments_tested\": "
        <> int.to_string(summary.total_arguments_tested)
        <> ",",
      "    \"arguments_passed\": " <> int.to_string(summary.arguments_passed),
      "  }",
    ],
    "\n",
  )
}

/// Format rule coverage as JSON
fn format_rule_coverage_json(coverage: List(RuleCoverage)) -> String {
  let items =
    coverage
    |> list.map(fn(rc) {
      string.join(
        [
          "    {",
          "      \"rule_id\": \"" <> rc.rule_id <> "\",",
          "      \"rule_name\": \"" <> rc.rule_name <> "\",",
          "      \"times_applied\": " <> int.to_string(rc.times_applied) <> ",",
          "      \"times_succeeded\": "
            <> int.to_string(rc.times_succeeded)
            <> ",",
          "      \"pass_rate\": " <> float.to_string(rc.pass_rate) <> ",",
          "      \"status\": \"" <> status_to_text(rc.status) <> "\"",
          "    }",
        ],
        "\n",
      )
    })
    |> string.join(",\n")

  "[\n" <> items <> "\n  ]"
}

/// Format axiom coverage as JSON
fn format_axiom_coverage_json(coverage: List(AxiomCoverage)) -> String {
  let items =
    coverage
    |> list.map(fn(ac) {
      string.join(
        [
          "    {",
          "      \"axiom_id\": \"" <> ac.axiom_id <> "\",",
          "      \"axiom_name\": \"" <> ac.axiom_name <> "\",",
          "      \"times_referenced\": "
            <> int.to_string(ac.times_referenced)
            <> ",",
          "      \"status\": \"" <> status_to_text(ac.status) <> "\"",
          "    }",
        ],
        "\n",
      )
    })
    |> string.join(",\n")

  "[\n" <> items <> "\n  ]"
}

/// Format system coverage as JSON
fn format_system_coverage_json(coverage: Dict(String, SystemCoverage)) -> String {
  let items =
    coverage
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(name, sc) = pair
      string.join(
        [
          "    \"" <> name <> "\": {",
          "      \"rules_tested\": " <> int.to_string(sc.rules_tested) <> ",",
          "      \"total_rules\": " <> int.to_string(sc.total_rules) <> ",",
          "      \"coverage_percent\": "
            <> float.to_string(sc.coverage_percent)
            <> ",",
          "      \"pass_rate\": " <> float.to_string(sc.pass_rate),
          "    }",
        ],
        "\n",
      )
    })
    |> string.join(",\n")

  "{\n" <> items <> "\n  }"
}

/// Format string list as JSON
fn format_string_list_json(items: List(String)) -> String {
  case items {
    [] -> "[]"
    _ -> {
      let quoted =
        items
        |> list.map(fn(s) { "\"" <> s <> "\"" })
        |> string.join(", ")
      "[" <> quoted <> "]"
    }
  }
}

// ============ Utility Functions ============

/// Convert float to percentage string
fn float_to_percent(value: Float) -> String {
  let rounded = float.round(value) |> int.to_string
  rounded <> "%"
}

/// Convert status to emoji
fn status_to_emoji(status: CoverageStatus) -> String {
  case status {
    WellTested -> "✅"
    PartiallyTested -> "🟡"
    MinimallyTested -> "🟠"
    Untested -> "❌"
  }
}

/// Convert status to CSS class
fn status_to_class(status: CoverageStatus) -> String {
  case status {
    WellTested -> "status-well-tested"
    PartiallyTested -> "status-partially-tested"
    MinimallyTested -> "status-minimally-tested"
    Untested -> "status-untested"
  }
}

/// Convert status to text
fn status_to_text(status: CoverageStatus) -> String {
  case status {
    WellTested -> "Well Tested"
    PartiallyTested -> "Partially Tested"
    MinimallyTested -> "Minimally Tested"
    Untested -> "Untested"
  }
}

/// Convert logic system to string
fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    proposition.K -> "K"
    proposition.T -> "T"
    proposition.K4 -> "K4"
    proposition.S4 -> "S4"
    proposition.S5 -> "S5"
    proposition.KD -> "KD"
    proposition.KD45 -> "KD45"
  }
}

/// Check if coverage meets threshold
pub fn meets_threshold(report: CoverageReport, threshold: Float) -> Bool {
  let rule_pct = report.summary.rule_coverage_percent /. 100.0
  let axiom_pct = report.summary.axiom_coverage_percent /. 100.0
  let avg = { rule_pct +. axiom_pct } /. 2.0

  avg >=. threshold
}

/// Get coverage grade
pub fn coverage_grade(report: CoverageReport) -> String {
  let avg =
    {
      report.summary.rule_coverage_percent
      +. report.summary.axiom_coverage_percent
    }
    /. 2.0

  case avg {
    a if a >=. 90.0 -> "A"
    a if a >=. 80.0 -> "B"
    a if a >=. 70.0 -> "C"
    a if a >=. 60.0 -> "D"
    _ -> "F"
  }
}

/// Get summary string
pub fn summary_string(report: CoverageReport) -> String {
  let s = report.summary
  string.join(
    [
      "Coverage Report Summary",
      "======================",
      "Rules: "
        <> int.to_string(s.tested_rules)
        <> "/"
        <> int.to_string(s.total_rules)
        <> " ("
        <> float_to_percent(s.rule_coverage_percent)
        <> ")",
      "Axioms: "
        <> int.to_string(s.tested_axioms)
        <> "/"
        <> int.to_string(s.total_axioms)
        <> " ("
        <> float_to_percent(s.axiom_coverage_percent)
        <> ")",
      "Pass Rate: " <> float_to_percent(s.overall_pass_rate),
      "Grade: " <> coverage_grade(report),
    ],
    "\n",
  )
}
