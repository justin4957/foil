//// Formula Complexity Metrics and Optimization
////
//// Analyzes formula complexity and provides optimization suggestions
//// to help users understand verification performance and write more
//// efficient formulas.

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, K4, KD,
  KD45, Knows, Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}

// =============================================================================
// Types
// =============================================================================

/// Complexity metrics for a formula
pub type ComplexityMetrics {
  ComplexityMetrics(
    modal_depth: Int,
    operator_count: Int,
    atom_count: Int,
    total_nodes: Int,
    nesting_level: Int,
    complexity_score: Float,
    estimated_verification_ms: Int,
  )
}

/// Optimization suggestion
pub type Optimization {
  Optimization(
    suggestion_type: OptimizationType,
    description: String,
    original: String,
    optimized: String,
    expected_improvement: String,
  )
}

/// Type of optimization
pub type OptimizationType {
  DoubleNegationElimination
  ModalDistribution
  NecessitationSimplification
  DeMorganTransform
  ImplicationSimplification
  RedundancyRemoval
}

/// Complexity analysis result
pub type ComplexityAnalysis {
  ComplexityAnalysis(
    formula: String,
    metrics: ComplexityMetrics,
    complexity_level: ComplexityLevel,
    optimizations: List(Optimization),
    warnings: List(String),
  )
}

/// Complexity level classification
pub type ComplexityLevel {
  Low
  Medium
  High
  VeryHigh
}

// =============================================================================
// Complexity Analysis
// =============================================================================

/// Analyze formula complexity
pub fn analyze(formula: Proposition) -> ComplexityAnalysis {
  let metrics = calculate_metrics(formula)
  let level = classify_complexity(metrics)
  let optimizations = find_optimizations(formula)
  let warnings = generate_warnings(metrics, level)

  ComplexityAnalysis(
    formula: string.inspect(formula),
    metrics: metrics,
    complexity_level: level,
    optimizations: optimizations,
    warnings: warnings,
  )
}

/// Calculate complexity metrics
fn calculate_metrics(formula: Proposition) -> ComplexityMetrics {
  let modal_depth = calculate_modal_depth(formula)
  let operator_count = count_operators(formula)
  let atom_count = count_atoms(formula)
  let total_nodes = count_total_nodes(formula)
  let nesting_level = calculate_nesting_level(formula)

  // Complexity score: weighted combination
  let complexity_score =
    int.to_float(modal_depth)
    *. 3.0
    +. int.to_float(operator_count)
    *. 1.5
    +. int.to_float(nesting_level)
    *. 2.0

  // Estimate verification time based on complexity
  let estimated_ms = estimate_verification_time(complexity_score)

  ComplexityMetrics(
    modal_depth: modal_depth,
    operator_count: operator_count,
    atom_count: atom_count,
    total_nodes: total_nodes,
    nesting_level: nesting_level,
    complexity_score: complexity_score,
    estimated_verification_ms: estimated_ms,
  )
}

/// Calculate modal operator depth (max nesting of □, ◇)
fn calculate_modal_depth(formula: Proposition) -> Int {
  case formula {
    Atom(_) -> 0
    Not(p) -> calculate_modal_depth(p)
    And(p, q) | Or(p, q) | Implies(p, q) ->
      int.max(calculate_modal_depth(p), calculate_modal_depth(q))
    Necessary(p) | Possible(p) | Obligatory(p) | Permitted(p) ->
      1 + calculate_modal_depth(p)
    Knows(_, p) | Believes(_, p) -> 1 + calculate_modal_depth(p)
  }
}

/// Count total operators
fn count_operators(formula: Proposition) -> Int {
  case formula {
    Atom(_) -> 0
    Not(p) -> 1 + count_operators(p)
    And(p, q) | Or(p, q) | Implies(p, q) ->
      1 + count_operators(p) + count_operators(q)
    Necessary(p) | Possible(p) | Obligatory(p) | Permitted(p) ->
      1 + count_operators(p)
    Knows(_, p) | Believes(_, p) -> 1 + count_operators(p)
  }
}

/// Count atomic propositions
fn count_atoms(formula: Proposition) -> Int {
  case formula {
    Atom(_) -> 1
    Not(p) -> count_atoms(p)
    And(p, q) | Or(p, q) | Implies(p, q) -> count_atoms(p) + count_atoms(q)
    Necessary(p) | Possible(p) | Obligatory(p) | Permitted(p) -> count_atoms(p)
    Knows(_, p) | Believes(_, p) -> count_atoms(p)
  }
}

/// Count total nodes in formula tree
fn count_total_nodes(formula: Proposition) -> Int {
  case formula {
    Atom(_) -> 1
    Not(p) -> 1 + count_total_nodes(p)
    And(p, q) | Or(p, q) | Implies(p, q) ->
      1 + count_total_nodes(p) + count_total_nodes(q)
    Necessary(p) | Possible(p) | Obligatory(p) | Permitted(p) ->
      1 + count_total_nodes(p)
    Knows(_, p) | Believes(_, p) -> 1 + count_total_nodes(p)
  }
}

/// Calculate maximum nesting level
fn calculate_nesting_level(formula: Proposition) -> Int {
  case formula {
    Atom(_) -> 0
    Not(p) -> calculate_nesting_level(p)
    And(p, q) | Or(p, q) | Implies(p, q) ->
      1 + int.max(calculate_nesting_level(p), calculate_nesting_level(q))
    Necessary(p) | Possible(p) | Obligatory(p) | Permitted(p) ->
      1 + calculate_nesting_level(p)
    Knows(_, p) | Believes(_, p) -> 1 + calculate_nesting_level(p)
  }
}

/// Estimate verification time based on complexity score
fn estimate_verification_time(score: Float) -> Int {
  case score {
    s if s <. 5.0 -> 100
    s if s <. 15.0 -> 500
    s if s <. 30.0 -> 2000
    s if s <. 50.0 -> 5000
    _ -> 10_000
  }
}

/// Classify complexity level
fn classify_complexity(metrics: ComplexityMetrics) -> ComplexityLevel {
  case metrics.complexity_score {
    s if s <. 10.0 -> Low
    s if s <. 25.0 -> Medium
    s if s <. 50.0 -> High
    _ -> VeryHigh
  }
}

// =============================================================================
// Optimization Detection
// =============================================================================

/// Find optimization opportunities
fn find_optimizations(formula: Proposition) -> List(Optimization) {
  list.flatten([
    find_double_negation(formula),
    find_modal_distribution(formula),
    find_necessitation_simplification(formula),
  ])
}

/// Find double negation (¬¬p → p)
fn find_double_negation(formula: Proposition) -> List(Optimization) {
  case formula {
    Not(Not(p)) -> [
      Optimization(
        suggestion_type: DoubleNegationElimination,
        description: "Remove double negation",
        original: "¬¬" <> string.inspect(p),
        optimized: string.inspect(p),
        expected_improvement: "Simpler formula, faster verification",
      ),
    ]
    Not(p) -> find_double_negation(p)
    And(p, q) | Or(p, q) | Implies(p, q) ->
      list.append(find_double_negation(p), find_double_negation(q))
    Necessary(p) | Possible(p) | Obligatory(p) | Permitted(p) ->
      find_double_negation(p)
    Knows(_, p) | Believes(_, p) -> find_double_negation(p)
    Atom(_) -> []
  }
}

/// Find modal distribution opportunities
fn find_modal_distribution(_formula: Proposition) -> List(Optimization) {
  // Simplified: Return empty for now
  // Full implementation would detect □(p ∧ q) → □p ∧ □q patterns
  []
}

/// Find necessitation simplification (□□p → □p in K4, S4, S5)
fn find_necessitation_simplification(formula: Proposition) -> List(Optimization) {
  case formula {
    Necessary(Necessary(p)) -> [
      Optimization(
        suggestion_type: NecessitationSimplification,
        description: "Simplify nested necessity (valid in K4, S4, S5)",
        original: "□□" <> string.inspect(p),
        optimized: "□" <> string.inspect(p),
        expected_improvement: "Reduced modal depth, faster in transitive systems",
      ),
    ]
    Necessary(p) -> find_necessitation_simplification(p)
    Possible(p) | Obligatory(p) | Permitted(p) ->
      find_necessitation_simplification(p)
    Not(p) -> find_necessitation_simplification(p)
    And(p, q) | Or(p, q) | Implies(p, q) ->
      list.append(
        find_necessitation_simplification(p),
        find_necessitation_simplification(q),
      )
    Knows(_, p) | Believes(_, p) -> find_necessitation_simplification(p)
    Atom(_) -> []
  }
}

// =============================================================================
// Warnings Generation
// =============================================================================

/// Generate warnings based on metrics
fn generate_warnings(
  metrics: ComplexityMetrics,
  level: ComplexityLevel,
) -> List(String) {
  let warnings = []

  let warnings = case metrics.modal_depth > 5 {
    True ->
      list.append(warnings, [
        "High modal depth ("
        <> int.to_string(metrics.modal_depth)
        <> ") may cause slow verification",
      ])
    False -> warnings
  }

  let warnings = case metrics.operator_count > 20 {
    True ->
      list.append(warnings, [
        "Many operators ("
        <> int.to_string(metrics.operator_count)
        <> ") may increase verification time",
      ])
    False -> warnings
  }

  let warnings = case level {
    VeryHigh ->
      list.append(warnings, [
        "Very high complexity - consider simplifying or splitting into smaller formulas",
      ])
    _ -> warnings
  }

  let warnings = case metrics.estimated_verification_ms > 5000 {
    True ->
      list.append(warnings, [
        "Estimated verification time: "
        <> int.to_string(metrics.estimated_verification_ms)
        <> "ms - may timeout",
      ])
    False -> warnings
  }

  warnings
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get complexity level name
pub fn complexity_level_name(level: ComplexityLevel) -> String {
  case level {
    Low -> "low"
    Medium -> "medium"
    High -> "high"
    VeryHigh -> "very_high"
  }
}

/// Get optimization type name
pub fn optimization_type_name(opt_type: OptimizationType) -> String {
  case opt_type {
    DoubleNegationElimination -> "double_negation_elimination"
    ModalDistribution -> "modal_distribution"
    NecessitationSimplification -> "necessitation_simplification"
    DeMorganTransform -> "de_morgan_transform"
    ImplicationSimplification -> "implication_simplification"
    RedundancyRemoval -> "redundancy_removal"
  }
}

/// Format metrics as string
pub fn format_metrics(metrics: ComplexityMetrics) -> String {
  "Complexity Metrics:\n"
  <> "  Modal Depth: "
  <> int.to_string(metrics.modal_depth)
  <> "\n"
  <> "  Operator Count: "
  <> int.to_string(metrics.operator_count)
  <> "\n"
  <> "  Atom Count: "
  <> int.to_string(metrics.atom_count)
  <> "\n"
  <> "  Total Nodes: "
  <> int.to_string(metrics.total_nodes)
  <> "\n"
  <> "  Nesting Level: "
  <> int.to_string(metrics.nesting_level)
  <> "\n"
  <> "  Complexity Score: "
  <> float.to_string(metrics.complexity_score)
  <> "\n"
  <> "  Estimated Verification: "
  <> int.to_string(metrics.estimated_verification_ms)
  <> "ms"
}

/// Format analysis result
pub fn format_analysis(analysis: ComplexityAnalysis) -> String {
  let header = "Complexity Analysis for: " <> analysis.formula <> "\n\n"

  let metrics_str = format_metrics(analysis.metrics) <> "\n"

  let level_str =
    "\nComplexity Level: "
    <> complexity_level_name(analysis.complexity_level)
    <> "\n"

  let opts_str = case list.length(analysis.optimizations) {
    0 -> "\nNo optimization suggestions found.\n"
    count -> {
      "\nOptimization Suggestions ("
      <> int.to_string(count)
      <> "):\n"
      <> format_optimizations(analysis.optimizations)
    }
  }

  let warnings_str = case list.length(analysis.warnings) {
    0 -> ""
    _ -> "\nWarnings:\n" <> format_warnings(analysis.warnings)
  }

  header <> metrics_str <> level_str <> opts_str <> warnings_str
}

fn format_optimizations(optimizations: List(Optimization)) -> String {
  optimizations
  |> list.index_map(fn(opt, i) {
    "  "
    <> int.to_string(i + 1)
    <> ". "
    <> opt.description
    <> "\n"
    <> "     Original: "
    <> opt.original
    <> "\n"
    <> "     Optimized: "
    <> opt.optimized
    <> "\n"
    <> "     Improvement: "
    <> opt.expected_improvement
  })
  |> string.join("\n\n")
}

fn format_warnings(warnings: List(String)) -> String {
  warnings
  |> list.map(fn(w) { "  ⚠️  " <> w })
  |> string.join("\n")
}

// =============================================================================
// Quick Metrics (for simple formulas)
// =============================================================================

/// Get just the complexity score
pub fn quick_score(formula: Proposition) -> Float {
  let metrics = calculate_metrics(formula)
  metrics.complexity_score
}

/// Get estimated verification time
pub fn quick_estimate(formula: Proposition) -> Int {
  let metrics = calculate_metrics(formula)
  metrics.estimated_verification_ms
}

/// Check if formula is likely to timeout
pub fn likely_timeout(formula: Proposition, timeout_ms: Int) -> Bool {
  let estimated = quick_estimate(formula)
  estimated > timeout_ms
}

// =============================================================================
// Complexity Comparison
// =============================================================================

/// Compare complexity of two formulas
pub fn compare(
  formula1: Proposition,
  formula2: Proposition,
) -> #(ComplexityMetrics, ComplexityMetrics, String) {
  let metrics1 = calculate_metrics(formula1)
  let metrics2 = calculate_metrics(formula2)

  let comparison = case
    float.compare(metrics1.complexity_score, metrics2.complexity_score)
  {
    order.Lt -> "Formula 1 is simpler than Formula 2"
    order.Eq -> "Formulas have equal complexity"
    order.Gt -> "Formula 1 is more complex than Formula 2"
  }

  #(metrics1, metrics2, comparison)
}

// =============================================================================
// Batch Complexity Analysis
// =============================================================================

/// Analyze multiple formulas
pub fn analyze_batch(formulas: List(Proposition)) -> List(ComplexityAnalysis) {
  formulas
  |> list.map(analyze)
}

/// Get summary statistics for batch
pub fn batch_summary(
  analyses: List(ComplexityAnalysis),
) -> BatchComplexitySummary {
  let total = list.length(analyses)

  let avg_modal_depth =
    analyses
    |> list.map(fn(a) { a.metrics.modal_depth })
    |> list_average

  let avg_complexity =
    analyses
    |> list.map(fn(a) { a.metrics.complexity_score })
    |> float_list_average

  let by_level =
    analyses
    |> list.group(fn(a) { complexity_level_name(a.complexity_level) })
    |> dict.map_values(fn(_, group) { list.length(group) })

  BatchComplexitySummary(
    total_formulas: total,
    average_modal_depth: avg_modal_depth,
    average_complexity_score: avg_complexity,
    by_complexity_level: by_level,
  )
}

pub type BatchComplexitySummary {
  BatchComplexitySummary(
    total_formulas: Int,
    average_modal_depth: Int,
    average_complexity_score: Float,
    by_complexity_level: Dict(String, Int),
  )
}

fn list_average(numbers: List(Int)) -> Int {
  case list.length(numbers) {
    0 -> 0
    count -> {
      let sum = list.fold(numbers, 0, fn(acc, n) { acc + n })
      sum / count
    }
  }
}

fn float_list_average(numbers: List(Float)) -> Float {
  case list.length(numbers) {
    0 -> 0.0
    count -> {
      let sum = list.fold(numbers, 0.0, fn(acc, n) { acc +. n })
      sum /. int.to_float(count)
    }
  }
}

import gleam/dict.{type Dict}
import gleam/order
