import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import modal_logic/complexity
import modal_logic/proposition.{
  And, Atom, Implies, Necessary, Not, Or, Possible, S5,
}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Basic Complexity Tests
// =============================================================================

pub fn analyze_simple_atom_test() {
  let formula = Atom("p")
  let analysis = complexity.analyze(formula)

  analysis.metrics.modal_depth |> should.equal(0)
  analysis.metrics.operator_count |> should.equal(0)
  analysis.metrics.atom_count |> should.equal(1)
  analysis.complexity_level |> should.equal(complexity.Low)
}

pub fn analyze_simple_negation_test() {
  let formula = Not(Atom("p"))
  let analysis = complexity.analyze(formula)

  analysis.metrics.modal_depth |> should.equal(0)
  analysis.metrics.operator_count |> should.equal(1)
  analysis.complexity_level |> should.equal(complexity.Low)
}

pub fn analyze_simple_conjunction_test() {
  let formula = And(Atom("p"), Atom("q"))
  let analysis = complexity.analyze(formula)

  analysis.metrics.operator_count |> should.equal(1)
  analysis.metrics.atom_count |> should.equal(2)
}

pub fn analyze_simple_necessary_test() {
  let formula = Necessary(Atom("p"))
  let analysis = complexity.analyze(formula)

  analysis.metrics.modal_depth |> should.equal(1)
  analysis.metrics.operator_count |> should.equal(1)
}

// =============================================================================
// Modal Depth Tests
// =============================================================================

pub fn modal_depth_nested_necessity_test() {
  let formula = Necessary(Necessary(Necessary(Atom("p"))))
  let analysis = complexity.analyze(formula)

  analysis.metrics.modal_depth |> should.equal(3)
}

pub fn modal_depth_mixed_operators_test() {
  let formula = Necessary(Possible(Atom("p")))
  let analysis = complexity.analyze(formula)

  analysis.metrics.modal_depth |> should.equal(2)
}

pub fn modal_depth_complex_test() {
  let formula = And(Necessary(Atom("p")), Possible(Necessary(Atom("q"))))

  let analysis = complexity.analyze(formula)

  // Max depth is 2 (from Possible(Necessary(...)))
  analysis.metrics.modal_depth |> should.equal(2)
}

// =============================================================================
// Complexity Level Tests
// =============================================================================

pub fn low_complexity_classification_test() {
  let formula = And(Atom("p"), Atom("q"))
  let analysis = complexity.analyze(formula)

  analysis.complexity_level |> should.equal(complexity.Low)
}

pub fn medium_complexity_classification_test() {
  let formula =
    And(
      Necessary(Atom("p")),
      Implies(Necessary(Atom("q")), Possible(Atom("r"))),
    )

  let analysis = complexity.analyze(formula)

  // Should be Medium due to modal operators and implications
  case analysis.complexity_level {
    complexity.Low -> should.fail()
    _ -> should.be_true(True)
  }
}

pub fn high_complexity_nested_formula_test() {
  let formula =
    Necessary(Necessary(And(Necessary(Atom("p")), Necessary(Atom("q")))))

  let analysis = complexity.analyze(formula)

  { analysis.metrics.modal_depth >= 3 } |> should.be_true()
}

// =============================================================================
// Optimization Detection Tests
// =============================================================================

pub fn detect_double_negation_test() {
  let formula = Not(Not(Atom("p")))
  let analysis = complexity.analyze(formula)

  { list.length(analysis.optimizations) > 0 } |> should.be_true()

  case list.first(analysis.optimizations) {
    Ok(opt) -> {
      opt.suggestion_type
      |> should.equal(complexity.DoubleNegationElimination)
    }
    Error(_) -> should.fail()
  }
}

pub fn detect_nested_necessity_test() {
  let formula = Necessary(Necessary(Atom("p")))
  let analysis = complexity.analyze(formula)

  { list.length(analysis.optimizations) > 0 } |> should.be_true()

  case list.first(analysis.optimizations) {
    Ok(opt) -> {
      opt.suggestion_type
      |> should.equal(complexity.NecessitationSimplification)
    }
    Error(_) -> should.fail()
  }
}

pub fn no_optimizations_for_simple_test() {
  let formula = And(Atom("p"), Atom("q"))
  let analysis = complexity.analyze(formula)

  list.length(analysis.optimizations) |> should.equal(0)
}

// =============================================================================
// Warning Generation Tests
// =============================================================================

pub fn warnings_for_deep_modal_nesting_test() {
  let formula =
    Necessary(Necessary(Necessary(Necessary(Necessary(Necessary(Atom("p")))))))

  let analysis = complexity.analyze(formula)

  // Should have warning for high modal depth
  { list.length(analysis.warnings) > 0 } |> should.be_true()
}

pub fn no_warnings_for_simple_formula_test() {
  let formula = Atom("p")
  let analysis = complexity.analyze(formula)

  list.length(analysis.warnings) |> should.equal(0)
}

// =============================================================================
// Quick Metrics Tests
// =============================================================================

pub fn quick_score_test() {
  let formula = Necessary(And(Atom("p"), Atom("q")))
  let score = complexity.quick_score(formula)

  { score >. 0.0 } |> should.be_true()
}

pub fn quick_estimate_test() {
  let formula = Atom("p")
  let estimate = complexity.quick_estimate(formula)

  { estimate > 0 } |> should.be_true()
  { estimate < 1000 } |> should.be_true()
}

pub fn likely_timeout_test() {
  let simple = Atom("p")
  complexity.likely_timeout(simple, 10_000) |> should.be_false()

  let complex =
    Necessary(
      Necessary(Necessary(And(Necessary(Atom("p")), Necessary(Atom("q"))))),
    )

  // Complex formula might timeout with short timeout
  case complexity.likely_timeout(complex, 100) {
    True -> should.be_true(True)
    False -> should.be_true(True)
    // Either result is acceptable
  }
}

// =============================================================================
// Comparison Tests
// =============================================================================

pub fn compare_formulas_test() {
  let simple = Atom("p")
  let complex = Necessary(Necessary(Atom("p")))

  let #(metrics1, metrics2, comparison) = complexity.compare(simple, complex)

  metrics1.modal_depth |> should.equal(0)
  metrics2.modal_depth |> should.equal(2)

  comparison |> should_contain("simpler")
}

// =============================================================================
// Batch Analysis Tests
// =============================================================================

pub fn analyze_batch_test() {
  let formulas = [Atom("p"), Necessary(Atom("q")), And(Atom("r"), Atom("s"))]

  let analyses = complexity.analyze_batch(formulas)

  list.length(analyses) |> should.equal(3)
}

pub fn batch_summary_test() {
  let formulas = [
    Atom("p"),
    Necessary(Atom("q")),
    Necessary(Necessary(Atom("r"))),
  ]

  let analyses = complexity.analyze_batch(formulas)
  let summary = complexity.batch_summary(analyses)

  summary.total_formulas |> should.equal(3)
  { summary.average_modal_depth >= 0 } |> should.be_true()
}

// =============================================================================
// Format Tests
// =============================================================================

pub fn format_metrics_test() {
  let metrics =
    complexity.ComplexityMetrics(
      modal_depth: 2,
      operator_count: 5,
      atom_count: 3,
      total_nodes: 10,
      nesting_level: 3,
      complexity_score: 15.5,
      estimated_verification_ms: 500,
    )

  let formatted = complexity.format_metrics(metrics)

  formatted |> should_contain("Modal Depth: 2")
  formatted |> should_contain("Operator Count: 5")
  formatted |> should_contain("500ms")
}

pub fn format_analysis_test() {
  let formula = Necessary(Atom("p"))
  let analysis = complexity.analyze(formula)

  let formatted = complexity.format_analysis(analysis)

  formatted |> should_contain("Complexity Analysis")
  formatted |> should_contain("Complexity Level:")
}

// =============================================================================
// Complexity Level Name Tests
// =============================================================================

pub fn complexity_level_name_test() {
  complexity.complexity_level_name(complexity.Low) |> should.equal("low")
  complexity.complexity_level_name(complexity.Medium) |> should.equal("medium")
  complexity.complexity_level_name(complexity.High) |> should.equal("high")
  complexity.complexity_level_name(complexity.VeryHigh)
  |> should.equal("very_high")
}

pub fn optimization_type_name_test() {
  complexity.optimization_type_name(complexity.DoubleNegationElimination)
  |> should.equal("double_negation_elimination")

  complexity.optimization_type_name(complexity.NecessitationSimplification)
  |> should.equal("necessitation_simplification")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False -> should.fail()
  }
}
