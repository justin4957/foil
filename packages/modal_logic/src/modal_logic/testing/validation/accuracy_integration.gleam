//// Accuracy Integration
////
//// This module integrates the philosophical testing framework with
//// the accuracy testing metrics system.

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/proposition.{type Proposition, K}
import modal_logic/rules/rule_store.{type RuleStore}
import modal_logic/testing/accuracy/accuracy_tests.{
  type AccuracyResults, type EndToEndMetrics, type LogicDetectionMetrics,
  type OverallMetrics, type TranslationMetrics, type ValidationMetrics,
  AccuracyResults, EndToEndMetrics, LogicDetectionMetrics, OverallMetrics,
  TranslationMetrics, ValidationMetrics,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture, TestFixture}
import modal_logic/testing/test_config.{
  ClassicArgument, ExpectedInvalid, ExpectedValid, Medium,
}
import modal_logic/testing/validation/argument_corpus.{
  type PhilosophicalArgument, all_arguments, category_to_string,
}
import modal_logic/testing/validation/philosophical_tester.{
  type PhilosophicalTestConfig, type PhilosophicalTestResult, run_tests,
}

// ============ Core Types ============

/// Combined accuracy result from philosophical testing
pub type PhilosophicalAccuracyResult {
  PhilosophicalAccuracyResult(
    /// Philosophical test results
    philosophical: PhilosophicalTestResult,
    /// Converted accuracy metrics
    accuracy: AccuracyResults,
    /// Per-category breakdown
    category_breakdown: List(CategoryAccuracy),
  )
}

/// Accuracy breakdown by category
pub type CategoryAccuracy {
  CategoryAccuracy(category: String, total: Int, correct: Int, accuracy: Float)
}

// ============ Integration Functions ============

/// Run philosophical tests and convert results to accuracy metrics
pub fn run_with_accuracy_metrics(
  store: RuleStore,
  config: PhilosophicalTestConfig,
) -> PhilosophicalAccuracyResult {
  // Run philosophical tests
  let philo_result = run_tests(store, config)

  // Convert to accuracy metrics
  let accuracy = convert_to_accuracy_metrics(philo_result)

  // Calculate category breakdown
  let breakdown = calculate_category_breakdown(philo_result)

  PhilosophicalAccuracyResult(
    philosophical: philo_result,
    accuracy: accuracy,
    category_breakdown: breakdown,
  )
}

/// Convert philosophical test results to accuracy metrics format
pub fn convert_to_accuracy_metrics(
  result: PhilosophicalTestResult,
) -> AccuracyResults {
  let total = result.total_tested
  let correct = result.correctly_validated
  let incorrect = result.incorrectly_validated

  // Translation metrics - based on whether we could process arguments
  let translation =
    TranslationMetrics(
      total: total,
      exact_matches: correct,
      partial_matches: 0,
      no_matches: incorrect,
      avg_proposition_match: result.soundness_assessment.score,
      confidence_correlation: result.soundness_assessment.score,
    )

  // Logic detection metrics - all philosophical arguments have known systems
  let logic_detection =
    LogicDetectionMetrics(total: total, correct: total, by_system: [])

  // Validation metrics - based on pass/fail
  let true_positives =
    list.count(result.argument_results, fn(r) {
      r.passed && r.expected_valid == True
    })

  let true_negatives =
    list.count(result.argument_results, fn(r) {
      r.passed && r.expected_valid == False
    })

  let false_positives =
    list.count(result.argument_results, fn(r) {
      !r.passed && r.expected_valid == False
    })

  let false_negatives =
    list.count(result.argument_results, fn(r) {
      !r.passed && r.expected_valid == True
    })

  let precision = safe_divide(true_positives, true_positives + false_positives)
  let recall = safe_divide(true_positives, true_positives + false_negatives)
  let f1 = safe_divide_float(2.0 *. precision *. recall, precision +. recall)

  let validation =
    ValidationMetrics(
      total: total,
      true_positives: true_positives,
      true_negatives: true_negatives,
      false_positives: false_positives,
      false_negatives: false_negatives,
      precision: precision,
      recall: recall,
      f1_score: f1,
    )

  // End-to-end metrics
  let end_to_end =
    EndToEndMetrics(
      total: total,
      successful: correct,
      failed_translation: 0,
      failed_validation: incorrect,
      avg_duration_ms: 1,
    )

  // Overall metrics
  let accuracy_score = result.soundness_assessment.score *. 100.0
  let grade = case accuracy_score {
    a if a >=. 90.0 -> "A"
    a if a >=. 80.0 -> "B"
    a if a >=. 70.0 -> "C"
    a if a >=. 60.0 -> "D"
    _ -> "F"
  }

  let overall =
    OverallMetrics(
      total_tests: total,
      fully_passed: correct,
      accuracy: accuracy_score,
      grade: grade,
    )

  AccuracyResults(
    translation: translation,
    logic_detection: logic_detection,
    validation: validation,
    validation_by_system: [],
    validation_by_complexity: [],
    end_to_end: end_to_end,
    overall: overall,
  )
}

/// Calculate accuracy breakdown by category
fn calculate_category_breakdown(
  result: PhilosophicalTestResult,
) -> List(CategoryAccuracy) {
  // Group results by category
  let categories = [
    "Modal",
    "Epistemic",
    "Deontic",
    "Classical",
    "Historical",
    "Fallacy",
  ]

  list.map(categories, fn(cat) {
    let category_results =
      list.filter(result.argument_results, fn(r) {
        category_to_string(r.argument.category) == cat
      })

    let total = list.length(category_results)
    let correct = list.count(category_results, fn(r) { r.passed })
    let accuracy = case total {
      0 -> 0.0
      _ -> int.to_float(correct) /. int.to_float(total)
    }

    CategoryAccuracy(
      category: cat,
      total: total,
      correct: correct,
      accuracy: accuracy,
    )
  })
  |> list.filter(fn(c) { c.total > 0 })
}

/// Safe float division for Int parameters
fn safe_divide(numerator: Int, denominator: Int) -> Float {
  case denominator {
    0 -> 0.0
    _ -> int.to_float(numerator) /. int.to_float(denominator)
  }
}

/// Safe float division for Float parameters
fn safe_divide_float(numerator: Float, denominator: Float) -> Float {
  case denominator {
    0.0 -> 0.0
    _ -> numerator /. denominator
  }
}

// ============ Fixture Conversion ============

/// Convert philosophical arguments to test fixtures
pub fn arguments_to_fixtures(
  arguments: List(PhilosophicalArgument),
) -> List(TestFixture) {
  list.map(arguments, argument_to_fixture)
}

/// Convert a single philosophical argument to a test fixture
pub fn argument_to_fixture(arg: PhilosophicalArgument) -> TestFixture {
  // Get the first valid logic system, default to K if none specified
  let logic_system = case arg.valid_in {
    [sys, ..] -> sys
    [] -> K
  }

  TestFixture(
    id: arg.id,
    name: arg.name,
    category: ClassicArgument,
    natural_language: arg.description,
    expected_logic_system: logic_system,
    expected_premises: arg.premises,
    expected_conclusion: arg.conclusion,
    expected_validity: case arg.is_valid {
      True -> ExpectedValid
      False -> ExpectedInvalid(Some(arg.description))
    },
    difficulty: Medium,
    tags: arg.tags,
    source: arg.source,
  )
}

/// Get all philosophical arguments as test fixtures
pub fn all_fixtures() -> List(TestFixture) {
  arguments_to_fixtures(all_arguments())
}

// ============ Reporting ============

/// Format combined accuracy report
pub fn format_combined_report(result: PhilosophicalAccuracyResult) -> String {
  let category_section =
    result.category_breakdown
    |> list.map(fn(c) {
      "  "
      <> c.category
      <> ": "
      <> int.to_string(c.correct)
      <> "/"
      <> int.to_string(c.total)
      <> " ("
      <> int.to_string(float.round(c.accuracy *. 100.0))
      <> "%)\n"
    })
    |> string.concat

  string.concat([
    "\n",
    string.repeat("=", 70),
    "\n",
    "PHILOSOPHICAL ARGUMENT ACCURACY REPORT",
    "\n",
    string.repeat("=", 70),
    "\n\n",
    "## Summary\n",
    "  Total Arguments: ",
    int.to_string(result.philosophical.total_tested),
    "\n",
    "  Correctly Validated: ",
    int.to_string(result.philosophical.correctly_validated),
    "\n",
    "  Incorrectly Validated: ",
    int.to_string(result.philosophical.incorrectly_validated),
    "\n",
    "  Soundness Score: ",
    int.to_string(float.round(
      result.philosophical.soundness_assessment.score *. 100.0,
    )),
    "%\n",
    "  Grade: ",
    result.accuracy.overall.grade,
    "\n",
    "\n",
    "## Category Breakdown\n",
    category_section,
    "\n",
    "## Validation Metrics\n",
    "  Precision: ",
    int.to_string(float.round(result.accuracy.validation.precision *. 100.0)),
    "%\n",
    "  Recall: ",
    int.to_string(float.round(result.accuracy.validation.recall *. 100.0)),
    "%\n",
    "  F1 Score: ",
    int.to_string(float.round(result.accuracy.validation.f1_score *. 100.0)),
    "%\n",
    "\n",
    string.repeat("-", 70),
    "\n",
  ])
}

/// Quick accuracy check - returns pass/fail and score
pub fn quick_accuracy_check(
  store: RuleStore,
  config: PhilosophicalTestConfig,
) -> #(Bool, Float) {
  let result = run_with_accuracy_metrics(store, config)
  let passed = result.philosophical.soundness_assessment.score >=. 0.5
  #(passed, result.philosophical.soundness_assessment.score)
}
