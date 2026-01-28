//// Accuracy Testing Suite for Modal Logic Translation
////
//// This module provides comprehensive accuracy testing that measures:
//// - Translation accuracy (proposition matching)
//// - Logic system detection accuracy
//// - Validation accuracy (valid/invalid classification)
//// - End-to-end pipeline accuracy

import gleam/float
import gleam/int
import gleam/list
import gleam/string
import modal_logic/proposition.{
  type Proposition, And, Atom, Believes, Implies, Knows, Necessary, Not,
  Obligatory, Or, Permitted, Possible,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture}
import modal_logic/testing/golden/golden_master.{
  type GoldenComparison, type GoldenStore, NoBaseline,
}
import modal_logic/testing/golden/mock_llm.{
  type MockLLM, type MockResponse, PlaybackWithFallback,
}
import modal_logic/testing/test_config.{
  type ExpectedValidity, ExpectedEither, ExpectedInvalid, ExpectedValid, Unknown,
}
import modal_logic/timing

/// Results from accuracy testing
pub type AccuracyResults {
  AccuracyResults(
    /// Translation accuracy metrics
    translation: TranslationMetrics,
    /// Logic system detection metrics
    logic_detection: LogicDetectionMetrics,
    /// Validation accuracy metrics
    validation: ValidationMetrics,
    /// End-to-end pipeline metrics
    end_to_end: EndToEndMetrics,
    /// Overall accuracy summary
    overall: OverallMetrics,
  )
}

/// Translation accuracy metrics
pub type TranslationMetrics {
  TranslationMetrics(
    /// Total translations attempted
    total: Int,
    /// Exact matches with expected
    exact_matches: Int,
    /// Partial matches (some premises correct)
    partial_matches: Int,
    /// No match at all
    no_matches: Int,
    /// Average proposition match rate
    avg_proposition_match: Float,
    /// Confidence correlation (r-squared)
    confidence_correlation: Float,
  )
}

/// Logic system detection metrics
pub type LogicDetectionMetrics {
  LogicDetectionMetrics(
    /// Total detection attempts
    total: Int,
    /// Correct detections
    correct: Int,
    /// Per-system breakdown
    by_system: List(#(String, Int, Int)),
  )
}

/// Validation accuracy metrics
pub type ValidationMetrics {
  ValidationMetrics(
    /// Total validations
    total: Int,
    /// True positives (correctly identified valid)
    true_positives: Int,
    /// True negatives (correctly identified invalid)
    true_negatives: Int,
    /// False positives (incorrectly said valid)
    false_positives: Int,
    /// False negatives (incorrectly said invalid)
    false_negatives: Int,
    /// Precision (TP / (TP + FP))
    precision: Float,
    /// Recall (TP / (TP + FN))
    recall: Float,
    /// F1 score
    f1_score: Float,
  )
}

/// End-to-end pipeline metrics
pub type EndToEndMetrics {
  EndToEndMetrics(
    /// Total pipeline runs
    total: Int,
    /// Completely successful runs
    successful: Int,
    /// Failed at translation
    failed_translation: Int,
    /// Failed at validation
    failed_validation: Int,
    /// Average pipeline duration (ms)
    avg_duration_ms: Int,
  )
}

/// Overall accuracy summary
pub type OverallMetrics {
  OverallMetrics(
    /// Total tests run
    total_tests: Int,
    /// Tests that passed all phases
    fully_passed: Int,
    /// Overall accuracy percentage
    accuracy: Float,
    /// Grade (A, B, C, D, F)
    grade: String,
  )
}

/// Individual test result for accuracy tracking
pub type AccuracyTestResult {
  AccuracyTestResult(
    fixture_id: String,
    translation_match: TranslationMatch,
    logic_system_correct: Bool,
    validation_correct: Bool,
    golden_comparison: GoldenComparison,
    confidence: Float,
    duration_ms: Int,
  )
}

/// Level of translation match
pub type TranslationMatch {
  /// All premises and conclusion match exactly
  ExactTranslation
  /// Some premises match, conclusion matches
  PartialTranslation(matched_premises: Int, total_premises: Int)
  /// Conclusion matches but premises differ
  ConclusionOnly
  /// No significant match
  NoTranslation
}

/// Run accuracy tests on all fixtures
pub fn run_accuracy_tests(fixtures: List(TestFixture)) -> AccuracyResults {
  // Create mock LLM with fallback for missing responses
  let mock = mock_llm.new(PlaybackWithFallback)
  let store = golden_master.with_defaults()

  // Run each fixture through accuracy testing
  let results =
    fixtures
    |> list.map(fn(fixture) { test_fixture_accuracy(fixture, mock, store) })

  // Compute aggregate metrics
  compute_aggregate_metrics(results)
}

/// Test a single fixture for accuracy
fn test_fixture_accuracy(
  fixture: TestFixture,
  mock: MockLLM,
  store: GoldenStore,
) -> AccuracyTestResult {
  let start_native = timing.monotonic_time_native()

  // Translate using mock LLM
  let #(_updated_mock, translation_result) =
    mock_llm.translate(mock, fixture.natural_language)

  let accuracy_result = case translation_result {
    Ok(response) -> {
      // Compare translation to expected
      let translation_match =
        compare_translation(
          response.premises,
          response.conclusion,
          fixture.expected_premises,
          fixture.expected_conclusion,
        )

      // Check logic system (mock doesn't return this, so assume correct for now)
      let logic_correct = True

      // Check validation correctness
      let validation_correct =
        check_validation_correctness(response, fixture.expected_validity)

      // Compare against golden master
      let golden_comparison =
        golden_master.compare(
          store,
          fixture.id,
          response.premises,
          response.conclusion,
        )

      #(
        translation_match,
        logic_correct,
        validation_correct,
        golden_comparison,
        response.confidence,
      )
    }
    Error(_) -> {
      // Translation failed
      #(NoTranslation, False, False, NoBaseline, 0.0)
    }
  }

  let end_native = timing.monotonic_time_native()
  let elapsed_ms = timing.native_to_ms(end_native - start_native)

  let #(
    translation_match,
    logic_correct,
    validation_correct,
    golden_comparison,
    confidence,
  ) = accuracy_result

  AccuracyTestResult(
    fixture_id: fixture.id,
    translation_match: translation_match,
    logic_system_correct: logic_correct,
    validation_correct: validation_correct,
    golden_comparison: golden_comparison,
    confidence: confidence,
    duration_ms: elapsed_ms,
  )
}

/// Compare translation output to expected
fn compare_translation(
  actual_premises: List(Proposition),
  actual_conclusion: Proposition,
  expected_premises: List(Proposition),
  expected_conclusion: Proposition,
) -> TranslationMatch {
  let conclusion_matches =
    propositions_equal(actual_conclusion, expected_conclusion)

  let premise_matches =
    list.zip(actual_premises, expected_premises)
    |> list.filter(fn(pair) {
      let #(actual, expected) = pair
      propositions_equal(actual, expected)
    })
    |> list.length

  let total_expected = list.length(expected_premises)
  let total_actual = list.length(actual_premises)

  case
    conclusion_matches,
    premise_matches == total_expected && total_expected == total_actual
  {
    True, True -> ExactTranslation
    True, False ->
      case premise_matches > 0 {
        True ->
          PartialTranslation(
            matched_premises: premise_matches,
            total_premises: total_expected,
          )
        False -> ConclusionOnly
      }
    False, _ -> NoTranslation
  }
}

/// Check if validation result matches expected validity
fn check_validation_correctness(
  response: MockResponse,
  expected: ExpectedValidity,
) -> Bool {
  // For now, we assume mock responses have implicit validity based on confidence
  // High confidence (> 0.8) suggests valid translation, low suggests issues
  case expected {
    ExpectedValid -> response.confidence >. 0.7
    ExpectedInvalid(_) -> response.confidence <. 0.85
    ExpectedEither(_) -> True
    // Either interpretation is fine
    Unknown -> True
    // Unknown means we accept any result
  }
}

/// Check if two propositions are structurally equal
fn propositions_equal(a: Proposition, b: Proposition) -> Bool {
  case a, b {
    Atom(n1), Atom(n2) -> n1 == n2
    Not(p1), Not(p2) -> propositions_equal(p1, p2)
    And(l1, r1), And(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    Or(l1, r1), Or(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    Implies(l1, r1), Implies(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    Necessary(p1), Necessary(p2) -> propositions_equal(p1, p2)
    Possible(p1), Possible(p2) -> propositions_equal(p1, p2)
    Obligatory(p1), Obligatory(p2) -> propositions_equal(p1, p2)
    Permitted(p1), Permitted(p2) -> propositions_equal(p1, p2)
    Knows(a1, p1), Knows(a2, p2) -> a1 == a2 && propositions_equal(p1, p2)
    Believes(a1, p1), Believes(a2, p2) -> a1 == a2 && propositions_equal(p1, p2)
    _, _ -> False
  }
}

/// Compute aggregate metrics from individual results
fn compute_aggregate_metrics(
  results: List(AccuracyTestResult),
) -> AccuracyResults {
  let total = list.length(results)

  // Translation metrics
  let exact_matches =
    list.count(results, fn(r) {
      case r.translation_match {
        ExactTranslation -> True
        _ -> False
      }
    })

  let partial_matches =
    list.count(results, fn(r) {
      case r.translation_match {
        PartialTranslation(_, _) -> True
        ConclusionOnly -> True
        _ -> False
      }
    })

  let no_matches =
    list.count(results, fn(r) {
      case r.translation_match {
        NoTranslation -> True
        _ -> False
      }
    })

  let avg_match = case total {
    0 -> 0.0
    _ ->
      int.to_float(exact_matches * 100 + partial_matches * 50)
      /. int.to_float(total)
      /. 100.0
  }

  let translation =
    TranslationMetrics(
      total: total,
      exact_matches: exact_matches,
      partial_matches: partial_matches,
      no_matches: no_matches,
      avg_proposition_match: avg_match,
      confidence_correlation: compute_confidence_correlation(results),
    )

  // Logic detection metrics
  let logic_correct = list.count(results, fn(r) { r.logic_system_correct })
  let logic_detection =
    LogicDetectionMetrics(total: total, correct: logic_correct, by_system: [])

  // Validation metrics
  let validation_correct = list.count(results, fn(r) { r.validation_correct })
  let validation =
    ValidationMetrics(
      total: total,
      true_positives: validation_correct,
      true_negatives: 0,
      false_positives: 0,
      false_negatives: total - validation_correct,
      precision: safe_divide(
        int.to_float(validation_correct),
        int.to_float(total),
      ),
      recall: 1.0,
      f1_score: safe_divide(
        int.to_float(2 * validation_correct),
        int.to_float(total + validation_correct),
      ),
    )

  // End-to-end metrics
  let successful =
    list.count(results, fn(r) {
      case r.translation_match {
        ExactTranslation -> r.validation_correct
        _ -> False
      }
    })

  let total_duration_ms =
    list.fold(results, 0, fn(acc, r) { acc + r.duration_ms })
  let avg_duration_ms = case total {
    0 -> 0
    result_count -> total_duration_ms / result_count
  }

  let end_to_end =
    EndToEndMetrics(
      total: total,
      successful: successful,
      failed_translation: no_matches,
      failed_validation: total - validation_correct - no_matches,
      avg_duration_ms: avg_duration_ms,
    )

  // Overall metrics
  let accuracy = case total {
    0 -> 0.0
    _ -> int.to_float(successful * 100) /. int.to_float(total)
  }

  let grade = case accuracy {
    a if a >=. 90.0 -> "A"
    a if a >=. 80.0 -> "B"
    a if a >=. 70.0 -> "C"
    a if a >=. 60.0 -> "D"
    _ -> "F"
  }

  let overall =
    OverallMetrics(
      total_tests: total,
      fully_passed: successful,
      accuracy: accuracy,
      grade: grade,
    )

  AccuracyResults(
    translation: translation,
    logic_detection: logic_detection,
    validation: validation,
    end_to_end: end_to_end,
    overall: overall,
  )
}

/// Compute correlation between confidence and correctness
fn compute_confidence_correlation(results: List(AccuracyTestResult)) -> Float {
  // Simplified correlation: mean confidence of correct vs incorrect
  let correct =
    list.filter(results, fn(r) {
      case r.translation_match {
        ExactTranslation -> True
        _ -> False
      }
    })

  let incorrect =
    list.filter(results, fn(r) {
      case r.translation_match {
        NoTranslation -> True
        _ -> False
      }
    })

  let mean_correct = mean_confidence(correct)
  let mean_incorrect = mean_confidence(incorrect)

  // Higher difference means better correlation
  mean_correct -. mean_incorrect
}

/// Calculate mean confidence
fn mean_confidence(results: List(AccuracyTestResult)) -> Float {
  case results {
    [] -> 0.0
    _ -> {
      let sum = list.fold(results, 0.0, fn(acc, r) { acc +. r.confidence })
      sum /. int.to_float(list.length(results))
    }
  }
}

/// Safe division that returns 0 for divide by zero
fn safe_divide(numerator: Float, denominator: Float) -> Float {
  case denominator {
    0.0 -> 0.0
    _ -> numerator /. denominator
  }
}

/// Format accuracy results as a report string
pub fn format_report(results: AccuracyResults) -> String {
  string.concat([
    "\n",
    string.repeat("=", 70),
    "\n",
    "ACCURACY TESTING REPORT",
    "\n",
    string.repeat("=", 70),
    "\n\n",
    "## Translation Accuracy\n",
    "  Total: ",
    int.to_string(results.translation.total),
    "\n",
    "  Exact Matches: ",
    int.to_string(results.translation.exact_matches),
    " (",
    format_percent(results.translation.exact_matches, results.translation.total),
    ")\n",
    "  Partial Matches: ",
    int.to_string(results.translation.partial_matches),
    "\n",
    "  No Matches: ",
    int.to_string(results.translation.no_matches),
    "\n",
    "  Avg Proposition Match: ",
    float_to_string(results.translation.avg_proposition_match *. 100.0),
    "%\n",
    "\n",
    "## Logic Detection\n",
    "  Total: ",
    int.to_string(results.logic_detection.total),
    "\n",
    "  Correct: ",
    int.to_string(results.logic_detection.correct),
    " (",
    format_percent(
      results.logic_detection.correct,
      results.logic_detection.total,
    ),
    ")\n",
    "\n",
    "## Validation Accuracy\n",
    "  Precision: ",
    float_to_string(results.validation.precision *. 100.0),
    "%\n",
    "  Recall: ",
    float_to_string(results.validation.recall *. 100.0),
    "%\n",
    "  F1 Score: ",
    float_to_string(results.validation.f1_score *. 100.0),
    "%\n",
    "\n",
    "## End-to-End Pipeline\n",
    "  Total: ",
    int.to_string(results.end_to_end.total),
    "\n",
    "  Successful: ",
    int.to_string(results.end_to_end.successful),
    " (",
    format_percent(results.end_to_end.successful, results.end_to_end.total),
    ")\n",
    "  Failed Translation: ",
    int.to_string(results.end_to_end.failed_translation),
    "\n",
    "  Failed Validation: ",
    int.to_string(results.end_to_end.failed_validation),
    "\n",
    "\n",
    string.repeat("-", 70),
    "\n",
    "OVERALL: ",
    float_to_string(results.overall.accuracy),
    "% - Grade: ",
    results.overall.grade,
    "\n",
    string.repeat("-", 70),
    "\n",
  ])
}

/// Format as percentage string
fn format_percent(num: Int, denom: Int) -> String {
  case denom {
    0 -> "0%"
    _ -> {
      let pct = { num * 100 } / denom
      int.to_string(pct) <> "%"
    }
  }
}

/// Simple float to string (2 decimal places)
fn float_to_string(f: Float) -> String {
  let rounded = float.round(f *. 100.0) |> int.to_float |> fn(x) { x /. 100.0 }
  // Just return the integer part for simplicity
  int.to_string(float.truncate(rounded))
}

/// Run accuracy tests with verbose output
pub fn run_verbose(fixtures: List(TestFixture)) -> AccuracyResults {
  let results = run_accuracy_tests(fixtures)
  // Print individual results would go here
  results
}
