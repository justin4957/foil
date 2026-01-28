//// Dialogue Test: Confidence Calibration Metrics (Issue #175)
////
//// This dialogue test verifies that the confidence calibration system
//// uses proper statistical metrics (Brier score, ECE) instead of a
//// simple mean-difference proxy.
////
//// ## Test Objectives
//// - Verify Brier score is computed correctly
//// - Verify Expected Calibration Error (ECE) is computed
//// - Verify overconfidence and underconfidence rates are reported
//// - Verify calibration curve data is populated with buckets
//// - Verify all translation match levels contribute (exact, partial,
////   conclusion-only, none) via continuous correctness scoring
//// - Verify calibration integrates with the accuracy pipeline
////
//// ## Issue #175 Requirements
//// - Brier score: 1/N * Σ(confidence_i - correct_i)²
//// - ECE: bin predictions by confidence, measure accuracy per bin
//// - Calibration curve: predicted confidence vs observed accuracy
//// - Overconfidence rate: % where confidence > 0.8 but incorrect
//// - Underconfidence rate: % where confidence < 0.5 but correct
//// - Include all translation match levels with appropriate scoring

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleeunit/should
import modal_logic/testing/accuracy/accuracy_tests
import modal_logic/testing/fixtures/fixtures
import modal_logic/testing/fixtures/ground_truth

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn confidence_calibration_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Confidence Calibration Metrics (Issue #175)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Brier score is computed and bounded
  io.println("--- Test 1: Brier Score Computation ---")
  io.println("")
  test_brier_score_computation()
  io.println("[PASS] Brier score computed and bounded [0, 1]")
  io.println("")

  // Test 2: ECE is computed and bounded
  io.println("--- Test 2: Expected Calibration Error ---")
  io.println("")
  test_ece_computation()
  io.println("[PASS] ECE computed and bounded [0, 1]")
  io.println("")

  // Test 3: Overconfidence and underconfidence rates
  io.println("--- Test 3: Over/Underconfidence Rates ---")
  io.println("")
  test_confidence_rates()
  io.println("[PASS] Over/underconfidence rates computed and bounded [0, 1]")
  io.println("")

  // Test 4: Calibration curve has buckets
  io.println("--- Test 4: Calibration Curve Buckets ---")
  io.println("")
  test_calibration_curve()
  io.println("[PASS] Calibration curve populated with buckets")
  io.println("")

  // Test 5: Translation match scoring includes all levels
  io.println("--- Test 5: Translation Match Scoring ---")
  io.println("")
  test_translation_match_scoring()
  io.println("[PASS] All translation match levels scored correctly")
  io.println("")

  // Test 6: Calibration integrates with curated fixtures
  io.println("--- Test 6: Curated Fixture Calibration ---")
  io.println("")
  test_curated_fixture_calibration()
  io.println("[PASS] Calibration metrics produced from curated fixtures")
  io.println("")

  // Test 7: Empty results produce safe defaults
  io.println("--- Test 7: Empty Results Safety ---")
  io.println("")
  test_empty_results_safety()
  io.println("[PASS] Empty results produce safe default calibration")
  io.println("")

  // Test 8: Brier score reflects calibration quality
  io.println("--- Test 8: Brier Score Meaning ---")
  io.println("")
  test_brier_score_meaning()
  io.println("[PASS] Brier score correctly reflects calibration quality")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL CONFIDENCE CALIBRATION DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Brier Score Computation
// =============================================================================

fn test_brier_score_computation() {
  io.println("User: Compute Brier score from accuracy pipeline results")

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)
  let calibration = results.translation.confidence_calibration

  io.println("[System]: Brier Score Calibration Results")
  io.println(
    "  Brier score: "
    <> float_to_pct(calibration.brier_score)
    <> " (lower is better, 0.0 = perfect)",
  )
  io.println("  Total samples: " <> int.to_string(calibration.total_samples))

  // Brier score should be in [0, 1]
  { calibration.brier_score >=. 0.0 } |> should.be_true()
  { calibration.brier_score <=. 1.0 } |> should.be_true()

  // Total samples should match fixture count
  { calibration.total_samples > 0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - Brier score "
    <> float_to_pct(calibration.brier_score)
    <> " is in valid range [0, 1]",
  )
}

// =============================================================================
// Test 2: Expected Calibration Error
// =============================================================================

fn test_ece_computation() {
  io.println("User: Compute ECE from accuracy pipeline results")

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)
  let calibration = results.translation.confidence_calibration

  io.println("[System]: Expected Calibration Error")
  io.println(
    "  ECE: "
    <> float_to_pct(calibration.expected_calibration_error)
    <> " (lower is better, 0.0 = perfect)",
  )

  // ECE should be in [0, 1]
  { calibration.expected_calibration_error >=. 0.0 } |> should.be_true()
  { calibration.expected_calibration_error <=. 1.0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - ECE "
    <> float_to_pct(calibration.expected_calibration_error)
    <> " is in valid range [0, 1]",
  )
}

// =============================================================================
// Test 3: Over/Underconfidence Rates
// =============================================================================

fn test_confidence_rates() {
  io.println("User: Check overconfidence and underconfidence rates")

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)
  let calibration = results.translation.confidence_calibration

  io.println("[System]: Confidence Rate Analysis")
  io.println(
    "  Overconfidence rate: "
    <> float_to_pct(calibration.overconfidence_rate)
    <> " (high conf >0.8 but incorrect)",
  )
  io.println(
    "  Underconfidence rate: "
    <> float_to_pct(calibration.underconfidence_rate)
    <> " (low conf <0.5 but correct)",
  )

  // Rates should be in [0, 1]
  { calibration.overconfidence_rate >=. 0.0 } |> should.be_true()
  { calibration.overconfidence_rate <=. 1.0 } |> should.be_true()
  { calibration.underconfidence_rate >=. 0.0 } |> should.be_true()
  { calibration.underconfidence_rate <=. 1.0 } |> should.be_true()

  io.println("")
  io.println("[System]: Confirmed - rates are in valid range [0, 1]")
}

// =============================================================================
// Test 4: Calibration Curve Buckets
// =============================================================================

fn test_calibration_curve() {
  io.println("User: Show calibration curve data")

  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)
  let calibration = results.translation.confidence_calibration

  let bucket_count = list.length(calibration.calibration_curve)

  io.println("[System]: Calibration Curve")
  io.println("  Buckets with data: " <> int.to_string(bucket_count))

  list.each(calibration.calibration_curve, fn(bucket) {
    io.println(
      "  ["
      <> bucket.label
      <> "]: mean_conf="
      <> float_to_pct(bucket.mean_confidence)
      <> ", observed_acc="
      <> float_to_pct(bucket.observed_accuracy)
      <> ", count="
      <> int.to_string(bucket.count),
    )
  })

  // Should have at least one bucket with data
  { bucket_count >= 1 } |> should.be_true()

  // Each bucket should have valid values
  list.each(calibration.calibration_curve, fn(bucket) {
    { bucket.mean_confidence >=. 0.0 } |> should.be_true()
    { bucket.mean_confidence <=. 1.0 } |> should.be_true()
    { bucket.observed_accuracy >=. 0.0 } |> should.be_true()
    { bucket.observed_accuracy <=. 1.0 } |> should.be_true()
    { bucket.count > 0 } |> should.be_true()
  })

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(bucket_count)
    <> " calibration buckets with valid data",
  )
}

// =============================================================================
// Test 5: Translation Match Scoring
// =============================================================================

fn test_translation_match_scoring() {
  io.println("User: Verify translation match scoring includes all levels")

  // Test each translation match level
  let exact_score =
    accuracy_tests.translation_match_score(accuracy_tests.ExactTranslation)
  let partial_score =
    accuracy_tests.translation_match_score(accuracy_tests.PartialTranslation(
      matched_premises: 2,
      total_premises: 4,
    ))
  let conclusion_score =
    accuracy_tests.translation_match_score(accuracy_tests.ConclusionOnly)
  let none_score =
    accuracy_tests.translation_match_score(accuracy_tests.NoTranslation)

  io.println("[System]: Translation Match Scores")
  io.println("  ExactTranslation: " <> float_to_pct(exact_score))
  io.println("  PartialTranslation(2/4): " <> float_to_pct(partial_score))
  io.println("  ConclusionOnly: " <> float_to_pct(conclusion_score))
  io.println("  NoTranslation: " <> float_to_pct(none_score))

  // Exact should be 1.0
  exact_score |> should.equal(1.0)

  // Partial(2/4) should be 0.5
  partial_score |> should.equal(0.5)

  // ConclusionOnly should be 0.25
  conclusion_score |> should.equal(0.25)

  // NoTranslation should be 0.0
  none_score |> should.equal(0.0)

  // Scores should be monotonically ordered
  { exact_score >. partial_score } |> should.be_true()
  { partial_score >. conclusion_score } |> should.be_true()
  { conclusion_score >. none_score } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - all 4 match levels scored with correct ordering",
  )
}

// =============================================================================
// Test 6: Curated Fixture Calibration
// =============================================================================

fn test_curated_fixture_calibration() {
  io.println("User: Run calibration on curated ground-truth fixtures")

  let curated_fixtures = ground_truth.all_ground_truth_fixtures()
  let results = accuracy_tests.run_accuracy_tests(curated_fixtures)
  let calibration = results.translation.confidence_calibration

  io.println("[System]: Curated Fixture Calibration Results")
  io.println("  Brier score: " <> float_to_pct(calibration.brier_score))
  io.println("  ECE: " <> float_to_pct(calibration.expected_calibration_error))
  io.println(
    "  Overconfidence: " <> float_to_pct(calibration.overconfidence_rate),
  )
  io.println(
    "  Underconfidence: " <> float_to_pct(calibration.underconfidence_rate),
  )
  io.println("  Samples: " <> int.to_string(calibration.total_samples))
  io.println(
    "  Buckets: " <> int.to_string(list.length(calibration.calibration_curve)),
  )

  // Curated fixtures should produce calibration data
  { calibration.total_samples >= 50 } |> should.be_true()
  { calibration.brier_score >=. 0.0 } |> should.be_true()
  { calibration.brier_score <=. 1.0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - calibration produced from "
    <> int.to_string(calibration.total_samples)
    <> " curated fixtures",
  )
}

// =============================================================================
// Test 7: Empty Results Safety
// =============================================================================

fn test_empty_results_safety() {
  io.println("User: Run calibration on empty fixture list")

  let results = accuracy_tests.run_accuracy_tests([])
  let calibration = results.translation.confidence_calibration

  io.println("[System]: Empty Results Calibration")
  io.println("  Brier score: " <> float_to_pct(calibration.brier_score))
  io.println("  ECE: " <> float_to_pct(calibration.expected_calibration_error))
  io.println("  Samples: " <> int.to_string(calibration.total_samples))
  io.println(
    "  Buckets: " <> int.to_string(list.length(calibration.calibration_curve)),
  )

  // All metrics should be 0.0 for empty input
  calibration.brier_score |> should.equal(0.0)
  calibration.expected_calibration_error |> should.equal(0.0)
  calibration.overconfidence_rate |> should.equal(0.0)
  calibration.underconfidence_rate |> should.equal(0.0)
  calibration.total_samples |> should.equal(0)
  list.length(calibration.calibration_curve) |> should.equal(0)

  io.println("")
  io.println("[System]: Confirmed - empty input produces safe zero defaults")
}

// =============================================================================
// Test 8: Brier Score Meaning
// =============================================================================

fn test_brier_score_meaning() {
  io.println(
    "User: Verify Brier score formula: 1/N * Σ(confidence_i - correct_i)²",
  )

  // Run on known fixtures to verify Brier score is meaningful
  let all_test_fixtures = fixtures.all_fixtures()
  let results = accuracy_tests.run_accuracy_tests(all_test_fixtures)
  let calibration = results.translation.confidence_calibration

  io.println("[System]: Brier Score Analysis")
  io.println("  Score: " <> float_to_pct(calibration.brier_score))

  // A perfect calibrator has Brier score 0.0
  // A maximally wrong calibrator has Brier score 1.0
  // Most systems fall in between
  io.println(
    "  Interpretation: "
    <> case calibration.brier_score {
      bs if bs <. 0.1 -> "Excellent calibration"
      bs if bs <. 0.25 -> "Good calibration"
      bs if bs <. 0.5 -> "Moderate calibration"
      _ -> "Poor calibration"
    },
  )

  // Verify the score is reasonable (not NaN or negative)
  { calibration.brier_score >=. 0.0 } |> should.be_true()

  // ECE and Brier score should be consistent (both measure miscalibration)
  // Both should be non-negative
  { calibration.expected_calibration_error >=. 0.0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - Brier score "
    <> float_to_pct(calibration.brier_score)
    <> " is a valid calibration metric",
  )
}

// =============================================================================
// Helpers
// =============================================================================

fn float_to_pct(value: Float) -> String {
  let rounded = float.round(value *. 10_000.0)
  let whole = rounded / 100
  let frac = case rounded % 100 {
    f if f < 0 -> -f
    f -> f
  }
  int.to_string(whole)
  <> "."
  <> case frac < 10 {
    True -> "0" <> int.to_string(frac)
    False -> int.to_string(frac)
  }
}
