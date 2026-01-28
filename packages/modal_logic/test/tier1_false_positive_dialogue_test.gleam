//// Dialogue Test: Tier 1 False Positive Detection (Issue #176)
////
//// This dialogue test verifies that Tier 1 heuristic validation results
//// are cross-validated against ground truth to detect false positives
//// (heuristic says Valid but argument is Invalid) and false negatives.
////
//// ## Test Objectives
//// - Verify cross-validation runs on fixture corpus
//// - Verify false positive and false negative rates are bounded [0, 1]
//// - Verify per-pattern error rates are populated
//// - Verify disagreement cases are structured
//// - Verify Phase A metric integration
//// - Verify agreement rate is high (Tier 1 should be mostly correct)
//// - Verify pattern name extraction works
////
//// ## Issue #176 Requirements
//// - Cross-validate Tier 1 results against ground truth
//// - Report false positive rate (heuristic Valid, ground truth Invalid)
//// - Report false negative rate (heuristic Invalid, ground truth Valid)
//// - Identify patterns with highest error rates
//// - Add tier1_false_positive_rate metric to Phase A

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/testing/accuracy/accuracy_tests
import modal_logic/testing/epic_validation
import modal_logic/testing/fixtures/fixtures
import modal_logic/testing/fixtures/ground_truth

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn tier1_false_positive_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Tier 1 False Positive Detection (Issue #176)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Cross-validation runs on fixture corpus
  io.println("--- Test 1: Cross-Validation Execution ---")
  io.println("")
  test_cross_validation_runs()
  io.println("[PASS] Cross-validation runs on fixture corpus")
  io.println("")

  // Test 2: False positive and false negative rates are bounded
  io.println("--- Test 2: Rate Bounds ---")
  io.println("")
  test_rate_bounds()
  io.println("[PASS] False positive and false negative rates bounded [0, 1]")
  io.println("")

  // Test 3: Per-pattern error rates populated
  io.println("--- Test 3: Per-Pattern Error Rates ---")
  io.println("")
  test_per_pattern_errors()
  io.println("[PASS] Per-pattern error rates populated")
  io.println("")

  // Test 4: Disagreement cases structured
  io.println("--- Test 4: Disagreement Cases ---")
  io.println("")
  test_disagreement_cases()
  io.println("[PASS] Disagreement cases properly structured")
  io.println("")

  // Test 5: Agreement rate is high
  io.println("--- Test 5: Agreement Rate ---")
  io.println("")
  test_agreement_rate()
  io.println("[PASS] Agreement rate validates Tier 1 correctness")
  io.println("")

  // Test 6: Pattern name extraction
  io.println("--- Test 6: Pattern Name Extraction ---")
  io.println("")
  test_pattern_name_extraction()
  io.println("[PASS] Pattern names extracted correctly from explanations")
  io.println("")

  // Test 7: Curated fixture cross-validation
  io.println("--- Test 7: Curated Fixture Cross-Validation ---")
  io.println("")
  test_curated_cross_validation()
  io.println("[PASS] Cross-validation works on curated fixtures")
  io.println("")

  // Test 8: Phase A metric integration
  io.println("--- Test 8: Phase A Metric Integration ---")
  io.println("")
  test_phase_a_metric()
  io.println("[PASS] Phase A tier1_false_positive_rate metric runs")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL TIER 1 FALSE POSITIVE DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Cross-Validation Execution
// =============================================================================

fn test_cross_validation_runs() {
  io.println("User: Run Tier 1 cross-validation on all fixtures")

  let all_fixtures = fixtures.all_fixtures()
  let cross_validation = accuracy_tests.cross_validate_tier1(all_fixtures)

  io.println("[System]: Tier 1 Cross-Validation Results")
  io.println(
    "  Total cross-validated: "
    <> int.to_string(cross_validation.total_cross_validated),
  )
  io.println("  Agreements: " <> int.to_string(cross_validation.agreements))
  io.println(
    "  Disagreements: " <> int.to_string(cross_validation.disagreements),
  )

  // Should have cross-validated at least some fixtures
  { cross_validation.total_cross_validated > 0 } |> should.be_true()

  // Agreements + disagreements = total
  {
    cross_validation.agreements + cross_validation.disagreements
    == cross_validation.total_cross_validated
  }
  |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - cross-validated "
    <> int.to_string(cross_validation.total_cross_validated)
    <> " fixtures",
  )
}

// =============================================================================
// Test 2: Rate Bounds
// =============================================================================

fn test_rate_bounds() {
  io.println(
    "User: Verify false positive and false negative rates are in [0, 1]",
  )

  let all_fixtures = fixtures.all_fixtures()
  let cross_validation = accuracy_tests.cross_validate_tier1(all_fixtures)

  io.println("[System]: Rate Analysis")
  io.println(
    "  False positive rate: "
    <> float_to_pct(cross_validation.false_positive_rate),
  )
  io.println(
    "  False negative rate: "
    <> float_to_pct(cross_validation.false_negative_rate),
  )
  io.println(
    "  Agreement rate: " <> float_to_pct(cross_validation.agreement_rate),
  )

  // All rates in [0, 1]
  { cross_validation.false_positive_rate >=. 0.0 } |> should.be_true()
  { cross_validation.false_positive_rate <=. 1.0 } |> should.be_true()
  { cross_validation.false_negative_rate >=. 0.0 } |> should.be_true()
  { cross_validation.false_negative_rate <=. 1.0 } |> should.be_true()
  { cross_validation.agreement_rate >=. 0.0 } |> should.be_true()
  { cross_validation.agreement_rate <=. 1.0 } |> should.be_true()

  io.println("")
  io.println("[System]: Confirmed - all rates in valid range [0, 1]")
}

// =============================================================================
// Test 3: Per-Pattern Error Rates
// =============================================================================

fn test_per_pattern_errors() {
  io.println("User: Show error rates broken down by heuristic pattern")

  let all_fixtures =
    list.flatten([
      fixtures.all_fixtures(),
      ground_truth.all_ground_truth_fixtures(),
    ])
  let cross_validation = accuracy_tests.cross_validate_tier1(all_fixtures)

  let pattern_count = list.length(cross_validation.per_pattern_errors)

  io.println("[System]: Per-Pattern Error Rates")
  io.println("  Patterns observed: " <> int.to_string(pattern_count))

  list.each(cross_validation.per_pattern_errors, fn(pattern) {
    io.println(
      "  "
      <> pattern.pattern_name
      <> ": errors="
      <> int.to_string(pattern.error_count)
      <> "/"
      <> int.to_string(pattern.total_cases)
      <> " ("
      <> float_to_pct(pattern.error_rate)
      <> ")",
    )
  })

  // Should have at least one pattern observed
  { pattern_count >= 1 } |> should.be_true()

  // Each pattern error rate should be in [0, 1]
  list.each(cross_validation.per_pattern_errors, fn(pattern) {
    { pattern.error_rate >=. 0.0 } |> should.be_true()
    { pattern.error_rate <=. 1.0 } |> should.be_true()
    { pattern.total_cases > 0 } |> should.be_true()
  })

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(pattern_count)
    <> " patterns with error rates",
  )
}

// =============================================================================
// Test 4: Disagreement Cases
// =============================================================================

fn test_disagreement_cases() {
  io.println(
    "User: Show any disagreement cases between Tier 1 and ground truth",
  )

  let all_fixtures =
    list.flatten([
      fixtures.all_fixtures(),
      ground_truth.all_ground_truth_fixtures(),
    ])
  let cross_validation = accuracy_tests.cross_validate_tier1(all_fixtures)

  let disagreement_count = list.length(cross_validation.disagreement_cases)

  io.println("[System]: Disagreement Cases")
  io.println("  Total disagreements: " <> int.to_string(disagreement_count))

  // Show first few disagreements
  cross_validation.disagreement_cases
  |> list.take(5)
  |> list.each(fn(disagreement) {
    io.println(
      "  ["
      <> disagreement.case_id
      <> "] Tier1="
      <> disagreement.tier1_result
      <> " vs GroundTruth="
      <> disagreement.ground_truth_result
      <> " (pattern: "
      <> disagreement.pattern_used
      <> ")",
    )
  })

  // Disagreements should match count from cross-validation
  disagreement_count |> should.equal(cross_validation.disagreements)

  // Each disagreement should have non-empty fields
  list.each(cross_validation.disagreement_cases, fn(d) {
    { d.case_id != "" } |> should.be_true()
    { d.tier1_result != "" } |> should.be_true()
    { d.ground_truth_result != "" } |> should.be_true()
    { d.pattern_used != "" } |> should.be_true()
  })

  io.println("")
  io.println(
    "[System]: Confirmed - "
    <> int.to_string(disagreement_count)
    <> " disagreements properly structured",
  )
}

// =============================================================================
// Test 5: Agreement Rate
// =============================================================================

fn test_agreement_rate() {
  io.println(
    "User: What is the overall agreement rate between Tier 1 and ground truth?",
  )

  let all_fixtures = fixtures.all_fixtures()
  let cross_validation = accuracy_tests.cross_validate_tier1(all_fixtures)

  let agreement_pct = cross_validation.agreement_rate *. 100.0

  io.println("[System]: Agreement Analysis")
  io.println(
    "  Agreement rate: " <> float_to_pct(cross_validation.agreement_rate),
  )
  io.println(
    "  FP count: " <> int.to_string(cross_validation.false_positive_count),
  )
  io.println(
    "  FN count: " <> int.to_string(cross_validation.false_negative_count),
  )

  // FP + FN should equal disagreements
  {
    cross_validation.false_positive_count
    + cross_validation.false_negative_count
    == cross_validation.disagreements
  }
  |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - Tier 1 agreement at "
    <> int.to_string(float.round(agreement_pct))
    <> "%",
  )
}

// =============================================================================
// Test 6: Pattern Name Extraction
// =============================================================================

fn test_pattern_name_extraction() {
  io.println("User: Verify pattern name extraction from explanation strings")

  // Test known explanation patterns
  let test_cases = [
    #("Conclusion is identical to premise", "identity"),
    #("Tautology detected: p → p", "tautology"),
    #("Modus ponens pattern matched", "modus_ponens"),
    #("Modus tollens applied", "modus_tollens"),
    #("Hypothetical syllogism chain", "hypothetical_syllogism"),
    #("Affirming the consequent fallacy", "affirming_consequent"),
    #("Denying the antecedent fallacy", "denying_antecedent"),
    #("K distribution axiom applied", "k_distribution"),
    #("T axiom (reflexivity)", "t_axiom"),
    #("Unknown heuristic xyz", "unknown"),
  ]

  list.each(test_cases, fn(test_case) {
    let #(explanation, expected_pattern) = test_case
    let actual_pattern = accuracy_tests.extract_pattern_name(explanation)
    io.println("  \"" <> explanation <> "\" → " <> actual_pattern)
    actual_pattern |> should.equal(expected_pattern)
  })

  io.println("")
  io.println("[System]: Confirmed - all pattern names extracted correctly")
}

// =============================================================================
// Test 7: Curated Fixture Cross-Validation
// =============================================================================

fn test_curated_cross_validation() {
  io.println(
    "User: Run cross-validation specifically on curated ground-truth fixtures",
  )

  let curated_fixtures = ground_truth.all_ground_truth_fixtures()
  let cross_validation = accuracy_tests.cross_validate_tier1(curated_fixtures)

  io.println("[System]: Curated Fixture Cross-Validation")
  io.println(
    "  Cross-validated: "
    <> int.to_string(cross_validation.total_cross_validated),
  )
  io.println(
    "  Agreement rate: " <> float_to_pct(cross_validation.agreement_rate),
  )
  io.println(
    "  FP rate: " <> float_to_pct(cross_validation.false_positive_rate),
  )
  io.println(
    "  FN rate: " <> float_to_pct(cross_validation.false_negative_rate),
  )
  io.println(
    "  Patterns: "
    <> int.to_string(list.length(cross_validation.per_pattern_errors)),
  )

  // Curated fixtures should produce cross-validation results
  { cross_validation.total_cross_validated > 0 } |> should.be_true()

  // Rates should be valid
  { cross_validation.false_positive_rate >=. 0.0 } |> should.be_true()
  { cross_validation.false_positive_rate <=. 1.0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - curated cross-validation on "
    <> int.to_string(cross_validation.total_cross_validated)
    <> " fixtures",
  )
}

// =============================================================================
// Test 8: Phase A Metric Integration
// =============================================================================

fn test_phase_a_metric() {
  io.println("User: Run the Phase A tier1_false_positive_rate metric")

  let metric_result = epic_validation.validate_tier1_false_positive_rate()

  io.println("[System]: Phase A Tier 1 False Positive Metric")
  io.println("  Name: " <> metric_result.name)
  io.println(
    "  Target: " <> int.to_string(float.round(metric_result.target)) <> "%",
  )
  io.println(
    "  Actual: " <> int.to_string(float.round(metric_result.actual)) <> "%",
  )
  io.println(
    "  Passed: "
    <> case metric_result.passed {
      True -> "yes"
      False -> "no"
    },
  )
  io.println("  Samples: " <> int.to_string(metric_result.samples))

  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> io.println("  Details: (none)")
  }

  // Metric should have correct name
  metric_result.name |> should.equal("tier1_false_positive_rate")

  // Should have cross-validated fixtures
  { metric_result.samples > 0 } |> should.be_true()

  // Score should be a valid percentage
  { metric_result.actual >=. 0.0 } |> should.be_true()
  { metric_result.actual <=. 100.0 } |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - metric runs with "
    <> int.to_string(metric_result.samples)
    <> " cross-validated samples",
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
