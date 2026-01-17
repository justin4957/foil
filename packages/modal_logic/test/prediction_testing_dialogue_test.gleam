//// Prediction Testing Dialogue Test
////
//// This test demonstrates the back-and-forth interaction with the
//// prediction testing framework, validating that predicates and logical
//// conclusions can be automatically tested.
////
//// Test coverage:
//// - Predicate set creation and relationships
//// - Test case builder pattern
//// - Suite execution with validator integration
//// - Result formatting and analysis

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import modal_logic/prediction_testing.{
  type PredictionTestCase, type PredictionTestResult, type PredictionTestSuite,
  Abductive, CausalInference, Critical, Deductive, ExpectInvalid, ExpectValid,
  High, Inductive, Normal, ProbabilisticPrediction, TemporalPrediction,
}
import modal_logic/proposition.{
  And, Atom, Implies, Necessary, Not, Or, Possible, S5,
}
import modal_logic/validator

// =============================================================================
// Main Test
// =============================================================================

pub fn prediction_testing_dialogue_test() {
  io.println("")
  io.println(string.repeat("=", 70))
  io.println("PREDICTION TESTING DIALOGUE TEST")
  io.println(string.repeat("=", 70))
  io.println("")

  // Run all dialogue tests
  test_predicate_set_creation()
  test_builder_pattern()
  test_deductive_reasoning()
  test_invalid_arguments()
  test_modal_predictions()
  test_suite_execution()
  test_formatting()

  io.println("")
  io.println(string.repeat("=", 70))
  io.println("ALL DIALOGUE TESTS PASSED")
  io.println(string.repeat("=", 70))
  io.println("")
}

// =============================================================================
// Test 1: Predicate Set Creation
// =============================================================================

fn test_predicate_set_creation() {
  io.println("--- Test 1: Predicate Set Creation Dialogue ---")
  io.println("")

  io.println("User: Create a predicate set for weather domain")
  let weather_predicates =
    prediction_testing.predicate_set("weather")
    |> prediction_testing.add_predicate("sunny", "The weather is sunny")
    |> prediction_testing.add_predicate("rainy", "It is raining")
    |> prediction_testing.add_predicate("cloudy", "The sky is cloudy")
    |> prediction_testing.add_relationship(
      prediction_testing.MutuallyExclusive(["sunny", "rainy"]),
    )
    |> prediction_testing.add_relationship(prediction_testing.ImpliesRelation(
      "rainy",
      "cloudy",
    ))

  io.println("[System]: Created predicate set 'weather' with:")
  io.println("  - 3 predicates: sunny, rainy, cloudy")
  io.println("  - Relationship: sunny and rainy are mutually exclusive")
  io.println("  - Relationship: rainy implies cloudy")
  io.println("")

  // Verify predicate set
  weather_predicates.name |> should.equal("weather")
  list.length(weather_predicates.relationships) |> should.equal(2)

  io.println("User: Create a predicate for financial domain")
  let finance_predicates =
    prediction_testing.predicate_set("finance")
    |> prediction_testing.add_predicate("bull_market", "Market is bullish")
    |> prediction_testing.add_predicate("high_volume", "Trading volume is high")
    |> prediction_testing.add_predicate("price_rise", "Prices are rising")
    |> prediction_testing.add_relationship(prediction_testing.StrongerThan(
      "bull_market",
      "price_rise",
    ))

  io.println("[System]: Created predicate set 'finance' with:")
  io.println("  - 3 predicates: bull_market, high_volume, price_rise")
  io.println("  - Relationship: bull_market is stronger than price_rise")
  io.println("")

  finance_predicates.name |> should.equal("finance")

  io.println("[PASS] Predicate set creation verified")
  io.println("")
}

// =============================================================================
// Test 2: Builder Pattern
// =============================================================================

fn test_builder_pattern() {
  io.println("--- Test 2: Builder Pattern Dialogue ---")
  io.println("")

  io.println("User: Build a prediction test case using fluent API")
  let test_case =
    prediction_testing.prediction_test_case("modus_ponens_01")
    |> prediction_testing.with_name("Modus Ponens Test")
    |> prediction_testing.with_description("Classic deductive argument form")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.case_domain("logic")
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Atom("P"))
    |> prediction_testing.with_conclusion(Atom("Q"))
    |> prediction_testing.expect_valid()
    |> prediction_testing.with_confidence_threshold(0.8)
    |> prediction_testing.with_tags(["classic", "syllogism"])
    |> prediction_testing.with_priority(High)

  io.println("[System]: Built test case 'modus_ponens_01':")
  io.println("  Name: Modus Ponens Test")
  io.println("  Category: Deductive")
  io.println("  Domain: logic")
  io.println("  Premises: (P -> Q), P")
  io.println("  Conclusion: Q")
  io.println("  Expected: Valid")
  io.println("  Confidence threshold: 0.8")
  io.println("  Tags: classic, syllogism")
  io.println("  Priority: High")
  io.println("")

  // Verify test case
  test_case.id |> should.equal("modus_ponens_01")
  test_case.name |> should.equal("Modus Ponens Test")
  list.length(test_case.premises) |> should.equal(2)
  list.length(test_case.tags) |> should.equal(2)

  io.println("[PASS] Builder pattern verified")
  io.println("")
}

// =============================================================================
// Test 3: Deductive Reasoning Tests
// =============================================================================

fn test_deductive_reasoning() {
  io.println("--- Test 3: Deductive Reasoning Dialogue ---")
  io.println("")

  let config = validator.default_config()

  io.println("User: Test modus ponens (P -> Q, P |- Q)")
  let modus_ponens =
    prediction_testing.prediction_test_case("deductive_01")
    |> prediction_testing.with_name("Modus Ponens")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Atom("P"))
    |> prediction_testing.with_conclusion(Atom("Q"))
    |> prediction_testing.expect_valid()

  let result = prediction_testing.run_prediction_test(modus_ponens, config)

  io.println("[System]: Running validation...")
  io.println("  Result: " <> validation_result_string(result))
  io.println("  Confidence: " <> float_to_string(result.confidence))
  io.println("  Tier: " <> result.tier_used)
  io.println("")

  result.passed |> should.be_true()

  io.println("User: Test hypothetical syllogism (P -> Q, Q -> R |- P -> R)")
  let hypothetical =
    prediction_testing.prediction_test_case("deductive_02")
    |> prediction_testing.with_name("Hypothetical Syllogism")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Implies(Atom("Q"), Atom("R")))
    |> prediction_testing.with_conclusion(Implies(Atom("P"), Atom("R")))
    |> prediction_testing.expect_valid()

  let result2 = prediction_testing.run_prediction_test(hypothetical, config)

  io.println("[System]: Running validation...")
  io.println("  Result: " <> validation_result_string(result2))
  io.println("  Confidence: " <> float_to_string(result2.confidence))
  io.println("")

  result2.passed |> should.be_true()

  io.println("User: Test disjunctive syllogism (P v Q, ~P |- Q)")
  let disjunctive =
    prediction_testing.prediction_test_case("deductive_03")
    |> prediction_testing.with_name("Disjunctive Syllogism")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_premise(Or(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Not(Atom("P")))
    |> prediction_testing.with_conclusion(Atom("Q"))
    |> prediction_testing.expect_valid()

  let result3 = prediction_testing.run_prediction_test(disjunctive, config)

  io.println("[System]: Running validation...")
  io.println("  Result: " <> validation_result_string(result3))
  io.println("")

  result3.passed |> should.be_true()

  io.println("[PASS] Deductive reasoning tests verified")
  io.println("")
}

// =============================================================================
// Test 4: Invalid Arguments
// =============================================================================

fn test_invalid_arguments() {
  io.println("--- Test 4: Invalid Arguments Dialogue ---")
  io.println("")

  let config = validator.default_config()

  io.println("User: Test affirming the consequent fallacy (P -> Q, Q |- P)")
  let affirming_consequent =
    prediction_testing.prediction_test_case("fallacy_01")
    |> prediction_testing.with_name("Affirming the Consequent")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Atom("Q"))
    |> prediction_testing.with_conclusion(Atom("P"))
    |> prediction_testing.expect_invalid(
      "Formal fallacy: affirming the consequent",
    )

  let result =
    prediction_testing.run_prediction_test(affirming_consequent, config)

  io.println("[System]: Running validation...")
  io.println("  Result: " <> validation_result_string(result))
  io.println("  Expected: Invalid")
  case result.countermodel {
    Some(cm) ->
      io.println("  Countermodel: " <> string.slice(cm, 0, 50) <> "...")
    None -> io.println("  Countermodel: (none)")
  }
  io.println("")

  // The test passes if the result is invalid as expected
  result.passed |> should.be_true()

  io.println("User: Test denying the antecedent fallacy (P -> Q, ~P |- ~Q)")
  let denying_antecedent =
    prediction_testing.prediction_test_case("fallacy_02")
    |> prediction_testing.with_name("Denying the Antecedent")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Not(Atom("P")))
    |> prediction_testing.with_conclusion(Not(Atom("Q")))
    |> prediction_testing.expect_invalid(
      "Formal fallacy: denying the antecedent",
    )

  let result2 =
    prediction_testing.run_prediction_test(denying_antecedent, config)

  io.println("[System]: Running validation...")
  io.println("  Result: " <> validation_result_string(result2))
  io.println("")

  result2.passed |> should.be_true()

  io.println("[PASS] Invalid argument tests verified")
  io.println("")
}

// =============================================================================
// Test 5: Modal Predictions
// =============================================================================

fn test_modal_predictions() {
  io.println("--- Test 5: Modal Predictions Dialogue ---")
  io.println("")

  let config = validator.default_config()

  // Note: Modal tests use expect_either because Z3 may not be available in CI
  io.println("User: Test S5 axiom: Necessarily P implies P")
  let s5_axiom =
    prediction_testing.prediction_test_case("modal_01")
    |> prediction_testing.with_name("Necessity implies Actuality")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_logic_system(S5)
    |> prediction_testing.with_premise(Necessary(Atom("P")))
    |> prediction_testing.with_conclusion(Atom("P"))
    |> prediction_testing.expect_either("Valid with Z3, Unknown without")

  let result = prediction_testing.run_prediction_test(s5_axiom, config)

  io.println("[System]: Running S5 modal validation...")
  io.println("  Result: " <> validation_result_string(result))
  io.println("  Logic System: S5")
  io.println("")

  // Modal tests may return Unknown if Z3 is unavailable, which is acceptable
  { result.passed || is_z3_unavailable(result) } |> should.be_true()

  io.println("User: Test modal K axiom: [](P -> Q) -> ([]P -> []Q)")
  let modal_k =
    prediction_testing.prediction_test_case("modal_02")
    |> prediction_testing.with_name("Modal K Distribution")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_logic_system(S5)
    |> prediction_testing.with_premise(Necessary(Implies(Atom("P"), Atom("Q"))))
    |> prediction_testing.with_premise(Necessary(Atom("P")))
    |> prediction_testing.with_conclusion(Necessary(Atom("Q")))
    |> prediction_testing.expect_either("Valid with Z3, Unknown without")

  let result2 = prediction_testing.run_prediction_test(modal_k, config)

  io.println("[System]: Running validation...")
  io.println("  Result: " <> validation_result_string(result2))
  io.println("")

  { result2.passed || is_z3_unavailable(result2) } |> should.be_true()

  io.println("User: Test possibility doesn't imply necessity")
  let possible_not_necessary =
    prediction_testing.prediction_test_case("modal_03")
    |> prediction_testing.with_name("Possibility doesn't imply necessity")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_logic_system(S5)
    |> prediction_testing.with_premise(Possible(Atom("P")))
    |> prediction_testing.with_conclusion(Necessary(Atom("P")))
    |> prediction_testing.expect_either("Invalid with Z3, Unknown without")

  let result3 =
    prediction_testing.run_prediction_test(possible_not_necessary, config)

  io.println("[System]: Running validation...")
  io.println("  Result: " <> validation_result_string(result3))
  io.println("")

  { result3.passed || is_z3_unavailable(result3) } |> should.be_true()

  io.println("[PASS] Modal prediction tests verified")
  io.println("")
}

// =============================================================================
// Test 6: Suite Execution
// =============================================================================

fn test_suite_execution() {
  io.println("--- Test 6: Suite Execution Dialogue ---")
  io.println("")

  let config = validator.default_config()

  io.println("User: Create and run a test suite")

  // Build test cases
  let test1 =
    prediction_testing.prediction_test_case("suite_01")
    |> prediction_testing.with_name("Modus Ponens")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_priority(Critical)
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Atom("P"))
    |> prediction_testing.with_conclusion(Atom("Q"))
    |> prediction_testing.expect_valid()

  let test2 =
    prediction_testing.prediction_test_case("suite_02")
    |> prediction_testing.with_name("Contraposition")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_priority(High)
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_conclusion(Implies(
      Not(Atom("Q")),
      Not(Atom("P")),
    ))
    |> prediction_testing.expect_valid()

  let test3 =
    prediction_testing.prediction_test_case("suite_03")
    |> prediction_testing.with_name("Invalid Fallacy")
    |> prediction_testing.with_category(Deductive)
    |> prediction_testing.with_priority(Normal)
    |> prediction_testing.with_premise(Implies(Atom("P"), Atom("Q")))
    |> prediction_testing.with_premise(Atom("Q"))
    |> prediction_testing.with_conclusion(Atom("P"))
    |> prediction_testing.expect_invalid("Affirming consequent")

  // Build suite
  let suite =
    prediction_testing.test_suite("Deduction Test Suite")
    |> prediction_testing.suite_description(
      "Tests basic deductive reasoning patterns",
    )
    |> prediction_testing.add_test(test1)
    |> prediction_testing.add_test(test2)
    |> prediction_testing.add_test(test3)
    |> prediction_testing.with_config(prediction_testing.fast_suite_config())

  io.println("[System]: Created suite 'Deduction Test Suite' with 3 tests")
  io.println("")

  io.println("User: Execute the test suite")
  let suite_result = prediction_testing.run_prediction_suite(suite, config)

  io.println("[System]: Suite execution complete")
  io.println("  Total tests: " <> int.to_string(suite_result.total_tests))
  io.println("  Passed: " <> int.to_string(suite_result.passed))
  io.println("  Failed: " <> int.to_string(suite_result.failed))
  io.println("  Pass rate: " <> float_to_string(suite_result.pass_rate) <> "%")
  io.println(
    "  Duration: " <> int.to_string(suite_result.total_duration_ms) <> "ms",
  )
  io.println("")

  // Verify suite results
  suite_result.total_tests |> should.equal(3)
  { suite_result.passed >= 2 } |> should.be_true()

  io.println("[PASS] Suite execution verified")
  io.println("")
}

// =============================================================================
// Test 7: Formatting
// =============================================================================

fn test_formatting() {
  io.println("--- Test 7: Formatting Dialogue ---")
  io.println("")

  let config = validator.default_config()

  io.println("User: Format a test result for display")

  let test_case =
    prediction_testing.prediction_test_case("format_01")
    |> prediction_testing.with_name("Formatting Test")
    |> prediction_testing.with_premise(Implies(Atom("A"), Atom("B")))
    |> prediction_testing.with_premise(Atom("A"))
    |> prediction_testing.with_conclusion(Atom("B"))
    |> prediction_testing.expect_valid()

  let result = prediction_testing.run_prediction_test(test_case, config)
  let formatted = prediction_testing.format_test_result(result)

  io.println("[System]: Formatted result:")
  io.println(formatted)
  io.println("")

  // Verify formatting contains expected elements
  { string.contains(formatted, "format_01") } |> should.be_true()
  { string.contains(formatted, "Formatting Test") } |> should.be_true()
  { string.contains(formatted, "Confidence") } |> should.be_true()

  io.println("[PASS] Formatting verified")
  io.println("")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn validation_result_string(result: PredictionTestResult) -> String {
  case result.passed {
    True -> "PASSED - " <> result.explanation
    False -> "FAILED - " <> result.explanation
  }
}

/// Check if the result indicates Z3 is unavailable (acceptable in CI)
fn is_z3_unavailable(result: PredictionTestResult) -> Bool {
  string.contains(result.explanation, "Z3 unavailable")
  || string.contains(result.explanation, "unknown")
}

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let frac = float_truncate({ f -. int.to_float(whole) } *. 100.0)
  int.to_string(whole) <> "." <> pad_left(int.to_string(frac), 2, "0")
}

fn pad_left(s: String, len: Int, char: String) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left(char <> s, len, char)
  }
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int
