//// Dialogue Test: Phase E Probabilistic Logic Metrics (Issue #166)
////
//// This dialogue test verifies that probabilistic logic validation metrics
//// are properly connected to the epic validation infrastructure.
////
//// ## Test Objectives
//// - Verify probabilistic content detection works correctly
//// - Verify constraint extraction accuracy meets 95%+ target
//// - Verify chain rule accuracy meets 90%+ target
//// - Verify probability bound accuracy meets 95%+ target
//// - Verify conditional probability handling meets 90%+ target
//// - Verify Phase E shows as PASS in epic validation
////
//// ## Issue #166 Requirements
//// Phase E metrics must connect probabilistic logic validation:
//// - probabilistic_content_detection: 100% detection rate
//// - constraint_extraction_accuracy: 95%+
//// - chain_rule_accuracy: 90%+
//// - probability_bound_accuracy: 95%+
//// - conditional_probability_handling: 90%+

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/probabilistic
import modal_logic/proposition.{
  Atom, CondProb, Implies, Necessary, Possible, ProbAtLeast, ProbAtMost,
  ProbRange, Probable,
}
import modal_logic/testing/epic_validation

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn probabilistic_metrics_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Phase E Probabilistic Logic Metrics (Issue #166)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Probabilistic Content Detection
  io.println("--- Test 1: Probabilistic Content Detection ---")
  io.println("")
  test_probabilistic_content_detection()
  io.println("[PASS] Probabilistic content detection verified")
  io.println("")

  // Test 2: Constraint Extraction
  io.println("--- Test 2: Constraint Extraction Accuracy ---")
  io.println("")
  test_constraint_extraction()
  io.println("[PASS] Constraint extraction accuracy verified")
  io.println("")

  // Test 3: Chain Rule Accuracy
  io.println("--- Test 3: Chain Rule Accuracy ---")
  io.println("")
  test_chain_rule_accuracy()
  io.println("[PASS] Chain rule accuracy verified")
  io.println("")

  // Test 4: Probability Bound Accuracy
  io.println("--- Test 4: Probability Bound Accuracy ---")
  io.println("")
  test_probability_bound_accuracy()
  io.println("[PASS] Probability bound accuracy verified")
  io.println("")

  // Test 5: Conditional Probability Handling
  io.println("--- Test 5: Conditional Probability Handling ---")
  io.println("")
  test_conditional_probability_handling()
  io.println("[PASS] Conditional probability handling verified")
  io.println("")

  // Test 6: Full Phase E Validation
  io.println("--- Test 6: Full Phase E Validation ---")
  io.println("")
  test_phase_e_validation()
  io.println("[PASS] Phase E validation complete")
  io.println("")

  // Test 7: Probabilistic Validation Examples
  io.println("--- Test 7: Probabilistic Validation Examples ---")
  io.println("")
  test_probabilistic_validation_examples()
  io.println("[PASS] Probabilistic validation examples verified")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL PROBABILISTIC METRICS DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Probabilistic Content Detection
// =============================================================================

fn test_probabilistic_content_detection() {
  io.println(
    "User: Validate probabilistic content detection with 50 test cases",
  )

  let metric_result = epic_validation.validate_probabilistic_detection(50)

  io.println("[System]: Probabilistic Content Detection Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Test specific detection cases
  io.println("User: Test specific probabilistic detection cases")

  let p = Atom("p")
  let q = Atom("q")

  // Probabilistic cases
  let prob_case1 = probabilistic.has_probabilistic_content([], Probable(p))
  let prob_case2 =
    probabilistic.has_probabilistic_content([ProbAtLeast(p, 0.7)], q)
  let prob_case3 =
    probabilistic.has_probabilistic_content([CondProb(p, q, 0.8)], Probable(p))

  // Non-probabilistic cases
  let non_prob_case1 =
    probabilistic.has_probabilistic_content([Necessary(p)], Possible(p))
  let non_prob_case2 =
    probabilistic.has_probabilistic_content([Implies(p, q), p], q)

  io.println("[System]: Detection test results:")
  io.println(
    "  Probable(p) conclusion: "
    <> bool_to_string(prob_case1)
    <> " (expected: true)",
  )
  io.println(
    "  ProbAtLeast premise: "
    <> bool_to_string(prob_case2)
    <> " (expected: true)",
  )
  io.println(
    "  CondProb premise: " <> bool_to_string(prob_case3) <> " (expected: true)",
  )
  io.println(
    "  Modal only: " <> bool_to_string(non_prob_case1) <> " (expected: false)",
  )
  io.println(
    "  Propositional only: "
    <> bool_to_string(non_prob_case2)
    <> " (expected: false)",
  )

  // Verify detection
  prob_case1 |> should.be_true()
  prob_case2 |> should.be_true()
  prob_case3 |> should.be_true()
  non_prob_case1 |> should.be_false()
  non_prob_case2 |> should.be_false()

  io.println("[System]: All detection cases correct")
}

// =============================================================================
// Test 2: Constraint Extraction Accuracy
// =============================================================================

fn test_constraint_extraction() {
  io.println("User: Validate constraint extraction accuracy with 50 test cases")

  let metric_result =
    epic_validation.validate_probabilistic_constraint_extraction(50)

  io.println("[System]: Constraint Extraction Accuracy Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Verify meets 95% target
  io.println("User: Verify constraint extraction is at least 95%")
  { metric_result.actual >=. 95.0 } |> should.be_true()
  io.println(
    "[System]: Confirmed - Constraint extraction of "
    <> float_to_string(metric_result.actual)
    <> "% meets 95% target",
  )
}

// =============================================================================
// Test 3: Chain Rule Accuracy
// =============================================================================

fn test_chain_rule_accuracy() {
  io.println("User: Validate chain rule accuracy with 50 test cases")

  let metric_result = epic_validation.validate_chain_rule_accuracy(50)

  io.println("[System]: Chain Rule Accuracy Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Test specific chain rule case
  io.println("User: Test chain rule: P(A|B) * P(B) bounds P(A)")
  let market_up = Atom("market_up")
  let stock_up = Atom("stock_up")

  // P(market_up) >= 0.7, P(stock_up|market_up) = 0.9
  // Therefore P(stock_up) >= 0.63 > 0.5, so Probable(stock_up) holds
  let premises = [
    ProbAtLeast(market_up, 0.7),
    CondProb(stock_up, market_up, 0.9),
  ]
  let conclusion = Probable(stock_up)
  let result = probabilistic.validate_probabilistic(premises, conclusion)

  io.println("[System]: Chain rule test:")
  io.println("  Premises: P(market_up) >= 0.7, P(stock_up|market_up) = 0.9")
  io.println("  Conclusion: Probable(stock_up)")
  io.println("  Expected: Valid (0.7 * 0.9 = 0.63 > 0.5)")
  io.println("  Result: " <> bool_to_string(result.valid))
  io.println(
    "  Bounds: "
    <> float_to_string(result.bounds.lower)
    <> " <= P <= "
    <> float_to_string(result.bounds.upper),
  )

  result.valid |> should.be_true()

  // Verify meets 90% target
  io.println("")
  io.println("User: Verify chain rule accuracy is at least 90%")
  { metric_result.actual >=. 90.0 } |> should.be_true()
  io.println(
    "[System]: Confirmed - Chain rule accuracy of "
    <> float_to_string(metric_result.actual)
    <> "% meets 90% target",
  )
}

// =============================================================================
// Test 4: Probability Bound Accuracy
// =============================================================================

fn test_probability_bound_accuracy() {
  io.println("User: Validate probability bound accuracy with 50 test cases")

  let metric_result = epic_validation.validate_probability_bound_accuracy(50)

  io.println("[System]: Probability Bound Accuracy Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Test bound computation
  io.println("User: Test bound computation for ProbAtLeast")
  let p = Atom("p")
  let result =
    probabilistic.validate_probabilistic([ProbAtLeast(p, 0.7)], Probable(p))

  io.println("[System]: Bound computation test:")
  io.println("  Premise: P(p) >= 0.7")
  io.println("  Expected bounds: [0.7, 1.0]")
  io.println(
    "  Computed bounds: ["
    <> float_to_string(result.bounds.lower)
    <> ", "
    <> float_to_string(result.bounds.upper)
    <> "]",
  )

  // Lower bound should be approximately 0.7
  { result.bounds.lower >=. 0.69 && result.bounds.lower <=. 0.71 }
  |> should.be_true()

  // Verify meets 95% target
  io.println("")
  io.println("User: Verify probability bound accuracy is at least 95%")
  { metric_result.actual >=. 95.0 } |> should.be_true()
  io.println(
    "[System]: Confirmed - Probability bound accuracy of "
    <> float_to_string(metric_result.actual)
    <> "% meets 95% target",
  )
}

// =============================================================================
// Test 5: Conditional Probability Handling
// =============================================================================

fn test_conditional_probability_handling() {
  io.println(
    "User: Validate conditional probability handling with 50 test cases",
  )

  let metric_result =
    epic_validation.validate_conditional_probability_handling(50)

  io.println("[System]: Conditional Probability Handling Results")
  io.println("  Metric: " <> metric_result.name)
  io.println("  Target: " <> float_to_string(metric_result.target) <> "%")
  io.println("  Actual: " <> float_to_string(metric_result.actual) <> "%")
  io.println("  Samples: " <> int.to_string(metric_result.samples))
  io.println(
    "  Status: "
    <> case metric_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  case metric_result.details {
    Some(details) -> io.println("  Details: " <> details)
    None -> Nil
  }
  io.println("")

  // Test conditional probability
  io.println("User: Test conditional probability: P(wet|rain) = 0.95")
  let rain = Atom("rain")
  let wet = Atom("wet")

  let premises = [ProbAtLeast(rain, 0.8), CondProb(wet, rain, 0.95)]
  let conclusion = Probable(wet)
  let result = probabilistic.validate_probabilistic(premises, conclusion)

  io.println("[System]: Conditional probability test:")
  io.println("  Premises: P(rain) >= 0.8, P(wet|rain) = 0.95")
  io.println("  Conclusion: Probable(wet)")
  io.println("  Expected: Valid (0.8 * 0.95 = 0.76 > 0.5)")
  io.println("  Result: " <> bool_to_string(result.valid))
  io.println("  Confidence: " <> float_to_string(result.confidence))

  result.valid |> should.be_true()

  // Verify meets 90% target
  io.println("")
  io.println("User: Verify conditional probability handling is at least 90%")
  { metric_result.actual >=. 90.0 } |> should.be_true()
  io.println(
    "[System]: Confirmed - Conditional probability handling of "
    <> float_to_string(metric_result.actual)
    <> "% meets 90% target",
  )
}

// =============================================================================
// Test 6: Full Phase E Validation
// =============================================================================

fn test_phase_e_validation() {
  io.println("User: Run full Phase E validation")

  let config = epic_validation.default_config()
  let phase_result =
    epic_validation.validate_phase(epic_validation.PhaseE, config)

  io.println("[System]: Phase E Validation Results")
  io.println("  Phase: " <> phase_result.name)
  io.println(
    "  Overall Status: "
    <> case phase_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  io.println("  Duration: " <> int.to_string(phase_result.duration_ms) <> "ms")
  io.println("")

  // Show individual metrics
  io.println("  Individual Metrics:")
  phase_result.metrics
  |> list.each(fn(m) {
    let status = case m.passed {
      True -> "[PASS]"
      False -> "[FAIL]"
    }
    io.println(
      "    "
      <> status
      <> " "
      <> m.name
      <> ": "
      <> float_to_string(m.actual)
      <> m.unit
      <> " (target: "
      <> float_to_string(m.target)
      <> m.unit
      <> ")",
    )
  })
  io.println("")

  // Count passing metrics
  let passing_count = list.count(phase_result.metrics, fn(m) { m.passed })
  let total_count = list.length(phase_result.metrics)

  io.println(
    "  Summary: "
    <> int.to_string(passing_count)
    <> "/"
    <> int.to_string(total_count)
    <> " metrics passing",
  )

  // Phase E should pass (all 5 metrics)
  io.println("")
  io.println("User: Verify Phase E is passing")
  phase_result.passed |> should.be_true()
  io.println(
    "[System]: Confirmed - Phase E passing with "
    <> int.to_string(passing_count)
    <> " metrics",
  )
}

// =============================================================================
// Test 7: Probabilistic Validation Examples
// =============================================================================

fn test_probabilistic_validation_examples() {
  io.println("User: Show probabilistic validation working examples")

  let p = Atom("p")
  let q = Atom("q")

  // Example 1: Direct probability implies Probable
  io.println("")
  io.println("[System]: Example 1: Direct high probability")
  io.println("  Premise: P(p) >= 0.8")
  io.println("  Conclusion: Probable(p)")
  let result1 =
    probabilistic.validate_probabilistic([ProbAtLeast(p, 0.8)], Probable(p))
  io.println(
    "  Valid: " <> bool_to_string(result1.valid) <> " (expected: true)",
  )
  result1.valid |> should.be_true()

  // Example 2: Low probability does not imply Probable
  io.println("")
  io.println("[System]: Example 2: Low probability")
  io.println("  Premise: P(p) >= 0.4")
  io.println("  Conclusion: Probable(p)")
  let result2 =
    probabilistic.validate_probabilistic([ProbAtLeast(p, 0.4)], Probable(p))
  io.println(
    "  Valid: " <> bool_to_string(result2.valid) <> " (expected: false)",
  )
  result2.valid |> should.be_false()

  // Example 3: Probability range
  io.println("")
  io.println("[System]: Example 3: Probability range")
  io.println("  Premise: 0.6 <= P(p) <= 0.8")
  io.println("  Conclusion: Probable(p)")
  let result3 =
    probabilistic.validate_probabilistic([ProbRange(p, 0.6, 0.8)], Probable(p))
  io.println(
    "  Valid: " <> bool_to_string(result3.valid) <> " (expected: true)",
  )
  result3.valid |> should.be_true()

  // Example 4: Upper bound constraint
  io.println("")
  io.println("[System]: Example 4: Upper bound constraint")
  io.println("  Premise: P(p) <= 0.3")
  io.println("  Conclusion: Probable(p)")
  let result4 =
    probabilistic.validate_probabilistic([ProbAtMost(p, 0.3)], Probable(p))
  io.println(
    "  Valid: " <> bool_to_string(result4.valid) <> " (expected: false)",
  )
  result4.valid |> should.be_false()

  // Example 5: Conditional chain
  io.println("")
  io.println("[System]: Example 5: Conditional probability chain")
  io.println("  Premises: P(p) >= 0.9, P(q|p) = 0.95")
  io.println("  Conclusion: Probable(q)")
  let result5 =
    probabilistic.validate_probabilistic(
      [ProbAtLeast(p, 0.9), CondProb(q, p, 0.95)],
      Probable(q),
    )
  io.println(
    "  Valid: "
    <> bool_to_string(result5.valid)
    <> " (expected: true, 0.9*0.95=0.855>0.5)",
  )
  result5.valid |> should.be_true()

  io.println("")
  io.println("[System]: All probabilistic validation examples verified")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let decimal = float_truncate({ f -. int_to_float(whole) } *. 100.0)
  int.to_string(whole)
  <> "."
  <> case decimal < 10 {
    True -> "0"
    False -> ""
  }
  <> int.to_string(int_abs(decimal))
}

fn float_truncate(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float_truncate(0.0 -. f)
    False -> {
      case f <. 1.0 {
        True -> 0
        False -> 1 + float_truncate(f -. 1.0)
      }
    }
  }
}

fn int_to_float(n: Int) -> Float {
  case n {
    0 -> 0.0
    _ -> 1.0 +. int_to_float(n - 1)
  }
}

fn int_abs(n: Int) -> Int {
  case n < 0 {
    True -> 0 - n
    False -> n
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
