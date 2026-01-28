//// Dialogue Test: Probabilistic Bounds Analytical Validation (Issue #178)
////
//// This dialogue test verifies that probabilistic logic bounds are
//// accurately computed against hand-calculated analytical solutions.
////
//// ## Test Objectives
//// - Near-boundary probability detection at 0.5 threshold
//// - Conflicting constraint handling (inconsistent bounds)
//// - Multi-step chain rule propagation with error accumulation
//// - Interval overlap narrowing via bounds intersection
//// - Numerical stability at probability extremes (near 0.0 and 1.0)
//// - Config consistency across fast/default/strict configs
//// - Tolerance verification against analytical values
//// - Phase E metric integration
////
//// ## Issue #178 Requirements
//// - Adversarial test cases with hand-calculated expected values
//// - Verify bound computation against analytical solutions
//// - Test numerical stability with P values near 0.0 and 1.0
//// - Verify fast_config() and strict_config() produce consistent results
//// - Report maximum observed error across test cases

import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/probabilistic
import modal_logic/proposition.{
  Atom, CondProb, ProbAtLeast, ProbAtMost, ProbRange, Probable,
}
import modal_logic/testing/epic_validation

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn probabilistic_bounds_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println(
    "DIALOGUE TEST: Probabilistic Bounds Analytical Validation (Issue #178)",
  )
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Near-Boundary Probabilities
  io.println("--- Test 1: Near-Boundary Probabilities ---")
  io.println("")
  test_near_boundary_probabilities()
  io.println("[PASS] Near-boundary probability detection verified")
  io.println("")

  // Test 2: Conflicting Constraints
  io.println("--- Test 2: Conflicting Constraints ---")
  io.println("")
  test_conflicting_constraints()
  io.println("[PASS] Conflicting constraint behavior documented")
  io.println("")

  // Test 3: Multi-Step Chain Propagation
  io.println("--- Test 3: Multi-Step Chain Propagation ---")
  io.println("")
  test_multi_step_chain()
  io.println("[PASS] Multi-step chain propagation verified")
  io.println("")

  // Test 4: Interval Overlap Narrowing
  io.println("--- Test 4: Interval Overlap Narrowing ---")
  io.println("")
  test_interval_overlap()
  io.println("[PASS] Interval overlap narrowing verified")
  io.println("")

  // Test 5: Numerical Stability
  io.println("--- Test 5: Numerical Stability ---")
  io.println("")
  test_numerical_stability()
  io.println("[PASS] Numerical stability at extremes verified")
  io.println("")

  // Test 6: Config Consistency
  io.println("--- Test 6: Config Consistency ---")
  io.println("")
  test_config_consistency()
  io.println("[PASS] Config consistency verified across fast/default/strict")
  io.println("")

  // Test 7: Tolerance Verification
  io.println("--- Test 7: Tolerance Verification ---")
  io.println("")
  test_tolerance_verification()
  io.println("[PASS] Results within config tolerance of analytical values")
  io.println("")

  // Test 8: Phase E Metric Integration
  io.println("--- Test 8: Phase E Metric Integration ---")
  io.println("")
  test_phase_e_metric()
  io.println("[PASS] Phase E analytical bounds metric passes")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL PROBABILISTIC BOUNDS DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Near-Boundary Probabilities
// =============================================================================

fn test_near_boundary_probabilities() {
  io.println(
    "User: Test probability detection at the 0.5 boundary with tolerance 0.0001",
  )

  let a = Atom("boundary_test")
  let config = probabilistic.default_config()

  // Case 1: P(a) >= 0.5001 — exactly at boundary
  // Probable checks: lower > 0.5 + 0.0001 = 0.5001
  // 0.5001 > 0.5001 is False (strict greater-than)
  let result_at =
    probabilistic.validate_probabilistic_with_config(
      [ProbAtLeast(a, 0.5001)],
      Probable(a),
      config,
    )

  // Case 2: P(a) >= 0.5002 — above boundary
  // 0.5002 > 0.5001 is True
  let result_above =
    probabilistic.validate_probabilistic_with_config(
      [ProbAtLeast(a, 0.5002)],
      Probable(a),
      config,
    )

  io.println("[System]: Near-Boundary Analysis")
  io.println(
    "  P(a)>=0.5001 -> Probable(a): "
    <> bool_to_string(result_at.valid)
    <> " (expected: false, at threshold)",
  )
  io.println(
    "  Bounds: ["
    <> float.to_string(result_at.bounds.lower)
    <> ", "
    <> float.to_string(result_at.bounds.upper)
    <> "]",
  )
  io.println(
    "  P(a)>=0.5002 -> Probable(a): "
    <> bool_to_string(result_above.valid)
    <> " (expected: true, above threshold)",
  )
  io.println(
    "  Bounds: ["
    <> float.to_string(result_above.bounds.lower)
    <> ", "
    <> float.to_string(result_above.bounds.upper)
    <> "]",
  )

  // Verify
  result_at.valid |> should.be_false()
  result_above.valid |> should.be_true()

  // Verify bounds
  { float.absolute_value(result_at.bounds.lower -. 0.5001) <. 0.001 }
  |> should.be_true()
  { float.absolute_value(result_above.bounds.lower -. 0.5002) <. 0.001 }
  |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - boundary detection distinguishes 0.5001 (at) from 0.5002 (above)",
  )
}

// =============================================================================
// Test 2: Conflicting Constraints
// =============================================================================

fn test_conflicting_constraints() {
  io.println("User: Test P(a) >= 0.8 AND P(a) <= 0.3 (conflicting constraints)")

  let a = Atom("conflict_test")
  let config = probabilistic.default_config()

  let result =
    probabilistic.validate_probabilistic_with_config(
      [ProbAtLeast(a, 0.8), ProbAtMost(a, 0.3)],
      Probable(a),
      config,
    )

  io.println("[System]: Conflicting Constraint Analysis")
  io.println(
    "  Bounds: ["
    <> float.to_string(result.bounds.lower)
    <> ", "
    <> float.to_string(result.bounds.upper)
    <> "]",
  )
  io.println(
    "  bounds_valid: "
    <> bool_to_string(probabilistic.bounds_valid(result.bounds)),
  )
  io.println(
    "  Probable(a): "
    <> bool_to_string(result.valid)
    <> " (engine checks lower > 0.5001, ignores inconsistency)",
  )

  // Analytical: lower=0.8, upper=0.3
  { float.absolute_value(result.bounds.lower -. 0.8) <. 0.001 }
  |> should.be_true()
  { float.absolute_value(result.bounds.upper -. 0.3) <. 0.001 }
  |> should.be_true()

  // Bounds are invalid (lower > upper)
  probabilistic.bounds_valid(result.bounds) |> should.be_false()

  // Engine still reports valid (checks only lower bound)
  result.valid |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - conflicting constraints produce invalid bounds [0.8, 0.3]",
  )
  io.println(
    "  Note: Engine reports valid=true (known limitation, only checks lower bound)",
  )
}

// =============================================================================
// Test 3: Multi-Step Chain Propagation
// =============================================================================

fn test_multi_step_chain() {
  io.println(
    "User: Test multi-step chain P(a)->P(b|a)->P(c|b) with error accumulation",
  )

  let a = Atom("chain_a")
  let b = Atom("chain_b")
  let c = Atom("chain_c")
  let config = probabilistic.default_config()

  // Passing chain: P(a)>=0.9, P(b|a)=0.8, P(c|b)=0.7
  // Analytical: P(b)>=0.72, P(c)>=0.504
  let result_pass =
    probabilistic.validate_probabilistic_with_config(
      [ProbAtLeast(a, 0.9), CondProb(b, a, 0.8), CondProb(c, b, 0.7)],
      Probable(c),
      config,
    )

  // Failing chain: P(a)>=0.7, P(b|a)=0.8, P(c|b)=0.7
  // Analytical: P(b)>=0.56, P(c)>=0.392
  let result_fail =
    probabilistic.validate_probabilistic_with_config(
      [ProbAtLeast(a, 0.7), CondProb(b, a, 0.8), CondProb(c, b, 0.7)],
      Probable(c),
      config,
    )

  io.println("[System]: Multi-Step Chain Analysis")
  io.println("  Passing chain (P(a)>=0.9):")
  io.println("    Analytical: P(b)>=0.72, P(c)>=0.504 (>0.5001 -> valid)")
  io.println(
    "    Computed bounds for c: ["
    <> float.to_string(result_pass.bounds.lower)
    <> ", "
    <> float.to_string(result_pass.bounds.upper)
    <> "]",
  )
  io.println("    Valid: " <> bool_to_string(result_pass.valid))
  io.println("")
  io.println("  Failing chain (P(a)>=0.7):")
  io.println("    Analytical: P(b)>=0.56, P(c)>=0.392 (<0.5001 -> invalid)")
  io.println(
    "    Computed bounds for c: ["
    <> float.to_string(result_fail.bounds.lower)
    <> ", "
    <> float.to_string(result_fail.bounds.upper)
    <> "]",
  )
  io.println("    Valid: " <> bool_to_string(result_fail.valid))

  // Verify passing chain
  result_pass.valid |> should.be_true()
  { float.absolute_value(result_pass.bounds.lower -. 0.504) <. 0.01 }
  |> should.be_true()

  // Verify failing chain
  result_fail.valid |> should.be_false()
  { float.absolute_value(result_fail.bounds.lower -. 0.392) <. 0.01 }
  |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - multi-step chain correctly propagates through 2 hops",
  )
}

// =============================================================================
// Test 4: Interval Overlap Narrowing
// =============================================================================

fn test_interval_overlap() {
  io.println(
    "User: Test interval overlap P(a) in [0.3,0.7] AND P(a) in [0.5,0.9]",
  )

  let a = Atom("interval_test")
  let config = probabilistic.default_config()

  // Two overlapping ranges: should narrow to intersection [0.5, 0.7]
  let result =
    probabilistic.validate_probabilistic_with_config(
      [ProbRange(a, 0.3, 0.7), ProbRange(a, 0.5, 0.9)],
      ProbRange(a, 0.5, 0.7),
      config,
    )

  io.println("[System]: Interval Overlap Analysis")
  io.println("  Input ranges: [0.3, 0.7] and [0.5, 0.9]")
  io.println(
    "  Computed bounds: ["
    <> float.to_string(result.bounds.lower)
    <> ", "
    <> float.to_string(result.bounds.upper)
    <> "]",
  )
  io.println("  Expected intersection: [0.5, 0.7]")
  io.println(
    "  ProbRange(a, 0.5, 0.7) entailed: " <> bool_to_string(result.valid),
  )

  // Also test intersect_bounds directly
  let bounds_a =
    probabilistic.ProbabilityBounds(lower: 0.3, upper: 0.7, exact: False)
  let bounds_b =
    probabilistic.ProbabilityBounds(lower: 0.5, upper: 0.9, exact: False)
  let intersection = probabilistic.intersect_bounds(bounds_a, bounds_b)

  io.println("")
  io.println("[System]: Direct intersect_bounds test")
  io.println(
    "  intersect([0.3,0.7], [0.5,0.9]) = ["
    <> float.to_string(intersection.lower)
    <> ", "
    <> float.to_string(intersection.upper)
    <> "]",
  )

  // Verify computed bounds
  { float.absolute_value(result.bounds.lower -. 0.5) <. 0.001 }
  |> should.be_true()
  { float.absolute_value(result.bounds.upper -. 0.7) <. 0.001 }
  |> should.be_true()
  result.valid |> should.be_true()

  // Verify direct intersection
  { float.absolute_value(intersection.lower -. 0.5) <. 0.001 }
  |> should.be_true()
  { float.absolute_value(intersection.upper -. 0.7) <. 0.001 }
  |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - interval overlap narrows correctly to [0.5, 0.7]",
  )
}

// =============================================================================
// Test 5: Numerical Stability
// =============================================================================

fn test_numerical_stability() {
  io.println("User: Test numerical stability near P=0.0 and P=1.0")

  let a = Atom("stability_a")
  let b = Atom("stability_b")
  let config = probabilistic.default_config()

  // Near-zero: P(a)>=0.0001, P(b|a)=0.0001
  // Implied: 0.0001 * 0.0001 = 0.00000001 (below tolerance for update)
  let near_zero =
    probabilistic.validate_probabilistic_with_config(
      [ProbAtLeast(a, 0.0001), CondProb(b, a, 0.0001)],
      Probable(b),
      config,
    )

  // Near-one: P(a)>=0.9999, P(b|a)=0.9999
  // Implied: 0.9999 * 0.9999 = 0.99980001
  let near_one =
    probabilistic.validate_probabilistic_with_config(
      [ProbAtLeast(a, 0.9999), CondProb(b, a, 0.9999)],
      Probable(b),
      config,
    )

  io.println("[System]: Numerical Stability Analysis")
  io.println("  Near-zero case:")
  io.println("    P(a)>=0.0001, P(b|a)=0.0001 -> implied P(b)>=1e-8")
  io.println(
    "    Computed bounds: ["
    <> float.to_string(near_zero.bounds.lower)
    <> ", "
    <> float.to_string(near_zero.bounds.upper)
    <> "]",
  )
  io.println(
    "    Valid: " <> bool_to_string(near_zero.valid) <> " (expected: false)",
  )
  io.println("")
  io.println("  Near-one case:")
  io.println("    P(a)>=0.9999, P(b|a)=0.9999 -> implied P(b)>=0.9998")
  io.println(
    "    Computed bounds: ["
    <> float.to_string(near_one.bounds.lower)
    <> ", "
    <> float.to_string(near_one.bounds.upper)
    <> "]",
  )
  io.println(
    "    Valid: " <> bool_to_string(near_one.valid) <> " (expected: true)",
  )

  // Near-zero should not be valid (implied lower too small to propagate)
  near_zero.valid |> should.be_false()

  // Near-one should be valid
  near_one.valid |> should.be_true()

  // Near-one bounds should be close to analytical
  { near_one.bounds.lower >. 0.999 } |> should.be_true()

  // No NaN or infinity (bounds should be within [0, 1])
  { probabilistic.is_valid_probability(near_zero.bounds.lower) }
  |> should.be_true()
  { probabilistic.is_valid_probability(near_zero.bounds.upper) }
  |> should.be_true()
  { probabilistic.is_valid_probability(near_one.bounds.lower) }
  |> should.be_true()
  { probabilistic.is_valid_probability(near_one.bounds.upper) }
  |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - no NaN/infinity, bounds remain valid probabilities",
  )
}

// =============================================================================
// Test 6: Config Consistency
// =============================================================================

fn test_config_consistency() {
  io.println(
    "User: Test same case with default_config, strict_config, and fast_config",
  )

  let a = Atom("cfg_a")
  let b = Atom("cfg_b")

  let premises = [ProbAtLeast(a, 0.8), CondProb(b, a, 0.9)]
  let conclusion = Probable(b)

  // Analytical: P(b) >= 0.9 * 0.8 = 0.72 > 0.5 + tolerance for all configs
  let default_result =
    probabilistic.validate_probabilistic_with_config(
      premises,
      conclusion,
      probabilistic.default_config(),
    )
  let strict_result =
    probabilistic.validate_probabilistic_with_config(
      premises,
      conclusion,
      probabilistic.strict_config(),
    )
  let fast_result =
    probabilistic.validate_probabilistic_with_config(
      premises,
      conclusion,
      probabilistic.fast_config(),
    )

  io.println("[System]: Config Consistency Analysis")
  io.println(
    "  Case: P(a)>=0.8, P(b|a)=0.9 -> Probable(b), analytical P(b)>=0.72",
  )
  io.println("")
  io.println(
    "  default_config (tol=0.0001): valid="
    <> bool_to_string(default_result.valid)
    <> ", lower="
    <> float.to_string(default_result.bounds.lower),
  )
  io.println(
    "  strict_config  (tol=0.00001): valid="
    <> bool_to_string(strict_result.valid)
    <> ", lower="
    <> float.to_string(strict_result.bounds.lower),
  )
  io.println(
    "  fast_config    (tol=0.001):   valid="
    <> bool_to_string(fast_result.valid)
    <> ", lower="
    <> float.to_string(fast_result.bounds.lower),
  )

  // All should agree on validity
  default_result.valid |> should.be_true()
  strict_result.valid |> should.be_true()
  fast_result.valid |> should.be_true()

  // All should have similar lower bounds (within 0.01)
  { float.absolute_value(default_result.bounds.lower -. 0.72) <. 0.01 }
  |> should.be_true()
  { float.absolute_value(strict_result.bounds.lower -. 0.72) <. 0.01 }
  |> should.be_true()
  { float.absolute_value(fast_result.bounds.lower -. 0.72) <. 0.01 }
  |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - all three configs agree: valid=true, lower~=0.72",
  )
}

// =============================================================================
// Test 7: Tolerance Verification
// =============================================================================

fn test_tolerance_verification() {
  io.println(
    "User: Verify computed bounds are within config.tolerance of analytical values",
  )

  let config = probabilistic.default_config()
  let tolerance = config.tolerance

  // Test cases with known analytical solutions
  let test_cases = [
    // (description, premises, conclusion, analytical_lower, analytical_upper)
    #(
      "Direct lower bound",
      [ProbAtLeast(Atom("tol_a"), 0.7)],
      Probable(Atom("tol_a")),
      0.7,
      1.0,
    ),
    #(
      "Chain rule: 0.8 * 0.9 = 0.72",
      [
        ProbAtLeast(Atom("tol_b"), 0.8),
        CondProb(Atom("tol_c"), Atom("tol_b"), 0.9),
      ],
      Probable(Atom("tol_c")),
      0.72,
      1.0,
    ),
    #(
      "Range bounds: [0.4, 0.6]",
      [ProbRange(Atom("tol_d"), 0.4, 0.6)],
      ProbRange(Atom("tol_d"), 0.4, 0.6),
      0.4,
      0.6,
    ),
    #(
      "Chain rule: 0.7 * 0.9 = 0.63",
      [
        ProbAtLeast(Atom("tol_e"), 0.7),
        CondProb(Atom("tol_f"), Atom("tol_e"), 0.9),
      ],
      Probable(Atom("tol_f")),
      0.63,
      1.0,
    ),
  ]

  let max_error =
    list.fold(test_cases, 0.0, fn(max_err, test_case) {
      let #(
        description,
        premises,
        conclusion,
        analytical_lower,
        analytical_upper,
      ) = test_case
      let result =
        probabilistic.validate_probabilistic_with_config(
          premises,
          conclusion,
          config,
        )

      let lower_error =
        float.absolute_value(result.bounds.lower -. analytical_lower)
      let upper_error =
        float.absolute_value(result.bounds.upper -. analytical_upper)
      let case_error = float.max(lower_error, upper_error)

      io.println(
        "  "
        <> description
        <> ": error="
        <> float.to_string(case_error)
        <> " (within tolerance: "
        <> bool_to_string(case_error <. tolerance)
        <> ")",
      )

      // Each case must be within tolerance
      { case_error <. tolerance } |> should.be_true()

      float.max(max_err, case_error)
    })

  io.println("")
  io.println(
    "[System]: Max observed error: "
    <> float.to_string(max_error)
    <> " (tolerance: "
    <> float.to_string(tolerance)
    <> ")",
  )
  io.println("")
  io.println("[System]: Confirmed - all cases within config tolerance")
}

// =============================================================================
// Test 8: Phase E Metric Integration
// =============================================================================

fn test_phase_e_metric() {
  io.println("User: Run the validate_probabilistic_analytical_bounds metric")

  let metric_result =
    epic_validation.validate_probabilistic_analytical_bounds(50)

  io.println("[System]: Phase E Analytical Bounds Metric")
  io.println("  Name: " <> metric_result.name)
  io.println("  Target: " <> float_to_pct(metric_result.target /. 100.0))
  io.println("  Actual: " <> float_to_pct(metric_result.actual /. 100.0))
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
  metric_result.name |> should.equal("probabilistic_analytical_bounds")

  // Should have test cases
  { metric_result.samples > 0 } |> should.be_true()

  // Score should be valid percentage
  { metric_result.actual >=. 0.0 } |> should.be_true()
  { metric_result.actual <=. 100.0 } |> should.be_true()

  // Metric should pass (>= 90%)
  metric_result.passed |> should.be_true()

  io.println("")
  io.println(
    "[System]: Confirmed - analytical bounds metric passes with "
    <> int.to_string(metric_result.samples)
    <> " adversarial cases",
  )
}

// =============================================================================
// Helpers
// =============================================================================

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

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
