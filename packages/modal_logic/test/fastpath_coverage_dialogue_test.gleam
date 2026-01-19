//// Dialogue Test: Fastpath Coverage Improvement (Issue #164)
////
//// This dialogue test demonstrates the improved Tier 1 heuristic coverage
//// that resolves more formula patterns without requiring Z3 solver.
////
//// ## Test Objectives
//// - Verify classical inference patterns are handled at Tier 1
//// - Verify fallacy detection works at Tier 1
//// - Verify modal inference patterns are handled at Tier 1
//// - Confirm fastpath coverage >= 80% target
////
//// ## Changes Implemented
//// 1. Added classical inference patterns to Tier 1:
////    - Modus ponens: p → q, p ⊢ q
////    - Modus tollens: p → q, ¬q ⊢ ¬p
////    - Hypothetical syllogism: p → q, q → r ⊢ p → r
////    - Disjunctive syllogism: p ∨ q, ¬p ⊢ q
////    - Conjunction elimination: p ∧ q ⊢ p
////    - Conjunction introduction: p, q ⊢ p ∧ q
////    - Disjunction introduction: p ⊢ p ∨ q
////    - Double negation elimination: ¬¬p ⊢ p
////
//// 2. Added fallacy detection at Tier 1:
////    - Affirming the consequent: p → q, q ⊢ p (invalid)
////    - Denying the antecedent: p → q, ¬p ⊢ ¬q (invalid)
////    - Affirming a disjunct: p ∨ q, p ⊢ ¬q (invalid)
////
//// 3. Added modal inference patterns at Tier 1:
////    - K distribution: □(p → q), □p ⊢ □q
////    - Modal modus ponens: □(p → q), p ⊢ q

import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import modal_logic/argument.{
  type ValidationResult, Formalization, Invalid, Valid,
}
import modal_logic/heuristics.{type ValidationTier, Tier1Syntactic}
import modal_logic/proposition.{
  And, Atom, Implies, K, Necessary, Not, Or, Possible, S5, T,
}
import modal_logic/testing/epic_validation

// =============================================================================
// Main Dialogue Test
// =============================================================================

pub fn fastpath_coverage_dialogue_test() {
  io.println("")
  io.println(
    "======================================================================",
  )
  io.println("DIALOGUE TEST: Fastpath Coverage Improvement (Issue #164)")
  io.println(
    "======================================================================",
  )
  io.println("")

  // Test 1: Classical inference patterns
  io.println("--- Test 1: Classical Inference Patterns ---")
  io.println("")
  test_classical_inference_patterns()
  io.println("[PASS] Classical inference patterns verified")
  io.println("")

  // Test 2: Fallacy detection
  io.println("--- Test 2: Fallacy Detection at Tier 1 ---")
  io.println("")
  test_fallacy_detection()
  io.println("[PASS] Fallacy detection verified")
  io.println("")

  // Test 3: Modal inference patterns
  io.println("--- Test 3: Modal Inference Patterns ---")
  io.println("")
  test_modal_inference_patterns()
  io.println("[PASS] Modal inference patterns verified")
  io.println("")

  // Test 4: Coverage metrics
  io.println("--- Test 4: Coverage Metrics Validation ---")
  io.println("")
  test_coverage_metrics()
  io.println("[PASS] Coverage metrics verified")
  io.println("")

  io.println(
    "======================================================================",
  )
  io.println("ALL DIALOGUE TESTS PASSED")
  io.println(
    "======================================================================",
  )
  io.println("")
}

// =============================================================================
// Test 1: Classical Inference Patterns
// =============================================================================

fn test_classical_inference_patterns() {
  let p = Atom("p")
  let q = Atom("q")
  let r = Atom("r")

  // Modus ponens
  io.println("User: Test modus ponens: p → q, p ⊢ q")
  let mp_result = test_formalization([Implies(p, q), p], q, K)
  io.println(
    "[System]: " <> format_result(mp_result) <> " - " <> tier_name(mp_result.0),
  )
  mp_result.0 |> should.equal(Tier1Syntactic)
  mp_result.1 |> should.equal(Valid)
  io.println("")

  // Modus tollens
  io.println("User: Test modus tollens: p → q, ¬q ⊢ ¬p")
  let mt_result = test_formalization([Implies(p, q), Not(q)], Not(p), K)
  io.println(
    "[System]: " <> format_result(mt_result) <> " - " <> tier_name(mt_result.0),
  )
  mt_result.0 |> should.equal(Tier1Syntactic)
  mt_result.1 |> should.equal(Valid)
  io.println("")

  // Hypothetical syllogism
  io.println("User: Test hypothetical syllogism: p → q, q → r ⊢ p → r")
  let hs_result =
    test_formalization([Implies(p, q), Implies(q, r)], Implies(p, r), K)
  io.println(
    "[System]: " <> format_result(hs_result) <> " - " <> tier_name(hs_result.0),
  )
  hs_result.0 |> should.equal(Tier1Syntactic)
  hs_result.1 |> should.equal(Valid)
  io.println("")

  // Disjunctive syllogism
  io.println("User: Test disjunctive syllogism: p ∨ q, ¬p ⊢ q")
  let ds_result = test_formalization([Or(p, q), Not(p)], q, K)
  io.println(
    "[System]: " <> format_result(ds_result) <> " - " <> tier_name(ds_result.0),
  )
  ds_result.0 |> should.equal(Tier1Syntactic)
  ds_result.1 |> should.equal(Valid)
  io.println("")

  // Conjunction elimination
  io.println("User: Test conjunction elimination: p ∧ q ⊢ p")
  let ce_result = test_formalization([And(p, q)], p, K)
  io.println(
    "[System]: " <> format_result(ce_result) <> " - " <> tier_name(ce_result.0),
  )
  ce_result.0 |> should.equal(Tier1Syntactic)
  ce_result.1 |> should.equal(Valid)
  io.println("")

  // Conjunction introduction
  io.println("User: Test conjunction introduction: p, q ⊢ p ∧ q")
  let ci_result = test_formalization([p, q], And(p, q), K)
  io.println(
    "[System]: " <> format_result(ci_result) <> " - " <> tier_name(ci_result.0),
  )
  ci_result.0 |> should.equal(Tier1Syntactic)
  ci_result.1 |> should.equal(Valid)
  io.println("")

  // Disjunction introduction
  io.println("User: Test disjunction introduction: p ⊢ p ∨ q")
  let di_result = test_formalization([p], Or(p, q), K)
  io.println(
    "[System]: " <> format_result(di_result) <> " - " <> tier_name(di_result.0),
  )
  di_result.0 |> should.equal(Tier1Syntactic)
  di_result.1 |> should.equal(Valid)
  io.println("")

  // Double negation elimination
  io.println("User: Test double negation: ¬¬p ⊢ p")
  let dn_result = test_formalization([Not(Not(p))], p, K)
  io.println(
    "[System]: " <> format_result(dn_result) <> " - " <> tier_name(dn_result.0),
  )
  dn_result.0 |> should.equal(Tier1Syntactic)
  dn_result.1 |> should.equal(Valid)
}

// =============================================================================
// Test 2: Fallacy Detection
// =============================================================================

fn test_fallacy_detection() {
  let p = Atom("p")
  let q = Atom("q")

  // Affirming the consequent
  io.println("User: Test affirming the consequent: p → q, q ⊢ p (invalid)")
  let ac_result = test_formalization([Implies(p, q), q], p, K)
  io.println(
    "[System]: " <> format_result(ac_result) <> " - " <> tier_name(ac_result.0),
  )
  io.println("         Correctly identified as INVALID fallacy")
  ac_result.0 |> should.equal(Tier1Syntactic)
  case ac_result.1 {
    Invalid(_) -> Nil
    _ -> panic as "Expected Invalid for affirming consequent"
  }
  io.println("")

  // Denying the antecedent
  io.println("User: Test denying the antecedent: p → q, ¬p ⊢ ¬q (invalid)")
  let da_result = test_formalization([Implies(p, q), Not(p)], Not(q), K)
  io.println(
    "[System]: " <> format_result(da_result) <> " - " <> tier_name(da_result.0),
  )
  io.println("         Correctly identified as INVALID fallacy")
  da_result.0 |> should.equal(Tier1Syntactic)
  case da_result.1 {
    Invalid(_) -> Nil
    _ -> panic as "Expected Invalid for denying antecedent"
  }
}

// =============================================================================
// Test 3: Modal Inference Patterns
// =============================================================================

fn test_modal_inference_patterns() {
  let p = Atom("p")
  let q = Atom("q")

  // K distribution
  io.println("User: Test K distribution: □(p → q), □p ⊢ □q")
  let kd_result =
    test_formalization(
      [Necessary(Implies(p, q)), Necessary(p)],
      Necessary(q),
      K,
    )
  io.println(
    "[System]: " <> format_result(kd_result) <> " - " <> tier_name(kd_result.0),
  )
  kd_result.0 |> should.equal(Tier1Syntactic)
  kd_result.1 |> should.equal(Valid)
  io.println("")

  // T axiom in T system
  io.println("User: Test T axiom in T system: □p → p")
  let t_formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: T,
      premises: [],
      conclusion: Implies(Necessary(p), p),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )
  let t_result = case heuristics.try_heuristic_validation(t_formalization) {
    Some(r) -> #(r.tier, r.result)
    None -> panic as "Expected Tier 1 match"
  }
  io.println(
    "[System]: " <> format_result(t_result) <> " - " <> tier_name(t_result.0),
  )
  t_result.0 |> should.equal(Tier1Syntactic)
  t_result.1 |> should.equal(Valid)
  io.println("")

  // 5 axiom in S5 system
  io.println("User: Test 5 axiom in S5 system: ◇p → □◇p")
  let s5_formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: S5,
      premises: [],
      conclusion: Implies(Possible(p), Necessary(Possible(p))),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )
  let s5_result = case heuristics.try_heuristic_validation(s5_formalization) {
    Some(r) -> #(r.tier, r.result)
    None -> panic as "Expected Tier 1 match"
  }
  io.println(
    "[System]: " <> format_result(s5_result) <> " - " <> tier_name(s5_result.0),
  )
  s5_result.0 |> should.equal(Tier1Syntactic)
  s5_result.1 |> should.equal(Valid)
}

// =============================================================================
// Test 4: Coverage Metrics
// =============================================================================

fn test_coverage_metrics() {
  io.println("User: Measure fastpath coverage with 100 mixed test cases")

  let coverage_result = epic_validation.validate_fastpath_coverage(100)

  io.println(
    "[System]: Fastpath coverage: "
    <> float_to_string(coverage_result.actual)
    <> "%",
  )
  io.println(
    "         Target: " <> float_to_string(coverage_result.target) <> "%",
  )
  io.println(
    "         Status: "
    <> case coverage_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )
  io.println("")

  // Verify coverage meets target
  { coverage_result.actual >=. 80.0 } |> should.be_true()

  io.println("User: Verify tier selection accuracy")
  let tier_result = epic_validation.validate_tier_selection_accuracy(100)
  io.println(
    "[System]: Tier selection accuracy: "
    <> float_to_string(tier_result.actual)
    <> "%",
  )
  io.println("         Target: " <> float_to_string(tier_result.target) <> "%")
  io.println(
    "         Status: "
    <> case tier_result.passed {
      True -> "PASS"
      False -> "FAIL"
    },
  )

  // Verify tier selection accuracy meets target
  { tier_result.actual >=. 95.0 } |> should.be_true()
}

// =============================================================================
// Helper Functions
// =============================================================================

fn test_formalization(
  premises: List(proposition.Proposition),
  conclusion: proposition.Proposition,
  system: proposition.LogicSystem,
) -> #(ValidationTier, ValidationResult) {
  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: system,
      premises: premises,
      conclusion: conclusion,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> #(result.tier, result.result)
    None -> panic as "Expected heuristic validation to succeed"
  }
}

fn format_result(result: #(ValidationTier, ValidationResult)) -> String {
  case result.1 {
    Valid -> "Valid"
    Invalid(reason) -> "Invalid: " <> string.slice(reason, 0, 50)
    argument.Unknown(reason) -> "Unknown: " <> string.slice(reason, 0, 50)
    argument.Timeout -> "Timeout"
    argument.Error(err) -> "Error: " <> string.slice(err, 0, 50)
  }
}

fn tier_name(tier: ValidationTier) -> String {
  case tier {
    heuristics.Tier1Syntactic -> "Tier1 (Syntactic)"
    heuristics.Tier2TruthTable -> "Tier2 (TruthTable)"
    heuristics.Tier3Z3 -> "Tier3 (Z3)"
  }
}

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let decimal = float_truncate({ f -. int_to_float(whole) } *. 100.0)
  int.to_string(whole)
  <> "."
  <> case decimal < 10 {
    True -> "0"
    False -> ""
  }
  <> int.to_string(decimal)
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
