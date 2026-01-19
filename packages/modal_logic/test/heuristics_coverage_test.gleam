//// Test heuristics coverage improvements for Issue #164
////
//// This test verifies that the new Tier 1 patterns are being detected correctly.

import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/argument.{Formalization}
import modal_logic/heuristics
import modal_logic/proposition.{And, Atom, Implies, K, Not, Or}

// =============================================================================
// Test: Modus Ponens Detection
// =============================================================================

pub fn modus_ponens_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [Implies(p, q), p],
      conclusion: q,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for modus ponens"
  }
}

// =============================================================================
// Test: Modus Tollens Detection
// =============================================================================

pub fn modus_tollens_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [Implies(p, q), Not(q)],
      conclusion: Not(p),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for modus tollens"
  }
}

// =============================================================================
// Test: Hypothetical Syllogism Detection
// =============================================================================

pub fn hypothetical_syllogism_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")
  let r = Atom("r")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [Implies(p, q), Implies(q, r)],
      conclusion: Implies(p, r),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for hypothetical syllogism"
  }
}

// =============================================================================
// Test: Disjunctive Syllogism Detection
// =============================================================================

pub fn disjunctive_syllogism_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [Or(p, q), Not(p)],
      conclusion: q,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for disjunctive syllogism"
  }
}

// =============================================================================
// Test: Conjunction Elimination Detection
// =============================================================================

pub fn conjunction_elimination_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [And(p, q)],
      conclusion: p,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for conjunction elimination"
  }
}

// =============================================================================
// Test: Conjunction Introduction Detection
// =============================================================================

pub fn conjunction_introduction_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [p, q],
      conclusion: And(p, q),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for conjunction introduction"
  }
}

// =============================================================================
// Test: Disjunction Introduction Detection
// =============================================================================

pub fn disjunction_introduction_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [p],
      conclusion: Or(p, q),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for disjunction introduction"
  }
}

// =============================================================================
// Test: Double Negation Elimination Detection
// =============================================================================

pub fn double_negation_elimination_tier1_test() {
  let p = Atom("p")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [Not(Not(p))],
      conclusion: p,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      result.tier |> should.equal(heuristics.Tier1Syntactic)
    }
    None -> panic as "Expected Tier 1 match for double negation elimination"
  }
}

// =============================================================================
// Test: Fallacy Detection - Affirming the Consequent
// =============================================================================

pub fn affirming_consequent_fallacy_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [Implies(p, q), q],
      conclusion: p,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      // Should be caught as invalid at Tier 1
      result.tier |> should.equal(heuristics.Tier1Syntactic)
      case result.result {
        argument.Invalid(_) -> Nil
        _ -> panic as "Expected Invalid result for affirming consequent"
      }
    }
    None -> panic as "Expected Tier 1 match for affirming consequent fallacy"
  }
}

// =============================================================================
// Test: Fallacy Detection - Denying the Antecedent
// =============================================================================

pub fn denying_antecedent_fallacy_tier1_test() {
  let p = Atom("p")
  let q = Atom("q")

  let formalization =
    Formalization(
      id: "test",
      argument_id: "test",
      logic_system: K,
      premises: [Implies(p, q), Not(p)],
      conclusion: Not(q),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  case heuristics.try_heuristic_validation(formalization) {
    Some(result) -> {
      // Should be caught as invalid at Tier 1
      result.tier |> should.equal(heuristics.Tier1Syntactic)
      case result.result {
        argument.Invalid(_) -> Nil
        _ -> panic as "Expected Invalid result for denying antecedent"
      }
    }
    None -> panic as "Expected Tier 1 match for denying antecedent fallacy"
  }
}

// =============================================================================
// Integration Test: Measure Tier 1 Coverage Improvement
// =============================================================================

pub fn tier1_coverage_improvement_test() {
  let p = Atom("p")
  let q = Atom("q")
  let r = Atom("r")

  // All of these should now be handled by Tier 1
  let test_cases = [
    // Modus ponens
    Formalization(
      id: "mp",
      argument_id: "mp",
      logic_system: K,
      premises: [Implies(p, q), p],
      conclusion: q,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    ),
    // Modus tollens
    Formalization(
      id: "mt",
      argument_id: "mt",
      logic_system: K,
      premises: [Implies(p, q), Not(q)],
      conclusion: Not(p),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    ),
    // Hypothetical syllogism
    Formalization(
      id: "hs",
      argument_id: "hs",
      logic_system: K,
      premises: [Implies(p, q), Implies(q, r)],
      conclusion: Implies(p, r),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    ),
    // Disjunctive syllogism
    Formalization(
      id: "ds",
      argument_id: "ds",
      logic_system: K,
      premises: [Or(p, q), Not(p)],
      conclusion: q,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    ),
    // Affirming consequent (fallacy)
    Formalization(
      id: "ac",
      argument_id: "ac",
      logic_system: K,
      premises: [Implies(p, q), q],
      conclusion: p,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    ),
    // Denying antecedent (fallacy)
    Formalization(
      id: "da",
      argument_id: "da",
      logic_system: K,
      premises: [Implies(p, q), Not(p)],
      conclusion: Not(q),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    ),
  ]

  let tier1_count =
    test_cases
    |> list.count(fn(f) {
      case heuristics.try_heuristic_validation(f) {
        Some(result) -> result.tier == heuristics.Tier1Syntactic
        None -> False
      }
    })

  io.println("")
  io.println("Tier 1 Coverage Test:")
  io.println(
    "  "
    <> int_to_string(tier1_count)
    <> "/"
    <> int_to_string(list.length(test_cases))
    <> " cases handled at Tier 1",
  )

  // All 6 cases should be handled at Tier 1
  tier1_count |> should.equal(list.length(test_cases))
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "10"
    _ -> "many"
  }
}
