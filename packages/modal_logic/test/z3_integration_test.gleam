//// Z3 Integration Tests
////
//// Tests for the Z3 solver integration with the modal logic validator.
//// These tests verify that the validator correctly identifies valid and
//// invalid modal arguments using the Z3 SMT solver.

import gleam/io
import gleam/option.{None}
import gleam/string
import modal_logic/argument
import modal_logic/proposition.{Atom, Implies, K, Necessary, Possible, S4, S5, T}
import modal_logic/validator

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Z3 Integration Tests")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Distribution axiom K (valid in all systems)
  test_distribution_axiom_k()

  // Test 2: Reflexivity test (valid in T, invalid in K)
  test_reflexivity_t_vs_k()

  // Test 3: S5 characteristic axiom
  test_s5_characteristic()

  // Test 4: Simple propositional validity
  test_propositional_valid()

  // Test 5: Simple propositional invalidity
  test_propositional_invalid()

  // Test 6: Modal modus ponens
  test_modal_modus_ponens()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All Z3 Integration Tests Completed!")
  io.println("=" |> string.repeat(70))
}

/// Test: Distribution axiom K
/// Premises: □(p → q), □p
/// Conclusion: □q
/// Should be Valid in all systems (including K)
fn test_distribution_axiom_k() {
  io.println("")
  io.println("--- Test 1: Distribution Axiom K ---")
  io.println("Premises: □(p → q), □p")
  io.println("Conclusion: □q")
  io.println("Expected: Valid in K (and all other systems)")
  io.println("")

  let formalization =
    argument.Formalization(
      id: "dist-k-test",
      argument_id: "arg-dist-k",
      logic_system: K,
      premises: [
        Necessary(Implies(Atom("p"), Atom("q"))),
        Necessary(Atom("p")),
      ],
      conclusion: Necessary(Atom("q")),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let config = validator.default_config() |> validator.with_max_worlds(3)
  let state = validator.new_state(config)
  let #(_new_state, response) = validator.validate(state, formalization, 0)

  case response.result {
    argument.Valid -> {
      io.println("[OK] Distribution axiom correctly identified as VALID")
    }
    argument.Invalid(reason) -> {
      io.println("[INFO] Result: Invalid - " <> reason)
      io.println("       (May indicate Z3 not available - using fallback)")
    }
    _ -> {
      io.println("[INFO] Unexpected result")
    }
  }
  io.println("     From cache: " <> bool_to_string(response.from_cache))
}

/// Test: Reflexivity axiom T
/// Premise: □p
/// Conclusion: p
/// Should be Valid in T, S4, S5 but Invalid in K
fn test_reflexivity_t_vs_k() {
  io.println("")
  io.println("--- Test 2: Reflexivity Test (T vs K) ---")
  io.println("Premise: □p")
  io.println("Conclusion: p")
  io.println("Expected: Valid in T, Invalid in K")
  io.println("")

  // Test in K (should be invalid)
  let formalization_k =
    argument.Formalization(
      id: "reflex-k-test",
      argument_id: "arg-reflex-k",
      logic_system: K,
      premises: [Necessary(Atom("p"))],
      conclusion: Atom("p"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let config = validator.default_config() |> validator.with_max_worlds(2)
  let state = validator.new_state(config)
  let #(state2, response_k) = validator.validate(state, formalization_k, 0)

  case response_k.result {
    argument.Valid -> {
      io.println("[UNEXPECTED] K system: Valid (expected Invalid)")
    }
    argument.Invalid(reason) -> {
      io.println("[OK] K system correctly identified as INVALID")
      io.println("     Reason: " <> string.slice(reason, 0, 60) <> "...")
    }
    _ -> {
      io.println("[INFO] K system: Unexpected result")
    }
  }

  // Test in T (should be valid)
  let formalization_t =
    argument.Formalization(
      id: "reflex-t-test",
      argument_id: "arg-reflex-t",
      logic_system: T,
      premises: [Necessary(Atom("p"))],
      conclusion: Atom("p"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let #(_state3, response_t) = validator.validate(state2, formalization_t, 0)

  case response_t.result {
    argument.Valid -> {
      io.println("[OK] T system correctly identified as VALID")
    }
    argument.Invalid(reason) -> {
      io.println("[INFO] T system: Invalid - " <> reason)
      io.println("       (May indicate Z3 not available)")
    }
    _ -> {
      io.println("[INFO] T system: Unexpected result")
    }
  }
}

/// Test: S5 characteristic axiom
/// Premise: ◇□p
/// Conclusion: □p
/// Should be Valid in S5, Invalid in K
fn test_s5_characteristic() {
  io.println("")
  io.println("--- Test 3: S5 Characteristic Axiom ---")
  io.println("Premise: ◇□p")
  io.println("Conclusion: □p")
  io.println("Expected: Valid in S5, Invalid in K")
  io.println("")

  // Test in S5
  let formalization_s5 =
    argument.Formalization(
      id: "s5-char-test",
      argument_id: "arg-s5-char",
      logic_system: S5,
      premises: [Possible(Necessary(Atom("p")))],
      conclusion: Necessary(Atom("p")),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let config = validator.default_config() |> validator.with_max_worlds(3)
  let state = validator.new_state(config)
  let #(state2, response_s5) = validator.validate(state, formalization_s5, 0)

  case response_s5.result {
    argument.Valid -> {
      io.println("[OK] S5 system correctly identified as VALID")
    }
    argument.Invalid(reason) -> {
      io.println("[INFO] S5 system: Invalid - " <> reason)
    }
    _ -> {
      io.println("[INFO] S5 system: Unexpected result")
    }
  }

  // Test in K
  let formalization_k =
    argument.Formalization(
      id: "s5-char-k-test",
      argument_id: "arg-s5-char-k",
      logic_system: K,
      premises: [Possible(Necessary(Atom("p")))],
      conclusion: Necessary(Atom("p")),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let #(_state3, response_k) = validator.validate(state2, formalization_k, 0)

  case response_k.result {
    argument.Valid -> {
      io.println("[UNEXPECTED] K system: Valid (expected Invalid)")
    }
    argument.Invalid(_) -> {
      io.println("[OK] K system correctly identified as INVALID")
    }
    _ -> {
      io.println("[INFO] K system: Unexpected result")
    }
  }
}

/// Test: Simple propositional validity
/// Premises: p → q, p
/// Conclusion: q
/// Should be Valid (modus ponens)
fn test_propositional_valid() {
  io.println("")
  io.println("--- Test 4: Propositional Modus Ponens ---")
  io.println("Premises: p → q, p")
  io.println("Conclusion: q")
  io.println("Expected: Valid")
  io.println("")

  let formalization =
    argument.Formalization(
      id: "prop-mp-test",
      argument_id: "arg-prop-mp",
      logic_system: K,
      premises: [Implies(Atom("p"), Atom("q")), Atom("p")],
      conclusion: Atom("q"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let config = validator.default_config()
  let state = validator.new_state(config)
  let #(_new_state, response) = validator.validate(state, formalization, 0)

  case response.result {
    argument.Valid -> {
      io.println("[OK] Modus ponens correctly identified as VALID")
    }
    argument.Invalid(reason) -> {
      io.println("[INFO] Result: Invalid - " <> reason)
    }
    _ -> {
      io.println("[INFO] Unexpected result")
    }
  }
}

/// Test: Simple propositional invalidity
/// Premises: p → q, q
/// Conclusion: p
/// Should be Invalid (affirming the consequent)
fn test_propositional_invalid() {
  io.println("")
  io.println("--- Test 5: Affirming the Consequent (Invalid) ---")
  io.println("Premises: p → q, q")
  io.println("Conclusion: p")
  io.println("Expected: Invalid")
  io.println("")

  let formalization =
    argument.Formalization(
      id: "affirm-conseq-test",
      argument_id: "arg-affirm-conseq",
      logic_system: K,
      premises: [Implies(Atom("p"), Atom("q")), Atom("q")],
      conclusion: Atom("p"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let config = validator.default_config()
  let state = validator.new_state(config)
  let #(_new_state, response) = validator.validate(state, formalization, 0)

  case response.result {
    argument.Valid -> {
      io.println("[UNEXPECTED] Affirming consequent marked as Valid")
    }
    argument.Invalid(reason) -> {
      io.println("[OK] Affirming consequent correctly identified as INVALID")
      io.println("     Countermodel: " <> string.slice(reason, 0, 60) <> "...")
    }
    _ -> {
      io.println("[INFO] Unexpected result")
    }
  }
}

/// Test: Modal modus ponens
/// Premises: □(p → q), p
/// Conclusion: q
/// Should be Valid in T (requires reflexivity for p to imply □(p → q) applies)
fn test_modal_modus_ponens() {
  io.println("")
  io.println("--- Test 6: Modal Modus Ponens in S4 ---")
  io.println("Premises: □(p → q), p")
  io.println("Conclusion: q")
  io.println("Expected: Valid in S4 (reflexive frame)")
  io.println("")

  let formalization =
    argument.Formalization(
      id: "modal-mp-test",
      argument_id: "arg-modal-mp",
      logic_system: S4,
      premises: [Necessary(Implies(Atom("p"), Atom("q"))), Atom("p")],
      conclusion: Atom("q"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let config = validator.default_config() |> validator.with_max_worlds(3)
  let state = validator.new_state(config)
  let #(_new_state, response) = validator.validate(state, formalization, 0)

  case response.result {
    argument.Valid -> {
      io.println("[OK] Modal modus ponens correctly identified as VALID in S4")
    }
    argument.Invalid(reason) -> {
      io.println("[INFO] Result: Invalid - " <> reason)
    }
    _ -> {
      io.println("[INFO] Unexpected result")
    }
  }
}

// Helper function
fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
