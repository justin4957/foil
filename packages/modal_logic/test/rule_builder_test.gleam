//// Tests for Rule Builder DSL
////
//// This module tests the inference rule builder DSL and pattern matching.

import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/proposition.{
  And, Atom, Implies, Knows, Necessary, Not, Possible, S5,
}
import modal_logic/rules/axiom
import modal_logic/rules/inference_rule.{
  Applied, NoMatch, apply_rule, empty_bindings, match_pattern,
}
import modal_logic/rules/rule_builder.{
  all_rules, atom, build, derives, four_axiom_application,
  hypothetical_syllogism, implies, inference_rule, modal_modus_ponens,
  modus_ponens, modus_tollens, named, necessary, possible, t_axiom_application,
  valid_in_all, with_premise,
}

// ============ Pattern Matching Tests ============

pub fn match_atom_pattern_test() {
  let pattern = atom("p")
  let prop = Atom("rain")

  let result = match_pattern(pattern, prop, empty_bindings())

  result
  |> should.be_some
  |> dict.get("p")
  |> should.equal(Ok(Atom("rain")))
}

pub fn match_necessary_pattern_test() {
  let pattern = necessary(atom("p"))
  let prop = Necessary(Atom("true"))

  let result = match_pattern(pattern, prop, empty_bindings())

  result
  |> should.be_some
  |> dict.get("p")
  |> should.equal(Ok(Atom("true")))
}

pub fn match_implies_pattern_test() {
  let pattern = implies(atom("p"), atom("q"))
  let prop = Implies(Atom("rain"), Atom("wet"))

  let result = match_pattern(pattern, prop, empty_bindings())

  let bindings = result |> should.be_some
  bindings |> dict.get("p") |> should.equal(Ok(Atom("rain")))
  bindings |> dict.get("q") |> should.equal(Ok(Atom("wet")))
}

pub fn match_complex_pattern_test() {
  // Pattern: □(p → q)
  let pattern = necessary(implies(atom("p"), atom("q")))
  let prop = Necessary(Implies(Atom("rain"), Atom("wet")))

  let result = match_pattern(pattern, prop, empty_bindings())

  let bindings = result |> should.be_some
  bindings |> dict.get("p") |> should.equal(Ok(Atom("rain")))
  bindings |> dict.get("q") |> should.equal(Ok(Atom("wet")))
}

pub fn match_pattern_failure_test() {
  // Pattern expects Necessary, prop is Possible
  let pattern = necessary(atom("p"))
  let prop = Possible(Atom("true"))

  let result = match_pattern(pattern, prop, empty_bindings())
  result |> should.be_none
}

pub fn match_pattern_variable_consistency_test() {
  // Pattern uses same variable twice - must match same proposition
  let pattern = implies(atom("p"), atom("p"))

  // Same atom on both sides - should match
  let prop1 = Implies(Atom("rain"), Atom("rain"))
  match_pattern(pattern, prop1, empty_bindings())
  |> should.be_some

  // Different atoms - should not match
  let prop2 = Implies(Atom("rain"), Atom("wet"))
  match_pattern(pattern, prop2, empty_bindings())
  |> should.be_none
}

// ============ Rule Builder Tests ============

pub fn build_simple_rule_test() {
  let result =
    inference_rule("test_rule")
    |> named("Test Rule")
    |> with_premise(atom("p"))
    |> derives(atom("q"))
    |> build()

  case result {
    Ok(rule) -> {
      rule.id |> should.equal("test_rule")
      rule.name |> should.equal("Test Rule")
    }
    Error(msg) -> panic as { "Expected Ok, got Error: " <> msg }
  }
}

pub fn build_rule_without_conclusion_test() {
  let result =
    inference_rule("incomplete_rule")
    |> with_premise(atom("p"))
    |> build()

  case result {
    Ok(_) -> panic as "Expected Error for rule without conclusion"
    Error(msg) ->
      msg |> should.equal("Inference rule must have a conclusion pattern")
  }
}

// ============ Built-in Rule Tests ============

pub fn modus_ponens_rule_test() {
  let rule = modus_ponens()

  rule.id |> should.equal("modus_ponens")
  rule.name |> should.equal("Modus Ponens")
}

pub fn modus_ponens_application_test() {
  let rule = modus_ponens()

  // Apply modus ponens: p, p → q ⊢ q
  let premises = [Atom("rain"), Implies(Atom("rain"), Atom("wet"))]

  case apply_rule(rule, premises) {
    Applied(conclusion, _bindings) -> conclusion |> should.equal(Atom("wet"))
    NoMatch(reason) -> panic as { "Expected Applied, got NoMatch: " <> reason }
    _ -> panic as "Unexpected result"
  }
}

pub fn modus_tollens_rule_test() {
  let rule = modus_tollens()

  // Apply modus tollens: p → q, ¬q ⊢ ¬p
  let premises = [Implies(Atom("rain"), Atom("wet")), Not(Atom("wet"))]

  case apply_rule(rule, premises) {
    Applied(conclusion, _bindings) ->
      conclusion |> should.equal(Not(Atom("rain")))
    NoMatch(reason) -> panic as { "Expected Applied, got NoMatch: " <> reason }
    _ -> panic as "Unexpected result"
  }
}

pub fn hypothetical_syllogism_test() {
  let rule = hypothetical_syllogism()

  // Apply: p → q, q → r ⊢ p → r
  let premises = [
    Implies(Atom("a"), Atom("b")),
    Implies(Atom("b"), Atom("c")),
  ]

  case apply_rule(rule, premises) {
    Applied(conclusion, _bindings) ->
      conclusion |> should.equal(Implies(Atom("a"), Atom("c")))
    NoMatch(reason) -> panic as { "Expected Applied, got NoMatch: " <> reason }
    _ -> panic as "Unexpected result"
  }
}

pub fn modal_modus_ponens_test() {
  let rule = modal_modus_ponens()

  // Apply: □(p → q), □p ⊢ □q
  let premises = [
    Necessary(Implies(Atom("rain"), Atom("wet"))),
    Necessary(Atom("rain")),
  ]

  case apply_rule(rule, premises) {
    Applied(conclusion, _bindings) ->
      conclusion |> should.equal(Necessary(Atom("wet")))
    NoMatch(reason) -> panic as { "Expected Applied, got NoMatch: " <> reason }
    _ -> panic as "Unexpected result"
  }
}

pub fn t_axiom_application_test() {
  let rule = t_axiom_application()

  // Apply T axiom: □p ⊢ p
  let premises = [Necessary(Atom("truth"))]

  case apply_rule(rule, premises) {
    Applied(conclusion, _bindings) -> conclusion |> should.equal(Atom("truth"))
    NoMatch(reason) -> panic as { "Expected Applied, got NoMatch: " <> reason }
    _ -> panic as "Unexpected result"
  }
}

pub fn four_axiom_application_test() {
  let rule = four_axiom_application()

  // Apply 4 axiom: □p ⊢ □□p
  let premises = [Necessary(Atom("truth"))]

  case apply_rule(rule, premises) {
    Applied(conclusion, _bindings) ->
      conclusion |> should.equal(Necessary(Necessary(Atom("truth"))))
    NoMatch(reason) -> panic as { "Expected Applied, got NoMatch: " <> reason }
    _ -> panic as "Unexpected result"
  }
}

pub fn rule_no_match_wrong_structure_test() {
  let rule = modus_ponens()

  // Wrong structure - both premises are atoms
  let premises = [Atom("p"), Atom("q")]

  case apply_rule(rule, premises) {
    NoMatch(_) -> should.be_true(True)
    _ -> panic as "Expected NoMatch for wrong premise structure"
  }
}

pub fn rule_no_match_wrong_count_test() {
  let rule = modus_ponens()

  // Wrong number of premises
  let premises = [Atom("p")]

  case apply_rule(rule, premises) {
    NoMatch(reason) -> reason |> should.equal("Expected 2 premises, got 1")
    _ -> panic as "Expected NoMatch for wrong premise count"
  }
}

// ============ All Rules Test ============

pub fn all_rules_not_empty_test() {
  let rules = all_rules()

  rules
  |> should.not_equal([])
}

pub fn all_rules_have_ids_test() {
  let rules = all_rules()

  rules
  |> list.all(fn(rule) { rule.id != "" })
  |> should.be_true
}

// ============ Axiom Tests ============

pub fn k_axiom_test() {
  let ax = axiom.k_axiom()

  ax.id |> should.equal("K")
  ax.name |> should.equal("Distribution Axiom")
  ax.frame_property |> should.equal(None)
}

pub fn t_axiom_test() {
  let ax = axiom.t_axiom()

  ax.id |> should.equal("T")
  ax.name |> should.equal("Reflexivity Axiom")
  ax.frame_property |> should.equal(Some(axiom.Reflexive))
}

pub fn axioms_for_system_test() {
  // S5 includes K, T, 4, 5, B axioms
  let s5_axioms = axiom.axioms_for_system(S5)

  // Should include K
  s5_axioms
  |> list.any(fn(ax) { ax.id == "K" })
  |> should.be_true

  // Should include T
  s5_axioms
  |> list.any(fn(ax) { ax.id == "T" })
  |> should.be_true

  // Should include 4
  s5_axioms
  |> list.any(fn(ax) { ax.id == "4" })
  |> should.be_true

  // Should include 5
  s5_axioms
  |> list.any(fn(ax) { ax.id == "5" })
  |> should.be_true
}

pub fn frame_properties_for_s5_test() {
  let props = axiom.frame_properties_for_system(S5)

  // S5 should have reflexive, transitive, symmetric, euclidean
  props |> list.contains(axiom.Reflexive) |> should.be_true
  props |> list.contains(axiom.Transitive) |> should.be_true
  props |> list.contains(axiom.Symmetric) |> should.be_true
  props |> list.contains(axiom.Euclidean) |> should.be_true
}

pub fn get_axiom_by_id_test() {
  axiom.get_axiom_by_id("K")
  |> should.be_some
  |> fn(ax) { ax.id }
  |> should.equal("K")

  axiom.get_axiom_by_id("UNKNOWN")
  |> should.be_none
}

// Helper import needed
import gleam/list
