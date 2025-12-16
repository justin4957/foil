//// Tests for Rule Store
////
//// This module tests the rule store with versioning functionality.

import gleam/dict
import gleam/list
import gleeunit/should
import modal_logic/proposition.{S5, T}
import modal_logic/rules/axiom
import modal_logic/rules/inference_rule.{InferenceRule, default_metadata}
import modal_logic/rules/rule_builder
import modal_logic/rules/rule_store.{
  AxiomNotFound, DuplicateId, RuleNotFound, RuleSetNotFound,
}

// ============ Store Creation Tests ============

pub fn new_store_test() {
  let store = rule_store.new()

  store.metadata.name |> should.equal("default")
  store.metadata.total_rules |> should.equal(0)
  store.metadata.active_rules |> should.equal(0)
}

pub fn new_named_store_test() {
  let store = rule_store.new_named("test_store")

  store.metadata.name |> should.equal("test_store")
}

// ============ Rule Operations Tests ============

pub fn add_rule_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let result = rule_store.add_rule(store, rule)

  case result {
    Ok(new_store) -> {
      new_store.metadata.total_rules |> should.equal(1)
      new_store.metadata.active_rules |> should.equal(1)
    }
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn add_duplicate_rule_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)
  let result = rule_store.add_rule(store2, rule)

  case result {
    Ok(_) -> panic as "Expected duplicate error"
    Error(DuplicateId(id, entity_type)) -> {
      id |> should.equal("modus_ponens")
      entity_type |> should.equal("rule")
    }
    Error(_) -> panic as "Expected DuplicateId error"
  }
}

pub fn get_rule_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)

  case rule_store.get_rule(store2, "modus_ponens") {
    Ok(retrieved) -> retrieved.id |> should.equal("modus_ponens")
    Error(_) -> panic as "Expected to find rule"
  }
}

pub fn get_nonexistent_rule_test() {
  let store = rule_store.new()

  case rule_store.get_rule(store, "nonexistent") {
    Ok(_) -> panic as "Expected RuleNotFound"
    Error(RuleNotFound(id)) -> id |> should.equal("nonexistent")
    Error(_) -> panic as "Expected RuleNotFound error"
  }
}

pub fn update_rule_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)

  // Create updated version
  let updated = InferenceRule(..rule, description: "Updated description")

  let result =
    rule_store.update_rule(store2, updated, option.Some("Updated description"))

  case result {
    Ok(store3) -> {
      let assert Ok(versioned) =
        rule_store.get_versioned_rule(store3, "modus_ponens")
      versioned.version |> should.equal(2)
      versioned.rule.description |> should.equal("Updated description")
      list.length(versioned.history) |> should.equal(1)
    }
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn delete_rule_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)
  let assert Ok(store3) = rule_store.delete_rule(store2, "modus_ponens")

  // Rule should not be accessible
  case rule_store.get_rule(store3, "modus_ponens") {
    Ok(_) -> panic as "Expected deleted rule to be inaccessible"
    Error(RuleNotFound(_)) -> should.be_true(True)
    Error(_) -> panic as "Expected RuleNotFound"
  }

  // But versioned entry should exist
  case rule_store.get_versioned_rule(store3, "modus_ponens") {
    Ok(versioned) -> versioned.is_active |> should.equal(False)
    Error(_) -> panic as "Expected versioned entry to exist"
  }
}

pub fn restore_rule_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)
  let assert Ok(store3) = rule_store.delete_rule(store2, "modus_ponens")
  let assert Ok(store4) = rule_store.restore_rule(store3, "modus_ponens")

  // Rule should be accessible again
  case rule_store.get_rule(store4, "modus_ponens") {
    Ok(retrieved) -> retrieved.id |> should.equal("modus_ponens")
    Error(_) -> panic as "Expected rule to be restored"
  }
}

pub fn get_rule_version_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)

  let updated = InferenceRule(..rule, description: "Version 2")
  let assert Ok(store3) =
    rule_store.update_rule(store2, updated, option.Some("v2"))

  let updated2 = InferenceRule(..rule, description: "Version 3")
  let assert Ok(store4) =
    rule_store.update_rule(store3, updated2, option.Some("v3"))

  // Get current version (3)
  case rule_store.get_rule_version(store4, "modus_ponens", 3) {
    Ok(r) -> r.description |> should.equal("Version 3")
    Error(_) -> panic as "Expected version 3"
  }

  // Get version 2
  case rule_store.get_rule_version(store4, "modus_ponens", 2) {
    Ok(r) -> r.description |> should.equal("Version 2")
    Error(_) -> panic as "Expected version 2"
  }

  // Get version 1 (original)
  case rule_store.get_rule_version(store4, "modus_ponens", 1) {
    Ok(r) -> r.description |> should.equal("From p and p implies q, derive q")
    Error(_) -> panic as "Expected version 1"
  }
}

// ============ List and Query Tests ============

pub fn list_rules_test() {
  let store = rule_store.new()
  let mp = rule_builder.modus_ponens()
  let mt = rule_builder.modus_tollens()

  let assert Ok(store2) = rule_store.add_rule(store, mp)
  let assert Ok(store3) = rule_store.add_rule(store2, mt)

  let rules = rule_store.list_rules(store3)
  list.length(rules) |> should.equal(2)
}

pub fn list_rules_by_tag_test() {
  let store = rule_store.new()
  let mp = rule_builder.modus_ponens()
  // modus_ponens has "classical" and "fundamental" tags
  let modal_mp = rule_builder.modal_modus_ponens()
  // has "modal" tag

  let assert Ok(store2) = rule_store.add_rule(store, mp)
  let assert Ok(store3) = rule_store.add_rule(store2, modal_mp)

  let classical_rules = rule_store.list_rules_by_tag(store3, "classical")
  list.length(classical_rules) |> should.equal(1)

  let modal_rules = rule_store.list_rules_by_tag(store3, "modal")
  list.length(modal_rules) |> should.equal(1)
}

pub fn search_rules_test() {
  let store = rule_store.new()
  let mp = rule_builder.modus_ponens()
  let mt = rule_builder.modus_tollens()

  let assert Ok(store2) = rule_store.add_rule(store, mp)
  let assert Ok(store3) = rule_store.add_rule(store2, mt)

  let results = rule_store.search_rules(store3, "ponens")
  list.length(results) |> should.equal(1)
  let assert [found] = results
  found.id |> should.equal("modus_ponens")
}

// ============ Axiom Tests ============

pub fn add_axiom_test() {
  let store = rule_store.new()
  let ax = axiom.k_axiom()

  let result = rule_store.add_axiom(store, ax)

  case result {
    Ok(new_store) -> {
      new_store.metadata.total_axioms |> should.equal(1)
      new_store.metadata.active_axioms |> should.equal(1)
    }
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn get_axiom_test() {
  let store = rule_store.new()
  let ax = axiom.k_axiom()

  let assert Ok(store2) = rule_store.add_axiom(store, ax)

  case rule_store.get_axiom(store2, "K") {
    Ok(retrieved) -> retrieved.id |> should.equal("K")
    Error(_) -> panic as "Expected to find axiom"
  }
}

pub fn list_axioms_for_system_test() {
  let store = rule_store.new()

  let assert Ok(store2) = rule_store.add_axiom(store, axiom.k_axiom())
  let assert Ok(store3) = rule_store.add_axiom(store2, axiom.t_axiom())
  let assert Ok(store4) = rule_store.add_axiom(store3, axiom.axiom_4())
  let assert Ok(store5) = rule_store.add_axiom(store4, axiom.axiom_5())

  // S5 includes K, T, 4, 5
  let s5_axioms = rule_store.list_axioms_for_system(store5, S5)
  list.length(s5_axioms) |> should.equal(4)

  // T includes only K and T
  let t_axioms = rule_store.list_axioms_for_system(store5, T)
  list.length(t_axioms) |> should.equal(2)
}

// ============ Rule Set Tests ============

pub fn create_rule_set_test() {
  let store = rule_store.new()

  let result =
    rule_store.create_rule_set(
      store,
      "classical",
      "Classical Rules",
      "Standard classical logic rules",
    )

  case result {
    Ok(new_store) -> {
      let assert Ok(set) = rule_store.get_rule_set(new_store, "classical")
      set.name |> should.equal("Classical Rules")
    }
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn add_rule_to_set_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)
  let assert Ok(store3) =
    rule_store.create_rule_set(
      store2,
      "classical",
      "Classical",
      "Classical rules",
    )
  let assert Ok(store4) =
    rule_store.add_rule_to_set(store3, "classical", "modus_ponens")

  case rule_store.get_rules_in_set(store4, "classical") {
    Ok(rules) -> {
      list.length(rules) |> should.equal(1)
      let assert [r] = rules
      r.id |> should.equal("modus_ponens")
    }
    Error(_) -> panic as "Expected rules in set"
  }
}

pub fn add_to_nonexistent_set_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)

  case rule_store.add_rule_to_set(store2, "nonexistent", "modus_ponens") {
    Ok(_) -> panic as "Expected RuleSetNotFound"
    Error(RuleSetNotFound(id)) -> id |> should.equal("nonexistent")
    Error(_) -> panic as "Expected RuleSetNotFound error"
  }
}

// ============ Statistics Tests ============

pub fn statistics_test() {
  let store = rule_store.new()
  let mp = rule_builder.modus_ponens()
  let mt = rule_builder.modus_tollens()

  let assert Ok(store2) = rule_store.add_rule(store, mp)
  let assert Ok(store3) = rule_store.add_rule(store2, mt)
  let assert Ok(store4) = rule_store.add_axiom(store3, axiom.k_axiom())

  let stats = rule_store.statistics(store4)

  stats.total_rules |> should.equal(2)
  stats.active_rules |> should.equal(2)
  stats.total_axioms |> should.equal(1)
  stats.active_axioms |> should.equal(1)
}

pub fn count_rules_by_tag_test() {
  let store = rule_store.new()
  let mp = rule_builder.modus_ponens()
  // has "classical", "fundamental"
  let mt = rule_builder.modus_tollens()
  // has "classical"
  let modal = rule_builder.modal_modus_ponens()
  // has "modal", "k_axiom"

  let assert Ok(store2) = rule_store.add_rule(store, mp)
  let assert Ok(store3) = rule_store.add_rule(store2, mt)
  let assert Ok(store4) = rule_store.add_rule(store3, modal)

  let tag_counts = rule_store.count_rules_by_tag(store4)

  dict.get(tag_counts, "classical") |> should.equal(Ok(2))
  dict.get(tag_counts, "fundamental") |> should.equal(Ok(1))
  dict.get(tag_counts, "modal") |> should.equal(Ok(1))
}

// ============ Export Tests ============

pub fn export_test() {
  let store = rule_store.new_named("test_export")
  let mp = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, mp)
  let assert Ok(store3) = rule_store.add_axiom(store2, axiom.k_axiom())

  let export = rule_store.export(store3)

  export.name |> should.equal("test_export")
  list.length(export.rules) |> should.equal(1)
  list.length(export.axioms) |> should.equal(1)
}

pub fn export_summary_test() {
  let store = rule_store.new_named("summary_test")
  let mp = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, mp)
  let export = rule_store.export(store2)

  let summary = rule_store.export_summary(export)
  summary
  |> should.not_equal("")
}

// ============ Standard Store Tests ============

pub fn standard_store_test() {
  let store = rule_store.standard_store()

  // Should have standard rules and axioms
  let rules = rule_store.list_rules(store)
  rules |> should.not_equal([])

  let axioms = rule_store.list_axioms(store)
  axioms |> should.not_equal([])
}

pub fn with_standard_rules_test() {
  let store =
    rule_store.new()
    |> rule_store.with_standard_rules

  let rules = rule_store.list_rules(store)
  rules |> should.not_equal([])
}

pub fn with_standard_axioms_test() {
  let store =
    rule_store.new()
    |> rule_store.with_standard_axioms

  let axioms = rule_store.list_axioms(store)
  axioms |> should.not_equal([])
}

// Helper import
import gleam/option
