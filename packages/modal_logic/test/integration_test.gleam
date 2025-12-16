//// Integration Tests
////
//// Comprehensive integration tests ensuring all components of the
//// validation testing suite work together correctly.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import modal_logic/proposition.{Atom, Implies, K, Necessary, S4, S5, T}
import modal_logic/rules/axiom
import modal_logic/rules/inference_rule.{
  type Bindings, AnyAtom, InferenceRule, PatternNecessary, apply_rule,
  match_pattern,
}
import modal_logic/rules/rule_builder.{
  hypothetical_syllogism, modal_modus_ponens, modus_ponens, modus_tollens,
  necessitation,
}
import modal_logic/rules/rule_store
import modal_logic/testing/docs/doc_generator
import modal_logic/testing/external/folio_adapter
import modal_logic/testing/external/sep_adapter
import modal_logic/testing/validation/argument_corpus
import modal_logic/testing/validation/philosophical_tester
import modal_logic/testing/validation/soundness_checker

// ============ End-to-End Workflow Tests ============

/// Test the complete workflow: build rules -> store -> test -> document
pub fn complete_workflow_test() {
  // 1. Create a rule store with standard rules
  let store = rule_store.standard_store()

  // 2. Verify rules are in the store
  let rules = rule_store.list_rules(store)
  rules
  |> list.length
  |> should.not_equal(0)

  // 3. Run philosophical tests
  let config = philosophical_tester.default_config()
  let test_result = philosophical_tester.run_tests(store, config)

  // 4. Verify test results
  test_result.total_tested
  |> should.not_equal(0)

  // 5. Generate documentation
  let doc_config = doc_generator.default_config()
  let doc = doc_generator.generate_store_doc(store, doc_config)

  // 6. Verify documentation
  doc.title
  |> should.not_equal("")

  doc.sections
  |> list.length
  |> should.not_equal(0)
}

/// Test rule builder to rule application pipeline
pub fn rule_builder_to_application_test() {
  // Build modus ponens rule
  let mp = modus_ponens()

  // Create propositions to apply rule
  // Modus ponens pattern: p, p -> q |- q
  let p = Atom("rain")
  let q = Atom("wet")
  let implication = Implies(p, q)

  // Apply the rule (order matters: p first, then p -> q)
  let result = apply_rule(mp, [p, implication])

  // Verify application succeeded
  case result {
    inference_rule.Applied(conclusion, _bindings) ->
      conclusion
      |> should.equal(q)
    inference_rule.NoMatch(_) -> should.fail()
    inference_rule.DerivationFailed(_) -> should.fail()
  }
}

/// Test rule store with versioning workflow
pub fn rule_store_versioning_workflow_test() {
  // Create store and add a rule
  let store = rule_store.new()
  let mp = modus_ponens()

  // Add rule
  let assert Ok(store) = rule_store.add_rule(store, mp)

  // Verify rule exists
  let assert Ok(retrieved) = rule_store.get_rule(store, mp.id)
  retrieved.name
  |> should.equal(mp.name)

  // Update rule (modify metadata)
  let updated_mp =
    InferenceRule(
      ..mp,
      metadata: inference_rule.RuleMetadata(
        ..mp.metadata,
        author: Some("Updated Author"),
      ),
    )
  let assert Ok(store) =
    rule_store.update_rule(store, updated_mp, Some("Added author"))

  // Verify version history
  let assert Ok(versioned) = rule_store.get_versioned_rule(store, mp.id)
  versioned.version
  |> should.equal(2)

  // Retrieve historical version
  let assert Ok(v1) = rule_store.get_rule_version(store, mp.id, 1)
  v1.metadata.author
  |> should.equal(None)
}

// ============ External Data Integration Tests ============

/// Test SEP adapter produces valid fixtures
pub fn sep_adapter_fixtures_test() {
  let fixtures = sep_adapter.get_mock_fixtures()

  fixtures
  |> list.length
  |> should.not_equal(0)

  // Verify fixtures have required fields
  let first = case fixtures {
    [f, ..] -> f
    [] -> panic as "Expected fixtures"
  }

  first.id
  |> string.is_empty
  |> should.be_false

  first.name
  |> string.is_empty
  |> should.be_false
}

/// Test FOLIO adapter produces valid fixtures
pub fn folio_adapter_fixtures_test() {
  let config = folio_adapter.default_config()
  let fixtures = folio_adapter.get_all_fixtures(config)

  fixtures
  |> list.length
  |> should.not_equal(0)
}

/// Test argument corpus is complete
pub fn argument_corpus_completeness_test() {
  let modal = argument_corpus.modal_arguments()
  let epistemic = argument_corpus.epistemic_arguments()
  let deontic = argument_corpus.deontic_arguments()
  let classical = argument_corpus.classical_arguments()
  let fallacy = argument_corpus.fallacy_arguments()
  let all = argument_corpus.all_arguments()

  // All categories should be non-empty
  modal
  |> list.length
  |> should.not_equal(0)

  epistemic
  |> list.length
  |> should.not_equal(0)

  deontic
  |> list.length
  |> should.not_equal(0)

  classical
  |> list.length
  |> should.not_equal(0)

  fallacy
  |> list.length
  |> should.not_equal(0)

  // All arguments should include content from all categories
  // (may have additional arguments, so just verify minimum)
  let category_total =
    list.length(modal)
    + list.length(epistemic)
    + list.length(deontic)
    + list.length(classical)
    + list.length(fallacy)

  // all_arguments should have at least as many as the sum of named categories
  { list.length(all) >= category_total }
  |> should.be_true
}

// ============ Validation Pipeline Tests ============

/// Test soundness checking pipeline
pub fn soundness_checking_pipeline_test() {
  let store = rule_store.standard_store()

  // Run soundness check
  let result = soundness_checker.check_store_soundness(store)

  // All standard rules should be sound
  result.sound_rules
  |> should.equal(result.rules_checked)

  result.problematic_rules
  |> should.equal(0)
}

/// Test rule-argument validation
pub fn rule_argument_validation_test() {
  let store = rule_store.standard_store()

  // Test with specific config
  let config =
    philosophical_tester.PhilosophicalTestConfig(
      ..philosophical_tester.default_config(),
      systems: Some([K, T, S4, S5]),
      max_arguments: Some(10),
    )

  let result = philosophical_tester.run_tests(store, config)

  // Should have tested some arguments
  result.total_tested
  |> should.not_equal(0)
}

// ============ Documentation Integration Tests ============

/// Test documentation generation from test results
pub fn doc_generation_from_tests_test() {
  let store = rule_store.standard_store()
  let test_config = philosophical_tester.default_config()
  let test_result = philosophical_tester.run_tests(store, test_config)

  let doc_config = doc_generator.comprehensive_config()
  let doc = doc_generator.generate_test_doc(test_result, doc_config)

  doc.title
  |> should.equal("Modal Logic Test Results")

  let rendered = doc_generator.render(doc)
  rendered
  |> string.contains("Test Summary")
  |> should.be_true
}

/// Test documentation generation from soundness analysis
pub fn doc_generation_from_soundness_test() {
  let store = rule_store.standard_store()
  let soundness_result = soundness_checker.check_store_soundness(store)

  let doc_config = doc_generator.default_config()
  let doc = doc_generator.generate_soundness_doc(soundness_result, doc_config)

  doc.title
  |> should.equal("Soundness Analysis Report")

  let rendered = doc_generator.render(doc)
  rendered
  |> string.contains("Soundness Summary")
  |> should.be_true
}

// ============ Pattern Matching Integration Tests ============

/// Test pattern matching across different rule types
pub fn pattern_matching_integration_test() {
  let rules = [
    modus_ponens(),
    modus_tollens(),
    hypothetical_syllogism(),
    necessitation(),
    modal_modus_ponens(),
  ]

  // Each rule should have valid patterns
  rules
  |> list.each(fn(rule) {
    rule.premise_patterns
    |> list.length
    |> should.not_equal(0)
  })
}

/// Test pattern binding propagation
pub fn pattern_binding_propagation_test() {
  // Pattern: □p
  let pattern = PatternNecessary(AnyAtom("p"))
  let prop = Necessary(Atom("truth"))

  let assert Some(bindings) = match_pattern(pattern, prop, dict.new())

  // Should have bound "p" -> Atom("truth")
  let assert Ok(bound) = dict.get(bindings, "p")
  bound
  |> should.equal(Atom("truth"))
}

// ============ Cross-Component Tests ============

/// Test axiom store and validation
pub fn axiom_store_validation_test() {
  let store = rule_store.standard_store()
  let axioms = rule_store.list_axioms(store)

  // Should have standard axioms
  axioms
  |> list.length
  |> should.not_equal(0)

  // Verify axiom K is present
  let k_axiom =
    axioms
    |> list.filter(fn(ax) { ax.id == "K" })

  k_axiom
  |> list.length
  |> should.equal(1)

  // K axiom should be in all systems
  case k_axiom {
    [ax] ->
      ax.included_in
      |> list.contains(K)
      |> should.be_true
    _ -> should.fail()
  }
}

/// Test rule sets grouping
pub fn rule_sets_integration_test() {
  let store = rule_store.standard_store()

  // Create a rule set
  let assert Ok(store) =
    rule_store.create_rule_set(
      store,
      "test_set",
      "Test Rule Set",
      "A test rule set for integration testing",
    )

  // Should be able to retrieve the created set
  let assert Ok(test_set) = rule_store.get_rule_set(store, "test_set")

  test_set.name
  |> should.equal("Test Rule Set")

  test_set.description
  |> should.equal("A test rule set for integration testing")
}

// ============ Export/Import Tests ============

/// Test store export functionality
pub fn store_export_test() {
  let store = rule_store.standard_store()

  // Export store
  let export = rule_store.export(store)

  // Should have rules
  export.rules
  |> list.length
  |> should.not_equal(0)

  // Should have axioms
  export.axioms
  |> list.length
  |> should.not_equal(0)

  // Export summary should not be empty
  let summary = rule_store.export_summary(export)
  summary
  |> string.is_empty
  |> should.be_false
}

// ============ Error Handling Tests ============

/// Test graceful handling of missing rules
pub fn missing_rule_handling_test() {
  let store = rule_store.new()

  let result = rule_store.get_rule(store, "nonexistent")

  case result {
    Ok(_) -> should.fail()
    Error(rule_store.RuleNotFound(_)) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

/// Test graceful handling of missing axioms
pub fn missing_axiom_handling_test() {
  let store = rule_store.new()

  let result = rule_store.get_axiom(store, "nonexistent")

  case result {
    Ok(_) -> should.fail()
    Error(rule_store.AxiomNotFound(_)) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

/// Test duplicate rule handling
pub fn duplicate_rule_handling_test() {
  let store = rule_store.new()
  let mp = modus_ponens()

  // Add rule first time
  let assert Ok(store) = rule_store.add_rule(store, mp)

  // Try to add again
  let result = rule_store.add_rule(store, mp)

  case result {
    Ok(_) -> should.fail()
    Error(rule_store.DuplicateId(id: _, entity_type: _)) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

// ============ Data Consistency Tests ============

/// Test that all arguments have valid propositions
pub fn argument_proposition_validity_test() {
  let all_args = argument_corpus.all_arguments()

  all_args
  |> list.each(fn(arg) {
    // Each argument should have at least one premise
    arg.premises
    |> list.length
    |> should.not_equal(0)

    // Conclusion should not be empty (check it's an Atom with content)
    case arg.conclusion {
      Atom(name) ->
        name
        |> string.is_empty
        |> should.be_false
      _ -> should.be_true(True)
    }
  })
}

/// Test that rule patterns are consistent
pub fn rule_pattern_consistency_test() {
  let rules = rule_builder.all_rules()

  rules
  |> list.each(fn(rule) {
    // Each rule should be valid in at least one system
    rule.valid_in
    |> list.length
    |> should.not_equal(0)

    // ID should not be empty
    rule.id
    |> string.is_empty
    |> should.be_false

    // Name should not be empty
    rule.name
    |> string.is_empty
    |> should.be_false
  })
}

/// Test axiom frame property consistency
pub fn axiom_frame_property_consistency_test() {
  let _axioms = axiom.all_standard_axioms()

  // T axiom should have reflexive property
  let t = axiom.t_axiom()
  t.frame_property
  |> should.equal(Some(axiom.Reflexive))

  // 4 axiom should have transitive property
  let four = axiom.axiom_4()
  four.frame_property
  |> should.equal(Some(axiom.Transitive))

  // 5 axiom should have euclidean property
  let five = axiom.axiom_5()
  five.frame_property
  |> should.equal(Some(axiom.Euclidean))
}
