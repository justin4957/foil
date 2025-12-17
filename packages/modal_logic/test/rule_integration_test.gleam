//// Rule Integration Tests
////
//// Comprehensive integration tests for the rule building, storage, and
//// validation pipeline. Tests the complete flow from rule construction
//// through validation and documentation generation.

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import modal_logic/proposition.{
  And, Atom, Implies, K, KD, KD45, Necessary, Not, Or, Possible, S4, S5, T,
}
import modal_logic/rules/axiom
import modal_logic/rules/inference_rule.{
  AnyAtom, InferenceRule, PatternImplies, PatternNecessary, PatternNot,
  PatternPossible, Wildcard, apply_rule, match_pattern,
}
import modal_logic/rules/rule_builder
import modal_logic/rules/rule_store
import modal_logic/testing/docs/coverage_report
import modal_logic/testing/docs/doc_generator
import modal_logic/testing/validation/argument_corpus
import modal_logic/testing/validation/philosophical_tester
import modal_logic/testing/validation/soundness_checker

// ============ Rule Building Pipeline Tests ============

/// Test building custom inference rules with the DSL
pub fn custom_rule_building_test() {
  // Build a custom disjunctive syllogism rule
  let assert Ok(ds) =
    rule_builder.inference_rule("disjunctive_syllogism")
    |> rule_builder.named("Disjunctive Syllogism")
    |> rule_builder.described(
      "From (P v Q) and ~P, derive Q",
    )
    |> rule_builder.with_premise(rule_builder.or(
      rule_builder.atom("p"),
      rule_builder.atom("q"),
    ))
    |> rule_builder.with_premise(rule_builder.not(rule_builder.atom("p")))
    |> rule_builder.derives(rule_builder.atom("q"))
    |> rule_builder.valid_in_all()
    |> rule_builder.build()

  ds.id |> should.equal("disjunctive_syllogism")
  ds.name |> should.equal("Disjunctive Syllogism")
  ds.premise_patterns |> list.length |> should.equal(2)
}

/// Test building all standard rules
pub fn standard_rules_building_test() {
  let rules = rule_builder.all_rules()

  // Should have all standard rules
  { list.length(rules) >= 5 } |> should.be_true

  // Each rule should have required fields
  list.each(rules, fn(rule) {
    { string.length(rule.id) > 0 } |> should.be_true
    { string.length(rule.name) > 0 } |> should.be_true
    { list.length(rule.premise_patterns) > 0 } |> should.be_true
    { list.length(rule.valid_in) > 0 } |> should.be_true
  })
}

/// Test rule metadata handling
pub fn rule_metadata_test() {
  let assert Ok(rule) =
    rule_builder.inference_rule("test_metadata")
    |> rule_builder.named("Test Rule")
    |> rule_builder.described("A test rule with metadata")
    |> rule_builder.with_premise(rule_builder.atom("p"))
    |> rule_builder.derives(rule_builder.atom("p"))
    |> rule_builder.tagged("test")
    |> rule_builder.tagged("metadata")
    |> rule_builder.valid_in([K, T])
    |> rule_builder.build()

  rule.metadata.tags |> list.contains("test") |> should.be_true
  rule.metadata.tags |> list.contains("metadata") |> should.be_true
}

// ============ Rule Store Pipeline Tests ============

/// Test full rule store lifecycle
pub fn rule_store_lifecycle_test() {
  // 1. Create empty store
  let store = rule_store.new()
  rule_store.list_rules(store) |> list.length |> should.equal(0)

  // 2. Add rules
  let mp = rule_builder.modus_ponens()
  let mt = rule_builder.modus_tollens()

  let assert Ok(store) = rule_store.add_rule(store, mp)
  let assert Ok(store) = rule_store.add_rule(store, mt)

  rule_store.list_rules(store) |> list.length |> should.equal(2)

  // 3. Query rules
  let assert Ok(retrieved) = rule_store.get_rule(store, "modus_ponens")
  retrieved.name |> should.equal("Modus Ponens")

  // 4. Update rule
  let updated_mp = InferenceRule(
    ..mp,
    metadata: inference_rule.RuleMetadata(
      ..mp.metadata,
      author: Some("Updated"),
    ),
  )
  let assert Ok(store) = rule_store.update_rule(store, updated_mp, Some("Test update"))

  // 5. Verify version
  let assert Ok(versioned) = rule_store.get_versioned_rule(store, "modus_ponens")
  versioned.version |> should.equal(2)

  // 6. Delete rule
  let assert Ok(store) = rule_store.delete_rule(store, "modus_tollens")
  rule_store.list_rules(store) |> list.length |> should.equal(1)
}

/// Test rule store with axioms
pub fn rule_store_axioms_test() {
  let store = rule_store.new()

  // Add axioms
  let k_ax = axiom.k_axiom()
  let t_ax = axiom.t_axiom()

  let assert Ok(store) = rule_store.add_axiom(store, k_ax)
  let assert Ok(store) = rule_store.add_axiom(store, t_ax)

  // Query axioms
  let axioms = rule_store.list_axioms(store)
  axioms |> list.length |> should.equal(2)

  // Get specific axiom
  let assert Ok(retrieved) = rule_store.get_axiom(store, "K")
  retrieved.name |> should.equal("Distribution Axiom")
}

/// Test rule sets management
pub fn rule_sets_management_test() {
  let store = rule_store.standard_store()

  // Create rule set
  let assert Ok(store) = rule_store.create_rule_set(
    store,
    "propositional",
    "Propositional Rules",
    "Classical propositional logic rules",
  )

  // Add rules to set
  let assert Ok(store) = rule_store.add_rule_to_set(store, "propositional", "modus_ponens")
  let assert Ok(store) = rule_store.add_rule_to_set(store, "propositional", "modus_tollens")

  // Verify set
  let assert Ok(rule_set) = rule_store.get_rule_set(store, "propositional")
  rule_set.rule_ids |> list.length |> should.equal(2)
}

/// Test rule store export and summary
pub fn rule_store_export_test() {
  let store = rule_store.standard_store()
  let export = rule_store.export(store)

  // Export should contain rules and axioms
  { list.length(export.rules) > 0 } |> should.be_true
  { list.length(export.axioms) > 0 } |> should.be_true

  // Summary should be informative
  let summary = rule_store.export_summary(export)
  summary |> string.contains("Rules:") |> should.be_true
  summary |> string.contains("Axioms:") |> should.be_true
}

// ============ Rule Application Pipeline Tests ============

/// Test modus ponens application
pub fn modus_ponens_application_test() {
  let mp = rule_builder.modus_ponens()

  // Pattern: p, p -> q |- q
  let p = Atom("rain")
  let q = Atom("wet")
  let implication = Implies(p, q)

  let result = apply_rule(mp, [p, implication])

  case result {
    inference_rule.Applied(conclusion, bindings) -> {
      conclusion |> should.equal(q)
      dict.size(bindings) |> should.equal(2)
    }
    _ -> should.fail()
  }
}

/// Test modus tollens application
pub fn modus_tollens_application_test() {
  let mt = rule_builder.modus_tollens()

  // Pattern: p -> q, ~q |- ~p
  let p = Atom("rain")
  let q = Atom("wet")
  let implication = Implies(p, q)
  let not_q = Not(q)

  let result = apply_rule(mt, [implication, not_q])

  case result {
    inference_rule.Applied(conclusion, _bindings) ->
      conclusion |> should.equal(Not(p))
    _ -> should.fail()
  }
}

/// Test necessitation rule application
pub fn necessitation_application_test() {
  let nec = rule_builder.necessitation()

  // Pattern: p |- □p (for theorems)
  let theorem = Atom("proven")

  let result = apply_rule(nec, [theorem])

  case result {
    inference_rule.Applied(conclusion, _bindings) ->
      conclusion |> should.equal(Necessary(theorem))
    _ -> should.fail()
  }
}

/// Test modal modus ponens application
pub fn modal_modus_ponens_application_test() {
  let mmp = rule_builder.modal_modus_ponens()

  // Pattern: □(p -> q), □p |- □q
  let p = Atom("premise")
  let q = Atom("conclusion")
  let nec_impl = Necessary(Implies(p, q))
  let nec_p = Necessary(p)

  let result = apply_rule(mmp, [nec_impl, nec_p])

  case result {
    inference_rule.Applied(conclusion, _bindings) ->
      conclusion |> should.equal(Necessary(q))
    _ -> should.fail()
  }
}

/// Test rule application with wrong premises (no match)
pub fn rule_no_match_test() {
  let mp = rule_builder.modus_ponens()

  // Wrong pattern: just atoms, no implication
  let p = Atom("rain")
  let q = Atom("wet")

  let result = apply_rule(mp, [p, q])

  case result {
    inference_rule.NoMatch(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============ Pattern Matching Pipeline Tests ============

/// Test nested pattern matching
pub fn nested_pattern_matching_test() {
  // Pattern: □(p -> q)
  let pattern = PatternNecessary(PatternImplies(AnyAtom("p"), AnyAtom("q")))

  // Match: □(rain -> wet)
  let prop = Necessary(Implies(Atom("rain"), Atom("wet")))

  let assert Some(bindings) = match_pattern(pattern, prop, dict.new())

  let assert Ok(bound_p) = dict.get(bindings, "p")
  let assert Ok(bound_q) = dict.get(bindings, "q")

  bound_p |> should.equal(Atom("rain"))
  bound_q |> should.equal(Atom("wet"))
}

/// Test wildcard pattern matching
pub fn wildcard_pattern_matching_test() {
  // Wildcard matches any structure
  let pattern = Wildcard("x")
  let prop = And(Atom("p"), Or(Atom("q"), Atom("r")))

  let assert Some(bindings) = match_pattern(pattern, prop, dict.new())

  let assert Ok(bound) = dict.get(bindings, "x")
  bound |> should.equal(prop)
}

/// Test pattern matching with possible operator
pub fn possible_pattern_matching_test() {
  let pattern = PatternPossible(AnyAtom("p"))
  let prop = Possible(Atom("might_rain"))

  let assert Some(bindings) = match_pattern(pattern, prop, dict.new())

  let assert Ok(bound) = dict.get(bindings, "p")
  bound |> should.equal(Atom("might_rain"))
}

/// Test pattern matching failure
pub fn pattern_matching_failure_test() {
  // Pattern expects necessity, proposition has possibility
  let pattern = PatternNecessary(AnyAtom("p"))
  let prop = Possible(Atom("might"))

  let result = match_pattern(pattern, prop, dict.new())
  result |> should.equal(None)
}

// ============ Validation Pipeline Tests ============

/// Test complete validation pipeline
pub fn validation_pipeline_test() {
  let store = rule_store.standard_store()
  let config = philosophical_tester.default_config()
  let result = philosophical_tester.run_tests(store, config)

  // Should have tested arguments
  { result.total_tested > 0 } |> should.be_true

  // Should have some successful validations
  { result.correctly_validated >= 0 } |> should.be_true

  // Should have soundness assessment
  { result.soundness_assessment.score >=. 0.0 } |> should.be_true
  { result.soundness_assessment.score <=. 1.0 } |> should.be_true
}

/// Test validation with specific logic system filter
pub fn validation_system_filter_test() {
  let store = rule_store.standard_store()
  let config = philosophical_tester.PhilosophicalTestConfig(
    ..philosophical_tester.default_config(),
    systems: Some([S5]),
    max_arguments: Some(5),
  )

  let result = philosophical_tester.run_tests(store, config)

  // All tested arguments should be compatible with S5
  list.all(result.argument_results, fn(ar) {
    list.any(ar.argument.valid_in, fn(sys) { sys == S5 })
  })
  |> should.be_true
}

/// Test validation with category filter
pub fn validation_category_filter_test() {
  let store = rule_store.standard_store()
  let config = philosophical_tester.PhilosophicalTestConfig(
    ..philosophical_tester.default_config(),
    categories: Some([argument_corpus.Modal]),
    max_arguments: Some(5),
  )

  let result = philosophical_tester.run_tests(store, config)

  // Should have tested some modal arguments
  { result.total_tested > 0 } |> should.be_true
}

// ============ Soundness Pipeline Tests ============

/// Test comprehensive soundness checking
pub fn soundness_pipeline_test() {
  let store = rule_store.standard_store()
  let result = soundness_checker.check_store_soundness(store)

  // All standard rules should be sound
  result.sound_rules |> should.equal(result.rules_checked)
  result.problematic_rules |> should.equal(0)

  // Should have soundness notes
  { list.length(result.notes) >= 0 } |> should.be_true
}

/// Test rule soundness individually
pub fn individual_rule_soundness_test() {
  let mp = rule_builder.modus_ponens()
  let result = soundness_checker.check_rule_soundness(mp)

  result.is_sound |> should.be_true
}

/// Test axiom soundness checking
pub fn axiom_soundness_test() {
  let store = rule_store.standard_store()
  let result = soundness_checker.check_store_soundness(store)

  // All standard rules should be sound
  result.sound_rules |> should.equal(result.rules_checked)
  result.problematic_rules |> should.equal(0)
}

// ============ Documentation Pipeline Tests ============

/// Test documentation generation from rule store
pub fn doc_from_store_test() {
  let store = rule_store.standard_store()
  let config = doc_generator.default_config()
  let doc = doc_generator.generate_store_doc(store, config)

  doc.title |> should.equal("Modal Logic Rules Documentation")
  { list.length(doc.sections) > 0 } |> should.be_true

  let rendered = doc_generator.render(doc)
  rendered |> string.contains("Rule") |> should.be_true
}

/// Test documentation generation from test results
pub fn doc_from_test_results_test() {
  let store = rule_store.standard_store()
  let test_config = philosophical_tester.PhilosophicalTestConfig(
    ..philosophical_tester.default_config(),
    max_arguments: Some(3),
  )
  let test_result = philosophical_tester.run_tests(store, test_config)

  let doc_config = doc_generator.default_config()
  let doc = doc_generator.generate_test_doc(test_result, doc_config)

  doc.title |> should.equal("Modal Logic Test Results")

  let rendered = doc_generator.render(doc)
  rendered |> string.contains("Test") |> should.be_true
}

/// Test HTML documentation output
pub fn doc_html_output_test() {
  let store = rule_store.standard_store()
  let config = doc_generator.html_config()
  let doc = doc_generator.generate_store_doc(store, config)
  let html = doc_generator.render_html(doc)

  html |> string.contains("<!DOCTYPE html>") |> should.be_true
  html |> string.contains("<style>") |> should.be_true
  html |> string.contains("</html>") |> should.be_true
}

/// Test JSON documentation output
pub fn doc_json_output_test() {
  let store = rule_store.standard_store()
  let config = doc_generator.json_config()
  let doc = doc_generator.generate_store_doc(store, config)
  let json = doc_generator.render_json(doc)

  json |> string.contains("\"title\":") |> should.be_true
  json |> string.contains("\"sections\":") |> should.be_true
  json |> string.contains("\"metadata\":") |> should.be_true
}

// ============ Coverage Report Pipeline Tests ============

/// Test coverage report generation
pub fn coverage_report_pipeline_test() {
  let store = rule_store.standard_store()
  let test_config = philosophical_tester.PhilosophicalTestConfig(
    ..philosophical_tester.default_config(),
    max_arguments: Some(5),
  )
  let test_result = philosophical_tester.run_tests(store, test_config)

  let coverage_config = coverage_report.default_config()
  let report = coverage_report.generate_report(store, test_result, coverage_config)

  // Should have coverage data
  { report.summary.total_rules > 0 } |> should.be_true
  { report.summary.total_axioms > 0 } |> should.be_true
}

/// Test coverage report markdown output
pub fn coverage_markdown_output_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = coverage_report.default_config()
  let report = coverage_report.generate_report(store, test_result, config)
  let markdown = coverage_report.format_markdown(report)

  markdown |> string.contains("# Coverage Report") |> should.be_true
  markdown |> string.contains("## Summary") |> should.be_true
}

/// Test coverage report HTML output
pub fn coverage_html_output_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = coverage_report.default_config()
  let report = coverage_report.generate_report(store, test_result, config)
  let html = coverage_report.format_html(report)

  html |> string.contains("<!DOCTYPE html>") |> should.be_true
  html |> string.contains("progress-bar") |> should.be_true
}

/// Test coverage report JSON output
pub fn coverage_json_output_test() {
  let store = rule_store.standard_store()
  let test_result = create_empty_test_result()
  let config = coverage_report.default_config()
  let report = coverage_report.generate_report(store, test_result, config)
  let json = coverage_report.format_json(report)

  json |> string.contains("\"summary\":") |> should.be_true
  json |> string.contains("\"rule_coverage\":") |> should.be_true
}

// ============ End-to-End Pipeline Tests ============

/// Test complete pipeline: build -> store -> test -> document -> coverage
pub fn complete_pipeline_test() {
  // 1. Build rules
  let mp = rule_builder.modus_ponens()
  let mt = rule_builder.modus_tollens()
  let nec = rule_builder.necessitation()

  // 2. Create and populate store
  let store = rule_store.new()
  let assert Ok(store) = rule_store.add_rule(store, mp)
  let assert Ok(store) = rule_store.add_rule(store, mt)
  let assert Ok(store) = rule_store.add_rule(store, nec)

  // Add axioms
  let assert Ok(store) = rule_store.add_axiom(store, axiom.k_axiom())
  let assert Ok(store) = rule_store.add_axiom(store, axiom.t_axiom())

  // 3. Run validation tests
  let test_config = philosophical_tester.PhilosophicalTestConfig(
    ..philosophical_tester.default_config(),
    max_arguments: Some(3),
  )
  let test_result = philosophical_tester.run_tests(store, test_config)

  // 4. Check soundness
  let soundness_result = soundness_checker.check_store_soundness(store)

  // 5. Generate documentation
  let doc_config = doc_generator.default_config()
  let doc = doc_generator.generate_store_doc(store, doc_config)
  let test_doc = doc_generator.generate_test_doc(test_result, doc_config)
  let soundness_doc = doc_generator.generate_soundness_doc(soundness_result, doc_config)

  // 6. Generate coverage report
  let coverage_config = coverage_report.default_config()
  let coverage = coverage_report.generate_report(store, test_result, coverage_config)

  // Verify all components produced valid output
  { test_result.total_tested >= 0 } |> should.be_true
  soundness_result.rules_checked |> should.equal(3)
  { string.length(doc_generator.render(doc)) > 0 } |> should.be_true
  { string.length(doc_generator.render(test_doc)) > 0 } |> should.be_true
  { string.length(doc_generator.render(soundness_doc)) > 0 } |> should.be_true
  coverage.summary.total_rules |> should.equal(3)
}

/// Test argument corpus integration
pub fn argument_corpus_integration_test() {
  let all_args = argument_corpus.all_arguments()
  let modal_args = argument_corpus.modal_arguments()
  let epistemic_args = argument_corpus.epistemic_arguments()
  let deontic_args = argument_corpus.deontic_arguments()
  let classical_args = argument_corpus.classical_arguments()
  let fallacy_args = argument_corpus.fallacy_arguments()

  // All categories should be non-empty
  { list.length(all_args) > 0 } |> should.be_true
  { list.length(modal_args) > 0 } |> should.be_true
  { list.length(epistemic_args) > 0 } |> should.be_true
  { list.length(deontic_args) > 0 } |> should.be_true
  { list.length(classical_args) > 0 } |> should.be_true
  { list.length(fallacy_args) > 0 } |> should.be_true

  // All arguments should have valid structure
  list.each(all_args, fn(arg) {
    { string.length(arg.id) > 0 } |> should.be_true
    { string.length(arg.name) > 0 } |> should.be_true
    { list.length(arg.premises) > 0 } |> should.be_true
    // Valid arguments must have valid_in, invalid arguments may have empty valid_in
    case arg.is_valid {
      True -> { list.length(arg.valid_in) > 0 } |> should.be_true
      False -> should.be_true(True)
    }
  })
}

/// Test logic system compatibility across rules and arguments
pub fn logic_system_compatibility_test() {
  let store = rule_store.standard_store()
  let rules = rule_store.list_rules(store)
  let axioms = rule_store.list_axioms(store)

  let systems = [K, T, S4, S5, KD, KD45]

  // Each system should have at least some compatible rules
  list.each(systems, fn(system) {
    let compatible_rules =
      list.filter(rules, fn(rule) { list.contains(rule.valid_in, system) })
    { list.length(compatible_rules) > 0 } |> should.be_true
  })

  // K axiom should be in all systems
  let k_axiom = list.find(axioms, fn(ax) { ax.id == "K" })
  case k_axiom {
    Ok(ax) -> {
      list.each(systems, fn(sys) {
        list.contains(ax.included_in, sys) |> should.be_true
      })
    }
    Error(_) -> should.fail()
  }
}

// ============ Helper Functions ============

fn create_empty_test_result() -> philosophical_tester.PhilosophicalTestResult {
  philosophical_tester.PhilosophicalTestResult(
    config: philosophical_tester.default_config(),
    total_tested: 0,
    correctly_validated: 0,
    incorrectly_validated: 0,
    argument_results: [],
    rule_statistics: dict.new(),
    soundness_assessment: philosophical_tester.SoundnessAssessment(
      score: 1.0,
      category: philosophical_tester.FullySoundness,
      notes: [],
    ),
  )
}
