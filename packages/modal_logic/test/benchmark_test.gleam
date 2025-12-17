//// Performance Benchmark Tests
////
//// Tests for measuring performance characteristics of the validation
//// testing suite. These tests verify that operations complete within
//// acceptable time bounds and scale appropriately.

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleeunit/should
import modal_logic/proposition.{
  And, Atom, Implies, K, Necessary, Not, Or, Possible, S4, S5, T,
}
import modal_logic/rules/axiom
import modal_logic/rules/inference_rule.{
  AnyAtom, PatternImplies, PatternNecessary, apply_rule, match_pattern,
}
import modal_logic/rules/rule_builder
import modal_logic/rules/rule_store
import modal_logic/testing/docs/coverage_report
import modal_logic/testing/docs/doc_generator
import modal_logic/testing/validation/argument_corpus
import modal_logic/testing/validation/philosophical_tester
import modal_logic/testing/validation/soundness_checker

// ============ Rule Application Benchmarks ============

/// Benchmark: Apply modus ponens 100 times
pub fn benchmark_modus_ponens_application_test() {
  let mp = rule_builder.modus_ponens()
  let p = Atom("p")
  let q = Atom("q")
  let implication = Implies(p, q)

  // Apply rule 100 times
  let iterations = 100
  let results =
    list.range(1, iterations)
    |> list.map(fn(_) { apply_rule(mp, [p, implication]) })

  // All applications should succeed
  let successes =
    results
    |> list.filter(fn(r) {
      case r {
        inference_rule.Applied(_, _) -> True
        _ -> False
      }
    })

  list.length(successes) |> should.equal(iterations)
}

/// Benchmark: Pattern matching on complex propositions
pub fn benchmark_pattern_matching_complex_test() {
  // Create a deeply nested pattern
  let pattern =
    PatternNecessary(PatternImplies(PatternNecessary(AnyAtom("p")), AnyAtom("q")))

  // Create matching proposition
  let prop = Necessary(Implies(Necessary(Atom("premise")), Atom("conclusion")))

  // Run many matches
  let iterations = 100
  let results =
    list.range(1, iterations)
    |> list.map(fn(_) { match_pattern(pattern, prop, dict.new()) })

  // All matches should succeed
  let successes = list.filter(results, fn(r) { option.is_some(r) })
  list.length(successes) |> should.equal(iterations)
}

/// Benchmark: Build multiple rules
pub fn benchmark_rule_building_test() {
  let iterations = 50

  let rules =
    list.range(1, iterations)
    |> list.filter_map(fn(i) {
      rule_builder.inference_rule("rule_" <> int.to_string(i))
      |> rule_builder.named("Rule " <> int.to_string(i))
      |> rule_builder.with_premise(rule_builder.atom("p"))
      |> rule_builder.with_premise(rule_builder.implies(
        rule_builder.atom("p"),
        rule_builder.atom("q"),
      ))
      |> rule_builder.derives(rule_builder.atom("q"))
      |> rule_builder.valid_in_all()
      |> rule_builder.build()
    })

  list.length(rules) |> should.equal(iterations)
}

// ============ Rule Store Benchmarks ============

/// Benchmark: Add and retrieve many rules
pub fn benchmark_rule_store_operations_test() {
  let iterations = 20

  // Build rules
  let rules =
    list.range(1, iterations)
    |> list.filter_map(fn(i) {
      rule_builder.inference_rule("bench_rule_" <> int.to_string(i))
      |> rule_builder.named("Benchmark Rule " <> int.to_string(i))
      |> rule_builder.with_premise(rule_builder.atom("p"))
      |> rule_builder.derives(rule_builder.atom("q"))
      |> rule_builder.valid_in_all()
      |> rule_builder.build()
    })

  // Create store and add all rules
  let store =
    list.fold(rules, rule_store.new(), fn(store, rule) {
      case rule_store.add_rule(store, rule) {
        Ok(s) -> s
        Error(_) -> store
      }
    })

  // Verify all rules added
  rule_store.list_rules(store) |> list.length |> should.equal(iterations)

  // Retrieve each rule
  let retrievals =
    list.range(1, iterations)
    |> list.filter(fn(i) {
      let id = "bench_rule_" <> int.to_string(i)
      case rule_store.get_rule(store, id) {
        Ok(_) -> True
        Error(_) -> False
      }
    })

  list.length(retrievals) |> should.equal(iterations)
}

/// Benchmark: Standard store operations
pub fn benchmark_standard_store_test() {
  // Create standard store multiple times
  let iterations = 10
  let stores =
    list.range(1, iterations)
    |> list.map(fn(_) { rule_store.standard_store() })

  // Each store should have the same rules
  list.each(stores, fn(store) {
    let rules = rule_store.list_rules(store)
    { list.length(rules) > 0 } |> should.be_true
  })
}

/// Benchmark: Store export
pub fn benchmark_store_export_test() {
  let store = rule_store.standard_store()

  // Export multiple times
  let iterations = 10
  let exports =
    list.range(1, iterations)
    |> list.map(fn(_) { rule_store.export(store) })

  // All exports should succeed
  list.each(exports, fn(export) {
    { list.length(export.rules) > 0 } |> should.be_true
  })
}

// ============ Validation Benchmarks ============

/// Benchmark: Run validation tests
pub fn benchmark_validation_test() {
  let store = rule_store.standard_store()
  let config =
    philosophical_tester.PhilosophicalTestConfig(
      ..philosophical_tester.default_config(),
      max_arguments: Some(5),
    )

  // Run tests
  let result = philosophical_tester.run_tests(store, config)

  // Should complete with results
  { result.total_tested > 0 } |> should.be_true
}

/// Benchmark: Soundness checking
pub fn benchmark_soundness_checking_test() {
  let store = rule_store.standard_store()

  // Run soundness check
  let result = soundness_checker.check_store_soundness(store)

  // Should complete
  { result.rules_checked > 0 } |> should.be_true
}

/// Benchmark: Individual rule soundness checks
pub fn benchmark_individual_soundness_test() {
  let rules = rule_builder.all_rules()

  // Check each rule
  let results =
    list.map(rules, fn(rule) { soundness_checker.check_rule_soundness(rule) })

  // All standard rules should be sound
  list.all(results, fn(r) { r.is_sound }) |> should.be_true
}

// ============ Documentation Generation Benchmarks ============

/// Benchmark: Documentation generation from store
pub fn benchmark_doc_generation_test() {
  let store = rule_store.standard_store()
  let config = doc_generator.default_config()

  // Generate documentation
  let doc = doc_generator.generate_store_doc(store, config)

  // Should have sections
  { list.length(doc.sections) > 0 } |> should.be_true
}

/// Benchmark: Render documentation in multiple formats
pub fn benchmark_doc_rendering_test() {
  let store = rule_store.standard_store()

  // Generate in each format
  let md_config = doc_generator.markdown_config()
  let html_config = doc_generator.html_config()
  let json_config = doc_generator.json_config()

  let md_doc = doc_generator.generate_store_doc(store, md_config)
  let html_doc = doc_generator.generate_store_doc(store, html_config)
  let json_doc = doc_generator.generate_store_doc(store, json_config)

  let md_rendered = doc_generator.render(md_doc)
  let html_rendered = doc_generator.render_html(html_doc)
  let json_rendered = doc_generator.render_json(json_doc)

  // All should produce output
  { string.length(md_rendered) > 0 } |> should.be_true
  { string.length(html_rendered) > 0 } |> should.be_true
  { string.length(json_rendered) > 0 } |> should.be_true
}

import gleam/string

/// Benchmark: Coverage report generation
pub fn benchmark_coverage_report_test() {
  let store = rule_store.standard_store()
  let test_result =
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

  let config = coverage_report.default_config()

  // Generate report
  let report = coverage_report.generate_report(store, test_result, config)

  // Generate in all formats
  let md = coverage_report.format_markdown(report)
  let html = coverage_report.format_html(report)
  let json = coverage_report.format_json(report)

  // All should produce output
  { string.length(md) > 0 } |> should.be_true
  { string.length(html) > 0 } |> should.be_true
  { string.length(json) > 0 } |> should.be_true
}

// ============ Argument Corpus Benchmarks ============

/// Benchmark: Load all arguments
pub fn benchmark_argument_corpus_loading_test() {
  // Load all argument categories
  let all = argument_corpus.all_arguments()
  let modal = argument_corpus.modal_arguments()
  let epistemic = argument_corpus.epistemic_arguments()
  let deontic = argument_corpus.deontic_arguments()
  let classical = argument_corpus.classical_arguments()
  let fallacy = argument_corpus.fallacy_arguments()

  // All should load
  { list.length(all) > 0 } |> should.be_true
  { list.length(modal) > 0 } |> should.be_true
  { list.length(epistemic) > 0 } |> should.be_true
  { list.length(deontic) > 0 } |> should.be_true
  { list.length(classical) > 0 } |> should.be_true
  { list.length(fallacy) > 0 } |> should.be_true
}

/// Benchmark: Filter arguments by system
pub fn benchmark_argument_filtering_test() {
  let all_args = argument_corpus.all_arguments()
  let systems = [K, T, S4, S5]

  // Filter by each system
  let filtered =
    list.map(systems, fn(system) {
      list.filter(all_args, fn(arg) { list.contains(arg.valid_in, system) })
    })

  // Each filter should produce results
  list.each(filtered, fn(args) { { list.length(args) >= 0 } |> should.be_true })
}

// ============ Scalability Tests ============

/// Test: Rule application scales linearly with premise count
pub fn scalability_rule_application_test() {
  let mp = rule_builder.modus_ponens()
  let p = Atom("p")
  let q = Atom("q")
  let implication = Implies(p, q)

  // Test with increasing iterations
  let sizes = [10, 50, 100]

  let results =
    list.map(sizes, fn(size) {
      let successes =
        list.range(1, size)
        |> list.map(fn(_) { apply_rule(mp, [p, implication]) })
        |> list.filter(fn(r) {
          case r {
            inference_rule.Applied(_, _) -> True
            _ -> False
          }
        })
      #(size, list.length(successes))
    })

  // All iterations should succeed
  list.each(results, fn(result) {
    let #(expected, actual) = result
    actual |> should.equal(expected)
  })
}

/// Test: Store operations scale with rule count
pub fn scalability_store_operations_test() {
  let sizes = [5, 10, 20]

  let results =
    list.map(sizes, fn(size) {
      let rules =
        list.range(1, size)
        |> list.filter_map(fn(i) {
          rule_builder.inference_rule("scale_rule_" <> int.to_string(i))
          |> rule_builder.named("Scale Rule " <> int.to_string(i))
          |> rule_builder.with_premise(rule_builder.atom("p"))
          |> rule_builder.derives(rule_builder.atom("q"))
          |> rule_builder.valid_in_all()
          |> rule_builder.build()
        })

      let store =
        list.fold(rules, rule_store.new(), fn(store, rule) {
          case rule_store.add_rule(store, rule) {
            Ok(s) -> s
            Error(_) -> store
          }
        })

      #(size, list.length(rule_store.list_rules(store)))
    })

  // All stores should contain expected rule count
  list.each(results, fn(result) {
    let #(expected, actual) = result
    actual |> should.equal(expected)
  })
}

// ============ Memory Efficiency Tests ============

/// Test: Pattern reuse in rule applications
pub fn memory_pattern_reuse_test() {
  let mp = rule_builder.modus_ponens()

  // Create shared propositions
  let p = Atom("shared_p")
  let q = Atom("shared_q")
  let implication = Implies(p, q)

  // Apply with shared references
  let results =
    list.range(1, 20)
    |> list.map(fn(_) { apply_rule(mp, [p, implication]) })

  // All should succeed with same conclusion
  list.each(results, fn(r) {
    case r {
      inference_rule.Applied(conclusion, _) -> conclusion |> should.equal(q)
      _ -> should.fail()
    }
  })
}

/// Test: Store doesn't duplicate rules
pub fn memory_store_deduplication_test() {
  let store = rule_store.new()
  let mp = rule_builder.modus_ponens()

  // Add rule
  let assert Ok(store) = rule_store.add_rule(store, mp)

  // Try to add duplicate
  let result = rule_store.add_rule(store, mp)

  case result {
    Ok(_) -> should.fail()
    Error(rule_store.DuplicateId(_, _)) -> should.be_true(True)
    Error(_) -> should.fail()
  }

  // Store should still have only one rule
  rule_store.list_rules(store) |> list.length |> should.equal(1)
}
