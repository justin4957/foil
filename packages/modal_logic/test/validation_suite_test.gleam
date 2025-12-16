//// Tests for Validation Suite
////
//// Tests the philosophical argument corpus, tester, and soundness checker.

import gleam/list
import gleam/option
import gleeunit/should
import modal_logic/proposition.{S5}
import modal_logic/rules/rule_builder
import modal_logic/rules/rule_store
import modal_logic/testing/validation/argument_corpus.{
  Classical, Deontic, Epistemic, Fallacy, Historical, Modal,
}
import modal_logic/testing/validation/philosophical_tester.{
  default_config, format_result, get_failures, run_tests, valid_only_config,
}
import modal_logic/testing/validation/soundness_checker.{
  check_rule_soundness, check_store_soundness, format_batch_result,
  format_soundness_result,
}

// ============ Argument Corpus Tests ============

pub fn all_arguments_not_empty_test() {
  let args = argument_corpus.all_arguments()
  args |> should.not_equal([])
}

pub fn modal_arguments_test() {
  let args = argument_corpus.modal_arguments()
  args |> should.not_equal([])

  // All should be modal category
  args
  |> list.all(fn(arg) { arg.category == Modal })
  |> should.be_true
}

pub fn epistemic_arguments_test() {
  let args = argument_corpus.epistemic_arguments()
  args |> should.not_equal([])

  args
  |> list.all(fn(arg) { arg.category == Epistemic })
  |> should.be_true
}

pub fn deontic_arguments_test() {
  let args = argument_corpus.deontic_arguments()
  args |> should.not_equal([])

  args
  |> list.all(fn(arg) { arg.category == Deontic })
  |> should.be_true
}

pub fn classical_arguments_test() {
  let args = argument_corpus.classical_arguments()
  args |> should.not_equal([])

  args
  |> list.all(fn(arg) { arg.category == Classical })
  |> should.be_true
}

pub fn historical_arguments_test() {
  let args = argument_corpus.historical_arguments()
  args |> should.not_equal([])

  args
  |> list.all(fn(arg) { arg.category == Historical })
  |> should.be_true
}

pub fn fallacy_arguments_test() {
  let args = argument_corpus.fallacy_arguments()
  args |> should.not_equal([])

  // All fallacies should be invalid
  args
  |> list.all(fn(arg) { !arg.is_valid })
  |> should.be_true
}

pub fn valid_arguments_test() {
  let valid = argument_corpus.valid_arguments()
  let all = argument_corpus.all_arguments()

  // Valid count should be less than total
  list.length(valid) |> should.not_equal(list.length(all))

  // All valid arguments should have is_valid = True
  valid
  |> list.all(fn(arg) { arg.is_valid })
  |> should.be_true
}

pub fn invalid_arguments_test() {
  let invalid = argument_corpus.invalid_arguments()

  // All invalid arguments should have is_valid = False
  invalid
  |> list.all(fn(arg) { !arg.is_valid })
  |> should.be_true
}

pub fn arguments_by_category_test() {
  let modal = argument_corpus.arguments_by_category(Modal)
  modal |> should.not_equal([])

  modal
  |> list.all(fn(arg) { arg.category == Modal })
  |> should.be_true
}

pub fn arguments_for_system_test() {
  let s5_args = argument_corpus.arguments_for_system(S5)
  s5_args |> should.not_equal([])

  // All should be valid in S5
  s5_args
  |> list.all(fn(arg) { list.contains(arg.valid_in, S5) })
  |> should.be_true
}

pub fn get_argument_test() {
  case argument_corpus.get_argument("classical_modus_ponens") {
    option.Some(arg) -> {
      arg.id |> should.equal("classical_modus_ponens")
      arg.name |> should.equal("Modus Ponens")
    }
    option.None -> panic as "Expected to find modus ponens argument"
  }
}

pub fn corpus_statistics_test() {
  let stats = argument_corpus.corpus_statistics()

  stats.total_arguments |> should.not_equal(0)
  stats.valid_arguments |> should.not_equal(0)
  stats.invalid_arguments |> should.not_equal(0)

  // Valid + invalid should equal total
  { stats.valid_arguments + stats.invalid_arguments }
  |> should.equal(stats.total_arguments)
}

// ============ Philosophical Tester Tests ============

pub fn default_config_test() {
  let config = default_config()

  config.test_valid |> should.equal(True)
  config.test_invalid |> should.equal(True)
  config.generate_traces |> should.equal(True)
}

pub fn valid_only_config_test() {
  let config = valid_only_config()

  config.test_valid |> should.equal(True)
  config.test_invalid |> should.equal(False)
}

pub fn run_tests_with_standard_store_test() {
  let store = rule_store.standard_store()
  let config =
    philosophical_tester.PhilosophicalTestConfig(
      ..default_config(),
      max_arguments: option.Some(5),
    )

  let result = run_tests(store, config)

  // Should have tested some arguments
  result.total_tested |> should.not_equal(0)

  // Should have a soundness score
  { result.soundness_assessment.score >=. 0.0 } |> should.be_true
  { result.soundness_assessment.score <=. 1.0 } |> should.be_true
}

pub fn run_tests_limited_test() {
  let store = rule_store.standard_store()
  let config =
    philosophical_tester.PhilosophicalTestConfig(
      ..default_config(),
      max_arguments: option.Some(3),
    )

  let result = run_tests(store, config)

  // Should have tested at most 3 arguments
  result.total_tested |> should.not_equal(0)
  { result.total_tested <= 3 } |> should.be_true
}

pub fn format_result_test() {
  let store = rule_store.standard_store()
  let config =
    philosophical_tester.PhilosophicalTestConfig(
      ..default_config(),
      max_arguments: option.Some(2),
    )

  let result = run_tests(store, config)
  let formatted = format_result(result)

  formatted |> should.not_equal("")
}

pub fn get_failures_test() {
  let store = rule_store.standard_store()
  let config =
    philosophical_tester.PhilosophicalTestConfig(
      ..default_config(),
      max_arguments: option.Some(5),
    )

  let result = run_tests(store, config)
  let failures = get_failures(result)

  // Number of failures should match incorrectly_validated
  list.length(failures) |> should.equal(result.incorrectly_validated)
}

// ============ Soundness Checker Tests ============

pub fn check_modus_ponens_soundness_test() {
  let rule = rule_builder.modus_ponens()
  let result = check_rule_soundness(rule)

  result.rule_id |> should.equal("modus_ponens")
  result.is_sound |> should.equal(True)
  result.sound_in |> should.not_equal([])
}

pub fn check_modal_modus_ponens_soundness_test() {
  let rule = rule_builder.modal_modus_ponens()
  let result = check_rule_soundness(rule)

  result.rule_id |> should.equal("modal_modus_ponens")
  result.is_sound |> should.equal(True)
}

pub fn check_store_soundness_test() {
  let store = rule_store.standard_store()
  let result = check_store_soundness(store)

  result.rules_checked |> should.not_equal(0)
  result.sound_rules |> should.not_equal(0)
}

pub fn format_soundness_result_test() {
  let rule = rule_builder.modus_ponens()
  let result = check_rule_soundness(rule)
  let formatted = format_soundness_result(result)

  formatted |> should.not_equal("")
}

pub fn format_batch_result_test() {
  let store = rule_store.standard_store()
  let result = check_store_soundness(store)
  let formatted = format_batch_result(result)

  formatted |> should.not_equal("")
}
