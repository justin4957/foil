//// Tests for LogiQA Adapter
////
//// Tests the LogiQA 2.0 dataset integration including premise-hypothesis
//// pair parsing, reasoning type detection, and fixture conversion.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/proposition.{K, S4, S5, T}
import modal_logic/testing/external/huggingface_loader.{
  DatasetExample, Entailment,
}
import modal_logic/testing/external/logiqa_adapter.{
  Categorical, Causal, Disjunctive, Mixed, NecessaryCondition,
  SufficientCondition, default_config, extended_logiqa_examples,
  get_all_fixtures, get_mock_fixtures,
}

// ============ Configuration Tests ============

pub fn default_config_test() {
  let config = default_config()

  config.max_examples |> should.equal(100)
  config.reasoning_type_filter |> should.equal(None)
  { config.min_confidence >=. 0.5 } |> should.be_true
  config.include_context |> should.be_true
}

pub fn categorical_config_test() {
  let config = logiqa_adapter.categorical_config()

  config.reasoning_type_filter |> should.equal(Some(Categorical))
}

pub fn conditional_config_test() {
  let config = logiqa_adapter.conditional_config()

  config.reasoning_type_filter |> should.equal(Some(SufficientCondition))
}

pub fn comprehensive_config_test() {
  let config = logiqa_adapter.comprehensive_config()

  config.max_examples |> should.equal(500)
  { config.min_confidence <=. 0.6 } |> should.be_true
}

// ============ Example Structure Tests ============

pub fn extended_examples_test() {
  let examples = extended_logiqa_examples()

  examples |> should.not_equal([])
  { list.length(examples) >= 5 } |> should.be_true
}

pub fn example_has_premise_hypothesis_pairs_test() {
  let examples = extended_logiqa_examples()

  list.all(examples, fn(ex) { ex.premise_hypothesis_pairs != [] })
  |> should.be_true
}

pub fn example_structure_test() {
  let examples = extended_logiqa_examples()

  list.all(examples, fn(ex) {
    ex.id != ""
    && ex.context != ""
    && ex.question != ""
    && ex.options != []
    && ex.correct_answer >= 0
    && ex.correct_answer < list.length(ex.options)
  })
  |> should.be_true
}

// ============ Reasoning Type Tests ============

pub fn categorical_examples_exist_test() {
  let examples = extended_logiqa_examples()
  let categorical =
    list.filter(examples, fn(ex) { ex.reasoning_type == Categorical })

  categorical |> should.not_equal([])
}

pub fn sufficient_condition_examples_exist_test() {
  let examples = extended_logiqa_examples()
  let sufficient =
    list.filter(examples, fn(ex) { ex.reasoning_type == SufficientCondition })

  sufficient |> should.not_equal([])
}

pub fn disjunctive_examples_exist_test() {
  let examples = extended_logiqa_examples()
  let disjunctive =
    list.filter(examples, fn(ex) { ex.reasoning_type == Disjunctive })

  disjunctive |> should.not_equal([])
}

pub fn causal_examples_exist_test() {
  let examples = extended_logiqa_examples()
  let causal = list.filter(examples, fn(ex) { ex.reasoning_type == Causal })

  causal |> should.not_equal([])
}

// ============ Translation Tests ============

pub fn translate_categorical_example_test() {
  let examples = extended_logiqa_examples()
  let categorical =
    list.find(examples, fn(ex) { ex.reasoning_type == Categorical })

  case categorical {
    Ok(ex) -> {
      let config = default_config()
      let translations = logiqa_adapter.translate_example(ex, config)

      translations |> should.not_equal([])

      // Check translation structure
      list.all(translations, fn(t) {
        t.original_id == ex.id
        && t.translated_premises != []
        && t.confidence >. 0.0
      })
      |> should.be_true
    }
    Error(_) -> panic as "Expected to find categorical example"
  }
}

pub fn translate_conditional_example_test() {
  let examples = extended_logiqa_examples()
  let conditional =
    list.find(examples, fn(ex) { ex.reasoning_type == SufficientCondition })

  case conditional {
    Ok(ex) -> {
      let config = default_config()
      let translations = logiqa_adapter.translate_example(ex, config)

      translations |> should.not_equal([])

      // Should have some patterns detected (modus ponens or conditional)
      list.any(translations, fn(t) {
        list.contains(t.patterns, logiqa_adapter.ModusPonens)
        || list.contains(t.patterns, logiqa_adapter.ConditionalReasoning)
        || t.patterns != []
      })
      |> should.be_true
    }
    Error(_) -> panic as "Expected to find conditional example"
  }
}

pub fn translate_disjunctive_example_test() {
  let examples = extended_logiqa_examples()
  let disjunctive =
    list.find(examples, fn(ex) { ex.reasoning_type == Disjunctive })

  case disjunctive {
    Ok(ex) -> {
      let config = default_config()
      let translations = logiqa_adapter.translate_example(ex, config)

      translations |> should.not_equal([])

      // Should have disjunctive pattern
      list.any(translations, fn(t) {
        list.contains(t.patterns, logiqa_adapter.DisjunctiveSyllogism)
      })
      |> should.be_true
    }
    Error(_) -> panic as "Expected to find disjunctive example"
  }
}

// ============ Logic System Detection Tests ============

pub fn epistemic_logic_system_test() {
  let examples = extended_logiqa_examples()
  let epistemic = list.find(examples, fn(ex) { ex.id == "logiqa_epi_1" })

  case epistemic {
    Ok(ex) -> {
      let config = default_config()
      let translations = logiqa_adapter.translate_example(ex, config)

      list.all(translations, fn(t) { t.logic_system == S5 }) |> should.be_true
    }
    Error(_) -> panic as "Expected to find epistemic example"
  }
}

pub fn deontic_logic_system_test() {
  let examples = extended_logiqa_examples()
  let deontic = list.find(examples, fn(ex) { ex.id == "logiqa_deo_1" })

  case deontic {
    Ok(ex) -> {
      let config = default_config()
      let translations = logiqa_adapter.translate_example(ex, config)

      list.all(translations, fn(t) { t.logic_system == proposition.KD })
      |> should.be_true
    }
    Error(_) -> panic as "Expected to find deontic example"
  }
}

// ============ Fixture Conversion Tests ============

pub fn get_all_fixtures_test() {
  let config = default_config()
  let fixtures = get_all_fixtures(config)

  fixtures |> should.not_equal([])
}

pub fn fixture_structure_test() {
  let fixtures = get_mock_fixtures()

  list.all(fixtures, fn(f) {
    f.id != "" && f.name != "" && f.expected_premises != [] && f.tags != []
  })
  |> should.be_true
}

pub fn fixture_has_logiqa_tag_test() {
  let fixtures = get_mock_fixtures()

  list.all(fixtures, fn(f) { list.contains(f.tags, "logiqa") })
  |> should.be_true
}

pub fn fixture_has_source_test() {
  let fixtures = get_mock_fixtures()

  list.all(fixtures, fn(f) {
    case f.source {
      Some(s) -> s != ""
      None -> False
    }
  })
  |> should.be_true
}

// ============ Statistics Tests ============

pub fn dataset_statistics_test() {
  let stats = logiqa_adapter.dataset_statistics()

  stats.total_examples |> should.not_equal(0)
  stats.total_pairs |> should.not_equal(0)
  dict.size(stats.by_reasoning_type) |> should.not_equal(0)
  { stats.average_premises >. 0.0 } |> should.be_true
}

pub fn reasoning_type_to_string_test() {
  logiqa_adapter.reasoning_type_to_string(Categorical)
  |> should.equal("categorical")

  logiqa_adapter.reasoning_type_to_string(SufficientCondition)
  |> should.equal("sufficient_condition")

  logiqa_adapter.reasoning_type_to_string(Disjunctive)
  |> should.equal("disjunctive")
}

// ============ Filtering Tests ============

pub fn filter_by_reasoning_type_test() {
  let config =
    logiqa_adapter.LogiQAConfig(
      ..default_config(),
      reasoning_type_filter: Some(Categorical),
    )
  let fixtures = get_all_fixtures(config)

  // All fixtures should be from categorical examples
  list.all(fixtures, fn(f) { list.contains(f.tags, "categorical") })
  |> should.be_true
}

pub fn max_examples_limit_test() {
  // max_examples limits examples taken, not fixtures generated
  // Each example may generate multiple fixtures
  let config_low = logiqa_adapter.LogiQAConfig(..default_config(), max_examples: 2)
  let config_high = logiqa_adapter.LogiQAConfig(..default_config(), max_examples: 10)

  let fixtures_low = get_all_fixtures(config_low)
  let fixtures_high = get_all_fixtures(config_high)

  // With more examples allowed, should have >= fixtures
  { list.length(fixtures_low) <= list.length(fixtures_high) } |> should.be_true
}

// ============ DatasetExample Conversion Tests ============

pub fn from_dataset_example_test() {
  let example =
    DatasetExample(
      id: "test_1",
      premises: ["All cats are mammals", "Fluffy is a cat"],
      conclusion: "Fluffy is a mammal",
      label: Entailment,
      source_format: "test",
      metadata: dict.new(),
    )

  let logiqa_example = logiqa_adapter.from_dataset_example(example)

  logiqa_example.id |> should.equal("test_1")
  logiqa_example.reasoning_type |> should.equal(Categorical)
  logiqa_example.premise_hypothesis_pairs |> should.not_equal([])
}
