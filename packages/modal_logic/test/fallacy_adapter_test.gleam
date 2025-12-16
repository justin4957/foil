//// Tests for Fallacy Adapter
////
//// Tests the logical fallacy dataset integration including fallacy type
//// detection, translation, and negative testing fixture generation.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/proposition.{K, T}
import modal_logic/testing/external/fallacy_adapter.{
  AdHominem, AffirmingConsequent, AffirmingDisjunct, CircularReasoning,
  DenyingAntecedent, FalseDilemma, HastyGeneralization, SlipperySlope, StrawMan,
  UndistributedMiddle, default_config, extended_fallacy_examples,
  formal_fallacy_config, get_all_fixtures, get_formal_fixtures,
  get_mock_fixtures,
}
import modal_logic/testing/test_config.{ExpectedInvalid}

// ============ Configuration Tests ============

pub fn default_config_test() {
  let config = default_config()

  config.max_examples |> should.equal(100)
  config.fallacy_type_filter |> should.equal(None)
  config.formal_only |> should.be_false
  { config.min_confidence >=. 0.5 } |> should.be_true
}

pub fn formal_fallacy_config_test() {
  let config = formal_fallacy_config()

  config.formal_only |> should.be_true
}

pub fn type_config_test() {
  let config = fallacy_adapter.type_config(AffirmingConsequent)

  config.fallacy_type_filter |> should.equal(Some(AffirmingConsequent))
}

pub fn comprehensive_config_test() {
  let config = fallacy_adapter.comprehensive_config()

  config.max_examples |> should.equal(500)
  { config.min_confidence <=. 0.6 } |> should.be_true
}

// ============ Example Structure Tests ============

pub fn extended_examples_test() {
  let examples = extended_fallacy_examples()

  examples |> should.not_equal([])
  { list.length(examples) >= 5 } |> should.be_true
}

pub fn example_structure_test() {
  let examples = extended_fallacy_examples()

  list.all(examples, fn(ex) {
    ex.id != ""
    && ex.argument_text != ""
    && ex.premises != []
    && ex.conclusion != ""
    && ex.explanation != ""
  })
  |> should.be_true
}

pub fn formal_fallacies_marked_correctly_test() {
  let examples = extended_fallacy_examples()

  list.all(examples, fn(ex) {
    case ex.fallacy_type {
      AffirmingConsequent
      | DenyingAntecedent
      | AffirmingDisjunct
      | UndistributedMiddle -> ex.is_formal
      AdHominem | StrawMan | SlipperySlope | CircularReasoning -> !ex.is_formal
      _ -> True
    }
  })
  |> should.be_true
}

// ============ Fallacy Type Tests ============

pub fn affirming_consequent_examples_test() {
  let examples = extended_fallacy_examples()
  let ac =
    list.filter(examples, fn(ex) { ex.fallacy_type == AffirmingConsequent })

  ac |> should.not_equal([])
  { list.length(ac) >= 2 } |> should.be_true
}

pub fn denying_antecedent_examples_test() {
  let examples = extended_fallacy_examples()
  let da =
    list.filter(examples, fn(ex) { ex.fallacy_type == DenyingAntecedent })

  da |> should.not_equal([])
}

pub fn undistributed_middle_examples_test() {
  let examples = extended_fallacy_examples()
  let um =
    list.filter(examples, fn(ex) { ex.fallacy_type == UndistributedMiddle })

  um |> should.not_equal([])
}

pub fn false_dilemma_examples_test() {
  let examples = extended_fallacy_examples()
  let fd = list.filter(examples, fn(ex) { ex.fallacy_type == FalseDilemma })

  fd |> should.not_equal([])
}

pub fn informal_fallacies_exist_test() {
  let examples = extended_fallacy_examples()
  let informal = list.filter(examples, fn(ex) { !ex.is_formal })

  informal |> should.not_equal([])
}

// ============ Translation Tests ============

pub fn translate_affirming_consequent_test() {
  let examples = extended_fallacy_examples()
  let ac =
    list.find(examples, fn(ex) { ex.fallacy_type == AffirmingConsequent })

  case ac {
    Ok(ex) -> {
      let config = default_config()
      let translation = fallacy_adapter.translate_example(ex, config)

      translation.original_id |> should.equal(ex.id)
      translation.fallacy_type |> should.equal(AffirmingConsequent)
      translation.translated_premises |> should.not_equal([])
      translation.invalidity_reason |> should.not_equal("")
      { translation.confidence >. 0.0 } |> should.be_true
    }
    Error(_) -> panic as "Expected to find affirming consequent example"
  }
}

pub fn translate_undistributed_middle_test() {
  let examples = extended_fallacy_examples()
  let um =
    list.find(examples, fn(ex) { ex.fallacy_type == UndistributedMiddle })

  case um {
    Ok(ex) -> {
      let config = default_config()
      let translation = fallacy_adapter.translate_example(ex, config)

      translation.fallacy_type |> should.equal(UndistributedMiddle)
      translation.logic_system |> should.equal(T)
    }
    Error(_) -> panic as "Expected to find undistributed middle example"
  }
}

// ============ Fixture Conversion Tests ============

pub fn get_all_fixtures_test() {
  let config = default_config()
  let fixtures = get_all_fixtures(config)

  fixtures |> should.not_equal([])
}

pub fn fixtures_are_invalid_test() {
  let fixtures = get_mock_fixtures()

  list.all(fixtures, fn(f) {
    case f.expected_validity {
      ExpectedInvalid(_) -> True
      _ -> False
    }
  })
  |> should.be_true
}

pub fn fixture_has_fallacy_tag_test() {
  let fixtures = get_mock_fixtures()

  list.all(fixtures, fn(f) { list.contains(f.tags, "fallacy") })
  |> should.be_true
}

pub fn fixture_has_invalid_tag_test() {
  let fixtures = get_mock_fixtures()

  list.all(fixtures, fn(f) { list.contains(f.tags, "invalid") })
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

// ============ Formal vs Informal Tests ============

pub fn get_formal_fixtures_test() {
  let fixtures = get_formal_fixtures()

  fixtures |> should.not_equal([])
}

pub fn formal_fixtures_filter_test() {
  let all_fixtures = get_mock_fixtures()
  let formal_fixtures = get_formal_fixtures()

  // Formal should be subset of all
  { list.length(formal_fixtures) <= list.length(all_fixtures) }
  |> should.be_true
}

// ============ Statistics Tests ============

pub fn dataset_statistics_test() {
  let stats = fallacy_adapter.dataset_statistics()

  stats.total_examples |> should.not_equal(0)
  { stats.formal_fallacies + stats.informal_fallacies == stats.total_examples }
  |> should.be_true
  dict.size(stats.by_type) |> should.not_equal(0)
}

pub fn fallacy_type_to_string_test() {
  fallacy_adapter.fallacy_type_to_string(AffirmingConsequent)
  |> should.equal("affirming_consequent")

  fallacy_adapter.fallacy_type_to_string(DenyingAntecedent)
  |> should.equal("denying_antecedent")

  fallacy_adapter.fallacy_type_to_string(AdHominem)
  |> should.equal("ad_hominem")
}

pub fn is_formal_fallacy_test() {
  fallacy_adapter.is_formal_fallacy(AffirmingConsequent) |> should.be_true
  fallacy_adapter.is_formal_fallacy(DenyingAntecedent) |> should.be_true
  fallacy_adapter.is_formal_fallacy(UndistributedMiddle) |> should.be_true

  fallacy_adapter.is_formal_fallacy(AdHominem) |> should.be_false
  fallacy_adapter.is_formal_fallacy(SlipperySlope) |> should.be_false
  fallacy_adapter.is_formal_fallacy(CircularReasoning) |> should.be_false
}

// ============ Filtering Tests ============

pub fn filter_by_fallacy_type_test() {
  let config = fallacy_adapter.type_config(AffirmingConsequent)
  let fixtures = get_all_fixtures(config)

  list.all(fixtures, fn(f) { list.contains(f.tags, "affirming_consequent") })
  |> should.be_true
}

pub fn max_examples_limit_test() {
  let config =
    fallacy_adapter.FallacyConfig(..default_config(), max_examples: 3)
  let fixtures = get_all_fixtures(config)

  { list.length(fixtures) <= 3 } |> should.be_true
}

// ============ Fallacy Type Lists ============

pub fn formal_fallacy_types_test() {
  let formal = fallacy_adapter.formal_fallacy_types()

  formal |> should.not_equal([])
  list.all(formal, fallacy_adapter.is_formal_fallacy) |> should.be_true
}

pub fn informal_fallacy_types_test() {
  let informal = fallacy_adapter.informal_fallacy_types()

  informal |> should.not_equal([])
  list.all(informal, fn(ft) { !fallacy_adapter.is_formal_fallacy(ft) })
  |> should.be_true
}
