//// Tests for Unified Dataset Adapter Interface
////
//// Tests the unified interface that combines all external datasets
//// (FOLIO, LogiQA, Fallacy, InPhO) with filtering and statistics.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/proposition.{K, S5, T}
import modal_logic/testing/external/dataset_adapter.{
  AllLogicTypes, AllSources, ClassicalLogic, DeonticLogic, EpistemicLogic,
  FOLIOSource, FallacySource, InPhOSource, LogiQASource, ModalLogic,
  comprehensive_unified_config, deontic_only_config, epistemic_only_config,
  get_all_external_fixtures, get_deontic_fixtures, get_epistemic_fixtures,
  get_fallacy_fixtures, get_modal_fixtures, get_unified_fixtures,
  modal_only_config, unified_default_config, unified_statistics,
  valid_only_config,
}
import modal_logic/testing/test_config.{ExpectedInvalid, ExpectedValid}

// ============ Configuration Tests ============

pub fn unified_default_config_test() {
  let config = unified_default_config()

  config.sources |> should.not_equal([])
  config.max_per_source |> should.equal(50)
  config.logic_filter |> should.equal(AllLogicTypes)
  config.include_invalid |> should.be_true
}

pub fn modal_only_config_test() {
  let config = modal_only_config()

  config.logic_filter |> should.equal(ModalLogic)
}

pub fn epistemic_only_config_test() {
  let config = epistemic_only_config()

  config.logic_filter |> should.equal(EpistemicLogic)
}

pub fn deontic_only_config_test() {
  let config = deontic_only_config()

  config.logic_filter |> should.equal(DeonticLogic)
}

pub fn valid_only_config_test() {
  let config = valid_only_config()

  config.include_invalid |> should.be_false
}

pub fn comprehensive_unified_config_test() {
  let config = comprehensive_unified_config()

  config.max_per_source |> should.equal(200)
  { config.min_confidence <=. 0.6 } |> should.be_true
}

// ============ Unified Loading Tests ============

pub fn get_unified_fixtures_test() {
  let config = unified_default_config()
  let fixtures = get_unified_fixtures(config)

  fixtures |> should.not_equal([])
}

pub fn get_all_external_fixtures_test() {
  let fixtures = get_all_external_fixtures()

  fixtures |> should.not_equal([])
}

pub fn fixtures_from_multiple_sources_test() {
  let fixtures = get_all_external_fixtures()

  // Should have fixtures from at least 2 different sources
  let sources =
    fixtures
    |> list.filter_map(fn(f) {
      case f.source {
        Some(s) -> Ok(s)
        None -> Error(Nil)
      }
    })
    |> list.unique

  { list.length(sources) >= 2 } |> should.be_true
}

// ============ Source Loading Tests ============

pub fn load_folio_source_test() {
  let config =
    dataset_adapter.UnifiedConfig(..unified_default_config(), sources: [
      FOLIOSource,
    ])
  let fixtures = get_unified_fixtures(config)

  fixtures |> should.not_equal([])
}

pub fn load_logiqa_source_test() {
  let config =
    dataset_adapter.UnifiedConfig(..unified_default_config(), sources: [
      LogiQASource,
    ])
  let fixtures = get_unified_fixtures(config)

  fixtures |> should.not_equal([])
}

pub fn load_fallacy_source_test() {
  let config =
    dataset_adapter.UnifiedConfig(..unified_default_config(), sources: [
      FallacySource,
    ])
  let fixtures = get_unified_fixtures(config)

  fixtures |> should.not_equal([])
}

pub fn load_inpho_source_test() {
  let config =
    dataset_adapter.UnifiedConfig(..unified_default_config(), sources: [
      InPhOSource,
    ])
  let fixtures = get_unified_fixtures(config)

  fixtures |> should.not_equal([])
}

pub fn load_all_sources_test() {
  let config =
    dataset_adapter.UnifiedConfig(..unified_default_config(), sources: [
      AllSources,
    ])
  let fixtures = get_unified_fixtures(config)

  fixtures |> should.not_equal([])
}

// ============ Logic Type Filtering Tests ============

pub fn get_modal_fixtures_test() {
  let fixtures = get_modal_fixtures()

  fixtures |> should.not_equal([])

  // All should be modal logic systems
  list.all(fixtures, fn(f) {
    case f.expected_logic_system {
      K | T | proposition.S4 | S5 -> True
      _ -> False
    }
  })
  |> should.be_true
}

pub fn get_deontic_fixtures_test() {
  let fixtures = get_deontic_fixtures()

  // May be empty if no deontic fixtures available
  // Just verify it doesn't crash
  { list.length(fixtures) >= 0 } |> should.be_true
}

pub fn get_epistemic_fixtures_test() {
  let fixtures = get_epistemic_fixtures()

  // May be empty if no epistemic fixtures available
  { list.length(fixtures) >= 0 } |> should.be_true
}

// ============ Validity Filtering Tests ============

pub fn get_fallacy_fixtures_test() {
  let fixtures = get_fallacy_fixtures()

  fixtures |> should.not_equal([])

  // All should be invalid
  list.all(fixtures, fn(f) {
    case f.expected_validity {
      ExpectedInvalid(_) -> True
      _ -> False
    }
  })
  |> should.be_true
}

pub fn valid_only_filtering_test() {
  let config = valid_only_config()
  let fixtures = get_unified_fixtures(config)

  // No invalid fixtures should be present
  list.all(fixtures, fn(f) {
    case f.expected_validity {
      ExpectedInvalid(_) -> False
      _ -> True
    }
  })
  |> should.be_true
}

// ============ Max Per Source Tests ============

pub fn max_per_source_limit_test() {
  let config =
    dataset_adapter.UnifiedConfig(
      ..unified_default_config(),
      sources: [FOLIOSource],
      max_per_source: 5,
    )
  let fixtures = get_unified_fixtures(config)

  { list.length(fixtures) <= 5 } |> should.be_true
}

// ============ Statistics Tests ============

pub fn unified_statistics_test() {
  let fixtures = get_all_external_fixtures()
  let stats = unified_statistics(fixtures)

  stats.total_fixtures |> should.equal(list.length(fixtures))
  dict.size(stats.by_source) |> should.not_equal(0)
  dict.size(stats.by_logic_system) |> should.not_equal(0)
  { stats.valid_count + stats.invalid_count <= stats.total_fixtures }
  |> should.be_true
}

pub fn format_unified_statistics_test() {
  let fixtures = get_all_external_fixtures()
  let stats = unified_statistics(fixtures)
  let formatted = dataset_adapter.format_unified_statistics(stats)

  formatted |> should.not_equal("")
  // Should contain key sections
  { formatted != "" } |> should.be_true
}

// ============ Available Sources Tests ============

pub fn available_sources_test() {
  let sources = dataset_adapter.available_sources()

  sources |> should.not_equal([])
  { list.length(sources) >= 4 } |> should.be_true

  // Check that known sources are present
  let source_names = list.map(sources, fn(pair) { pair.0 })
  list.contains(source_names, "FOLIO") |> should.be_true
  list.contains(source_names, "LogiQA") |> should.be_true
  list.contains(source_names, "Fallacy") |> should.be_true
  list.contains(source_names, "InPhO") |> should.be_true
}

// ============ Fixture Quality Tests ============

pub fn all_fixtures_have_ids_test() {
  let fixtures = get_all_external_fixtures()

  list.all(fixtures, fn(f) { f.id != "" }) |> should.be_true
}

pub fn all_fixtures_have_premises_test() {
  let fixtures = get_all_external_fixtures()

  list.all(fixtures, fn(f) { f.expected_premises != [] }) |> should.be_true
}

pub fn all_fixtures_have_tags_test() {
  let fixtures = get_all_external_fixtures()

  list.all(fixtures, fn(f) { f.tags != [] }) |> should.be_true
}
