//// Debug test for benchmark metrics
////
//// This test helps identify why benchmark metrics return 0%

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/benchmark_runner
import modal_logic/testing/external/dataset_adapter
import modal_logic/testing/external/folio_adapter
import modal_logic/testing/external/logiqa_adapter

pub fn debug_folio_fixtures_test() {
  io.println("")
  io.println("=== DEBUG: FOLIO Fixtures ===")

  // Check what folio_adapter returns directly
  let folio_config = folio_adapter.default_config()
  let folio_fixtures = folio_adapter.get_all_fixtures(folio_config)
  io.println(
    "FOLIO adapter fixtures count: "
    <> int.to_string(list.length(folio_fixtures)),
  )

  // Check unified fixtures
  let unified_config =
    dataset_adapter.UnifiedConfig(
      sources: [dataset_adapter.FOLIOSource],
      max_per_source: 100,
      logic_filter: dataset_adapter.AllLogicTypes,
      min_confidence: 0.5,
      include_invalid: True,
      cache_duration: 3600,
    )
  let unified_fixtures = dataset_adapter.get_unified_fixtures(unified_config)
  io.println(
    "Unified FOLIO fixtures count: "
    <> int.to_string(list.length(unified_fixtures)),
  )

  // Check benchmark suite
  let folio_suite = benchmark_runner.folio_suite()
  io.println(
    "FOLIO suite test cases: "
    <> int.to_string(list.length(folio_suite.test_cases)),
  )

  // If we have test cases, show first one
  case list.first(folio_suite.test_cases) {
    Ok(tc) -> {
      io.println("First test case ID: " <> tc.id)
      io.println("  Input: " <> tc.input)
      io.println(
        "  Premises count: " <> int.to_string(list.length(tc.premises)),
      )
      io.println("  Category: " <> tc.category)
      io.println(
        "  Expected validity: "
        <> case tc.expected_validity {
          benchmark_runner.ExpectedValidBenchmark -> "Valid"
          benchmark_runner.ExpectedInvalidBenchmark -> "Invalid"
          benchmark_runner.ExpectedEitherBenchmark(_) -> "Either"
          benchmark_runner.UnknownBenchmark -> "Unknown"
        },
      )
    }
    Error(_) -> io.println("No test cases found!")
  }

  // Should have at least some fixtures
  { list.length(folio_fixtures) > 0 } |> should.be_true()
}

pub fn debug_logiqa_fixtures_test() {
  io.println("")
  io.println("=== DEBUG: LogiQA Fixtures ===")

  let logiqa_config = logiqa_adapter.default_config()
  let logiqa_fixtures = logiqa_adapter.get_all_fixtures(logiqa_config)
  io.println(
    "LogiQA adapter fixtures count: "
    <> int.to_string(list.length(logiqa_fixtures)),
  )

  let logiqa_suite = benchmark_runner.logiqa_suite()
  io.println(
    "LogiQA suite test cases: "
    <> int.to_string(list.length(logiqa_suite.test_cases)),
  )

  { list.length(logiqa_fixtures) > 0 } |> should.be_true()
}

pub fn debug_benchmark_run_test() {
  io.println("")
  io.println("=== DEBUG: Benchmark Run ===")

  let folio_suite = benchmark_runner.folio_suite()
  let limited_suite =
    benchmark_runner.BenchmarkSuite(
      ..folio_suite,
      test_cases: list.take(folio_suite.test_cases, 5),
    )

  let config = benchmark_runner.fast_config()
  let results = benchmark_runner.run_benchmark_suite(limited_suite, config)

  io.println("Total cases: " <> int.to_string(results.total_cases))
  io.println(
    "True positives: " <> int.to_string(results.accuracy.true_positives),
  )
  io.println(
    "True negatives: " <> int.to_string(results.accuracy.true_negatives),
  )
  io.println(
    "False positives: " <> int.to_string(results.accuracy.false_positives),
  )
  io.println(
    "False negatives: " <> int.to_string(results.accuracy.false_negatives),
  )
  io.println("F1 score: " <> float_to_percent(results.accuracy.f1_score))
  io.println("Accuracy: " <> float_to_percent(results.accuracy.accuracy))

  // Show individual case results
  io.println("")
  io.println("Individual case results:")
  results.case_results
  |> list.each(fn(r) {
    io.println(
      "  "
      <> r.case_id
      <> ": predicted="
      <> option_bool_to_string(r.predicted_valid)
      <> ", expected="
      <> option_bool_to_string(r.expected_valid)
      <> ", correct="
      <> bool_to_string(r.correct),
    )
  })

  // Basic sanity check
  { results.total_cases > 0 } |> should.be_true()
}

fn option_bool_to_string(ob: option.Option(Bool)) -> String {
  case ob {
    Some(True) -> "True"
    Some(False) -> "False"
    None -> "None"
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn float_to_percent(f: Float) -> String {
  let pct = f *. 100.0
  int.to_string(truncate_float(pct)) <> "%"
}

fn truncate_float(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - truncate_float(0.0 -. f)
    False -> {
      case f <. 1.0 {
        True -> 0
        False -> 1 + truncate_float(f -. 1.0)
      }
    }
  }
}
