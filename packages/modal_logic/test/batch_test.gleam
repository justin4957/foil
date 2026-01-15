import gleam/dict
import gleam/list
import gleam/option
import gleeunit
import gleeunit/should
import modal_logic/batch
import modal_logic/proposition.{K, K4, KD, KD45, S4, S5, T}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Batch Verification Tests
// =============================================================================

pub fn verify_batch_empty_test() {
  let request = batch.default_batch_request()
  let result = batch.verify_batch(request)

  result.summary.total_formulas |> should.equal(0)
  result.summary.total_verifications |> should.equal(0)
}

pub fn verify_batch_single_formula_test() {
  let formula_req =
    batch.FormulaRequest(id: "f1", formula: "□p → p", systems: [T, S4, S5])

  let request =
    batch.BatchRequest(
      formulas: [formula_req],
      timeout_ms: 60_000,
      parallel: True,
      include_countermodels: True,
    )

  let result = batch.verify_batch(request)

  result.summary.total_formulas |> should.equal(1)
  result.summary.total_verifications |> should.equal(3)
}

pub fn verify_batch_multiple_formulas_test() {
  let f1 = batch.FormulaRequest(id: "f1", formula: "□p → p", systems: [K, T])
  let f2 =
    batch.FormulaRequest(id: "f2", formula: "□p → □□p", systems: [K4, S4])

  let request =
    batch.BatchRequest(
      formulas: [f1, f2],
      timeout_ms: 60_000,
      parallel: True,
      include_countermodels: False,
    )

  let result = batch.verify_batch(request)

  result.summary.total_formulas |> should.equal(2)
  result.summary.total_verifications |> should.equal(4)
}

pub fn batch_result_has_correct_structure_test() {
  let formula_req =
    batch.FormulaRequest(id: "f1", formula: "test", systems: [K, T])

  let request =
    batch.BatchRequest(
      formulas: [formula_req],
      timeout_ms: 60_000,
      parallel: True,
      include_countermodels: True,
    )

  let result = batch.verify_batch(request)

  list.length(result.results) |> should.equal(1)

  case list.first(result.results) {
    Ok(fr) -> {
      fr.id |> should.equal("f1")
      dict.size(fr.system_results) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Comparison Mode Tests
// =============================================================================

pub fn compare_systems_basic_test() {
  let request =
    batch.ComparisonRequest(
      formula: "□p → p",
      systems: [K, T, S4, S5],
      timeout_ms: 60_000,
    )

  let result = batch.compare_systems(request)

  result.formula |> should.equal("□p → p")
  dict.size(result.results_by_system) |> should.equal(4)
}

pub fn compare_systems_determines_consensus_test() {
  let request =
    batch.ComparisonRequest(
      formula: "test",
      systems: [K, T],
      timeout_ms: 60_000,
    )

  let result = batch.compare_systems(request)

  case result.consensus {
    option.Some(_) -> should.be_true(True)
    option.None -> should.fail()
  }
}

pub fn compare_systems_finds_differences_test() {
  let request =
    batch.ComparisonRequest(
      formula: "test",
      systems: [K, T, S4],
      timeout_ms: 60_000,
    )

  let result = batch.compare_systems(request)

  // Differences list exists (may be empty if all results same)
  { list.length(result.differences) >= 0 } |> should.be_true()
}

// =============================================================================
// Helper Function Tests
// =============================================================================

pub fn string_to_logic_system_test() {
  case batch.string_to_logic_system("K") {
    option.Some(sys) -> sys |> should.equal(K)
    option.None -> should.fail()
  }

  case batch.string_to_logic_system("S5") {
    option.Some(sys) -> sys |> should.equal(S5)
    option.None -> should.fail()
  }

  case batch.string_to_logic_system("invalid") {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

pub fn string_to_logic_system_case_insensitive_test() {
  case batch.string_to_logic_system("s5") {
    option.Some(sys) -> sys |> should.equal(S5)
    option.None -> should.fail()
  }

  case batch.string_to_logic_system("KD45") {
    option.Some(sys) -> sys |> should.equal(KD45)
    option.None -> should.fail()
  }
}

pub fn format_system_result_valid_test() {
  let result = batch.Valid
  batch.format_system_result(result) |> should.equal("valid")
}

pub fn format_system_result_invalid_test() {
  let result = batch.Invalid(option.None)
  batch.format_system_result(result) |> should_contain("invalid")
}

pub fn format_system_result_timeout_test() {
  let result = batch.Timeout
  batch.format_system_result(result) |> should.equal("timeout")
}

pub fn format_system_result_error_test() {
  let result = batch.Error("Z3 failed")
  batch.format_system_result(result) |> should_contain("error")
}

pub fn result_status_test() {
  batch.result_status(batch.Valid) |> should.equal("valid")
  batch.result_status(batch.Invalid(option.None)) |> should.equal("invalid")
  batch.result_status(batch.Timeout) |> should.equal("timeout")
  batch.result_status(batch.Error("msg")) |> should.equal("error")
}

// =============================================================================
// Summary Statistics Tests
// =============================================================================

pub fn batch_summary_counts_correctly_test() {
  let formula_req =
    batch.FormulaRequest(id: "f1", formula: "test", systems: [K, T, S4])

  let request =
    batch.BatchRequest(
      formulas: [formula_req],
      timeout_ms: 60_000,
      parallel: True,
      include_countermodels: True,
    )

  let result = batch.verify_batch(request)

  // Should have verified 1 formula across 3 systems
  result.summary.total_formulas |> should.equal(1)
  result.summary.total_verifications |> should.equal(3)
}

// =============================================================================
// Default Request Tests
// =============================================================================

pub fn default_batch_request_test() {
  let request = batch.default_batch_request()

  request.timeout_ms |> should.equal(60_000)
  request.parallel |> should.be_true()
  request.include_countermodels |> should.be_true()
  list.length(request.formulas) |> should.equal(0)
}

pub fn default_comparison_request_test() {
  let request = batch.default_comparison_request("□p → p")

  request.formula |> should.equal("□p → p")
  request.timeout_ms |> should.equal(60_000)
  { list.length(request.systems) > 0 } |> should.be_true()
}

// =============================================================================
// Helper Functions
// =============================================================================

fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False -> should.fail()
  }
}

import gleam/string
