//// Batch Verification Module
////
//// Enables verification of multiple formulas across different modal systems
//// simultaneously. Supports systematic analysis and comparison workflows.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, K, K4, KD, KD45, S4, S5, T,
}

// =============================================================================
// Types
// =============================================================================

/// Batch verification request
pub type BatchRequest {
  BatchRequest(
    formulas: List(FormulaRequest),
    timeout_ms: Int,
    parallel: Bool,
    include_countermodels: Bool,
  )
}

/// Individual formula request
pub type FormulaRequest {
  FormulaRequest(id: String, formula: String, systems: List(LogicSystem))
}

/// Batch verification result
pub type BatchResult {
  BatchResult(
    results: List(FormulaResult),
    summary: BatchSummary,
    execution_time_ms: Int,
  )
}

/// Result for a single formula
pub type FormulaResult {
  FormulaResult(
    id: String,
    formula: String,
    system_results: Dict(String, SystemResult),
  )
}

/// Result for a formula in a specific system
pub type SystemResult {
  Valid
  Invalid(countermodel: Option(String))
  Timeout
  Error(message: String)
}

/// Batch execution summary
pub type BatchSummary {
  BatchSummary(
    total_formulas: Int,
    total_verifications: Int,
    valid_count: Int,
    invalid_count: Int,
    timeout_count: Int,
    error_count: Int,
    average_time_ms: Int,
  )
}

/// Comparison request for same formula across systems
pub type ComparisonRequest {
  ComparisonRequest(
    formula: String,
    systems: List(LogicSystem),
    timeout_ms: Int,
  )
}

/// Comparison result
pub type ComparisonResult {
  ComparisonResult(
    formula: String,
    results_by_system: Dict(String, SystemResult),
    differences: List(SystemDifference),
    consensus: Option(ConsensusOutcome),
  )
}

/// Difference between system results
pub type SystemDifference {
  SystemDifference(
    system1: String,
    system2: String,
    result1: SystemResult,
    result2: SystemResult,
    explanation: String,
  )
}

/// Consensus outcome across systems
pub type ConsensusOutcome {
  AllValid
  AllInvalid
  Mixed(valid_systems: List(String), invalid_systems: List(String))
}

// =============================================================================
// Batch Verification
// =============================================================================

/// Execute batch verification request (simplified implementation)
pub fn verify_batch(request: BatchRequest) -> BatchResult {
  // Simplified: Mock verification for each formula across systems
  let results =
    request.formulas
    |> list.map(fn(formula_req) {
      verify_formula_across_systems(formula_req, request.timeout_ms)
    })

  let summary = calculate_summary(results, 0)

  BatchResult(results: results, summary: summary, execution_time_ms: 0)
}

/// Verify a single formula across multiple systems
fn verify_formula_across_systems(
  formula_req: FormulaRequest,
  timeout_ms: Int,
) -> FormulaResult {
  let system_results =
    formula_req.systems
    |> list.map(fn(system) {
      let system_name = logic_system_to_string(system)
      let result = mock_verify(formula_req.formula, system, timeout_ms)
      #(system_name, result)
    })
    |> dict.from_list

  FormulaResult(
    id: formula_req.id,
    formula: formula_req.formula,
    system_results: system_results,
  )
}

/// Mock verification (simplified - real implementation would call actual validator)
fn mock_verify(
  formula: String,
  system: LogicSystem,
  timeout_ms: Int,
) -> SystemResult {
  // Simplified: Return valid for demonstration
  // Real implementation would integrate with validator.gleam
  Valid
}

/// Calculate batch summary statistics
fn calculate_summary(
  results: List(FormulaResult),
  execution_time_ms: Int,
) -> BatchSummary {
  let total_formulas = list.length(results)

  let all_system_results =
    results
    |> list.flat_map(fn(fr) { dict.values(fr.system_results) })

  let total_verifications = list.length(all_system_results)

  let valid_count =
    all_system_results
    |> list.count(fn(r) {
      case r {
        Valid -> True
        _ -> False
      }
    })

  let invalid_count =
    all_system_results
    |> list.count(fn(r) {
      case r {
        Invalid(_) -> True
        _ -> False
      }
    })

  let timeout_count =
    all_system_results
    |> list.count(fn(r) {
      case r {
        Timeout -> True
        _ -> False
      }
    })

  let error_count =
    all_system_results
    |> list.count(fn(r) {
      case r {
        Error(_) -> True
        _ -> False
      }
    })

  let average_time_ms = case total_verifications {
    0 -> 0
    n -> execution_time_ms / n
  }

  BatchSummary(
    total_formulas: total_formulas,
    total_verifications: total_verifications,
    valid_count: valid_count,
    invalid_count: invalid_count,
    timeout_count: timeout_count,
    error_count: error_count,
    average_time_ms: average_time_ms,
  )
}

// =============================================================================
// Comparison Mode
// =============================================================================

/// Compare a single formula across multiple systems
pub fn compare_systems(request: ComparisonRequest) -> ComparisonResult {
  let results_by_system =
    request.systems
    |> list.map(fn(system) {
      let system_name = logic_system_to_string(system)
      let result = mock_verify(request.formula, system, request.timeout_ms)
      #(system_name, result)
    })
    |> dict.from_list

  let differences = find_differences(results_by_system)
  let consensus = determine_consensus(results_by_system)

  ComparisonResult(
    formula: request.formula,
    results_by_system: results_by_system,
    differences: differences,
    consensus: Some(consensus),
  )
}

/// Find differences between system results
fn find_differences(
  results: Dict(String, SystemResult),
) -> List(SystemDifference) {
  // Simplified: Return empty list for now
  // Full implementation would compare all system pairs
  []
}

/// Check if two results differ
fn results_differ(r1: SystemResult, r2: SystemResult) -> Bool {
  case r1, r2 {
    Valid, Valid -> False
    Invalid(_), Invalid(_) -> False
    Timeout, Timeout -> False
    Error(_), Error(_) -> False
    _, _ -> True
  }
}

/// Explain difference between system results
fn explain_difference(
  system1: String,
  system2: String,
  result1: SystemResult,
  result2: SystemResult,
) -> String {
  case result1, result2 {
    Valid, Invalid(_) ->
      "Valid in "
      <> system1
      <> " but invalid in "
      <> system2
      <> " (stronger axioms)"
    Invalid(_), Valid ->
      "Invalid in "
      <> system1
      <> " but valid in "
      <> system2
      <> " (weaker axioms)"
    _, _ -> "Different results"
  }
}

/// Determine consensus outcome
fn determine_consensus(results: Dict(String, SystemResult)) -> ConsensusOutcome {
  let values = dict.values(results)

  let all_valid =
    values
    |> list.all(fn(r) {
      case r {
        Valid -> True
        _ -> False
      }
    })

  let all_invalid =
    values
    |> list.all(fn(r) {
      case r {
        Invalid(_) -> True
        _ -> False
      }
    })

  case all_valid, all_invalid {
    True, _ -> AllValid
    _, True -> AllInvalid
    _, _ -> {
      let valid_systems =
        dict.to_list(results)
        |> list.filter(fn(pair) {
          let #(_, res) = pair
          case res {
            Valid -> True
            _ -> False
          }
        })
        |> list.map(fn(pair) {
          let #(sys, _) = pair
          sys
        })

      let invalid_systems =
        dict.to_list(results)
        |> list.filter(fn(pair) {
          let #(_, res) = pair
          case res {
            Invalid(_) -> True
            _ -> False
          }
        })
        |> list.map(fn(pair) {
          let #(sys, _) = pair
          sys
        })

      Mixed(valid_systems: valid_systems, invalid_systems: invalid_systems)
    }
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Convert logic system to string
fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    K -> "K"
    T -> "T"
    K4 -> "K4"
    S4 -> "S4"
    S5 -> "S5"
    KD -> "KD"
    KD45 -> "KD45"
  }
}

/// Parse logic system from string
pub fn string_to_logic_system(s: String) -> Option(LogicSystem) {
  case string.uppercase(s) {
    "K" -> Some(K)
    "T" -> Some(T)
    "K4" -> Some(K4)
    "S4" -> Some(S4)
    "S5" -> Some(S5)
    "KD" -> Some(KD)
    "KD45" -> Some(KD45)
    _ -> None
  }
}

/// Format system result as string
pub fn format_system_result(result: SystemResult) -> String {
  case result {
    Valid -> "valid"
    Invalid(None) -> "invalid"
    Invalid(Some(cm)) -> "invalid (countermodel: " <> cm <> ")"
    Timeout -> "timeout"
    Error(msg) -> "error: " <> msg
  }
}

/// Get result status
pub fn result_status(result: SystemResult) -> String {
  case result {
    Valid -> "valid"
    Invalid(_) -> "invalid"
    Timeout -> "timeout"
    Error(_) -> "error"
  }
}

/// Create default batch request
pub fn default_batch_request() -> BatchRequest {
  BatchRequest(
    formulas: [],
    timeout_ms: 60_000,
    parallel: True,
    include_countermodels: True,
  )
}

/// Create default comparison request
pub fn default_comparison_request(formula: String) -> ComparisonRequest {
  ComparisonRequest(
    formula: formula,
    systems: [K, T, S4, S5, KD, KD45, K4],
    timeout_ms: 60_000,
  )
}
