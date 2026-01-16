//// Confidence Scoring for Validation Results
////
//// This module provides confidence scoring for modal logic validation results.
//// Confidence scores indicate how certain we are about the validation outcome.
////
//// ## Confidence Factors
////
//// | Factor | Weight | Description |
//// |--------|--------|-------------|
//// | Full Z3 proof | +0.95 | Z3 completed with definitive result |
//// | Countermodel found | +0.90 | Concrete counterexample exists |
//// | Heuristic match | +0.70 | Pattern matched known valid/invalid |
//// | Truth table complete | +0.85 | Exhaustive enumeration |
//// | Timeout with partial | +0.30 | Z3 timed out but had partial info |
//// | Unknown result | +0.10 | No determination possible |
//// | Frame completeness | +0.10 | All frame conditions checked |
//// | World bound hit | -0.20 | May need more worlds |
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/confidence.{compute_confidence, ConfidenceResult}
////
//// let conf_result = compute_confidence(validation_result, tier_used, context)
//// // conf_result.score is 0.0 to 1.0
//// // conf_result.factors explains the breakdown
//// ```

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/argument.{type ValidationResult, Invalid, Valid}
import modal_logic/heuristics.{
  type ValidationTier, Tier1Syntactic, Tier2TruthTable, Tier3Z3,
}

// =============================================================================
// Types
// =============================================================================

/// A factor contributing to confidence score
pub type ConfidenceFactor {
  ConfidenceFactor(
    /// Factor identifier (e.g., "full_z3_proof", "heuristic_match")
    factor: String,
    /// How much this factor contributes (can be negative)
    contribution: Float,
    /// Human-readable description
    description: String,
  )
}

/// Result of confidence computation
pub type ConfidenceResult {
  ConfidenceResult(
    /// Overall confidence score (0.0 to 1.0)
    score: Float,
    /// List of factors that contributed to the score
    factors: List(ConfidenceFactor),
    /// Confidence level classification
    level: ConfidenceLevel,
    /// Whether this is a high-confidence result (>0.8)
    is_high_confidence: Bool,
  )
}

/// Classification of confidence levels
pub type ConfidenceLevel {
  /// Very high confidence (0.9-1.0)
  VeryHigh
  /// High confidence (0.8-0.9)
  High
  /// Medium confidence (0.6-0.8)
  Medium
  /// Low confidence (0.4-0.6)
  Low
  /// Very low confidence (0.0-0.4)
  VeryLow
}

/// Context for confidence computation
pub type ConfidenceContext {
  ConfidenceContext(
    /// Which validation tier was used
    tier_used: ValidationTier,
    /// Whether Z3 timed out
    z3_timeout: Bool,
    /// Whether world bound was hit
    world_bound_hit: Bool,
    /// Number of worlds explored (if applicable)
    worlds_explored: Option(Int),
    /// Maximum worlds configured
    max_worlds: Option(Int),
    /// Number of variables in formula
    variable_count: Option(Int),
    /// Whether frame conditions were fully checked
    frame_complete: Bool,
    /// Additional tier-specific explanation
    tier_explanation: Option(String),
  )
}

/// Extended validation result with confidence
pub type ScoredValidationResult {
  ScoredValidationResult(
    /// The base validation result
    result: ValidationResult,
    /// Confidence scoring
    confidence: ConfidenceResult,
    /// Validation tier used
    tier_used: ValidationTier,
    /// Tier explanation
    tier_explanation: Option(String),
  )
}

// =============================================================================
// Configuration Constants
// =============================================================================

/// Weight for full Z3 proof
const weight_full_z3_proof = 0.95

/// Weight for countermodel found
const weight_countermodel_found = 0.9

/// Weight for Tier 1 heuristic match
const weight_tier1_heuristic = 0.7

/// Weight for Tier 2 truth table complete
const weight_truth_table_complete = 0.85

/// Weight for timeout with partial result
const weight_timeout_partial = 0.3

/// Weight for unknown result
const weight_unknown = 0.1

/// Weight for frame completeness bonus
const weight_frame_complete = 0.1

/// Penalty for hitting world bound
const penalty_world_bound_hit = -0.2

// =============================================================================
// Confidence Computation
// =============================================================================

/// Compute confidence score for a validation result
pub fn compute_confidence(
  result: ValidationResult,
  context: ConfidenceContext,
) -> ConfidenceResult {
  let factors = compute_confidence_factors(result, context)
  let raw_score = sum_contributions(factors)
  let score = clamp(raw_score, 0.0, 1.0)
  let level = classify_confidence(score)

  ConfidenceResult(
    score: score,
    factors: factors,
    level: level,
    is_high_confidence: score >=. 0.8,
  )
}

/// Compute individual confidence factors
fn compute_confidence_factors(
  result: ValidationResult,
  context: ConfidenceContext,
) -> List(ConfidenceFactor) {
  let base_factors = case context.tier_used {
    Tier1Syntactic -> tier1_factors(result, context)
    Tier2TruthTable -> tier2_factors(result, context)
    Tier3Z3 -> tier3_factors(result, context)
  }

  // Add common adjustment factors
  let adjustment_factors = common_adjustment_factors(context)

  list.flatten([base_factors, adjustment_factors])
}

/// Factors for Tier 1 (syntactic pattern matching)
fn tier1_factors(
  result: ValidationResult,
  context: ConfidenceContext,
) -> List(ConfidenceFactor) {
  case result {
    Valid -> [
      ConfidenceFactor(
        factor: "tier1_syntactic_valid",
        contribution: weight_tier1_heuristic,
        description: "Syntactic pattern matched known valid form",
      ),
      ConfidenceFactor(
        factor: "fast_path_success",
        contribution: 0.05,
        description: "Fast path validation succeeded",
      ),
    ]
    Invalid(_) -> [
      ConfidenceFactor(
        factor: "tier1_syntactic_invalid",
        contribution: weight_tier1_heuristic,
        description: "Syntactic pattern matched known invalid form",
      ),
      ConfidenceFactor(
        factor: "fast_path_success",
        contribution: 0.05,
        description: "Fast path validation succeeded",
      ),
    ]
    _ -> [
      ConfidenceFactor(
        factor: "tier1_inconclusive",
        contribution: weight_unknown,
        description: "Syntactic checks were inconclusive",
      ),
    ]
  }
}

/// Factors for Tier 2 (truth table enumeration)
fn tier2_factors(
  result: ValidationResult,
  context: ConfidenceContext,
) -> List(ConfidenceFactor) {
  let base_contribution = case result {
    Valid ->
      ConfidenceFactor(
        factor: "truth_table_exhaustive_valid",
        contribution: weight_truth_table_complete,
        description: "Truth table analysis proved validity for all assignments",
      )
    Invalid(_) ->
      ConfidenceFactor(
        factor: "truth_table_counterexample",
        contribution: weight_truth_table_complete,
        description: "Truth table analysis found concrete counterexample",
      )
    _ ->
      ConfidenceFactor(
        factor: "truth_table_inconclusive",
        contribution: weight_unknown,
        description: "Truth table analysis was inconclusive",
      )
  }

  // Add variable count bonus/penalty
  let var_factor = case context.variable_count {
    Some(n) if n <= 3 ->
      Some(ConfidenceFactor(
        factor: "small_formula",
        contribution: 0.05,
        description: "Formula has few variables (" <> int.to_string(n) <> ")",
      ))
    Some(n) if n >= 5 ->
      Some(ConfidenceFactor(
        factor: "large_formula",
        contribution: -0.05,
        description: "Formula has many variables (" <> int.to_string(n) <> ")",
      ))
    _ -> None
  }

  case var_factor {
    Some(f) -> [base_contribution, f]
    None -> [base_contribution]
  }
}

/// Factors for Tier 3 (Z3 SMT solving)
fn tier3_factors(
  result: ValidationResult,
  context: ConfidenceContext,
) -> List(ConfidenceFactor) {
  case result, context.z3_timeout {
    Valid, False -> [
      ConfidenceFactor(
        factor: "z3_proof_complete",
        contribution: weight_full_z3_proof,
        description: "Z3 solver proved validity with complete proof",
      ),
    ]
    Invalid(_), False -> [
      ConfidenceFactor(
        factor: "z3_countermodel_found",
        contribution: weight_countermodel_found,
        description: "Z3 solver found concrete countermodel",
      ),
    ]
    _, True -> [
      ConfidenceFactor(
        factor: "z3_timeout",
        contribution: weight_timeout_partial,
        description: "Z3 solver timed out before completion",
      ),
    ]
    _, _ -> [
      ConfidenceFactor(
        factor: "z3_unknown",
        contribution: weight_unknown,
        description: "Z3 solver returned unknown result",
      ),
    ]
  }
}

/// Common adjustment factors that apply to all tiers
fn common_adjustment_factors(
  context: ConfidenceContext,
) -> List(ConfidenceFactor) {
  let mut_factors = []

  // Frame completeness bonus
  let factors_with_frame = case context.frame_complete {
    True -> [
      ConfidenceFactor(
        factor: "frame_complete",
        contribution: weight_frame_complete,
        description: "All frame conditions verified",
      ),
      ..mut_factors
    ]
    False -> mut_factors
  }

  // World bound penalty
  let factors_with_bound = case context.world_bound_hit {
    True -> [
      ConfidenceFactor(
        factor: "world_bound_hit",
        contribution: penalty_world_bound_hit,
        description: "Exploration limited by world bound (may need more worlds)",
      ),
      ..factors_with_frame
    ]
    False -> factors_with_frame
  }

  // Worlds explored factor
  case context.worlds_explored, context.max_worlds {
    Some(explored), Some(max) if explored < max -> [
      ConfidenceFactor(
        factor: "worlds_under_limit",
        contribution: 0.05,
        description: "Explored "
          <> int.to_string(explored)
          <> " of "
          <> int.to_string(max)
          <> " possible worlds",
      ),
      ..factors_with_bound
    ]
    _, _ -> factors_with_bound
  }
}

/// Sum contributions from all factors
fn sum_contributions(factors: List(ConfidenceFactor)) -> Float {
  factors
  |> list.fold(0.0, fn(acc, factor) { acc +. factor.contribution })
}

/// Classify confidence score into a level
fn classify_confidence(score: Float) -> ConfidenceLevel {
  case score {
    s if s >=. 0.9 -> VeryHigh
    s if s >=. 0.8 -> High
    s if s >=. 0.6 -> Medium
    s if s >=. 0.4 -> Low
    _ -> VeryLow
  }
}

/// Clamp a float value between min and max
fn clamp(value: Float, min: Float, max: Float) -> Float {
  case value {
    v if v <. min -> min
    v if v >. max -> max
    v -> v
  }
}

// =============================================================================
// Convenience Functions
// =============================================================================

/// Create a default context for basic confidence computation
pub fn default_context(tier: ValidationTier) -> ConfidenceContext {
  ConfidenceContext(
    tier_used: tier,
    z3_timeout: False,
    world_bound_hit: False,
    worlds_explored: None,
    max_worlds: None,
    variable_count: None,
    frame_complete: True,
    tier_explanation: None,
  )
}

/// Create context for a successful heuristic result
pub fn heuristic_context(
  tier: ValidationTier,
  explanation: String,
) -> ConfidenceContext {
  ConfidenceContext(
    tier_used: tier,
    z3_timeout: False,
    world_bound_hit: False,
    worlds_explored: None,
    max_worlds: None,
    variable_count: None,
    frame_complete: True,
    tier_explanation: Some(explanation),
  )
}

/// Create context for Z3 result
pub fn z3_context(
  timeout: Bool,
  world_bound_hit: Bool,
  worlds_explored: Option(Int),
  max_worlds: Int,
) -> ConfidenceContext {
  ConfidenceContext(
    tier_used: Tier3Z3,
    z3_timeout: timeout,
    world_bound_hit: world_bound_hit,
    worlds_explored: worlds_explored,
    max_worlds: Some(max_worlds),
    variable_count: None,
    frame_complete: !world_bound_hit,
    tier_explanation: None,
  )
}

/// Create a scored validation result from components
pub fn create_scored_result(
  result: ValidationResult,
  tier: ValidationTier,
  context: ConfidenceContext,
) -> ScoredValidationResult {
  let confidence = compute_confidence(result, context)

  ScoredValidationResult(
    result: result,
    confidence: confidence,
    tier_used: tier,
    tier_explanation: context.tier_explanation,
  )
}

/// Quick function to compute confidence for heuristic results
pub fn confidence_for_heuristic(
  result: ValidationResult,
  tier: ValidationTier,
  explanation: String,
) -> ConfidenceResult {
  let context = heuristic_context(tier, explanation)
  compute_confidence(result, context)
}

/// Quick function to compute confidence for Z3 results
pub fn confidence_for_z3(
  result: ValidationResult,
  timeout: Bool,
  world_bound_hit: Bool,
) -> ConfidenceResult {
  let context = z3_context(timeout, world_bound_hit, None, 100)
  compute_confidence(result, context)
}

// =============================================================================
// Formatting
// =============================================================================

/// Format confidence level as string
pub fn level_to_string(level: ConfidenceLevel) -> String {
  case level {
    VeryHigh -> "Very High"
    High -> "High"
    Medium -> "Medium"
    Low -> "Low"
    VeryLow -> "Very Low"
  }
}

/// Format confidence result for display
pub fn format_confidence(conf: ConfidenceResult) -> String {
  let score_pct =
    float.to_string(conf.score *. 100.0)
    |> string.slice(0, 5)

  "Confidence: " <> score_pct <> "% (" <> level_to_string(conf.level) <> ")"
}

/// Format confidence factors for detailed display
pub fn format_factors(factors: List(ConfidenceFactor)) -> String {
  factors
  |> list.map(fn(f) {
    let sign = case f.contribution >=. 0.0 {
      True -> "+"
      False -> ""
    }
    let pct = float.to_string(f.contribution *. 100.0) |> string.slice(0, 5)
    "  " <> sign <> pct <> "%: " <> f.description
  })
  |> string.join("\n")
}

/// Format scored result for display
pub fn format_scored_result(scored: ScoredValidationResult) -> String {
  let result_str = case scored.result {
    Valid -> "VALID"
    Invalid(cm) -> "INVALID\n  Countermodel: " <> cm
    argument.Unknown(reason) -> "UNKNOWN (" <> reason <> ")"
    argument.Timeout -> "TIMEOUT"
    argument.Error(msg) -> "ERROR: " <> msg
  }

  let tier_str = case scored.tier_used {
    Tier1Syntactic -> "Tier 1 (Syntactic)"
    Tier2TruthTable -> "Tier 2 (Truth Table)"
    Tier3Z3 -> "Tier 3 (Z3)"
  }

  result_str
  <> "\n"
  <> format_confidence(scored.confidence)
  <> "\nValidation Tier: "
  <> tier_str
  <> "\n\nConfidence Factors:\n"
  <> format_factors(scored.confidence.factors)
}

// =============================================================================
// JSON Serialization
// =============================================================================

/// Convert confidence result to JSON string
pub fn confidence_to_json(conf: ConfidenceResult) -> String {
  let factors_json =
    conf.factors
    |> list.map(factor_to_json)
    |> string.join(",\n      ")

  "{\n"
  <> "    \"score\": "
  <> float.to_string(conf.score)
  <> ",\n"
  <> "    \"level\": \""
  <> level_to_string(conf.level)
  <> "\",\n"
  <> "    \"is_high_confidence\": "
  <> bool_to_json(conf.is_high_confidence)
  <> ",\n"
  <> "    \"factors\": [\n      "
  <> factors_json
  <> "\n    ]\n"
  <> "  }"
}

/// Convert a single factor to JSON
fn factor_to_json(f: ConfidenceFactor) -> String {
  "{"
  <> "\"factor\": \""
  <> f.factor
  <> "\", "
  <> "\"contribution\": "
  <> float.to_string(f.contribution)
  <> ", "
  <> "\"description\": \""
  <> escape_json_string(f.description)
  <> "\"}"
}

/// Convert scored result to JSON
pub fn scored_result_to_json(scored: ScoredValidationResult) -> String {
  let result_json = case scored.result {
    Valid -> "\"validity\": \"valid\""
    Invalid(cm) ->
      "\"validity\": \"invalid\", \"countermodel\": \""
      <> escape_json_string(cm)
      <> "\""
    argument.Unknown(reason) ->
      "\"validity\": \"unknown\", \"reason\": \""
      <> escape_json_string(reason)
      <> "\""
    argument.Timeout -> "\"validity\": \"timeout\""
    argument.Error(msg) ->
      "\"validity\": \"error\", \"message\": \""
      <> escape_json_string(msg)
      <> "\""
  }

  let tier_str = case scored.tier_used {
    Tier1Syntactic -> "tier1_syntactic"
    Tier2TruthTable -> "tier2_truth_table"
    Tier3Z3 -> "tier3_z3"
  }

  let explanation_json = case scored.tier_explanation {
    Some(exp) ->
      ",\n  \"tier_explanation\": \"" <> escape_json_string(exp) <> "\""
    None -> ""
  }

  "{\n"
  <> "  "
  <> result_json
  <> ",\n"
  <> "  \"tier\": \""
  <> tier_str
  <> "\""
  <> explanation_json
  <> ",\n"
  <> "  \"confidence\": "
  <> confidence_to_json(scored.confidence)
  <> "\n}"
}

fn bool_to_json(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}
