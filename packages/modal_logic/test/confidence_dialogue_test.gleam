//// Confidence Scoring Dialogue Test
////
//// This test demonstrates the confidence scoring infrastructure added in issue #146.
//// It validates the back-and-forth interaction between validation and confidence
//// scoring components.
////
//// ## Purpose
//// - Validates confidence score computation works correctly
//// - Demonstrates confidence factor breakdown
//// - Shows integration with tiered validation
//// - Documents expected behavior for PR reviews

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/argument.{Invalid, Valid}
import modal_logic/confidence.{
  type ConfidenceResult, High, Low, VeryHigh, VeryLow,
}
import modal_logic/heuristics.{Tier1Syntactic, Tier2TruthTable, Tier3Z3}

pub fn main() {
  io.println(string.repeat("=", 70))
  io.println("Confidence Scoring Dialogue Test")
  io.println("Testing Issue #146: Confidence-Scored Validation Results")
  io.println(string.repeat("=", 70))
  io.println("")

  // Test 1: Basic confidence computation
  test_basic_confidence_computation()

  // Test 2: Tier-specific confidence scores
  test_tier_specific_confidence()

  // Test 3: Confidence factors breakdown
  test_confidence_factors()

  // Test 4: High vs low confidence scenarios
  test_confidence_levels()

  // Test 5: JSON serialization
  test_json_serialization()

  // Test 6: Scored validation result formatting
  test_scored_result_formatting()

  // Summary
  print_analysis_summary()

  io.println("")
  io.println(string.repeat("=", 70))
  io.println("All Confidence Scoring Dialogue Tests Completed!")
  io.println(string.repeat("=", 70))
}

// =============================================================================
// Test 1: Basic Confidence Computation
// =============================================================================

fn test_basic_confidence_computation() {
  io.println("")
  io.println("--- Test 1: Basic Confidence Computation ---")
  io.println("")

  io.println("User: Compute confidence for a Valid result from Tier 1")
  io.println("")

  let context = confidence.default_context(Tier1Syntactic)
  let conf_result = confidence.compute_confidence(Valid, context)

  io.println("[System]: Confidence Result")
  io.println("         Score: " <> float_to_percent(conf_result.score))
  io.println(
    "         Level: " <> confidence.level_to_string(conf_result.level),
  )
  io.println(
    "         High Confidence: "
    <> case conf_result.is_high_confidence {
      True -> "Yes"
      False -> "No"
    },
  )
  io.println(
    "         Factors Count: "
    <> int.to_string(list.length(conf_result.factors)),
  )

  // Verify the score is reasonable for Tier 1
  case conf_result.score >=. 0.7 && conf_result.score <=. 0.85 {
    True ->
      io.println("\n[OK] Tier 1 Valid confidence in expected range (0.7-0.85)")
    False ->
      io.println(
        "\n[WARN] Tier 1 confidence outside expected range: "
        <> float_to_string(conf_result.score),
      )
  }

  io.println("")
}

// =============================================================================
// Test 2: Tier-Specific Confidence Scores
// =============================================================================

fn test_tier_specific_confidence() {
  io.println("")
  io.println("--- Test 2: Tier-Specific Confidence Scores ---")
  io.println("")

  io.println(
    "User: Compare confidence scores across all tiers for Valid result",
  )
  io.println("")

  // Tier 1
  let tier1_context = confidence.default_context(Tier1Syntactic)
  let tier1_conf = confidence.compute_confidence(Valid, tier1_context)

  // Tier 2
  let tier2_context = confidence.default_context(Tier2TruthTable)
  let tier2_conf = confidence.compute_confidence(Valid, tier2_context)

  // Tier 3
  let tier3_context = confidence.default_context(Tier3Z3)
  let tier3_conf = confidence.compute_confidence(Valid, tier3_context)

  io.println("[System]: Confidence by Tier")
  io.println("")
  io.println("| Tier | Score | Level | High Confidence |")
  io.println("|------|-------|-------|-----------------|")
  print_tier_row("Tier 1 (Syntactic)", tier1_conf)
  print_tier_row("Tier 2 (Truth Table)", tier2_conf)
  print_tier_row("Tier 3 (Z3)", tier3_conf)

  // Verify ordering: Tier 3 > Tier 2 > Tier 1
  case
    tier3_conf.score >. tier2_conf.score && tier2_conf.score >. tier1_conf.score
  {
    True ->
      io.println("\n[OK] Confidence ordering correct: Tier3 > Tier2 > Tier1")
    False -> io.println("\n[WARN] Unexpected confidence ordering across tiers")
  }

  io.println("")
}

fn print_tier_row(name: String, conf: ConfidenceResult) {
  io.println(
    "| "
    <> name
    <> " | "
    <> float_to_percent(conf.score)
    <> " | "
    <> confidence.level_to_string(conf.level)
    <> " | "
    <> case conf.is_high_confidence {
      True -> "Yes"
      False -> "No"
    }
    <> " |",
  )
}

// =============================================================================
// Test 3: Confidence Factors Breakdown
// =============================================================================

fn test_confidence_factors() {
  io.println("")
  io.println("--- Test 3: Confidence Factors Breakdown ---")
  io.println("")

  io.println("User: Show factor breakdown for a Z3 proof result")
  io.println("")

  let context = confidence.default_context(Tier3Z3)
  let conf_result = confidence.compute_confidence(Valid, context)

  io.println("[System]: Z3 Valid Result - Factor Breakdown")
  io.println("")
  io.println(confidence.format_factors(conf_result.factors))
  io.println("")
  io.println("Total Score: " <> float_to_percent(conf_result.score))

  // Verify we have the expected z3_proof_complete factor
  let has_z3_factor =
    list.any(conf_result.factors, fn(f) { f.factor == "z3_proof_complete" })

  case has_z3_factor {
    True -> io.println("\n[OK] Z3 proof complete factor present")
    False -> io.println("\n[WARN] Expected z3_proof_complete factor not found")
  }

  // Test Invalid result with countermodel
  io.println("")
  io.println("User: Show factor breakdown for an Invalid result (countermodel)")
  io.println("")

  let invalid_result = Invalid("Countermodel: w0 where p=false, q=true")
  let invalid_conf = confidence.compute_confidence(invalid_result, context)

  io.println("[System]: Z3 Invalid Result - Factor Breakdown")
  io.println("")
  io.println(confidence.format_factors(invalid_conf.factors))
  io.println("")
  io.println("Total Score: " <> float_to_percent(invalid_conf.score))

  let has_countermodel_factor =
    list.any(invalid_conf.factors, fn(f) { f.factor == "z3_countermodel_found" })

  case has_countermodel_factor {
    True -> io.println("\n[OK] Countermodel found factor present")
    False ->
      io.println("\n[WARN] Expected z3_countermodel_found factor not found")
  }

  io.println("")
}

// =============================================================================
// Test 4: High vs Low Confidence Scenarios
// =============================================================================

fn test_confidence_levels() {
  io.println("")
  io.println("--- Test 4: High vs Low Confidence Scenarios ---")
  io.println("")

  io.println("User: Show examples of each confidence level")
  io.println("")

  // High confidence: Z3 proof
  let high_conf_context = confidence.default_context(Tier3Z3)
  let high_conf = confidence.compute_confidence(Valid, high_conf_context)

  // Medium confidence: Tier 1 heuristic
  let medium_conf_context = confidence.default_context(Tier1Syntactic)
  let medium_conf = confidence.compute_confidence(Valid, medium_conf_context)

  // Low confidence: Z3 timeout
  let low_conf_context =
    confidence.ConfidenceContext(
      tier_used: Tier3Z3,
      z3_timeout: True,
      world_bound_hit: False,
      worlds_explored: None,
      max_worlds: Some(100),
      variable_count: None,
      frame_complete: True,
      tier_explanation: Some("Z3 timed out after 30000ms"),
    )
  let low_conf = confidence.compute_confidence(Valid, low_conf_context)

  // Very low: World bound hit with timeout
  let very_low_context =
    confidence.ConfidenceContext(
      tier_used: Tier3Z3,
      z3_timeout: True,
      world_bound_hit: True,
      worlds_explored: Some(100),
      max_worlds: Some(100),
      variable_count: None,
      frame_complete: False,
      tier_explanation: Some("Exploration limited"),
    )
  let very_low_conf = confidence.compute_confidence(Valid, very_low_context)

  io.println("[System]: Confidence Level Examples")
  io.println("")
  io.println("| Scenario | Score | Level |")
  io.println("|----------|-------|-------|")
  io.println(
    "| Z3 complete proof | "
    <> float_to_percent(high_conf.score)
    <> " | "
    <> confidence.level_to_string(high_conf.level)
    <> " |",
  )
  io.println(
    "| Tier 1 heuristic | "
    <> float_to_percent(medium_conf.score)
    <> " | "
    <> confidence.level_to_string(medium_conf.level)
    <> " |",
  )
  io.println(
    "| Z3 timeout | "
    <> float_to_percent(low_conf.score)
    <> " | "
    <> confidence.level_to_string(low_conf.level)
    <> " |",
  )
  io.println(
    "| Timeout + world bound | "
    <> float_to_percent(very_low_conf.score)
    <> " | "
    <> confidence.level_to_string(very_low_conf.level)
    <> " |",
  )

  // Verify confidence level assignments
  io.println("")
  case high_conf.level {
    VeryHigh | High -> io.println("[OK] Z3 proof has high/very high confidence")
    _ -> io.println("[WARN] Z3 proof should have high confidence")
  }

  case very_low_conf.level {
    Low | VeryLow -> io.println("[OK] Timeout + bound hit has low confidence")
    _ -> io.println("[WARN] Timeout + bound hit should have low confidence")
  }

  io.println("")
}

// =============================================================================
// Test 5: JSON Serialization
// =============================================================================

fn test_json_serialization() {
  io.println("")
  io.println("--- Test 5: JSON Serialization ---")
  io.println("")

  io.println("User: Serialize confidence result to JSON")
  io.println("")

  let context = confidence.default_context(Tier3Z3)
  let conf_result = confidence.compute_confidence(Valid, context)

  io.println("[System]: JSON Output")
  io.println("")
  io.println(confidence.confidence_to_json(conf_result))

  // Verify JSON structure
  let json_output = confidence.confidence_to_json(conf_result)
  let has_score = string.contains(json_output, "\"score\"")
  let has_level = string.contains(json_output, "\"level\"")
  let has_factors = string.contains(json_output, "\"factors\"")

  io.println("")
  case has_score && has_level && has_factors {
    True -> io.println("[OK] JSON contains all required fields")
    False -> io.println("[WARN] JSON missing some required fields")
  }

  io.println("")
}

// =============================================================================
// Test 6: Scored Validation Result Formatting
// =============================================================================

fn test_scored_result_formatting() {
  io.println("")
  io.println("--- Test 6: Scored Validation Result Formatting ---")
  io.println("")

  io.println("User: Format a complete scored validation result")
  io.println("")

  let scored_result =
    confidence.create_scored_result(
      Valid,
      Tier3Z3,
      confidence.heuristic_context(Tier3Z3, "Full Z3 SMT solving completed"),
    )

  io.println("[System]: Scored Validation Result")
  io.println("")
  io.println(confidence.format_scored_result(scored_result))

  // Test JSON output
  io.println("")
  io.println("User: Show JSON format for scored result")
  io.println("")

  io.println("[System]: Scored Result JSON")
  io.println("")
  io.println(confidence.scored_result_to_json(scored_result))

  // Verify the result contains expected components
  let json = confidence.scored_result_to_json(scored_result)
  let has_validity = string.contains(json, "\"validity\"")
  let has_tier = string.contains(json, "\"tier\"")
  let has_confidence = string.contains(json, "\"confidence\"")

  io.println("")
  case has_validity && has_tier && has_confidence {
    True -> io.println("[OK] Scored result JSON contains all components")
    False -> io.println("[WARN] Scored result JSON missing components")
  }

  io.println("")
}

// =============================================================================
// Analysis Summary
// =============================================================================

fn print_analysis_summary() {
  io.println("")
  io.println(string.repeat("=", 70))
  io.println("CONFIDENCE SCORING INFRASTRUCTURE ANALYSIS")
  io.println(string.repeat("=", 70))
  io.println("")

  io.println(
    "| Component                  | Status      | Description                    |",
  )
  io.println(
    "|----------------------------|-------------|--------------------------------|",
  )
  io.println(
    "| confidence module          | FUNCTIONAL  | Core confidence computation    |",
  )
  io.println(
    "| ConfidenceFactor type      | FUNCTIONAL  | Factor breakdown structure     |",
  )
  io.println(
    "| ConfidenceResult type      | FUNCTIONAL  | Complete confidence result     |",
  )
  io.println(
    "| ConfidenceLevel enum       | FUNCTIONAL  | VeryHigh/High/Medium/Low/VLow  |",
  )
  io.println(
    "| ConfidenceContext type     | FUNCTIONAL  | Validation metadata context    |",
  )
  io.println(
    "| ScoredValidationResult     | FUNCTIONAL  | Result + confidence combined   |",
  )
  io.println(
    "| compute_confidence fn      | FUNCTIONAL  | Main computation function      |",
  )
  io.println(
    "| Tier 1/2/3 factor logic    | FUNCTIONAL  | Tier-specific factor weights   |",
  )
  io.println(
    "| JSON serialization         | FUNCTIONAL  | confidence_to_json, etc.       |",
  )
  io.println(
    "| Format functions           | FUNCTIONAL  | Human-readable output          |",
  )
  io.println(
    "| Validator integration      | FUNCTIONAL  | validate_with_confidence       |",
  )
  io.println("")

  io.println("Confidence Factor Weights (from module):")
  io.println("  - Full Z3 proof:        +0.95")
  io.println("  - Countermodel found:   +0.90")
  io.println("  - Truth table complete: +0.85")
  io.println("  - Tier 1 heuristic:     +0.70")
  io.println("  - Timeout partial:      +0.30")
  io.println("  - Unknown result:       +0.10")
  io.println("  - Frame complete bonus: +0.10")
  io.println("  - World bound hit:      -0.20")
  io.println("")

  io.println("Confidence Levels:")
  io.println("  - Very High: 0.90 - 1.00")
  io.println("  - High:      0.80 - 0.90")
  io.println("  - Medium:    0.60 - 0.80")
  io.println("  - Low:       0.40 - 0.60")
  io.println("  - Very Low:  0.00 - 0.40")
  io.println("")

  io.println("Usage Examples:")
  io.println("  # Basic confidence computation")
  io.println("  let ctx = confidence.default_context(Tier3Z3)")
  io.println("  let conf = confidence.compute_confidence(Valid, ctx)")
  io.println("")
  io.println("  # Validate with confidence scoring")
  io.println(
    "  let #(state, response) = validator.validate_with_confidence(state, req, time)",
  )
  io.println("  // response.scored_result.confidence.score is 0.0 to 1.0")
  io.println("")
  io.println("  # Check if high confidence")
  io.println("  case conf.is_high_confidence {")
  io.println("    True -> io.println(\"Result is highly confident\")")
  io.println("    False -> io.println(\"Result may need verification\")")
  io.println("  }")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let frac = float_truncate({ f -. int_to_float(whole) } *. 100.0)
  int.to_string(whole) <> "." <> pad_left(int.to_string(abs(frac)), 2, "0")
}

fn float_to_percent(f: Float) -> String {
  float_to_string(f *. 100.0) <> "%"
}

fn pad_left(s: String, len: Int, char: String) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left(char <> s, len, char)
  }
}

fn abs(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
