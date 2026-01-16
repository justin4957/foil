//// Validity Trace Module
////
//// This module provides step-by-step tracing for modal logic validation,
//// showing how validity was determined and identifying critical paths
//// that lead to the final result.
////
//// ## Purpose
//// - Understand *why* reasoning is valid/invalid
//// - Identify which specific step fails
//// - Explain the assessment to stakeholders
//// - Provide transparent validation for prediction accuracy
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/validity_trace
////
//// let trace = validity_trace.generate_trace(formalization, result, config)
//// let formatted = validity_trace.format_trace(trace, DetailedFormat)
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/argument.{
  type Formalization, type ValidationResult, Invalid, Unknown, Valid,
}
import modal_logic/heuristics.{
  type ValidationTier, Tier1Syntactic, Tier2TruthTable, Tier3Z3,
}
import modal_logic/proof_tree.{type InferenceRule, type WorldState, WorldState}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, K4, KD,
  KD45, Knows, Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}
import modal_logic/validator.{type ValidationResponse, ValidationResponse}

// =============================================================================
// Types
// =============================================================================

/// Complete validity trace for a validation
pub type ValidityTrace {
  ValidityTrace(
    /// Step-by-step trace entries
    steps: List(TraceStep),
    /// Final validation result
    final_result: ValidationResult,
    /// Indices of steps leading to result (critical path)
    critical_path: List(Int),
    /// Metadata about the trace
    metadata: TraceMetadata,
  )
}

/// Metadata about the trace generation
pub type TraceMetadata {
  TraceMetadata(
    /// Logic system being validated
    logic_system: LogicSystem,
    /// Validation tier used
    tier_used: ValidationTier,
    /// Total duration of validation in ms
    duration_ms: Int,
    /// Number of worlds in final model
    world_count: Int,
    /// Confidence in the result (0.0 to 1.0)
    confidence: Float,
    /// Whether a countermodel was found
    has_countermodel: Bool,
  )
}

/// A single step in the validity trace
pub type TraceStep {
  TraceStep(
    /// Step number (1-indexed)
    step_number: Int,
    /// Human-readable description
    description: String,
    /// Category of this step
    category: StepCategory,
    /// Input formulas for this step
    input_formulas: List(String),
    /// Output formula (if produced)
    output_formula: Option(String),
    /// Rule or axiom applied
    rule_applied: Option(String),
    /// Result of this step
    result: StepResult,
    /// World state at this step (for modal logic)
    world_state: Option(TraceWorldState),
    /// Duration of this step in ms
    duration_ms: Int,
    /// Confidence contribution
    confidence_delta: Float,
  )
}

/// Categories of trace steps
pub type StepCategory {
  /// Initial setup and frame construction
  FrameSetup
  /// Premise introduction
  PremiseIntroduction
  /// Conclusion checking
  ConclusionCheck
  /// Modal operator processing
  ModalProcessing
  /// World exploration
  WorldExploration
  /// Accessibility relation checking
  AccessibilityCheck
  /// Rule application
  RuleApplication
  /// Countermodel construction
  CountermodelConstruction
  /// Final result determination
  FinalDetermination
  /// Heuristic tier evaluation
  HeuristicEvaluation
}

/// Result of a trace step
pub type StepResult {
  /// Step passed with explanation
  Passed(explanation: String)
  /// Step failed with reason and optional counterexample
  Failed(reason: String, counterexample: Option(String))
  /// Step was inconclusive
  Inconclusive(reason: String)
  /// Step is informational only
  Info(detail: String)
}

/// World state for modal logic traces
pub type TraceWorldState {
  TraceWorldState(
    /// Current worlds in the model
    worlds: List(TraceWorld),
    /// Accessibility relations
    accessibility: List(#(String, String)),
    /// The actual/designated world
    actual_world: String,
    /// Frame properties satisfied
    frame_properties: List(FrameProperty),
  )
}

/// A world in the trace
pub type TraceWorld {
  TraceWorld(
    /// World name
    name: String,
    /// Atoms true in this world
    true_atoms: List(String),
    /// Atoms false in this world
    false_atoms: List(String),
    /// Whether premises hold here
    premises_hold: Bool,
    /// Whether conclusion holds here
    conclusion_holds: Bool,
  )
}

/// Frame properties for logic systems
pub type FrameProperty {
  /// Reflexivity: every world accesses itself
  Reflexive
  /// Symmetry: if w accesses v, then v accesses w
  Symmetric
  /// Transitivity: if w accesses v and v accesses u, then w accesses u
  Transitive
  /// Seriality: every world accesses at least one world
  Serial
  /// Euclidean: if w accesses v and w accesses u, then v accesses u
  Euclidean
}

/// Trace format style
pub type TraceFormatStyle {
  /// Brief one-line per step
  BriefTraceFormat
  /// Standard multi-line format
  StandardTraceFormat
  /// Detailed format with all information
  DetailedTraceFormat
  /// JSON format for serialization
  JsonTraceFormat
}

/// Configuration for trace generation
pub type TraceConfig {
  TraceConfig(
    /// Include world states in trace
    include_world_states: Bool,
    /// Include timing information
    include_timing: Bool,
    /// Include confidence deltas
    include_confidence: Bool,
    /// Maximum steps to record (0 = unlimited)
    max_steps: Int,
    /// Verbosity level (1-3)
    verbosity: Int,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Default trace configuration
pub fn default_config() -> TraceConfig {
  TraceConfig(
    include_world_states: True,
    include_timing: True,
    include_confidence: True,
    max_steps: 0,
    verbosity: 2,
  )
}

/// Brief trace configuration
pub fn brief_config() -> TraceConfig {
  TraceConfig(
    include_world_states: False,
    include_timing: False,
    include_confidence: False,
    max_steps: 10,
    verbosity: 1,
  )
}

/// Detailed trace configuration
pub fn detailed_config() -> TraceConfig {
  TraceConfig(
    include_world_states: True,
    include_timing: True,
    include_confidence: True,
    max_steps: 0,
    verbosity: 3,
  )
}

// =============================================================================
// Trace Generation
// =============================================================================

/// Generate a validity trace from a validation response
pub fn generate_trace(
  formalization: Formalization,
  response: ValidationResponse,
  config: TraceConfig,
) -> ValidityTrace {
  let tier = option.unwrap(response.tier_used, Tier3Z3)
  let steps = generate_trace_steps(formalization, response, tier, config)
  let critical_path = identify_critical_path(steps, response.result)
  let world_count = count_worlds_from_result(response.result)

  ValidityTrace(
    steps: steps,
    final_result: response.result,
    critical_path: critical_path,
    metadata: TraceMetadata(
      logic_system: formalization.logic_system,
      tier_used: tier,
      duration_ms: response.duration_ms,
      world_count: world_count,
      confidence: calculate_trace_confidence(steps),
      has_countermodel: has_countermodel(response.result),
    ),
  )
}

/// Generate trace from a formalization and result directly
pub fn generate_trace_from_result(
  formalization: Formalization,
  result: ValidationResult,
  tier: ValidationTier,
  duration_ms: Int,
  config: TraceConfig,
) -> ValidityTrace {
  let response =
    ValidationResponse(
      request_id: "trace-gen",
      result: result,
      from_cache: False,
      duration_ms: duration_ms,
      worlds_explored: None,
      smt_formula: None,
      tier_used: Some(tier),
      tier_explanation: None,
    )
  generate_trace(formalization, response, config)
}

/// Generate trace steps based on validation tier and result
fn generate_trace_steps(
  formalization: Formalization,
  response: ValidationResponse,
  tier: ValidationTier,
  config: TraceConfig,
) -> List(TraceStep) {
  let base_steps = case tier {
    Tier1Syntactic -> generate_tier1_steps(formalization, response, config)
    Tier2TruthTable -> generate_tier2_steps(formalization, response, config)
    Tier3Z3 -> generate_tier3_steps(formalization, response, config)
  }

  // Add final result step
  let final_step =
    create_final_step(formalization, response, list.length(base_steps) + 1)
  list.append(base_steps, [final_step])
}

/// Generate steps for Tier 1 (syntactic) validation
fn generate_tier1_steps(
  formalization: Formalization,
  response: ValidationResponse,
  config: TraceConfig,
) -> List(TraceStep) {
  let system_name = logic_system_to_string(formalization.logic_system)

  // Step 1: Pattern analysis
  let pattern_step =
    TraceStep(
      step_number: 1,
      description: "Syntactic pattern analysis",
      category: HeuristicEvaluation,
      input_formulas: list.map(formalization.premises, proposition_to_string),
      output_formula: None,
      rule_applied: Some("Tier 1: Pattern Matching"),
      result: Passed("Checking for known syntactic patterns"),
      world_state: None,
      duration_ms: response.duration_ms / 2,
      confidence_delta: 0.0,
    )

  // Step 2: Tautology/Identity check
  let check_result = case response.result {
    Valid -> Passed("Identified as valid pattern")
    Invalid(_) -> Failed("Identified as invalid pattern", None)
    Unknown(_) -> Inconclusive("No pattern match found")
    _ -> Info("Syntactic check completed")
  }

  let check_step =
    TraceStep(
      step_number: 2,
      description: "Check for tautology, identity, or contradiction patterns",
      category: HeuristicEvaluation,
      input_formulas: [proposition_to_string(formalization.conclusion)],
      output_formula: None,
      rule_applied: Some("Tautology/Contradiction Detection"),
      result: check_result,
      world_state: None,
      duration_ms: response.duration_ms / 2,
      confidence_delta: case response.result {
        Valid -> 0.7
        Invalid(_) -> 0.7
        _ -> 0.0
      },
    )

  [pattern_step, check_step]
}

/// Generate steps for Tier 2 (truth table) validation
fn generate_tier2_steps(
  formalization: Formalization,
  response: ValidationResponse,
  config: TraceConfig,
) -> List(TraceStep) {
  let atoms = extract_atoms_from_formalization(formalization)
  let atom_count = list.length(atoms)
  let row_count = pow2(atom_count)

  // Step 1: Variable enumeration
  let enum_step =
    TraceStep(
      step_number: 1,
      description: "Enumerate propositional variables",
      category: HeuristicEvaluation,
      input_formulas: atoms,
      output_formula: None,
      rule_applied: Some("Tier 2: Truth Table"),
      result: Passed(
        "Found " <> int.to_string(atom_count) <> " propositional variables",
      ),
      world_state: None,
      duration_ms: 1,
      confidence_delta: 0.0,
    )

  // Step 2: Truth table construction
  let construct_step =
    TraceStep(
      step_number: 2,
      description: "Construct truth table with "
        <> int.to_string(row_count)
        <> " rows",
      category: HeuristicEvaluation,
      input_formulas: [],
      output_formula: None,
      rule_applied: Some("Complete Enumeration"),
      result: Passed(
        "Evaluating all " <> int.to_string(row_count) <> " possible valuations",
      ),
      world_state: None,
      duration_ms: response.duration_ms / 3,
      confidence_delta: 0.0,
    )

  // Step 3: Evaluation
  let eval_result = case response.result {
    Valid ->
      Passed("All rows where premises are true also have conclusion true")
    Invalid(_) ->
      Failed("Found row(s) where premises true but conclusion false", None)
    _ -> Inconclusive("Evaluation incomplete")
  }

  let eval_step =
    TraceStep(
      step_number: 3,
      description: "Evaluate argument validity across all valuations",
      category: HeuristicEvaluation,
      input_formulas: [],
      output_formula: None,
      rule_applied: Some("Truth-Functional Evaluation"),
      result: eval_result,
      world_state: None,
      duration_ms: response.duration_ms * 2 / 3,
      confidence_delta: case response.result {
        Valid -> 0.85
        Invalid(_) -> 0.85
        _ -> 0.0
      },
    )

  [enum_step, construct_step, eval_step]
}

/// Generate steps for Tier 3 (Z3 SMT) validation
fn generate_tier3_steps(
  formalization: Formalization,
  response: ValidationResponse,
  config: TraceConfig,
) -> List(TraceStep) {
  let system = formalization.logic_system
  let frame_props = get_frame_properties(system)
  let world_count = option.unwrap(response.worlds_explored, 3)

  // Step 1: Frame setup
  let frame_step =
    TraceStep(
      step_number: 1,
      description: "Create Kripke frame for " <> logic_system_to_string(system),
      category: FrameSetup,
      input_formulas: [],
      output_formula: None,
      rule_applied: Some("Frame Construction"),
      result: Passed(
        "Created frame with "
        <> int.to_string(world_count)
        <> " worlds, "
        <> describe_frame_properties(frame_props),
      ),
      world_state: case config.include_world_states {
        True -> Some(initial_world_state(world_count, frame_props))
        False -> None
      },
      duration_ms: response.duration_ms / 10,
      confidence_delta: 0.1,
    )

  // Step 2: Premise assertion
  let premise_step =
    TraceStep(
      step_number: 2,
      description: "Assert premises at actual world w0",
      category: PremiseIntroduction,
      input_formulas: list.map(formalization.premises, proposition_to_string),
      output_formula: None,
      rule_applied: Some("Premise Assertion"),
      result: Passed("Premises must hold at designated world"),
      world_state: None,
      duration_ms: response.duration_ms / 10,
      confidence_delta: 0.0,
    )

  // Step 3: Conclusion negation
  let negate_step =
    TraceStep(
      step_number: 3,
      description: "Negate conclusion for refutation check",
      category: ConclusionCheck,
      input_formulas: [proposition_to_string(formalization.conclusion)],
      output_formula: Some(
        "¬(" <> proposition_to_string(formalization.conclusion) <> ")",
      ),
      rule_applied: Some("Refutation Setup"),
      result: Passed("Checking if premises ∧ ¬conclusion is satisfiable"),
      world_state: None,
      duration_ms: response.duration_ms / 10,
      confidence_delta: 0.0,
    )

  // Step 4: Z3 solving
  let solve_result = case response.result {
    Valid -> Passed("UNSAT - No model where premises true and conclusion false")
    Invalid(cm) ->
      Failed(
        "SAT - Found countermodel",
        Some(string.slice(cm, 0, 100) <> "..."),
      )
    Unknown(reason) -> Inconclusive("Z3 returned unknown: " <> reason)
    _ -> Info("Solver completed")
  }

  let solve_step =
    TraceStep(
      step_number: 4,
      description: "Run Z3 SMT solver",
      category: FinalDetermination,
      input_formulas: [],
      output_formula: None,
      rule_applied: Some("SMT Solving"),
      result: solve_result,
      world_state: None,
      duration_ms: response.duration_ms * 6 / 10,
      confidence_delta: case response.result {
        Valid -> 0.95
        Invalid(_) -> 0.9
        Unknown(_) -> 0.1
        _ -> 0.0
      },
    )

  // Step 5: Model extraction (if invalid)
  case response.result {
    Invalid(countermodel_str) -> {
      let extract_step =
        TraceStep(
          step_number: 5,
          description: "Extract countermodel from Z3 solution",
          category: CountermodelConstruction,
          input_formulas: [],
          output_formula: Some(countermodel_str),
          rule_applied: Some("Model Extraction"),
          result: Failed(
            "Countermodel demonstrates invalidity",
            Some(summarize_countermodel(countermodel_str)),
          ),
          world_state: case config.include_world_states {
            True ->
              Some(parse_countermodel_to_world_state(countermodel_str, system))
            False -> None
          },
          duration_ms: response.duration_ms / 10,
          confidence_delta: 0.05,
        )
      [frame_step, premise_step, negate_step, solve_step, extract_step]
    }
    _ -> [frame_step, premise_step, negate_step, solve_step]
  }
}

/// Create the final determination step
fn create_final_step(
  formalization: Formalization,
  response: ValidationResponse,
  step_num: Int,
) -> TraceStep {
  let result_str = case response.result {
    Valid -> "VALID"
    Invalid(_) -> "INVALID"
    Unknown(_) -> "UNKNOWN"
    _ -> "ERROR"
  }

  let explanation = case response.result {
    Valid ->
      "The argument is logically valid - the conclusion necessarily follows from the premises"
    Invalid(_) ->
      "The argument is invalid - a countermodel exists showing premises can be true while conclusion is false"
    Unknown(r) -> "Validity could not be determined: " <> r
    _ -> "Validation encountered an error"
  }

  TraceStep(
    step_number: step_num,
    description: "Final determination: " <> result_str,
    category: FinalDetermination,
    input_formulas: [],
    output_formula: None,
    rule_applied: None,
    result: case response.result {
      Valid -> Passed(explanation)
      Invalid(_) -> Failed(explanation, None)
      _ -> Inconclusive(explanation)
    },
    world_state: None,
    duration_ms: 0,
    confidence_delta: 0.0,
  )
}

// =============================================================================
// Critical Path Analysis
// =============================================================================

/// Identify the critical path leading to the final result
pub fn identify_critical_path(
  steps: List(TraceStep),
  result: ValidationResult,
) -> List(Int) {
  // For valid results, the critical path is all steps that contributed positively
  // For invalid results, the path leads to the first failure
  let critical_steps = case result {
    Valid -> find_positive_contribution_steps(steps)
    Invalid(_) -> find_failure_path(steps)
    Unknown(_) -> find_inconclusive_steps(steps)
    _ -> []
  }

  // Always include the final step
  let final_step_num = list.length(steps)
  case list.contains(critical_steps, final_step_num) {
    True -> critical_steps
    False -> list.append(critical_steps, [final_step_num])
  }
}

/// Find steps that contributed positively to validity
fn find_positive_contribution_steps(steps: List(TraceStep)) -> List(Int) {
  steps
  |> list.filter(fn(step) {
    case step.result {
      Passed(_) -> step.confidence_delta >. 0.0
      _ -> False
    }
  })
  |> list.map(fn(step) { step.step_number })
}

/// Find the path leading to the first failure
fn find_failure_path(steps: List(TraceStep)) -> List(Int) {
  // Include all steps up to and including the first failure
  let #(path, _found) =
    list.fold(steps, #([], False), fn(acc, step) {
      let #(path, found) = acc
      case found {
        True -> acc
        False -> {
          let new_path = list.append(path, [step.step_number])
          case step.result {
            Failed(_, _) -> #(new_path, True)
            _ -> #(new_path, False)
          }
        }
      }
    })
  path
}

/// Find steps that led to inconclusive result
fn find_inconclusive_steps(steps: List(TraceStep)) -> List(Int) {
  steps
  |> list.filter(fn(step) {
    case step.result {
      Inconclusive(_) -> True
      _ -> False
    }
  })
  |> list.map(fn(step) { step.step_number })
}

// =============================================================================
// Formatting
// =============================================================================

/// Format a validity trace for display
pub fn format_trace(trace: ValidityTrace, style: TraceFormatStyle) -> String {
  case style {
    BriefTraceFormat -> format_brief(trace)
    StandardTraceFormat -> format_standard(trace)
    DetailedTraceFormat -> format_detailed(trace)
    JsonTraceFormat -> format_json(trace)
  }
}

/// Format trace in brief style
fn format_brief(trace: ValidityTrace) -> String {
  let result_str = case trace.final_result {
    Valid -> "VALID"
    Invalid(_) -> "INVALID"
    Unknown(_) -> "UNKNOWN"
    _ -> "ERROR"
  }

  let step_count = list.length(trace.steps)
  let critical_count = list.length(trace.critical_path)

  "Result: "
  <> result_str
  <> " | Steps: "
  <> int.to_string(step_count)
  <> " | Critical: "
  <> int.to_string(critical_count)
  <> " | Confidence: "
  <> format_percentage(trace.metadata.confidence)
  <> " | Tier: "
  <> tier_to_string(trace.metadata.tier_used)
}

/// Format trace in standard style
fn format_standard(trace: ValidityTrace) -> String {
  let header = build_header(trace)
  let steps = list.map(trace.steps, format_step_standard)
  let footer = build_footer(trace)

  string.join([header, ..list.append(steps, [footer])], "\n\n")
}

/// Format trace in detailed style
fn format_detailed(trace: ValidityTrace) -> String {
  let header = build_detailed_header(trace)
  let steps =
    list.map(trace.steps, fn(step) {
      format_step_detailed(
        step,
        list.contains(trace.critical_path, step.step_number),
      )
    })
  let footer = build_detailed_footer(trace)

  string.join([header, ..list.append(steps, [footer])], "\n\n")
}

/// Format trace as JSON
fn format_json(trace: ValidityTrace) -> String {
  let result_str = case trace.final_result {
    Valid -> "\"valid\""
    Invalid(cm) -> "\"invalid\""
    Unknown(r) -> "\"unknown\""
    _ -> "\"error\""
  }

  let steps_json =
    trace.steps
    |> list.map(step_to_json)
    |> string.join(",\n    ")

  let critical_json =
    trace.critical_path
    |> list.map(int.to_string)
    |> string.join(", ")

  "{\n"
  <> "  \"result\": "
  <> result_str
  <> ",\n"
  <> "  \"steps\": [\n    "
  <> steps_json
  <> "\n  ],\n"
  <> "  \"critical_path\": ["
  <> critical_json
  <> "],\n"
  <> "  \"metadata\": {\n"
  <> "    \"logic_system\": \""
  <> logic_system_to_string(trace.metadata.logic_system)
  <> "\",\n"
  <> "    \"tier_used\": \""
  <> tier_to_string(trace.metadata.tier_used)
  <> "\",\n"
  <> "    \"duration_ms\": "
  <> int.to_string(trace.metadata.duration_ms)
  <> ",\n"
  <> "    \"world_count\": "
  <> int.to_string(trace.metadata.world_count)
  <> ",\n"
  <> "    \"confidence\": "
  <> float.to_string(trace.metadata.confidence)
  <> "\n"
  <> "  }\n"
  <> "}"
}

/// Build header for standard format
fn build_header(trace: ValidityTrace) -> String {
  let system = logic_system_to_string(trace.metadata.logic_system)
  let result = case trace.final_result {
    Valid -> "VALID"
    Invalid(_) -> "INVALID"
    Unknown(_) -> "UNKNOWN"
    _ -> "ERROR"
  }

  string.repeat("=", 60)
  <> "\n"
  <> "Validity Trace for "
  <> system
  <> " validation\n"
  <> string.repeat("=", 60)
  <> "\n"
  <> "Result: "
  <> result
  <> " (confidence: "
  <> format_percentage(trace.metadata.confidence)
  <> ")"
}

/// Build detailed header
fn build_detailed_header(trace: ValidityTrace) -> String {
  let system = logic_system_to_string(trace.metadata.logic_system)
  let result = case trace.final_result {
    Valid -> "VALID"
    Invalid(_) -> "INVALID"
    Unknown(_) -> "UNKNOWN"
    _ -> "ERROR"
  }
  let frame_props = get_frame_properties(trace.metadata.logic_system)

  string.repeat("=", 70)
  <> "\n"
  <> "VALIDITY TRACE\n"
  <> string.repeat("=", 70)
  <> "\n\n"
  <> "Logic System:    "
  <> system
  <> "\n"
  <> "Frame Properties: "
  <> describe_frame_properties(frame_props)
  <> "\n"
  <> "Validation Tier: "
  <> tier_to_string(trace.metadata.tier_used)
  <> "\n"
  <> "Duration:        "
  <> int.to_string(trace.metadata.duration_ms)
  <> "ms\n"
  <> "Worlds Explored: "
  <> int.to_string(trace.metadata.world_count)
  <> "\n"
  <> "Final Result:    "
  <> result
  <> "\n"
  <> "Confidence:      "
  <> format_percentage(trace.metadata.confidence)
  <> "\n"
  <> string.repeat("-", 70)
}

/// Format a single step in standard style
fn format_step_standard(step: TraceStep) -> String {
  let status = case step.result {
    Passed(_) -> "✓"
    Failed(_, _) -> "✗"
    Inconclusive(_) -> "?"
    Info(_) -> "ℹ"
  }

  let result_text = case step.result {
    Passed(exp) -> exp
    Failed(reason, _) -> reason
    Inconclusive(reason) -> reason
    Info(detail) -> detail
  }

  "Step "
  <> int.to_string(step.step_number)
  <> ": ["
  <> category_to_string(step.category)
  <> "]\n"
  <> "  "
  <> step.description
  <> "\n"
  <> "  "
  <> status
  <> " "
  <> result_text
}

/// Format a single step in detailed style
fn format_step_detailed(step: TraceStep, is_critical: Bool) -> String {
  let critical_marker = case is_critical {
    True -> " ★ CRITICAL"
    False -> ""
  }

  let status = case step.result {
    Passed(_) -> "✓ Passed"
    Failed(_, _) -> "✗ Failed"
    Inconclusive(_) -> "? Inconclusive"
    Info(_) -> "ℹ Info"
  }

  let result_text = case step.result {
    Passed(exp) -> exp
    Failed(reason, counter) -> {
      let base = reason
      case counter {
        Some(c) -> base <> "\n    Counterexample: " <> c
        None -> base
      }
    }
    Inconclusive(reason) -> reason
    Info(detail) -> detail
  }

  let inputs = case list.is_empty(step.input_formulas) {
    True -> ""
    False -> "\n  Input: " <> string.join(step.input_formulas, ", ")
  }

  let output = case step.output_formula {
    Some(f) -> "\n  Output: " <> f
    None -> ""
  }

  let rule = case step.rule_applied {
    Some(r) -> "\n  Rule: " <> r
    None -> ""
  }

  let world = case step.world_state {
    Some(ws) -> "\n  Worlds: " <> format_world_state_brief(ws)
    None -> ""
  }

  let timing = case step.duration_ms > 0 {
    True -> " (" <> int.to_string(step.duration_ms) <> "ms)"
    False -> ""
  }

  "Step "
  <> int.to_string(step.step_number)
  <> ": ["
  <> category_to_string(step.category)
  <> "]"
  <> critical_marker
  <> timing
  <> "\n"
  <> "  "
  <> step.description
  <> inputs
  <> output
  <> rule
  <> world
  <> "\n"
  <> "  → "
  <> status
  <> ": "
  <> result_text
}

/// Build footer for standard format
fn build_footer(trace: ValidityTrace) -> String {
  let critical_str =
    trace.critical_path
    |> list.map(int.to_string)
    |> string.join(" → ")

  string.repeat("-", 60) <> "\n" <> "Critical Path: Steps " <> critical_str
}

/// Build detailed footer
fn build_detailed_footer(trace: ValidityTrace) -> String {
  let critical_str =
    trace.critical_path
    |> list.map(int.to_string)
    |> string.join(" → ")

  let countermodel_info = case trace.final_result {
    Invalid(cm) -> "\n\nCountermodel:\n" <> cm
    _ -> ""
  }

  string.repeat("-", 70)
  <> "\n"
  <> "SUMMARY\n"
  <> string.repeat("-", 70)
  <> "\n"
  <> "Critical Path: Steps "
  <> critical_str
  <> "\n"
  <> "Total Steps: "
  <> int.to_string(list.length(trace.steps))
  <> "\n"
  <> "Has Countermodel: "
  <> bool_to_string(trace.metadata.has_countermodel)
  <> countermodel_info
  <> "\n"
  <> string.repeat("=", 70)
}

/// Format a step as JSON
fn step_to_json(step: TraceStep) -> String {
  let result_json = case step.result {
    Passed(exp) ->
      "{\"status\": \"passed\", \"explanation\": \""
      <> escape_json(exp)
      <> "\"}"
    Failed(reason, counter) -> {
      let counter_json = case counter {
        Some(c) -> ", \"counterexample\": \"" <> escape_json(c) <> "\""
        None -> ""
      }
      "{\"status\": \"failed\", \"reason\": \""
      <> escape_json(reason)
      <> "\""
      <> counter_json
      <> "}"
    }
    Inconclusive(reason) ->
      "{\"status\": \"inconclusive\", \"reason\": \""
      <> escape_json(reason)
      <> "\"}"
    Info(detail) ->
      "{\"status\": \"info\", \"detail\": \"" <> escape_json(detail) <> "\"}"
  }

  "{"
  <> "\"step\": "
  <> int.to_string(step.step_number)
  <> ", "
  <> "\"category\": \""
  <> category_to_string(step.category)
  <> "\", "
  <> "\"description\": \""
  <> escape_json(step.description)
  <> "\", "
  <> "\"result\": "
  <> result_json
  <> "}"
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Convert logic system to string
pub fn logic_system_to_string(system: LogicSystem) -> String {
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

/// Convert tier to string
fn tier_to_string(tier: ValidationTier) -> String {
  case tier {
    Tier1Syntactic -> "Tier 1 (Syntactic)"
    Tier2TruthTable -> "Tier 2 (Truth Table)"
    Tier3Z3 -> "Tier 3 (Z3 SMT)"
  }
}

/// Convert category to string
pub fn category_to_string(category: StepCategory) -> String {
  case category {
    FrameSetup -> "Frame Setup"
    PremiseIntroduction -> "Premise Introduction"
    ConclusionCheck -> "Conclusion Check"
    ModalProcessing -> "Modal Processing"
    WorldExploration -> "World Exploration"
    AccessibilityCheck -> "Accessibility Check"
    RuleApplication -> "Rule Application"
    CountermodelConstruction -> "Countermodel Construction"
    FinalDetermination -> "Final Determination"
    HeuristicEvaluation -> "Heuristic Evaluation"
  }
}

/// Convert proposition to string (simplified)
fn proposition_to_string(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "¬" <> proposition_to_string(inner)
    And(left, right) ->
      "("
      <> proposition_to_string(left)
      <> " ∧ "
      <> proposition_to_string(right)
      <> ")"
    Or(left, right) ->
      "("
      <> proposition_to_string(left)
      <> " ∨ "
      <> proposition_to_string(right)
      <> ")"
    Implies(left, right) ->
      "("
      <> proposition_to_string(left)
      <> " → "
      <> proposition_to_string(right)
      <> ")"
    Necessary(inner) -> "□" <> proposition_to_string(inner)
    Possible(inner) -> "◇" <> proposition_to_string(inner)
    Knows(agent, inner) ->
      "K_" <> agent <> "(" <> proposition_to_string(inner) <> ")"
    Believes(agent, inner) ->
      "B_" <> agent <> "(" <> proposition_to_string(inner) <> ")"
    Obligatory(inner) -> "O(" <> proposition_to_string(inner) <> ")"
    Permitted(inner) -> "P(" <> proposition_to_string(inner) <> ")"
  }
}

/// Get frame properties for a logic system
fn get_frame_properties(system: LogicSystem) -> List(FrameProperty) {
  case system {
    K -> []
    T -> [Reflexive]
    K4 -> [Transitive]
    S4 -> [Reflexive, Transitive]
    S5 -> [Reflexive, Symmetric, Transitive]
    KD -> [Serial]
    KD45 -> [Serial, Transitive, Euclidean]
  }
}

/// Describe frame properties
fn describe_frame_properties(props: List(FrameProperty)) -> String {
  case list.is_empty(props) {
    True -> "no special conditions"
    False -> {
      props
      |> list.map(fn(p) {
        case p {
          Reflexive -> "reflexive"
          Symmetric -> "symmetric"
          Transitive -> "transitive"
          Serial -> "serial"
          Euclidean -> "euclidean"
        }
      })
      |> string.join(", ")
    }
  }
}

/// Create initial world state
fn initial_world_state(
  world_count: Int,
  props: List(FrameProperty),
) -> TraceWorldState {
  let worlds =
    list.range(0, world_count - 1)
    |> list.map(fn(i) {
      TraceWorld(
        name: "w" <> int.to_string(i),
        true_atoms: [],
        false_atoms: [],
        premises_hold: i == 0,
        conclusion_holds: False,
      )
    })

  TraceWorldState(
    worlds: worlds,
    accessibility: [],
    actual_world: "w0",
    frame_properties: props,
  )
}

/// Format world state briefly
fn format_world_state_brief(ws: TraceWorldState) -> String {
  let world_names =
    ws.worlds
    |> list.map(fn(w) { w.name })
    |> string.join(", ")

  "{" <> world_names <> "}, actual=" <> ws.actual_world
}

/// Parse countermodel string to world state
fn parse_countermodel_to_world_state(
  cm: String,
  system: LogicSystem,
) -> TraceWorldState {
  // Simple parsing - in production would be more sophisticated
  let props = get_frame_properties(system)

  TraceWorldState(
    worlds: [
      TraceWorld(
        name: "w0",
        true_atoms: [],
        false_atoms: [],
        premises_hold: True,
        conclusion_holds: False,
      ),
      TraceWorld(
        name: "w1",
        true_atoms: [],
        false_atoms: [],
        premises_hold: False,
        conclusion_holds: True,
      ),
    ],
    accessibility: [#("w0", "w1")],
    actual_world: "w0",
    frame_properties: props,
  )
}

/// Summarize a countermodel string
fn summarize_countermodel(cm: String) -> String {
  let lines = string.split(cm, "\n")
  case list.first(lines) {
    Ok(first) -> first
    Error(_) -> "Countermodel found"
  }
}

/// Extract atoms from formalization
fn extract_atoms_from_formalization(
  formalization: Formalization,
) -> List(String) {
  let premise_atoms = list.flat_map(formalization.premises, extract_atoms)
  let conclusion_atoms = extract_atoms(formalization.conclusion)

  list.append(premise_atoms, conclusion_atoms)
  |> list.unique()
  |> list.sort(string.compare)
}

/// Extract atoms from a proposition
fn extract_atoms(prop: Proposition) -> List(String) {
  case prop {
    Atom(name) -> [name]
    Not(inner) -> extract_atoms(inner)
    And(left, right) -> list.append(extract_atoms(left), extract_atoms(right))
    Or(left, right) -> list.append(extract_atoms(left), extract_atoms(right))
    Implies(left, right) ->
      list.append(extract_atoms(left), extract_atoms(right))
    Necessary(inner) -> extract_atoms(inner)
    Possible(inner) -> extract_atoms(inner)
    Knows(_, inner) -> extract_atoms(inner)
    Believes(_, inner) -> extract_atoms(inner)
    Obligatory(inner) -> extract_atoms(inner)
    Permitted(inner) -> extract_atoms(inner)
  }
}

/// Calculate total confidence from steps
fn calculate_trace_confidence(steps: List(TraceStep)) -> Float {
  let total =
    list.fold(steps, 0.0, fn(acc, step) { acc +. step.confidence_delta })

  // Clamp to 0.0 - 1.0
  case total <. 0.0 {
    True -> 0.0
    False ->
      case total >. 1.0 {
        True -> 1.0
        False -> total
      }
  }
}

/// Check if result has countermodel
fn has_countermodel(result: ValidationResult) -> Bool {
  case result {
    Invalid(_) -> True
    _ -> False
  }
}

/// Count worlds from result
fn count_worlds_from_result(result: ValidationResult) -> Int {
  case result {
    Invalid(cm) -> {
      // Count "w" occurrences as rough world count
      let parts = string.split(cm, "w")
      list.length(parts) - 1
      |> int.max(1)
    }
    _ -> 1
  }
}

/// Format percentage
fn format_percentage(value: Float) -> String {
  let pct = value *. 100.0
  let rounded = float.round(pct)
  int.to_string(rounded) <> "%"
}

/// Simple power of 2
fn pow2(n: Int) -> Int {
  case n <= 0 {
    True -> 1
    False -> 2 * pow2(n - 1)
  }
}

/// Boolean to string
fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "Yes"
    False -> "No"
  }
}

/// Escape JSON string
fn escape_json(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

// =============================================================================
// Specialized Trace Generation
// =============================================================================

/// Generate a trace for the T-axiom example (□p → p)
pub fn generate_t_axiom_trace(system: LogicSystem) -> ValidityTrace {
  let is_valid = case system {
    T | S4 | S5 -> True
    _ -> False
  }

  let steps = [
    TraceStep(
      step_number: 1,
      description: "Create Kripke frame for " <> logic_system_to_string(system),
      category: FrameSetup,
      input_formulas: [],
      output_formula: None,
      rule_applied: Some("Frame Construction"),
      result: Passed(
        "Created frame with 3 worlds, "
        <> describe_frame_properties(get_frame_properties(system)),
      ),
      world_state: Some(initial_world_state(3, get_frame_properties(system))),
      duration_ms: 5,
      confidence_delta: 0.1,
    ),
    TraceStep(
      step_number: 2,
      description: "Assume □p holds at world w0",
      category: PremiseIntroduction,
      input_formulas: ["□p"],
      output_formula: None,
      rule_applied: Some("Premise Assertion"),
      result: Passed("This means: p holds at all worlds accessible from w0"),
      world_state: None,
      duration_ms: 2,
      confidence_delta: 0.0,
    ),
    TraceStep(
      step_number: 3,
      description: "Check if p holds at w0",
      category: ConclusionCheck,
      input_formulas: ["p"],
      output_formula: None,
      rule_applied: Some("T-Axiom Check"),
      result: case is_valid {
        True -> Passed("w0 accesses itself (reflexivity), so p must hold at w0")
        False ->
          Failed(
            "w0 may not be accessible from itself (no reflexivity in "
              <> logic_system_to_string(system)
              <> ")",
            Some(
              "Worlds: {w0, w1}, Access: {(w0, w1)}, p: true at w1, false at w0",
            ),
          )
      },
      world_state: None,
      duration_ms: 10,
      confidence_delta: case is_valid {
        True -> 0.85
        False -> 0.9
      },
    ),
    TraceStep(
      step_number: 4,
      description: case is_valid {
        True -> "Final determination: VALID"
        False -> "Final determination: INVALID"
      },
      category: FinalDetermination,
      input_formulas: [],
      output_formula: None,
      rule_applied: None,
      result: case is_valid {
        True ->
          Passed(
            "The T-axiom □p → p is valid in " <> logic_system_to_string(system),
          )
        False ->
          Failed(
            "The T-axiom □p → p is invalid in "
              <> logic_system_to_string(system)
              <> " due to lack of reflexivity",
            None,
          )
      },
      world_state: None,
      duration_ms: 0,
      confidence_delta: 0.0,
    ),
  ]

  let critical_path = case is_valid {
    True -> [1, 2, 3, 4]
    False -> [1, 2, 3, 4]
  }

  ValidityTrace(
    steps: steps,
    final_result: case is_valid {
      True -> Valid
      False ->
        Invalid(
          "Countermodel: Worlds={w0,w1}, Access={(w0,w1)}, p=false@w0, p=true@w1",
        )
    },
    critical_path: critical_path,
    metadata: TraceMetadata(
      logic_system: system,
      tier_used: Tier3Z3,
      duration_ms: 17,
      world_count: case is_valid {
        True -> 1
        False -> 2
      },
      confidence: case is_valid {
        True -> 0.95
        False -> 1.0
      },
      has_countermodel: !is_valid,
    ),
  )
}

/// Generate a trace for modus ponens validation
pub fn generate_modus_ponens_trace(system: LogicSystem) -> ValidityTrace {
  let steps = [
    TraceStep(
      step_number: 1,
      description: "Identify argument structure",
      category: HeuristicEvaluation,
      input_formulas: ["p", "p → q"],
      output_formula: Some("q"),
      rule_applied: Some("Tier 1: Pattern Matching"),
      result: Passed("Recognized modus ponens pattern"),
      world_state: None,
      duration_ms: 1,
      confidence_delta: 0.7,
    ),
    TraceStep(
      step_number: 2,
      description: "Apply modus ponens rule",
      category: RuleApplication,
      input_formulas: ["p", "p → q"],
      output_formula: Some("q"),
      rule_applied: Some("Modus Ponens: From A and A→B, derive B"),
      result: Passed("Conclusion q follows directly from premises"),
      world_state: None,
      duration_ms: 1,
      confidence_delta: 0.25,
    ),
    TraceStep(
      step_number: 3,
      description: "Final determination: VALID",
      category: FinalDetermination,
      input_formulas: [],
      output_formula: None,
      rule_applied: None,
      result: Passed(
        "Modus ponens is a valid inference rule in all logic systems",
      ),
      world_state: None,
      duration_ms: 0,
      confidence_delta: 0.0,
    ),
  ]

  ValidityTrace(
    steps: steps,
    final_result: Valid,
    critical_path: [1, 2, 3],
    metadata: TraceMetadata(
      logic_system: system,
      tier_used: Tier1Syntactic,
      duration_ms: 2,
      world_count: 1,
      confidence: 0.95,
      has_countermodel: False,
    ),
  )
}
