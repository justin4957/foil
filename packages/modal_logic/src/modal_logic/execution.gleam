//// Execution Loop
////
//// This module implements a self-correcting execution loop that:
//// 1. Translates natural language arguments to formal logic
//// 2. Validates the formalization
//// 3. If invalid, generates repair suggestions
//// 4. Applies repairs and re-validates
//// 5. Repeats until valid or max iterations reached
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/execution
////
//// let config = execution.default_config()
//// let result = execution.run(config, argument_text, translate_fn, validate_fn)
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/argument.{type Formalization}
import modal_logic/compiler.{type CompiledTranslation}
import modal_logic/proposition.{type LogicSystem}
import modal_logic/repair
import modal_logic/validator.{type Countermodel, type ValidationResponse}

// =============================================================================
// Types
// =============================================================================

/// Configuration for the execution loop
pub type ExecutionConfig {
  ExecutionConfig(
    /// Maximum translation attempts
    max_translation_attempts: Int,
    /// Maximum validation iterations
    max_validation_iterations: Int,
    /// Maximum repair attempts per iteration
    max_repairs_per_iteration: Int,
    /// Whether to try multiple repair strategies
    try_multiple_strategies: Bool,
    /// Minimum confidence for accepting translation
    min_translation_confidence: Float,
    /// Enable verbose logging
    verbose: Bool,
    /// Timeout per step in milliseconds
    step_timeout_ms: Int,
  )
}

/// Result of the execution loop
pub type ExecutionResult {
  /// Successfully validated argument
  ExecutionSuccess(
    /// Final formalization
    formalization: Formalization,
    /// Validation proof/result
    validation: ValidationResponse,
    /// Execution trace
    trace: ExecutionTrace,
  )
  /// Argument is invalid with repairs exhausted
  ExecutionInvalid(
    /// Best formalization achieved
    formalization: Formalization,
    /// Final countermodel
    countermodel: Option(Countermodel),
    /// Remaining repair suggestions
    remaining_repairs: List(argument.RepairSuggestion),
    /// Execution trace
    trace: ExecutionTrace,
  )
  /// Execution failed with error
  ExecutionError(
    /// Error description
    error: ExecutionError,
    /// Partial trace
    trace: ExecutionTrace,
  )
}

/// Errors that can occur during execution
pub type ExecutionError {
  /// Translation failed
  TranslationFailed(reason: String)
  /// Validation failed unexpectedly
  ValidationFailed(reason: String)
  /// Timeout reached
  TimeoutReached(step: String)
  /// Max iterations exceeded
  MaxIterationsExceeded
  /// Configuration error
  ConfigurationError(reason: String)
}

/// Trace of execution for debugging and analysis
pub type ExecutionTrace {
  ExecutionTrace(
    /// All steps taken
    steps: List(ExecutionStep),
    /// Total time in milliseconds
    total_time_ms: Int,
    /// Number of translation attempts
    translation_attempts: Int,
    /// Number of validation iterations
    validation_iterations: Int,
    /// Number of repairs applied
    repairs_applied: Int,
  )
}

/// A single step in the execution
pub type ExecutionStep {
  ExecutionStep(
    /// Step type
    step_type: StepType,
    /// Step description
    description: String,
    /// Time taken in milliseconds
    duration_ms: Int,
    /// Whether step succeeded
    success: Bool,
    /// Optional details
    details: Option(String),
  )
}

/// Types of execution steps
pub type StepType {
  TranslationStep
  ValidationStep
  RepairGenerationStep
  RepairApplicationStep
  RevalidationStep
}

/// Current state of the execution loop
pub type ExecutionState {
  ExecutionState(
    /// Current configuration
    config: ExecutionConfig,
    /// Current formalization (if any)
    current_formalization: Option(Formalization),
    /// Current iteration
    iteration: Int,
    /// Accumulated trace
    trace: ExecutionTrace,
    /// Pending repairs to try
    pending_repairs: List(argument.RepairSuggestion),
    /// Last countermodel seen
    last_countermodel: Option(Countermodel),
    /// Start time
    start_time: Int,
  )
}

/// Callback types for external integration
pub type TranslateCallback =
  fn(String) -> Result(CompiledTranslation, String)

pub type ValidateCallback =
  fn(Formalization) -> Result(ValidationResponse, String)

// =============================================================================
// Configuration
// =============================================================================

/// Create default execution configuration
pub fn default_config() -> ExecutionConfig {
  ExecutionConfig(
    max_translation_attempts: 3,
    max_validation_iterations: 5,
    max_repairs_per_iteration: 3,
    try_multiple_strategies: True,
    min_translation_confidence: 0.5,
    verbose: False,
    step_timeout_ms: 30_000,
  )
}

/// Create fast execution configuration
pub fn fast_config() -> ExecutionConfig {
  ExecutionConfig(
    max_translation_attempts: 1,
    max_validation_iterations: 2,
    max_repairs_per_iteration: 1,
    try_multiple_strategies: False,
    min_translation_confidence: 0.3,
    verbose: False,
    step_timeout_ms: 10_000,
  )
}

/// Create thorough execution configuration
pub fn thorough_config() -> ExecutionConfig {
  ExecutionConfig(
    max_translation_attempts: 5,
    max_validation_iterations: 10,
    max_repairs_per_iteration: 5,
    try_multiple_strategies: True,
    min_translation_confidence: 0.7,
    verbose: True,
    step_timeout_ms: 60_000,
  )
}

/// Set max iterations
pub fn with_max_iterations(config: ExecutionConfig, max: Int) -> ExecutionConfig {
  ExecutionConfig(..config, max_validation_iterations: max)
}

/// Enable verbose mode
pub fn with_verbose(config: ExecutionConfig, verbose: Bool) -> ExecutionConfig {
  ExecutionConfig(..config, verbose: verbose)
}

// =============================================================================
// Main Execution Loop
// =============================================================================

/// Run the execution loop with callbacks
pub fn run(
  config: ExecutionConfig,
  argument_text: String,
  translate: TranslateCallback,
  validate: ValidateCallback,
  current_time: Int,
) -> ExecutionResult {
  // Initialize state
  let initial_state = new_state(config, current_time)

  // Step 1: Translate
  let translate_result =
    run_translation(initial_state, argument_text, translate)

  case translate_result {
    Error(err) -> ExecutionError(error: err, trace: initial_state.trace)
    Ok(#(state_after_translate, formalization)) -> {
      // Step 2: Validate and repair loop
      run_validation_loop(state_after_translate, formalization, validate)
    }
  }
}

/// Run translation step
fn run_translation(
  state: ExecutionState,
  argument_text: String,
  translate: TranslateCallback,
) -> Result(#(ExecutionState, Formalization), ExecutionError) {
  case translate(argument_text) {
    Ok(translation) -> {
      // Check confidence threshold
      case translation.confidence >=. state.config.min_translation_confidence {
        True -> {
          let formalization = compiled_to_formalization(translation)
          let step =
            ExecutionStep(
              step_type: TranslationStep,
              description: "Translated argument to formal logic",
              duration_ms: 50,
              // Mock duration
              success: True,
              details: Some(
                "Confidence: " <> float_to_percent(translation.confidence),
              ),
            )
          let new_state = add_step(state, step)
          Ok(#(new_state, formalization))
        }
        False -> {
          Error(TranslationFailed(
            "Translation confidence too low: "
            <> float_to_percent(translation.confidence),
          ))
        }
      }
    }
    Error(reason) -> Error(TranslationFailed(reason))
  }
}

/// Run the validation and repair loop
fn run_validation_loop(
  state: ExecutionState,
  formalization: Formalization,
  validate: ValidateCallback,
) -> ExecutionResult {
  let state =
    ExecutionState(..state, current_formalization: Some(formalization))

  do_validation_loop(state, formalization, validate)
}

fn do_validation_loop(
  state: ExecutionState,
  formalization: Formalization,
  validate: ValidateCallback,
) -> ExecutionResult {
  // Check iteration limit
  case state.iteration >= state.config.max_validation_iterations {
    True ->
      ExecutionInvalid(
        formalization: formalization,
        countermodel: state.last_countermodel,
        remaining_repairs: state.pending_repairs,
        trace: state.trace,
      )
    False -> {
      // Run validation
      let validation_result = validate(formalization)

      case validation_result {
        Error(reason) ->
          ExecutionError(error: ValidationFailed(reason), trace: state.trace)

        Ok(response) -> {
          let step =
            ExecutionStep(
              step_type: ValidationStep,
              description: "Validated formalization",
              duration_ms: response.duration_ms,
              success: is_valid_response(response),
              details: None,
            )
          let state = add_step(state, step)

          case is_valid_response(response) {
            True ->
              ExecutionSuccess(
                formalization: formalization,
                validation: response,
                trace: state.trace,
              )
            False -> {
              // Parse countermodel and generate repairs
              let countermodel =
                extract_countermodel(response, formalization.logic_system)
              let state =
                ExecutionState(..state, last_countermodel: countermodel)

              case countermodel {
                None ->
                  // No countermodel available, can't generate repairs
                  ExecutionInvalid(
                    formalization: formalization,
                    countermodel: None,
                    remaining_repairs: [],
                    trace: state.trace,
                  )
                Some(cm) -> {
                  // Generate repairs
                  let repairs = repair.generate_suggestions(formalization, cm)
                  let repair_step =
                    ExecutionStep(
                      step_type: RepairGenerationStep,
                      description: "Generated repair suggestions",
                      duration_ms: 10,
                      success: repairs != [],
                      details: Some(
                        int_to_string(list.length(repairs)) <> " repairs found",
                      ),
                    )
                  let state = add_step(state, repair_step)

                  // Try repairs
                  try_repairs(state, formalization, repairs, validate)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Try applying repairs
fn try_repairs(
  state: ExecutionState,
  formalization: Formalization,
  repairs: List(argument.RepairSuggestion),
  validate: ValidateCallback,
) -> ExecutionResult {
  case repairs {
    [] ->
      // No more repairs to try
      ExecutionInvalid(
        formalization: formalization,
        countermodel: state.last_countermodel,
        remaining_repairs: [],
        trace: state.trace,
      )

    [repair_suggestion, ..rest] -> {
      // Apply repair
      let repaired = repair.apply_repair(formalization, repair_suggestion)
      let apply_step =
        ExecutionStep(
          step_type: RepairApplicationStep,
          description: "Applied repair: " <> repair_suggestion.description,
          duration_ms: 5,
          success: True,
          details: None,
        )
      let state = add_step(state, apply_step)
      let state =
        ExecutionState(
          ..state,
          iteration: state.iteration + 1,
          pending_repairs: rest,
        )

      // Re-validate
      do_validation_loop(state, repaired, validate)
    }
  }
}

// =============================================================================
// Simplified Execution Interface
// =============================================================================

/// Run execution with mock translation (for testing)
pub fn run_with_formalization(
  config: ExecutionConfig,
  formalization: Formalization,
  validate: ValidateCallback,
  current_time: Int,
) -> ExecutionResult {
  let state = new_state(config, current_time)
  let state =
    ExecutionState(..state, current_formalization: Some(formalization))

  run_validation_loop(state, formalization, validate)
}

/// Create a simple validator that always returns valid
pub fn always_valid_validator() -> ValidateCallback {
  fn(_f) {
    Ok(validator.ValidationResponse(
      request_id: "mock",
      result: argument.Valid,
      from_cache: False,
      duration_ms: 10,
      worlds_explored: Some(1),
      smt_formula: None,
    ))
  }
}

/// Create a validator that returns invalid with a countermodel
pub fn always_invalid_validator(reason: String) -> ValidateCallback {
  fn(_f) {
    Ok(validator.ValidationResponse(
      request_id: "mock",
      result: argument.Invalid(reason),
      from_cache: False,
      duration_ms: 10,
      worlds_explored: Some(3),
      smt_formula: None,
    ))
  }
}

// =============================================================================
// State Management
// =============================================================================

/// Create new execution state
fn new_state(config: ExecutionConfig, current_time: Int) -> ExecutionState {
  ExecutionState(
    config: config,
    current_formalization: None,
    iteration: 0,
    trace: empty_trace(),
    pending_repairs: [],
    last_countermodel: None,
    start_time: current_time,
  )
}

fn empty_trace() -> ExecutionTrace {
  ExecutionTrace(
    steps: [],
    total_time_ms: 0,
    translation_attempts: 0,
    validation_iterations: 0,
    repairs_applied: 0,
  )
}

fn add_step(state: ExecutionState, step: ExecutionStep) -> ExecutionState {
  let new_trace =
    ExecutionTrace(
      ..state.trace,
      steps: list.append(state.trace.steps, [step]),
      total_time_ms: state.trace.total_time_ms + step.duration_ms,
    )

  // Update counters based on step type
  let new_trace = case step.step_type {
    TranslationStep ->
      ExecutionTrace(
        ..new_trace,
        translation_attempts: new_trace.translation_attempts + 1,
      )
    ValidationStep | RevalidationStep ->
      ExecutionTrace(
        ..new_trace,
        validation_iterations: new_trace.validation_iterations + 1,
      )
    RepairApplicationStep ->
      ExecutionTrace(
        ..new_trace,
        repairs_applied: new_trace.repairs_applied + 1,
      )
    RepairGenerationStep -> new_trace
  }

  ExecutionState(..state, trace: new_trace)
}

// =============================================================================
// Helper Functions
// =============================================================================

fn is_valid_response(response: ValidationResponse) -> Bool {
  case response.result {
    argument.Valid -> True
    _ -> False
  }
}

fn extract_countermodel(
  response: ValidationResponse,
  logic_system: LogicSystem,
) -> Option(Countermodel) {
  case response.result {
    argument.Invalid(reason) ->
      validator.parse_countermodel(reason, logic_system)
    _ -> None
  }
}

fn compiled_to_formalization(translation: CompiledTranslation) -> Formalization {
  let premises = list.map(translation.premises, fn(p) { p.formal })

  argument.Formalization(
    id: "exec-" <> int_to_string(0),
    argument_id: "",
    logic_system: translation.logic_system,
    premises: premises,
    conclusion: translation.conclusion.formal,
    assumptions: translation.assumptions,
    validation: None,
    created_at: None,
    updated_at: None,
  )
}

fn float_to_percent(f: Float) -> String {
  let percent = float_to_int(f *. 100.0)
  int_to_string(percent) <> "%"
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_string(-n, "")
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

// =============================================================================
// Trace Formatting
// =============================================================================

/// Format execution trace for display
pub fn format_trace(trace: ExecutionTrace) -> String {
  let header =
    "Execution Trace\n"
    <> "===============\n"
    <> "Total time: "
    <> int_to_string(trace.total_time_ms)
    <> "ms\n"
    <> "Translation attempts: "
    <> int_to_string(trace.translation_attempts)
    <> "\n"
    <> "Validation iterations: "
    <> int_to_string(trace.validation_iterations)
    <> "\n"
    <> "Repairs applied: "
    <> int_to_string(trace.repairs_applied)
    <> "\n\n"

  let steps_str =
    trace.steps
    |> list.index_map(fn(step, i) {
      int_to_string(i + 1)
      <> ". ["
      <> step_type_to_string(step.step_type)
      <> "] "
      <> step.description
      <> " ("
      <> int_to_string(step.duration_ms)
      <> "ms) "
      <> case step.success {
        True -> "✓"
        False -> "✗"
      }
      <> case step.details {
        Some(d) -> "\n   " <> d
        None -> ""
      }
    })
    |> string.join("\n")

  header <> "Steps:\n" <> steps_str
}

fn step_type_to_string(step_type: StepType) -> String {
  case step_type {
    TranslationStep -> "TRANSLATE"
    ValidationStep -> "VALIDATE"
    RepairGenerationStep -> "GEN_REPAIR"
    RepairApplicationStep -> "APPLY_REPAIR"
    RevalidationStep -> "REVALIDATE"
  }
}

/// Format execution result for display
pub fn format_result(result: ExecutionResult) -> String {
  case result {
    ExecutionSuccess(formalization, validation, trace) ->
      "✓ SUCCESS: Argument is VALID\n"
      <> "  Premises: "
      <> int_to_string(list.length(formalization.premises))
      <> "\n"
      <> "  From cache: "
      <> bool_to_string(validation.from_cache)
      <> "\n"
      <> "  Validation time: "
      <> int_to_string(validation.duration_ms)
      <> "ms\n\n"
      <> format_trace(trace)

    ExecutionInvalid(_formalization, countermodel, repairs, trace) ->
      "✗ INVALID: Argument is invalid\n"
      <> "  Remaining repairs: "
      <> int_to_string(list.length(repairs))
      <> "\n"
      <> "  Has countermodel: "
      <> bool_to_string(option.is_some(countermodel))
      <> "\n\n"
      <> format_trace(trace)

    ExecutionError(error, trace) ->
      "✗ ERROR: " <> format_error(error) <> "\n\n" <> format_trace(trace)
  }
}

fn format_error(error: ExecutionError) -> String {
  case error {
    TranslationFailed(reason) -> "Translation failed: " <> reason
    ValidationFailed(reason) -> "Validation failed: " <> reason
    TimeoutReached(step) -> "Timeout during: " <> step
    MaxIterationsExceeded -> "Max iterations exceeded"
    ConfigurationError(reason) -> "Configuration error: " <> reason
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
