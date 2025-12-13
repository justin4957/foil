//// Translation Service for LLM-based Argument Analysis
////
//// This module provides the service layer for translating natural language
//// arguments into formal modal logic representations using an LLM.
////
//// ## Features
////
//// - Format prompts for Claude API with JSON mode
//// - Parse and validate LLM responses
//// - Graceful failure handling with retries
//// - Multiple interpretation support
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/translation_service
////
//// let service = translation_service.new_service(config)
//// let result = translation_service.translate(service, argument_text)
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/argument.{type Ambiguity, type Formalization}
import modal_logic/compiler.{type CompileError, type CompiledTranslation}
import modal_logic/logic_detector.{type LogicDetection}
import modal_logic/prompts.{type PromptConfig, type TranslationPrompt}
import modal_logic/proposition.{type LogicSystem}

// =============================================================================
// Types
// =============================================================================

/// Configuration for the translation service
pub type ServiceConfig {
  ServiceConfig(
    /// Prompt configuration
    prompt_config: PromptConfig,
    /// Maximum retry attempts
    max_retries: Int,
    /// Whether to attempt rephrasing on failure
    rephrase_on_failure: Bool,
    /// Whether to generate multiple interpretations
    generate_alternatives: Bool,
    /// Minimum confidence to accept result
    min_confidence: Float,
    /// API timeout in milliseconds
    timeout_ms: Int,
  )
}

/// A translation service instance
pub type TranslationService {
  TranslationService(
    /// Service configuration
    config: ServiceConfig,
    /// Statistics for monitoring
    stats: ServiceStats,
  )
}

/// Service statistics
pub type ServiceStats {
  ServiceStats(
    /// Total translation requests
    total_requests: Int,
    /// Successful translations
    successful: Int,
    /// Failed translations
    failed: Int,
    /// Retried translations
    retried: Int,
    /// Average confidence score
    avg_confidence: Float,
  )
}

/// Result of a translation attempt
pub type TranslationResult {
  TranslationResult(
    /// Primary translation
    primary: CompiledTranslation,
    /// Alternative interpretations (if generated)
    alternatives: List(CompiledTranslation),
    /// Logic system detection result
    logic_detection: LogicDetection,
    /// Number of retries needed
    retry_count: Int,
    /// Any warnings generated
    warnings: List(String),
  )
}

/// Translation error types
pub type TranslationError {
  /// API call failed
  ApiError(message: String, code: Option(Int))
  /// Response parsing failed
  ParseError(compile_error: CompileError)
  /// Confidence too low
  LowConfidenceError(confidence: Float, min_required: Float)
  /// All retries exhausted
  RetriesExhaustedError(last_error: TranslationError, attempts: Int)
  /// Timeout occurred
  TimeoutError(timeout_ms: Int)
  /// Multiple errors occurred
  MultipleErrors(errors: List(TranslationError))
  /// User cancelled
  CancelledError
  /// Internal error
  InternalError(message: String)
}

/// Request for the Claude API
pub type ApiRequest {
  ApiRequest(
    /// System message
    system: String,
    /// User message
    user: String,
    /// Model identifier
    model: String,
    /// Maximum tokens
    max_tokens: Int,
    /// Temperature
    temperature: Float,
    /// Whether to use JSON mode
    json_mode: Bool,
  )
}

/// Response from the Claude API
pub type ApiResponse {
  ApiResponse(
    /// Response content
    content: String,
    /// Model used
    model: String,
    /// Input tokens used
    input_tokens: Int,
    /// Output tokens used
    output_tokens: Int,
    /// Stop reason
    stop_reason: String,
  )
}

/// Retry strategy
pub type RetryStrategy {
  /// No retry
  NoRetry
  /// Retry with same prompt
  RetryUnmodified
  /// Retry with simplified prompt
  RetrySimplified
  /// Retry with rephrased input
  RetryRephrased(new_input: String)
  /// Retry with different logic system
  RetryWithLogic(system: LogicSystem)
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default service configuration
pub fn default_config() -> ServiceConfig {
  ServiceConfig(
    prompt_config: prompts.default_config(),
    max_retries: 3,
    rephrase_on_failure: True,
    generate_alternatives: False,
    min_confidence: 0.5,
    timeout_ms: 30_000,
  )
}

/// Create a strict configuration (higher confidence required)
pub fn strict_config() -> ServiceConfig {
  ServiceConfig(
    prompt_config: prompts.verbose_config(),
    max_retries: 2,
    rephrase_on_failure: True,
    generate_alternatives: True,
    min_confidence: 0.75,
    timeout_ms: 60_000,
  )
}

/// Create a fast configuration (lower confidence, fewer retries)
pub fn fast_config() -> ServiceConfig {
  ServiceConfig(
    prompt_config: prompts.minimal_config(),
    max_retries: 1,
    rephrase_on_failure: False,
    generate_alternatives: False,
    min_confidence: 0.3,
    timeout_ms: 15_000,
  )
}

/// Set maximum retries
pub fn with_max_retries(config: ServiceConfig, retries: Int) -> ServiceConfig {
  ServiceConfig(..config, max_retries: retries)
}

/// Set minimum confidence
pub fn with_min_confidence(
  config: ServiceConfig,
  confidence: Float,
) -> ServiceConfig {
  ServiceConfig(..config, min_confidence: confidence)
}

/// Enable/disable rephrasing on failure
pub fn with_rephrase(config: ServiceConfig, enabled: Bool) -> ServiceConfig {
  ServiceConfig(..config, rephrase_on_failure: enabled)
}

/// Enable/disable alternative generation
pub fn with_alternatives(config: ServiceConfig, enabled: Bool) -> ServiceConfig {
  ServiceConfig(..config, generate_alternatives: enabled)
}

// =============================================================================
// Service Creation
// =============================================================================

/// Create a new translation service
pub fn new_service(config: ServiceConfig) -> TranslationService {
  TranslationService(config: config, stats: empty_stats())
}

/// Create empty statistics
fn empty_stats() -> ServiceStats {
  ServiceStats(
    total_requests: 0,
    successful: 0,
    failed: 0,
    retried: 0,
    avg_confidence: 0.0,
  )
}

/// Update service with new stats
fn update_stats(
  service: TranslationService,
  success: Bool,
  retries: Int,
  confidence: Float,
) -> TranslationService {
  let stats = service.stats
  let new_total = stats.total_requests + 1
  let new_successful = stats.successful + bool_to_int(success)
  let new_failed = stats.failed + bool_to_int(!success)
  let new_retried = stats.retried + retries

  // Update running average confidence
  let new_avg = case success {
    True -> {
      let old_sum = stats.avg_confidence *. int_to_float(stats.successful)
      let new_sum = old_sum +. confidence
      new_sum /. int_to_float(new_successful)
    }
    False -> stats.avg_confidence
  }

  TranslationService(
    ..service,
    stats: ServiceStats(
      total_requests: new_total,
      successful: new_successful,
      failed: new_failed,
      retried: new_retried,
      avg_confidence: new_avg,
    ),
  )
}

// =============================================================================
// API Request Building
// =============================================================================

/// Build an API request from a translation prompt
pub fn build_api_request(prompt: TranslationPrompt, model: String) -> ApiRequest {
  ApiRequest(
    system: prompt.system_message,
    user: prompt.user_message,
    model: model,
    max_tokens: 4096,
    temperature: 0.3,
    json_mode: True,
  )
}

/// Build API request for ambiguity analysis
pub fn build_ambiguity_request(
  argument_text: String,
  model: String,
) -> ApiRequest {
  let prompt = prompts.build_ambiguity_prompt(argument_text)
  build_api_request(prompt, model)
}

/// Build API request for logic system recommendation
pub fn build_logic_request(argument_text: String, model: String) -> ApiRequest {
  let prompt = prompts.build_logic_system_prompt(argument_text)
  build_api_request(prompt, model)
}

/// Serialize API request to JSON string
pub fn serialize_request(request: ApiRequest) -> String {
  "{"
  <> "\"model\": "
  <> quote_string(request.model)
  <> ", "
  <> "\"max_tokens\": "
  <> int_to_string(request.max_tokens)
  <> ", "
  <> "\"temperature\": "
  <> float_to_string(request.temperature)
  <> ", "
  <> "\"system\": "
  <> quote_string(request.system)
  <> ", "
  <> "\"messages\": [{\"role\": \"user\", \"content\": "
  <> quote_string(request.user)
  <> "}]"
  <> "}"
}

// =============================================================================
// Response Processing
// =============================================================================

/// Process an API response into a translation result
pub fn process_response(
  service: TranslationService,
  response: ApiResponse,
  argument_text: String,
) -> Result(TranslationResult, TranslationError) {
  // Parse and compile the response
  case compiler.compile_translation(response.content) {
    Error(compile_error) -> Error(ParseError(compile_error))

    Ok(translation) -> {
      // Check confidence
      case translation.confidence <. service.config.min_confidence {
        True ->
          Error(LowConfidenceError(
            translation.confidence,
            service.config.min_confidence,
          ))

        False -> {
          // Perform logic detection
          let logic_detection = logic_detector.detect_from_text(argument_text)

          Ok(
            TranslationResult(
              primary: translation,
              alternatives: [],
              logic_detection: logic_detection,
              retry_count: 0,
              warnings: [],
            ),
          )
        }
      }
    }
  }
}

// =============================================================================
// Retry Handling
// =============================================================================

/// Determine retry strategy based on error
pub fn determine_retry_strategy(
  error: TranslationError,
  attempt: Int,
  max_retries: Int,
) -> RetryStrategy {
  case attempt >= max_retries {
    True -> NoRetry
    False -> {
      case error {
        ApiError(_, _) -> RetryUnmodified
        ParseError(_) -> RetrySimplified
        LowConfidenceError(_, _) -> RetrySimplified
        TimeoutError(_) -> RetryUnmodified
        _ -> NoRetry
      }
    }
  }
}

/// Check if error is retryable
pub fn is_retryable(error: TranslationError) -> Bool {
  case error {
    ApiError(_, code) -> {
      case code {
        Some(429) -> True
        // Rate limit
        Some(500) -> True
        // Server error
        Some(502) -> True
        Some(503) -> True
        Some(504) -> True
        _ -> False
      }
    }
    ParseError(compile_error) -> compiler.is_recoverable(compile_error)
    LowConfidenceError(_, _) -> True
    TimeoutError(_) -> True
    RetriesExhaustedError(_, _) -> False
    MultipleErrors(_) -> False
    CancelledError -> False
    InternalError(_) -> False
  }
}

/// Get simplified prompt config for retries
pub fn simplify_config(config: PromptConfig) -> PromptConfig {
  prompts.PromptConfig(
    ..config,
    include_examples: True,
    max_premises: config.max_premises / 2 + 1,
    ambiguity_detail: prompts.Minimal,
  )
}

// =============================================================================
// Multiple Interpretations
// =============================================================================

/// Generate alternative interpretations for ambiguous arguments
pub fn generate_alternatives_for_ambiguity(
  translation: CompiledTranslation,
  ambiguity: Ambiguity,
) -> List(CompiledTranslation) {
  case ambiguity {
    argument.ModalAmbiguity(_, interpretations) -> {
      // Generate one alternative per interpretation
      list.filter_map(interpretations, fn(interp) {
        let new_logic = interpretation_to_logic(interp)
        case new_logic != translation.logic_system {
          True ->
            Ok(
              compiler.CompiledTranslation(
                ..translation,
                logic_system: new_logic,
              ),
            )
          False -> Error(Nil)
        }
      })
    }
    argument.ScopeAmbiguity(_, _) -> []
    // Would need reparsing
    argument.LexicalAmbiguity(_, _) -> []
    // Would need reparsing
    argument.StructuralAmbiguity(_, _) -> []
    // Would need reparsing
  }
}

/// Map modal interpretation to logic system
fn interpretation_to_logic(
  interp: proposition.ModalInterpretation,
) -> LogicSystem {
  case interp {
    proposition.Epistemic -> proposition.S5
    proposition.Deontic -> proposition.KD
    proposition.Alethic -> proposition.S4
    proposition.Temporal -> proposition.K4
  }
}

// =============================================================================
// Error Handling
// =============================================================================

/// Format a translation error as a human-readable string
pub fn format_error(error: TranslationError) -> String {
  case error {
    ApiError(msg, code) -> {
      let code_str = case code {
        Some(c) -> " (code: " <> int_to_string(c) <> ")"
        None -> ""
      }
      "API error: " <> msg <> code_str
    }

    ParseError(compile_error) ->
      "Parse error: " <> compiler.format_error(compile_error)

    LowConfidenceError(conf, min) ->
      "Low confidence: "
      <> float_to_string(conf)
      <> " < "
      <> float_to_string(min)
      <> " required"

    RetriesExhaustedError(last, attempts) ->
      "All "
      <> int_to_string(attempts)
      <> " retries exhausted. Last error: "
      <> format_error(last)

    TimeoutError(ms) -> "Request timed out after " <> int_to_string(ms) <> "ms"

    MultipleErrors(errors) ->
      "Multiple errors:\n" <> string.join(list.map(errors, format_error), "\n")

    CancelledError -> "Translation cancelled by user"

    InternalError(msg) -> "Internal error: " <> msg
  }
}

/// Get user-friendly explanation of a translation error
pub fn explain_error(error: TranslationError) -> String {
  case error {
    ApiError(_, Some(429)) ->
      "The translation service is temporarily overloaded. Please wait a moment and try again."

    ApiError(_, Some(code)) if code >= 500 ->
      "The translation service is experiencing technical difficulties. Please try again later."

    ApiError(_, _) ->
      "Unable to connect to the translation service. Please check your network connection."

    ParseError(_) ->
      "The argument could not be properly analyzed. Try rephrasing it more clearly."

    LowConfidenceError(_, _) ->
      "The argument structure is unclear. Consider breaking it into simpler statements."

    RetriesExhaustedError(_, _) ->
      "Multiple attempts to translate failed. The argument may be too complex or ambiguous."

    TimeoutError(_) ->
      "The translation took too long. Try with a shorter argument."

    CancelledError -> "The translation was cancelled."

    MultipleErrors(_) ->
      "Several issues were encountered. Please simplify the argument and try again."

    InternalError(_) ->
      "An unexpected error occurred. Please try again or contact support."
  }
}

/// Create error with suggestions for recovery
pub fn with_recovery_suggestions(
  error: TranslationError,
) -> #(TranslationError, List(String)) {
  let suggestions = case error {
    ParseError(_) -> [
      "Ensure each premise and conclusion is a complete sentence",
      "Avoid nested parenthetical clauses",
      "Use explicit connectives (and, or, therefore)",
    ]

    LowConfidenceError(_, _) -> [
      "Break the argument into smaller sub-arguments",
      "Clarify modal words (must, might, should)",
      "Specify which interpretation is intended",
    ]

    ApiError(_, _) -> [
      "Wait a few seconds and retry",
      "Check network connectivity",
    ]

    TimeoutError(_) -> [
      "Reduce the length of the argument",
      "Remove unnecessary context",
    ]

    _ -> []
  }

  #(error, suggestions)
}

// =============================================================================
// Conversion Functions
// =============================================================================

/// Convert translation result to formalization
pub fn to_formalization(
  result: TranslationResult,
  id: String,
  argument_id: String,
) -> Formalization {
  compiler.to_formalization(result.primary, id, argument_id)
}

/// Get all ambiguities from translation result
pub fn get_ambiguities(result: TranslationResult) -> List(Ambiguity) {
  result.primary.ambiguities
}

/// Get recommended logic system
pub fn get_recommended_logic(result: TranslationResult) -> Option(LogicSystem) {
  result.logic_detection.recommended
}

// =============================================================================
// Statistics
// =============================================================================

/// Get service statistics
pub fn get_stats(service: TranslationService) -> ServiceStats {
  service.stats
}

/// Format statistics as string
pub fn format_stats(stats: ServiceStats) -> String {
  let success_rate = case stats.total_requests > 0 {
    True ->
      int_to_float(stats.successful * 100) /. int_to_float(stats.total_requests)
    False -> 0.0
  }

  "TranslationStats{"
  <> "total="
  <> int_to_string(stats.total_requests)
  <> ", success="
  <> int_to_string(stats.successful)
  <> " ("
  <> float_to_string(success_rate)
  <> "%)"
  <> ", failed="
  <> int_to_string(stats.failed)
  <> ", retried="
  <> int_to_string(stats.retried)
  <> ", avg_confidence="
  <> float_to_string(stats.avg_confidence)
  <> "}"
}

/// Reset service statistics
pub fn reset_stats(service: TranslationService) -> TranslationService {
  TranslationService(..service, stats: empty_stats())
}

// =============================================================================
// Helper Functions
// =============================================================================

fn bool_to_int(b: Bool) -> Int {
  case b {
    True -> 1
    False -> 0
  }
}

fn quote_string(s: String) -> String {
  "\""
  <> s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
  <> "\""
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

fn float_to_string(f: Float) -> String {
  let int_part = float_to_int(f)
  let frac_part = float_to_int({ f -. int_to_float(int_part) } *. 100.0)
  int_to_string(int_part)
  <> "."
  <> pad_left(int_to_string(abs_int(frac_part)), 2, "0")
}

fn pad_left(s: String, len: Int, pad: String) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left(pad <> s, len, pad)
  }
}

fn abs_int(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
