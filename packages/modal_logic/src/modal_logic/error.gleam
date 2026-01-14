//// Error Handling Module
////
//// This module provides structured error types and formatting for the
//// modal logic system. It includes context-aware error messages with
//// suggestions, line numbers, and actionable guidance.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// Error Types
// =============================================================================

/// Main error type for the modal logic system
pub type FoilError {
  /// Parsing errors with location information
  ParseError(
    error_code: String,
    line: Int,
    column: Int,
    snippet: String,
    message: String,
    suggestion: Option(String),
  )

  /// LLM translation errors with retry information
  TranslationError(
    error_code: String,
    llm_error: String,
    retry_count: Int,
    original_text: String,
    suggestion: Option(String),
  )

  /// Z3 verification errors with timeout information
  VerificationError(
    error_code: String,
    z3_output: String,
    timeout: Bool,
    formula: String,
    suggestion: Option(String),
  )

  /// Configuration errors with expected values
  ConfigurationError(
    error_code: String,
    parameter: String,
    provided: String,
    expected: String,
    suggestion: Option(String),
  )

  /// System errors (file I/O, network, etc.)
  SystemError(
    error_code: String,
    operation: String,
    details: String,
    suggestion: Option(String),
  )

  /// Validation errors for invalid input
  ValidationError(
    error_code: String,
    field: String,
    value: String,
    reason: String,
    suggestion: Option(String),
  )
}

// =============================================================================
// Error Code Constants
// =============================================================================

/// Parse error codes
pub const e001_syntax_error = "E001"

pub const e002_unexpected_token = "E002"

pub const e003_unclosed_paren = "E003"

pub const e004_invalid_operator = "E004"

/// Translation error codes
pub const e101_llm_api_error = "E101"

pub const e102_llm_timeout = "E102"

pub const e103_invalid_json = "E103"

pub const e104_low_confidence = "E104"

/// Verification error codes
pub const e201_z3_error = "E201"

pub const e202_z3_timeout = "E202"

pub const e203_invalid_formula = "E203"

pub const e204_unsupported_logic = "E204"

/// Configuration error codes
pub const e301_missing_param = "E301"

pub const e302_invalid_param = "E302"

pub const e303_invalid_type = "E303"

/// System error codes
pub const e401_file_not_found = "E401"

pub const e402_permission_denied = "E402"

pub const e403_network_error = "E403"

/// Validation error codes
pub const e501_empty_input = "E501"

pub const e502_invalid_format = "E502"

pub const e503_constraint_violation = "E503"

// =============================================================================
// Error Formatting
// =============================================================================

/// Format error for display with full context
pub fn format_error(error: FoilError) -> String {
  case error {
    ParseError(code, line, column, snippet, message, suggestion) ->
      format_parse_error(code, line, column, snippet, message, suggestion)

    TranslationError(code, llm_error, retry_count, original_text, suggestion) ->
      format_translation_error(
        code,
        llm_error,
        retry_count,
        original_text,
        suggestion,
      )

    VerificationError(code, z3_output, timeout, formula, suggestion) ->
      format_verification_error(code, z3_output, timeout, formula, suggestion)

    ConfigurationError(code, parameter, provided, expected, suggestion) ->
      format_configuration_error(
        code,
        parameter,
        provided,
        expected,
        suggestion,
      )

    SystemError(code, operation, details, suggestion) ->
      format_system_error(code, operation, details, suggestion)

    ValidationError(code, field, value, reason, suggestion) ->
      format_validation_error(code, field, value, reason, suggestion)
  }
}

fn format_parse_error(
  code: String,
  line: Int,
  column: Int,
  snippet: String,
  message: String,
  suggestion: Option(String),
) -> String {
  let location =
    "["
    <> code
    <> "] Parse Error at line "
    <> int.to_string(line)
    <> ", column "
    <> int.to_string(column)

  let pointer = string.repeat(" ", column - 1) <> "^"

  let base =
    location
    <> "\n"
    <> "  "
    <> snippet
    <> "\n"
    <> "  "
    <> pointer
    <> "\n"
    <> "  "
    <> message

  case suggestion {
    Some(s) -> base <> "\n\nSuggestion: " <> s
    None -> base
  }
}

fn format_translation_error(
  code: String,
  llm_error: String,
  retry_count: Int,
  original_text: String,
  suggestion: Option(String),
) -> String {
  let header = "[" <> code <> "] Translation Error"
  let retries = " (after " <> int.to_string(retry_count) <> " retries)"

  let base =
    header
    <> retries
    <> "\n"
    <> "  LLM Error: "
    <> llm_error
    <> "\n"
    <> "  Original text: "
    <> truncate(original_text, 100)

  case suggestion {
    Some(s) -> base <> "\n\nSuggestion: " <> s
    None -> base
  }
}

fn format_verification_error(
  code: String,
  z3_output: String,
  timeout: Bool,
  formula: String,
  suggestion: Option(String),
) -> String {
  let header = "[" <> code <> "] Verification Error"
  let timeout_msg = case timeout {
    True -> " (timeout)"
    False -> ""
  }

  let base =
    header
    <> timeout_msg
    <> "\n"
    <> "  Z3 Output: "
    <> truncate(z3_output, 200)
    <> "\n"
    <> "  Formula: "
    <> truncate(formula, 100)

  case suggestion {
    Some(s) -> base <> "\n\nSuggestion: " <> s
    None -> base
  }
}

fn format_configuration_error(
  code: String,
  parameter: String,
  provided: String,
  expected: String,
  suggestion: Option(String),
) -> String {
  let header = "[" <> code <> "] Configuration Error"

  let base =
    header
    <> "\n"
    <> "  Parameter: "
    <> parameter
    <> "\n"
    <> "  Provided: "
    <> provided
    <> "\n"
    <> "  Expected: "
    <> expected

  case suggestion {
    Some(s) -> base <> "\n\nSuggestion: " <> s
    None -> base
  }
}

fn format_system_error(
  code: String,
  operation: String,
  details: String,
  suggestion: Option(String),
) -> String {
  let header = "[" <> code <> "] System Error"

  let base =
    header
    <> "\n"
    <> "  Operation: "
    <> operation
    <> "\n"
    <> "  Details: "
    <> details

  case suggestion {
    Some(s) -> base <> "\n\nSuggestion: " <> s
    None -> base
  }
}

fn format_validation_error(
  code: String,
  field: String,
  value: String,
  reason: String,
  suggestion: Option(String),
) -> String {
  let header = "[" <> code <> "] Validation Error"

  let base =
    header
    <> "\n"
    <> "  Field: "
    <> field
    <> "\n"
    <> "  Value: "
    <> truncate(value, 50)
    <> "\n"
    <> "  Reason: "
    <> reason

  case suggestion {
    Some(s) -> base <> "\n\nSuggestion: " <> s
    None -> base
  }
}

// =============================================================================
// Error Helper Functions
// =============================================================================

/// Truncate string to max length with ellipsis
fn truncate(s: String, max_length: Int) -> String {
  case string.length(s) > max_length {
    True -> string.slice(s, 0, max_length) <> "..."
    False -> s
  }
}

/// Extract error code from error
pub fn get_error_code(error: FoilError) -> String {
  case error {
    ParseError(code, ..) -> code
    TranslationError(code, ..) -> code
    VerificationError(code, ..) -> code
    ConfigurationError(code, ..) -> code
    SystemError(code, ..) -> code
    ValidationError(code, ..) -> code
  }
}

/// Get suggestion from error if available
pub fn get_suggestion(error: FoilError) -> Option(String) {
  case error {
    ParseError(_, _, _, _, _, suggestion) -> suggestion
    TranslationError(_, _, _, _, suggestion) -> suggestion
    VerificationError(_, _, _, _, suggestion) -> suggestion
    ConfigurationError(_, _, _, _, suggestion) -> suggestion
    SystemError(_, _, _, suggestion) -> suggestion
    ValidationError(_, _, _, _, suggestion) -> suggestion
  }
}

/// Check if error is recoverable (can retry)
pub fn is_recoverable(error: FoilError) -> Bool {
  case error {
    TranslationError(..) -> True
    VerificationError(_, _, timeout, _, _) -> timeout
    SystemError(code, ..) -> code == e403_network_error
    _ -> False
  }
}

// =============================================================================
// Common Error Constructors
// =============================================================================

/// Create a syntax error with location
pub fn syntax_error(
  line: Int,
  column: Int,
  snippet: String,
  message: String,
) -> FoilError {
  ParseError(
    error_code: e001_syntax_error,
    line: line,
    column: column,
    snippet: snippet,
    message: message,
    suggestion: Some(
      "Check for matching parentheses and correct operator syntax",
    ),
  )
}

/// Create an unexpected token error
pub fn unexpected_token(
  line: Int,
  column: Int,
  snippet: String,
  found: String,
  expected: List(String),
) -> FoilError {
  let exp_str = string.join(expected, ", ")
  ParseError(
    error_code: e002_unexpected_token,
    line: line,
    column: column,
    snippet: snippet,
    message: "Expected " <> exp_str <> ", found '" <> found <> "'",
    suggestion: Some("Valid operators: →, ∧, ∨, ¬, □, ◇"),
  )
}

/// Create an LLM API error
pub fn llm_api_error(
  llm_error: String,
  retry_count: Int,
  original_text: String,
) -> FoilError {
  TranslationError(
    error_code: e101_llm_api_error,
    llm_error: llm_error,
    retry_count: retry_count,
    original_text: original_text,
    suggestion: Some(
      "Check API key and network connection, or try simplifying the input",
    ),
  )
}

/// Create a Z3 timeout error
pub fn z3_timeout_error(formula: String) -> FoilError {
  VerificationError(
    error_code: e202_z3_timeout,
    z3_output: "Solver timed out",
    timeout: True,
    formula: formula,
    suggestion: Some(
      "Try simplifying the formula or increasing the timeout value",
    ),
  )
}

/// Create a missing parameter error
pub fn missing_parameter(parameter: String, expected: String) -> FoilError {
  ConfigurationError(
    error_code: e301_missing_param,
    parameter: parameter,
    provided: "(not provided)",
    expected: expected,
    suggestion: Some(
      "Provide the required parameter: --" <> parameter <> " <value>",
    ),
  )
}

/// Create an empty input validation error
pub fn empty_input_error(field: String) -> FoilError {
  ValidationError(
    error_code: e501_empty_input,
    field: field,
    value: "(empty)",
    reason: "Input cannot be empty",
    suggestion: Some("Provide a non-empty value for " <> field),
  )
}
