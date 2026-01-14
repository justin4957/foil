import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import modal_logic/error

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Parse Error Tests
// =============================================================================

pub fn syntax_error_test() {
  let err = error.syntax_error(5, 12, "Necessary(p → q)", "Invalid character")

  error.get_error_code(err) |> should.equal(error.e001_syntax_error)

  let formatted = error.format_error(err)
  formatted |> should_contain("E001")
  formatted |> should_contain("line 5")
  formatted |> should_contain("column 12")
  formatted |> should_contain("Necessary(p → q)")
}

pub fn unexpected_token_test() {
  let err = error.unexpected_token(3, 8, "p && q", "&&", ["∧", "∨", "→"])

  error.get_error_code(err) |> should.equal(error.e002_unexpected_token)

  let formatted = error.format_error(err)
  formatted |> should_contain("E002")
  formatted |> should_contain("Expected")
  formatted |> should_contain("&&")
}

pub fn parse_error_with_suggestion_test() {
  let err = error.syntax_error(1, 5, "test", "error message")

  case error.get_suggestion(err) {
    Some(s) -> s |> should_contain("parentheses")
    None -> should.fail()
  }
}

// =============================================================================
// Translation Error Tests
// =============================================================================

pub fn llm_api_error_test() {
  let err =
    error.llm_api_error("Rate limit exceeded", 3, "Original argument text")

  error.get_error_code(err) |> should.equal(error.e101_llm_api_error)

  let formatted = error.format_error(err)
  formatted |> should_contain("E101")
  formatted |> should_contain("after 3 retries")
  formatted |> should_contain("Rate limit exceeded")
}

pub fn translation_error_truncates_long_text_test() {
  let long_text = string.repeat("a", 200)
  let err = error.llm_api_error("Error", 1, long_text)

  let formatted = error.format_error(err)
  // Should be truncated to ~100 chars
  formatted |> should_contain("...")
}

// =============================================================================
// Verification Error Tests
// =============================================================================

pub fn z3_timeout_error_test() {
  let err = error.z3_timeout_error("□□□(p → q)")

  error.get_error_code(err) |> should.equal(error.e202_z3_timeout)

  let formatted = error.format_error(err)
  formatted |> should_contain("E202")
  formatted |> should_contain("timeout")
  formatted |> should_contain("□□□(p → q)")
}

pub fn verification_error_has_suggestion_test() {
  let err = error.z3_timeout_error("complex formula")

  case error.get_suggestion(err) {
    Some(s) -> {
      s |> should_contain("simplifying")
      s |> should_contain("timeout")
    }
    None -> should.fail()
  }
}

// =============================================================================
// Configuration Error Tests
// =============================================================================

pub fn missing_parameter_test() {
  let err = error.missing_parameter("api_key", "Valid Anthropic API key")

  error.get_error_code(err) |> should.equal(error.e301_missing_param)

  let formatted = error.format_error(err)
  formatted |> should_contain("E301")
  formatted |> should_contain("api_key")
  formatted |> should_contain("(not provided)")
}

pub fn configuration_error_with_values_test() {
  let err =
    error.ConfigurationError(
      error_code: error.e302_invalid_param,
      parameter: "timeout",
      provided: "-100",
      expected: "Positive integer",
      suggestion: Some("Use a positive value"),
    )

  let formatted = error.format_error(err)
  formatted |> should_contain("timeout")
  formatted |> should_contain("-100")
  formatted |> should_contain("Positive integer")
}

// =============================================================================
// Validation Error Tests
// =============================================================================

pub fn empty_input_error_test() {
  let err = error.empty_input_error("argument_text")

  error.get_error_code(err) |> should.equal(error.e501_empty_input)

  let formatted = error.format_error(err)
  formatted |> should_contain("E501")
  formatted |> should_contain("argument_text")
  formatted |> should_contain("empty")
}

// =============================================================================
// Error Recovery Tests
// =============================================================================

pub fn translation_error_is_recoverable_test() {
  let err = error.llm_api_error("Network error", 1, "text")
  error.is_recoverable(err) |> should.be_true()
}

pub fn timeout_error_is_recoverable_test() {
  let err = error.z3_timeout_error("formula")
  error.is_recoverable(err) |> should.be_true()
}

pub fn parse_error_not_recoverable_test() {
  let err = error.syntax_error(1, 1, "test", "message")
  error.is_recoverable(err) |> should.be_false()
}

pub fn network_error_is_recoverable_test() {
  let err =
    error.SystemError(
      error_code: error.e403_network_error,
      operation: "api_call",
      details: "Connection failed",
      suggestion: None,
    )
  error.is_recoverable(err) |> should.be_true()
}

// =============================================================================
// Error Code Extraction Tests
// =============================================================================

pub fn get_error_code_from_parse_error_test() {
  let err = error.syntax_error(1, 1, "test", "msg")
  error.get_error_code(err) |> should.equal("E001")
}

pub fn get_error_code_from_translation_error_test() {
  let err = error.llm_api_error("error", 1, "text")
  error.get_error_code(err) |> should.equal("E101")
}

pub fn get_error_code_from_verification_error_test() {
  let err = error.z3_timeout_error("formula")
  error.get_error_code(err) |> should.equal("E202")
}

// =============================================================================
// Error Message Formatting Tests
// =============================================================================

pub fn parse_error_includes_location_test() {
  let err = error.syntax_error(10, 15, "snippet", "message")

  let formatted = error.format_error(err)
  formatted |> should_contain("line 10")
  formatted |> should_contain("column 15")
}

pub fn parse_error_includes_pointer_test() {
  let err =
    error.ParseError(
      error_code: error.e001_syntax_error,
      line: 1,
      column: 5,
      snippet: "test line",
      message: "error",
      suggestion: None,
    )

  let formatted = error.format_error(err)
  // Should include pointer arrow (^) at column 5
  formatted |> should_contain("^")
}

pub fn error_with_suggestion_includes_suggestion_test() {
  let err = error.missing_parameter("param", "value")

  let formatted = error.format_error(err)
  formatted |> should_contain("Suggestion:")
}

pub fn error_without_suggestion_no_suggestion_section_test() {
  let err =
    error.ConfigurationError(
      error_code: error.e302_invalid_param,
      parameter: "test",
      provided: "x",
      expected: "y",
      suggestion: None,
    )

  let formatted = error.format_error(err)
  // Should not contain "Suggestion:" section
  case string.contains(formatted, "Suggestion:") {
    True -> should.fail()
    False -> should.be_true(True)
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False -> {
      io.println("Expected '" <> haystack <> "' to contain '" <> needle <> "'")
      should.fail()
    }
  }
}

import gleam/io
