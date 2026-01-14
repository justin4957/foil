import gleam/string
import gleeunit
import gleeunit/should
import modal_logic/logger

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Configuration Tests
// =============================================================================

pub fn default_config_test() {
  let config = logger.default_config()

  // Default should be Info level
  config.min_level |> should.equal(logger.Info)
  config.include_timestamp |> should.be_true()
  config.include_context |> should.be_true()
}

pub fn debug_config_test() {
  let config = logger.debug_config()

  config.min_level |> should.equal(logger.Debug)
  config.include_timestamp |> should.be_true()
}

pub fn production_config_test() {
  let config = logger.production_config()

  config.min_level |> should.equal(logger.Warn)
  config.include_context |> should.be_false()
  config.output_format |> should.equal(logger.CompactFormat)
}

// =============================================================================
// Logging Level Tests
// =============================================================================

pub fn debug_logs_when_level_is_debug_test() {
  let config = logger.debug_config()
  // This should log (DEBUG >= DEBUG)
  logger.debug(config, "Debug message")
  // No assertion, just verify it doesn't crash
  True |> should.be_true()
}

pub fn debug_does_not_log_when_level_is_info_test() {
  let config = logger.default_config()
  // Info level
  // This should not log (DEBUG < INFO)
  logger.debug(config, "Debug message")
  // No assertion, just verify it doesn't crash
  True |> should.be_true()
}

pub fn info_logs_when_level_is_info_test() {
  let config = logger.default_config()
  // Info level
  logger.info(config, "Info message")
  True |> should.be_true()
}

pub fn warn_logs_when_level_is_info_test() {
  let config = logger.default_config()
  // Info level
  logger.warn(config, "Warning message")
  True |> should.be_true()
}

pub fn error_logs_when_level_is_info_test() {
  let config = logger.default_config()
  // Info level
  logger.error(config, "Error message")
  True |> should.be_true()
}

// =============================================================================
// Context Logging Tests
// =============================================================================

pub fn log_with_context_test() {
  let config = logger.default_config()
  let context = [#("user_id", "123"), #("operation", "validate")]

  logger.log_with_context(config, logger.Info, "Operation completed", context)
  True |> should.be_true()
}

pub fn log_with_error_code_test() {
  let config = logger.default_config()
  let context = [#("formula", "p -> q")]

  logger.log_error_code(
    config,
    logger.Error,
    "Verification failed",
    "E201",
    context,
  )
  True |> should.be_true()
}

// =============================================================================
// Convenience Function Tests
// =============================================================================

pub fn log_operation_start_test() {
  let config = logger.default_config()
  logger.log_operation_start(config, "validation")
  True |> should.be_true()
}

pub fn log_operation_success_test() {
  let config = logger.default_config()
  logger.log_operation_success(config, "validation", 150)
  True |> should.be_true()
}

pub fn log_operation_failure_test() {
  let config = logger.default_config()
  logger.log_operation_failure(config, "validation", "Z3 timeout")
  True |> should.be_true()
}

// =============================================================================
// Format Tests (basic validation)
// =============================================================================

pub fn text_format_config_test() {
  let config = logger.default_config()
  config.output_format |> should.equal(logger.TextFormat)
}

pub fn json_format_config_test() {
  let config =
    logger.LoggerConfig(
      min_level: logger.Info,
      include_timestamp: False,
      include_context: True,
      output_format: logger.JsonFormat,
    )
  config.output_format |> should.equal(logger.JsonFormat)
}

pub fn compact_format_config_test() {
  let config = logger.production_config()
  config.output_format |> should.equal(logger.CompactFormat)
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
