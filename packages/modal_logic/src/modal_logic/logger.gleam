//// Logging Module
////
//// Provides structured logging with multiple severity levels.
//// Supports DEBUG, INFO, WARN, and ERROR levels with context.

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Log severity levels
pub type LogLevel {
  Debug
  Info
  Warn
  Error
}

/// Log entry with structured data
pub type LogEntry {
  LogEntry(
    level: LogLevel,
    message: String,
    context: List(#(String, String)),
    timestamp: Option(String),
    error_code: Option(String),
  )
}

/// Logger configuration
pub type LoggerConfig {
  LoggerConfig(
    min_level: LogLevel,
    include_timestamp: Bool,
    include_context: Bool,
    output_format: OutputFormat,
  )
}

/// Output format for log entries
pub type OutputFormat {
  TextFormat
  JsonFormat
  CompactFormat
}

// =============================================================================
// Configuration
// =============================================================================

/// Default logger configuration
pub fn default_config() -> LoggerConfig {
  LoggerConfig(
    min_level: Info,
    include_timestamp: True,
    include_context: True,
    output_format: TextFormat,
  )
}

/// Debug configuration (logs everything)
pub fn debug_config() -> LoggerConfig {
  LoggerConfig(
    min_level: Debug,
    include_timestamp: True,
    include_context: True,
    output_format: TextFormat,
  )
}

/// Production configuration (errors and warnings only)
pub fn production_config() -> LoggerConfig {
  LoggerConfig(
    min_level: Warn,
    include_timestamp: True,
    include_context: False,
    output_format: CompactFormat,
  )
}

// =============================================================================
// Logging Functions
// =============================================================================

/// Log a debug message
pub fn debug(config: LoggerConfig, message: String) -> Nil {
  log_with_context(config, Debug, message, [])
}

/// Log an info message
pub fn info(config: LoggerConfig, message: String) -> Nil {
  log_with_context(config, Info, message, [])
}

/// Log a warning message
pub fn warn(config: LoggerConfig, message: String) -> Nil {
  log_with_context(config, Warn, message, [])
}

/// Log an error message
pub fn error(config: LoggerConfig, message: String) -> Nil {
  log_with_context(config, Error, message, [])
}

/// Log with additional context
pub fn log_with_context(
  config: LoggerConfig,
  level: LogLevel,
  message: String,
  context: List(#(String, String)),
) -> Nil {
  case should_log(config.min_level, level) {
    False -> Nil
    True -> {
      let entry =
        LogEntry(
          level: level,
          message: message,
          context: context,
          timestamp: None,
          error_code: None,
        )
      output_log(config, entry)
    }
  }
}

/// Log with error code
pub fn log_error_code(
  config: LoggerConfig,
  level: LogLevel,
  message: String,
  error_code: String,
  context: List(#(String, String)),
) -> Nil {
  case should_log(config.min_level, level) {
    False -> Nil
    True -> {
      let entry =
        LogEntry(
          level: level,
          message: message,
          context: context,
          timestamp: None,
          error_code: Some(error_code),
        )
      output_log(config, entry)
    }
  }
}

// =============================================================================
// Internal Functions
// =============================================================================

/// Check if message should be logged based on level
fn should_log(min_level: LogLevel, message_level: LogLevel) -> Bool {
  let min_severity = level_to_int(min_level)
  let msg_severity = level_to_int(message_level)
  msg_severity >= min_severity
}

/// Convert log level to integer for comparison
fn level_to_int(level: LogLevel) -> Int {
  case level {
    Debug -> 0
    Info -> 1
    Warn -> 2
    Error -> 3
  }
}

/// Output log entry based on configuration
fn output_log(config: LoggerConfig, entry: LogEntry) -> Nil {
  let formatted = case config.output_format {
    TextFormat -> format_text(config, entry)
    JsonFormat -> format_json(config, entry)
    CompactFormat -> format_compact(entry)
  }
  io.println(formatted)
}

/// Format log entry as human-readable text
fn format_text(config: LoggerConfig, entry: LogEntry) -> String {
  let level_str = format_level(entry.level)

  let error_code_str = case entry.error_code {
    Some(code) -> " [" <> code <> "]"
    None -> ""
  }

  let base = level_str <> error_code_str <> ": " <> entry.message

  case config.include_context && list.length(entry.context) > 0 {
    False -> base
    True -> {
      let context_str = format_context(entry.context)
      base <> "\n  Context: " <> context_str
    }
  }
}

/// Format log entry as JSON
fn format_json(config: LoggerConfig, entry: LogEntry) -> String {
  let level_str = level_to_string(entry.level)
  let msg = escape_json(entry.message)

  let error_code_field = case entry.error_code {
    Some(code) -> ", \"error_code\": \"" <> code <> "\""
    None -> ""
  }

  let context_field = case
    config.include_context && list.length(entry.context) > 0
  {
    False -> ""
    True -> {
      let ctx_json = format_context_json(entry.context)
      ", \"context\": " <> ctx_json
    }
  }

  "{\"level\": \""
  <> level_str
  <> "\", \"message\": \""
  <> msg
  <> "\""
  <> error_code_field
  <> context_field
  <> "}"
}

/// Format log entry in compact format
fn format_compact(entry: LogEntry) -> String {
  let level_abbr = case entry.level {
    Debug -> "D"
    Info -> "I"
    Warn -> "W"
    Error -> "E"
  }

  let error_code_str = case entry.error_code {
    Some(code) -> "[" <> code <> "] "
    None -> ""
  }

  level_abbr <> " " <> error_code_str <> entry.message
}

/// Format log level with color/style
fn format_level(level: LogLevel) -> String {
  case level {
    Debug -> "[DEBUG]"
    Info -> "[INFO] "
    Warn -> "[WARN] "
    Error -> "[ERROR]"
  }
}

/// Convert log level to string
fn level_to_string(level: LogLevel) -> String {
  case level {
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Error -> "error"
  }
}

/// Format context as string
fn format_context(context: List(#(String, String))) -> String {
  context
  |> list.map(fn(pair) {
    let #(key, value) = pair
    key <> "=" <> value
  })
  |> string.join(", ")
}

/// Format context as JSON object
fn format_context_json(context: List(#(String, String))) -> String {
  let fields =
    context
    |> list.map(fn(pair) {
      let #(key, value) = pair
      "\"" <> key <> "\": \"" <> escape_json(value) <> "\""
    })
    |> string.join(", ")

  "{" <> fields <> "}"
}

/// Escape string for JSON
fn escape_json(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

// =============================================================================
// Convenience Functions
// =============================================================================

/// Log an operation start
pub fn log_operation_start(config: LoggerConfig, operation: String) -> Nil {
  info(config, "Starting operation: " <> operation)
}

/// Log an operation success
pub fn log_operation_success(
  config: LoggerConfig,
  operation: String,
  duration_ms: Int,
) -> Nil {
  log_with_context(config, Info, "Operation completed: " <> operation, [
    #("duration_ms", int.to_string(duration_ms)),
  ])
}

/// Log an operation failure
pub fn log_operation_failure(
  config: LoggerConfig,
  operation: String,
  error_message: String,
) -> Nil {
  log_with_context(config, Error, "Operation failed: " <> operation, [
    #("error", error_message),
  ])
}
