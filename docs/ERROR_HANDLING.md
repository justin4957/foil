# Error Handling Guide

Complete guide to error handling in the Foil Modal Logic Engine.

## Overview

Foil uses a structured error system with:
- **Type-safe errors**: All errors use the `FoilError` type
- **Error codes**: Unique codes for each error category (E001-E599)
- **Context-aware messages**: Line numbers, snippets, and suggestions
- **Structured logging**: DEBUG, INFO, WARN, ERROR levels
- **Actionable guidance**: Every error includes resolution steps

## Quick Start

### Using Errors in Code

```gleam
import modal_logic/error
import modal_logic/logger

// Create and log an error
let config = logger.default_config()
let err = error.syntax_error(5, 12, "Necessary(p → q)", "Invalid character")

// Format for display
let message = error.format_error(err)
io.println(message)

// Log the error
logger.log_error_code(
  config,
  logger.Error,
  "Parse failed",
  error.get_error_code(err),
  [#("file", "input.txt")],
)

// Check if recoverable
case error.is_recoverable(err) {
  True -> // Retry logic
  False -> // Fatal error
}
```

### Error Categories

| Category | Range | Recoverable | Examples |
|----------|-------|-------------|----------|
| Parse | E001-E099 | No | Syntax errors, unexpected tokens |
| Translation | E101-E199 | Yes | LLM API errors, timeouts |
| Verification | E201-E299 | Partial | Z3 errors, timeouts |
| Configuration | E301-E399 | No | Missing params, invalid values |
| System | E401-E499 | Yes | File I/O, network errors |
| Validation | E501-E599 | No | Empty input, format errors |

## Error Handling Patterns

### Pattern 1: Try-Catch with Logging

```gleam
import modal_logic/error
import modal_logic/logger

pub fn validate_with_logging(formula: String, config: LoggerConfig) -> Result(ValidationResult, error.FoilError) {
  logger.log_operation_start(config, "validation")

  case validate_formula(formula) {
    Ok(result) -> {
      logger.log_operation_success(config, "validation", 150)
      Ok(result)
    }
    Error(err) -> {
      let error_msg = error.format_error(err)
      logger.log_operation_failure(config, "validation", error_msg)
      Error(err)
    }
  }
}
```

### Pattern 2: Retry on Recoverable Errors

```gleam
pub fn validate_with_retry(formula: String, max_retries: Int) -> Result(a, error.FoilError) {
  validate_with_retry_helper(formula, 0, max_retries)
}

fn validate_with_retry_helper(formula: String, retry_count: Int, max_retries: Int) -> Result(a, error.FoilError) {
  case validate_formula(formula) {
    Ok(result) -> Ok(result)
    Error(err) -> {
      case error.is_recoverable(err) && retry_count < max_retries {
        True -> {
          // Wait and retry
          validate_with_retry_helper(formula, retry_count + 1, max_retries)
        }
        False -> Error(err)
      }
    }
  }
}
```

### Pattern 3: Error Context Enrichment

```gleam
pub fn enrich_error(err: error.FoilError, additional_context: String) -> error.FoilError {
  case err {
    error.ParseError(code, line, column, snippet, message, suggestion) -> {
      let enriched_message = message <> " (" <> additional_context <> ")"
      error.ParseError(code, line, column, snippet, enriched_message, suggestion)
    }
    _ -> err
  }
}
```

## Logging

### Log Levels

Configure logging verbosity based on environment:

```gleam
// Development: See everything
let dev_config = logger.debug_config()

// Production: Warnings and errors only
let prod_config = logger.production_config()

// Custom configuration
let custom_config = logger.LoggerConfig(
  min_level: logger.Info,
  include_timestamp: True,
  include_context: True,
  output_format: logger.JsonFormat,
)
```

### Logging with Context

```gleam
// Simple logging
logger.debug(config, "Debug message")
logger.info(config, "Info message")
logger.warn(config, "Warning message")
logger.error(config, "Error message")

// With context
logger.log_with_context(
  config,
  logger.Error,
  "Validation failed",
  [
    #("formula", "□p → p"),
    #("system", "K"),
    #("duration_ms", "150"),
  ],
)

// With error code
logger.log_error_code(
  config,
  logger.Error,
  "Z3 timeout",
  "E202",
  [#("formula", "complex formula")],
)
```

### Output Formats

**Text Format** (default, human-readable):
```
[ERROR] [E202]: Z3 timeout
  Context: formula=complex formula
```

**JSON Format** (machine-parseable):
```json
{"level": "error", "message": "Z3 timeout", "error_code": "E202", "context": {"formula": "complex formula"}}
```

**Compact Format** (minimal):
```
E [E202] Z3 timeout
```

## Error Codes Reference

See [ERROR_CODES.md](ERROR_CODES.md) for the complete catalog of error codes with:
- Detailed descriptions
- Example error messages
- Common causes
- Resolution steps

## Integration with Interfaces

### API Error Responses

API endpoints automatically convert `FoilError` to HTTP responses:

```json
{
  "error": {
    "code": "E001",
    "message": "Parse Error at line 5, column 12",
    "details": "Expected closing parenthesis, found '→'",
    "suggestion": "Check for matching parentheses and correct operator syntax"
  }
}
```

HTTP status codes:
- Parse/Validation errors: 400 (Bad Request)
- Translation errors: 502 (Bad Gateway)
- Verification timeouts: 504 (Gateway Timeout)
- Configuration errors: 400 (Bad Request)
- System errors: 500 (Internal Server Error)

### CLI Error Display

CLI formats errors with color and context:

```
[E001] Parse Error at line 5, column 12
  5 | Necessary(p → q)
                  ^
  Expected closing parenthesis, found '→'

Suggestion: Check for matching parentheses and correct operator syntax
```

### WebSocket Error Events

WebSocket sends error events:

```json
{
  "type": "error",
  "timestamp": "2026-01-14T23:00:00Z",
  "error": {
    "code": "E202",
    "message": "Verification timeout",
    "recoverable": true
  }
}
```

## Best Practices

### For Users

1. **Read Error Codes**: They categorize the issue
2. **Follow Suggestions**: Errors include actionable guidance
3. **Enable Debug Logging**: For detailed troubleshooting
4. **Report Persistent Errors**: Help improve the system

### For Developers

1. **Always Use Structured Errors**: Never return plain strings
2. **Provide Context**: Line numbers, snippets, values
3. **Include Suggestions**: Help users fix issues
4. **Test Error Paths**: Write tests for error scenarios
5. **Log Appropriately**: Use correct log levels
6. **Document Error Codes**: Update ERROR_CODES.md

## Common Error Scenarios

### Scenario 1: Parse Error in User Input

```gleam
case parse_formula(input) {
  Ok(formula) -> // Continue
  Error(parse_err) -> {
    let err = error.syntax_error(
      line,
      column,
      snippet,
      "Invalid syntax",
    )
    logger.error(config, error.format_error(err))
    Error(err)
  }
}
```

### Scenario 2: LLM API Failure with Retry

```gleam
fn translate_with_retry(text: String, retries: Int) -> Result(Translation, error.FoilError) {
  case call_llm_api(text) {
    Ok(response) -> Ok(response)
    Error(api_err) -> {
      let err = error.llm_api_error(api_err, retries, text)

      case error.is_recoverable(err) && retries < 3 {
        True -> {
          logger.warn(config, "Retrying LLM call...")
          translate_with_retry(text, retries + 1)
        }
        False -> Error(err)
      }
    }
  }
}
```

### Scenario 3: Configuration Validation

```gleam
pub fn validate_config(params: Dict(String, String)) -> Result(Config, error.FoilError) {
  case dict.get(params, "api_key") {
    Error(_) -> {
      Error(error.missing_parameter("api_key", "Valid Anthropic API key"))
    }
    Ok(key) -> {
      case string.length(key) > 0 {
        False -> Error(error.empty_input_error("api_key"))
        True -> Ok(Config(api_key: key))
      }
    }
  }
}
```

## Error Testing

### Testing Error Scenarios

```gleam
import modal_logic/error
import gleeunit/should

pub fn syntax_error_formatting_test() {
  let err = error.syntax_error(5, 12, "test snippet", "Invalid syntax")

  let formatted = error.format_error(err)
  formatted |> should_contain("E001")
  formatted |> should_contain("line 5")
  formatted |> should_contain("column 12")
}

pub fn error_is_recoverable_test() {
  let timeout_err = error.z3_timeout_error("formula")
  error.is_recoverable(timeout_err) |> should.be_true()

  let parse_err = error.syntax_error(1, 1, "test", "msg")
  error.is_recoverable(parse_err) |> should.be_false()
}
```

## Performance Considerations

### Error Creation

- Error construction is lightweight
- Format errors only when displaying to user
- Avoid formatting in hot paths

### Logging

- Logging respects configured minimum level
- Debug logs are skipped in production
- Use structured logging for searchability

### Error Recovery

- Implement exponential backoff for retries
- Limit maximum retry attempts
- Log retry attempts for monitoring

## See Also

- [ERROR_CODES.md](ERROR_CODES.md) - Complete error code catalog
- [API Documentation](API.md) - API error responses
- [CLI Documentation](../packages/modal_logic/README.md#cli) - CLI error display
- [Testing Guide](../packages/modal_logic/docs/TESTING.md) - Error testing patterns
