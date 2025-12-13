# Changelog

All notable changes to anthropic_gleam will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **Retry Logic** (A4.1)
  - `anthropic/retry` module with exponential backoff
  - Configurable max retries, base delay, max delay, jitter factor
  - Automatic retry for rate limits (429), overloaded (529), timeouts, network errors
  - `retry_with_backoff/2` function with detailed result metadata
  - Pre-built configurations: `default_retry_config`, `aggressive_retry_config`, `no_retry_config`

- **Request Validation** (A4.2)
  - `anthropic/validation` module for comprehensive request validation
  - Validates messages, model name, max_tokens, temperature, top_p, top_k
  - Validates system prompt, stop sequences, and tool definitions
  - Model-specific token limits
  - Detailed validation errors with field and value context

- **Logging Hooks** (A4.3)
  - `anthropic/hooks` module for observability
  - Hook points: `on_request_start`, `on_request_end`, `on_retry`, `on_stream_event`
  - Pre-built hooks: `simple_logging_hooks`, `metrics_hooks`
  - Request and response summarization (without sensitive data)
  - Hook composition with `combine_hooks`

- **Documentation** (A4.4)
  - Comprehensive README with examples
  - CHANGELOG following Keep a Changelog format
  - CONTRIBUTING guide for contributors

## [0.3.0] - 2024-12-13

### Added

- **Tool Use Support** (A3.1-A3.5)
  - `anthropic/types/tool` module with PropertySchema, InputSchema, Tool, ToolChoice types
  - `anthropic/tools` module for tool use workflow
  - `anthropic/tools/builder` module with fluent builder API
  - Tool extraction from responses
  - Tool result submission
  - Tool dispatch with handler mapping

## [0.2.0] - 2024-12-13

### Added

- **Streaming Support** (A2.1-A2.5)
  - `anthropic/streaming/stream` module for streaming requests
  - `anthropic/streaming/events` module with typed streaming events
  - `anthropic/streaming/parser` module for SSE parsing
  - Real-time text accumulation
  - Stop reason handling

## [0.1.0] - 2024-12-13

### Added

- **Core API** (A1.1-A1.7)
  - `anthropic/api` module for Messages API
  - `anthropic/client` module for HTTP client
  - `anthropic/config` module for configuration
  - `anthropic/types/message` module for message types
  - `anthropic/types/request` module for request/response types
  - `anthropic/types/error` module for error handling
  - Environment variable configuration
  - JSON serialization/deserialization
  - Basic request validation

[Unreleased]: https://github.com/justin4957/foil/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/justin4957/foil/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/justin4957/foil/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/justin4957/foil/releases/tag/v0.1.0
