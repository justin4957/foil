//// Anthropic Gleam - A typed Gleam client for Anthropic's Claude API
////
//// This library provides a well-typed, idiomatic interface to Claude's API,
//// including support for streaming responses and tool use.
////
//// ## Quick Start
////
//// ```gleam
//// import anthropic/types/message.{user_message}
//// import anthropic/types/request.{create_request, request_to_json_string}
////
//// let request = create_request(
////   "claude-sonnet-4-20250514",
////   [user_message("Hello, Claude!")],
////   1024,
//// )
//// let json_string = request_to_json_string(request)
//// ```
////
//// ## Message Types
////
//// The library provides comprehensive types for the Messages API:
//// - `Role` - User or Assistant
//// - `ContentBlock` - TextBlock, ImageBlock, ToolUseBlock, ToolResultBlock
//// - `Message` - A complete message with role and content
////
//// ## Request/Response Types
////
//// - `CreateMessageRequest` - Request to create a message
//// - `CreateMessageResponse` - Response from the API
//// - `Usage` - Token usage information
//// - `StopReason` - Why generation stopped
////
//// ## Error Types
////
//// - `AnthropicError` - Sum type for all error categories
//// - `ApiErrorType` - Specific API error types
//// - `ApiErrorDetails` - Details from API error responses
////
//// All types support JSON encoding/decoding for API communication.

import anthropic/api
import anthropic/client
import anthropic/config
import anthropic/testing
import anthropic/types/error
import anthropic/types/message
import anthropic/types/request

// Re-export types for easier access
pub type Role =
  message.Role

pub type ContentBlock =
  message.ContentBlock

pub type Message =
  message.Message

pub type ImageSource =
  message.ImageSource

pub type ImageSourceType =
  message.ImageSourceType

// Re-export constructors
pub const user_message = message.user_message

pub const assistant_message = message.assistant_message

pub const text = message.text

pub const image = message.image

pub const tool_use = message.tool_use

pub const tool_result = message.tool_result

pub const tool_error = message.tool_error

// Re-export encoders
pub const role_to_json = message.role_to_json

pub const role_from_string = message.role_from_string

pub const role_to_string = message.role_to_string

pub const message_to_json = message.message_to_json

pub const messages_to_json = message.messages_to_json

pub const message_to_json_string = message.message_to_json_string

pub const content_block_to_json = message.content_block_to_json

pub const content_block_to_json_string = message.content_block_to_json_string

pub const image_source_to_json = message.image_source_to_json

// Re-export utility functions
pub const message_text = message.message_text

pub const has_tool_use = message.has_tool_use

pub const get_tool_uses = message.get_tool_uses

pub const content_block_type = message.content_block_type

// =============================================================================
// Configuration
// =============================================================================

// Re-export configuration types
pub type Config =
  config.Config

pub type ConfigOptions =
  config.ConfigOptions

// Re-export configuration defaults
pub const default_base_url = config.default_base_url

pub const default_timeout_ms = config.default_timeout_ms

pub const default_max_retries = config.default_max_retries

// Re-export configuration helpers
pub const config_options = config.config_options

pub const with_api_key = config.with_api_key

pub const with_base_url = config.with_base_url

pub const with_default_model = config.with_default_model

pub const with_timeout_ms = config.with_timeout_ms

pub const with_max_retries = config.with_max_retries

pub const load_config = config.load_config

// =============================================================================
// Request/Response Types (from anthropic/types/request)
// =============================================================================

// Re-export request/response types
pub type CreateMessageRequest =
  request.CreateMessageRequest

pub type CreateMessageResponse =
  request.CreateMessageResponse

pub type Usage =
  request.Usage

pub type StopReason =
  request.StopReason

pub type Metadata =
  request.Metadata

// Re-export request constructors and builders
pub const create_request = request.create_request

pub const with_system = request.with_system

pub const with_temperature = request.with_temperature

pub const with_top_p = request.with_top_p

pub const with_top_k = request.with_top_k

pub const with_stop_sequences = request.with_stop_sequences

pub const with_stream = request.with_stream

pub const with_metadata = request.with_metadata

pub const with_user_id = request.with_user_id

// Re-export request encoders
pub const request_to_json = request.request_to_json

pub const request_to_json_string = request.request_to_json_string

// Re-export response encoders
pub const response_to_json = request.response_to_json

pub const response_to_json_string = request.response_to_json_string

pub const create_response = request.create_response

pub const create_response_with_stop_sequence = request.create_response_with_stop_sequence

// Re-export response utility functions
pub const response_text = request.response_text

pub const response_has_tool_use = request.response_has_tool_use

pub const response_get_tool_uses = request.response_get_tool_uses

// Re-export StopReason functions
pub const stop_reason_to_string = request.stop_reason_to_string

pub const stop_reason_from_string = request.stop_reason_from_string

// Re-export Usage functions
pub const usage_to_json = request.usage_to_json

pub const usage = request.usage

// Re-export Metadata functions
pub const metadata_to_json = request.metadata_to_json

// =============================================================================
// Error Types (from anthropic/types/error)
// =============================================================================

// Re-export error types
pub type AnthropicError =
  error.AnthropicError

pub type ApiErrorType =
  error.ApiErrorType

pub type ApiErrorDetails =
  error.ApiErrorDetails

// Re-export error constructors
pub const api_error = error.api_error

pub const authentication_error = error.authentication_error

pub const invalid_request_error = error.invalid_request_error

pub const rate_limit_error = error.rate_limit_error

pub const internal_api_error = error.internal_api_error

pub const overloaded_error = error.overloaded_error

pub const http_error = error.http_error

pub const json_error = error.json_error

pub const config_error = error.config_error

pub const timeout_error = error.timeout_error

pub const network_error = error.network_error

pub const missing_api_key_error = error.missing_api_key_error

pub const invalid_api_key_error = error.invalid_api_key_error

// Re-export error details constructors
pub const api_error_details = error.api_error_details

pub const api_error_details_full = error.api_error_details_full

// Re-export error display functions
pub const error_to_string = error.error_to_string

pub const error_category = error.error_category

pub const api_error_details_to_string = error.api_error_details_to_string

// Re-export error predicates
pub const is_retryable = error.is_retryable

pub const is_authentication_error = error.is_authentication_error

pub const is_rate_limit_error = error.is_rate_limit_error

pub const is_overloaded_error = error.is_overloaded_error

pub const get_status_code = error.get_status_code

// Re-export error type conversion functions
pub const api_error_type_from_string = error.api_error_type_from_string

pub const api_error_type_to_string = error.api_error_type_to_string

// Re-export error JSON functions
pub const error_to_json = error.error_to_json

pub const error_to_json_string = error.error_to_json_string

pub const api_error_details_to_json = error.api_error_details_to_json

// =============================================================================
// Client (from anthropic/client)
// =============================================================================

// Re-export client type
pub type Client =
  client.Client

// Re-export client constants
pub const api_version = client.api_version

pub const messages_endpoint = client.messages_endpoint

// Re-export client constructors
pub const new_client = client.new

// Re-export HTTP functions
pub const post_json = client.post_json

pub const handle_response = client.handle_response

pub const post_and_handle = client.post_and_handle

// =============================================================================
// API (from anthropic/api)
// =============================================================================

// Re-export create_message function
pub const create_message = api.create_message

// =============================================================================
// Testing Utilities (from anthropic/testing)
// =============================================================================

// Re-export mock response builders
pub const mock_text_response = testing.mock_text_response

pub const mock_tool_use_response = testing.mock_tool_use_response

pub const mock_error_response = testing.mock_error_response

pub const mock_auth_error = testing.mock_auth_error

pub const mock_rate_limit_error = testing.mock_rate_limit_error

pub const mock_overloaded_error = testing.mock_overloaded_error

pub const mock_invalid_request_error = testing.mock_invalid_request_error

// Re-export mock body builders
pub const mock_text_response_body = testing.mock_text_response_body

pub const mock_tool_use_response_body = testing.mock_tool_use_response_body

pub const mock_error_body = testing.mock_error_body

// Re-export test fixtures
pub const fixture_simple_response = testing.fixture_simple_response

pub const fixture_conversation_response = testing.fixture_conversation_response

pub const fixture_tool_use_response = testing.fixture_tool_use_response

pub const fixture_max_tokens_response = testing.fixture_max_tokens_response

pub const fixture_stop_sequence_response = testing.fixture_stop_sequence_response

// Re-export integration test helpers
pub const has_api_key = testing.has_api_key
