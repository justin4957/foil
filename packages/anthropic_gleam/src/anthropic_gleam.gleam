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
//// All types support JSON encoding/decoding for API communication.

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
