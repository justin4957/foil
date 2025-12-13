//// Anthropic Gleam - A typed Gleam client for Anthropic's Claude API
////
//// This library provides a well-typed, idiomatic interface to Claude's API,
//// including support for streaming responses and tool use.
////
//// ## Quick Start
////
//// ```gleam
//// import anthropic/types/message.{user_message, message_to_json}
//// import gleam/json
////
//// let msg = user_message("Hello, Claude!")
//// let json_string = msg |> message_to_json |> json.to_string
//// ```
////
//// ## Message Types
////
//// The library provides comprehensive types for the Messages API:
//// - `Role` - User or Assistant
//// - `ContentBlock` - TextBlock, ImageBlock, ToolUseBlock, ToolResultBlock
//// - `Message` - A complete message with role and content
////
//// All types support JSON encoding for API communication.

import anthropic/types/message

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
