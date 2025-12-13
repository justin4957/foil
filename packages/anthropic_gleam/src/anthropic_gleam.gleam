//// Anthropic Gleam - A typed Gleam client for Anthropic's Claude API
////
//// This library provides a well-typed, idiomatic interface to Claude's API,
//// including support for streaming responses and tool use.
////
//// ## Quick Start
////
//// ```gleam
//// import anthropic_gleam
//// import anthropic_gleam/types/message
////
//// // Coming soon: Basic usage example
//// ```

import anthropic/types/message

// Re-export types for easier access
pub type Role = message.Role
pub type ContentBlock = message.ContentBlock
pub type Message = message.Message
pub type ImageSource = message.ImageSource
