//// Core message types for the Anthropic Messages API
//// This module defines the fundamental types for working with Claude's API

/// Role of a message in a conversation
pub type Role {
  User
  Assistant
}

/// Content block in a message
pub type ContentBlock {
  TextBlock(text: String)
  ImageBlock(source: ImageSource)
  ToolUseBlock(id: String, name: String, input: String)
  ToolResultBlock(tool_use_id: String, content: String)
}

/// Image source for image content blocks
pub type ImageSource {
  ImageSource(
    image_type: String,
    media_type: String,
    data: String,
  )
}

/// A message in a conversation
pub type Message {
  Message(
    role: Role,
    content: List(ContentBlock),
  )
}
