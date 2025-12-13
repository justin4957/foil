import anthropic/types/message.{
  Assistant, Base64, ImageBlock, ImageSource, Message, TextBlock,
  ToolResultBlock, ToolUseBlock, User, assistant_message, content_block_to_json,
  content_block_to_json_string, content_block_type, get_tool_uses, has_tool_use,
  image, image_source_to_json, message, message_text, message_to_json,
  message_to_json_string, messages_to_json, role_from_string, role_to_json,
  role_to_string, text, tool_error, tool_result, tool_use, user_message,
}
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// Role Tests
// =============================================================================

pub fn role_to_json_user_test() {
  let result = role_to_json(User) |> json.to_string
  assert result == "\"user\""
}

pub fn role_to_json_assistant_test() {
  let result = role_to_json(Assistant) |> json.to_string
  assert result == "\"assistant\""
}

pub fn role_from_string_user_test() {
  let result = role_from_string("user")
  assert result == Ok(User)
}

pub fn role_from_string_assistant_test() {
  let result = role_from_string("assistant")
  assert result == Ok(Assistant)
}

pub fn role_from_string_invalid_test() {
  let result = role_from_string("invalid")
  assert result == Error("Invalid role: invalid")
}

pub fn role_to_string_user_test() {
  let result = role_to_string(User)
  assert result == "user"
}

pub fn role_to_string_assistant_test() {
  let result = role_to_string(Assistant)
  assert result == "assistant"
}

// =============================================================================
// ContentBlock Tests
// =============================================================================

pub fn text_block_to_json_test() {
  let block = TextBlock(text: "Hello, world!")
  let result = content_block_to_json(block) |> json.to_string

  assert string.contains(result, "\"type\":\"text\"")
  assert string.contains(result, "\"text\":\"Hello, world!\"")
}

pub fn image_block_to_json_test() {
  let source =
    ImageSource(source_type: Base64, media_type: "image/png", data: "abc123")
  let block = ImageBlock(source: source)
  let result = content_block_to_json(block) |> json.to_string

  assert string.contains(result, "\"type\":\"image\"")
  assert string.contains(result, "\"media_type\":\"image/png\"")
  assert string.contains(result, "\"data\":\"abc123\"")
}

pub fn tool_use_block_to_json_test() {
  let block =
    ToolUseBlock(
      id: "tool_123",
      name: "get_weather",
      input: "{\"city\":\"NYC\"}",
    )
  let result = content_block_to_json(block) |> json.to_string

  assert string.contains(result, "\"type\":\"tool_use\"")
  assert string.contains(result, "\"id\":\"tool_123\"")
  assert string.contains(result, "\"name\":\"get_weather\"")
}

pub fn tool_result_block_to_json_test() {
  let block =
    ToolResultBlock(tool_use_id: "tool_123", content: "72°F", is_error: None)
  let result = content_block_to_json(block) |> json.to_string

  assert string.contains(result, "\"type\":\"tool_result\"")
  assert string.contains(result, "\"tool_use_id\":\"tool_123\"")
  assert string.contains(result, "\"content\":\"72°F\"")
  // Should not contain is_error when None
  assert !string.contains(result, "is_error")
}

pub fn tool_result_block_with_error_to_json_test() {
  let block =
    ToolResultBlock(
      tool_use_id: "tool_123",
      content: "Failed to fetch",
      is_error: Some(True),
    )
  let result = content_block_to_json(block) |> json.to_string

  assert string.contains(result, "\"is_error\":true")
}

pub fn content_block_type_test() {
  assert content_block_type(TextBlock(text: "hi")) == "text"
  assert content_block_type(
      ImageBlock(source: ImageSource(
        source_type: Base64,
        media_type: "image/png",
        data: "",
      )),
    )
    == "image"
  assert content_block_type(ToolUseBlock(id: "", name: "", input: ""))
    == "tool_use"
  assert content_block_type(ToolResultBlock(
      tool_use_id: "",
      content: "",
      is_error: None,
    ))
    == "tool_result"
}

// =============================================================================
// ImageSource Tests
// =============================================================================

pub fn image_source_to_json_test() {
  let source =
    ImageSource(
      source_type: Base64,
      media_type: "image/jpeg",
      data: "base64encodeddata",
    )
  let result = image_source_to_json(source) |> json.to_string

  assert string.contains(result, "\"type\":\"base64\"")
  assert string.contains(result, "\"media_type\":\"image/jpeg\"")
  assert string.contains(result, "\"data\":\"base64encodeddata\"")
}

// =============================================================================
// Message Tests
// =============================================================================

pub fn message_to_json_test() {
  let msg = Message(role: User, content: [TextBlock(text: "Hello!")])
  let result = message_to_json(msg) |> json.to_string

  assert string.contains(result, "\"role\":\"user\"")
  assert string.contains(result, "\"content\":")
  assert string.contains(result, "\"text\":\"Hello!\"")
}

pub fn message_with_multiple_blocks_test() {
  let msg =
    Message(role: User, content: [
      TextBlock(text: "Check this image:"),
      ImageBlock(source: ImageSource(
        source_type: Base64,
        media_type: "image/png",
        data: "abc",
      )),
    ])
  let result = message_to_json(msg) |> json.to_string

  assert string.contains(result, "Check this image:")
  assert string.contains(result, "\"type\":\"image\"")
}

pub fn messages_to_json_test() {
  let msgs = [
    Message(role: User, content: [TextBlock(text: "Hi")]),
    Message(role: Assistant, content: [TextBlock(text: "Hello!")]),
  ]
  let result = messages_to_json(msgs) |> json.to_string

  assert string.starts_with(result, "[")
  assert string.ends_with(result, "]")
  assert string.contains(result, "\"role\":\"user\"")
  assert string.contains(result, "\"role\":\"assistant\"")
}

pub fn message_text_single_block_test() {
  let msg = Message(role: User, content: [TextBlock(text: "Hello")])
  assert message_text(msg) == "Hello"
}

pub fn message_text_multiple_blocks_test() {
  let msg =
    Message(role: User, content: [
      TextBlock(text: "Hello "),
      TextBlock(text: "World"),
    ])
  assert message_text(msg) == "Hello World"
}

pub fn message_text_with_non_text_blocks_test() {
  let msg =
    Message(role: User, content: [
      TextBlock(text: "Text"),
      ImageBlock(source: ImageSource(
        source_type: Base64,
        media_type: "image/png",
        data: "",
      )),
    ])
  assert message_text(msg) == "Text"
}

pub fn has_tool_use_true_test() {
  let msg =
    Message(role: Assistant, content: [
      TextBlock(text: "Let me help"),
      ToolUseBlock(id: "123", name: "search", input: "{}"),
    ])
  assert has_tool_use(msg) == True
}

pub fn has_tool_use_false_test() {
  let msg = Message(role: Assistant, content: [TextBlock(text: "Hello")])
  assert has_tool_use(msg) == False
}

pub fn get_tool_uses_test() {
  let tool1 = ToolUseBlock(id: "1", name: "tool1", input: "{}")
  let tool2 = ToolUseBlock(id: "2", name: "tool2", input: "{}")
  let msg =
    Message(role: Assistant, content: [
      TextBlock(text: "Using tools:"),
      tool1,
      tool2,
    ])
  let tools = get_tool_uses(msg)
  assert list.length(tools) == 2
}

// =============================================================================
// Convenience Constructor Tests
// =============================================================================

pub fn user_message_test() {
  let msg = user_message("Hello, Claude!")
  assert msg.role == User
  assert list.length(msg.content) == 1
  let assert Ok(TextBlock(text: t)) = list.first(msg.content)
  assert t == "Hello, Claude!"
}

pub fn assistant_message_test() {
  let msg = assistant_message("Hello!")
  assert msg.role == Assistant
}

pub fn text_constructor_test() {
  let block = text("test content")
  assert block == TextBlock(text: "test content")
}

pub fn image_constructor_test() {
  let block = image("image/png", "base64data")
  let assert ImageBlock(source: ImageSource(
    source_type: Base64,
    media_type: mt,
    data: d,
  )) = block
  assert mt == "image/png"
  assert d == "base64data"
}

pub fn tool_use_constructor_test() {
  let block = tool_use("id1", "my_tool", "{\"arg\":1}")
  assert block == ToolUseBlock(id: "id1", name: "my_tool", input: "{\"arg\":1}")
}

pub fn tool_result_constructor_test() {
  let block = tool_result("id1", "success")
  assert block
    == ToolResultBlock(tool_use_id: "id1", content: "success", is_error: None)
}

pub fn tool_error_constructor_test() {
  let block = tool_error("id1", "failed")
  assert block
    == ToolResultBlock(
      tool_use_id: "id1",
      content: "failed",
      is_error: Some(True),
    )
}

pub fn message_constructor_test() {
  let msg = message(User, [text("Hello"), text("World")])
  assert msg.role == User
  assert list.length(msg.content) == 2
}

// =============================================================================
// JSON String Helper Tests
// =============================================================================

pub fn message_to_json_string_test() {
  let msg = user_message("Test")
  let result = message_to_json_string(msg)
  assert string.is_empty(result) == False
  assert string.contains(result, "Test")
}

pub fn content_block_to_json_string_test() {
  let block = text("Hello")
  let result = content_block_to_json_string(block)
  assert string.contains(result, "Hello")
}
