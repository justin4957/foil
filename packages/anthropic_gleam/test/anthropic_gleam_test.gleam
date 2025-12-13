import anthropic/types/message.{
  Assistant, Base64, ImageBlock, ImageSource, Message, TextBlock,
  ToolResultBlock, ToolUseBlock, User, assistant_message, content_block_to_json,
  content_block_to_json_string, content_block_type, get_tool_uses, has_tool_use,
  image, image_source_to_json, message, message_text, message_to_json,
  message_to_json_string, messages_to_json, role_from_string, role_to_json,
  role_to_string, text, tool_error, tool_result, tool_use, user_message,
}
import anthropic/types/request.{
  EndTurn, MaxTokens, Metadata, StopSequence, ToolUse, create_request,
  create_response, create_response_with_stop_sequence, metadata_to_json,
  request_to_json, request_to_json_string, response_get_tool_uses,
  response_has_tool_use, response_text, response_to_json,
  response_to_json_string, stop_reason_from_string, stop_reason_to_json,
  stop_reason_to_string, usage, usage_to_json, with_metadata,
  with_stop_sequences, with_stream, with_system, with_temperature, with_top_k,
  with_top_p, with_user_id,
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

// =============================================================================
// StopReason Tests
// =============================================================================

pub fn stop_reason_to_string_end_turn_test() {
  assert stop_reason_to_string(EndTurn) == "end_turn"
}

pub fn stop_reason_to_string_max_tokens_test() {
  assert stop_reason_to_string(MaxTokens) == "max_tokens"
}

pub fn stop_reason_to_string_stop_sequence_test() {
  assert stop_reason_to_string(StopSequence) == "stop_sequence"
}

pub fn stop_reason_to_string_tool_use_test() {
  assert stop_reason_to_string(ToolUse) == "tool_use"
}

pub fn stop_reason_from_string_end_turn_test() {
  assert stop_reason_from_string("end_turn") == Ok(EndTurn)
}

pub fn stop_reason_from_string_max_tokens_test() {
  assert stop_reason_from_string("max_tokens") == Ok(MaxTokens)
}

pub fn stop_reason_from_string_stop_sequence_test() {
  assert stop_reason_from_string("stop_sequence") == Ok(StopSequence)
}

pub fn stop_reason_from_string_tool_use_test() {
  assert stop_reason_from_string("tool_use") == Ok(ToolUse)
}

pub fn stop_reason_from_string_invalid_test() {
  assert stop_reason_from_string("invalid")
    == Error("Invalid stop reason: invalid")
}

pub fn stop_reason_to_json_test() {
  let result = stop_reason_to_json(EndTurn) |> json.to_string
  assert result == "\"end_turn\""
}

// =============================================================================
// Usage Tests
// =============================================================================

pub fn usage_constructor_test() {
  let u = usage(100, 50)
  assert u.input_tokens == 100
  assert u.output_tokens == 50
}

pub fn usage_to_json_test() {
  let u = usage(100, 50)
  let result = usage_to_json(u) |> json.to_string
  assert string.contains(result, "\"input_tokens\":100")
  assert string.contains(result, "\"output_tokens\":50")
}

// =============================================================================
// Metadata Tests
// =============================================================================

pub fn metadata_with_user_id_to_json_test() {
  let m = Metadata(user_id: Some("user_123"))
  let result = metadata_to_json(m) |> json.to_string
  assert string.contains(result, "\"user_id\":\"user_123\"")
}

pub fn metadata_without_user_id_to_json_test() {
  let m = Metadata(user_id: None)
  let result = metadata_to_json(m) |> json.to_string
  assert result == "{}"
}

// =============================================================================
// CreateMessageRequest Tests
// =============================================================================

pub fn create_request_basic_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
  assert req.model == "claude-sonnet-4-20250514"
  assert req.max_tokens == 1024
  assert req.system == None
  assert req.temperature == None
}

pub fn create_request_with_system_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_system("You are a helpful assistant.")
  assert req.system == Some("You are a helpful assistant.")
}

pub fn create_request_with_temperature_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_temperature(0.7)
  assert req.temperature == Some(0.7)
}

pub fn create_request_with_top_p_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_top_p(0.9)
  assert req.top_p == Some(0.9)
}

pub fn create_request_with_top_k_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_top_k(40)
  assert req.top_k == Some(40)
}

pub fn create_request_with_stop_sequences_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_stop_sequences(["END", "STOP"])
  assert req.stop_sequences == Some(["END", "STOP"])
}

pub fn create_request_with_stream_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_stream(True)
  assert req.stream == Some(True)
}

pub fn create_request_with_metadata_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_metadata(Metadata(user_id: Some("user_123")))
  let assert Some(m) = req.metadata
  assert m.user_id == Some("user_123")
}

pub fn create_request_with_user_id_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_user_id("user_456")
  let assert Some(m) = req.metadata
  assert m.user_id == Some("user_456")
}

pub fn create_request_chained_builders_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hello")], 1024)
    |> with_system("Be helpful")
    |> with_temperature(0.5)
    |> with_stream(True)
  assert req.system == Some("Be helpful")
  assert req.temperature == Some(0.5)
  assert req.stream == Some(True)
}

pub fn request_to_json_basic_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hi")], 1024)
  let result = request_to_json(req) |> json.to_string

  assert string.contains(result, "\"model\":\"claude-sonnet-4-20250514\"")
  assert string.contains(result, "\"max_tokens\":1024")
  assert string.contains(result, "\"messages\":")
}

pub fn request_to_json_with_options_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hi")], 1024)
    |> with_system("Be brief")
    |> with_temperature(0.8)
  let result = request_to_json(req) |> json.to_string

  assert string.contains(result, "\"system\":\"Be brief\"")
  assert string.contains(result, "\"temperature\":0.8")
}

pub fn request_to_json_with_stop_sequences_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Hi")], 1024)
    |> with_stop_sequences(["END"])
  let result = request_to_json(req) |> json.to_string

  assert string.contains(result, "\"stop_sequences\":")
  assert string.contains(result, "\"END\"")
}

pub fn request_to_json_string_test() {
  let req =
    create_request("claude-sonnet-4-20250514", [user_message("Test")], 512)
  let result = request_to_json_string(req)

  assert string.is_empty(result) == False
  assert string.contains(result, "claude-sonnet-4-20250514")
}

// =============================================================================
// CreateMessageResponse Tests
// =============================================================================

pub fn create_response_test() {
  let resp =
    create_response(
      "msg_123",
      [TextBlock(text: "Hello!")],
      "claude-sonnet-4-20250514",
      Some(EndTurn),
      usage(10, 20),
    )

  assert resp.id == "msg_123"
  assert resp.response_type == "message"
  assert resp.role == Assistant
  assert resp.model == "claude-sonnet-4-20250514"
  assert resp.stop_reason == Some(EndTurn)
  assert resp.usage.input_tokens == 10
  assert resp.usage.output_tokens == 20
}

pub fn create_response_with_stop_sequence_test() {
  let resp =
    create_response_with_stop_sequence(
      "msg_456",
      [TextBlock(text: "Done")],
      "claude-sonnet-4-20250514",
      StopSequence,
      "END",
      usage(15, 25),
    )

  assert resp.stop_reason == Some(StopSequence)
  assert resp.stop_sequence == Some("END")
}

pub fn response_text_test() {
  let resp =
    create_response(
      "msg_123",
      [TextBlock(text: "Hello "), TextBlock(text: "World!")],
      "claude-sonnet-4-20250514",
      Some(EndTurn),
      usage(10, 20),
    )

  assert response_text(resp) == "Hello World!"
}

pub fn response_text_with_tool_use_test() {
  let resp =
    create_response(
      "msg_123",
      [
        TextBlock(text: "Let me help"),
        ToolUseBlock(id: "tool_1", name: "search", input: "{}"),
      ],
      "claude-sonnet-4-20250514",
      Some(ToolUse),
      usage(10, 20),
    )

  assert response_text(resp) == "Let me help"
}

pub fn response_has_tool_use_true_test() {
  let resp =
    create_response(
      "msg_123",
      [
        TextBlock(text: "Using tool"),
        ToolUseBlock(id: "tool_1", name: "calc", input: "{}"),
      ],
      "claude-sonnet-4-20250514",
      Some(ToolUse),
      usage(10, 20),
    )

  assert response_has_tool_use(resp) == True
}

pub fn response_has_tool_use_false_test() {
  let resp =
    create_response(
      "msg_123",
      [TextBlock(text: "Just text")],
      "claude-sonnet-4-20250514",
      Some(EndTurn),
      usage(10, 20),
    )

  assert response_has_tool_use(resp) == False
}

pub fn response_get_tool_uses_test() {
  let tool1 = ToolUseBlock(id: "t1", name: "tool1", input: "{}")
  let tool2 = ToolUseBlock(id: "t2", name: "tool2", input: "{}")
  let resp =
    create_response(
      "msg_123",
      [TextBlock(text: "Using tools"), tool1, tool2],
      "claude-sonnet-4-20250514",
      Some(ToolUse),
      usage(10, 20),
    )

  let tools = response_get_tool_uses(resp)
  assert list.length(tools) == 2
}

pub fn response_to_json_test() {
  let resp =
    create_response(
      "msg_123",
      [TextBlock(text: "Hello!")],
      "claude-sonnet-4-20250514",
      Some(EndTurn),
      usage(10, 20),
    )
  let result = response_to_json(resp) |> json.to_string

  assert string.contains(result, "\"id\":\"msg_123\"")
  assert string.contains(result, "\"type\":\"message\"")
  assert string.contains(result, "\"role\":\"assistant\"")
  assert string.contains(result, "\"model\":\"claude-sonnet-4-20250514\"")
  assert string.contains(result, "\"stop_reason\":\"end_turn\"")
  assert string.contains(result, "\"input_tokens\":10")
  assert string.contains(result, "\"output_tokens\":20")
}

pub fn response_to_json_with_stop_sequence_test() {
  let resp =
    create_response_with_stop_sequence(
      "msg_456",
      [TextBlock(text: "Done")],
      "claude-sonnet-4-20250514",
      StopSequence,
      "END",
      usage(15, 25),
    )
  let result = response_to_json(resp) |> json.to_string

  assert string.contains(result, "\"stop_reason\":\"stop_sequence\"")
  assert string.contains(result, "\"stop_sequence\":\"END\"")
}

pub fn response_to_json_string_test() {
  let resp =
    create_response(
      "msg_789",
      [TextBlock(text: "Test")],
      "claude-sonnet-4-20250514",
      None,
      usage(5, 10),
    )
  let result = response_to_json_string(resp)

  assert string.is_empty(result) == False
  assert string.contains(result, "msg_789")
}
