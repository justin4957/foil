import anthropic/types/error.{
  ApiError, AuthenticationError, ConfigError, HttpError, InternalApiError,
  InvalidRequestError, JsonError, NetworkError, NotFoundError, OverloadedError,
  PermissionError, RateLimitError, TimeoutError, UnknownApiError,
  api_error_details, api_error_details_full, api_error_details_to_json,
  api_error_details_to_string, api_error_type_from_string,
  api_error_type_to_string, authentication_error, config_error, error_category,
  error_to_json, error_to_json_string, error_to_string, get_status_code,
  http_error, internal_api_error, invalid_api_key_error, invalid_request_error,
  is_authentication_error, is_overloaded_error, is_rate_limit_error,
  is_retryable, json_error, missing_api_key_error, network_error,
  overloaded_error, rate_limit_error, timeout_error,
}
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

// =============================================================================
// ApiErrorType Tests
// =============================================================================

pub fn api_error_type_from_string_authentication_test() {
  assert api_error_type_from_string("authentication_error")
    == AuthenticationError
}

pub fn api_error_type_from_string_invalid_request_test() {
  assert api_error_type_from_string("invalid_request_error")
    == InvalidRequestError
}

pub fn api_error_type_from_string_rate_limit_test() {
  assert api_error_type_from_string("rate_limit_error") == RateLimitError
}

pub fn api_error_type_from_string_api_error_test() {
  assert api_error_type_from_string("api_error") == InternalApiError
}

pub fn api_error_type_from_string_overloaded_test() {
  assert api_error_type_from_string("overloaded_error") == OverloadedError
}

pub fn api_error_type_from_string_permission_test() {
  assert api_error_type_from_string("permission_error") == PermissionError
}

pub fn api_error_type_from_string_not_found_test() {
  assert api_error_type_from_string("not_found_error") == NotFoundError
}

pub fn api_error_type_from_string_unknown_test() {
  assert api_error_type_from_string("some_new_error")
    == UnknownApiError("some_new_error")
}

pub fn api_error_type_to_string_authentication_test() {
  assert api_error_type_to_string(AuthenticationError) == "authentication_error"
}

pub fn api_error_type_to_string_invalid_request_test() {
  assert api_error_type_to_string(InvalidRequestError)
    == "invalid_request_error"
}

pub fn api_error_type_to_string_rate_limit_test() {
  assert api_error_type_to_string(RateLimitError) == "rate_limit_error"
}

pub fn api_error_type_to_string_api_error_test() {
  assert api_error_type_to_string(InternalApiError) == "api_error"
}

pub fn api_error_type_to_string_overloaded_test() {
  assert api_error_type_to_string(OverloadedError) == "overloaded_error"
}

pub fn api_error_type_to_string_unknown_test() {
  assert api_error_type_to_string(UnknownApiError("custom")) == "custom"
}

// =============================================================================
// ApiErrorDetails Tests
// =============================================================================

pub fn api_error_details_basic_test() {
  let details = api_error_details(AuthenticationError, "Invalid API key")
  assert details.error_type == AuthenticationError
  assert details.message == "Invalid API key"
  assert details.param == None
  assert details.code == None
}

pub fn api_error_details_full_test() {
  let details =
    api_error_details_full(
      InvalidRequestError,
      "Missing required field",
      Some("messages"),
      Some("MISSING_FIELD"),
    )
  assert details.error_type == InvalidRequestError
  assert details.message == "Missing required field"
  assert details.param == Some("messages")
  assert details.code == Some("MISSING_FIELD")
}

pub fn api_error_details_to_string_basic_test() {
  let details = api_error_details(RateLimitError, "Too many requests")
  let result = api_error_details_to_string(details)
  assert result == "rate_limit_error: Too many requests"
}

pub fn api_error_details_to_string_with_param_test() {
  let details =
    api_error_details_full(
      InvalidRequestError,
      "Invalid value",
      Some("temperature"),
      None,
    )
  let result = api_error_details_to_string(details)
  assert string.contains(result, "invalid_request_error")
  assert string.contains(result, "Invalid value")
  assert string.contains(result, "(param: temperature)")
}

pub fn api_error_details_to_string_with_code_test() {
  let details =
    api_error_details_full(
      InvalidRequestError,
      "Invalid value",
      None,
      Some("ERR_001"),
    )
  let result = api_error_details_to_string(details)
  assert string.contains(result, "[code: ERR_001]")
}

pub fn api_error_details_to_json_test() {
  let details = api_error_details(AuthenticationError, "Invalid key")
  let result = api_error_details_to_json(details) |> json.to_string
  assert string.contains(result, "\"type\":\"authentication_error\"")
  assert string.contains(result, "\"message\":\"Invalid key\"")
}

// =============================================================================
// Error Constructor Tests
// =============================================================================

pub fn authentication_error_constructor_test() {
  let err = authentication_error("Invalid API key")
  let assert ApiError(status_code: status, details: details) = err
  assert status == 401
  assert details.error_type == AuthenticationError
  assert details.message == "Invalid API key"
}

pub fn invalid_request_error_constructor_test() {
  let err = invalid_request_error("Missing messages field")
  let assert ApiError(status_code: status, details: details) = err
  assert status == 400
  assert details.error_type == InvalidRequestError
}

pub fn rate_limit_error_constructor_test() {
  let err = rate_limit_error("Rate limit exceeded")
  let assert ApiError(status_code: status, details: details) = err
  assert status == 429
  assert details.error_type == RateLimitError
}

pub fn internal_api_error_constructor_test() {
  let err = internal_api_error("Internal server error")
  let assert ApiError(status_code: status, details: details) = err
  assert status == 500
  assert details.error_type == InternalApiError
}

pub fn overloaded_error_constructor_test() {
  let err = overloaded_error("API is overloaded")
  let assert ApiError(status_code: status, details: details) = err
  assert status == 529
  assert details.error_type == OverloadedError
}

pub fn http_error_constructor_test() {
  let err = http_error("Connection refused")
  let assert HttpError(reason: reason) = err
  assert reason == "Connection refused"
}

pub fn json_error_constructor_test() {
  let err = json_error("Invalid JSON syntax")
  let assert JsonError(reason: reason) = err
  assert reason == "Invalid JSON syntax"
}

pub fn config_error_constructor_test() {
  let err = config_error("Invalid configuration")
  let assert ConfigError(reason: reason) = err
  assert reason == "Invalid configuration"
}

pub fn timeout_error_constructor_test() {
  let err = timeout_error(30_000)
  let assert TimeoutError(timeout_ms: ms) = err
  assert ms == 30_000
}

pub fn network_error_constructor_test() {
  let err = network_error("DNS resolution failed")
  let assert NetworkError(reason: reason) = err
  assert reason == "DNS resolution failed"
}

pub fn missing_api_key_error_constructor_test() {
  let err = missing_api_key_error()
  let assert ConfigError(reason: reason) = err
  assert string.contains(reason, "API key")
}

pub fn invalid_api_key_error_constructor_test() {
  let err = invalid_api_key_error()
  let assert ConfigError(reason: reason) = err
  assert string.contains(reason, "API key")
}

// =============================================================================
// Error Display Tests
// =============================================================================

pub fn error_to_string_api_error_test() {
  let err = authentication_error("Invalid API key")
  let result = error_to_string(err)
  assert string.contains(result, "API Error")
  assert string.contains(result, "401")
  assert string.contains(result, "authentication_error")
  assert string.contains(result, "Invalid API key")
}

pub fn error_to_string_http_error_test() {
  let err = http_error("Connection timeout")
  let result = error_to_string(err)
  assert result == "HTTP Error: Connection timeout"
}

pub fn error_to_string_json_error_test() {
  let err = json_error("Parse error at line 5")
  let result = error_to_string(err)
  assert result == "JSON Error: Parse error at line 5"
}

pub fn error_to_string_config_error_test() {
  let err = config_error("Missing API key")
  let result = error_to_string(err)
  assert result == "Configuration Error: Missing API key"
}

pub fn error_to_string_timeout_error_test() {
  let err = timeout_error(60_000)
  let result = error_to_string(err)
  assert string.contains(result, "Timeout Error")
  assert string.contains(result, "60000ms")
}

pub fn error_to_string_network_error_test() {
  let err = network_error("No route to host")
  let result = error_to_string(err)
  assert result == "Network Error: No route to host"
}

pub fn error_category_api_test() {
  let err = authentication_error("test")
  assert error_category(err) == "api"
}

pub fn error_category_http_test() {
  let err = http_error("test")
  assert error_category(err) == "http"
}

pub fn error_category_json_test() {
  let err = json_error("test")
  assert error_category(err) == "json"
}

pub fn error_category_config_test() {
  let err = config_error("test")
  assert error_category(err) == "config"
}

pub fn error_category_timeout_test() {
  let err = timeout_error(1000)
  assert error_category(err) == "timeout"
}

pub fn error_category_network_test() {
  let err = network_error("test")
  assert error_category(err) == "network"
}

// =============================================================================
// Error Predicate Tests
// =============================================================================

pub fn is_retryable_rate_limit_test() {
  let err = rate_limit_error("test")
  assert is_retryable(err) == True
}

pub fn is_retryable_overloaded_test() {
  let err = overloaded_error("test")
  assert is_retryable(err) == True
}

pub fn is_retryable_internal_api_test() {
  let err = internal_api_error("test")
  assert is_retryable(err) == True
}

pub fn is_retryable_http_test() {
  let err = http_error("test")
  assert is_retryable(err) == True
}

pub fn is_retryable_timeout_test() {
  let err = timeout_error(1000)
  assert is_retryable(err) == True
}

pub fn is_retryable_network_test() {
  let err = network_error("test")
  assert is_retryable(err) == True
}

pub fn is_retryable_auth_test() {
  let err = authentication_error("test")
  assert is_retryable(err) == False
}

pub fn is_retryable_config_test() {
  let err = config_error("test")
  assert is_retryable(err) == False
}

pub fn is_retryable_json_test() {
  let err = json_error("test")
  assert is_retryable(err) == False
}

pub fn is_authentication_error_true_test() {
  let err = authentication_error("test")
  assert is_authentication_error(err) == True
}

pub fn is_authentication_error_false_test() {
  let err = rate_limit_error("test")
  assert is_authentication_error(err) == False
}

pub fn is_rate_limit_error_true_test() {
  let err = rate_limit_error("test")
  assert is_rate_limit_error(err) == True
}

pub fn is_rate_limit_error_false_test() {
  let err = authentication_error("test")
  assert is_rate_limit_error(err) == False
}

pub fn is_overloaded_error_true_test() {
  let err = overloaded_error("test")
  assert is_overloaded_error(err) == True
}

pub fn is_overloaded_error_false_test() {
  let err = rate_limit_error("test")
  assert is_overloaded_error(err) == False
}

pub fn get_status_code_api_error_test() {
  let err = authentication_error("test")
  assert get_status_code(err) == Some(401)
}

pub fn get_status_code_http_error_test() {
  let err = http_error("test")
  assert get_status_code(err) == None
}

// =============================================================================
// Error JSON Tests
// =============================================================================

pub fn error_to_json_api_error_test() {
  let err = authentication_error("Invalid key")
  let result = error_to_json(err) |> json.to_string
  assert string.contains(result, "\"category\":\"api\"")
  assert string.contains(result, "\"status_code\":401")
  assert string.contains(result, "\"error\":")
}

pub fn error_to_json_http_error_test() {
  let err = http_error("Connection failed")
  let result = error_to_json(err) |> json.to_string
  assert string.contains(result, "\"category\":\"http\"")
  assert string.contains(result, "\"reason\":\"Connection failed\"")
}

pub fn error_to_json_json_error_test() {
  let err = json_error("Parse error")
  let result = error_to_json(err) |> json.to_string
  assert string.contains(result, "\"category\":\"json\"")
}

pub fn error_to_json_config_error_test() {
  let err = config_error("Missing config")
  let result = error_to_json(err) |> json.to_string
  assert string.contains(result, "\"category\":\"config\"")
}

pub fn error_to_json_timeout_error_test() {
  let err = timeout_error(5000)
  let result = error_to_json(err) |> json.to_string
  assert string.contains(result, "\"category\":\"timeout\"")
  assert string.contains(result, "\"timeout_ms\":5000")
}

pub fn error_to_json_network_error_test() {
  let err = network_error("DNS failed")
  let result = error_to_json(err) |> json.to_string
  assert string.contains(result, "\"category\":\"network\"")
}

pub fn error_to_json_string_test() {
  let err = rate_limit_error("Too many requests")
  let result = error_to_json_string(err)
  assert string.is_empty(result) == False
  assert string.contains(result, "rate_limit_error")
}
