//// WebSocket Support
////
//// This module provides WebSocket support for real-time updates during
//// modal logic analysis. It enables streaming of progress events including
//// translation, formalization, validation, and completion.
////
//// ## Events
////
//// - `analysis.started` - Analysis has begun
//// - `translation.progress` - Translation update
//// - `formalization.complete` - Formalization finished
//// - `validation.progress` - Validation update
//// - `validation.complete` - Validation finished
//// - `repair.suggestion` - New repair suggestion
//// - `analysis.complete` - Full analysis complete
//// - `error` - Error occurred
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/websocket
////
//// let handler = websocket.create_handler(config)
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// WebSocket configuration
pub type WebSocketConfig {
  WebSocketConfig(
    /// Path for WebSocket connections
    path: String,
    /// Ping interval in milliseconds
    ping_interval_ms: Int,
    /// Connection timeout in milliseconds
    connection_timeout_ms: Int,
    /// Maximum message size in bytes
    max_message_size: Int,
    /// Enable compression
    enable_compression: Bool,
  )
}

/// WebSocket connection
pub type Connection {
  Connection(
    /// Connection ID
    id: String,
    /// Client address
    address: String,
    /// Connection state
    state: ConnectionState,
    /// Subscribed topics
    subscriptions: List(String),
    /// Connection metadata
    metadata: List(#(String, String)),
  )
}

/// Connection state
pub type ConnectionState {
  Connecting
  Connected
  Closing
  Closed
}

/// WebSocket message
pub type Message {
  /// Text message
  TextMessage(content: String)
  /// Binary message
  BinaryMessage(data: List(Int))
  /// Ping message
  PingMessage
  /// Pong message
  PongMessage
  /// Close message
  CloseMessage(code: Int, reason: String)
}

/// Event types for analysis progress
pub type EventType {
  AnalysisStarted
  TranslationProgress
  FormalizationComplete
  ValidationProgress
  ValidationComplete
  RepairSuggestion
  AnalysisComplete
  ErrorEvent
}

/// Progress event
pub type ProgressEvent {
  ProgressEvent(
    /// Event type
    event_type: EventType,
    /// Request ID
    request_id: String,
    /// Timestamp (ISO 8601)
    timestamp: String,
    /// Progress percentage (0-100)
    progress: Int,
    /// Event message
    message: String,
    /// Additional data
    data: Option(String),
  )
}

/// Subscription request
pub type SubscriptionRequest {
  SubscriptionRequest(
    /// Action (subscribe/unsubscribe)
    action: SubscriptionAction,
    /// Topic to subscribe to
    topic: String,
    /// Request ID to track
    request_id: Option(String),
  )
}

/// Subscription action
pub type SubscriptionAction {
  Subscribe
  Unsubscribe
}

/// Client message (incoming)
pub type ClientMessage {
  /// Subscribe to analysis updates
  SubscribeMessage(request_id: String)
  /// Unsubscribe from updates
  UnsubscribeMessage(request_id: String)
  /// Start new analysis
  StartAnalysisMessage(text: String, options: Option(String))
  /// Ping message
  ClientPing
  /// Unknown message
  UnknownMessage(content: String)
}

/// Server message (outgoing)
pub type ServerMessage {
  /// Progress update
  ProgressMessage(event: ProgressEvent)
  /// Subscription confirmed
  SubscribedMessage(request_id: String)
  /// Unsubscribed confirmed
  UnsubscribedMessage(request_id: String)
  /// Error message
  ErrorMessage(code: String, message: String)
  /// Pong response
  ServerPong
}

/// WebSocket handler
pub type Handler {
  Handler(
    /// Configuration
    config: WebSocketConfig,
    /// Active connections
    connections: List(Connection),
    /// Event buffer
    event_buffer: List(ProgressEvent),
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default WebSocket configuration
pub fn default_config() -> WebSocketConfig {
  WebSocketConfig(
    path: "/ws",
    ping_interval_ms: 30_000,
    connection_timeout_ms: 60_000,
    max_message_size: 65_536,
    enable_compression: True,
  )
}

/// Create low-latency configuration
pub fn low_latency_config() -> WebSocketConfig {
  WebSocketConfig(
    path: "/ws",
    ping_interval_ms: 10_000,
    connection_timeout_ms: 30_000,
    max_message_size: 32_768,
    enable_compression: False,
  )
}

/// Set WebSocket path
pub fn with_path(config: WebSocketConfig, path: String) -> WebSocketConfig {
  WebSocketConfig(..config, path: path)
}

/// Set ping interval
pub fn with_ping_interval(
  config: WebSocketConfig,
  interval_ms: Int,
) -> WebSocketConfig {
  WebSocketConfig(..config, ping_interval_ms: interval_ms)
}

// =============================================================================
// Handler
// =============================================================================

/// Create a new WebSocket handler
pub fn create_handler(config: WebSocketConfig) -> Handler {
  Handler(config: config, connections: [], event_buffer: [])
}

/// Add a connection
pub fn add_connection(handler: Handler, connection: Connection) -> Handler {
  Handler(..handler, connections: [connection, ..handler.connections])
}

/// Remove a connection
pub fn remove_connection(handler: Handler, connection_id: String) -> Handler {
  Handler(
    ..handler,
    connections: list.filter(handler.connections, fn(c) {
      c.id != connection_id
    }),
  )
}

/// Get connection by ID
pub fn get_connection(handler: Handler, id: String) -> Option(Connection) {
  list.find(handler.connections, fn(c) { c.id == id })
  |> option.from_result
}

/// Get all connections
pub fn get_connections(handler: Handler) -> List(Connection) {
  handler.connections
}

/// Get connection count
pub fn connection_count(handler: Handler) -> Int {
  list.length(handler.connections)
}

// =============================================================================
// Message Handling
// =============================================================================

/// Parse a client message from JSON
pub fn parse_client_message(json: String) -> ClientMessage {
  case extract_message_type(json) {
    "subscribe" -> {
      let request_id = extract_json_field(json, "request_id")
      SubscribeMessage(request_id)
    }
    "unsubscribe" -> {
      let request_id = extract_json_field(json, "request_id")
      UnsubscribeMessage(request_id)
    }
    "analyze" -> {
      let text = extract_json_field(json, "text")
      let options = case string.contains(json, "\"options\"") {
        True -> Some(extract_json_field(json, "options"))
        False -> None
      }
      StartAnalysisMessage(text, options)
    }
    "ping" -> ClientPing
    _ -> UnknownMessage(json)
  }
}

/// Serialize a server message to JSON
pub fn serialize_server_message(message: ServerMessage) -> String {
  case message {
    ProgressMessage(event) -> serialize_progress_event(event)
    SubscribedMessage(request_id) ->
      "{\n"
      <> "  \"type\": \"subscribed\",\n"
      <> "  \"request_id\": \""
      <> request_id
      <> "\"\n"
      <> "}"
    UnsubscribedMessage(request_id) ->
      "{\n"
      <> "  \"type\": \"unsubscribed\",\n"
      <> "  \"request_id\": \""
      <> request_id
      <> "\"\n"
      <> "}"
    ErrorMessage(code, msg) ->
      "{\n"
      <> "  \"type\": \"error\",\n"
      <> "  \"code\": \""
      <> code
      <> "\",\n"
      <> "  \"message\": \""
      <> escape_json(msg)
      <> "\"\n"
      <> "}"
    ServerPong -> "{\"type\": \"pong\"}"
  }
}

/// Handle a client message
pub fn handle_message(
  handler: Handler,
  connection_id: String,
  message: ClientMessage,
) -> #(Handler, List(ServerMessage)) {
  case message {
    SubscribeMessage(request_id) -> {
      let updated =
        update_connection_subscriptions(
          handler,
          connection_id,
          request_id,
          True,
        )
      #(updated, [SubscribedMessage(request_id)])
    }
    UnsubscribeMessage(request_id) -> {
      let updated =
        update_connection_subscriptions(
          handler,
          connection_id,
          request_id,
          False,
        )
      #(updated, [UnsubscribedMessage(request_id)])
    }
    StartAnalysisMessage(text, _options) -> {
      let request_id = generate_request_id()
      let event =
        ProgressEvent(
          event_type: AnalysisStarted,
          request_id: request_id,
          timestamp: get_timestamp(),
          progress: 0,
          message: "Analysis started for: " <> truncate(text, 50),
          data: None,
        )
      let updated = add_event(handler, event)
      #(updated, [ProgressMessage(event)])
    }
    ClientPing -> #(handler, [ServerPong])
    UnknownMessage(_) -> #(handler, [
      ErrorMessage("unknown_message", "Unknown message type"),
    ])
  }
}

fn update_connection_subscriptions(
  handler: Handler,
  connection_id: String,
  topic: String,
  add: Bool,
) -> Handler {
  let updated_connections =
    list.map(handler.connections, fn(c) {
      case c.id == connection_id {
        True ->
          case add {
            True -> Connection(..c, subscriptions: [topic, ..c.subscriptions])
            False ->
              Connection(
                ..c,
                subscriptions: list.filter(c.subscriptions, fn(s) { s != topic }),
              )
          }
        False -> c
      }
    })
  Handler(..handler, connections: updated_connections)
}

// =============================================================================
// Event Broadcasting
// =============================================================================

/// Broadcast an event to subscribed connections
pub fn broadcast_event(
  handler: Handler,
  event: ProgressEvent,
) -> #(Handler, List(#(String, ServerMessage))) {
  let messages =
    handler.connections
    |> list.filter(fn(c) { list.contains(c.subscriptions, event.request_id) })
    |> list.map(fn(c) { #(c.id, ProgressMessage(event)) })

  let updated = add_event(handler, event)
  #(updated, messages)
}

/// Add an event to the buffer
fn add_event(handler: Handler, event: ProgressEvent) -> Handler {
  // Keep last 100 events
  let buffer = [event, ..handler.event_buffer]
  let trimmed = list.take(buffer, 100)
  Handler(..handler, event_buffer: trimmed)
}

/// Get recent events for a request
pub fn get_events_for_request(
  handler: Handler,
  request_id: String,
) -> List(ProgressEvent) {
  list.filter(handler.event_buffer, fn(e) { e.request_id == request_id })
}

// =============================================================================
// Progress Event Creation
// =============================================================================

/// Create an analysis started event
pub fn analysis_started(request_id: String, text: String) -> ProgressEvent {
  ProgressEvent(
    event_type: AnalysisStarted,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: 0,
    message: "Analysis started",
    data: Some(
      "{\"text_length\": " <> int_to_string(string.length(text)) <> "}",
    ),
  )
}

/// Create a translation progress event
pub fn translation_progress(
  request_id: String,
  progress: Int,
  message: String,
) -> ProgressEvent {
  ProgressEvent(
    event_type: TranslationProgress,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: progress,
    message: message,
    data: None,
  )
}

/// Create a formalization complete event
pub fn formalization_complete(
  request_id: String,
  premises: List(String),
  conclusion: String,
) -> ProgressEvent {
  let premise_count = list.length(premises)
  ProgressEvent(
    event_type: FormalizationComplete,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: 50,
    message: "Formalization complete with "
      <> int_to_string(premise_count)
      <> " premises",
    data: Some(
      "{\"premises\": "
      <> int_to_string(premise_count)
      <> ", \"conclusion\": \""
      <> escape_json(conclusion)
      <> "\"}",
    ),
  )
}

/// Create a validation progress event
pub fn validation_progress(
  request_id: String,
  progress: Int,
  worlds_explored: Int,
) -> ProgressEvent {
  ProgressEvent(
    event_type: ValidationProgress,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: 50 + progress / 2,
    message: "Validating... "
      <> int_to_string(worlds_explored)
      <> " worlds explored",
    data: Some(
      "{\"worlds_explored\": " <> int_to_string(worlds_explored) <> "}",
    ),
  )
}

/// Create a validation complete event
pub fn validation_complete(
  request_id: String,
  is_valid: Bool,
  message: String,
) -> ProgressEvent {
  ProgressEvent(
    event_type: ValidationComplete,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: 90,
    message: message,
    data: Some("{\"is_valid\": " <> bool_to_string(is_valid) <> "}"),
  )
}

/// Create a repair suggestion event
pub fn repair_suggestion(
  request_id: String,
  repair_type: String,
  description: String,
  confidence: Float,
) -> ProgressEvent {
  ProgressEvent(
    event_type: RepairSuggestion,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: 95,
    message: "Repair suggestion: " <> description,
    data: Some(
      "{\"type\": \""
      <> repair_type
      <> "\", \"confidence\": "
      <> float_to_string(confidence)
      <> "}",
    ),
  )
}

/// Create an analysis complete event
pub fn analysis_complete(
  request_id: String,
  is_valid: Bool,
  summary: String,
) -> ProgressEvent {
  ProgressEvent(
    event_type: AnalysisComplete,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: 100,
    message: summary,
    data: Some("{\"is_valid\": " <> bool_to_string(is_valid) <> "}"),
  )
}

/// Create an error event
pub fn error_event(
  request_id: String,
  error_code: String,
  error_message: String,
) -> ProgressEvent {
  ProgressEvent(
    event_type: ErrorEvent,
    request_id: request_id,
    timestamp: get_timestamp(),
    progress: 0,
    message: error_message,
    data: Some("{\"code\": \"" <> error_code <> "\"}"),
  )
}

// =============================================================================
// Serialization
// =============================================================================

fn serialize_progress_event(event: ProgressEvent) -> String {
  "{\n"
  <> "  \"type\": \"progress\",\n"
  <> "  \"event_type\": \""
  <> event_type_to_string(event.event_type)
  <> "\",\n"
  <> "  \"request_id\": \""
  <> event.request_id
  <> "\",\n"
  <> "  \"timestamp\": \""
  <> event.timestamp
  <> "\",\n"
  <> "  \"progress\": "
  <> int_to_string(event.progress)
  <> ",\n"
  <> "  \"message\": \""
  <> escape_json(event.message)
  <> "\""
  <> case event.data {
    Some(data) -> ",\n  \"data\": " <> data
    None -> ""
  }
  <> "\n}"
}

fn event_type_to_string(event_type: EventType) -> String {
  case event_type {
    AnalysisStarted -> "analysis.started"
    TranslationProgress -> "translation.progress"
    FormalizationComplete -> "formalization.complete"
    ValidationProgress -> "validation.progress"
    ValidationComplete -> "validation.complete"
    RepairSuggestion -> "repair.suggestion"
    AnalysisComplete -> "analysis.complete"
    ErrorEvent -> "error"
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn extract_message_type(json: String) -> String {
  extract_json_field(json, "type")
}

fn extract_json_field(json: String, field: String) -> String {
  let search = "\"" <> field <> "\":"
  case string.split(json, search) {
    [_, rest, ..] -> {
      let trimmed = string.trim_start(rest)
      case string.starts_with(trimmed, "\"") {
        True -> {
          let without_quote = string.drop_start(trimmed, 1)
          case string.split(without_quote, "\"") {
            [value, ..] -> value
            _ -> ""
          }
        }
        False -> ""
      }
    }
    _ -> ""
  }
}

fn escape_json(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}

fn truncate(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len) <> "..."
    False -> s
  }
}

fn generate_request_id() -> String {
  "req-12345678"
}

fn get_timestamp() -> String {
  "2025-01-01T00:00:00Z"
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_string(-n, "")
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}

fn float_to_string(f: Float) -> String {
  let int_part = float_to_int(f)
  let frac_part = float_to_int({ f -. int_to_float(int_part) } *. 100.0)
  int_to_string(int_part)
  <> "."
  <> pad_left(int_to_string(abs_int(frac_part)), 2)
}

fn pad_left(s: String, len: Int) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left("0" <> s, len)
  }
}

fn abs_int(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
