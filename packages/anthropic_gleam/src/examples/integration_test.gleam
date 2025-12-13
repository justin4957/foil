//// Comprehensive Integration test for Anthropic API
////
//// This script tests all current functionality with real API interactions
//// Run with: gleam run -m examples/integration_test

import anthropic/api
import anthropic/client
import anthropic/config
import anthropic/streaming/accumulator
import anthropic/streaming/handler
import anthropic/types/error
import anthropic/types/message
import anthropic/types/request
import anthropic/types/streaming
import gleam/erlang/charlist
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// Main Entry Point
// =============================================================================

/// Main entry point
pub fn main() {
  io.println("=============================================================")
  io.println("  ANTHROPIC GLEAM - COMPREHENSIVE INTEGRATION TEST")
  io.println("=============================================================")
  io.println("")

  // Get API key
  case get_api_key() {
    Error(Nil) -> {
      io.println("ERROR: ANTHROPIC_API_KEY environment variable not set")
      io.println("Please set it and try again:")
      io.println("  export ANTHROPIC_API_KEY=your-api-key")
    }
    Ok(api_key) -> {
      io.println(
        "API Key: [REDACTED - " <> string.slice(api_key, 0, 8) <> "...]",
      )
      io.println("")
      run_all_tests(api_key)
    }
  }
}

fn run_all_tests(api_key: String) {
  let results = [
    // Non-streaming tests
    run_test("1. Simple Text Message", fn() { test_simple_message(api_key) }),
    run_test("2. Multi-turn Conversation", fn() { test_conversation(api_key) }),
    run_test("3. System Prompt", fn() { test_system_prompt(api_key) }),
    run_test("4. Temperature Parameter", fn() { test_temperature(api_key) }),
    run_test("5. Max Tokens Limit", fn() { test_max_tokens(api_key) }),
    run_test("6. Stop Sequences", fn() { test_stop_sequences(api_key) }),
    run_test("7. Error Handling (Invalid Model)", fn() {
      test_error_handling(api_key)
    }),
    // Streaming tests
    run_test("8. Streaming Message", fn() { test_streaming_message(api_key) }),
    run_test("9. Streaming with Accumulator", fn() {
      test_streaming_accumulator(api_key)
    }),
    run_test("10. Streaming Event Analysis", fn() {
      test_streaming_event_analysis(api_key)
    }),
  ]

  // Print summary
  io.println("")
  io.println("=============================================================")
  io.println("  TEST SUMMARY")
  io.println("=============================================================")

  let passed = list.filter(results, fn(r) { r.1 })
  let failed = list.filter(results, fn(r) { !r.1 })

  io.println("")
  io.println(
    "  PASSED: "
    <> int.to_string(list.length(passed))
    <> " / "
    <> int.to_string(list.length(results)),
  )
  io.println("  FAILED: " <> int.to_string(list.length(failed)))
  io.println("")

  case list.length(failed) > 0 {
    True -> {
      io.println("  Failed tests:")
      list.each(failed, fn(f) { io.println("    - " <> f.0) })
    }
    False -> {
      io.println("  All tests passed!")
    }
  }

  io.println("")
  io.println("=============================================================")
}

fn run_test(name: String, test_fn: fn() -> Bool) -> #(String, Bool) {
  io.println("")
  io.println("-------------------------------------------------------------")
  io.println("  TEST: " <> name)
  io.println("-------------------------------------------------------------")
  let result = test_fn()
  case result {
    True -> io.println("  RESULT: PASSED")
    False -> io.println("  RESULT: FAILED")
  }
  #(name, result)
}

// =============================================================================
// Non-Streaming Tests
// =============================================================================

fn test_simple_message(api_key: String) -> Bool {
  io.println("  Creating client and request...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [message.user_message("Say 'Hello from Gleam!' and nothing else.")],
          100,
        )

      io.println("")
      io.println("  REQUEST:")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Message: Say 'Hello from Gleam!' and nothing else.")
      io.println("    Max tokens: 100")
      io.println("")

      case api.create_message(api_client, req) {
        Ok(response) -> {
          io.println("  RESPONSE:")
          io.println("    ID: " <> response.id)
          io.println("    Model: " <> response.model)
          io.println("    Role: " <> message.role_to_string(response.role))
          io.println(
            "    Stop reason: " <> stop_reason_to_string(response.stop_reason),
          )
          io.println(
            "    Input tokens: " <> int.to_string(response.usage.input_tokens),
          )
          io.println(
            "    Output tokens: " <> int.to_string(response.usage.output_tokens),
          )
          io.println("    Content: " <> request.response_text(response))
          True
        }
        Error(err) -> {
          io.println("  ERROR: " <> error.error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_conversation(api_key: String) -> Bool {
  io.println("  Testing multi-turn conversation...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [
            message.user_message("My name is Alice."),
            message.assistant_message("Nice to meet you, Alice!"),
            message.user_message(
              "What is my name? Reply with just the name, nothing else.",
            ),
          ],
          50,
        )

      io.println("")
      io.println("  REQUEST:")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Messages:")
      io.println("      [user]: My name is Alice.")
      io.println("      [assistant]: Nice to meet you, Alice!")
      io.println(
        "      [user]: What is my name? Reply with just the name, nothing else.",
      )
      io.println("")

      case api.create_message(api_client, req) {
        Ok(response) -> {
          let content = request.response_text(response)
          io.println("  RESPONSE:")
          io.println("    ID: " <> response.id)
          io.println("    Content: " <> content)
          io.println(
            "    Input tokens: " <> int.to_string(response.usage.input_tokens),
          )
          io.println(
            "    Output tokens: " <> int.to_string(response.usage.output_tokens),
          )
          // Verify the response mentions Alice
          string.contains(string.lowercase(content), "alice")
        }
        Error(err) -> {
          io.println("  ERROR: " <> error.error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_system_prompt(api_key: String) -> Bool {
  io.println("  Testing system prompt...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [message.user_message("What do you do?")],
          150,
        )
        |> request.with_system(
          "You are a pirate. Always respond in pirate speak. Use words like 'Arrr', 'matey', 'ye', 'landlubber'.",
        )

      io.println("")
      io.println("  REQUEST:")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println(
        "    System: You are a pirate. Always respond in pirate speak...",
      )
      io.println("    Message: What do you do?")
      io.println("")

      case api.create_message(api_client, req) {
        Ok(response) -> {
          let content = request.response_text(response)
          io.println("  RESPONSE:")
          io.println("    ID: " <> response.id)
          io.println("    Content: " <> content)
          io.println(
            "    Input tokens: " <> int.to_string(response.usage.input_tokens),
          )
          io.println(
            "    Output tokens: " <> int.to_string(response.usage.output_tokens),
          )
          // Check for pirate words
          let lower_content = string.lowercase(content)
          let has_pirate_words =
            string.contains(lower_content, "arr")
            || string.contains(lower_content, "matey")
            || string.contains(lower_content, "ye")
            || string.contains(lower_content, "ahoy")
            || string.contains(lower_content, "ship")
            || string.contains(lower_content, "sea")
          has_pirate_words
        }
        Error(err) -> {
          io.println("  ERROR: " <> error.error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_temperature(api_key: String) -> Bool {
  io.println("  Testing temperature parameter...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      // Low temperature for deterministic response
      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [message.user_message("What is 2 + 2? Reply with just the number.")],
          10,
        )
        |> request.with_temperature(0.0)

      io.println("")
      io.println("  REQUEST:")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Temperature: 0.0 (deterministic)")
      io.println("    Message: What is 2 + 2? Reply with just the number.")
      io.println("")

      case api.create_message(api_client, req) {
        Ok(response) -> {
          let content = request.response_text(response)
          io.println("  RESPONSE:")
          io.println("    ID: " <> response.id)
          io.println("    Content: " <> content)
          io.println(
            "    Input tokens: " <> int.to_string(response.usage.input_tokens),
          )
          io.println(
            "    Output tokens: " <> int.to_string(response.usage.output_tokens),
          )
          // Should contain "4"
          string.contains(content, "4")
        }
        Error(err) -> {
          io.println("  ERROR: " <> error.error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_max_tokens(api_key: String) -> Bool {
  io.println("  Testing max tokens limit...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      // Very low max tokens to force truncation
      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [
            message.user_message(
              "Write a very long story about a dragon. Make it at least 500 words.",
            ),
          ],
          15,
        )

      io.println("")
      io.println("  REQUEST:")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Max tokens: 15 (very short)")
      io.println("    Message: Write a very long story about a dragon...")
      io.println("")

      case api.create_message(api_client, req) {
        Ok(response) -> {
          let content = request.response_text(response)
          io.println("  RESPONSE:")
          io.println("    ID: " <> response.id)
          io.println(
            "    Stop reason: " <> stop_reason_to_string(response.stop_reason),
          )
          io.println(
            "    Output tokens: " <> int.to_string(response.usage.output_tokens),
          )
          io.println("    Content: " <> content)
          // Should stop due to max_tokens
          response.stop_reason == Some(request.MaxTokens)
        }
        Error(err) -> {
          io.println("  ERROR: " <> error.error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_stop_sequences(api_key: String) -> Bool {
  io.println("  Testing stop sequences...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [
            message.user_message(
              "Count from 1 to 10, separating numbers with commas: 1, 2, 3, ...",
            ),
          ],
          100,
        )
        |> request.with_stop_sequences(["5"])

      io.println("")
      io.println("  REQUEST:")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Stop sequences: [\"5\"]")
      io.println(
        "    Message: Count from 1 to 10, separating numbers with commas...",
      )
      io.println("")

      case api.create_message(api_client, req) {
        Ok(response) -> {
          let content = request.response_text(response)
          io.println("  RESPONSE:")
          io.println("    ID: " <> response.id)
          io.println(
            "    Stop reason: " <> stop_reason_to_string(response.stop_reason),
          )
          io.println(
            "    Stop sequence: "
            <> option.unwrap(response.stop_sequence, "none"),
          )
          io.println("    Content: " <> content)
          // Should stop due to stop_sequence
          response.stop_reason == Some(request.StopSequence)
        }
        Error(err) -> {
          io.println("  ERROR: " <> error.error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_error_handling(api_key: String) -> Bool {
  io.println("  Testing error handling with invalid model...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "invalid-model-name",
          [message.user_message("Hello")],
          100,
        )

      io.println("")
      io.println("  REQUEST:")
      io.println("    Model: invalid-model-name (intentionally wrong)")
      io.println("    Message: Hello")
      io.println("")
      io.println("  Expecting an error...")
      io.println("")

      case api.create_message(api_client, req) {
        Ok(_response) -> {
          io.println("  UNEXPECTED SUCCESS (expected error)")
          False
        }
        Error(err) -> {
          io.println("  EXPECTED ERROR RECEIVED:")
          io.println("    Error: " <> error.error_to_string(err))
          io.println("    Category: " <> error.error_category(err))
          io.println(
            "    Retryable: " <> bool_to_string(error.is_retryable(err)),
          )
          True
        }
      }
    }
  }
}

// =============================================================================
// Streaming Tests
// =============================================================================

fn test_streaming_message(api_key: String) -> Bool {
  io.println("  Testing streaming message...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [
            message.user_message(
              "Count from 1 to 5, one number per line. Nothing else.",
            ),
          ],
          100,
        )

      io.println("")
      io.println("  REQUEST (Streaming):")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Message: Count from 1 to 5, one number per line.")
      io.println("")

      case handler.stream_message(api_client, req) {
        Ok(result) -> {
          let event_count = list.length(result.events)
          let full_text = handler.get_full_text(result.events)
          let message_id = handler.get_message_id(result.events)
          let model = handler.get_model(result.events)
          let is_complete = handler.is_complete(result.events)

          io.println("  STREAM RESPONSE:")
          io.println("    Events received: " <> int.to_string(event_count))
          io.println(
            "    Message ID: " <> option.unwrap(result_to_option(message_id), "N/A"),
          )
          io.println(
            "    Model: " <> option.unwrap(result_to_option(model), "N/A"),
          )
          io.println("    Is complete: " <> bool_to_string(is_complete))
          io.println("    Full text:")
          io.println("      " <> string.replace(full_text, "\n", "\n      "))

          // Verify we got events and the stream completed
          event_count > 0 && is_complete && string.length(full_text) > 0
        }
        Error(err) -> {
          io.println("  STREAM ERROR: " <> stream_error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_streaming_accumulator(api_key: String) -> Bool {
  io.println("  Testing streaming with accumulator...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [
            message.user_message(
              "What is the capital of France? Reply with just the city name.",
            ),
          ],
          50,
        )

      io.println("")
      io.println("  REQUEST (Streaming with Accumulator):")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Message: What is the capital of France?")
      io.println("")

      case handler.stream_message(api_client, req) {
        Ok(result) -> {
          // Use accumulator to build complete response
          case accumulator.accumulate(result.events) {
            Ok(response) -> {
              let content = request.response_text(response)
              io.println("  ACCUMULATED RESPONSE:")
              io.println("    ID: " <> response.id)
              io.println("    Model: " <> response.model)
              io.println("    Role: " <> message.role_to_string(response.role))
              io.println(
                "    Stop reason: "
                <> stop_reason_to_string(response.stop_reason),
              )
              io.println(
                "    Input tokens: "
                <> int.to_string(response.usage.input_tokens),
              )
              io.println(
                "    Output tokens: "
                <> int.to_string(response.usage.output_tokens),
              )
              io.println("    Content: " <> content)

              // Verify response mentions Paris
              string.contains(string.lowercase(content), "paris")
            }
            Error(err) -> {
              io.println("  ACCUMULATOR ERROR: " <> err)
              False
            }
          }
        }
        Error(err) -> {
          io.println("  STREAM ERROR: " <> stream_error_to_string(err))
          False
        }
      }
    }
  }
}

fn test_streaming_event_analysis(api_key: String) -> Bool {
  io.println("  Testing streaming event analysis...")

  let config_result =
    config.config_options()
    |> config.with_api_key(api_key)
    |> config.load_config()

  case config_result {
    Error(err) -> {
      io.println("  Config error: " <> error.error_to_string(err))
      False
    }
    Ok(cfg) -> {
      let api_client = client.new(cfg)

      let req =
        request.create_request(
          "claude-3-5-haiku-20241022",
          [message.user_message("Say 'OK' and nothing else.")],
          20,
        )

      io.println("")
      io.println("  REQUEST (Event Analysis):")
      io.println("    Model: claude-3-5-haiku-20241022")
      io.println("    Message: Say 'OK' and nothing else.")
      io.println("")

      case handler.stream_message(api_client, req) {
        Ok(result) -> {
          io.println("  EVENT ANALYSIS:")

          // Count each event type
          let event_counts = count_event_types(result.events)
          io.println("    Event types received:")
          list.each(event_counts, fn(pair) {
            io.println(
              "      - " <> pair.0 <> ": " <> int.to_string(pair.1),
            )
          })

          // Get text deltas
          let deltas = handler.get_text_deltas(result.events)
          io.println("")
          io.println(
            "    Text deltas count: " <> int.to_string(list.length(deltas)),
          )
          io.println("    Text deltas: " <> string.inspect(deltas))

          // Check for expected event types
          let has_message_start = has_event_type(result.events, "message_start")
          let has_content_block_start =
            has_event_type(result.events, "content_block_start")
          let has_content_block_delta =
            has_event_type(result.events, "content_block_delta")
          let has_message_stop = has_event_type(result.events, "message_stop")

          io.println("")
          io.println("    Has message_start: " <> bool_to_string(has_message_start))
          io.println(
            "    Has content_block_start: "
            <> bool_to_string(has_content_block_start),
          )
          io.println(
            "    Has content_block_delta: "
            <> bool_to_string(has_content_block_delta),
          )
          io.println("    Has message_stop: " <> bool_to_string(has_message_stop))

          // All essential event types should be present
          has_message_start
          && has_content_block_start
          && has_content_block_delta
          && has_message_stop
        }
        Error(err) -> {
          io.println("  STREAM ERROR: " <> stream_error_to_string(err))
          False
        }
      }
    }
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn stop_reason_to_string(reason: Option(request.StopReason)) -> String {
  case reason {
    None -> "none"
    Some(r) -> request.stop_reason_to_string(r)
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn result_to_option(result: Result(a, b)) -> Option(a) {
  case result {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

fn stream_error_to_string(err: handler.StreamError) -> String {
  case err {
    handler.HttpError(e) -> "HTTP Error: " <> error.error_to_string(e)
    handler.SseParseError(msg) -> "SSE Parse Error: " <> msg
    handler.EventDecodeError(msg) -> "Event Decode Error: " <> msg
    handler.ApiError(status, body) ->
      "API Error (status "
      <> int.to_string(status)
      <> "): "
      <> string.slice(body, 0, 200)
  }
}

fn count_event_types(
  events: List(streaming.StreamEvent),
) -> List(#(String, Int)) {
  let types =
    list.map(events, fn(event) { streaming.event_type_string(event) })

  let unique_types = list.unique(types)

  list.map(unique_types, fn(event_type) {
    let count =
      list.filter(types, fn(t) { t == event_type })
      |> list.length
    #(event_type, count)
  })
}

fn has_event_type(events: List(streaming.StreamEvent), event_type: String) -> Bool {
  list.any(events, fn(event) {
    streaming.event_type_string(event) == event_type
  })
}

/// Get environment variable using charlist conversion
@external(erlang, "os", "getenv")
fn ffi_getenv(
  name: charlist.Charlist,
  default: charlist.Charlist,
) -> charlist.Charlist

fn get_api_key() -> Result(String, Nil) {
  let value =
    ffi_getenv(
      charlist.from_string("ANTHROPIC_API_KEY"),
      charlist.from_string(""),
    )
    |> charlist.to_string
  case value {
    "" -> Error(Nil)
    v -> Ok(v)
  }
}
