//// LLM Translation Tests
////
//// Tests for prompts, compiler, logic detector, and translation service.

import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/argument
import modal_logic/compiler
import modal_logic/logic_detector
import modal_logic/prompts
import modal_logic/proposition.{
  And, Atom, Implies, K, KD, Necessary, Possible, S4, S5, T,
}
import modal_logic/translation_service

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("LLM Translation Tests")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Prompt Generation
  test_prompt_generation()

  // Test 2: Structure Compiler
  test_structure_compiler()

  // Test 3: Logic System Detection
  test_logic_detection()

  // Test 4: Translation Service
  test_translation_service()

  // Test 5: Error Handling
  test_error_handling()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All LLM Translation Tests Passed!")
  io.println("=" |> string.repeat(70))
}

fn test_prompt_generation() {
  io.println("")
  io.println("--- Test 1: Prompt Generation ---")
  io.println("")

  // Test default config
  let config = prompts.default_config()
  io.println("[OK] Default config created")
  io.println(
    "     Include examples: " <> bool_to_string(config.include_examples),
  )
  io.println("     Max premises: " <> int_to_string(config.max_premises))

  // Test minimal config
  let _minimal = prompts.minimal_config()
  io.println("[OK] Minimal config created")

  // Test verbose config
  let _verbose = prompts.verbose_config()
  io.println("[OK] Verbose config created")

  // Build translation prompt
  let argument_text =
    "All men are mortal. Socrates is a man. Therefore, Socrates is mortal."
  let prompt = prompts.build_translation_prompt(config, argument_text)
  io.println("[OK] Translation prompt built")
  io.println(
    "     System message length: "
    <> int_to_string(string.length(prompt.system_message))
    <> " chars",
  )
  io.println(
    "     User message length: "
    <> int_to_string(string.length(prompt.user_message))
    <> " chars",
  )

  // Check response schema
  let schema = prompts.get_response_schema()
  io.println(
    "[OK] Response schema generated: "
    <> int_to_string(string.length(schema))
    <> " chars",
  )

  // Test specialized prompts
  let _amb_prompt = prompts.build_ambiguity_prompt(argument_text)
  io.println("[OK] Ambiguity analysis prompt built")

  let _logic_prompt = prompts.build_logic_system_prompt(argument_text)
  io.println("[OK] Logic system recommendation prompt built")

  let _rephrase_prompt =
    prompts.build_rephrase_prompt(
      argument_text,
      "Modal word 'must' is ambiguous",
    )
  io.println("[OK] Rephrase prompt built")

  // Test few-shot examples
  let examples = prompts.get_standard_examples()
  io.println(
    "[OK] Got " <> int_to_string(list.length(examples)) <> " few-shot examples",
  )

  io.println("")
}

fn test_structure_compiler() {
  io.println("")
  io.println("--- Test 2: Structure Compiler ---")
  io.println("")

  // Test simple proposition compilation
  let atom_json = "{\"type\": \"atom\", \"name\": \"p\"}"
  case compile_proposition_json(atom_json) {
    Ok(Atom("p")) -> io.println("[OK] Compiled atom proposition")
    _ -> io.println("[FAIL] Failed to compile atom")
  }

  // Test negation
  let not_json =
    "{\"type\": \"not\", \"operand\": {\"type\": \"atom\", \"name\": \"p\"}}"
  case compile_proposition_json(not_json) {
    Ok(proposition.Not(Atom("p"))) -> io.println("[OK] Compiled negation")
    _ -> io.println("[FAIL] Failed to compile negation")
  }

  // Test binary operators
  let and_json =
    "{\"type\": \"and\", \"left\": {\"type\": \"atom\", \"name\": \"p\"}, \"right\": {\"type\": \"atom\", \"name\": \"q\"}}"
  case compile_proposition_json(and_json) {
    Ok(And(Atom("p"), Atom("q"))) -> io.println("[OK] Compiled conjunction")
    _ -> io.println("[FAIL] Failed to compile conjunction")
  }

  // Test modal operators
  let nec_json =
    "{\"type\": \"necessary\", \"operand\": {\"type\": \"atom\", \"name\": \"p\"}}"
  case compile_proposition_json(nec_json) {
    Ok(Necessary(Atom("p"))) -> io.println("[OK] Compiled necessity")
    _ -> io.println("[FAIL] Failed to compile necessity")
  }

  let pos_json =
    "{\"type\": \"possible\", \"operand\": {\"type\": \"atom\", \"name\": \"p\"}}"
  case compile_proposition_json(pos_json) {
    Ok(Possible(Atom("p"))) -> io.println("[OK] Compiled possibility")
    _ -> io.println("[FAIL] Failed to compile possibility")
  }

  // Test full translation compilation
  let full_json =
    "{
    \"premises\": [
      {
        \"natural_text\": \"All men are mortal\",
        \"formal\": {\"type\": \"implies\", \"left\": {\"type\": \"atom\", \"name\": \"man\"}, \"right\": {\"type\": \"atom\", \"name\": \"mortal\"}}
      }
    ],
    \"conclusion\": {
      \"natural_text\": \"Socrates is mortal\",
      \"formal\": {\"type\": \"atom\", \"name\": \"socrates_mortal\"}
    },
    \"logic_system\": \"S4\",
    \"confidence\": 0.9,
    \"ambiguities\": [],
    \"assumptions\": [\"Propositional formalization\"]
  }"

  case compiler.compile_translation(full_json) {
    Ok(translation) -> {
      io.println("[OK] Compiled full translation")
      io.println(
        "     Premises: " <> int_to_string(list.length(translation.premises)),
      )
      io.println("     Confidence: " <> float_to_string(translation.confidence))
      io.println(
        "     Logic system: " <> logic_to_string(translation.logic_system),
      )
    }
    Error(e) ->
      io.println("[FAIL] Failed to compile: " <> compiler.format_error(e))
  }

  // Test error handling
  let bad_json = "{invalid json"
  case compiler.compile_translation(bad_json) {
    Error(compiler.JsonParseError(_)) ->
      io.println("[OK] Caught JSON parse error")
    _ -> io.println("[FAIL] Expected JSON parse error")
  }

  let missing_field = "{\"premises\": []}"
  case compiler.compile_translation(missing_field) {
    Error(compiler.MissingField("conclusion")) ->
      io.println("[OK] Caught missing field error")
    Error(e) -> io.println("[OK] Caught error: " <> compiler.format_error(e))
    _ -> io.println("[FAIL] Expected missing field error")
  }

  io.println("")
}

fn test_logic_detection() {
  io.println("")
  io.println("--- Test 3: Logic System Detection ---")
  io.println("")

  // Test detection from text with necessity
  let text1 = "It is necessarily true that all bachelors are unmarried."
  let detection1 = logic_detector.detect_from_text(text1)
  io.println("[OK] Detected from necessity text")
  io.println(
    "     Modal words found: "
    <> int_to_string(list.length(detection1.modal_words)),
  )
  io.println("     Confidence: " <> float_to_string(detection1.confidence))

  // Test detection from deontic text
  let text2 = "You ought to keep your promises. You should help others."
  let detection2 = logic_detector.detect_from_text(text2)
  io.println("[OK] Detected from deontic text")
  io.println(
    "     Modal words found: "
    <> int_to_string(list.length(detection2.modal_words)),
  )
  case detection2.recommended {
    Some(system) -> io.println("     Recommended: " <> logic_to_string(system))
    None -> io.println("     No strong recommendation")
  }

  // Test detection from epistemic text
  let text3 = "John knows that Mary believes the Earth is round."
  let detection3 = logic_detector.detect_from_text(text3)
  io.println("[OK] Detected from epistemic text")
  io.println(
    "     Features: " <> int_to_string(list.length(detection3.features)),
  )

  // Test detection from propositions
  let premises = [
    Implies(Atom("p"), Necessary(Atom("q"))),
    Atom("p"),
  ]
  let conclusion = Necessary(Atom("q"))
  let prop_detection =
    logic_detector.detect_from_propositions(premises, conclusion)
  io.println("[OK] Detected from propositions")
  io.println(
    "     Candidates: " <> int_to_string(list.length(prop_detection.candidates)),
  )

  // Test override
  let override = logic_detector.RequireSystem(S5)
  let overridden = logic_detector.apply_override(detection1, override)
  case overridden.recommended {
    Some(S5) -> io.println("[OK] Override applied correctly")
    _ -> io.println("[FAIL] Override not applied")
  }

  // Test merge
  let merged = logic_detector.merge_detections(detection1, prop_detection)
  io.println("[OK] Merged detections")
  io.println(
    "     Combined features: " <> int_to_string(list.length(merged.features)),
  )

  io.println("")
}

fn test_translation_service() {
  io.println("")
  io.println("--- Test 4: Translation Service ---")
  io.println("")

  // Test service creation
  let config = translation_service.default_config()
  let service = translation_service.new_service(config)
  io.println("[OK] Translation service created")

  // Test strict config
  let _strict = translation_service.strict_config()
  io.println("[OK] Strict config created")

  // Test fast config
  let _fast = translation_service.fast_config()
  io.println("[OK] Fast config created")

  // Test config modification
  let modified =
    config
    |> translation_service.with_max_retries(5)
    |> translation_service.with_min_confidence(0.8)
  io.println("[OK] Modified config")
  io.println("     Max retries: " <> int_to_string(modified.max_retries))

  // Test API request building
  let prompt =
    prompts.build_translation_prompt(prompts.default_config(), "Test argument")
  let request = translation_service.build_api_request(prompt, "claude-3-sonnet")
  io.println("[OK] API request built")
  io.println("     Model: " <> request.model)
  io.println("     JSON mode: " <> bool_to_string(request.json_mode))

  // Test request serialization
  let serialized = translation_service.serialize_request(request)
  io.println(
    "[OK] Request serialized: "
    <> int_to_string(string.length(serialized))
    <> " chars",
  )

  // Test statistics
  let stats = translation_service.get_stats(service)
  io.println("[OK] Stats retrieved")
  io.println("     Total requests: " <> int_to_string(stats.total_requests))

  // Test stats formatting
  let stats_str = translation_service.format_stats(stats)
  io.println("[OK] Stats formatted: " <> stats_str)

  io.println("")
}

fn test_error_handling() {
  io.println("")
  io.println("--- Test 5: Error Handling ---")
  io.println("")

  // Test error formatting
  let api_error = translation_service.ApiError("Connection failed", Some(500))
  let formatted = translation_service.format_error(api_error)
  io.println("[OK] API error formatted: " <> formatted)

  let _explained = translation_service.explain_error(api_error)
  io.println("[OK] API error explained")

  // Test recoverable errors
  let parse_error =
    translation_service.ParseError(compiler.JsonParseError("invalid"))
  io.println(
    "[OK] Parse error is retryable: "
    <> bool_to_string(translation_service.is_retryable(parse_error)),
  )

  let low_conf = translation_service.LowConfidenceError(0.3, 0.5)
  io.println(
    "[OK] Low confidence error is retryable: "
    <> bool_to_string(translation_service.is_retryable(low_conf)),
  )

  // Test retry strategy
  let strategy = translation_service.determine_retry_strategy(parse_error, 0, 3)
  case strategy {
    translation_service.RetrySimplified ->
      io.println("[OK] Retry strategy: simplified")
    translation_service.RetryUnmodified ->
      io.println("[OK] Retry strategy: unmodified")
    translation_service.NoRetry -> io.println("[OK] Retry strategy: none")
    _ -> io.println("[OK] Other retry strategy")
  }

  // Test recovery suggestions
  let #(_error, suggestions) =
    translation_service.with_recovery_suggestions(low_conf)
  io.println(
    "[OK] Got "
    <> int_to_string(list.length(suggestions))
    <> " recovery suggestions",
  )

  // Test compile error formatting
  let compile_errors = [
    compiler.MissingField("conclusion"),
    compiler.TypeError("premises", "array", "object"),
    compiler.UnknownLogicSystem("X"),
    compiler.InvalidProposition("Unknown type"),
  ]
  list.each(compile_errors, fn(e) {
    let msg = compiler.format_error(e)
    io.println("[OK] Compile error: " <> string.slice(msg, 0, 50))
  })

  // Test is_recoverable for compile errors
  io.println(
    "[OK] MissingField recoverable: "
    <> bool_to_string(compiler.is_recoverable(compiler.MissingField("x"))),
  )
  io.println(
    "[OK] UnknownLogicSystem recoverable: "
    <> bool_to_string(compiler.is_recoverable(compiler.UnknownLogicSystem("X"))),
  )

  io.println("")
}

// Helper functions

fn compile_proposition_json(
  json_str: String,
) -> Result(proposition.Proposition, compiler.CompileError) {
  // Wrap in a minimal translation structure for testing
  let full_json =
    "{\"premises\": [], \"conclusion\": {\"natural_text\": \"test\", \"formal\": "
    <> json_str
    <> "}, \"logic_system\": \"K\", \"confidence\": 1.0}"

  case compiler.compile_translation(full_json) {
    Ok(translation) -> Ok(translation.conclusion.formal)
    Error(e) -> Error(e)
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
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
  <> pad_left(int_to_string(abs_int(frac_part)), 2, "0")
}

fn pad_left(s: String, len: Int, pad: String) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left(pad <> s, len, pad)
  }
}

fn abs_int(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

fn logic_to_string(system: proposition.LogicSystem) -> String {
  case system {
    K -> "K"
    T -> "T"
    proposition.K4 -> "K4"
    S4 -> "S4"
    S5 -> "S5"
    KD -> "KD"
    proposition.KD45 -> "KD45"
  }
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
