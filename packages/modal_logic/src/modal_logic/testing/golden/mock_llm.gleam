//// Mock LLM for deterministic translation testing
////
//// This module provides pre-recorded LLM responses keyed by input hash,
//// enabling deterministic testing of the translation pipeline.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type Proposition, And, Atom, Believes, Implies, Knows, Necessary, Not,
  Obligatory, Or, Permitted, Possible,
}

/// A recorded LLM translation response
pub type MockResponse {
  MockResponse(
    /// Hash of the input natural language
    input_hash: String,
    /// The original natural language input
    natural_language: String,
    /// The translated propositions (premises)
    premises: List(Proposition),
    /// The translated conclusion
    conclusion: Proposition,
    /// Confidence score (0.0-1.0)
    confidence: Float,
    /// Response metadata
    metadata: ResponseMetadata,
  )
}

/// Metadata about the mock response
pub type ResponseMetadata {
  ResponseMetadata(
    /// When this response was recorded
    recorded_at: Option(String),
    /// Model that generated the original response
    model_version: String,
    /// Any notes about this response
    notes: Option(String),
  )
}

/// Mode for the mock LLM
pub type MockMode {
  /// Use pre-recorded responses only
  Playback
  /// Record new responses (would need real LLM)
  Recording
  /// Use pre-recorded or fall back to simple heuristics
  PlaybackWithFallback
}

/// Mock LLM state
pub type MockLLM {
  MockLLM(
    mode: MockMode,
    /// Response bank keyed by input hash
    responses: Dict(String, MockResponse),
    /// Record of inputs without responses
    missing_inputs: List(String),
    /// Statistics
    stats: MockStats,
  )
}

/// Statistics about mock LLM usage
pub type MockStats {
  MockStats(
    cache_hits: Int,
    cache_misses: Int,
    fallback_used: Int,
  )
}

/// Create a new mock LLM with pre-loaded responses
pub fn new(mode: MockMode) -> MockLLM {
  MockLLM(
    mode: mode,
    responses: load_response_bank(),
    missing_inputs: [],
    stats: MockStats(cache_hits: 0, cache_misses: 0, fallback_used: 0),
  )
}

/// Create an empty mock LLM for recording
pub fn new_empty(mode: MockMode) -> MockLLM {
  MockLLM(
    mode: mode,
    responses: dict.new(),
    missing_inputs: [],
    stats: MockStats(cache_hits: 0, cache_misses: 0, fallback_used: 0),
  )
}

/// Translate natural language using mock LLM
pub fn translate(
  mock: MockLLM,
  natural_language: String,
) -> #(MockLLM, Result(MockResponse, String)) {
  let hash = compute_hash(natural_language)

  case dict.get(mock.responses, hash) {
    Ok(response) -> {
      // Cache hit
      let updated_stats =
        MockStats(..mock.stats, cache_hits: mock.stats.cache_hits + 1)
      #(MockLLM(..mock, stats: updated_stats), Ok(response))
    }
    Error(_) -> {
      // Cache miss
      case mock.mode {
        Playback -> {
          let updated_stats =
            MockStats(..mock.stats, cache_misses: mock.stats.cache_misses + 1)
          let updated_missing = [natural_language, ..mock.missing_inputs]
          #(
            MockLLM(..mock, stats: updated_stats, missing_inputs: updated_missing),
            Error("No cached response for input: " <> truncate(natural_language, 50)),
          )
        }
        Recording -> {
          // In recording mode, we'd call the real LLM
          // For now, just note it as missing
          let updated_stats =
            MockStats(..mock.stats, cache_misses: mock.stats.cache_misses + 1)
          let updated_missing = [natural_language, ..mock.missing_inputs]
          #(
            MockLLM(..mock, stats: updated_stats, missing_inputs: updated_missing),
            Error("Recording mode: would call real LLM for: " <> truncate(natural_language, 50)),
          )
        }
        PlaybackWithFallback -> {
          // Use heuristic fallback
          let updated_stats =
            MockStats(..mock.stats, fallback_used: mock.stats.fallback_used + 1)
          let response = generate_fallback_response(natural_language)
          #(MockLLM(..mock, stats: updated_stats), Ok(response))
        }
      }
    }
  }
}

/// Add a response to the mock LLM bank
pub fn add_response(mock: MockLLM, response: MockResponse) -> MockLLM {
  let updated_responses = dict.insert(mock.responses, response.input_hash, response)
  MockLLM(..mock, responses: updated_responses)
}

/// Get the list of inputs that had no cached response
pub fn get_missing_inputs(mock: MockLLM) -> List(String) {
  mock.missing_inputs
}

/// Get statistics about mock usage
pub fn get_stats(mock: MockLLM) -> MockStats {
  mock.stats
}

/// Compute a simple hash for input text
pub fn compute_hash(text: String) -> String {
  // Simple hash: lowercase, trim, take first 100 chars, compute checksum
  let normalized =
    text
    |> string.lowercase
    |> string.trim
    |> string.slice(0, 100)

  // Simple checksum using character codes
  let checksum =
    normalized
    |> string.to_utf_codepoints
    |> list.fold(0, fn(acc, cp) {
      let code = string.utf_codepoint_to_int(cp)
      { acc * 31 + code } % 1_000_000_007
    })

  "hash_" <> int.to_string(checksum)
}

/// Truncate a string with ellipsis
fn truncate(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len - 3) <> "..."
    False -> s
  }
}

/// Generate a fallback response using simple heuristics
fn generate_fallback_response(natural_language: String) -> MockResponse {
  let lower = string.lowercase(natural_language)

  // Detect modal operators from keywords
  let has_necessary = string.contains(lower, "necessarily") || string.contains(lower, "must be")
  let has_possible = string.contains(lower, "possibly") || string.contains(lower, "might be")
  let has_knows = string.contains(lower, "knows") || string.contains(lower, "knowledge")
  let has_believes = string.contains(lower, "believes") || string.contains(lower, "belief")
  let has_ought = string.contains(lower, "ought") || string.contains(lower, "should")
  let has_permitted = string.contains(lower, "permitted") || string.contains(lower, "allowed")

  // Generate a simple proposition based on detected modalities
  let base_prop = Atom("p")
  let wrapped_prop = case has_necessary, has_possible, has_knows, has_believes, has_ought, has_permitted {
    True, _, _, _, _, _ -> Necessary(base_prop)
    _, True, _, _, _, _ -> Possible(base_prop)
    _, _, True, _, _, _ -> Knows("agent", base_prop)
    _, _, _, True, _, _ -> Believes("agent", base_prop)
    _, _, _, _, True, _ -> Obligatory(base_prop)
    _, _, _, _, _, True -> Permitted(base_prop)
    _, _, _, _, _, _ -> base_prop
  }

  // Detect implication structure
  let has_therefore = string.contains(lower, "therefore") || string.contains(lower, "thus")
  let has_if_then = string.contains(lower, "if") && string.contains(lower, "then")

  let conclusion = case has_if_then {
    True -> Implies(wrapped_prop, Atom("q"))
    False -> wrapped_prop
  }

  let premises = case has_therefore {
    True -> [wrapped_prop, Implies(wrapped_prop, Atom("q"))]
    False -> [wrapped_prop]
  }

  MockResponse(
    input_hash: compute_hash(natural_language),
    natural_language: natural_language,
    premises: premises,
    conclusion: conclusion,
    confidence: 0.5,
    // Low confidence for heuristic
    metadata: ResponseMetadata(
      recorded_at: None,
      model_version: "heuristic-v1",
      notes: Some("Generated by fallback heuristics"),
    ),
  )
}

/// Load the pre-recorded response bank
fn load_response_bank() -> Dict(String, MockResponse) {
  // Pre-recorded responses for classic philosophical arguments
  let responses = [
    // Modus Ponens
    create_response(
      "If it is raining, then the ground is wet. It is raining. Therefore, the ground is wet.",
      [
        Implies(Atom("raining"), Atom("ground_wet")),
        Atom("raining"),
      ],
      Atom("ground_wet"),
      0.95,
    ),
    // Modal Modus Ponens
    create_response(
      "Necessarily, if something is a bachelor then it is unmarried. John is necessarily a bachelor. Therefore, John is necessarily unmarried.",
      [
        Necessary(Implies(Atom("bachelor"), Atom("unmarried"))),
        Necessary(Atom("bachelor")),
      ],
      Necessary(Atom("unmarried")),
      0.92,
    ),
    // Ontological argument (simplified)
    create_response(
      "God is defined as a being than which nothing greater can be conceived. It is greater to exist in reality than only in the mind. Therefore, God necessarily exists.",
      [
        Implies(Atom("greatest_conceivable"), Necessary(Atom("exists"))),
        Atom("greatest_conceivable"),
      ],
      Necessary(Atom("exists")),
      0.75,
    ),
    // K Axiom
    create_response(
      "If it is necessary that p implies q, and it is necessary that p, then it is necessary that q.",
      [
        Necessary(Implies(Atom("p"), Atom("q"))),
        Necessary(Atom("p")),
      ],
      Necessary(Atom("q")),
      0.98,
    ),
    // T Axiom
    create_response(
      "If it is necessary that p, then p is actually the case.",
      [Necessary(Atom("p"))],
      Atom("p"),
      0.97,
    ),
    // Deontic reasoning
    create_response(
      "If you promised to help, then you ought to help. You promised to help. Therefore, you ought to help.",
      [
        Implies(Atom("promised"), Obligatory(Atom("help"))),
        Atom("promised"),
      ],
      Obligatory(Atom("help")),
      0.90,
    ),
    // Epistemic reasoning
    create_response(
      "Alice knows that Bob knows the secret. If someone knows something, they believe it. Therefore, Alice believes Bob knows the secret.",
      [
        Knows("alice", Knows("bob", Atom("secret"))),
        Implies(Knows("x", Atom("p")), Believes("x", Atom("p"))),
      ],
      Believes("alice", Knows("bob", Atom("secret"))),
      0.88,
    ),
    // Invalid argument (affirming the consequent)
    create_response(
      "If it rains, the ground is wet. The ground is wet. Therefore, it rained.",
      [
        Implies(Atom("rains"), Atom("ground_wet")),
        Atom("ground_wet"),
      ],
      Atom("rains"),
      0.85,
    ),
    // Disjunctive syllogism
    create_response(
      "Either it is raining or it is snowing. It is not raining. Therefore, it is snowing.",
      [
        Or(Atom("raining"), Atom("snowing")),
        Not(Atom("raining")),
      ],
      Atom("snowing"),
      0.93,
    ),
    // Hypothetical syllogism
    create_response(
      "If A then B. If B then C. Therefore, if A then C.",
      [
        Implies(Atom("A"), Atom("B")),
        Implies(Atom("B"), Atom("C")),
      ],
      Implies(Atom("A"), Atom("C")),
      0.96,
    ),
    // Double negation
    create_response(
      "It is not the case that it is not raining. Therefore, it is raining.",
      [Not(Not(Atom("raining")))],
      Atom("raining"),
      0.94,
    ),
    // Modal possibility
    create_response(
      "It is possible that unicorns exist. If something is possible, it is not necessarily false. Therefore, it is not necessarily the case that unicorns do not exist.",
      [
        Possible(Atom("unicorns_exist")),
        Implies(Possible(Atom("p")), Not(Necessary(Not(Atom("p"))))),
      ],
      Not(Necessary(Not(Atom("unicorns_exist")))),
      0.89,
    ),
    // Permission and obligation
    create_response(
      "It is obligatory to follow the law. If something is obligatory, it is permitted. Therefore, following the law is permitted.",
      [
        Obligatory(Atom("follow_law")),
        Implies(Obligatory(Atom("p")), Permitted(Atom("p"))),
      ],
      Permitted(Atom("follow_law")),
      0.91,
    ),
    // S5 characteristic
    create_response(
      "It is possible that p. In S5, if something is possible, it is necessarily possible. Therefore, it is necessarily possible that p.",
      [Possible(Atom("p"))],
      Necessary(Possible(Atom("p"))),
      0.87,
    ),
    // Conjunction elimination
    create_response(
      "Both p and q are true. Therefore, p is true.",
      [And(Atom("p"), Atom("q"))],
      Atom("p"),
      0.99,
    ),
  ]

  responses
  |> list.fold(dict.new(), fn(d, resp) {
    dict.insert(d, resp.input_hash, resp)
  })
}

/// Helper to create a mock response
fn create_response(
  natural_language: String,
  premises: List(Proposition),
  conclusion: Proposition,
  confidence: Float,
) -> MockResponse {
  MockResponse(
    input_hash: compute_hash(natural_language),
    natural_language: natural_language,
    premises: premises,
    conclusion: conclusion,
    confidence: confidence,
    metadata: ResponseMetadata(
      recorded_at: Some("2024-01-15T00:00:00Z"),
      model_version: "claude-3-sonnet-20241022",
      notes: None,
    ),
  )
}

/// Format mock stats as string
pub fn stats_to_string(stats: MockStats) -> String {
  "MockLLM Stats: "
  <> int.to_string(stats.cache_hits)
  <> " hits, "
  <> int.to_string(stats.cache_misses)
  <> " misses, "
  <> int.to_string(stats.fallback_used)
  <> " fallbacks"
}

/// Get cache hit rate
pub fn hit_rate(stats: MockStats) -> Float {
  let total = stats.cache_hits + stats.cache_misses + stats.fallback_used
  case total {
    0 -> 0.0
    _ -> int.to_float(stats.cache_hits) /. int.to_float(total)
  }
}
