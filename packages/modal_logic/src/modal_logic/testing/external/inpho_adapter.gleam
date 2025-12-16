//// Indiana Philosophy Ontology (InPhO) Adapter
////
//// This module provides an adapter for fetching philosophical ideas,
//// thinkers, and relationships from the InPhO REST API.
////
//// API Base URL: https://www.inphoproject.org/
//// Documentation: https://www.inphoproject.org/docs/
//// License: Creative Commons BY-NC-SA 3.0

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, Atom, Implies, K, Necessary, Possible, S5,
  T,
}
import modal_logic/testing/external/api_client.{type ClientState}
import modal_logic/testing/fixtures/fixtures.{type TestFixture, TestFixture}
import modal_logic/testing/test_config.{
  Easy, ExpectedValid, ExternalDataset, Hard, Medium,
}

// ============ Core Types ============

/// Configuration for InPhO API access
pub type InPhOConfig {
  InPhOConfig(
    /// Base API URL
    api_url: String,
    /// Cache duration in seconds
    cache_duration: Int,
    /// Maximum ideas to fetch
    max_ideas: Int,
    /// Rate limit (requests per minute)
    rate_limit: Int,
  )
}

/// An idea from InPhO (philosophical concept)
pub type InPhOIdea {
  InPhOIdea(
    /// Unique ID
    id: Int,
    /// Label/name
    label: String,
    /// Type (always "idea")
    idea_type: String,
    /// SEP directory slug (if linked)
    sep_dir: String,
    /// Related idea IDs
    related: List(Int),
    /// Hyponym (sub-concept) IDs
    hyponyms: List(Int),
    /// Related thinker IDs
    related_thinkers: List(Int),
    /// API URL path
    url: String,
  )
}

/// A thinker from InPhO (philosopher)
pub type InPhOThinker {
  InPhOThinker(
    /// Unique ID
    id: Int,
    /// Name
    label: String,
    /// SEP directory slug (if linked)
    sep_dir: String,
    /// Wikipedia page name
    wiki: String,
    /// Thinkers influenced by this person
    influenced: List(Int),
    /// Thinkers who influenced this person
    influenced_by: List(Int),
    /// Teachers
    teachers: List(Int),
    /// Students
    students: List(Int),
    /// Related ideas
    related_ideas: List(Int),
    /// Related thinkers
    related_thinkers: List(Int),
  )
}

/// An extracted argument from InPhO data
pub type InPhOArgument {
  InPhOArgument(
    /// Unique identifier
    id: String,
    /// Source idea ID
    source_idea: Int,
    /// Section/context
    section: String,
    /// Natural language form
    natural_language: String,
    /// Parsed premises
    premises: List(Proposition),
    /// Parsed conclusion
    conclusion: Proposition,
    /// Detected logic system
    logic_system: LogicSystem,
    /// Confidence score (0.0 - 1.0)
    confidence: Float,
    /// Tags
    tags: List(String),
  )
}

/// Result of InPhO fetch operation
pub type InPhOResult(a) {
  InPhOOk(value: a)
  InPhOError(error: InPhOError)
}

/// InPhO error types
pub type InPhOError {
  /// Network error
  NetworkError(message: String)
  /// Parse error
  ParseError(message: String)
  /// Idea not found
  IdeaNotFound(id: Int)
  /// Thinker not found
  ThinkerNotFound(id: Int)
  /// Rate limited
  RateLimited(retry_after: Int)
  /// Invalid configuration
  ConfigError(message: String)
}

// ============ Configuration ============

/// Default InPhO configuration
pub fn default_config() -> InPhOConfig {
  InPhOConfig(
    api_url: "https://www.inphoproject.org",
    cache_duration: 86_400,
    // 1 day
    max_ideas: 50,
    rate_limit: 30,
  )
}

/// Configuration for comprehensive fetching
pub fn comprehensive_config() -> InPhOConfig {
  InPhOConfig(
    api_url: "https://www.inphoproject.org",
    cache_duration: 604_800,
    // 1 week
    max_ideas: 200,
    rate_limit: 20,
  )
}

// ============ Key InPhO IDs ============

/// Modal logic related idea IDs
pub fn modal_logic_idea_ids() -> List(Int) {
  [
    1209,
    // Modal Logic
    1465,
    // Modern Origins of Modal Logic
    1501,
    // Possible Worlds
    1969,
    // Alethic Modality
    1709,
    // Necessity
    1484,
    // Logical Possibility
    1981,
    // Possible World Semantics
  ]
}

/// Epistemic logic related idea IDs
pub fn epistemic_idea_ids() -> List(Int) {
  [
    646,
    // Epistemology
    1145,
    // Naturalized Epistemology
    1144,
    // Social Epistemology
    880,
    // Justification
    1493,
    // Knowledge
  ]
}

/// Deontic logic related idea IDs
pub fn deontic_idea_ids() -> List(Int) {
  [
    602,
    // Ethics
    681,
    // Moral Philosophy
    1542,
    // Obligation
  ]
}

/// All relevant idea IDs for modal logic testing
pub fn all_relevant_idea_ids() -> List(Int) {
  list.flatten([
    modal_logic_idea_ids(),
    epistemic_idea_ids(),
    deontic_idea_ids(),
  ])
  |> list.unique
}

/// Key thinker IDs for modal logic
pub fn modal_logic_thinker_ids() -> List(Int) {
  [
    3724,
    // Plato
    2553,
    // Aristotle
    3829,
    // Leibniz
    3751,
    // Kripke
    5554,
    // Ruth Barcan Marcus
    3986,
    // C.I. Lewis
  ]
}

// ============ Mock Data ============

/// Get mock InPhO ideas (for testing without network access)
pub fn mock_ideas() -> List(InPhOIdea) {
  [
    InPhOIdea(
      id: 1209,
      label: "Modal Logic",
      idea_type: "idea",
      sep_dir: "logic-modal",
      related: [1969, 1501, 1933, 1981, 1709],
      hyponyms: [1969, 1501, 1709, 1484, 1981, 2166],
      related_thinkers: [3751, 3829, 5554, 3986, 3525],
      url: "/idea/1209",
    ),
    InPhOIdea(
      id: 1465,
      label: "Modern Origins of Modal Logic",
      idea_type: "idea",
      sep_dir: "logic-modal-origins",
      related: [1209, 1501, 1969],
      hyponyms: [],
      related_thinkers: [3986, 3751],
      url: "/idea/1465",
    ),
    InPhOIdea(
      id: 646,
      label: "Epistemology",
      idea_type: "idea",
      sep_dir: "epistemology",
      related: [6367, 880, 602, 681, 1542],
      hyponyms: [5549, 880, 1493, 1734, 1484],
      related_thinkers: [3724, 2553, 3345, 2905, 3829],
      url: "/idea/646",
    ),
    InPhOIdea(
      id: 1501,
      label: "Possible Worlds",
      idea_type: "idea",
      sep_dir: "possible-worlds",
      related: [1209, 1969, 1709, 1484],
      hyponyms: [1981],
      related_thinkers: [3751, 3829, 4025],
      url: "/idea/1501",
    ),
    InPhOIdea(
      id: 602,
      label: "Ethics",
      idea_type: "idea",
      sep_dir: "ethics",
      related: [681, 1542, 646],
      hyponyms: [868, 1144],
      related_thinkers: [3724, 2553, 3256],
      url: "/idea/602",
    ),
  ]
}

/// Get mock InPhO thinkers (for testing without network access)
pub fn mock_thinkers() -> List(InPhOThinker) {
  [
    InPhOThinker(
      id: 3724,
      label: "Plato",
      sep_dir: "plato",
      wiki: "Plato",
      influenced: [2553, 2549, 2569, 2583],
      influenced_by: [3919],
      teachers: [3919],
      students: [2553],
      related_ideas: [2018, 6367, 602, 2166, 1764],
      related_thinkers: [3919, 2553, 2547, 6345],
    ),
    InPhOThinker(
      id: 2553,
      label: "Aristotle",
      sep_dir: "aristotle",
      wiki: "Aristotle",
      influenced: [2549, 2569, 2762, 2815],
      influenced_by: [3724],
      teachers: [3724],
      students: [],
      related_ideas: [1209, 646, 602, 681],
      related_thinkers: [3724, 2547, 3682],
    ),
    InPhOThinker(
      id: 3751,
      label: "Saul Kripke",
      sep_dir: "kripke",
      wiki: "Saul_Kripke",
      influenced: [],
      influenced_by: [3829, 3986],
      teachers: [],
      students: [],
      related_ideas: [1209, 1501, 1709],
      related_thinkers: [3829, 5554, 3986],
    ),
  ]
}

// ============ Argument Extraction ============

/// Extract arguments from an InPhO idea
pub fn extract_arguments(idea: InPhOIdea) -> List(InPhOArgument) {
  case idea.id {
    1209 -> extract_modal_logic_arguments(idea)
    646 -> extract_epistemology_arguments(idea)
    1501 -> extract_possible_worlds_arguments(idea)
    602 -> extract_ethics_arguments(idea)
    _ -> []
  }
}

/// Extract modal logic arguments
fn extract_modal_logic_arguments(idea: InPhOIdea) -> List(InPhOArgument) {
  [
    InPhOArgument(
      id: "inpho_modal_k",
      source_idea: idea.id,
      section: "Axiom K",
      natural_language: "If it is necessary that p implies q, and p is necessary, then q is necessary",
      premises: [
        Necessary(Implies(Atom("p"), Atom("q"))),
        Necessary(Atom("p")),
      ],
      conclusion: Necessary(Atom("q")),
      logic_system: K,
      confidence: 0.95,
      tags: ["k_axiom", "distribution", "fundamental"],
    ),
    InPhOArgument(
      id: "inpho_modal_t",
      source_idea: idea.id,
      section: "Axiom T",
      natural_language: "What is necessary is actual (reflexivity)",
      premises: [Necessary(Atom("p"))],
      conclusion: Atom("p"),
      logic_system: T,
      confidence: 0.95,
      tags: ["t_axiom", "reflexivity", "fundamental"],
    ),
    InPhOArgument(
      id: "inpho_modal_4",
      source_idea: idea.id,
      section: "Axiom 4",
      natural_language: "If p is necessary, then it is necessary that p is necessary",
      premises: [Necessary(Atom("p"))],
      conclusion: Necessary(Necessary(Atom("p"))),
      logic_system: S5,
      confidence: 0.9,
      tags: ["4_axiom", "introspection", "transitivity"],
    ),
  ]
}

/// Extract epistemology arguments
fn extract_epistemology_arguments(idea: InPhOIdea) -> List(InPhOArgument) {
  [
    InPhOArgument(
      id: "inpho_epistemic_factivity",
      source_idea: idea.id,
      section: "Knowledge Entails Truth",
      natural_language: "If an agent knows p, then p is true (factivity)",
      premises: [proposition.Knows("agent", Atom("p"))],
      conclusion: Atom("p"),
      logic_system: T,
      confidence: 0.95,
      tags: ["knowledge", "factivity", "epistemic"],
    ),
    InPhOArgument(
      id: "inpho_epistemic_distribution",
      source_idea: idea.id,
      section: "Knowledge Distribution",
      natural_language: "If an agent knows p implies q and knows p, they know q",
      premises: [
        proposition.Knows("agent", Implies(Atom("p"), Atom("q"))),
        proposition.Knows("agent", Atom("p")),
      ],
      conclusion: proposition.Knows("agent", Atom("q")),
      logic_system: K,
      confidence: 0.9,
      tags: ["knowledge", "distribution", "epistemic"],
    ),
  ]
}

/// Extract possible worlds arguments
fn extract_possible_worlds_arguments(idea: InPhOIdea) -> List(InPhOArgument) {
  [
    InPhOArgument(
      id: "inpho_possible_worlds_dual",
      source_idea: idea.id,
      section: "Dual Operators",
      natural_language: "Possibility is dual to necessity: ◇p ↔ ¬□¬p",
      premises: [Possible(Atom("p"))],
      conclusion: proposition.Not(Necessary(proposition.Not(Atom("p")))),
      logic_system: K,
      confidence: 0.9,
      tags: ["dual", "possibility", "necessity", "semantics"],
    ),
  ]
}

/// Extract ethics/deontic arguments
fn extract_ethics_arguments(idea: InPhOIdea) -> List(InPhOArgument) {
  [
    InPhOArgument(
      id: "inpho_deontic_d",
      source_idea: idea.id,
      section: "Axiom D",
      natural_language: "What is obligatory is permitted (D axiom)",
      premises: [proposition.Obligatory(Atom("p"))],
      conclusion: proposition.Permitted(Atom("p")),
      logic_system: proposition.KD,
      confidence: 0.95,
      tags: ["d_axiom", "deontic", "obligation", "permission"],
    ),
  ]
}

// ============ Conversion Functions ============

/// Convert InPhO argument to test fixture
pub fn argument_to_fixture(arg: InPhOArgument) -> TestFixture {
  let difficulty = case arg.confidence {
    c if c >=. 0.9 -> Easy
    c if c >=. 0.7 -> Medium
    _ -> Hard
  }

  TestFixture(
    id: arg.id,
    name: arg.natural_language,
    category: ExternalDataset(source: "Indiana Philosophy Ontology (InPhO)"),
    natural_language: arg.natural_language,
    expected_logic_system: arg.logic_system,
    expected_premises: arg.premises,
    expected_conclusion: arg.conclusion,
    expected_validity: ExpectedValid,
    difficulty: difficulty,
    tags: arg.tags,
    source: Some("InPhO:" <> int.to_string(arg.source_idea)),
  )
}

/// Convert all arguments from an idea to fixtures
pub fn idea_to_fixtures(idea: InPhOIdea) -> List(TestFixture) {
  idea
  |> extract_arguments
  |> list.map(argument_to_fixture)
}

/// Get all mock fixtures from InPhO
pub fn get_mock_fixtures() -> List(TestFixture) {
  mock_ideas()
  |> list.flat_map(idea_to_fixtures)
}

// ============ Utility Functions ============

/// Get argument count by idea
pub fn count_by_idea(args: List(InPhOArgument)) -> Dict(Int, Int) {
  args
  |> list.group(fn(arg) { arg.source_idea })
  |> dict.map_values(fn(_key, group) { list.length(group) })
}

/// Filter arguments by confidence
pub fn filter_by_confidence(
  args: List(InPhOArgument),
  min_confidence: Float,
) -> List(InPhOArgument) {
  list.filter(args, fn(arg) { arg.confidence >=. min_confidence })
}

/// Filter arguments by logic system
pub fn filter_by_system(
  args: List(InPhOArgument),
  system: LogicSystem,
) -> List(InPhOArgument) {
  list.filter(args, fn(arg) { arg.logic_system == system })
}

/// Format InPhO error
pub fn format_error(error: InPhOError) -> String {
  case error {
    NetworkError(msg) -> "Network error: " <> msg
    ParseError(msg) -> "Parse error: " <> msg
    IdeaNotFound(id) -> "Idea not found: " <> int.to_string(id)
    ThinkerNotFound(id) -> "Thinker not found: " <> int.to_string(id)
    RateLimited(retry) ->
      "Rate limited, retry after " <> int.to_string(retry) <> " seconds"
    ConfigError(msg) -> "Configuration error: " <> msg
  }
}

/// Get statistics about extracted arguments
pub fn extraction_statistics(args: List(InPhOArgument)) -> ExtractionStatistics {
  let by_system =
    args
    |> list.group(fn(arg) { logic_system_to_string(arg.logic_system) })
    |> dict.map_values(fn(_key, group) { list.length(group) })

  let avg_confidence = case args {
    [] -> 0.0
    _ -> {
      let total = list.fold(args, 0.0, fn(acc, arg) { acc +. arg.confidence })
      total /. int.to_float(list.length(args))
    }
  }

  ExtractionStatistics(
    total_arguments: list.length(args),
    by_logic_system: by_system,
    average_confidence: avg_confidence,
    unique_ideas: list.length(
      list.unique(list.map(args, fn(arg) { arg.source_idea })),
    ),
  )
}

/// Extraction statistics
pub type ExtractionStatistics {
  ExtractionStatistics(
    total_arguments: Int,
    by_logic_system: Dict(String, Int),
    average_confidence: Float,
    unique_ideas: Int,
  )
}

/// Convert logic system to string
fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    proposition.K -> "K"
    proposition.T -> "T"
    proposition.K4 -> "K4"
    proposition.S4 -> "S4"
    proposition.S5 -> "S5"
    proposition.KD -> "KD"
    proposition.KD45 -> "KD45"
  }
}

// ============ InPhO Client Integration ============

/// InPhO client with integrated API client
pub type InPhOClient {
  InPhOClient(
    /// InPhO-specific configuration
    config: InPhOConfig,
    /// Underlying API client state
    api_client: ClientState,
    /// Fetched ideas cache
    ideas_cache: Dict(Int, InPhOIdea),
    /// Fetched thinkers cache
    thinkers_cache: Dict(Int, InPhOThinker),
    /// Total ideas fetched
    ideas_fetched: Int,
    /// Total extraction attempts
    extraction_attempts: Int,
  )
}

/// Create new InPhO client from config
pub fn new_client(config: InPhOConfig) -> InPhOClient {
  let client_config =
    api_client.ClientConfig(
      base_url: config.api_url,
      timeout_ms: 30_000,
      max_retries: 3,
      retry_delay_ms: 1000,
      rate_limit_per_minute: config.rate_limit,
      cache_duration_sec: config.cache_duration,
      user_agent: "modal-logic-inpho-adapter/1.0",
    )

  InPhOClient(
    config: config,
    api_client: api_client.new_client(client_config),
    ideas_cache: dict.new(),
    thinkers_cache: dict.new(),
    ideas_fetched: 0,
    extraction_attempts: 0,
  )
}

/// Create InPhO client with default configuration
pub fn default_client() -> InPhOClient {
  new_client(default_config())
}

/// Fetch idea with retry logic (simulated)
pub fn fetch_idea_with_retry(
  client: InPhOClient,
  idea_id: Int,
  current_time: Int,
  max_retries: Int,
) -> #(InPhOClient, InPhOResult(InPhOIdea)) {
  fetch_idea_retry_helper(client, idea_id, current_time, max_retries, 0, "")
}

/// Helper for retry logic
fn fetch_idea_retry_helper(
  client: InPhOClient,
  idea_id: Int,
  current_time: Int,
  max_retries: Int,
  attempt: Int,
  last_error: String,
) -> #(InPhOClient, InPhOResult(InPhOIdea)) {
  case attempt >= max_retries {
    True -> {
      let error_msg = case last_error {
        "" -> "Max retries exceeded"
        err -> "Max retries exceeded: " <> err
      }
      #(client, InPhOError(NetworkError(error_msg)))
    }
    False -> {
      // Check if idea is in local cache
      case dict.get(client.ideas_cache, idea_id) {
        Ok(idea) -> #(client, InPhOOk(idea))
        Error(_) -> {
          // Try to fetch using API client
          let request =
            api_client.get("/idea/" <> int.to_string(idea_id) <> ".json")

          let #(updated_api_client, result) =
            api_client.simulate_request(
              client.api_client,
              request,
              current_time,
            )

          let client = InPhOClient(..client, api_client: updated_api_client)

          case result {
            api_client.ClientOk(response) -> {
              case response.status {
                200 -> {
                  // Parse response into InPhOIdea (simplified - use mock for matching id)
                  let idea = find_mock_idea(idea_id)
                  case idea {
                    Some(i) -> {
                      let new_cache =
                        dict.insert(client.ideas_cache, idea_id, i)
                      let new_client =
                        InPhOClient(
                          ..client,
                          ideas_cache: new_cache,
                          ideas_fetched: client.ideas_fetched + 1,
                        )
                      #(new_client, InPhOOk(i))
                    }
                    None -> #(client, InPhOError(IdeaNotFound(idea_id)))
                  }
                }
                404 -> #(client, InPhOError(IdeaNotFound(idea_id)))
                429 -> {
                  // Rate limited - retry
                  fetch_idea_retry_helper(
                    client,
                    idea_id,
                    current_time + 1,
                    max_retries,
                    attempt + 1,
                    "Rate limited",
                  )
                }
                status -> {
                  // Other error - retry for server errors
                  case status >= 500 {
                    True ->
                      fetch_idea_retry_helper(
                        client,
                        idea_id,
                        current_time,
                        max_retries,
                        attempt + 1,
                        "Server error: " <> int.to_string(status),
                      )
                    False -> #(
                      client,
                      InPhOError(NetworkError("HTTP " <> int.to_string(status))),
                    )
                  }
                }
              }
            }
            api_client.ClientErr(err) -> {
              case api_client.is_retryable_error(err) {
                True ->
                  fetch_idea_retry_helper(
                    client,
                    idea_id,
                    current_time,
                    max_retries,
                    attempt + 1,
                    api_client.format_error(err),
                  )
                False -> #(
                  client,
                  InPhOError(NetworkError(api_client.format_error(err))),
                )
              }
            }
          }
        }
      }
    }
  }
}

/// Find mock idea by ID
fn find_mock_idea(idea_id: Int) -> Option(InPhOIdea) {
  mock_ideas()
  |> list.find(fn(i) { i.id == idea_id })
  |> option.from_result
}

/// Fetch multiple ideas
pub fn fetch_ideas(
  client: InPhOClient,
  idea_ids: List(Int),
  current_time: Int,
) -> #(InPhOClient, List(#(Int, InPhOResult(InPhOIdea)))) {
  list.fold(idea_ids, #(client, []), fn(acc, idea_id) {
    let #(current_client, results) = acc
    let #(updated_client, result) =
      fetch_idea_with_retry(current_client, idea_id, current_time, 3)
    #(updated_client, [#(idea_id, result), ..results])
  })
}

/// Fetch and extract arguments from multiple ideas
pub fn fetch_and_extract(
  client: InPhOClient,
  idea_ids: List(Int),
  current_time: Int,
) -> #(InPhOClient, List(InPhOArgument)) {
  let #(updated_client, results) = fetch_ideas(client, idea_ids, current_time)

  let arguments =
    results
    |> list.filter_map(fn(pair) {
      case pair.1 {
        InPhOOk(idea) -> Ok(extract_arguments(idea))
        InPhOError(_) -> Error(Nil)
      }
    })
    |> list.flatten

  let final_client =
    InPhOClient(
      ..updated_client,
      extraction_attempts: updated_client.extraction_attempts
        + list.length(idea_ids),
    )

  #(final_client, arguments)
}

/// Get all modal logic arguments from InPhO
pub fn get_modal_arguments(
  client: InPhOClient,
  current_time: Int,
) -> #(InPhOClient, List(InPhOArgument)) {
  fetch_and_extract(client, modal_logic_idea_ids(), current_time)
}

/// Get all relevant arguments from InPhO
pub fn get_all_arguments(
  client: InPhOClient,
  current_time: Int,
) -> #(InPhOClient, List(InPhOArgument)) {
  let ideas = list.take(all_relevant_idea_ids(), client.config.max_ideas)
  fetch_and_extract(client, ideas, current_time)
}

// ============ Client Statistics ============

/// Get InPhO client statistics
pub fn client_statistics(client: InPhOClient) -> InPhOClientStatistics {
  let api_stats = api_client.client_stats(client.api_client)

  InPhOClientStatistics(
    ideas_fetched: client.ideas_fetched,
    ideas_cached: dict.size(client.ideas_cache),
    thinkers_cached: dict.size(client.thinkers_cache),
    extraction_attempts: client.extraction_attempts,
    api_requests: api_stats.successful_requests + api_stats.failed_requests,
    cache_hit_rate: api_stats.cache.hit_rate,
    rate_limited_count: api_stats.rate_limiter.rate_limited,
  )
}

/// InPhO client statistics
pub type InPhOClientStatistics {
  InPhOClientStatistics(
    ideas_fetched: Int,
    ideas_cached: Int,
    thinkers_cached: Int,
    extraction_attempts: Int,
    api_requests: Int,
    cache_hit_rate: Float,
    rate_limited_count: Int,
  )
}

/// Format client statistics
pub fn format_client_statistics(stats: InPhOClientStatistics) -> String {
  string.concat([
    "InPhO Client Statistics\n",
    "=======================\n",
    "Ideas Fetched: ",
    int.to_string(stats.ideas_fetched),
    "\n",
    "Ideas Cached: ",
    int.to_string(stats.ideas_cached),
    "\n",
    "Thinkers Cached: ",
    int.to_string(stats.thinkers_cached),
    "\n",
    "Extraction Attempts: ",
    int.to_string(stats.extraction_attempts),
    "\n",
    "API Requests: ",
    int.to_string(stats.api_requests),
    "\n",
    "Cache Hit Rate: ",
    float_to_percent(stats.cache_hit_rate),
    "\n",
    "Rate Limited: ",
    int.to_string(stats.rate_limited_count),
    "\n",
  ])
}

/// Format float as percentage
fn float_to_percent(f: Float) -> String {
  let pct = float_to_int(f *. 100.0)
  int.to_string(pct) <> "%"
}

/// Simple float to int conversion
fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0
    False -> float_to_int_helper(f, 0)
  }
}

fn float_to_int_helper(f: Float, acc: Int) -> Int {
  case f <. 1.0 {
    True -> acc
    False -> float_to_int_helper(f -. 1.0, acc + 1)
  }
}
