//// Stanford Encyclopedia of Philosophy Adapter
////
//// This module provides an adapter for extracting modal logic arguments
//// from the Stanford Encyclopedia of Philosophy.
////
//// Note: Uses the unofficial SEP API (github.com/writeonlycode/sep-api)
//// or direct parsing in production.

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

/// Configuration for SEP access
pub type SEPConfig {
  SEPConfig(
    /// Base API URL
    api_url: String,
    /// Cache duration in seconds
    cache_duration: Int,
    /// Maximum entries to fetch
    max_entries: Int,
    /// Rate limit (requests per minute)
    rate_limit: Int,
  )
}

/// An entry from SEP
pub type SEPEntry {
  SEPEntry(
    /// Entry slug/ID (e.g., "logic-modal")
    slug: String,
    /// Entry title
    title: String,
    /// Entry content (HTML or text)
    content: String,
    /// Entry URL
    url: String,
    /// Publication date
    pub_date: Option(String),
    /// Last updated
    last_updated: Option(String),
    /// Related entries
    related: List(String),
  )
}

/// An extracted argument from SEP
pub type SEPArgument {
  SEPArgument(
    /// Unique identifier
    id: String,
    /// Source entry slug
    source_entry: String,
    /// Section where found
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
    /// Tags extracted
    tags: List(String),
  )
}

/// Result of SEP fetch operation
pub type SEPResult(a) {
  SEPOk(value: a)
  SEPError(error: SEPError)
}

/// SEP error types
pub type SEPError {
  /// Network error
  NetworkError(message: String)
  /// Parse error
  ParseError(message: String)
  /// Entry not found
  EntryNotFound(slug: String)
  /// Rate limited
  RateLimited(retry_after: Int)
  /// Invalid configuration
  ConfigError(message: String)
}

// ============ Configuration ============

/// Default SEP configuration
pub fn default_config() -> SEPConfig {
  SEPConfig(
    api_url: "https://plato.stanford.edu",
    cache_duration: 86_400,
    // 1 day
    max_entries: 50,
    rate_limit: 10,
  )
}

/// Configuration for comprehensive crawling
pub fn comprehensive_config() -> SEPConfig {
  SEPConfig(
    api_url: "https://plato.stanford.edu",
    cache_duration: 604_800,
    // 1 week
    max_entries: 200,
    rate_limit: 5,
  )
}

// ============ Entry Lists ============

/// Modal logic related entry slugs
pub fn modal_logic_entries() -> List(String) {
  [
    "logic-modal",
    "logic-modal-origins",
    "logic-epistemic",
    "logic-deontic",
    "logic-temporal",
    "logic-provability",
    "possible-worlds",
    "possible-objects",
    "necessity-metaphysical",
    "modality-epistemology",
    "rigid-designators",
    "descriptions",
    "actualism",
    "possibilism-actualism",
  ]
}

/// Epistemic logic related entries
pub fn epistemic_logic_entries() -> List(String) {
  [
    "logic-epistemic",
    "epistemology",
    "knowledge-analysis",
    "knowledge-value",
    "common-knowledge",
    "belief",
    "justification-epistemic",
  ]
}

/// Deontic logic related entries
pub fn deontic_logic_entries() -> List(String) {
  [
    "logic-deontic",
    "ethics-deontological",
    "moral-reasoning",
    "normative-ethics",
    "supererogation",
    "permission",
  ]
}

/// All relevant entries for modal logic testing
pub fn all_relevant_entries() -> List(String) {
  list.flatten([
    modal_logic_entries(),
    epistemic_logic_entries(),
    deontic_logic_entries(),
  ])
  |> list.unique
}

// ============ Mock Data ============

/// Get mock SEP entries (for testing without network access)
pub fn mock_entries() -> List(SEPEntry) {
  [
    SEPEntry(
      slug: "logic-modal",
      title: "Modal Logic",
      content: "Modal logic is an extension of classical propositional logic...\n\n"
        <> "The basic modal operators are □ (necessity) and ◇ (possibility).\n\n"
        <> "Axiom K: □(p → q) → (□p → □q)\n"
        <> "This axiom states that necessity distributes over implication.\n\n"
        <> "Axiom T: □p → p\n"
        <> "What is necessary is actual (reflexivity).\n\n"
        <> "Axiom 4: □p → □□p\n"
        <> "Positive introspection (transitivity).",
      url: "https://plato.stanford.edu/entries/logic-modal/",
      pub_date: Some("2006-04-04"),
      last_updated: Some("2023-08-15"),
      related: ["possible-worlds", "logic-epistemic", "necessity-metaphysical"],
    ),
    SEPEntry(
      slug: "logic-epistemic",
      title: "Epistemic Logic",
      content: "Epistemic logic studies reasoning about knowledge and belief...\n\n"
        <> "The knowledge operator K satisfies:\n"
        <> "- Knowledge entails truth: Kp → p\n"
        <> "- Knowledge distributes: K(p → q) → (Kp → Kq)\n"
        <> "- Positive introspection: Kp → KKp\n"
        <> "- Negative introspection: ¬Kp → K¬Kp (in S5)\n\n"
        <> "Moore's Paradox: K(p ∧ ¬Kp) is always false.",
      url: "https://plato.stanford.edu/entries/logic-epistemic/",
      pub_date: Some("2001-03-14"),
      last_updated: Some("2022-11-21"),
      related: ["knowledge-analysis", "belief", "logic-modal"],
    ),
    SEPEntry(
      slug: "logic-deontic",
      title: "Deontic Logic",
      content: "Deontic logic deals with obligation, permission, and related concepts...\n\n"
        <> "Standard Deontic Logic (SDL) is based on KD.\n\n"
        <> "Axiom D: Op → Pp\n"
        <> "What is obligatory is permitted (no conflicts).\n\n"
        <> "Ross's Paradox: From 'You ought to mail the letter' we can derive\n"
        <> "'You ought to mail the letter or burn it' - counterintuitive!\n\n"
        <> "Good Samaritan Paradox: Problematic obligation inferences.",
      url: "https://plato.stanford.edu/entries/logic-deontic/",
      pub_date: Some("2006-02-21"),
      last_updated: Some("2021-09-08"),
      related: ["ethics-deontological", "logic-modal", "permission"],
    ),
    SEPEntry(
      slug: "possible-worlds",
      title: "Possible Worlds",
      content: "Possible worlds semantics provides the standard interpretation for modal logic...\n\n"
        <> "A Kripke model M = (W, R, V) where:\n"
        <> "- W is a set of worlds\n"
        <> "- R is an accessibility relation\n"
        <> "- V is a valuation function\n\n"
        <> "□p is true at w iff p is true at all worlds accessible from w.\n"
        <> "◇p is true at w iff p is true at some world accessible from w.\n\n"
        <> "Different frame conditions correspond to different axioms:\n"
        <> "- Reflexive frames: T axiom\n"
        <> "- Transitive frames: 4 axiom\n"
        <> "- Symmetric frames: B axiom\n"
        <> "- Euclidean frames: 5 axiom",
      url: "https://plato.stanford.edu/entries/possible-worlds/",
      pub_date: Some("1997-11-26"),
      last_updated: Some("2023-02-14"),
      related: ["logic-modal", "modality-epistemology", "actualism"],
    ),
  ]
}

// ============ Argument Extraction ============

/// Extract arguments from an SEP entry (simplified mock implementation)
pub fn extract_arguments(entry: SEPEntry) -> List(SEPArgument) {
  // Detect argument patterns based on entry content
  case entry.slug {
    "logic-modal" -> extract_modal_arguments(entry)
    "logic-epistemic" -> extract_epistemic_arguments(entry)
    "logic-deontic" -> extract_deontic_arguments(entry)
    "possible-worlds" -> extract_semantic_arguments(entry)
    _ -> []
  }
}

/// Extract modal logic arguments
fn extract_modal_arguments(entry: SEPEntry) -> List(SEPArgument) {
  [
    SEPArgument(
      id: "sep_modal_k",
      source_entry: entry.slug,
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
    SEPArgument(
      id: "sep_modal_t",
      source_entry: entry.slug,
      section: "Axiom T",
      natural_language: "What is necessary is actual (reflexivity)",
      premises: [Necessary(Atom("p"))],
      conclusion: Atom("p"),
      logic_system: T,
      confidence: 0.95,
      tags: ["t_axiom", "reflexivity", "fundamental"],
    ),
    SEPArgument(
      id: "sep_modal_4",
      source_entry: entry.slug,
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

/// Extract epistemic logic arguments
fn extract_epistemic_arguments(entry: SEPEntry) -> List(SEPArgument) {
  [
    SEPArgument(
      id: "sep_epistemic_factivity",
      source_entry: entry.slug,
      section: "Knowledge Entails Truth",
      natural_language: "If an agent knows p, then p is true (factivity)",
      premises: [proposition.Knows("agent", Atom("p"))],
      conclusion: Atom("p"),
      logic_system: T,
      confidence: 0.95,
      tags: ["knowledge", "factivity", "epistemic"],
    ),
    SEPArgument(
      id: "sep_epistemic_distribution",
      source_entry: entry.slug,
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

/// Extract deontic logic arguments
fn extract_deontic_arguments(entry: SEPEntry) -> List(SEPArgument) {
  [
    SEPArgument(
      id: "sep_deontic_d",
      source_entry: entry.slug,
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

/// Extract semantic arguments from possible worlds entry
fn extract_semantic_arguments(entry: SEPEntry) -> List(SEPArgument) {
  [
    SEPArgument(
      id: "sep_semantic_possibility_dual",
      source_entry: entry.slug,
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

// ============ Conversion Functions ============

/// Convert SEP argument to test fixture
pub fn argument_to_fixture(arg: SEPArgument) -> TestFixture {
  let difficulty = case arg.confidence {
    c if c >=. 0.9 -> Easy
    c if c >=. 0.7 -> Medium
    _ -> Hard
  }

  TestFixture(
    id: arg.id,
    name: arg.natural_language,
    category: ExternalDataset(source: "Stanford Encyclopedia of Philosophy"),
    natural_language: arg.natural_language,
    expected_logic_system: arg.logic_system,
    expected_premises: arg.premises,
    expected_conclusion: arg.conclusion,
    expected_validity: ExpectedValid,
    difficulty: difficulty,
    tags: arg.tags,
    source: Some("SEP:" <> arg.source_entry),
  )
}

/// Convert all arguments from an entry to fixtures
pub fn entry_to_fixtures(entry: SEPEntry) -> List(TestFixture) {
  entry
  |> extract_arguments
  |> list.map(argument_to_fixture)
}

/// Get all mock fixtures from SEP
pub fn get_mock_fixtures() -> List(TestFixture) {
  mock_entries()
  |> list.flat_map(entry_to_fixtures)
}

// ============ Utility Functions ============

/// Get argument count by entry
pub fn count_by_entry(args: List(SEPArgument)) -> Dict(String, Int) {
  args
  |> list.group(fn(arg) { arg.source_entry })
  |> dict.map_values(fn(_key, group) { list.length(group) })
}

/// Filter arguments by confidence
pub fn filter_by_confidence(
  args: List(SEPArgument),
  min_confidence: Float,
) -> List(SEPArgument) {
  list.filter(args, fn(arg) { arg.confidence >=. min_confidence })
}

/// Filter arguments by logic system
pub fn filter_by_system(
  args: List(SEPArgument),
  system: LogicSystem,
) -> List(SEPArgument) {
  list.filter(args, fn(arg) { arg.logic_system == system })
}

/// Format SEP error
pub fn format_error(error: SEPError) -> String {
  case error {
    NetworkError(msg) -> "Network error: " <> msg
    ParseError(msg) -> "Parse error: " <> msg
    EntryNotFound(slug) -> "Entry not found: " <> slug
    RateLimited(retry) ->
      "Rate limited, retry after " <> int.to_string(retry) <> " seconds"
    ConfigError(msg) -> "Configuration error: " <> msg
  }
}

/// Get statistics about extracted arguments
pub fn extraction_statistics(args: List(SEPArgument)) -> ExtractionStatistics {
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
    unique_entries: list.length(
      list.unique(list.map(args, fn(arg) { arg.source_entry })),
    ),
  )
}

/// Extraction statistics
pub type ExtractionStatistics {
  ExtractionStatistics(
    total_arguments: Int,
    by_logic_system: Dict(String, Int),
    average_confidence: Float,
    unique_entries: Int,
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

// ============ SEP Client Integration ============

/// SEP client with integrated API client
pub type SEPClient {
  SEPClient(
    /// SEP-specific configuration
    config: SEPConfig,
    /// Underlying API client state
    api_client: ClientState,
    /// Fetched entries cache
    entries_cache: Dict(String, SEPEntry),
    /// Total entries fetched
    entries_fetched: Int,
    /// Total extraction attempts
    extraction_attempts: Int,
  )
}

/// Create new SEP client from config
pub fn new_client(config: SEPConfig) -> SEPClient {
  let client_config =
    api_client.ClientConfig(
      base_url: config.api_url,
      timeout_ms: 30_000,
      max_retries: 3,
      retry_delay_ms: 1000,
      rate_limit_per_minute: config.rate_limit,
      cache_duration_sec: config.cache_duration,
      user_agent: "modal-logic-sep-adapter/1.0",
    )

  SEPClient(
    config: config,
    api_client: api_client.new_client(client_config),
    entries_cache: dict.new(),
    entries_fetched: 0,
    extraction_attempts: 0,
  )
}

/// Create SEP client with default configuration
pub fn default_client() -> SEPClient {
  new_client(default_config())
}

/// Fetch entry with retry logic (simulated)
pub fn fetch_entry_with_retry(
  client: SEPClient,
  slug: String,
  current_time: Int,
  max_retries: Int,
) -> #(SEPClient, SEPResult(SEPEntry)) {
  fetch_entry_retry_helper(client, slug, current_time, max_retries, 0, "")
}

/// Helper for retry logic
fn fetch_entry_retry_helper(
  client: SEPClient,
  slug: String,
  current_time: Int,
  max_retries: Int,
  attempt: Int,
  last_error: String,
) -> #(SEPClient, SEPResult(SEPEntry)) {
  case attempt >= max_retries {
    True -> {
      let error_msg = case last_error {
        "" -> "Max retries exceeded"
        err -> "Max retries exceeded: " <> err
      }
      #(client, SEPError(NetworkError(error_msg)))
    }
    False -> {
      // Check if entry is in local cache
      case dict.get(client.entries_cache, slug) {
        Ok(entry) -> #(client, SEPOk(entry))
        Error(_) -> {
          // Try to fetch using API client
          let request = api_client.get("/entries/" <> slug <> "/")

          let #(updated_api_client, result) =
            api_client.simulate_request(
              client.api_client,
              request,
              current_time,
            )

          let client = SEPClient(..client, api_client: updated_api_client)

          case result {
            api_client.ClientOk(response) -> {
              case response.status {
                200 -> {
                  // Parse response into SEPEntry (simplified - use mock for matching slug)
                  let entry = find_mock_entry(slug)
                  case entry {
                    Some(e) -> {
                      let new_cache = dict.insert(client.entries_cache, slug, e)
                      let new_client =
                        SEPClient(
                          ..client,
                          entries_cache: new_cache,
                          entries_fetched: client.entries_fetched + 1,
                        )
                      #(new_client, SEPOk(e))
                    }
                    None -> #(client, SEPError(EntryNotFound(slug)))
                  }
                }
                404 -> #(client, SEPError(EntryNotFound(slug)))
                429 -> {
                  // Rate limited - retry
                  fetch_entry_retry_helper(
                    client,
                    slug,
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
                      fetch_entry_retry_helper(
                        client,
                        slug,
                        current_time,
                        max_retries,
                        attempt + 1,
                        "Server error: " <> int.to_string(status),
                      )
                    False -> #(
                      client,
                      SEPError(NetworkError("HTTP " <> int.to_string(status))),
                    )
                  }
                }
              }
            }
            api_client.ClientErr(err) -> {
              case api_client.is_retryable_error(err) {
                True ->
                  fetch_entry_retry_helper(
                    client,
                    slug,
                    current_time,
                    max_retries,
                    attempt + 1,
                    api_client.format_error(err),
                  )
                False -> #(
                  client,
                  SEPError(NetworkError(api_client.format_error(err))),
                )
              }
            }
          }
        }
      }
    }
  }
}

/// Find mock entry by slug
fn find_mock_entry(slug: String) -> Option(SEPEntry) {
  mock_entries()
  |> list.find(fn(e) { e.slug == slug })
  |> option.from_result
}

/// Fetch multiple entries
pub fn fetch_entries(
  client: SEPClient,
  slugs: List(String),
  current_time: Int,
) -> #(SEPClient, List(#(String, SEPResult(SEPEntry)))) {
  list.fold(slugs, #(client, []), fn(acc, slug) {
    let #(current_client, results) = acc
    let #(updated_client, result) =
      fetch_entry_with_retry(current_client, slug, current_time, 3)
    #(updated_client, [#(slug, result), ..results])
  })
}

/// Fetch and extract arguments from multiple entries
pub fn fetch_and_extract(
  client: SEPClient,
  slugs: List(String),
  current_time: Int,
) -> #(SEPClient, List(SEPArgument)) {
  let #(updated_client, results) = fetch_entries(client, slugs, current_time)

  let arguments =
    results
    |> list.filter_map(fn(pair) {
      case pair.1 {
        SEPOk(entry) -> Ok(extract_arguments(entry))
        SEPError(_) -> Error(Nil)
      }
    })
    |> list.flatten

  let final_client =
    SEPClient(
      ..updated_client,
      extraction_attempts: updated_client.extraction_attempts
        + list.length(slugs),
    )

  #(final_client, arguments)
}

/// Get all modal logic arguments from SEP
pub fn get_modal_arguments(
  client: SEPClient,
  current_time: Int,
) -> #(SEPClient, List(SEPArgument)) {
  fetch_and_extract(client, modal_logic_entries(), current_time)
}

/// Get all relevant arguments from SEP
pub fn get_all_arguments(
  client: SEPClient,
  current_time: Int,
) -> #(SEPClient, List(SEPArgument)) {
  let entries = list.take(all_relevant_entries(), client.config.max_entries)
  fetch_and_extract(client, entries, current_time)
}

// ============ Client Statistics ============

/// Get SEP client statistics
pub fn client_statistics(client: SEPClient) -> SEPClientStatistics {
  let api_stats = api_client.client_stats(client.api_client)

  SEPClientStatistics(
    entries_fetched: client.entries_fetched,
    entries_cached: dict.size(client.entries_cache),
    extraction_attempts: client.extraction_attempts,
    api_requests: api_stats.successful_requests + api_stats.failed_requests,
    cache_hit_rate: api_stats.cache.hit_rate,
    rate_limited_count: api_stats.rate_limiter.rate_limited,
  )
}

/// SEP client statistics
pub type SEPClientStatistics {
  SEPClientStatistics(
    entries_fetched: Int,
    entries_cached: Int,
    extraction_attempts: Int,
    api_requests: Int,
    cache_hit_rate: Float,
    rate_limited_count: Int,
  )
}

/// Format client statistics
pub fn format_client_statistics(stats: SEPClientStatistics) -> String {
  string.concat([
    "SEP Client Statistics\n",
    "=====================\n",
    "Entries Fetched: ",
    int.to_string(stats.entries_fetched),
    "\n",
    "Entries Cached: ",
    int.to_string(stats.entries_cached),
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
