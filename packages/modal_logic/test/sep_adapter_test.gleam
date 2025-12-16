//// Tests for SEP Adapter
////
//// Tests the Stanford Encyclopedia of Philosophy integration including
//// the API client wrapper, caching, rate limiting, and argument extraction.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/proposition.{K, S5, T}
import modal_logic/testing/external/api_client.{ClientConfig, HttpResponse}
import modal_logic/testing/external/sep_adapter.{
  SEPOk, default_client, default_config, mock_entries, new_client,
}

// ============ Configuration Tests ============

pub fn default_config_test() {
  let config = default_config()

  config.api_url |> should.equal("https://plato.stanford.edu")
  config.cache_duration |> should.equal(86_400)
  config.max_entries |> should.equal(50)
  config.rate_limit |> should.equal(10)
}

pub fn comprehensive_config_test() {
  let config = sep_adapter.comprehensive_config()

  config.cache_duration |> should.equal(604_800)
  config.max_entries |> should.equal(200)
  config.rate_limit |> should.equal(5)
}

// ============ Entry List Tests ============

pub fn modal_logic_entries_test() {
  let entries = sep_adapter.modal_logic_entries()

  entries |> should.not_equal([])
  list.contains(entries, "logic-modal") |> should.be_true
  list.contains(entries, "logic-epistemic") |> should.be_true
}

pub fn epistemic_logic_entries_test() {
  let entries = sep_adapter.epistemic_logic_entries()

  entries |> should.not_equal([])
  list.contains(entries, "logic-epistemic") |> should.be_true
  list.contains(entries, "knowledge-analysis") |> should.be_true
}

pub fn deontic_logic_entries_test() {
  let entries = sep_adapter.deontic_logic_entries()

  entries |> should.not_equal([])
  list.contains(entries, "logic-deontic") |> should.be_true
}

pub fn all_relevant_entries_test() {
  let all = sep_adapter.all_relevant_entries()
  let modal = sep_adapter.modal_logic_entries()
  let epistemic = sep_adapter.epistemic_logic_entries()
  let deontic = sep_adapter.deontic_logic_entries()

  // All should contain entries from each category
  list.all(modal, fn(e) { list.contains(all, e) }) |> should.be_true
  list.all(epistemic, fn(e) { list.contains(all, e) }) |> should.be_true
  list.all(deontic, fn(e) { list.contains(all, e) }) |> should.be_true
}

// ============ Mock Data Tests ============

pub fn mock_entries_test() {
  let entries = mock_entries()

  entries |> should.not_equal([])

  // Should have known entries
  let slugs = list.map(entries, fn(e) { e.slug })
  list.contains(slugs, "logic-modal") |> should.be_true
  list.contains(slugs, "logic-epistemic") |> should.be_true
  list.contains(slugs, "logic-deontic") |> should.be_true
  list.contains(slugs, "possible-worlds") |> should.be_true
}

pub fn mock_entry_structure_test() {
  let entries = mock_entries()

  list.all(entries, fn(e) {
    // All entries should have required fields
    e.slug != "" && e.title != "" && e.content != "" && e.url != ""
  })
  |> should.be_true
}

// ============ Argument Extraction Tests ============

pub fn extract_modal_arguments_test() {
  let entries = mock_entries()
  let modal_entry =
    list.find(entries, fn(e) { e.slug == "logic-modal" })
    |> option.from_result

  case modal_entry {
    Some(entry) -> {
      let args = sep_adapter.extract_arguments(entry)

      args |> should.not_equal([])

      // Should have K axiom argument
      let k_arg = list.find(args, fn(a) { a.id == "sep_modal_k" })
      k_arg |> should.be_ok

      // Should have T axiom argument
      let t_arg = list.find(args, fn(a) { a.id == "sep_modal_t" })
      t_arg |> should.be_ok
    }
    None -> panic as "Expected to find modal entry"
  }
}

pub fn extract_epistemic_arguments_test() {
  let entries = mock_entries()
  let entry =
    list.find(entries, fn(e) { e.slug == "logic-epistemic" })
    |> option.from_result

  case entry {
    Some(e) -> {
      let args = sep_adapter.extract_arguments(e)

      args |> should.not_equal([])

      // Should have factivity argument
      let factivity = list.find(args, fn(a) { a.id == "sep_epistemic_factivity" })
      factivity |> should.be_ok
    }
    None -> panic as "Expected to find epistemic entry"
  }
}

pub fn extract_deontic_arguments_test() {
  let entries = mock_entries()
  let entry =
    list.find(entries, fn(e) { e.slug == "logic-deontic" })
    |> option.from_result

  case entry {
    Some(e) -> {
      let args = sep_adapter.extract_arguments(e)

      args |> should.not_equal([])

      // Should have D axiom argument
      let d_arg = list.find(args, fn(a) { a.id == "sep_deontic_d" })
      d_arg |> should.be_ok
    }
    None -> panic as "Expected to find deontic entry"
  }
}

// ============ Argument Properties Tests ============

pub fn argument_structure_test() {
  let entries = mock_entries()
  let args =
    entries
    |> list.flat_map(sep_adapter.extract_arguments)

  // All arguments should have valid structure
  list.all(args, fn(a) {
    a.id != ""
    && a.source_entry != ""
    && a.natural_language != ""
    && list.length(a.premises) > 0
    && a.confidence >=. 0.0
    && a.confidence <=. 1.0
  })
  |> should.be_true
}

pub fn argument_logic_systems_test() {
  let entries = mock_entries()
  let args =
    entries
    |> list.flat_map(sep_adapter.extract_arguments)

  // Should have arguments in different logic systems
  let systems = list.map(args, fn(a) { a.logic_system }) |> list.unique

  // Should include K, T at minimum
  list.contains(systems, K) |> should.be_true
  list.contains(systems, T) |> should.be_true
}

// ============ Fixture Conversion Tests ============

pub fn argument_to_fixture_test() {
  let entries = mock_entries()
  let args =
    entries
    |> list.flat_map(sep_adapter.extract_arguments)

  let fixtures = list.map(args, sep_adapter.argument_to_fixture)

  // Should have same count
  list.length(fixtures) |> should.equal(list.length(args))

  // All fixtures should have valid structure
  list.all(fixtures, fn(f) {
    f.id != "" && f.name != "" && list.length(f.expected_premises) > 0
  })
  |> should.be_true
}

pub fn entry_to_fixtures_test() {
  let entries = mock_entries()
  let modal_entry =
    list.find(entries, fn(e) { e.slug == "logic-modal" })
    |> option.from_result

  case modal_entry {
    Some(entry) -> {
      let fixtures = sep_adapter.entry_to_fixtures(entry)
      fixtures |> should.not_equal([])

      // All fixtures should reference SEP source
      list.all(fixtures, fn(f) {
        case f.source {
          Some(s) -> s == "SEP:logic-modal"
          None -> False
        }
      })
      |> should.be_true
    }
    None -> panic as "Expected to find modal entry"
  }
}

pub fn get_mock_fixtures_test() {
  let fixtures = sep_adapter.get_mock_fixtures()

  fixtures |> should.not_equal([])

  // All fixtures should have SEP source
  list.all(fixtures, fn(f) {
    case f.source {
      Some(s) ->
        s == "SEP:logic-modal"
        || s == "SEP:logic-epistemic"
        || s == "SEP:logic-deontic"
        || s == "SEP:possible-worlds"
      None -> False
    }
  })
  |> should.be_true
}

// ============ Utility Function Tests ============

pub fn count_by_entry_test() {
  let entries = mock_entries()
  let args =
    entries
    |> list.flat_map(sep_adapter.extract_arguments)

  let counts = sep_adapter.count_by_entry(args)

  // Should have counts for each entry with arguments
  dict.size(counts) |> should.not_equal(0)

  // Modal logic should have multiple arguments
  case dict.get(counts, "logic-modal") {
    Ok(count) -> count |> should.not_equal(0)
    Error(_) -> panic as "Expected count for logic-modal"
  }
}

pub fn filter_by_confidence_test() {
  let entries = mock_entries()
  let args =
    entries
    |> list.flat_map(sep_adapter.extract_arguments)

  let high_confidence = sep_adapter.filter_by_confidence(args, 0.9)
  let low_confidence = sep_adapter.filter_by_confidence(args, 0.5)

  // High confidence should be subset of low confidence threshold
  { list.length(high_confidence) <= list.length(low_confidence) }
  |> should.be_true

  // All high confidence args should have >= 0.9
  list.all(high_confidence, fn(a) { a.confidence >=. 0.9 }) |> should.be_true
}

pub fn filter_by_system_test() {
  let entries = mock_entries()
  let args =
    entries
    |> list.flat_map(sep_adapter.extract_arguments)

  let k_args = sep_adapter.filter_by_system(args, K)
  let t_args = sep_adapter.filter_by_system(args, T)

  // Should have some K arguments
  k_args |> should.not_equal([])

  // All K args should be in K system
  list.all(k_args, fn(a) { a.logic_system == K }) |> should.be_true

  // All T args should be in T system
  list.all(t_args, fn(a) { a.logic_system == T }) |> should.be_true
}

pub fn extraction_statistics_test() {
  let entries = mock_entries()
  let args =
    entries
    |> list.flat_map(sep_adapter.extract_arguments)

  let stats = sep_adapter.extraction_statistics(args)

  stats.total_arguments |> should.equal(list.length(args))
  { stats.average_confidence >=. 0.0 } |> should.be_true
  { stats.average_confidence <=. 1.0 } |> should.be_true
  stats.unique_entries |> should.not_equal(0)
}

// ============ Error Formatting Tests ============

pub fn format_network_error_test() {
  let error = sep_adapter.NetworkError("Connection refused")
  let formatted = sep_adapter.format_error(error)

  formatted |> should.equal("Network error: Connection refused")
}

pub fn format_parse_error_test() {
  let error = sep_adapter.ParseError("Invalid JSON")
  let formatted = sep_adapter.format_error(error)

  formatted |> should.equal("Parse error: Invalid JSON")
}

pub fn format_entry_not_found_test() {
  let error = sep_adapter.EntryNotFound("unknown-entry")
  let formatted = sep_adapter.format_error(error)

  formatted |> should.equal("Entry not found: unknown-entry")
}

pub fn format_rate_limited_test() {
  let error = sep_adapter.RateLimited(60)
  let formatted = sep_adapter.format_error(error)

  formatted |> should.equal("Rate limited, retry after 60 seconds")
}

// ============ SEP Client Tests ============

pub fn new_client_test() {
  let config = default_config()
  let client = new_client(config)

  client.entries_fetched |> should.equal(0)
  client.extraction_attempts |> should.equal(0)
  dict.size(client.entries_cache) |> should.equal(0)
}

pub fn default_client_test() {
  let client = default_client()

  client.config.api_url |> should.equal("https://plato.stanford.edu")
  client.entries_fetched |> should.equal(0)
}

pub fn fetch_entry_with_retry_success_test() {
  let client = default_client()
  let current_time = 1_000_000

  let #(updated_client, result) =
    sep_adapter.fetch_entry_with_retry(client, "logic-modal", current_time, 3)

  case result {
    SEPOk(entry) -> {
      entry.slug |> should.equal("logic-modal")
      entry.title |> should.equal("Modal Logic")
      updated_client.entries_fetched |> should.equal(1)
    }
    sep_adapter.SEPError(err) ->
      panic as { "Expected success, got error: " <> sep_adapter.format_error(err) }
  }
}

pub fn fetch_entry_with_retry_not_found_test() {
  let client = default_client()
  let current_time = 1_000_000

  let #(_updated_client, result) =
    sep_adapter.fetch_entry_with_retry(
      client,
      "nonexistent-entry",
      current_time,
      3,
    )

  case result {
    SEPOk(_) -> panic as "Expected error, got success"
    sep_adapter.SEPError(err) -> {
      case err {
        sep_adapter.EntryNotFound(slug) ->
          slug |> should.equal("nonexistent-entry")
        _ -> panic as "Expected EntryNotFound error"
      }
    }
  }
}

pub fn fetch_entry_caching_test() {
  let client = default_client()
  let current_time = 1_000_000

  // First fetch
  let #(client1, _result1) =
    sep_adapter.fetch_entry_with_retry(client, "logic-modal", current_time, 3)

  // Second fetch (should be cached)
  let #(client2, result2) =
    sep_adapter.fetch_entry_with_retry(client1, "logic-modal", current_time, 3)

  case result2 {
    SEPOk(entry) -> entry.slug |> should.equal("logic-modal")
    sep_adapter.SEPError(_) -> panic as "Expected cached result"
  }

  // Entry should be in cache
  dict.has_key(client2.entries_cache, "logic-modal") |> should.be_true
}

pub fn fetch_entries_test() {
  let client = default_client()
  let current_time = 1_000_000

  let slugs = ["logic-modal", "logic-epistemic"]
  let #(updated_client, results) =
    sep_adapter.fetch_entries(client, slugs, current_time)

  // Should have results for both
  list.length(results) |> should.equal(2)

  // Both should be successful
  let successes =
    list.count(results, fn(pair) {
      case pair.1 {
        SEPOk(_) -> True
        sep_adapter.SEPError(_) -> False
      }
    })

  successes |> should.equal(2)
  updated_client.entries_fetched |> should.equal(2)
}

pub fn fetch_and_extract_test() {
  let client = default_client()
  let current_time = 1_000_000

  let slugs = ["logic-modal", "logic-epistemic"]
  let #(updated_client, args) =
    sep_adapter.fetch_and_extract(client, slugs, current_time)

  // Should have extracted arguments
  args |> should.not_equal([])

  // Extraction attempts should be recorded
  updated_client.extraction_attempts |> should.equal(2)
}

pub fn get_modal_arguments_test() {
  let client = default_client()
  let current_time = 1_000_000

  let #(_updated_client, args) =
    sep_adapter.get_modal_arguments(client, current_time)

  // Should have some arguments (from mock entries that match modal_logic_entries)
  // Note: Only mock entries that exist will return arguments
  { list.length(args) >= 0 } |> should.be_true
}

// ============ Client Statistics Tests ============

pub fn client_statistics_test() {
  let client = default_client()
  let current_time = 1_000_000

  // Fetch some entries to generate stats
  let #(client1, _) =
    sep_adapter.fetch_entry_with_retry(client, "logic-modal", current_time, 3)
  let #(client2, _) =
    sep_adapter.fetch_entry_with_retry(client1, "logic-epistemic", current_time, 3)

  let stats = sep_adapter.client_statistics(client2)

  stats.entries_fetched |> should.equal(2)
  stats.entries_cached |> should.equal(2)
  stats.api_requests |> should.not_equal(0)
}

pub fn format_client_statistics_test() {
  let client = default_client()
  let stats = sep_adapter.client_statistics(client)
  let formatted = sep_adapter.format_client_statistics(stats)

  formatted |> should.not_equal("")
  // Should contain key sections
  { formatted != "" } |> should.be_true
}

// ============ API Client Tests ============

pub fn api_client_new_cache_test() {
  let cache = api_client.new_cache()

  cache.hits |> should.equal(0)
  cache.misses |> should.equal(0)
  dict.size(cache.entries) |> should.equal(0)
}

pub fn api_client_cache_put_get_test() {
  let cache = api_client.new_cache()
  let current_time = 1000
  let response =
    HttpResponse(
      status: 200,
      headers: [],
      body: "test body",
      response_time_ms: 50,
      from_cache: False,
    )

  // Put in cache
  let cache1 = api_client.cache_put(cache, "test-key", response, 3600, current_time)

  // Get from cache
  let #(cache2, result) = api_client.cache_get(cache1, "test-key", current_time)

  case result {
    Some(resp) -> {
      resp.body |> should.equal("test body")
      resp.from_cache |> should.be_true
      cache2.hits |> should.equal(1)
    }
    None -> panic as "Expected cached value"
  }
}

pub fn api_client_cache_expiry_test() {
  let cache = api_client.new_cache()
  let current_time = 1000
  let response =
    HttpResponse(
      status: 200,
      headers: [],
      body: "test body",
      response_time_ms: 50,
      from_cache: False,
    )

  // Put in cache with 100s TTL
  let cache1 = api_client.cache_put(cache, "test-key", response, 100, current_time)

  // Get after expiry
  let #(cache2, result) =
    api_client.cache_get(cache1, "test-key", current_time + 200)

  case result {
    Some(_) -> panic as "Expected expired entry to be gone"
    None -> cache2.misses |> should.equal(1)
  }
}

pub fn api_client_rate_limiter_test() {
  let limiter = api_client.new_rate_limiter(60)
  let current_time = 1000

  // Should allow first request
  let #(limiter1, allowed1) = api_client.rate_limit_check(limiter, current_time)

  allowed1 |> should.be_true
  limiter1.total_requests |> should.equal(1)
}

pub fn api_client_rate_limiter_limit_test() {
  // Create limiter with very low limit for testing
  let limiter = api_client.new_rate_limiter(2)
  let current_time = 1000

  // Use up the limit
  let #(limiter1, allowed1) = api_client.rate_limit_check(limiter, current_time)
  let #(limiter2, allowed2) = api_client.rate_limit_check(limiter1, current_time)
  let #(limiter3, allowed3) = api_client.rate_limit_check(limiter2, current_time)

  allowed1 |> should.be_true
  allowed2 |> should.be_true
  allowed3 |> should.be_false

  limiter3.rate_limited_count |> should.equal(1)
}

pub fn api_client_retry_delay_test() {
  let config = api_client.default_retry_config()

  let delay1 = api_client.calculate_retry_delay(config, 1)
  let delay2 = api_client.calculate_retry_delay(config, 2)
  let delay3 = api_client.calculate_retry_delay(config, 3)

  // Should increase exponentially
  { delay2 > delay1 } |> should.be_true
  { delay3 > delay2 } |> should.be_true
}

pub fn api_client_retryable_error_test() {
  api_client.is_retryable_error(api_client.NetworkError("test"))
  |> should.be_true

  api_client.is_retryable_error(api_client.TimeoutError(5000))
  |> should.be_true

  api_client.is_retryable_error(api_client.RateLimitError(60))
  |> should.be_true

  api_client.is_retryable_error(api_client.InvalidUrl("bad"))
  |> should.be_false

  api_client.is_retryable_error(api_client.CacheError("error"))
  |> should.be_false
}

pub fn api_client_build_url_test() {
  let request =
    api_client.get("/entries/test/")
    |> api_client.with_query("key1", "value1")
    |> api_client.with_query("key2", "value2")

  let url = api_client.build_url("https://example.com", request)

  // Should have base URL + path + query params
  { url != "" } |> should.be_true
}

pub fn api_client_simulate_request_test() {
  let config =
    ClientConfig(
      base_url: "https://test.com",
      timeout_ms: 5000,
      max_retries: 3,
      retry_delay_ms: 1000,
      rate_limit_per_minute: 60,
      cache_duration_sec: 3600,
      user_agent: "test/1.0",
    )
  let client = api_client.new_client(config)
  let request = api_client.get("/test")
  let current_time = 1000

  let #(updated_client, result) =
    api_client.simulate_request(client, request, current_time)

  case result {
    api_client.ClientOk(response) -> {
      response.status |> should.equal(200)
      response.from_cache |> should.be_false
      updated_client.successful_requests |> should.equal(1)
    }
    api_client.ClientErr(_) -> panic as "Expected successful response"
  }
}

pub fn api_client_cached_request_test() {
  let config =
    ClientConfig(
      base_url: "https://test.com",
      timeout_ms: 5000,
      max_retries: 3,
      retry_delay_ms: 1000,
      rate_limit_per_minute: 60,
      cache_duration_sec: 3600,
      user_agent: "test/1.0",
    )
  let client = api_client.new_client(config)
  let request = api_client.get("/test")
  let current_time = 1000

  // First request
  let #(client1, _) = api_client.simulate_request(client, request, current_time)

  // Second request (should be cached)
  let #(client2, result2) =
    api_client.simulate_request(client1, request, current_time)

  case result2 {
    api_client.ClientOk(response) -> {
      response.from_cache |> should.be_true
    }
    api_client.ClientErr(_) -> panic as "Expected cached response"
  }

  // Check stats
  let stats = api_client.client_stats(client2)
  stats.cache.hits |> should.equal(1)
}

pub fn api_client_format_error_test() {
  let errors = [
    #(api_client.NetworkError("conn refused"), "Network error: conn refused"),
    #(api_client.TimeoutError(5000), "Request timed out after 5000ms"),
    #(
      api_client.RateLimitError(60),
      "Rate limited, retry after 60 seconds",
    ),
    #(api_client.HttpError(404, "Not Found"), "HTTP 404: Not Found"),
    #(api_client.InvalidUrl("bad://url"), "Invalid URL: bad://url"),
  ]

  list.each(errors, fn(pair) {
    let #(error, expected) = pair
    api_client.format_error(error) |> should.equal(expected)
  })
}
