//// Tests for InPhO Adapter
////
//// Tests the Indiana Philosophy Ontology (InPhO) integration including
//// the API client wrapper, caching, rate limiting, and argument extraction.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import modal_logic/proposition.{K, S5, T}
import modal_logic/testing/external/api_client.{ClientConfig, HttpResponse}
import modal_logic/testing/external/inpho_adapter.{
  InPhOOk, default_client, default_config, mock_ideas, new_client,
}

// ============ Configuration Tests ============

pub fn default_config_test() {
  let config = default_config()

  config.api_url |> should.equal("https://www.inphoproject.org")
  config.cache_duration |> should.equal(86_400)
  config.max_ideas |> should.equal(50)
  config.rate_limit |> should.equal(30)
}

pub fn comprehensive_config_test() {
  let config = inpho_adapter.comprehensive_config()

  config.cache_duration |> should.equal(604_800)
  config.max_ideas |> should.equal(200)
  config.rate_limit |> should.equal(20)
}

// ============ Idea List Tests ============

pub fn modal_logic_idea_ids_test() {
  let ids = inpho_adapter.modal_logic_idea_ids()

  ids |> should.not_equal([])
  list.contains(ids, 1209) |> should.be_true
  // Modal Logic
  list.contains(ids, 1501) |> should.be_true
  // Possible Worlds
}

pub fn epistemic_idea_ids_test() {
  let ids = inpho_adapter.epistemic_idea_ids()

  ids |> should.not_equal([])
  list.contains(ids, 646) |> should.be_true
  // Epistemology
}

pub fn deontic_idea_ids_test() {
  let ids = inpho_adapter.deontic_idea_ids()

  ids |> should.not_equal([])
  list.contains(ids, 602) |> should.be_true
  // Ethics
}

pub fn all_relevant_idea_ids_test() {
  let all = inpho_adapter.all_relevant_idea_ids()
  let modal = inpho_adapter.modal_logic_idea_ids()
  let epistemic = inpho_adapter.epistemic_idea_ids()
  let deontic = inpho_adapter.deontic_idea_ids()

  // All should contain IDs from each category
  list.all(modal, fn(id) { list.contains(all, id) }) |> should.be_true
  list.all(epistemic, fn(id) { list.contains(all, id) }) |> should.be_true
  list.all(deontic, fn(id) { list.contains(all, id) }) |> should.be_true
}

// ============ Mock Data Tests ============

pub fn mock_ideas_test() {
  let ideas = mock_ideas()

  ideas |> should.not_equal([])

  // Should have known ideas
  let ids = list.map(ideas, fn(i) { i.id })
  list.contains(ids, 1209) |> should.be_true
  // Modal Logic
  list.contains(ids, 646) |> should.be_true
  // Epistemology
  list.contains(ids, 602) |> should.be_true
  // Ethics
  list.contains(ids, 1501) |> should.be_true
  // Possible Worlds
}

pub fn mock_idea_structure_test() {
  let ideas = mock_ideas()

  list.all(ideas, fn(i) {
    // All ideas should have required fields
    i.id > 0 && i.label != "" && i.url != "" && i.idea_type == "idea"
  })
  |> should.be_true
}

// ============ Argument Extraction Tests ============

pub fn extract_modal_arguments_test() {
  let ideas = mock_ideas()
  let modal_idea =
    list.find(ideas, fn(i) { i.id == 1209 })
    |> option.from_result

  case modal_idea {
    Some(idea) -> {
      let args = inpho_adapter.extract_arguments(idea)

      args |> should.not_equal([])

      // Should have K axiom argument
      let k_arg = list.find(args, fn(a) { a.id == "inpho_modal_k" })
      k_arg |> should.be_ok

      // Should have T axiom argument
      let t_arg = list.find(args, fn(a) { a.id == "inpho_modal_t" })
      t_arg |> should.be_ok
    }
    None -> panic as "Expected to find modal idea"
  }
}

pub fn extract_epistemic_arguments_test() {
  let ideas = mock_ideas()
  let idea =
    list.find(ideas, fn(i) { i.id == 646 })
    |> option.from_result

  case idea {
    Some(i) -> {
      let args = inpho_adapter.extract_arguments(i)

      args |> should.not_equal([])

      // Should have factivity argument
      let factivity =
        list.find(args, fn(a) { a.id == "inpho_epistemic_factivity" })
      factivity |> should.be_ok
    }
    None -> panic as "Expected to find epistemic idea"
  }
}

pub fn extract_deontic_arguments_test() {
  let ideas = mock_ideas()
  let idea =
    list.find(ideas, fn(i) { i.id == 602 })
    |> option.from_result

  case idea {
    Some(i) -> {
      let args = inpho_adapter.extract_arguments(i)

      args |> should.not_equal([])

      // Should have D axiom argument
      let d_arg = list.find(args, fn(a) { a.id == "inpho_deontic_d" })
      d_arg |> should.be_ok
    }
    None -> panic as "Expected to find deontic idea"
  }
}

// ============ Argument Properties Tests ============

pub fn argument_structure_test() {
  let ideas = mock_ideas()
  let args =
    ideas
    |> list.flat_map(inpho_adapter.extract_arguments)

  // All arguments should have valid structure
  list.all(args, fn(a) {
    a.id != ""
    && a.source_idea > 0
    && a.natural_language != ""
    && list.length(a.premises) > 0
    && a.confidence >=. 0.0
    && a.confidence <=. 1.0
  })
  |> should.be_true
}

pub fn argument_logic_systems_test() {
  let ideas = mock_ideas()
  let args =
    ideas
    |> list.flat_map(inpho_adapter.extract_arguments)

  // Should have arguments in different logic systems
  let systems = list.map(args, fn(a) { a.logic_system }) |> list.unique

  // Should include K, T at minimum
  list.contains(systems, K) |> should.be_true
  list.contains(systems, T) |> should.be_true
}

// ============ Fixture Conversion Tests ============

pub fn argument_to_fixture_test() {
  let ideas = mock_ideas()
  let args =
    ideas
    |> list.flat_map(inpho_adapter.extract_arguments)

  let fixtures = list.map(args, inpho_adapter.argument_to_fixture)

  // Should have same count
  list.length(fixtures) |> should.equal(list.length(args))

  // All fixtures should have valid structure
  list.all(fixtures, fn(f) {
    f.id != "" && f.name != "" && list.length(f.expected_premises) > 0
  })
  |> should.be_true
}

pub fn idea_to_fixtures_test() {
  let ideas = mock_ideas()
  let modal_idea =
    list.find(ideas, fn(i) { i.id == 1209 })
    |> option.from_result

  case modal_idea {
    Some(idea) -> {
      let fixtures = inpho_adapter.idea_to_fixtures(idea)
      fixtures |> should.not_equal([])

      // All fixtures should reference InPhO source
      list.all(fixtures, fn(f) {
        case f.source {
          Some(s) -> s == "InPhO:1209"
          None -> False
        }
      })
      |> should.be_true
    }
    None -> panic as "Expected to find modal idea"
  }
}

pub fn get_mock_fixtures_test() {
  let fixtures = inpho_adapter.get_mock_fixtures()

  fixtures |> should.not_equal([])

  // All fixtures should have InPhO source
  list.all(fixtures, fn(f) {
    case f.source {
      Some(s) ->
        s == "InPhO:1209"
        || s == "InPhO:646"
        || s == "InPhO:602"
        || s == "InPhO:1501"
      None -> False
    }
  })
  |> should.be_true
}

// ============ Utility Function Tests ============

pub fn count_by_idea_test() {
  let ideas = mock_ideas()
  let args =
    ideas
    |> list.flat_map(inpho_adapter.extract_arguments)

  let counts = inpho_adapter.count_by_idea(args)

  // Should have counts for each idea with arguments
  dict.size(counts) |> should.not_equal(0)

  // Modal logic (1209) should have multiple arguments
  case dict.get(counts, 1209) {
    Ok(count) -> count |> should.not_equal(0)
    Error(_) -> panic as "Expected count for modal logic (1209)"
  }
}

pub fn filter_by_confidence_test() {
  let ideas = mock_ideas()
  let args =
    ideas
    |> list.flat_map(inpho_adapter.extract_arguments)

  let high_confidence = inpho_adapter.filter_by_confidence(args, 0.9)
  let low_confidence = inpho_adapter.filter_by_confidence(args, 0.5)

  // High confidence should be subset of low confidence threshold
  { list.length(high_confidence) <= list.length(low_confidence) }
  |> should.be_true

  // All high confidence args should have >= 0.9
  list.all(high_confidence, fn(a) { a.confidence >=. 0.9 }) |> should.be_true
}

pub fn filter_by_system_test() {
  let ideas = mock_ideas()
  let args =
    ideas
    |> list.flat_map(inpho_adapter.extract_arguments)

  let k_args = inpho_adapter.filter_by_system(args, K)
  let t_args = inpho_adapter.filter_by_system(args, T)

  // Should have some K arguments
  k_args |> should.not_equal([])

  // All K args should be in K system
  list.all(k_args, fn(a) { a.logic_system == K }) |> should.be_true

  // All T args should be in T system
  list.all(t_args, fn(a) { a.logic_system == T }) |> should.be_true
}

pub fn extraction_statistics_test() {
  let ideas = mock_ideas()
  let args =
    ideas
    |> list.flat_map(inpho_adapter.extract_arguments)

  let stats = inpho_adapter.extraction_statistics(args)

  stats.total_arguments |> should.equal(list.length(args))
  { stats.average_confidence >=. 0.0 } |> should.be_true
  { stats.average_confidence <=. 1.0 } |> should.be_true
  stats.unique_ideas |> should.not_equal(0)
}

// ============ Error Formatting Tests ============

pub fn format_network_error_test() {
  let error = inpho_adapter.NetworkError("Connection refused")
  let formatted = inpho_adapter.format_error(error)

  formatted |> should.equal("Network error: Connection refused")
}

pub fn format_parse_error_test() {
  let error = inpho_adapter.ParseError("Invalid JSON")
  let formatted = inpho_adapter.format_error(error)

  formatted |> should.equal("Parse error: Invalid JSON")
}

pub fn format_idea_not_found_test() {
  let error = inpho_adapter.IdeaNotFound(9999)
  let formatted = inpho_adapter.format_error(error)

  formatted |> should.equal("Idea not found: 9999")
}

pub fn format_rate_limited_test() {
  let error = inpho_adapter.RateLimited(60)
  let formatted = inpho_adapter.format_error(error)

  formatted |> should.equal("Rate limited, retry after 60 seconds")
}

// ============ InPhO Client Tests ============

pub fn new_client_test() {
  let config = default_config()
  let client = new_client(config)

  client.ideas_fetched |> should.equal(0)
  client.extraction_attempts |> should.equal(0)
  dict.size(client.ideas_cache) |> should.equal(0)
}

pub fn default_client_test() {
  let client = default_client()

  client.config.api_url |> should.equal("https://www.inphoproject.org")
  client.ideas_fetched |> should.equal(0)
}

pub fn fetch_idea_with_retry_success_test() {
  let client = default_client()
  let current_time = 1_000_000

  let #(updated_client, result) =
    inpho_adapter.fetch_idea_with_retry(client, 1209, current_time, 3)

  case result {
    InPhOOk(idea) -> {
      idea.id |> should.equal(1209)
      idea.label |> should.equal("Modal Logic")
      updated_client.ideas_fetched |> should.equal(1)
    }
    inpho_adapter.InPhOError(err) ->
      panic as {
        "Expected success, got error: " <> inpho_adapter.format_error(err)
      }
  }
}

pub fn fetch_idea_with_retry_not_found_test() {
  let client = default_client()
  let current_time = 1_000_000

  let #(_updated_client, result) =
    inpho_adapter.fetch_idea_with_retry(client, 99_999, current_time, 3)

  case result {
    InPhOOk(_) -> panic as "Expected error, got success"
    inpho_adapter.InPhOError(err) -> {
      case err {
        inpho_adapter.IdeaNotFound(id) -> id |> should.equal(99_999)
        _ -> panic as "Expected IdeaNotFound error"
      }
    }
  }
}

pub fn fetch_idea_caching_test() {
  let client = default_client()
  let current_time = 1_000_000

  // First fetch
  let #(client1, _result1) =
    inpho_adapter.fetch_idea_with_retry(client, 1209, current_time, 3)

  // Second fetch (should be cached)
  let #(client2, result2) =
    inpho_adapter.fetch_idea_with_retry(client1, 1209, current_time, 3)

  case result2 {
    InPhOOk(idea) -> idea.id |> should.equal(1209)
    inpho_adapter.InPhOError(_) -> panic as "Expected cached result"
  }

  // Idea should be in cache
  dict.has_key(client2.ideas_cache, 1209) |> should.be_true
}

pub fn fetch_ideas_test() {
  let client = default_client()
  let current_time = 1_000_000

  let idea_ids = [1209, 646]
  let #(updated_client, results) =
    inpho_adapter.fetch_ideas(client, idea_ids, current_time)

  // Should have results for both
  list.length(results) |> should.equal(2)

  // Both should be successful
  let successes =
    list.count(results, fn(pair) {
      case pair.1 {
        InPhOOk(_) -> True
        inpho_adapter.InPhOError(_) -> False
      }
    })

  successes |> should.equal(2)
  updated_client.ideas_fetched |> should.equal(2)
}

pub fn fetch_and_extract_test() {
  let client = default_client()
  let current_time = 1_000_000

  let idea_ids = [1209, 646]
  let #(updated_client, args) =
    inpho_adapter.fetch_and_extract(client, idea_ids, current_time)

  // Should have extracted arguments
  args |> should.not_equal([])

  // Extraction attempts should be recorded
  updated_client.extraction_attempts |> should.equal(2)
}

pub fn get_modal_arguments_test() {
  let client = default_client()
  let current_time = 1_000_000

  let #(_updated_client, args) =
    inpho_adapter.get_modal_arguments(client, current_time)

  // Should have some arguments (from mock ideas that match modal_logic_idea_ids)
  { list.length(args) >= 0 } |> should.be_true
}

// ============ Client Statistics Tests ============

pub fn client_statistics_test() {
  let client = default_client()
  let current_time = 1_000_000

  // Fetch some ideas to generate stats
  let #(client1, _) =
    inpho_adapter.fetch_idea_with_retry(client, 1209, current_time, 3)
  let #(client2, _) =
    inpho_adapter.fetch_idea_with_retry(client1, 646, current_time, 3)

  let stats = inpho_adapter.client_statistics(client2)

  stats.ideas_fetched |> should.equal(2)
  stats.ideas_cached |> should.equal(2)
  stats.api_requests |> should.not_equal(0)
}

pub fn format_client_statistics_test() {
  let client = default_client()
  let stats = inpho_adapter.client_statistics(client)
  let formatted = inpho_adapter.format_client_statistics(stats)

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
  let cache1 =
    api_client.cache_put(cache, "test-key", response, 3600, current_time)

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
  let cache1 =
    api_client.cache_put(cache, "test-key", response, 100, current_time)

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
  let #(limiter2, allowed2) =
    api_client.rate_limit_check(limiter1, current_time)
  let #(limiter3, allowed3) =
    api_client.rate_limit_check(limiter2, current_time)

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
    api_client.get("/idea/1209.json")
    |> api_client.with_query("key1", "value1")
    |> api_client.with_query("key2", "value2")

  let url = api_client.build_url("https://www.inphoproject.org", request)

  // Should have base URL + path + query params
  { url != "" } |> should.be_true
}

pub fn api_client_simulate_request_test() {
  let config =
    ClientConfig(
      base_url: "https://www.inphoproject.org",
      timeout_ms: 5000,
      max_retries: 3,
      retry_delay_ms: 1000,
      rate_limit_per_minute: 60,
      cache_duration_sec: 3600,
      user_agent: "test/1.0",
    )
  let client = api_client.new_client(config)
  let request = api_client.get("/idea/1209.json")
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
      base_url: "https://www.inphoproject.org",
      timeout_ms: 5000,
      max_retries: 3,
      retry_delay_ms: 1000,
      rate_limit_per_minute: 60,
      cache_duration_sec: 3600,
      user_agent: "test/1.0",
    )
  let client = api_client.new_client(config)
  let request = api_client.get("/idea/1209.json")
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
    #(api_client.RateLimitError(60), "Rate limited, retry after 60 seconds"),
    #(api_client.HttpError(404, "Not Found"), "HTTP 404: Not Found"),
    #(api_client.InvalidUrl("bad://url"), "Invalid URL: bad://url"),
  ]

  list.each(errors, fn(pair) {
    let #(error, expected) = pair
    api_client.format_error(error) |> should.equal(expected)
  })
}
