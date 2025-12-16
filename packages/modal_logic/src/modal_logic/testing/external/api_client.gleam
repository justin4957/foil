//// API Client
////
//// This module provides a generic HTTP client wrapper with caching,
//// rate limiting, and retry logic for external API access.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============ Core Types ============

/// HTTP client configuration
pub type ClientConfig {
  ClientConfig(
    /// Base URL for the API
    base_url: String,
    /// Request timeout in milliseconds
    timeout_ms: Int,
    /// Maximum retries on failure
    max_retries: Int,
    /// Initial retry delay in milliseconds
    retry_delay_ms: Int,
    /// Rate limit (requests per minute)
    rate_limit_per_minute: Int,
    /// Cache duration in seconds
    cache_duration_sec: Int,
    /// User agent string
    user_agent: String,
  )
}

/// HTTP request
pub type HttpRequest {
  HttpRequest(
    /// Request method
    method: HttpMethod,
    /// URL path (appended to base_url)
    path: String,
    /// Query parameters
    query_params: List(#(String, String)),
    /// Request headers
    headers: List(#(String, String)),
    /// Request body (for POST/PUT)
    body: Option(String),
  )
}

/// HTTP method
pub type HttpMethod {
  Get
  Post
  Put
  Delete
  Head
}

/// HTTP response
pub type HttpResponse {
  HttpResponse(
    /// Status code
    status: Int,
    /// Response headers
    headers: List(#(String, String)),
    /// Response body
    body: String,
    /// Response time in milliseconds
    response_time_ms: Int,
    /// Whether response came from cache
    from_cache: Bool,
  )
}

/// Client error types
pub type ClientError {
  /// Network error occurred
  NetworkError(message: String)
  /// Request timed out
  TimeoutError(timeout_ms: Int)
  /// Rate limit exceeded
  RateLimitError(retry_after_sec: Int)
  /// HTTP error response
  HttpError(status: Int, message: String)
  /// Cache error
  CacheError(message: String)
  /// Invalid URL
  InvalidUrl(url: String)
  /// Retries exhausted
  RetriesExhausted(attempts: Int, last_error: String)
}

/// Client result type
pub type ClientResult(a) {
  ClientOk(a)
  ClientErr(ClientError)
}

// ============ Cache Types ============

/// Cache entry
pub type CacheEntry {
  CacheEntry(
    /// Cached response
    response: HttpResponse,
    /// Timestamp when cached (Unix timestamp)
    cached_at: Int,
    /// Time-to-live in seconds
    ttl_sec: Int,
  )
}

/// Cache state (in-memory cache)
pub type CacheState {
  CacheState(
    /// Cached responses by URL
    entries: Dict(String, CacheEntry),
    /// Total cache hits
    hits: Int,
    /// Total cache misses
    misses: Int,
  )
}

/// Initialize empty cache
pub fn new_cache() -> CacheState {
  CacheState(entries: dict.new(), hits: 0, misses: 0)
}

/// Check if cache entry is valid
pub fn is_cache_valid(entry: CacheEntry, current_time: Int) -> Bool {
  current_time - entry.cached_at < entry.ttl_sec
}

/// Get from cache if valid
pub fn cache_get(
  cache: CacheState,
  key: String,
  current_time: Int,
) -> #(CacheState, Option(HttpResponse)) {
  case dict.get(cache.entries, key) {
    Ok(entry) -> {
      case is_cache_valid(entry, current_time) {
        True -> {
          let new_cache = CacheState(..cache, hits: cache.hits + 1)
          #(new_cache, Some(HttpResponse(..entry.response, from_cache: True)))
        }
        False -> {
          // Expired entry
          let new_entries = dict.delete(cache.entries, key)
          let new_cache =
            CacheState(..cache, entries: new_entries, misses: cache.misses + 1)
          #(new_cache, None)
        }
      }
    }
    Error(_) -> {
      let new_cache = CacheState(..cache, misses: cache.misses + 1)
      #(new_cache, None)
    }
  }
}

/// Put response in cache
pub fn cache_put(
  cache: CacheState,
  key: String,
  response: HttpResponse,
  ttl_sec: Int,
  current_time: Int,
) -> CacheState {
  let entry =
    CacheEntry(response: response, cached_at: current_time, ttl_sec: ttl_sec)
  let new_entries = dict.insert(cache.entries, key, entry)
  CacheState(..cache, entries: new_entries)
}

/// Clear expired cache entries
pub fn cache_cleanup(cache: CacheState, current_time: Int) -> CacheState {
  let valid_entries =
    cache.entries
    |> dict.to_list
    |> list.filter(fn(pair) { is_cache_valid(pair.1, current_time) })
    |> dict.from_list

  CacheState(..cache, entries: valid_entries)
}

/// Get cache statistics
pub fn cache_stats(cache: CacheState) -> CacheStatistics {
  let total = cache.hits + cache.misses
  let hit_rate = case total {
    0 -> 0.0
    _ -> int.to_float(cache.hits) /. int.to_float(total)
  }

  CacheStatistics(
    entries: dict.size(cache.entries),
    hits: cache.hits,
    misses: cache.misses,
    hit_rate: hit_rate,
  )
}

/// Cache statistics
pub type CacheStatistics {
  CacheStatistics(entries: Int, hits: Int, misses: Int, hit_rate: Float)
}

// ============ Rate Limiter Types ============

/// Rate limiter state
pub type RateLimiterState {
  RateLimiterState(
    /// Request timestamps (Unix timestamps in seconds)
    request_times: List(Int),
    /// Rate limit (requests per minute)
    limit_per_minute: Int,
    /// Total requests made
    total_requests: Int,
    /// Total requests rate limited
    rate_limited_count: Int,
  )
}

/// Initialize rate limiter
pub fn new_rate_limiter(limit_per_minute: Int) -> RateLimiterState {
  RateLimiterState(
    request_times: [],
    limit_per_minute: limit_per_minute,
    total_requests: 0,
    rate_limited_count: 0,
  )
}

/// Check if request is allowed under rate limit
pub fn rate_limit_check(
  state: RateLimiterState,
  current_time: Int,
) -> #(RateLimiterState, Bool) {
  // Remove requests older than 1 minute
  let window_start = current_time - 60
  let recent_requests =
    list.filter(state.request_times, fn(t) { t > window_start })

  let count = list.length(recent_requests)

  case count < state.limit_per_minute {
    True -> {
      // Allowed - record this request
      let new_state =
        RateLimiterState(
          ..state,
          request_times: [current_time, ..recent_requests],
          total_requests: state.total_requests + 1,
        )
      #(new_state, True)
    }
    False -> {
      // Rate limited
      let new_state =
        RateLimiterState(
          ..state,
          request_times: recent_requests,
          rate_limited_count: state.rate_limited_count + 1,
        )
      #(new_state, False)
    }
  }
}

/// Get time until next request is allowed (in seconds)
pub fn rate_limit_wait_time(state: RateLimiterState, current_time: Int) -> Int {
  let window_start = current_time - 60
  let recent_requests =
    list.filter(state.request_times, fn(t) { t > window_start })

  case list.length(recent_requests) >= state.limit_per_minute {
    True -> {
      // Find oldest request and calculate wait time
      case list.last(recent_requests) {
        Ok(oldest) -> oldest + 60 - current_time + 1
        Error(_) -> 1
      }
    }
    False -> 0
  }
}

/// Get rate limiter statistics
pub fn rate_limiter_stats(state: RateLimiterState) -> RateLimiterStatistics {
  RateLimiterStatistics(
    total_requests: state.total_requests,
    rate_limited: state.rate_limited_count,
    current_window_count: list.length(state.request_times),
    limit_per_minute: state.limit_per_minute,
  )
}

/// Rate limiter statistics
pub type RateLimiterStatistics {
  RateLimiterStatistics(
    total_requests: Int,
    rate_limited: Int,
    current_window_count: Int,
    limit_per_minute: Int,
  )
}

// ============ Retry Logic ============

/// Retry configuration
pub type RetryConfig {
  RetryConfig(
    /// Maximum number of retries
    max_retries: Int,
    /// Initial delay in milliseconds
    initial_delay_ms: Int,
    /// Backoff multiplier
    backoff_multiplier: Float,
    /// Maximum delay in milliseconds
    max_delay_ms: Int,
    /// Status codes to retry on
    retryable_statuses: List(Int),
  )
}

/// Default retry configuration
pub fn default_retry_config() -> RetryConfig {
  RetryConfig(
    max_retries: 3,
    initial_delay_ms: 1000,
    backoff_multiplier: 2.0,
    max_delay_ms: 30_000,
    retryable_statuses: [408, 429, 500, 502, 503, 504],
  )
}

/// Calculate delay for retry attempt (exponential backoff)
pub fn calculate_retry_delay(config: RetryConfig, attempt: Int) -> Int {
  let delay_float =
    int.to_float(config.initial_delay_ms)
    *. pow(config.backoff_multiplier, int.to_float(attempt - 1))

  let delay = float_to_int(delay_float)

  case delay > config.max_delay_ms {
    True -> config.max_delay_ms
    False -> delay
  }
}

/// Check if status code is retryable
pub fn is_retryable_status(config: RetryConfig, status: Int) -> Bool {
  list.contains(config.retryable_statuses, status)
}

/// Check if error is retryable
pub fn is_retryable_error(error: ClientError) -> Bool {
  case error {
    NetworkError(_) -> True
    TimeoutError(_) -> True
    RateLimitError(_) -> True
    HttpError(status, _) ->
      list.contains([408, 429, 500, 502, 503, 504], status)
    CacheError(_) -> False
    InvalidUrl(_) -> False
    RetriesExhausted(_, _) -> False
  }
}

// ============ Client State ============

/// API client state
pub type ClientState {
  ClientState(
    /// Client configuration
    config: ClientConfig,
    /// Cache state
    cache: CacheState,
    /// Rate limiter state
    rate_limiter: RateLimiterState,
    /// Retry configuration
    retry_config: RetryConfig,
    /// Total successful requests
    successful_requests: Int,
    /// Total failed requests
    failed_requests: Int,
  )
}

/// Initialize new client
pub fn new_client(config: ClientConfig) -> ClientState {
  ClientState(
    config: config,
    cache: new_cache(),
    rate_limiter: new_rate_limiter(config.rate_limit_per_minute),
    retry_config: default_retry_config(),
    successful_requests: 0,
    failed_requests: 0,
  )
}

/// Default client configuration
pub fn default_config() -> ClientConfig {
  ClientConfig(
    base_url: "",
    timeout_ms: 30_000,
    max_retries: 3,
    retry_delay_ms: 1000,
    rate_limit_per_minute: 60,
    cache_duration_sec: 3600,
    user_agent: "modal-logic-tester/1.0",
  )
}

// ============ Request Building ============

/// Create a GET request
pub fn get(path: String) -> HttpRequest {
  HttpRequest(
    method: Get,
    path: path,
    query_params: [],
    headers: [],
    body: None,
  )
}

/// Add query parameter to request
pub fn with_query(
  request: HttpRequest,
  key: String,
  value: String,
) -> HttpRequest {
  HttpRequest(..request, query_params: [#(key, value), ..request.query_params])
}

/// Add header to request
pub fn with_header(
  request: HttpRequest,
  key: String,
  value: String,
) -> HttpRequest {
  HttpRequest(..request, headers: [#(key, value), ..request.headers])
}

/// Build full URL from base and request
pub fn build_url(base_url: String, request: HttpRequest) -> String {
  let path_url = base_url <> request.path

  case request.query_params {
    [] -> path_url
    params -> {
      let query_string =
        params
        |> list.map(fn(p) { p.0 <> "=" <> p.1 })
        |> string.join("&")
      path_url <> "?" <> query_string
    }
  }
}

// ============ Mock/Simulated HTTP ============

/// Simulated HTTP request (for testing without actual network)
/// In production, this would be replaced with actual HTTP calls
pub fn simulate_request(
  client: ClientState,
  request: HttpRequest,
  current_time: Int,
) -> #(ClientState, ClientResult(HttpResponse)) {
  let url = build_url(client.config.base_url, request)

  // Check cache first
  let #(cache_after_get, cached) = cache_get(client.cache, url, current_time)
  let client = ClientState(..client, cache: cache_after_get)

  case cached {
    Some(response) -> #(client, ClientOk(response))
    None -> {
      // Check rate limit
      let #(rate_limiter_after, allowed) =
        rate_limit_check(client.rate_limiter, current_time)
      let client = ClientState(..client, rate_limiter: rate_limiter_after)

      case allowed {
        False -> {
          let wait_time =
            rate_limit_wait_time(client.rate_limiter, current_time)
          let client =
            ClientState(..client, failed_requests: client.failed_requests + 1)
          #(client, ClientErr(RateLimitError(wait_time)))
        }
        True -> {
          // Simulate successful response
          let response =
            HttpResponse(
              status: 200,
              headers: [#("Content-Type", "text/html")],
              body: "Simulated response for " <> url,
              response_time_ms: 100,
              from_cache: False,
            )

          // Cache the response
          let cache_after_put =
            cache_put(
              client.cache,
              url,
              response,
              client.config.cache_duration_sec,
              current_time,
            )

          let client =
            ClientState(
              ..client,
              cache: cache_after_put,
              successful_requests: client.successful_requests + 1,
            )

          #(client, ClientOk(response))
        }
      }
    }
  }
}

// ============ Client Statistics ============

/// Get client statistics
pub fn client_stats(client: ClientState) -> ClientStatistics {
  ClientStatistics(
    successful_requests: client.successful_requests,
    failed_requests: client.failed_requests,
    cache: cache_stats(client.cache),
    rate_limiter: rate_limiter_stats(client.rate_limiter),
  )
}

/// Client statistics
pub type ClientStatistics {
  ClientStatistics(
    successful_requests: Int,
    failed_requests: Int,
    cache: CacheStatistics,
    rate_limiter: RateLimiterStatistics,
  )
}

/// Format client statistics as string
pub fn format_stats(stats: ClientStatistics) -> String {
  string.concat([
    "API Client Statistics\n",
    "=====================\n",
    "Successful Requests: ",
    int.to_string(stats.successful_requests),
    "\n",
    "Failed Requests: ",
    int.to_string(stats.failed_requests),
    "\n",
    "\nCache:\n",
    "  Entries: ",
    int.to_string(stats.cache.entries),
    "\n",
    "  Hits: ",
    int.to_string(stats.cache.hits),
    "\n",
    "  Misses: ",
    int.to_string(stats.cache.misses),
    "\n",
    "  Hit Rate: ",
    float_to_percent(stats.cache.hit_rate),
    "\n",
    "\nRate Limiter:\n",
    "  Total Requests: ",
    int.to_string(stats.rate_limiter.total_requests),
    "\n",
    "  Rate Limited: ",
    int.to_string(stats.rate_limiter.rate_limited),
    "\n",
    "  Current Window: ",
    int.to_string(stats.rate_limiter.current_window_count),
    "/",
    int.to_string(stats.rate_limiter.limit_per_minute),
    "\n",
  ])
}

// ============ Error Formatting ============

/// Format client error as string
pub fn format_error(error: ClientError) -> String {
  case error {
    NetworkError(msg) -> "Network error: " <> msg
    TimeoutError(ms) -> "Request timed out after " <> int.to_string(ms) <> "ms"
    RateLimitError(sec) ->
      "Rate limited, retry after " <> int.to_string(sec) <> " seconds"
    HttpError(status, msg) -> "HTTP " <> int.to_string(status) <> ": " <> msg
    CacheError(msg) -> "Cache error: " <> msg
    InvalidUrl(url) -> "Invalid URL: " <> url
    RetriesExhausted(attempts, last) ->
      "Retries exhausted after "
      <> int.to_string(attempts)
      <> " attempts: "
      <> last
  }
}

// ============ Utility Functions ============

/// Simple power function for floats
fn pow(base: Float, exp: Float) -> Float {
  // Simplified - only works for positive integer exponents
  case exp {
    e if e <=. 0.0 -> 1.0
    e if e <=. 1.0 -> base
    _ -> base *. pow(base, exp -. 1.0)
  }
}

/// Convert float to int (truncate)
fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float_to_int(0.0 -. f)
    False -> {
      // Simple conversion by repeated subtraction
      float_to_int_helper(f, 0)
    }
  }
}

fn float_to_int_helper(f: Float, acc: Int) -> Int {
  case f <. 1.0 {
    True -> acc
    False -> float_to_int_helper(f -. 1.0, acc + 1)
  }
}

/// Format float as percentage string
fn float_to_percent(f: Float) -> String {
  let pct = float_to_int(f *. 100.0)
  int.to_string(pct) <> "%"
}
