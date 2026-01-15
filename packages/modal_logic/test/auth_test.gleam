import gleam/dict
import gleam/list
import gleam/option
import gleam/string
import gleeunit
import gleeunit/should
import modal_logic/auth

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Rate Limit Tier Tests
// =============================================================================

pub fn free_tier_test() {
  let tier = auth.free_tier()

  auth.tier_name(tier) |> should.equal("free")
  auth.tier_requests_per_minute(tier) |> should.equal(100)

  case auth.tier_monthly_quota(tier) {
    option.None -> should.be_true(True)
    option.Some(_) -> should.fail()
  }
}

pub fn basic_tier_test() {
  let tier = auth.basic_tier()

  auth.tier_name(tier) |> should.equal("basic")
  auth.tier_requests_per_minute(tier) |> should.equal(500)

  case auth.tier_monthly_quota(tier) {
    option.Some(quota) -> quota |> should.equal(100_000)
    option.None -> should.fail()
  }
}

pub fn pro_tier_test() {
  let tier = auth.pro_tier()

  auth.tier_name(tier) |> should.equal("pro")
  auth.tier_requests_per_minute(tier) |> should.equal(1000)

  case auth.tier_monthly_quota(tier) {
    option.Some(quota) -> quota |> should.equal(1_000_000)
    option.None -> should.fail()
  }
}

pub fn enterprise_tier_test() {
  let tier = auth.enterprise_tier()

  auth.tier_name(tier) |> should.equal("enterprise")

  case auth.tier_monthly_quota(tier) {
    option.None -> should.be_true(True)
    option.Some(_) -> should.fail()
  }
}

pub fn string_to_tier_test() {
  case auth.string_to_tier("free") {
    option.Some(tier) -> auth.tier_name(tier) |> should.equal("free")
    option.None -> should.fail()
  }

  case auth.string_to_tier("pro") {
    option.Some(tier) -> auth.tier_name(tier) |> should.equal("pro")
    option.None -> should.fail()
  }

  case auth.string_to_tier("invalid") {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

// =============================================================================
// API Key Management Tests
// =============================================================================

pub fn new_store_test() {
  let store = auth.new_store()

  auth.list_keys(store) |> list.length |> should.equal(0)
}

pub fn generate_api_key_test() {
  let store = auth.new_store()
  let #(updated_store, api_key) = auth.generate_api_key(store, auth.free_tier())

  api_key.key |> should_start_with("foil_")
  auth.tier_name(api_key.tier) |> should.equal("free")
  api_key.request_count |> should.equal(0)

  auth.list_keys(updated_store) |> list.length |> should.equal(1)
}

pub fn validate_key_success_test() {
  let store = auth.new_store()
  let #(updated_store, api_key) = auth.generate_api_key(store, auth.pro_tier())

  case auth.validate_key(updated_store, api_key.key) {
    option.Some(validated) -> {
      validated.key |> should.equal(api_key.key)
      auth.tier_name(validated.tier) |> should.equal("pro")
    }
    option.None -> should.fail()
  }
}

pub fn validate_key_failure_test() {
  let store = auth.new_store()

  case auth.validate_key(store, "invalid_key") {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

pub fn update_key_usage_test() {
  let store = auth.new_store()
  let #(store2, api_key) = auth.generate_api_key(store, auth.free_tier())

  let store3 = auth.update_key_usage(store2, api_key.key)

  case auth.validate_key(store3, api_key.key) {
    option.Some(updated) -> {
      updated.request_count |> should.equal(1)
      updated.monthly_requests |> should.equal(1)
    }
    option.None -> should.fail()
  }
}

pub fn revoke_key_test() {
  let store = auth.new_store()
  let #(store2, api_key) = auth.generate_api_key(store, auth.free_tier())

  let store3 = auth.revoke_key(store2, api_key.key)

  case auth.validate_key(store3, api_key.key) {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

// =============================================================================
// Rate Limiting Tests
// =============================================================================

pub fn check_rate_limit_within_quota_test() {
  let api_key =
    auth.ApiKey(
      key: "test_key",
      tier: auth.free_tier(),
      created_at: "2026-01-14T00:00:00Z",
      last_used: option.None,
      request_count: 50,
      monthly_requests: 1000,
    )

  case auth.check_rate_limit(api_key) {
    Ok(status) -> {
      auth.tier_name(status.tier) |> should.equal("free")
      { status.requests_remaining > 0 } |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn check_monthly_quota_test() {
  let api_key =
    auth.ApiKey(
      key: "test_key",
      tier: auth.basic_tier(),
      created_at: "2026-01-14T00:00:00Z",
      last_used: option.None,
      request_count: 0,
      monthly_requests: 50_000,
    )

  auth.check_monthly_quota(api_key) |> should.be_true()
}

pub fn check_monthly_quota_exceeded_test() {
  let api_key =
    auth.ApiKey(
      key: "test_key",
      tier: auth.basic_tier(),
      created_at: "2026-01-14T00:00:00Z",
      last_used: option.None,
      request_count: 0,
      monthly_requests: 100_001,
    )

  auth.check_monthly_quota(api_key) |> should.be_false()
}

// =============================================================================
// JWT Token Tests
// =============================================================================

pub fn generate_token_test() {
  let token = auth.generate_token("test_api_key")

  token.api_key |> should.equal("test_api_key")
  token.token |> should_start_with("eyJ")
}

pub fn validate_token_success_test() {
  case auth.validate_token("eyJabcdefg") {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

pub fn validate_token_failure_test() {
  case auth.validate_token("invalid") {
    Ok(_) -> should.fail()
    Error(msg) -> msg |> should_contain("Invalid")
  }
}

pub fn is_token_expired_test() {
  let token = auth.generate_token("key")
  auth.is_token_expired(token) |> should.be_false()
}

// =============================================================================
// Authentication Tests
// =============================================================================

pub fn authenticate_with_valid_key_test() {
  let store = auth.new_store()
  let #(updated_store, api_key) = auth.generate_api_key(store, auth.pro_tier())

  case auth.authenticate(updated_store, option.Some(api_key.key)) {
    auth.Authenticated(key) -> {
      key.key |> should.equal(api_key.key)
    }
    _ -> should.fail()
  }
}

pub fn authenticate_without_key_test() {
  let store = auth.new_store()

  case auth.authenticate(store, option.None) {
    auth.Unauthenticated -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn authenticate_with_invalid_key_test() {
  let store = auth.new_store()

  case auth.authenticate(store, option.Some("invalid_key")) {
    auth.InvalidKey -> should.be_true(True)
    _ -> should.fail()
  }
}

// =============================================================================
// Header Parsing Tests
// =============================================================================

pub fn extract_api_key_from_authorization_test() {
  let headers = [#("Authorization", "Bearer abc123")]

  case auth.extract_api_key(headers) {
    option.Some(key) -> key |> should.equal("abc123")
    option.None -> should.fail()
  }
}

pub fn extract_api_key_from_x_api_key_test() {
  let headers = [#("X-API-Key", "xyz789")]

  case auth.extract_api_key(headers) {
    option.Some(key) -> key |> should.equal("xyz789")
    option.None -> should.fail()
  }
}

pub fn extract_api_key_no_header_test() {
  let headers = [#("Content-Type", "application/json")]

  case auth.extract_api_key(headers) {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

// =============================================================================
// Response Formatting Tests
// =============================================================================

pub fn format_rate_limit_headers_test() {
  let status =
    auth.RateLimitStatus(
      tier: auth.free_tier(),
      requests_remaining: 95,
      reset_time: "2026-01-14T00:01:00Z",
      monthly_remaining: option.None,
    )

  let headers = auth.format_rate_limit_headers(status)

  { list.length(headers) >= 3 } |> should.be_true()

  case list.key_find(headers, "X-RateLimit-Limit") {
    Ok(value) -> value |> should.equal("100")
    Error(_) -> should.fail()
  }
}

pub fn auth_result_to_status_test() {
  let api_key =
    auth.ApiKey(
      key: "test",
      tier: auth.free_tier(),
      created_at: "2026-01-14",
      last_used: option.None,
      request_count: 0,
      monthly_requests: 0,
    )

  auth.auth_result_to_status(auth.Authenticated(api_key)) |> should.equal(200)
  auth.auth_result_to_status(auth.Unauthenticated) |> should.equal(401)
  auth.auth_result_to_status(auth.RateLimited(60)) |> should.equal(429)
  auth.auth_result_to_status(auth.InvalidKey) |> should.equal(401)
}

pub fn auth_error_message_test() {
  let msg = auth.auth_error_message(auth.Unauthenticated)
  msg |> should_contain("Authentication required")

  let rate_msg = auth.auth_error_message(auth.RateLimited(60))
  rate_msg |> should_contain("Rate limit")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn should_start_with(haystack: String, prefix: String) -> Nil {
  case string.starts_with(haystack, prefix) {
    True -> Nil
    False -> should.fail()
  }
}

fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False -> should.fail()
  }
}
