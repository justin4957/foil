//// Authentication and Rate Limiting Module
////
//// Provides JWT-based authentication, API key management, and tiered
//// rate limiting for the modal logic API.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Rate limiting tier
pub type RateLimitTier {
  Free(requests_per_minute: Int)
  Basic(requests_per_minute: Int, monthly_quota: Int)
  Pro(requests_per_minute: Int, monthly_quota: Int)
  Enterprise(unlimited: Bool)
}

/// API key
pub type ApiKey {
  ApiKey(
    key: String,
    tier: RateLimitTier,
    created_at: String,
    last_used: Option(String),
    request_count: Int,
    monthly_requests: Int,
  )
}

/// Authentication token (JWT)
pub type AuthToken {
  AuthToken(
    token: String,
    api_key: String,
    expires_at: String,
    issued_at: String,
  )
}

/// Rate limit status
pub type RateLimitStatus {
  RateLimitStatus(
    tier: RateLimitTier,
    requests_remaining: Int,
    reset_time: String,
    monthly_remaining: Option(Int),
  )
}

/// Auth result
pub type AuthResult {
  Authenticated(api_key: ApiKey)
  Unauthenticated
  RateLimited(retry_after: Int)
  InvalidKey
  ExpiredToken
}

/// API key store (in-memory for now)
pub type ApiKeyStore {
  ApiKeyStore(keys: Dict(String, ApiKey))
}

// =============================================================================
// Rate Limit Tiers
// =============================================================================

/// Free tier configuration
pub fn free_tier() -> RateLimitTier {
  Free(requests_per_minute: 100)
}

/// Basic tier configuration
pub fn basic_tier() -> RateLimitTier {
  Basic(requests_per_minute: 500, monthly_quota: 100_000)
}

/// Pro tier configuration
pub fn pro_tier() -> RateLimitTier {
  Pro(requests_per_minute: 1000, monthly_quota: 1_000_000)
}

/// Enterprise tier configuration
pub fn enterprise_tier() -> RateLimitTier {
  Enterprise(unlimited: True)
}

/// Get tier name
pub fn tier_name(tier: RateLimitTier) -> String {
  case tier {
    Free(_) -> "free"
    Basic(_, _) -> "basic"
    Pro(_, _) -> "pro"
    Enterprise(_) -> "enterprise"
  }
}

/// Get requests per minute for tier
pub fn tier_requests_per_minute(tier: RateLimitTier) -> Int {
  case tier {
    Free(rpm) -> rpm
    Basic(rpm, _) -> rpm
    Pro(rpm, _) -> rpm
    Enterprise(_) -> 999_999
  }
}

/// Get monthly quota for tier
pub fn tier_monthly_quota(tier: RateLimitTier) -> Option(Int) {
  case tier {
    Free(_) -> None
    Basic(_, quota) -> Some(quota)
    Pro(_, quota) -> Some(quota)
    Enterprise(_) -> None
  }
}

// =============================================================================
// API Key Management
// =============================================================================

/// Create new API key store
pub fn new_store() -> ApiKeyStore {
  ApiKeyStore(keys: dict.new())
}

/// Generate new API key (simplified)
pub fn generate_api_key(
  store: ApiKeyStore,
  tier: RateLimitTier,
) -> #(ApiKeyStore, ApiKey) {
  let key_string = "foil_" <> generate_random_string(32)

  let api_key =
    ApiKey(
      key: key_string,
      tier: tier,
      created_at: "2026-01-14T00:00:00Z",
      last_used: None,
      request_count: 0,
      monthly_requests: 0,
    )

  let updated_store =
    ApiKeyStore(keys: dict.insert(store.keys, key_string, api_key))

  #(updated_store, api_key)
}

/// Validate API key
pub fn validate_key(store: ApiKeyStore, key: String) -> Option(ApiKey) {
  dict.get(store.keys, key)
  |> option.from_result
}

/// Update API key usage
pub fn update_key_usage(store: ApiKeyStore, key: String) -> ApiKeyStore {
  case dict.get(store.keys, key) {
    Error(_) -> store
    Ok(api_key) -> {
      let updated_key =
        ApiKey(
          ..api_key,
          last_used: Some("2026-01-14T00:00:00Z"),
          request_count: api_key.request_count + 1,
          monthly_requests: api_key.monthly_requests + 1,
        )

      ApiKeyStore(keys: dict.insert(store.keys, key, updated_key))
    }
  }
}

/// Revoke API key
pub fn revoke_key(store: ApiKeyStore, key: String) -> ApiKeyStore {
  ApiKeyStore(keys: dict.delete(store.keys, key))
}

/// List all API keys
pub fn list_keys(store: ApiKeyStore) -> List(ApiKey) {
  dict.values(store.keys)
}

// =============================================================================
// Rate Limiting
// =============================================================================

/// Check if request should be rate limited
pub fn check_rate_limit(api_key: ApiKey) -> Result(RateLimitStatus, Int) {
  let requests_per_minute = tier_requests_per_minute(api_key.tier)

  // Simplified: Always allow for now
  // Real implementation would track request timestamps
  let remaining = requests_per_minute - api_key.request_count % 60

  let status =
    RateLimitStatus(
      tier: api_key.tier,
      requests_remaining: remaining,
      reset_time: "2026-01-14T00:01:00Z",
      monthly_remaining: case tier_monthly_quota(api_key.tier) {
        None -> None
        Some(quota) -> Some(quota - api_key.monthly_requests)
      },
    )

  Ok(status)
}

/// Check if monthly quota exceeded
pub fn check_monthly_quota(api_key: ApiKey) -> Bool {
  case tier_monthly_quota(api_key.tier) {
    None -> True
    // No quota limit
    Some(quota) -> api_key.monthly_requests < quota
  }
}

// =============================================================================
// JWT Authentication (Simplified)
// =============================================================================

/// Generate JWT token (simplified - real implementation would use crypto)
pub fn generate_token(api_key: String) -> AuthToken {
  let token_string = "eyJ..." <> api_key <> "...xyz"

  AuthToken(
    token: token_string,
    api_key: api_key,
    expires_at: "2026-01-15T00:00:00Z",
    issued_at: "2026-01-14T00:00:00Z",
  )
}

/// Validate JWT token (simplified)
pub fn validate_token(token: String) -> Result(String, String) {
  // Simplified: Extract API key from token
  case string.starts_with(token, "eyJ") {
    True -> Ok("extracted_api_key")
    False -> Error("Invalid token format")
  }
}

/// Check if token is expired (simplified)
pub fn is_token_expired(_token: AuthToken) -> Bool {
  // Simplified: Never expired
  False
}

// =============================================================================
// Authentication Middleware
// =============================================================================

/// Authenticate request
pub fn authenticate(store: ApiKeyStore, api_key: Option(String)) -> AuthResult {
  case api_key {
    None -> Unauthenticated
    Some(key) ->
      case validate_key(store, key) {
        None -> InvalidKey
        Some(validated_key) ->
          case check_rate_limit(validated_key) {
            Error(retry_after) -> RateLimited(retry_after)
            Ok(_status) ->
              case check_monthly_quota(validated_key) {
                False -> RateLimited(3600)
                True -> Authenticated(validated_key)
              }
          }
      }
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Generate random string (simplified)
fn generate_random_string(length: Int) -> String {
  // Simplified: Return fixed string
  // Real implementation would use crypto random
  string.repeat("a", length)
}

/// Format rate limit status for headers
pub fn format_rate_limit_headers(
  status: RateLimitStatus,
) -> List(#(String, String)) {
  let rpm = tier_requests_per_minute(status.tier)

  [
    #("X-RateLimit-Limit", int.to_string(rpm)),
    #("X-RateLimit-Remaining", int.to_string(status.requests_remaining)),
    #("X-RateLimit-Reset", status.reset_time),
  ]
}

/// Get tier from string
pub fn string_to_tier(s: String) -> Option(RateLimitTier) {
  case string.lowercase(s) {
    "free" -> Some(free_tier())
    "basic" -> Some(basic_tier())
    "pro" -> Some(pro_tier())
    "enterprise" -> Some(enterprise_tier())
    _ -> None
  }
}

/// Extract API key from Authorization header
pub fn extract_api_key(headers: List(#(String, String))) -> Option(String) {
  case list.key_find(headers, "Authorization") {
    Error(_) ->
      // Try X-API-Key header
      case list.key_find(headers, "X-API-Key") {
        Error(_) -> None
        Ok(key) -> Some(key)
      }
    Ok(auth_header) -> {
      // Bearer token or direct API key
      case string.starts_with(auth_header, "Bearer ") {
        True -> Some(string.drop_start(auth_header, 7))
        False -> Some(auth_header)
      }
    }
  }
}

/// Format auth result as HTTP status
pub fn auth_result_to_status(result: AuthResult) -> Int {
  case result {
    Authenticated(_) -> 200
    Unauthenticated -> 401
    RateLimited(_) -> 429
    InvalidKey -> 401
    ExpiredToken -> 401
  }
}

/// Format auth error message
pub fn auth_error_message(result: AuthResult) -> String {
  case result {
    Authenticated(_) -> "Authenticated"
    Unauthenticated ->
      "Authentication required. Provide API key in Authorization or X-API-Key header"
    RateLimited(retry) ->
      "Rate limit exceeded. Retry after " <> int.to_string(retry) <> " seconds"
    InvalidKey -> "Invalid API key. Check your key or generate a new one"
    ExpiredToken -> "Token expired. Generate a new token"
  }
}
