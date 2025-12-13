//// Caching Layer
////
//// This module provides caching for validation results using a key-value
//// approach. It supports configurable TTL and normalization for consistent keys.
////
//// ## Cache Strategy
////
//// - **Validation Results**: Cached by hash of formalization (logic system + premises + conclusion)
//// - **TTL**: Configurable time-to-live for cache entries
//// - **Normalization**: Consistent key generation via proposition normalization
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/cache
////
//// let key = cache.validation_cache_key(formalization)
//// let cached = cache.get_validation(cache_store, key)
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/order
import gleam/string
import modal_logic/argument.{type Formalization, type ValidationResult}
import modal_logic/proposition.{type LogicSystem, type Proposition}

// =============================================================================
// Types
// =============================================================================

/// Cache configuration
pub type CacheConfig {
  CacheConfig(
    /// Default TTL in seconds (0 = no expiry)
    default_ttl_seconds: Int,
    /// Maximum number of entries (0 = unlimited)
    max_entries: Int,
    /// Enable normalization for consistent keys
    normalize_keys: Bool,
    /// Prefix for all cache keys
    key_prefix: String,
  )
}

/// A cached validation result with metadata
pub type CachedValidation {
  CachedValidation(
    /// The validation result
    result: ValidationResult,
    /// Cache key used
    cache_key: String,
    /// Unix timestamp when cached
    cached_at: Int,
    /// TTL in seconds (0 = no expiry)
    ttl_seconds: Int,
    /// Number of times this entry was accessed
    hit_count: Int,
  )
}

/// In-memory cache store (for testing and simple use cases)
pub type MemoryCache {
  MemoryCache(
    config: CacheConfig,
    entries: Dict(String, CachedValidation),
    stats: CacheStats,
  )
}

/// Cache statistics
pub type CacheStats {
  CacheStats(
    /// Total number of cache hits
    hits: Int,
    /// Total number of cache misses
    misses: Int,
    /// Total number of entries added
    additions: Int,
    /// Total number of entries evicted
    evictions: Int,
    /// Current number of entries
    current_size: Int,
  )
}

/// Cache operation result
pub type CacheResult(a) {
  CacheHit(value: a)
  CacheMiss
  CacheError(message: String)
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default cache configuration
pub fn default_config() -> CacheConfig {
  CacheConfig(
    default_ttl_seconds: 3600,
    // 1 hour
    max_entries: 10_000,
    normalize_keys: True,
    key_prefix: "ml:validation:",
  )
}

/// Create a fast cache configuration (shorter TTL, smaller size)
pub fn fast_config() -> CacheConfig {
  CacheConfig(
    default_ttl_seconds: 300,
    // 5 minutes
    max_entries: 1000,
    normalize_keys: True,
    key_prefix: "ml:validation:",
  )
}

/// Create a persistent cache configuration (longer TTL)
pub fn persistent_config() -> CacheConfig {
  CacheConfig(
    default_ttl_seconds: 86_400,
    // 24 hours
    max_entries: 100_000,
    normalize_keys: True,
    key_prefix: "ml:validation:",
  )
}

/// Create configuration with custom TTL
pub fn with_ttl(config: CacheConfig, ttl_seconds: Int) -> CacheConfig {
  CacheConfig(..config, default_ttl_seconds: ttl_seconds)
}

/// Create configuration with custom max entries
pub fn with_max_entries(config: CacheConfig, max_entries: Int) -> CacheConfig {
  CacheConfig(..config, max_entries: max_entries)
}

/// Create configuration with custom key prefix
pub fn with_key_prefix(config: CacheConfig, prefix: String) -> CacheConfig {
  CacheConfig(..config, key_prefix: prefix)
}

// =============================================================================
// Cache Key Generation
// =============================================================================

/// Generate a cache key for a formalization's validation result
pub fn validation_cache_key(
  config: CacheConfig,
  formalization: Formalization,
) -> String {
  let normalized_premises = case config.normalize_keys {
    True -> normalize_propositions(formalization.premises)
    False -> formalization.premises
  }

  let normalized_conclusion = case config.normalize_keys {
    True -> normalize_proposition(formalization.conclusion)
    False -> formalization.conclusion
  }

  let key_content =
    logic_system_to_string(formalization.logic_system)
    <> "::"
    <> propositions_to_string(normalized_premises)
    <> "::"
    <> proposition_to_string(normalized_conclusion)

  let hash = hash_string(key_content)
  config.key_prefix <> hash
}

/// Generate a cache key for a raw formalization specification
pub fn raw_cache_key(
  config: CacheConfig,
  logic_system: LogicSystem,
  premises: List(Proposition),
  conclusion: Proposition,
) -> String {
  let key_content =
    logic_system_to_string(logic_system)
    <> "::"
    <> propositions_to_string(premises)
    <> "::"
    <> proposition_to_string(conclusion)

  let hash = hash_string(key_content)
  config.key_prefix <> hash
}

/// Check if two cache keys are equivalent
pub fn keys_equivalent(key1: String, key2: String) -> Bool {
  key1 == key2
}

// =============================================================================
// Memory Cache Operations
// =============================================================================

/// Create a new in-memory cache
pub fn new_memory_cache(config: CacheConfig) -> MemoryCache {
  MemoryCache(config: config, entries: dict.new(), stats: empty_stats())
}

/// Get a validation result from the memory cache
pub fn memory_get(
  cache: MemoryCache,
  key: String,
  current_time: Int,
) -> #(MemoryCache, CacheResult(ValidationResult)) {
  case dict.get(cache.entries, key) {
    Ok(entry) -> {
      // Check if entry has expired
      case is_expired(entry, current_time) {
        True -> {
          // Entry expired, remove it
          let new_entries = dict.delete(cache.entries, key)
          let new_stats =
            CacheStats(
              ..cache.stats,
              misses: cache.stats.misses + 1,
              evictions: cache.stats.evictions + 1,
              current_size: cache.stats.current_size - 1,
            )
          #(
            MemoryCache(..cache, entries: new_entries, stats: new_stats),
            CacheMiss,
          )
        }
        False -> {
          // Entry valid, update hit count
          let updated_entry =
            CachedValidation(..entry, hit_count: entry.hit_count + 1)
          let new_entries = dict.insert(cache.entries, key, updated_entry)
          let new_stats = CacheStats(..cache.stats, hits: cache.stats.hits + 1)
          #(
            MemoryCache(..cache, entries: new_entries, stats: new_stats),
            CacheHit(entry.result),
          )
        }
      }
    }
    Error(_) -> {
      let new_stats = CacheStats(..cache.stats, misses: cache.stats.misses + 1)
      #(MemoryCache(..cache, stats: new_stats), CacheMiss)
    }
  }
}

/// Put a validation result into the memory cache
pub fn memory_put(
  cache: MemoryCache,
  key: String,
  result: ValidationResult,
  current_time: Int,
) -> MemoryCache {
  // Check if we need to evict entries
  let cache = case
    cache.config.max_entries > 0
    && cache.stats.current_size >= cache.config.max_entries
  {
    True -> evict_oldest(cache)
    False -> cache
  }

  let entry =
    CachedValidation(
      result: result,
      cache_key: key,
      cached_at: current_time,
      ttl_seconds: cache.config.default_ttl_seconds,
      hit_count: 0,
    )

  let is_new = !dict.has_key(cache.entries, key)
  let new_entries = dict.insert(cache.entries, key, entry)
  let new_stats =
    CacheStats(
      ..cache.stats,
      additions: cache.stats.additions + 1,
      current_size: case is_new {
        True -> cache.stats.current_size + 1
        False -> cache.stats.current_size
      },
    )

  MemoryCache(..cache, entries: new_entries, stats: new_stats)
}

/// Put a validation result with custom TTL
pub fn memory_put_with_ttl(
  cache: MemoryCache,
  key: String,
  result: ValidationResult,
  ttl_seconds: Int,
  current_time: Int,
) -> MemoryCache {
  let cache = case
    cache.config.max_entries > 0
    && cache.stats.current_size >= cache.config.max_entries
  {
    True -> evict_oldest(cache)
    False -> cache
  }

  let entry =
    CachedValidation(
      result: result,
      cache_key: key,
      cached_at: current_time,
      ttl_seconds: ttl_seconds,
      hit_count: 0,
    )

  let is_new = !dict.has_key(cache.entries, key)
  let new_entries = dict.insert(cache.entries, key, entry)
  let new_stats =
    CacheStats(
      ..cache.stats,
      additions: cache.stats.additions + 1,
      current_size: case is_new {
        True -> cache.stats.current_size + 1
        False -> cache.stats.current_size
      },
    )

  MemoryCache(..cache, entries: new_entries, stats: new_stats)
}

/// Delete an entry from the memory cache
pub fn memory_delete(cache: MemoryCache, key: String) -> MemoryCache {
  case dict.has_key(cache.entries, key) {
    True -> {
      let new_entries = dict.delete(cache.entries, key)
      let new_stats =
        CacheStats(..cache.stats, current_size: cache.stats.current_size - 1)
      MemoryCache(..cache, entries: new_entries, stats: new_stats)
    }
    False -> cache
  }
}

/// Clear all entries from the memory cache
pub fn memory_clear(cache: MemoryCache) -> MemoryCache {
  MemoryCache(
    ..cache,
    entries: dict.new(),
    stats: CacheStats(
      ..cache.stats,
      evictions: cache.stats.evictions + cache.stats.current_size,
      current_size: 0,
    ),
  )
}

/// Get cache statistics
pub fn memory_stats(cache: MemoryCache) -> CacheStats {
  cache.stats
}

/// Check if cache contains a key
pub fn memory_has(cache: MemoryCache, key: String) -> Bool {
  dict.has_key(cache.entries, key)
}

/// Get all cache keys
pub fn memory_keys(cache: MemoryCache) -> List(String) {
  dict.keys(cache.entries)
}

// =============================================================================
// Cache Statistics
// =============================================================================

/// Create empty cache statistics
pub fn empty_stats() -> CacheStats {
  CacheStats(hits: 0, misses: 0, additions: 0, evictions: 0, current_size: 0)
}

/// Calculate cache hit rate as a percentage
pub fn hit_rate(stats: CacheStats) -> Float {
  let total = stats.hits + stats.misses
  case total {
    0 -> 0.0
    _ -> int_to_float(stats.hits * 100) /. int_to_float(total)
  }
}

/// Format cache statistics as a string
pub fn format_stats(stats: CacheStats) -> String {
  "CacheStats{hits="
  <> int_to_string(stats.hits)
  <> ", misses="
  <> int_to_string(stats.misses)
  <> ", hit_rate="
  <> float_to_string(hit_rate(stats))
  <> "%, additions="
  <> int_to_string(stats.additions)
  <> ", evictions="
  <> int_to_string(stats.evictions)
  <> ", size="
  <> int_to_string(stats.current_size)
  <> "}"
}

// =============================================================================
// Redis Command Builders
// =============================================================================

/// Build Redis GET command for validation cache
pub fn redis_get_command(key: String) -> String {
  "GET " <> key
}

/// Build Redis SET command with TTL
pub fn redis_set_command(key: String, value: String, ttl_seconds: Int) -> String {
  case ttl_seconds {
    0 -> "SET " <> key <> " " <> quote_string(value)
    ttl ->
      "SETEX " <> key <> " " <> int_to_string(ttl) <> " " <> quote_string(value)
  }
}

/// Build Redis DEL command
pub fn redis_del_command(key: String) -> String {
  "DEL " <> key
}

/// Build Redis EXISTS command
pub fn redis_exists_command(key: String) -> String {
  "EXISTS " <> key
}

/// Build Redis TTL command
pub fn redis_ttl_command(key: String) -> String {
  "TTL " <> key
}

/// Build Redis KEYS command with pattern
pub fn redis_keys_command(pattern: String) -> String {
  "KEYS " <> pattern
}

/// Build Redis SCAN command for iterating keys
pub fn redis_scan_command(cursor: Int, pattern: String, count: Int) -> String {
  "SCAN "
  <> int_to_string(cursor)
  <> " MATCH "
  <> pattern
  <> " COUNT "
  <> int_to_string(count)
}

/// Build Redis MGET command for multiple keys
pub fn redis_mget_command(keys: List(String)) -> String {
  "MGET " <> string.join(keys, " ")
}

/// Build Redis pipeline for batch operations
pub fn redis_pipeline(commands: List(String)) -> String {
  string.join(commands, "\n")
}

// =============================================================================
// Proposition Normalization
// =============================================================================

/// Normalize a proposition for consistent caching
pub fn normalize_proposition(prop: Proposition) -> Proposition {
  case prop {
    proposition.Atom(name) -> proposition.Atom(string.lowercase(name))

    proposition.Not(inner) -> proposition.Not(normalize_proposition(inner))

    proposition.And(left, right) -> {
      let norm_left = normalize_proposition(left)
      let norm_right = normalize_proposition(right)
      // Sort operands for canonical form
      case compare_propositions(norm_left, norm_right) {
        order.Lt -> proposition.And(norm_left, norm_right)
        _ -> proposition.And(norm_right, norm_left)
      }
    }

    proposition.Or(left, right) -> {
      let norm_left = normalize_proposition(left)
      let norm_right = normalize_proposition(right)
      case compare_propositions(norm_left, norm_right) {
        order.Lt -> proposition.Or(norm_left, norm_right)
        _ -> proposition.Or(norm_right, norm_left)
      }
    }

    proposition.Implies(ante, cons) ->
      proposition.Implies(
        normalize_proposition(ante),
        normalize_proposition(cons),
      )

    proposition.Necessary(inner) ->
      proposition.Necessary(normalize_proposition(inner))

    proposition.Possible(inner) ->
      proposition.Possible(normalize_proposition(inner))

    proposition.Obligatory(inner) ->
      proposition.Obligatory(normalize_proposition(inner))

    proposition.Permitted(inner) ->
      proposition.Permitted(normalize_proposition(inner))

    proposition.Knows(agent, inner) ->
      proposition.Knows(string.lowercase(agent), normalize_proposition(inner))

    proposition.Believes(agent, inner) ->
      proposition.Believes(
        string.lowercase(agent),
        normalize_proposition(inner),
      )
  }
}

/// Normalize a list of propositions
pub fn normalize_propositions(props: List(Proposition)) -> List(Proposition) {
  props
  |> list.map(normalize_proposition)
  |> list.sort(compare_propositions)
}

// =============================================================================
// Internal Helpers
// =============================================================================

fn is_expired(entry: CachedValidation, current_time: Int) -> Bool {
  case entry.ttl_seconds {
    0 -> False
    // No expiry
    ttl -> current_time > entry.cached_at + ttl
  }
}

fn evict_oldest(cache: MemoryCache) -> MemoryCache {
  // Find the oldest entry
  let entries_list = dict.to_list(cache.entries)
  case entries_list {
    [] -> cache
    _ -> {
      let oldest =
        list.fold(entries_list, #("", 999_999_999_999), fn(acc, entry) {
          let #(key, value) = entry
          let #(_acc_key, acc_time) = acc
          case value.cached_at < acc_time {
            True -> #(key, value.cached_at)
            False -> acc
          }
        })
      let #(oldest_key, _) = oldest
      let new_entries = dict.delete(cache.entries, oldest_key)
      let new_stats =
        CacheStats(
          ..cache.stats,
          evictions: cache.stats.evictions + 1,
          current_size: cache.stats.current_size - 1,
        )
      MemoryCache(..cache, entries: new_entries, stats: new_stats)
    }
  }
}

/// Simple DJB2 hash function for cache keys
/// This doesn't need cryptographic security - just consistent key generation
fn hash_string(s: String) -> String {
  let hash_value = djb2_hash(s)
  int_to_hex(hash_value)
}

/// DJB2 hash algorithm
fn djb2_hash(s: String) -> Int {
  s
  |> string.to_utf_codepoints
  |> list.fold(5381, fn(hash, codepoint) {
    let char_code = string.utf_codepoint_to_int(codepoint)
    // hash * 33 + char_code, with wrapping to prevent overflow
    { { hash * 33 } % 0xFFFFFFFF + char_code } % 0xFFFFFFFF
  })
}

/// Convert an integer to hexadecimal string
fn int_to_hex(n: Int) -> String {
  do_int_to_hex(abs_int(n), "")
}

fn do_int_to_hex(n: Int, acc: String) -> String {
  case n {
    0 ->
      case acc {
        "" -> "0"
        _ -> acc
      }
    _ -> {
      let digit = n % 16
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
        9 -> "9"
        10 -> "a"
        11 -> "b"
        12 -> "c"
        13 -> "d"
        14 -> "e"
        _ -> "f"
      }
      do_int_to_hex(n / 16, char <> acc)
    }
  }
}

fn abs_int(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

fn compare_propositions(a: Proposition, b: Proposition) -> order.Order {
  string.compare(proposition_to_string(a), proposition_to_string(b))
}

fn proposition_to_string(prop: Proposition) -> String {
  case prop {
    proposition.Atom(name) -> "A(" <> name <> ")"
    proposition.Not(inner) -> "~" <> proposition_to_string(inner)
    proposition.And(left, right) ->
      "("
      <> proposition_to_string(left)
      <> "&"
      <> proposition_to_string(right)
      <> ")"
    proposition.Or(left, right) ->
      "("
      <> proposition_to_string(left)
      <> "|"
      <> proposition_to_string(right)
      <> ")"
    proposition.Implies(ante, cons) ->
      "("
      <> proposition_to_string(ante)
      <> "->"
      <> proposition_to_string(cons)
      <> ")"
    proposition.Necessary(inner) -> "[]" <> proposition_to_string(inner)
    proposition.Possible(inner) -> "<>" <> proposition_to_string(inner)
    proposition.Obligatory(inner) -> "O" <> proposition_to_string(inner)
    proposition.Permitted(inner) -> "P" <> proposition_to_string(inner)
    proposition.Knows(agent, inner) ->
      "K_" <> agent <> proposition_to_string(inner)
    proposition.Believes(agent, inner) ->
      "B_" <> agent <> proposition_to_string(inner)
  }
}

fn propositions_to_string(props: List(Proposition)) -> String {
  props
  |> list.map(proposition_to_string)
  |> string.join(",")
}

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

fn quote_string(s: String) -> String {
  "\"" <> escape_string(s) <> "\""
}

fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
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
  // Simple approximation for display
  let int_part = float_to_int(f)
  int_to_string(int_part)
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
