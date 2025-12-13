//// Timeout Handling Module
////
//// This module provides configurable timeout handling for Z3 solver operations.
//// It supports both Z3-native timeouts and BEAM process timeouts.
////
//// ## Timeout Strategy
////
//// Z3 operations can be bounded by two types of timeouts:
//// 1. **Z3 Native Timeout**: Configured in Z3 solver parameters, limits solving time
//// 2. **BEAM Process Timeout**: Erlang/OTP process timeout for port communication
////
//// ## Usage
////
//// ```gleam
//// import z3/timeout.{Timeout, TimeoutConfig}
////
//// // Create a timeout configuration
//// let config = timeout.config(
////   z3_timeout_ms: 5000,      // 5 seconds for Z3 solving
////   port_timeout_ms: 6000,    // 6 seconds for port communication
//// )
////
//// // Apply timeout to an operation
//// let result = timeout.with_timeout(config, fn() { expensive_operation() })
//// ```

import gleam/option.{type Option, None, Some}

// =============================================================================
// Types
// =============================================================================

/// Configuration for timeout behavior
pub type TimeoutConfig {
  TimeoutConfig(
    /// Z3 solver timeout in milliseconds (0 = no timeout)
    z3_timeout_ms: Int,
    /// BEAM/port communication timeout in milliseconds (0 = no timeout)
    port_timeout_ms: Int,
    /// Whether to allow soft timeout (Z3 may continue slightly past timeout)
    soft_timeout: Bool,
  )
}

/// Result of a timed operation
pub type TimedResult(a) {
  /// Operation completed successfully within timeout
  Completed(result: a, elapsed_ms: Int)
  /// Operation timed out
  TimedOut(elapsed_ms: Int, timeout_type: TimeoutType)
  /// Operation failed for other reasons
  Failed(reason: String)
}

/// Type of timeout that occurred
pub type TimeoutType {
  /// Z3 solver timeout was reached
  Z3Timeout
  /// BEAM process timeout was reached
  PortTimeout
  /// Combined timeout (both limits exceeded)
  CombinedTimeout
}

/// Statistics about timeout behavior
pub type TimeoutStats {
  TimeoutStats(
    /// Total operations attempted
    total_operations: Int,
    /// Operations that completed in time
    completed_count: Int,
    /// Operations that timed out
    timeout_count: Int,
    /// Average completion time in ms
    avg_completion_ms: Int,
    /// Maximum completion time in ms
    max_completion_ms: Int,
  )
}

// =============================================================================
// Configuration Constructors
// =============================================================================

/// Create a default timeout configuration (no timeouts)
pub fn default_config() -> TimeoutConfig {
  TimeoutConfig(z3_timeout_ms: 0, port_timeout_ms: 0, soft_timeout: False)
}

/// Create a timeout configuration with specified Z3 timeout
pub fn with_z3_timeout(timeout_ms: Int) -> TimeoutConfig {
  TimeoutConfig(
    z3_timeout_ms: timeout_ms,
    port_timeout_ms: timeout_ms + 1000,
    // Add buffer for port
    soft_timeout: False,
  )
}

/// Create a timeout configuration with specified port timeout
pub fn with_port_timeout(timeout_ms: Int) -> TimeoutConfig {
  TimeoutConfig(
    z3_timeout_ms: 0,
    port_timeout_ms: timeout_ms,
    soft_timeout: False,
  )
}

/// Create a timeout configuration with both timeouts
pub fn config(z3_timeout_ms: Int, port_timeout_ms: Int) -> TimeoutConfig {
  TimeoutConfig(
    z3_timeout_ms: z3_timeout_ms,
    port_timeout_ms: port_timeout_ms,
    soft_timeout: False,
  )
}

/// Create a timeout configuration with soft timeout enabled
pub fn with_soft_timeout(base_config: TimeoutConfig) -> TimeoutConfig {
  TimeoutConfig(..base_config, soft_timeout: True)
}

/// Common timeout presets
pub fn preset_fast() -> TimeoutConfig {
  config(1000, 2000)
}

pub fn preset_normal() -> TimeoutConfig {
  config(5000, 6000)
}

pub fn preset_slow() -> TimeoutConfig {
  config(30_000, 35_000)
}

pub fn preset_unlimited() -> TimeoutConfig {
  default_config()
}

// =============================================================================
// Timeout Checking
// =============================================================================

/// Check if a timeout configuration has any timeout set
pub fn has_timeout(config: TimeoutConfig) -> Bool {
  config.z3_timeout_ms > 0 || config.port_timeout_ms > 0
}

/// Get the effective timeout (minimum of non-zero timeouts)
pub fn effective_timeout(config: TimeoutConfig) -> Option(Int) {
  case config.z3_timeout_ms, config.port_timeout_ms {
    0, 0 -> None
    z3, 0 -> Some(z3)
    0, port -> Some(port)
    z3, port -> Some(min(z3, port))
  }
}

/// Check if an elapsed time exceeds the timeout
pub fn is_timed_out(config: TimeoutConfig, elapsed_ms: Int) -> Bool {
  case effective_timeout(config) {
    None -> False
    Some(timeout) -> elapsed_ms >= timeout
  }
}

/// Get remaining time until timeout
pub fn remaining_time(config: TimeoutConfig, elapsed_ms: Int) -> Option(Int) {
  case effective_timeout(config) {
    None -> None
    Some(timeout) -> {
      let remaining = timeout - elapsed_ms
      case remaining > 0 {
        True -> Some(remaining)
        False -> Some(0)
      }
    }
  }
}

// =============================================================================
// Z3 Parameter Generation
// =============================================================================

/// Generate Z3 solver parameters for timeout
pub fn to_z3_params(config: TimeoutConfig) -> List(#(String, String)) {
  case config.z3_timeout_ms {
    0 -> []
    ms -> [#("timeout", int_to_string(ms))]
  }
}

/// Generate Z3 tactic parameters for timeout
pub fn to_tactic_params(config: TimeoutConfig) -> List(#(String, String)) {
  let base = case config.z3_timeout_ms {
    0 -> []
    ms -> [#("timeout", int_to_string(ms))]
  }
  case config.soft_timeout {
    True -> [#("soft_timeout", "true"), ..base]
    False -> base
  }
}

// =============================================================================
// Timeout Statistics
// =============================================================================

/// Create empty timeout statistics
pub fn empty_stats() -> TimeoutStats {
  TimeoutStats(
    total_operations: 0,
    completed_count: 0,
    timeout_count: 0,
    avg_completion_ms: 0,
    max_completion_ms: 0,
  )
}

/// Update statistics with a completed operation
pub fn record_completion(stats: TimeoutStats, elapsed_ms: Int) -> TimeoutStats {
  let new_total = stats.total_operations + 1
  let new_completed = stats.completed_count + 1
  let total_time = stats.avg_completion_ms * stats.completed_count + elapsed_ms
  let new_avg = case new_completed {
    0 -> 0
    n -> total_time / n
  }
  let new_max = max(stats.max_completion_ms, elapsed_ms)
  TimeoutStats(
    total_operations: new_total,
    completed_count: new_completed,
    timeout_count: stats.timeout_count,
    avg_completion_ms: new_avg,
    max_completion_ms: new_max,
  )
}

/// Update statistics with a timed out operation
pub fn record_timeout(stats: TimeoutStats) -> TimeoutStats {
  TimeoutStats(
    ..stats,
    total_operations: stats.total_operations + 1,
    timeout_count: stats.timeout_count + 1,
  )
}

/// Calculate timeout rate as a percentage
pub fn timeout_rate(stats: TimeoutStats) -> Float {
  case stats.total_operations {
    0 -> 0.0
    total -> int_to_float(stats.timeout_count * 100) /. int_to_float(total)
  }
}

/// Format statistics as a string
pub fn format_stats(stats: TimeoutStats) -> String {
  "TimeoutStats{"
  <> "total="
  <> int_to_string(stats.total_operations)
  <> ", completed="
  <> int_to_string(stats.completed_count)
  <> ", timeouts="
  <> int_to_string(stats.timeout_count)
  <> ", avg_ms="
  <> int_to_string(stats.avg_completion_ms)
  <> ", max_ms="
  <> int_to_string(stats.max_completion_ms)
  <> "}"
}

// =============================================================================
// Configuration Validation
// =============================================================================

/// Validate a timeout configuration
pub fn validate_config(config: TimeoutConfig) -> Result(TimeoutConfig, String) {
  case config.z3_timeout_ms < 0 {
    True -> Error("Z3 timeout must be non-negative")
    False ->
      case config.port_timeout_ms < 0 {
        True -> Error("Port timeout must be non-negative")
        False ->
          case
            config.z3_timeout_ms > 0
            && config.port_timeout_ms > 0
            && config.port_timeout_ms < config.z3_timeout_ms
          {
            True ->
              Error(
                "Port timeout should be >= Z3 timeout to avoid premature termination",
              )
            False -> Ok(config)
          }
      }
  }
}

/// Adjust configuration to ensure valid timeouts
pub fn normalize_config(config: TimeoutConfig) -> TimeoutConfig {
  let z3 = max(0, config.z3_timeout_ms)
  let port = max(0, config.port_timeout_ms)
  // Ensure port timeout is at least as long as Z3 timeout
  let adjusted_port = case z3 > 0 && port > 0 && port < z3 {
    True -> z3 + 1000
    False -> port
  }
  TimeoutConfig(
    z3_timeout_ms: z3,
    port_timeout_ms: adjusted_port,
    soft_timeout: config.soft_timeout,
  )
}

// =============================================================================
// Internal Helpers
// =============================================================================

fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
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

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
