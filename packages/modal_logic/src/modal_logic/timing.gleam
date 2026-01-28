//// Timing Instrumentation Module
////
//// Provides real monotonic time measurement for benchmarks and validation
//// metrics using BEAM's `erlang:monotonic_time/0` via FFI.
////
//// ## Overview
////
//// This module replaces simulated/hardcoded timing with actual BEAM monotonic
//// clock measurements. Monotonic time is used (rather than wall-clock time)
//// because it is guaranteed to never go backwards, making it suitable for
//// measuring elapsed durations.
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/timing
////
//// // Measure a single operation
//// let #(result, elapsed_ms) = timing.measure_ms(fn() { expensive_work() })
////
//// // Get raw monotonic time for manual start/end measurement
//// let start = timing.monotonic_time_native()
//// let _result = do_work()
//// let end = timing.monotonic_time_native()
//// let elapsed_ms = timing.native_to_ms(end - start)
//// ```

import gleam/int
import gleam/list

/// Get current monotonic time in native time units.
///
/// Native units vary by platform but are consistent within a single BEAM
/// instance. Use `native_to_ms` to convert to milliseconds, or use
/// `monotonic_time_ms` for a direct millisecond reading.
pub fn monotonic_time_native() -> Int {
  erlang_monotonic_time()
}

/// Convert a duration in native time units to milliseconds.
///
/// Uses `erlang:convert_time_unit/3` for accurate conversion rather than
/// assuming a fixed native unit size.
pub fn native_to_ms(native_duration: Int) -> Int {
  erlang_convert_time_unit(native_duration, Native, Millisecond)
}

/// Convert a duration in native time units to microseconds.
///
/// Provides sub-millisecond precision for fast operations like Tier 1
/// syntactic pattern matching where millisecond granularity is too coarse.
pub fn native_to_microseconds(native_duration: Int) -> Int {
  erlang_convert_time_unit(native_duration, Native, Microsecond)
}

/// Get current monotonic time in milliseconds.
///
/// Convenience function for cases where only millisecond precision is needed.
pub fn monotonic_time_ms() -> Int {
  native_to_ms(monotonic_time_native())
}

/// Measure the elapsed time of a function call in milliseconds.
///
/// Returns a tuple of the function's return value and the elapsed time.
/// Uses monotonic time for accurate relative measurement.
///
/// ```gleam
/// let #(result, elapsed_ms) = timing.measure_ms(fn() {
///   heuristics.try_heuristic_validation(formalization)
/// })
/// ```
pub fn measure_ms(operation: fn() -> a) -> #(a, Int) {
  let start = monotonic_time_native()
  let result = operation()
  let end = monotonic_time_native()
  let elapsed_ms = native_to_ms(end - start)
  #(result, elapsed_ms)
}

/// Measure the elapsed time of a function call in microseconds.
///
/// Provides sub-millisecond precision for fast operations. Tier 1 syntactic
/// heuristics typically complete in under 1ms, so microsecond measurement
/// gives meaningful data.
///
/// ```gleam
/// let #(result, elapsed_us) = timing.measure_microseconds(fn() {
///   heuristics.try_heuristic_validation(formalization)
/// })
/// let elapsed_ms = int.to_float(elapsed_us) /. 1000.0
/// ```
pub fn measure_microseconds(operation: fn() -> a) -> #(a, Int) {
  let start = monotonic_time_native()
  let result = operation()
  let end = monotonic_time_native()
  let elapsed_us = native_to_microseconds(end - start)
  #(result, elapsed_us)
}

/// Measure elapsed time in native units for maximum precision.
///
/// Returns the raw native time difference. Convert using `native_to_ms`
/// or `native_to_microseconds` as needed.
pub fn measure_native(operation: fn() -> a) -> #(a, Int) {
  let start = monotonic_time_native()
  let result = operation()
  let end = monotonic_time_native()
  #(result, end - start)
}

/// Compute the P-th percentile from a sorted list of integer values.
///
/// Returns the value at the given percentile (0-100). For empty lists,
/// returns 0. The list must already be sorted in ascending order.
pub fn percentile(sorted_values: List(Int), pct: Int) -> Int {
  case sorted_values {
    [] -> 0
    _ -> {
      let count = list.length(sorted_values)
      let index = int.min({ pct * count } / 100, count - 1)
      sorted_values
      |> list.drop(index)
      |> list.first
      |> unwrap_or(0)
    }
  }
}

// =============================================================================
// Time unit types for FFI
// =============================================================================

/// Time unit for erlang:convert_time_unit/3
type TimeUnit {
  Native
  Millisecond
  Microsecond
}

// =============================================================================
// FFI Bindings
// =============================================================================

/// Get monotonic time in native units via erlang:monotonic_time/0
@external(erlang, "erlang", "monotonic_time")
fn erlang_monotonic_time() -> Int

/// Convert between time units via erlang:convert_time_unit/3
@external(erlang, "erlang", "convert_time_unit")
fn erlang_convert_time_unit(
  time: Int,
  from_unit: TimeUnit,
  to_unit: TimeUnit,
) -> Int

// =============================================================================
// Internal helpers
// =============================================================================

fn unwrap_or(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}
