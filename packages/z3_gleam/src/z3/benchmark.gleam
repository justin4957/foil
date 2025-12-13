//// Benchmark Module
////
//// This module provides benchmarking utilities for Z3 solver operations.
//// It enables profiling and optimization of critical paths in the solver.
////
//// ## Benchmark Categories
////
//// - **Expression Building**: Time to construct SMT expressions
//// - **SAT Checking**: Time to check satisfiability
//// - **Modal Validity**: Time to check modal logic validity
//// - **Incremental Solving**: Performance with push/pop operations
////
//// ## Usage
////
//// ```gleam
//// import z3/benchmark
////
//// // Run a benchmark
//// let result = benchmark.run("my_test", fn() { expensive_operation() })
////
//// // Get timing information
//// io.println("Elapsed: " <> benchmark.format_duration(result.elapsed_ns))
//// ```

import gleam/dict.{type Dict}
import gleam/list

// =============================================================================
// Types
// =============================================================================

/// Result of a single benchmark run
pub type BenchmarkResult {
  BenchmarkResult(
    /// Name of the benchmark
    name: String,
    /// Elapsed time in nanoseconds
    elapsed_ns: Int,
    /// Number of iterations
    iterations: Int,
    /// Whether the benchmark succeeded
    success: Bool,
    /// Additional metadata
    metadata: Dict(String, String),
  )
}

/// Aggregated benchmark statistics
pub type BenchmarkStats {
  BenchmarkStats(
    /// Name of the benchmark
    name: String,
    /// Number of samples
    samples: Int,
    /// Minimum time in nanoseconds
    min_ns: Int,
    /// Maximum time in nanoseconds
    max_ns: Int,
    /// Mean time in nanoseconds
    mean_ns: Int,
    /// Standard deviation in nanoseconds
    std_dev_ns: Int,
    /// Median time in nanoseconds
    median_ns: Int,
    /// 95th percentile time in nanoseconds
    p95_ns: Int,
    /// Throughput (operations per second)
    ops_per_sec: Float,
  )
}

/// Benchmark suite for running multiple benchmarks
pub type BenchmarkSuite {
  BenchmarkSuite(
    /// Suite name
    name: String,
    /// Individual benchmark results
    results: List(BenchmarkResult),
    /// Aggregated statistics
    stats: Dict(String, BenchmarkStats),
  )
}

/// Configuration for benchmark runs
pub type BenchmarkConfig {
  BenchmarkConfig(
    /// Number of warmup iterations (results discarded)
    warmup_iterations: Int,
    /// Number of measured iterations
    iterations: Int,
    /// Minimum run time in milliseconds
    min_runtime_ms: Int,
    /// Whether to collect memory statistics
    collect_memory: Bool,
  )
}

/// Memory statistics (if available)
pub type MemoryStats {
  MemoryStats(
    /// Memory used before benchmark
    before_bytes: Int,
    /// Memory used after benchmark
    after_bytes: Int,
    /// Peak memory usage during benchmark
    peak_bytes: Int,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Default benchmark configuration
pub fn default_config() -> BenchmarkConfig {
  BenchmarkConfig(
    warmup_iterations: 3,
    iterations: 10,
    min_runtime_ms: 100,
    collect_memory: False,
  )
}

/// Quick benchmark configuration (fewer iterations)
pub fn quick_config() -> BenchmarkConfig {
  BenchmarkConfig(
    warmup_iterations: 1,
    iterations: 5,
    min_runtime_ms: 50,
    collect_memory: False,
  )
}

/// Thorough benchmark configuration (more iterations)
pub fn thorough_config() -> BenchmarkConfig {
  BenchmarkConfig(
    warmup_iterations: 10,
    iterations: 100,
    min_runtime_ms: 1000,
    collect_memory: True,
  )
}

// =============================================================================
// Single Benchmark Execution
// =============================================================================

/// Run a single benchmark iteration and return the result
pub fn run(name: String, operation: fn() -> a) -> BenchmarkResult {
  let start = monotonic_time_ns()
  let _result = operation()
  let end = monotonic_time_ns()
  let elapsed = end - start

  BenchmarkResult(
    name: name,
    elapsed_ns: elapsed,
    iterations: 1,
    success: True,
    metadata: dict.new(),
  )
}

/// Run a benchmark with multiple iterations
pub fn run_n(
  name: String,
  iterations: Int,
  operation: fn() -> a,
) -> BenchmarkResult {
  let start = monotonic_time_ns()
  run_iterations(iterations, operation)
  let end = monotonic_time_ns()
  let total_elapsed = end - start

  BenchmarkResult(
    name: name,
    elapsed_ns: total_elapsed / iterations,
    iterations: iterations,
    success: True,
    metadata: dict.new(),
  )
}

/// Run a benchmark with configuration
pub fn run_with_config(
  name: String,
  config: BenchmarkConfig,
  operation: fn() -> a,
) -> BenchmarkResult {
  // Warmup phase
  run_iterations(config.warmup_iterations, operation)

  // Measurement phase
  let start = monotonic_time_ns()
  run_iterations(config.iterations, operation)
  let end = monotonic_time_ns()
  let total_elapsed = end - start

  BenchmarkResult(
    name: name,
    elapsed_ns: total_elapsed / config.iterations,
    iterations: config.iterations,
    success: True,
    metadata: dict.new(),
  )
}

fn run_iterations(n: Int, operation: fn() -> a) -> Nil {
  case n <= 0 {
    True -> Nil
    False -> {
      let _result = operation()
      run_iterations(n - 1, operation)
    }
  }
}

// =============================================================================
// Statistics Collection
// =============================================================================

/// Collect multiple samples and compute statistics
pub fn collect_stats(
  name: String,
  samples: Int,
  operation: fn() -> a,
) -> BenchmarkStats {
  let times = collect_samples(samples, operation, [])
  compute_stats(name, times)
}

fn collect_samples(
  remaining: Int,
  operation: fn() -> a,
  acc: List(Int),
) -> List(Int) {
  case remaining <= 0 {
    True -> acc
    False -> {
      let start = monotonic_time_ns()
      let _result = operation()
      let end = monotonic_time_ns()
      let elapsed = end - start
      collect_samples(remaining - 1, operation, [elapsed, ..acc])
    }
  }
}

fn compute_stats(name: String, times: List(Int)) -> BenchmarkStats {
  let sorted = list.sort(times, int_compare)
  let n = list.length(sorted)

  case n {
    0 ->
      BenchmarkStats(
        name: name,
        samples: 0,
        min_ns: 0,
        max_ns: 0,
        mean_ns: 0,
        std_dev_ns: 0,
        median_ns: 0,
        p95_ns: 0,
        ops_per_sec: 0.0,
      )
    _ -> {
      let min = list_min(sorted)
      let max = list_max(sorted)
      let sum = list_sum(sorted)
      let mean = sum / n
      let variance = compute_variance(sorted, mean)
      let std_dev = int_sqrt(variance)
      let median = list_nth(sorted, n / 2)
      let p95 = list_nth(sorted, n * 95 / 100)
      let ops_per_sec = case mean {
        0 -> 0.0
        _ -> 1_000_000_000.0 /. int_to_float(mean)
      }

      BenchmarkStats(
        name: name,
        samples: n,
        min_ns: min,
        max_ns: max,
        mean_ns: mean,
        std_dev_ns: std_dev,
        median_ns: median,
        p95_ns: p95,
        ops_per_sec: ops_per_sec,
      )
    }
  }
}

fn compute_variance(times: List(Int), mean: Int) -> Int {
  let sum_sq =
    list.fold(times, 0, fn(acc, t) {
      let diff = t - mean
      acc + diff * diff
    })
  let n = list.length(times)
  case n {
    0 -> 0
    _ -> sum_sq / n
  }
}

// =============================================================================
// Benchmark Suite
// =============================================================================

/// Create a new benchmark suite
pub fn new_suite(name: String) -> BenchmarkSuite {
  BenchmarkSuite(name: name, results: [], stats: dict.new())
}

/// Add a benchmark result to the suite
pub fn add_result(
  suite: BenchmarkSuite,
  result: BenchmarkResult,
) -> BenchmarkSuite {
  BenchmarkSuite(..suite, results: [result, ..suite.results])
}

/// Add benchmark statistics to the suite
pub fn add_stats(suite: BenchmarkSuite, stats: BenchmarkStats) -> BenchmarkSuite {
  BenchmarkSuite(..suite, stats: dict.insert(suite.stats, stats.name, stats))
}

/// Run a benchmark and add it to the suite
pub fn run_in_suite(
  suite: BenchmarkSuite,
  name: String,
  operation: fn() -> a,
) -> BenchmarkSuite {
  let result = run(name, operation)
  add_result(suite, result)
}

/// Collect statistics and add to the suite
pub fn collect_in_suite(
  suite: BenchmarkSuite,
  name: String,
  samples: Int,
  operation: fn() -> a,
) -> BenchmarkSuite {
  let stats = collect_stats(name, samples, operation)
  add_stats(suite, stats)
}

// =============================================================================
// Formatting
// =============================================================================

/// Format a duration in nanoseconds to a human-readable string
pub fn format_duration(ns: Int) -> String {
  case ns {
    _ if ns < 1000 -> int_to_string(ns) <> "ns"
    _ if ns < 1_000_000 -> int_to_string(ns / 1000) <> "µs"
    _ if ns < 1_000_000_000 -> int_to_string(ns / 1_000_000) <> "ms"
    _ -> int_to_string(ns / 1_000_000_000) <> "s"
  }
}

/// Format a benchmark result
pub fn format_result(result: BenchmarkResult) -> String {
  result.name
  <> ": "
  <> format_duration(result.elapsed_ns)
  <> " ("
  <> int_to_string(result.iterations)
  <> " iterations)"
}

/// Format benchmark statistics
pub fn format_stats(stats: BenchmarkStats) -> String {
  stats.name
  <> ":\n"
  <> "  samples: "
  <> int_to_string(stats.samples)
  <> "\n"
  <> "  min: "
  <> format_duration(stats.min_ns)
  <> "\n"
  <> "  max: "
  <> format_duration(stats.max_ns)
  <> "\n"
  <> "  mean: "
  <> format_duration(stats.mean_ns)
  <> "\n"
  <> "  median: "
  <> format_duration(stats.median_ns)
  <> "\n"
  <> "  p95: "
  <> format_duration(stats.p95_ns)
  <> "\n"
  <> "  std_dev: "
  <> format_duration(stats.std_dev_ns)
  <> "\n"
  <> "  ops/sec: "
  <> float_to_string(stats.ops_per_sec)
}

/// Format a benchmark suite
pub fn format_suite(suite: BenchmarkSuite) -> String {
  let header = "=== Benchmark Suite: " <> suite.name <> " ===\n\n"

  let results_section = case suite.results {
    [] -> ""
    results -> {
      let formatted =
        list.map(list.reverse(results), format_result)
        |> join_strings("\n")
      "Results:\n" <> formatted <> "\n\n"
    }
  }

  let stats_section = case dict.size(suite.stats) {
    0 -> ""
    _ -> {
      let formatted =
        dict.values(suite.stats)
        |> list.map(format_stats)
        |> join_strings("\n")
      "Statistics:\n" <> formatted <> "\n"
    }
  }

  header <> results_section <> stats_section
}

// =============================================================================
// Comparison Utilities
// =============================================================================

/// Compare two benchmark results
pub fn compare_results(
  baseline: BenchmarkResult,
  current: BenchmarkResult,
) -> String {
  let diff = current.elapsed_ns - baseline.elapsed_ns
  let pct_change = case baseline.elapsed_ns {
    0 -> 0.0
    _ -> int_to_float(diff * 100) /. int_to_float(baseline.elapsed_ns)
  }

  let change_str = case diff > 0 {
    True -> "+" <> format_duration(diff) <> " (slower)"
    False -> format_duration(-diff) <> " (faster)"
  }

  baseline.name
  <> ": "
  <> format_duration(baseline.elapsed_ns)
  <> " -> "
  <> format_duration(current.elapsed_ns)
  <> " ("
  <> change_str
  <> ", "
  <> float_to_string(pct_change)
  <> "%)"
}

/// Check if current is faster than baseline by at least the given percentage
pub fn is_faster_by(
  baseline: BenchmarkResult,
  current: BenchmarkResult,
  pct: Float,
) -> Bool {
  let threshold = int_to_float(baseline.elapsed_ns) *. { 1.0 -. pct /. 100.0 }
  int_to_float(current.elapsed_ns) <. threshold
}

/// Check if current is slower than baseline by more than the given percentage
pub fn is_slower_by(
  baseline: BenchmarkResult,
  current: BenchmarkResult,
  pct: Float,
) -> Bool {
  let threshold = int_to_float(baseline.elapsed_ns) *. { 1.0 +. pct /. 100.0 }
  int_to_float(current.elapsed_ns) >. threshold
}

// =============================================================================
// Internal Helpers
// =============================================================================

fn int_compare(a: Int, b: Int) -> order.Order {
  case a < b {
    True -> order.Lt
    False ->
      case a > b {
        True -> order.Gt
        False -> order.Eq
      }
  }
}

fn list_min(l: List(Int)) -> Int {
  case l {
    [] -> 0
    [x] -> x
    [x, ..rest] -> {
      let rest_min = list_min(rest)
      case x < rest_min {
        True -> x
        False -> rest_min
      }
    }
  }
}

fn list_max(l: List(Int)) -> Int {
  case l {
    [] -> 0
    [x] -> x
    [x, ..rest] -> {
      let rest_max = list_max(rest)
      case x > rest_max {
        True -> x
        False -> rest_max
      }
    }
  }
}

fn list_sum(l: List(Int)) -> Int {
  list.fold(l, 0, fn(acc, x) { acc + x })
}

fn list_nth(l: List(Int), n: Int) -> Int {
  case l, n {
    [], _ -> 0
    [x, ..], 0 -> x
    [_, ..rest], _ -> list_nth(rest, n - 1)
  }
}

fn int_sqrt(n: Int) -> Int {
  // Integer square root approximation
  case n <= 0 {
    True -> 0
    False -> do_int_sqrt(n, n)
  }
}

fn do_int_sqrt(n: Int, guess: Int) -> Int {
  let next = { guess + n / guess } / 2
  case next >= guess {
    True -> guess
    False -> do_int_sqrt(n, next)
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

fn join_strings(strings: List(String), sep: String) -> String {
  case strings {
    [] -> ""
    [s] -> s
    [s, ..rest] -> s <> sep <> join_strings(rest, sep)
  }
}

// External functions for timing
@external(erlang, "erlang", "monotonic_time")
fn monotonic_time_ns() -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float

fn float_to_string(f: Float) -> String {
  // Simple integer approximation for display
  let int_part = float_to_int(f)
  int_to_string(int_part)
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

// Import order module for comparison
import gleam/order
