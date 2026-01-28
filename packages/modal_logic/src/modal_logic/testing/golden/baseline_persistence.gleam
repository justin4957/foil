//// Baseline Persistence for Regression Testing
////
//// This module manages persisted metric baselines for detecting accuracy
//// regressions across runs. It provides:
//// - File-based baseline storage (JSON format via Erlang FFI)
//// - Regression detection with configurable threshold (default 2%)
//// - Metric trend analysis over the last N runs
//// - Conversion from AccuracyResults to BaselineSnapshot
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/testing/golden/baseline_persistence
////
//// // Run baseline check in CI
//// let result = baseline_persistence.run_baseline_check(
////   accuracy_results, "results/baseline.json", 0.02, None,
//// )
//// ```

import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/testing/accuracy/accuracy_tests.{type AccuracyResults}

// =============================================================================
// Types
// =============================================================================

/// A persisted snapshot of accuracy metrics at a point in time
pub type BaselineSnapshot {
  BaselineSnapshot(
    /// Schema version for forward compatibility
    schema_version: Int,
    /// When this snapshot was generated
    timestamp: String,
    /// Git commit hash (if available)
    git_commit: Option(String),
    /// Overall F1 score (0.0–1.0)
    f1_score: Float,
    /// Overall accuracy (0.0–1.0)
    accuracy: Float,
    /// Precision (0.0–1.0)
    precision: Float,
    /// Recall (0.0–1.0)
    recall: Float,
    /// Translation exact match rate (0.0–1.0)
    translation_accuracy: Float,
    /// Logic system detection accuracy (0.0–1.0)
    logic_detection_accuracy: Float,
    /// Per-system F1 scores
    per_system_f1: List(#(String, Float)),
    /// Per-complexity-bucket F1 scores
    per_complexity_f1: List(#(String, Float)),
    /// Number of test cases used
    total_cases: Int,
  )
}

/// A baseline file containing the current baseline and run history
pub type BaselineFile {
  BaselineFile(
    /// Current approved baseline for regression comparison
    current_baseline: BaselineSnapshot,
    /// History of recent runs (newest first, capped at max_history)
    run_history: List(BaselineSnapshot),
    /// Maximum number of history entries to retain
    max_history: Int,
  )
}

/// Result of comparing current metrics against a persisted baseline
pub type BaselineRegressionResult {
  BaselineRegressionResult(
    /// Whether any metric regressed beyond the threshold
    has_regression: Bool,
    /// F1 score delta (current - baseline)
    f1_delta: Float,
    /// Accuracy delta (current - baseline)
    accuracy_delta: Float,
    /// Precision delta
    precision_delta: Float,
    /// Recall delta
    recall_delta: Float,
    /// Per-system regressions where delta < -threshold
    system_regressions: List(#(String, Float)),
    /// Overall regression severity
    severity: RegressionSeverity,
    /// Human-readable summary
    summary: String,
  )
}

/// Regression severity classification
pub type RegressionSeverity {
  /// No regression detected
  NoRegression
  /// Minor regression (< threshold, not blocking)
  MinorRegression
  /// Major regression (threshold to 5%)
  MajorRegression
  /// Critical regression (> 5%)
  CriticalRegression
}

/// Metric trend over recent runs
pub type MetricTrend {
  MetricTrend(
    /// Overall trend direction
    direction: TrendDirection,
    /// Average change per run
    average_change_per_run: Float,
    /// Number of runs analyzed
    runs_analyzed: Int,
    /// Most recent value
    latest_value: Float,
    /// Oldest value in the window
    oldest_value: Float,
  )
}

/// Direction of a metric trend
pub type TrendDirection {
  /// Metric is improving over time
  TrendImproving
  /// Metric is declining over time
  TrendDeclining
  /// Metric is stable (within noise threshold)
  TrendStable
}

/// Errors from baseline operations
pub type BaselineError {
  /// Could not read the baseline file
  FileReadError(path: String, reason: String)
  /// Could not write the baseline file
  FileWriteError(path: String, reason: String)
  /// Could not parse JSON content
  JsonParseError(details: String)
  /// No baseline file exists at the path
  NoBaselineFound(path: String)
}

// =============================================================================
// Erlang FFI for File I/O
// =============================================================================

@external(erlang, "baseline_ffi", "read_file")
fn erlang_read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "baseline_ffi", "write_file")
fn erlang_write_file(path: String, data: BitArray) -> Result(Nil, Dynamic)

@external(erlang, "baseline_ffi", "ensure_dir")
fn erlang_ensure_dir(path: String) -> Result(Nil, Dynamic)

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float

// =============================================================================
// File I/O
// =============================================================================

/// Read a baseline file from disk
pub fn read_baseline(path: String) -> Result(BaselineFile, BaselineError) {
  case erlang_read_file(path) {
    Ok(bits) ->
      case bit_array.to_string(bits) {
        Ok(content) -> baseline_file_from_json(content)
        Error(_) -> Error(FileReadError(path, "Invalid UTF-8"))
      }
    Error(_) -> Error(NoBaselineFound(path))
  }
}

/// Write a baseline file to disk
pub fn write_baseline(
  path: String,
  baseline_file: BaselineFile,
) -> Result(Nil, BaselineError) {
  let _ = erlang_ensure_dir(path)
  let json_string = baseline_file_to_json(baseline_file)
  let bits = bit_array.from_string(json_string)
  case erlang_write_file(path, bits) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(FileWriteError(path, "Could not write file"))
  }
}

// =============================================================================
// Snapshot Creation
// =============================================================================

/// Create a BaselineSnapshot from AccuracyResults
pub fn snapshot_from_accuracy_results(
  results: AccuracyResults,
  timestamp: String,
  git_commit: Option(String),
) -> BaselineSnapshot {
  let translation_accuracy = case results.translation.total {
    0 -> 0.0
    total ->
      int.to_float(results.translation.exact_matches) /. int.to_float(total)
  }

  let logic_detection_accuracy = case results.logic_detection.total {
    0 -> 0.0
    total ->
      int.to_float(results.logic_detection.correct) /. int.to_float(total)
  }

  let per_system_f1 =
    list.map(results.validation_by_system, fn(entry) {
      let #(name, metrics) = entry
      #(name, metrics.f1_score)
    })

  let per_complexity_f1 =
    list.map(results.validation_by_complexity, fn(entry) {
      let #(name, metrics) = entry
      #(name, metrics.f1_score)
    })

  BaselineSnapshot(
    schema_version: 1,
    timestamp: timestamp,
    git_commit: git_commit,
    f1_score: results.validation.f1_score,
    accuracy: results.overall.accuracy /. 100.0,
    precision: results.validation.precision,
    recall: results.validation.recall,
    translation_accuracy: translation_accuracy,
    logic_detection_accuracy: logic_detection_accuracy,
    per_system_f1: per_system_f1,
    per_complexity_f1: per_complexity_f1,
    total_cases: results.overall.total_tests,
  )
}

// =============================================================================
// Regression Detection
// =============================================================================

/// Compare current metrics against a baseline snapshot
///
/// The threshold parameter sets the minimum drop required to flag a regression.
/// Default is 0.02 (2%), matching benchmark_runner.compare_to_baseline().
pub fn check_regression(
  current: BaselineSnapshot,
  baseline: BaselineSnapshot,
  threshold: Float,
) -> BaselineRegressionResult {
  let f1_delta = current.f1_score -. baseline.f1_score
  let accuracy_delta = current.accuracy -. baseline.accuracy
  let precision_delta = current.precision -. baseline.precision
  let recall_delta = current.recall -. baseline.recall
  let negative_threshold = float.negate(threshold)

  // Check per-system regressions
  let system_regressions =
    baseline.per_system_f1
    |> list.filter_map(fn(baseline_entry) {
      let #(system_name, baseline_f1) = baseline_entry
      let current_f1 =
        list.find(current.per_system_f1, fn(entry) { entry.0 == system_name })
        |> result.map(fn(entry) { entry.1 })
        |> result.unwrap(0.0)
      let delta = current_f1 -. baseline_f1
      case delta <. negative_threshold {
        True -> Ok(#(system_name, delta))
        False -> Error(Nil)
      }
    })

  // Determine if any metric regressed beyond threshold
  let has_regression =
    f1_delta <. negative_threshold
    || accuracy_delta <. negative_threshold
    || list.length(system_regressions) > 0

  // Determine severity from worst delta
  let worst_delta = float.min(f1_delta, accuracy_delta)
  let severity = classify_severity(worst_delta, negative_threshold)

  let summary =
    format_regression_summary(
      f1_delta,
      accuracy_delta,
      system_regressions,
      severity,
    )

  BaselineRegressionResult(
    has_regression: has_regression,
    f1_delta: f1_delta,
    accuracy_delta: accuracy_delta,
    precision_delta: precision_delta,
    recall_delta: recall_delta,
    system_regressions: system_regressions,
    severity: severity,
    summary: summary,
  )
}

fn classify_severity(
  worst_delta: Float,
  negative_threshold: Float,
) -> RegressionSeverity {
  case worst_delta >=. 0.0 {
    True -> NoRegression
    False ->
      case worst_delta >=. negative_threshold {
        True -> MinorRegression
        False ->
          // Use -0.051 to avoid float boundary issues at exactly -0.05
          case worst_delta >. -0.051 {
            True -> MajorRegression
            False -> CriticalRegression
          }
      }
  }
}

fn format_regression_summary(
  f1_delta: Float,
  accuracy_delta: Float,
  system_regressions: List(#(String, Float)),
  severity: RegressionSeverity,
) -> String {
  let severity_label = case severity {
    NoRegression -> "No regression"
    MinorRegression -> "Minor regression"
    MajorRegression -> "Major regression"
    CriticalRegression -> "Critical regression"
  }

  let system_details = case system_regressions {
    [] -> ""
    regressions ->
      ". System regressions: "
      <> string.join(
        list.map(regressions, fn(entry) {
          let #(name, delta) = entry
          name <> " (" <> format_delta(delta) <> ")"
        }),
        ", ",
      )
  }

  severity_label
  <> ". F1 delta: "
  <> format_delta(f1_delta)
  <> ", Accuracy delta: "
  <> format_delta(accuracy_delta)
  <> system_details
}

fn format_delta(delta: Float) -> String {
  let sign = case delta >=. 0.0 {
    True -> "+"
    False -> ""
  }
  sign <> float_to_string_2dp(delta *. 100.0) <> "%"
}

fn float_to_string_2dp(value: Float) -> String {
  let rounded = float.round(value *. 100.0)
  let whole = rounded / 100
  let frac = case rounded % 100 {
    f if f < 0 -> -f
    f -> f
  }
  int.to_string(whole)
  <> "."
  <> case frac < 10 {
    True -> "0" <> int.to_string(frac)
    False -> int.to_string(frac)
  }
}

// =============================================================================
// Baseline File Management
// =============================================================================

/// Update a baseline file with a new run
///
/// Adds the current run to history (capped at max_history).
/// If promote is True, also replaces the current_baseline.
pub fn update_baseline_file(
  baseline_file: BaselineFile,
  current_run: BaselineSnapshot,
  promote: Bool,
) -> BaselineFile {
  let updated_history =
    [current_run, ..baseline_file.run_history]
    |> list.take(baseline_file.max_history)

  let updated_baseline = case promote {
    True -> current_run
    False -> baseline_file.current_baseline
  }

  BaselineFile(
    current_baseline: updated_baseline,
    run_history: updated_history,
    max_history: baseline_file.max_history,
  )
}

/// Initialize a new baseline file from accuracy results
pub fn initialize_baseline(
  results: AccuracyResults,
  path: String,
  git_commit: Option(String),
) -> Result(BaselineFile, BaselineError) {
  let timestamp = get_timestamp()
  let snapshot = snapshot_from_accuracy_results(results, timestamp, git_commit)
  let baseline_file =
    BaselineFile(
      current_baseline: snapshot,
      run_history: [snapshot],
      max_history: 10,
    )
  case write_baseline(path, baseline_file) {
    Ok(_) -> Ok(baseline_file)
    Error(err) -> Error(err)
  }
}

// =============================================================================
// Trend Analysis
// =============================================================================

/// Compute the trend of a metric over recent runs
///
/// The extractor function selects which metric to analyze from each snapshot.
/// Example: `compute_metric_trend(history, fn(s) { s.f1_score })`
pub fn compute_metric_trend(
  history: List(BaselineSnapshot),
  metric_extractor: fn(BaselineSnapshot) -> Float,
) -> MetricTrend {
  let values = list.map(history, metric_extractor)
  let count = list.length(values)

  case count {
    0 ->
      MetricTrend(
        direction: TrendStable,
        average_change_per_run: 0.0,
        runs_analyzed: 0,
        latest_value: 0.0,
        oldest_value: 0.0,
      )
    1 ->
      MetricTrend(
        direction: TrendStable,
        average_change_per_run: 0.0,
        runs_analyzed: 1,
        latest_value: list.first(values) |> result.unwrap(0.0),
        oldest_value: list.first(values) |> result.unwrap(0.0),
      )
    _ -> {
      let latest = list.first(values) |> result.unwrap(0.0)
      let oldest = list.last(values) |> result.unwrap(0.0)
      let total_change = latest -. oldest
      let avg_change = total_change /. int.to_float(count - 1)
      let direction = case avg_change >. 0.01 {
        True -> TrendImproving
        False ->
          case avg_change <. -0.01 {
            True -> TrendDeclining
            False -> TrendStable
          }
      }

      MetricTrend(
        direction: direction,
        average_change_per_run: avg_change,
        runs_analyzed: count,
        latest_value: latest,
        oldest_value: oldest,
      )
    }
  }
}

/// Format trend direction as string
pub fn trend_direction_to_string(direction: TrendDirection) -> String {
  case direction {
    TrendImproving -> "improving"
    TrendDeclining -> "declining"
    TrendStable -> "stable"
  }
}

/// Format severity as string
pub fn severity_to_string(severity: RegressionSeverity) -> String {
  case severity {
    NoRegression -> "none"
    MinorRegression -> "minor"
    MajorRegression -> "major"
    CriticalRegression -> "critical"
  }
}

// =============================================================================
// Top-Level CI Workflow
// =============================================================================

/// Run a complete baseline regression check
///
/// Reads the baseline from disk, compares current results, updates
/// history, and writes back. If no baseline exists, initializes one.
pub fn run_baseline_check(
  results: AccuracyResults,
  baseline_path: String,
  regression_threshold: Float,
  git_commit: Option(String),
) -> Result(BaselineRegressionResult, BaselineError) {
  let timestamp = get_timestamp()
  let current_snapshot =
    snapshot_from_accuracy_results(results, timestamp, git_commit)

  case read_baseline(baseline_path) {
    Ok(baseline_file) -> {
      let regression_result =
        check_regression(
          current_snapshot,
          baseline_file.current_baseline,
          regression_threshold,
        )

      // Add current run to history without promoting as new baseline
      let updated_file =
        update_baseline_file(baseline_file, current_snapshot, False)

      case write_baseline(baseline_path, updated_file) {
        Ok(_) -> Ok(regression_result)
        Error(write_err) -> Error(write_err)
      }
    }
    Error(NoBaselineFound(_)) -> {
      // Initialize baseline from current run
      case initialize_baseline(results, baseline_path, git_commit) {
        Ok(_) ->
          Ok(BaselineRegressionResult(
            has_regression: False,
            f1_delta: 0.0,
            accuracy_delta: 0.0,
            precision_delta: 0.0,
            recall_delta: 0.0,
            system_regressions: [],
            severity: NoRegression,
            summary: "Baseline initialized from current run",
          ))
        Error(err) -> Error(err)
      }
    }
    Error(other_err) -> Error(other_err)
  }
}

// =============================================================================
// JSON Encoding
// =============================================================================

/// Encode a BaselineFile to JSON string
pub fn baseline_file_to_json(baseline_file: BaselineFile) -> String {
  encode_baseline_file(baseline_file)
  |> json.to_string
}

/// Encode a BaselineSnapshot to JSON string
pub fn snapshot_to_json(snapshot: BaselineSnapshot) -> String {
  encode_baseline_snapshot(snapshot)
  |> json.to_string
}

fn encode_baseline_file(baseline_file: BaselineFile) -> Json {
  json.object([
    #("schema_version", json.int(1)),
    #(
      "current_baseline",
      encode_baseline_snapshot(baseline_file.current_baseline),
    ),
    #(
      "run_history",
      json.array(baseline_file.run_history, encode_baseline_snapshot),
    ),
    #("max_history", json.int(baseline_file.max_history)),
  ])
}

fn encode_baseline_snapshot(snapshot: BaselineSnapshot) -> Json {
  json.object([
    #("schema_version", json.int(snapshot.schema_version)),
    #("timestamp", json.string(snapshot.timestamp)),
    #("git_commit", case snapshot.git_commit {
      Some(commit) -> json.string(commit)
      None -> json.null()
    }),
    #("f1_score", json.float(snapshot.f1_score)),
    #("accuracy", json.float(snapshot.accuracy)),
    #("precision", json.float(snapshot.precision)),
    #("recall", json.float(snapshot.recall)),
    #("translation_accuracy", json.float(snapshot.translation_accuracy)),
    #("logic_detection_accuracy", json.float(snapshot.logic_detection_accuracy)),
    #("per_system_f1", json.array(snapshot.per_system_f1, encode_named_metric)),
    #(
      "per_complexity_f1",
      json.array(snapshot.per_complexity_f1, encode_named_metric),
    ),
    #("total_cases", json.int(snapshot.total_cases)),
  ])
}

fn encode_named_metric(entry: #(String, Float)) -> Json {
  let #(name, value) = entry
  json.object([#("name", json.string(name)), #("value", json.float(value))])
}

// =============================================================================
// JSON Decoding
// =============================================================================

import gleam/dynamic/decode

/// Decode a BaselineFile from JSON string
pub fn baseline_file_from_json(
  json_string: String,
) -> Result(BaselineFile, BaselineError) {
  case json.parse(json_string, baseline_file_decoder()) {
    Ok(baseline_file) -> Ok(baseline_file)
    Error(_) -> Error(JsonParseError("Failed to parse baseline JSON"))
  }
}

fn baseline_file_decoder() -> decode.Decoder(BaselineFile) {
  use current_baseline <- decode.field(
    "current_baseline",
    baseline_snapshot_decoder(),
  )
  use run_history <- decode.optional_field(
    "run_history",
    [],
    decode.list(baseline_snapshot_decoder()),
  )
  use max_history <- decode.optional_field("max_history", 10, decode.int)

  decode.success(BaselineFile(
    current_baseline: current_baseline,
    run_history: run_history,
    max_history: max_history,
  ))
}

fn baseline_snapshot_decoder() -> decode.Decoder(BaselineSnapshot) {
  use schema_version <- decode.optional_field("schema_version", 1, decode.int)
  use timestamp <- decode.field("timestamp", decode.string)
  use git_commit <- decode.field("git_commit", decode.optional(decode.string))
  use f1_score <- decode.field("f1_score", float_or_int_decoder())
  use accuracy <- decode.field("accuracy", float_or_int_decoder())
  use precision <- decode.field("precision", float_or_int_decoder())
  use recall <- decode.field("recall", float_or_int_decoder())
  use translation_accuracy <- decode.field(
    "translation_accuracy",
    float_or_int_decoder(),
  )
  use logic_detection_accuracy <- decode.field(
    "logic_detection_accuracy",
    float_or_int_decoder(),
  )
  use per_system_f1 <- decode.optional_field(
    "per_system_f1",
    [],
    decode.list(named_metric_decoder()),
  )
  use per_complexity_f1 <- decode.optional_field(
    "per_complexity_f1",
    [],
    decode.list(named_metric_decoder()),
  )
  use total_cases <- decode.field("total_cases", decode.int)

  decode.success(BaselineSnapshot(
    schema_version: schema_version,
    timestamp: timestamp,
    git_commit: git_commit,
    f1_score: f1_score,
    accuracy: accuracy,
    precision: precision,
    recall: recall,
    translation_accuracy: translation_accuracy,
    logic_detection_accuracy: logic_detection_accuracy,
    per_system_f1: per_system_f1,
    per_complexity_f1: per_complexity_f1,
    total_cases: total_cases,
  ))
}

fn named_metric_decoder() -> decode.Decoder(#(String, Float)) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", float_or_int_decoder())
  decode.success(#(name, value))
}

/// Decoder that accepts both float and int as float
fn float_or_int_decoder() -> decode.Decoder(Float) {
  decode.one_of(decode.float, [
    decode.then(decode.int, fn(i) { decode.success(int_to_float(i)) }),
  ])
}

// =============================================================================
// Utilities
// =============================================================================

fn get_timestamp() -> String {
  let epoch_ms = erlang_system_time_ms()
  "epoch_ms:" <> int.to_string(epoch_ms)
}

@external(erlang, "os", "system_time")
fn erlang_system_time_ms() -> Int
