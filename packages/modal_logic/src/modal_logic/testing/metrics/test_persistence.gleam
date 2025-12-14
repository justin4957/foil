//// Test Persistence for Autonomous Testing
////
//// This module stores and retrieves test run results for tracking
//// trends, detecting regressions, and comparing against baselines.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/testing/metrics/test_report.{
  type BaselineComparison, type ReportSummary, type TestReport,
  BaselineComparison,
}

/// Persisted test run record
pub type TestRun {
  TestRun(
    /// Unique run identifier
    id: String,
    /// When the run completed
    timestamp: String,
    /// Summary metrics
    summary: ReportSummary,
    /// Git commit hash (if available)
    git_commit: Option(String),
    /// Git branch (if available)
    git_branch: Option(String),
    /// List of passing fixture IDs
    passing_ids: List(String),
    /// List of failing fixture IDs
    failing_ids: List(String),
  )
}

/// Store of test runs
pub type TestStore {
  TestStore(
    /// All runs keyed by ID
    runs: Dict(String, TestRun),
    /// Current baseline ID
    current_baseline: Option(String),
    /// Run history (newest first)
    history: List(String),
  )
}

/// Create a new empty store
pub fn new_store() -> TestStore {
  TestStore(runs: dict.new(), current_baseline: None, history: [])
}

/// Add a test run to the store
pub fn add_run(store: TestStore, run: TestRun) -> TestStore {
  let runs = dict.insert(store.runs, run.id, run)
  let history = [run.id, ..store.history]
  TestStore(..store, runs: runs, history: history)
}

/// Get a run by ID
pub fn get_run(store: TestStore, id: String) -> Option(TestRun) {
  case dict.get(store.runs, id) {
    Ok(run) -> Some(run)
    Error(_) -> None
  }
}

/// Get the most recent run
pub fn get_latest(store: TestStore) -> Option(TestRun) {
  case store.history {
    [] -> None
    [id, ..] -> get_run(store, id)
  }
}

/// Get the current baseline
pub fn get_baseline(store: TestStore) -> Option(TestRun) {
  case store.current_baseline {
    None -> None
    Some(id) -> get_run(store, id)
  }
}

/// Set the current baseline
pub fn set_baseline(store: TestStore, id: String) -> TestStore {
  TestStore(..store, current_baseline: Some(id))
}

/// Get recent runs
pub fn get_recent(store: TestStore, count: Int) -> List(TestRun) {
  store.history
  |> list.take(count)
  |> list.filter_map(fn(id) {
    case get_run(store, id) {
      Some(run) -> Ok(run)
      None -> Error(Nil)
    }
  })
}

/// Compare a run against the baseline
pub fn compare_to_baseline(
  store: TestStore,
  run: TestRun,
) -> Option(BaselineComparison) {
  case get_baseline(store) {
    None -> None
    Some(baseline) -> Some(compare_runs(baseline, run))
  }
}

/// Compare two runs
pub fn compare_runs(baseline: TestRun, current: TestRun) -> BaselineComparison {
  // Find regressions: tests that passed in baseline but fail now
  let regressions =
    list.filter(baseline.passing_ids, fn(id) {
      list.contains(current.failing_ids, id)
    })

  // Find improvements: tests that failed in baseline but pass now
  let improvements =
    list.filter(baseline.failing_ids, fn(id) {
      list.contains(current.passing_ids, id)
    })

  // Find new tests: in current but not in baseline
  let all_baseline = list.flatten([baseline.passing_ids, baseline.failing_ids])
  let all_current = list.flatten([current.passing_ids, current.failing_ids])
  let new_tests =
    list.filter(all_current, fn(id) { !list.contains(all_baseline, id) })

  // Calculate pass rate delta
  let pass_rate_delta = current.summary.pass_rate -. baseline.summary.pass_rate

  BaselineComparison(
    baseline_id: baseline.id,
    baseline_date: baseline.timestamp,
    regressions: regressions,
    improvements: improvements,
    new_tests: new_tests,
    pass_rate_delta: pass_rate_delta,
  )
}

/// Check if run has regressions compared to baseline
pub fn has_regressions(store: TestStore, run: TestRun) -> Bool {
  case compare_to_baseline(store, run) {
    None -> False
    Some(comparison) -> list.length(comparison.regressions) > 0
  }
}

/// Calculate trend over recent runs
pub fn calculate_trend(store: TestStore, count: Int) -> Trend {
  let runs = get_recent(store, count)

  case runs {
    [] -> Trend(direction: Stable, change: 0.0, runs_analyzed: 0)
    [_] -> Trend(direction: Stable, change: 0.0, runs_analyzed: 1)
    _ -> {
      let rates = list.map(runs, fn(r) { r.summary.pass_rate })
      let first = list.first(rates) |> option_result_to_float
      let last = list.last(rates) |> option_result_to_float
      let change = first -. last

      let direction = case change {
        c if c >. 5.0 -> Improving
        c if c <. -5.0 -> Declining
        _ -> Stable
      }

      Trend(
        direction: direction,
        change: change,
        runs_analyzed: list.length(runs),
      )
    }
  }
}

/// Trend direction
pub type TrendDirection {
  Improving
  Declining
  Stable
}

/// Trend analysis result
pub type Trend {
  Trend(direction: TrendDirection, change: Float, runs_analyzed: Int)
}

/// Helper to convert Result to Float
fn option_result_to_float(result: Result(Float, a)) -> Float {
  case result {
    Ok(f) -> f
    Error(_) -> 0.0
  }
}

/// Serialize store to JSON
pub fn store_to_json(store: TestStore) -> String {
  let runs_json =
    store.runs
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(_id, run) = pair
      run_to_json(run)
    })
    |> string.join(",\n")

  string.concat([
    "{\n",
    "  \"runs\": [\n",
    runs_json,
    "\n  ],\n",
    "  \"current_baseline\": ",
    case store.current_baseline {
      Some(id) -> "\"" <> id <> "\""
      None -> "null"
    },
    ",\n",
    "  \"history\": [",
    store.history
      |> list.map(fn(id) { "\"" <> id <> "\"" })
      |> string.join(", "),
    "]\n",
    "}\n",
  ])
}

/// Serialize a single run to JSON
fn run_to_json(run: TestRun) -> String {
  string.concat([
    "    {\n",
    "      \"id\": \"",
    run.id,
    "\",\n",
    "      \"timestamp\": \"",
    run.timestamp,
    "\",\n",
    "      \"pass_rate\": ",
    float_to_string(run.summary.pass_rate),
    ",\n",
    "      \"total\": ",
    int.to_string(run.summary.total_tests),
    ",\n",
    "      \"passed\": ",
    int.to_string(run.summary.passed),
    ",\n",
    "      \"failed\": ",
    int.to_string(run.summary.failed),
    ",\n",
    "      \"git_commit\": ",
    case run.git_commit {
      Some(c) -> "\"" <> c <> "\""
      None -> "null"
    },
    "\n",
    "    }",
  ])
}

/// Simple float to string
fn float_to_string(f: Float) -> String {
  // Convert to percentage integer
  let pct = f *. 100.0
  int.to_string(truncate_float(pct)) <> ".0"
}

/// Truncate float to int
fn truncate_float(f: Float) -> Int {
  case f >=. 0.0 {
    True -> truncate_positive(f)
    False -> truncate_negative(f)
  }
}

fn truncate_positive(f: Float) -> Int {
  case f <. 1.0 {
    True -> 0
    False -> 1 + truncate_positive(f -. 1.0)
  }
}

fn truncate_negative(f: Float) -> Int {
  case f >. -1.0 {
    True -> 0
    False -> -1 + truncate_negative(f +. 1.0)
  }
}

/// Format trend as string
pub fn trend_to_string(trend: Trend) -> String {
  let direction_str = case trend.direction {
    Improving -> "IMPROVING"
    Declining -> "DECLINING"
    Stable -> "STABLE"
  }

  direction_str
  <> " ("
  <> int.to_string(truncate_float(trend.change *. 100.0))
  <> "% over "
  <> int.to_string(trend.runs_analyzed)
  <> " runs)"
}

/// Create a test run from a report
pub fn run_from_report(
  report: TestReport,
  passing_ids: List(String),
  failing_ids: List(String),
) -> TestRun {
  TestRun(
    id: report.run_id,
    timestamp: report.completed_at,
    summary: report.summary,
    git_commit: report.environment.git_commit,
    git_branch: report.environment.git_branch,
    passing_ids: passing_ids,
    failing_ids: failing_ids,
  )
}
