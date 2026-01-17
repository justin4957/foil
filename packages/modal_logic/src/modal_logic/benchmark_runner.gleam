//// Live Benchmark Runner for Accuracy Assessment
////
//// This module provides a comprehensive benchmark runner for measuring
//// validation accuracy against standard datasets (FOLIO, LogiQA, InPhO).
////
//// ## Overview
////
//// - Automated benchmark execution across multiple datasets
//// - Live accuracy tracking with F1, precision, recall metrics
//// - Regression detection via baseline comparison
//// - Performance profiling per dataset
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/benchmark_runner.{run_benchmark_suite, folio_suite}
////
//// // Run FOLIO benchmark
//// let results = run_benchmark_suite(folio_suite(), default_config())
////
//// // Check for regressions
//// let comparison = compare_to_baseline(results, baseline)
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/argument.{type Formalization, Formalization, Invalid, Valid}
import modal_logic/heuristics
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Implies, K, K4, KD, KD45,
  Necessary, Not, Or, Possible, S4, S5, T,
}
import modal_logic/testing/external/dataset_adapter.{
  type DatasetSource, type UnifiedConfig, FOLIOSource, InPhOSource, LogiQASource,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture}
import modal_logic/testing/test_config.{
  type Difficulty, type ExpectedValidity, Easy, ExpectedEither, ExpectedInvalid,
  ExpectedValid, Hard, Medium, Unknown as UnknownValidity,
}

// =============================================================================
// Types
// =============================================================================

/// Type of dataset for benchmarking
pub type DatasetType {
  /// First-order logic inference dataset
  FOLIO
  /// Logical reasoning QA dataset
  LogiQA
  /// Indiana Philosophy Ontology dataset
  InPhO
  /// Custom user-provided dataset
  Custom(name: String)
}

/// Difficulty level for benchmark cases
pub type BenchmarkDifficulty {
  /// Trivial cases (immediate validation)
  Trivial
  /// Easy cases (single step reasoning)
  BenchmarkEasy
  /// Medium cases (multi-step reasoning)
  BenchmarkMedium
  /// Hard cases (complex modal reasoning)
  BenchmarkHard
  /// Research-level cases
  Research
}

/// Expected validity for benchmark cases
pub type ExpectedBenchmarkValidity {
  /// Expected to be valid
  ExpectedValidBenchmark
  /// Expected to be invalid
  ExpectedInvalidBenchmark
  /// Either valid or invalid is acceptable
  ExpectedEitherBenchmark(reason: String)
  /// Unknown validity
  UnknownBenchmark
}

/// A single benchmark test case
pub type BenchmarkCase {
  BenchmarkCase(
    /// Unique identifier
    id: String,
    /// Natural language input
    input: String,
    /// Expected validity
    expected_validity: ExpectedBenchmarkValidity,
    /// Expected logic system (if any)
    expected_system: Option(LogicSystem),
    /// Category (e.g., "modal", "epistemic", "propositional")
    category: String,
    /// Difficulty level
    difficulty: BenchmarkDifficulty,
    /// Parsed premises (if available)
    premises: List(Proposition),
    /// Parsed conclusion (if available)
    conclusion: Proposition,
  )
}

/// Configuration for a benchmark suite
pub type BenchmarkConfig {
  BenchmarkConfig(
    /// Name of the benchmark
    name: String,
    /// Dataset type
    dataset: DatasetType,
    /// Maximum cases per category
    max_per_category: Int,
    /// Timeout per case in milliseconds
    timeout_ms: Int,
    /// Whether to run in parallel
    parallel: Bool,
    /// Categories to include (empty = all)
    categories: List(String),
    /// Minimum difficulty
    min_difficulty: BenchmarkDifficulty,
    /// Maximum difficulty
    max_difficulty: BenchmarkDifficulty,
  )
}

/// A complete benchmark suite
pub type BenchmarkSuite {
  BenchmarkSuite(
    /// Suite name
    name: String,
    /// Dataset type
    dataset: DatasetType,
    /// Test cases to run
    test_cases: List(BenchmarkCase),
    /// Suite configuration
    config: BenchmarkConfig,
  )
}

/// Result of validating a single benchmark case
pub type CaseResult {
  CaseResult(
    /// Case ID
    case_id: String,
    /// Whether validation was correct
    correct: Bool,
    /// Predicted validity
    predicted_valid: Option(Bool),
    /// Expected validity
    expected_valid: Option(Bool),
    /// Confidence score
    confidence: Float,
    /// Duration in milliseconds
    duration_ms: Int,
    /// Logic system used
    system_used: LogicSystem,
    /// Category for this case
    category: String,
    /// Any error message
    error: Option(String),
  )
}

/// Accuracy metrics for a benchmark
pub type AccuracyMetrics {
  AccuracyMetrics(
    /// Total cases evaluated
    total: Int,
    /// True positives (correctly identified valid)
    true_positives: Int,
    /// True negatives (correctly identified invalid)
    true_negatives: Int,
    /// False positives (incorrectly said valid)
    false_positives: Int,
    /// False negatives (incorrectly said invalid)
    false_negatives: Int,
    /// Precision (TP / (TP + FP))
    precision: Float,
    /// Recall (TP / (TP + FN))
    recall: Float,
    /// F1 score (harmonic mean of precision and recall)
    f1_score: Float,
    /// Accuracy ((TP + TN) / total)
    accuracy: Float,
  )
}

/// Performance metrics for a benchmark
pub type PerformanceMetrics {
  PerformanceMetrics(
    /// Total duration in milliseconds
    total_duration_ms: Int,
    /// Average duration per case
    avg_duration_ms: Int,
    /// Minimum duration
    min_duration_ms: Int,
    /// Maximum duration
    max_duration_ms: Int,
    /// P50 (median) latency
    p50_ms: Int,
    /// P95 latency
    p95_ms: Int,
    /// P99 latency
    p99_ms: Int,
    /// Cases per second throughput
    throughput: Float,
  )
}

/// Results by category
pub type CategoryResults {
  CategoryResults(
    /// Category name
    category: String,
    /// Number of cases
    case_count: Int,
    /// Accuracy metrics for this category
    accuracy: AccuracyMetrics,
    /// Average confidence
    avg_confidence: Float,
  )
}

/// Comparison to baseline results
pub type BaselineComparison {
  BaselineComparison(
    /// Baseline timestamp
    baseline_timestamp: String,
    /// F1 score delta
    f1_delta: Float,
    /// Accuracy delta
    accuracy_delta: Float,
    /// Whether regression detected (>2% drop)
    regression_detected: Bool,
    /// Number of new failures
    new_failures: Int,
    /// Number of new successes
    new_successes: Int,
    /// Regression severity
    severity: RegressionSeverity,
  )
}

/// Severity of regression
pub type RegressionSeverity {
  /// No regression
  NoRegression
  /// Minor regression (<2%)
  MinorRegression
  /// Major regression (2-5%)
  MajorRegression
  /// Critical regression (>5%)
  CriticalRegression
}

/// Complete benchmark results
pub type BenchmarkResults {
  BenchmarkResults(
    /// Suite name
    suite_name: String,
    /// Dataset type
    dataset: DatasetType,
    /// Timestamp of run
    timestamp: String,
    /// Total cases run
    total_cases: Int,
    /// Individual case results
    case_results: List(CaseResult),
    /// Overall accuracy metrics
    accuracy: AccuracyMetrics,
    /// Results by category
    per_category: Dict(String, CategoryResults),
    /// Performance metrics
    performance: PerformanceMetrics,
    /// Comparison to baseline (if available)
    comparison_to_baseline: Option(BaselineComparison),
  )
}

// =============================================================================
// Default Configurations
// =============================================================================

/// Default benchmark configuration
pub fn default_config() -> BenchmarkConfig {
  BenchmarkConfig(
    name: "default",
    dataset: FOLIO,
    max_per_category: 100,
    timeout_ms: 5000,
    parallel: False,
    categories: [],
    min_difficulty: Trivial,
    max_difficulty: Research,
  )
}

/// Fast benchmark configuration (fewer cases, lower timeout)
pub fn fast_config() -> BenchmarkConfig {
  BenchmarkConfig(
    name: "fast",
    dataset: FOLIO,
    max_per_category: 20,
    timeout_ms: 1000,
    parallel: True,
    categories: [],
    min_difficulty: Trivial,
    max_difficulty: BenchmarkMedium,
  )
}

/// Thorough benchmark configuration (more cases, extended timeout)
pub fn thorough_config() -> BenchmarkConfig {
  BenchmarkConfig(
    name: "thorough",
    dataset: FOLIO,
    max_per_category: 500,
    timeout_ms: 10_000,
    parallel: True,
    categories: [],
    min_difficulty: Trivial,
    max_difficulty: Research,
  )
}

/// CI benchmark configuration (balanced for CI pipelines)
pub fn ci_config() -> BenchmarkConfig {
  BenchmarkConfig(
    name: "ci",
    dataset: FOLIO,
    max_per_category: 50,
    timeout_ms: 3000,
    parallel: True,
    categories: [],
    min_difficulty: Trivial,
    max_difficulty: BenchmarkHard,
  )
}

// =============================================================================
// Suite Construction
// =============================================================================

/// Create a FOLIO benchmark suite
pub fn folio_suite() -> BenchmarkSuite {
  let config =
    BenchmarkConfig(..default_config(), name: "FOLIO", dataset: FOLIO)
  let test_cases = load_folio_cases(config)

  BenchmarkSuite(
    name: "FOLIO Benchmark",
    dataset: FOLIO,
    test_cases: test_cases,
    config: config,
  )
}

/// Create a LogiQA benchmark suite
pub fn logiqa_suite() -> BenchmarkSuite {
  let config =
    BenchmarkConfig(..default_config(), name: "LogiQA", dataset: LogiQA)
  let test_cases = load_logiqa_cases(config)

  BenchmarkSuite(
    name: "LogiQA Benchmark",
    dataset: LogiQA,
    test_cases: test_cases,
    config: config,
  )
}

/// Create an InPhO benchmark suite
pub fn inpho_suite() -> BenchmarkSuite {
  let config =
    BenchmarkConfig(..default_config(), name: "InPhO", dataset: InPhO)
  let test_cases = load_inpho_cases(config)

  BenchmarkSuite(
    name: "InPhO Benchmark",
    dataset: InPhO,
    test_cases: test_cases,
    config: config,
  )
}

/// Create a combined benchmark suite with all datasets
pub fn combined_suite() -> BenchmarkSuite {
  let folio_cases = load_folio_cases(default_config())
  let logiqa_cases = load_logiqa_cases(default_config())
  let inpho_cases = load_inpho_cases(default_config())

  let all_cases = list.flatten([folio_cases, logiqa_cases, inpho_cases])

  BenchmarkSuite(
    name: "Combined Benchmark",
    dataset: Custom("combined"),
    test_cases: all_cases,
    config: BenchmarkConfig(
      ..default_config(),
      name: "combined",
      dataset: Custom("combined"),
    ),
  )
}

/// Create a custom benchmark suite
pub fn custom_suite(
  name: String,
  test_cases: List(BenchmarkCase),
  config: BenchmarkConfig,
) -> BenchmarkSuite {
  BenchmarkSuite(
    name: name,
    dataset: config.dataset,
    test_cases: test_cases,
    config: config,
  )
}

// =============================================================================
// Dataset Loading
// =============================================================================

/// Load FOLIO benchmark cases
fn load_folio_cases(config: BenchmarkConfig) -> List(BenchmarkCase) {
  let unified_config =
    dataset_adapter.UnifiedConfig(
      sources: [FOLIOSource],
      max_per_source: config.max_per_category * 5,
      logic_filter: dataset_adapter.AllLogicTypes,
      min_confidence: 0.5,
      include_invalid: True,
      cache_duration: 3600,
    )

  let fixtures = dataset_adapter.get_unified_fixtures(unified_config)

  fixtures
  |> list.map(fixture_to_benchmark_case)
  |> filter_by_config(config)
}

/// Load LogiQA benchmark cases
fn load_logiqa_cases(config: BenchmarkConfig) -> List(BenchmarkCase) {
  let unified_config =
    dataset_adapter.UnifiedConfig(
      sources: [LogiQASource],
      max_per_source: config.max_per_category * 5,
      logic_filter: dataset_adapter.AllLogicTypes,
      min_confidence: 0.5,
      include_invalid: True,
      cache_duration: 3600,
    )

  let fixtures = dataset_adapter.get_unified_fixtures(unified_config)

  fixtures
  |> list.map(fixture_to_benchmark_case)
  |> filter_by_config(config)
}

/// Load InPhO benchmark cases
fn load_inpho_cases(config: BenchmarkConfig) -> List(BenchmarkCase) {
  let unified_config =
    dataset_adapter.UnifiedConfig(
      sources: [InPhOSource],
      max_per_source: config.max_per_category * 5,
      logic_filter: dataset_adapter.AllLogicTypes,
      min_confidence: 0.5,
      include_invalid: True,
      cache_duration: 3600,
    )

  let fixtures = dataset_adapter.get_unified_fixtures(unified_config)

  fixtures
  |> list.map(fixture_to_benchmark_case)
  |> filter_by_config(config)
}

/// Convert TestFixture to BenchmarkCase
fn fixture_to_benchmark_case(fixture: TestFixture) -> BenchmarkCase {
  let expected_validity = case fixture.expected_validity {
    ExpectedValid -> ExpectedValidBenchmark
    ExpectedInvalid(_) -> ExpectedInvalidBenchmark
    ExpectedEither(reason) -> ExpectedEitherBenchmark(reason)
    UnknownValidity -> UnknownBenchmark
  }

  let difficulty = case fixture.difficulty {
    test_config.Trivial -> Trivial
    Easy -> BenchmarkEasy
    Medium -> BenchmarkMedium
    Hard -> BenchmarkHard
    test_config.Research -> Research
  }

  let category = extract_category(fixture)

  BenchmarkCase(
    id: fixture.id,
    input: fixture.natural_language,
    expected_validity: expected_validity,
    expected_system: Some(fixture.expected_logic_system),
    category: category,
    difficulty: difficulty,
    premises: fixture.expected_premises,
    conclusion: fixture.expected_conclusion,
  )
}

/// Extract category from fixture
fn extract_category(fixture: TestFixture) -> String {
  // Check tags for category hints
  let has_modal =
    list.any(fixture.tags, fn(t) { t == "modal" || t == "necessity" })
  let has_epistemic =
    list.any(fixture.tags, fn(t) { t == "epistemic" || t == "knowledge" })
  let has_deontic =
    list.any(fixture.tags, fn(t) { t == "deontic" || t == "obligation" })

  case has_modal, has_epistemic, has_deontic {
    True, _, _ -> "modal"
    _, True, _ -> "epistemic"
    _, _, True -> "deontic"
    _, _, _ -> "propositional"
  }
}

/// Filter cases by configuration
fn filter_by_config(
  cases: List(BenchmarkCase),
  config: BenchmarkConfig,
) -> List(BenchmarkCase) {
  cases
  |> list.filter(fn(c) { difficulty_in_range(c.difficulty, config) })
  |> list.filter(fn(c) { category_matches(c.category, config.categories) })
  |> group_and_limit_by_category(config.max_per_category)
}

/// Check if difficulty is in configured range
fn difficulty_in_range(
  difficulty: BenchmarkDifficulty,
  config: BenchmarkConfig,
) -> Bool {
  let diff_value = difficulty_to_int(difficulty)
  let min_value = difficulty_to_int(config.min_difficulty)
  let max_value = difficulty_to_int(config.max_difficulty)

  diff_value >= min_value && diff_value <= max_value
}

/// Convert difficulty to integer for comparison
fn difficulty_to_int(difficulty: BenchmarkDifficulty) -> Int {
  case difficulty {
    Trivial -> 0
    BenchmarkEasy -> 1
    BenchmarkMedium -> 2
    BenchmarkHard -> 3
    Research -> 4
  }
}

/// Check if category matches filter
fn category_matches(category: String, filter_categories: List(String)) -> Bool {
  case filter_categories {
    [] -> True
    categories -> list.contains(categories, category)
  }
}

/// Group by category and limit per category
fn group_and_limit_by_category(
  cases: List(BenchmarkCase),
  max_per_category: Int,
) -> List(BenchmarkCase) {
  cases
  |> list.group(fn(c) { c.category })
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(_category, category_cases) = pair
    list.take(category_cases, max_per_category)
  })
}

// =============================================================================
// Benchmark Execution
// =============================================================================

/// Run a complete benchmark suite
pub fn run_benchmark_suite(
  suite: BenchmarkSuite,
  config: BenchmarkConfig,
) -> BenchmarkResults {
  let start_time = get_current_time_ms()

  // Run each case and collect results
  let case_results =
    suite.test_cases
    |> list.map(fn(case_data) { run_single_case(case_data, config) })

  let end_time = get_current_time_ms()
  let total_duration = end_time - start_time

  // Compute metrics
  let accuracy = compute_accuracy_metrics(case_results)
  let per_category = compute_category_results(case_results)
  let performance = compute_performance_metrics(case_results, total_duration)

  BenchmarkResults(
    suite_name: suite.name,
    dataset: suite.dataset,
    timestamp: get_timestamp(),
    total_cases: list.length(case_results),
    case_results: case_results,
    accuracy: accuracy,
    per_category: per_category,
    performance: performance,
    comparison_to_baseline: None,
  )
}

/// Run a single benchmark case
fn run_single_case(
  case_data: BenchmarkCase,
  config: BenchmarkConfig,
) -> CaseResult {
  let start_time = get_current_time_ms()

  // Get the system to use (default to K if not specified)
  let system = case case_data.expected_system {
    Some(s) -> s
    None -> K
  }

  // Create formalization from the case
  let formalization =
    Formalization(
      id: case_data.id,
      argument_id: case_data.id,
      logic_system: system,
      premises: case_data.premises,
      conclusion: case_data.conclusion,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Run heuristic validation
  let heuristic_result = heuristics.try_heuristic_validation(formalization)

  let end_time = get_current_time_ms()
  let duration = end_time - start_time

  // Determine predicted validity
  let #(predicted_valid, confidence) = case heuristic_result {
    Some(hr) ->
      case hr.result {
        Valid -> #(Some(True), tier_to_confidence(hr.tier))
        Invalid(_) -> #(Some(False), tier_to_confidence(hr.tier))
        argument.Unknown(_) -> #(None, 0.5)
        argument.Timeout -> #(None, 0.3)
        argument.Error(_) -> #(None, 0.0)
      }
    None -> #(None, 0.5)
  }

  // Determine expected validity
  let expected_valid = case case_data.expected_validity {
    ExpectedValidBenchmark -> Some(True)
    ExpectedInvalidBenchmark -> Some(False)
    ExpectedEitherBenchmark(_) -> None
    UnknownBenchmark -> None
  }

  // Determine correctness
  let correct = case predicted_valid, expected_valid {
    Some(p), Some(e) -> p == e
    _, None -> True
    // Accept any result if expected is unknown
    None, _ -> False
    // No prediction is considered incorrect
  }

  CaseResult(
    case_id: case_data.id,
    correct: correct,
    predicted_valid: predicted_valid,
    expected_valid: expected_valid,
    confidence: confidence,
    duration_ms: duration,
    system_used: system,
    category: case_data.category,
    error: None,
  )
}

/// Convert validation tier to confidence score
fn tier_to_confidence(tier: heuristics.ValidationTier) -> Float {
  case tier {
    heuristics.Tier1Syntactic -> 0.95
    heuristics.Tier2TruthTable -> 0.99
    heuristics.Tier3Z3 -> 0.99
  }
}

// =============================================================================
// Metrics Computation
// =============================================================================

/// Compute accuracy metrics from case results
fn compute_accuracy_metrics(results: List(CaseResult)) -> AccuracyMetrics {
  let total = list.length(results)

  // Count true/false positives/negatives
  let tp =
    list.count(results, fn(r) {
      r.predicted_valid == Some(True) && r.expected_valid == Some(True)
    })

  let tn =
    list.count(results, fn(r) {
      r.predicted_valid == Some(False) && r.expected_valid == Some(False)
    })

  let fp =
    list.count(results, fn(r) {
      r.predicted_valid == Some(True) && r.expected_valid == Some(False)
    })

  let fn_count =
    list.count(results, fn(r) {
      r.predicted_valid == Some(False) && r.expected_valid == Some(True)
    })

  // Compute precision, recall, F1
  let precision = case tp + fp {
    0 -> 0.0
    n -> int.to_float(tp) /. int.to_float(n)
  }

  let recall = case tp + fn_count {
    0 -> 0.0
    n -> int.to_float(tp) /. int.to_float(n)
  }

  let f1 = case precision +. recall {
    0.0 -> 0.0
    sum -> 2.0 *. precision *. recall /. sum
  }

  let accuracy = case total {
    0 -> 0.0
    n -> int.to_float(tp + tn) /. int.to_float(n)
  }

  AccuracyMetrics(
    total: total,
    true_positives: tp,
    true_negatives: tn,
    false_positives: fp,
    false_negatives: fn_count,
    precision: precision,
    recall: recall,
    f1_score: f1,
    accuracy: accuracy,
  )
}

/// Compute results by category
fn compute_category_results(
  results: List(CaseResult),
) -> Dict(String, CategoryResults) {
  results
  |> list.group(fn(r) { r.category })
  |> dict.map_values(fn(category, category_results) {
    let accuracy = compute_accuracy_metrics(category_results)
    let avg_confidence = compute_avg_confidence(category_results)

    CategoryResults(
      category: category,
      case_count: list.length(category_results),
      accuracy: accuracy,
      avg_confidence: avg_confidence,
    )
  })
}

/// Compute average confidence
fn compute_avg_confidence(results: List(CaseResult)) -> Float {
  case results {
    [] -> 0.0
    _ -> {
      let total =
        results
        |> list.map(fn(r) { r.confidence })
        |> list.fold(0.0, fn(acc, c) { acc +. c })

      total /. int.to_float(list.length(results))
    }
  }
}

/// Compute performance metrics
fn compute_performance_metrics(
  results: List(CaseResult),
  total_duration: Int,
) -> PerformanceMetrics {
  let durations =
    results
    |> list.map(fn(r) { r.duration_ms })
    |> list.sort(int.compare)

  let count = list.length(durations)

  let #(min_d, max_d, avg_d) = case durations {
    [] -> #(0, 0, 0)
    _ -> {
      let min_val = list.first(durations) |> result.unwrap(0)
      let max_val = list.last(durations) |> result.unwrap(0)
      let sum = list.fold(durations, 0, fn(acc, d) { acc + d })
      let avg = case count {
        0 -> 0
        n -> sum / n
      }
      #(min_val, max_val, avg)
    }
  }

  let p50 = percentile(durations, 50)
  let p95 = percentile(durations, 95)
  let p99 = percentile(durations, 99)

  let throughput = case total_duration {
    0 -> 0.0
    ms -> int.to_float(count) /. { int.to_float(ms) /. 1000.0 }
  }

  PerformanceMetrics(
    total_duration_ms: total_duration,
    avg_duration_ms: avg_d,
    min_duration_ms: min_d,
    max_duration_ms: max_d,
    p50_ms: p50,
    p95_ms: p95,
    p99_ms: p99,
    throughput: throughput,
  )
}

/// Calculate percentile from sorted list
fn percentile(sorted: List(Int), pct: Int) -> Int {
  case sorted {
    [] -> 0
    _ -> {
      let count = list.length(sorted)
      let index = { pct * count } / 100
      let safe_index = int.min(index, count - 1)

      sorted
      |> list.drop(safe_index)
      |> list.first
      |> result.unwrap(0)
    }
  }
}

// =============================================================================
// Baseline Comparison
// =============================================================================

/// Compare results to a baseline
pub fn compare_to_baseline(
  results: BenchmarkResults,
  baseline: BenchmarkResults,
) -> BaselineComparison {
  let f1_delta = results.accuracy.f1_score -. baseline.accuracy.f1_score
  let accuracy_delta = results.accuracy.accuracy -. baseline.accuracy.accuracy

  // Count new failures and successes
  let baseline_ids =
    baseline.case_results
    |> list.filter_map(fn(r) {
      case r.correct {
        True -> Ok(r.case_id)
        False -> Error(Nil)
      }
    })

  let current_pass_ids =
    results.case_results
    |> list.filter_map(fn(r) {
      case r.correct {
        True -> Ok(r.case_id)
        False -> Error(Nil)
      }
    })

  let new_failures =
    baseline_ids
    |> list.filter(fn(id) { !list.contains(current_pass_ids, id) })
    |> list.length

  let new_successes =
    current_pass_ids
    |> list.filter(fn(id) { !list.contains(baseline_ids, id) })
    |> list.length

  // Determine regression severity
  let regression_detected = f1_delta <. -0.02
  let severity = case f1_delta {
    d if d >=. 0.0 -> NoRegression
    d if d >=. -0.02 -> MinorRegression
    d if d >=. -0.05 -> MajorRegression
    _ -> CriticalRegression
  }

  BaselineComparison(
    baseline_timestamp: baseline.timestamp,
    f1_delta: f1_delta,
    accuracy_delta: accuracy_delta,
    regression_detected: regression_detected,
    new_failures: new_failures,
    new_successes: new_successes,
    severity: severity,
  )
}

/// Check if regression is detected
pub fn has_regression(comparison: BaselineComparison) -> Bool {
  comparison.regression_detected
}

// =============================================================================
// Quick Benchmark Functions
// =============================================================================

/// Run a quick FOLIO benchmark (subset for fast feedback)
pub fn quick_folio_benchmark() -> BenchmarkResults {
  let config = fast_config()
  let suite =
    BenchmarkSuite(
      ..folio_suite(),
      test_cases: folio_suite().test_cases |> list.take(20),
      config: config,
    )
  run_benchmark_suite(suite, config)
}

/// Run a quick LogiQA benchmark
pub fn quick_logiqa_benchmark() -> BenchmarkResults {
  let config = fast_config()
  let suite =
    BenchmarkSuite(
      ..logiqa_suite(),
      test_cases: logiqa_suite().test_cases |> list.take(20),
      config: config,
    )
  run_benchmark_suite(suite, config)
}

/// Run a quick InPhO benchmark
pub fn quick_inpho_benchmark() -> BenchmarkResults {
  let config = fast_config()
  let suite =
    BenchmarkSuite(
      ..inpho_suite(),
      test_cases: inpho_suite().test_cases |> list.take(20),
      config: config,
    )
  run_benchmark_suite(suite, config)
}

/// Run all benchmarks quickly
pub fn quick_all_benchmarks() -> List(BenchmarkResults) {
  [quick_folio_benchmark(), quick_logiqa_benchmark(), quick_inpho_benchmark()]
}

// =============================================================================
// Output Formatting
// =============================================================================

/// Format benchmark results as a human-readable report
pub fn format_benchmark_report(results: BenchmarkResults) -> String {
  let header =
    "═══════════════════════════════════════════════════════════════════════\n"
    <> "  BENCHMARK RESULTS: "
    <> results.suite_name
    <> "\n"
    <> "═══════════════════════════════════════════════════════════════════════\n"

  let summary =
    "\n📊 ACCURACY METRICS\n"
    <> "───────────────────────────────────────────────────────────────────────\n"
    <> "  Total Cases:      "
    <> int.to_string(results.accuracy.total)
    <> "\n"
    <> "  True Positives:   "
    <> int.to_string(results.accuracy.true_positives)
    <> "\n"
    <> "  True Negatives:   "
    <> int.to_string(results.accuracy.true_negatives)
    <> "\n"
    <> "  False Positives:  "
    <> int.to_string(results.accuracy.false_positives)
    <> "\n"
    <> "  False Negatives:  "
    <> int.to_string(results.accuracy.false_negatives)
    <> "\n"
    <> "  Precision:        "
    <> format_percent(results.accuracy.precision)
    <> "\n"
    <> "  Recall:           "
    <> format_percent(results.accuracy.recall)
    <> "\n"
    <> "  F1 Score:         "
    <> format_percent(results.accuracy.f1_score)
    <> "\n"
    <> "  Accuracy:         "
    <> format_percent(results.accuracy.accuracy)
    <> "\n"

  let performance =
    "\n⚡ PERFORMANCE METRICS\n"
    <> "───────────────────────────────────────────────────────────────────────\n"
    <> "  Total Duration:   "
    <> int.to_string(results.performance.total_duration_ms)
    <> "ms\n"
    <> "  Avg per Case:     "
    <> int.to_string(results.performance.avg_duration_ms)
    <> "ms\n"
    <> "  P50 Latency:      "
    <> int.to_string(results.performance.p50_ms)
    <> "ms\n"
    <> "  P95 Latency:      "
    <> int.to_string(results.performance.p95_ms)
    <> "ms\n"
    <> "  P99 Latency:      "
    <> int.to_string(results.performance.p99_ms)
    <> "ms\n"
    <> "  Throughput:       "
    <> float.to_string(results.performance.throughput)
    <> " cases/sec\n"

  let category_section =
    "\n📁 RESULTS BY CATEGORY\n"
    <> "───────────────────────────────────────────────────────────────────────\n"
    <> format_category_results(results.per_category)

  let baseline_section = case results.comparison_to_baseline {
    Some(comparison) ->
      "\n📈 BASELINE COMPARISON\n"
      <> "───────────────────────────────────────────────────────────────────────\n"
      <> format_baseline_comparison(comparison)
    None -> ""
  }

  header <> summary <> performance <> category_section <> baseline_section
}

/// Format percentage value
fn format_percent(value: Float) -> String {
  let pct = value *. 100.0
  float.to_string(
    float.truncate(pct *. 100.0)
    |> int.to_float
    |> fn(x) { x /. 100.0 },
  )
  <> "%"
}

/// Format category results
fn format_category_results(categories: Dict(String, CategoryResults)) -> String {
  categories
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(name, cat) = pair
    "  "
    <> name
    <> ": "
    <> int.to_string(cat.case_count)
    <> " cases, F1="
    <> format_percent(cat.accuracy.f1_score)
    <> ", Acc="
    <> format_percent(cat.accuracy.accuracy)
  })
  |> string.join("\n")
}

/// Format baseline comparison
fn format_baseline_comparison(comparison: BaselineComparison) -> String {
  let status = case comparison.severity {
    NoRegression -> "✅ No regression"
    MinorRegression -> "⚠️  Minor regression"
    MajorRegression -> "🔶 Major regression"
    CriticalRegression -> "🔴 Critical regression"
  }

  status
  <> "\n"
  <> "  F1 Delta:         "
  <> format_delta(comparison.f1_delta)
  <> "\n"
  <> "  Accuracy Delta:   "
  <> format_delta(comparison.accuracy_delta)
  <> "\n"
  <> "  New Failures:     "
  <> int.to_string(comparison.new_failures)
  <> "\n"
  <> "  New Successes:    "
  <> int.to_string(comparison.new_successes)
  <> "\n"
}

/// Format delta value with sign
fn format_delta(delta: Float) -> String {
  let sign = case delta >=. 0.0 {
    True -> "+"
    False -> ""
  }
  sign <> format_percent(delta)
}

/// Format benchmark results as JSON
pub fn format_as_json(results: BenchmarkResults) -> String {
  let category_json =
    results.per_category
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(name, cat) = pair
      "    \""
      <> name
      <> "\": {"
      <> "\"case_count\": "
      <> int.to_string(cat.case_count)
      <> ", "
      <> "\"f1_score\": "
      <> float.to_string(cat.accuracy.f1_score)
      <> ", "
      <> "\"accuracy\": "
      <> float.to_string(cat.accuracy.accuracy)
      <> "}"
    })
    |> string.join(",\n")

  let baseline_json = case results.comparison_to_baseline {
    Some(c) ->
      ",\n  \"baseline_comparison\": {\n"
      <> "    \"f1_delta\": "
      <> float.to_string(c.f1_delta)
      <> ",\n"
      <> "    \"accuracy_delta\": "
      <> float.to_string(c.accuracy_delta)
      <> ",\n"
      <> "    \"regression_detected\": "
      <> bool_to_string(c.regression_detected)
      <> "\n  }"
    None -> ""
  }

  "{\n"
  <> "  \"suite_name\": \""
  <> results.suite_name
  <> "\",\n"
  <> "  \"timestamp\": \""
  <> results.timestamp
  <> "\",\n"
  <> "  \"total_cases\": "
  <> int.to_string(results.total_cases)
  <> ",\n"
  <> "  \"accuracy\": {\n"
  <> "    \"precision\": "
  <> float.to_string(results.accuracy.precision)
  <> ",\n"
  <> "    \"recall\": "
  <> float.to_string(results.accuracy.recall)
  <> ",\n"
  <> "    \"f1_score\": "
  <> float.to_string(results.accuracy.f1_score)
  <> ",\n"
  <> "    \"accuracy\": "
  <> float.to_string(results.accuracy.accuracy)
  <> "\n  },\n"
  <> "  \"performance\": {\n"
  <> "    \"total_duration_ms\": "
  <> int.to_string(results.performance.total_duration_ms)
  <> ",\n"
  <> "    \"avg_duration_ms\": "
  <> int.to_string(results.performance.avg_duration_ms)
  <> ",\n"
  <> "    \"p95_ms\": "
  <> int.to_string(results.performance.p95_ms)
  <> ",\n"
  <> "    \"throughput\": "
  <> float.to_string(results.performance.throughput)
  <> "\n  },\n"
  <> "  \"per_category\": {\n"
  <> category_json
  <> "\n  }"
  <> baseline_json
  <> "\n}"
}

/// Convert bool to JSON string
fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Get current timestamp
fn get_timestamp() -> String {
  "2026-01-16T00:00:00Z"
}

/// Get current time in milliseconds (simulated)
fn get_current_time_ms() -> Int {
  // In a real implementation, this would get actual time
  // For now, return a fixed value that allows duration calculation
  0
}

/// Dataset type to string
pub fn dataset_type_to_string(dataset: DatasetType) -> String {
  case dataset {
    FOLIO -> "FOLIO"
    LogiQA -> "LogiQA"
    InPhO -> "InPhO"
    Custom(name) -> name
  }
}

/// Parse dataset type from string
pub fn parse_dataset_type(s: String) -> Result(DatasetType, String) {
  case string.uppercase(s) {
    "FOLIO" -> Ok(FOLIO)
    "LOGIQA" -> Ok(LogiQA)
    "INPHO" -> Ok(InPhO)
    _ -> Ok(Custom(s))
  }
}

/// Get target metrics for a dataset
pub fn get_target_metrics(
  dataset: DatasetType,
) -> #(Float, Float, Int, Int, Int) {
  // Returns: (target_f1, target_accuracy, target_p50, target_p95, target_p99)
  case dataset {
    FOLIO -> #(0.8, 0.8, 100, 3000, 5000)
    LogiQA -> #(0.75, 0.75, 50, 2000, 3000)
    InPhO -> #(0.7, 0.7, 50, 2000, 3000)
    Custom(_) -> #(0.7, 0.7, 100, 3000, 5000)
  }
}

/// Check if results meet target for dataset
pub fn meets_targets(results: BenchmarkResults) -> Bool {
  let #(target_f1, target_acc, _, target_p95, _) =
    get_target_metrics(results.dataset)

  results.accuracy.f1_score >=. target_f1
  && results.accuracy.accuracy >=. target_acc
  && results.performance.p95_ms <= target_p95
}
