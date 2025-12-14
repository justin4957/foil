//// Configuration types for autonomous testing system

import gleam/option.{type Option}

/// Test run configuration
pub type TestConfig {
  TestConfig(
    /// Run mode (normal, recording, compare)
    run_mode: RunMode,
    /// Categories to run (empty = all)
    categories: List(FixtureCategory),
    /// Output format for results
    output_format: OutputFormat,
    /// Stop on first failure
    fail_fast: Bool,
    /// Verbose output
    verbose: Bool,
    /// Maximum tests to run (None = all)
    max_tests: Option(Int),
    /// Timeout per test in ms
    timeout_ms: Int,
    /// Compare against baseline
    compare_baseline: Bool,
    /// Baseline run ID to compare against
    baseline_id: Option(String),
  )
}

/// Run mode determines test behavior
pub type RunMode {
  /// Normal test execution with mock LLM
  Normal
  /// Record new LLM responses (non-deterministic)
  Recording
  /// Compare current run against baseline
  Comparing
  /// Only run golden master tests
  GoldenOnly
}

/// Categories of test fixtures
pub type FixtureCategory {
  /// Classic philosophical arguments
  ClassicArgument
  /// Standard modal logic theorems
  ModalTheorem
  /// Edge cases and boundary conditions
  EdgeCase
  /// Previously failing tests
  RegressionCase
  /// External dataset fixtures
  ExternalDataset(source: String)
}

/// Output format for test results
pub type OutputFormat {
  /// Console output (for terminal)
  Console
  /// JSON output (for programmatic access)
  Json
  /// Markdown output (for documentation)
  Markdown
  /// All formats
  All
}

/// Test difficulty levels
pub type Difficulty {
  /// Very simple, single operator
  Trivial
  /// Simple arguments
  Easy
  /// Moderate complexity
  Medium
  /// Complex nested modalities
  Hard
  /// Research-level edge cases
  Research
}

/// Expected validity of an argument
pub type ExpectedValidity {
  /// Expected to be valid
  ExpectedValid
  /// Expected to be invalid with optional countermodel shape
  ExpectedInvalid(countermodel_hint: Option(String))
  /// Multiple valid formalizations possible
  ExpectedEither(explanation: String)
  /// Unknown - used for external datasets
  Unknown
}

/// Create default test configuration
pub fn default_config() -> TestConfig {
  TestConfig(
    run_mode: Normal,
    categories: [],
    output_format: Console,
    fail_fast: False,
    verbose: False,
    max_tests: option.None,
    timeout_ms: 30_000,
    compare_baseline: False,
    baseline_id: option.None,
  )
}

/// Create verbose configuration
pub fn verbose_config() -> TestConfig {
  TestConfig(..default_config(), verbose: True)
}

/// Create CI configuration (fail fast, compare baseline)
pub fn ci_config() -> TestConfig {
  TestConfig(
    ..default_config(),
    fail_fast: True,
    compare_baseline: True,
    output_format: All,
  )
}

/// Create recording configuration
pub fn recording_config() -> TestConfig {
  TestConfig(..default_config(), run_mode: Recording, verbose: True)
}

/// Set run mode
pub fn with_run_mode(config: TestConfig, mode: RunMode) -> TestConfig {
  TestConfig(..config, run_mode: mode)
}

/// Set categories filter
pub fn with_categories(
  config: TestConfig,
  categories: List(FixtureCategory),
) -> TestConfig {
  TestConfig(..config, categories: categories)
}

/// Set output format
pub fn with_output_format(
  config: TestConfig,
  format: OutputFormat,
) -> TestConfig {
  TestConfig(..config, output_format: format)
}

/// Set fail fast
pub fn with_fail_fast(config: TestConfig, fail_fast: Bool) -> TestConfig {
  TestConfig(..config, fail_fast: fail_fast)
}

/// Set verbose
pub fn with_verbose(config: TestConfig, verbose: Bool) -> TestConfig {
  TestConfig(..config, verbose: verbose)
}

/// Set max tests
pub fn with_max_tests(config: TestConfig, max: Int) -> TestConfig {
  TestConfig(..config, max_tests: option.Some(max))
}

/// Set timeout
pub fn with_timeout(config: TestConfig, timeout_ms: Int) -> TestConfig {
  TestConfig(..config, timeout_ms: timeout_ms)
}

/// Enable baseline comparison
pub fn with_baseline_comparison(
  config: TestConfig,
  baseline_id: Option(String),
) -> TestConfig {
  TestConfig(..config, compare_baseline: True, baseline_id: baseline_id)
}

/// Convert category to string
pub fn category_to_string(category: FixtureCategory) -> String {
  case category {
    ClassicArgument -> "classic_argument"
    ModalTheorem -> "modal_theorem"
    EdgeCase -> "edge_case"
    RegressionCase -> "regression_case"
    ExternalDataset(source) -> "external:" <> source
  }
}

/// Convert difficulty to string
pub fn difficulty_to_string(difficulty: Difficulty) -> String {
  case difficulty {
    Trivial -> "trivial"
    Easy -> "easy"
    Medium -> "medium"
    Hard -> "hard"
    Research -> "research"
  }
}
