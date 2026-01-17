//// Prediction Testing Suite
////
//// A comprehensive framework for automatically testing predicates and logical
//// conclusions across a variety of propositions. Integrates with the modal logic
//// validation pipeline and Z3 solver for rigorous verification.
////
//// ## Overview
////
//// This module provides:
//// - PredictionTestCase: Define predictions with premises, conclusions, and expected outcomes
//// - PredicateSet: Group related predicates for reuse across test cases
//// - PredictionTestSuite: Organize test cases into runnable suites
//// - Automated evaluation with confidence scoring and Z3 integration
//// - Rich reporting with pass/fail analysis and countermodel extraction
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/prediction_testing.{
////   PredictionTestCase, PredicateSet, run_prediction_suite
//// }
////
//// // Define predicates
//// let market_predicates = predicate_set("market")
////   |> add_predicate("bull_market", "Market is in bull phase")
////   |> add_predicate("high_volume", "Trading volume is high")
////   |> add_predicate("price_rise", "Prices are rising")
////
//// // Create test case
//// let test = prediction_test_case("market_prediction_1")
////   |> with_premise(Implies(
////       And(Atom("bull_market"), Atom("high_volume")),
////       Atom("price_rise")
////     ))
////   |> with_premise(Atom("bull_market"))
////   |> with_premise(Atom("high_volume"))
////   |> with_conclusion(Atom("price_rise"))
////   |> expect_valid()
////
//// // Run test
//// let result = run_prediction_test(test, default_config())
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string
import modal_logic/argument.{
  type Formalization, type ValidationResult, Error, Formalization, Invalid,
  Timeout, Unknown, Valid,
}
import modal_logic/confidence.{type ConfidenceContext, type ConfidenceResult}
import modal_logic/heuristics.{
  type ValidationTier, Tier1Syntactic, Tier2TruthTable, Tier3Z3,
}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, Knows,
  Necessary, Not, Or, Possible, S4, S5, T,
}
import modal_logic/validator.{type ValidatorConfig, type ValidatorState}

// =============================================================================
// Core Types
// =============================================================================

/// A predicate definition with name, description, and optional constraints
pub type Predicate {
  Predicate(
    /// Unique identifier for the predicate (used in Atom)
    name: String,
    /// Human-readable description
    description: String,
    /// Domain or category the predicate belongs to
    domain: String,
    /// Whether this predicate is observable/verifiable
    observable: Bool,
    /// Optional type constraint (boolean, numeric, categorical)
    value_type: PredicateType,
  )
}

/// Type of value a predicate represents
pub type PredicateType {
  /// Boolean true/false
  BooleanPredicate
  /// Numeric comparison (e.g., "price > 100")
  NumericPredicate(comparison: NumericComparison)
  /// Categorical value (e.g., "weather = sunny")
  CategoricalPredicate(allowed_values: List(String))
  /// Temporal predicate (e.g., "happened_before")
  TemporalPredicate
  /// Probabilistic predicate
  ProbabilisticPredicate
}

/// Numeric comparison types
pub type NumericComparison {
  GreaterThan(threshold: Float)
  LessThan(threshold: Float)
  EqualTo(value: Float)
  Between(low: Float, high: Float)
}

/// A set of related predicates for a domain
pub type PredicateSet {
  PredicateSet(
    /// Name of the predicate set
    name: String,
    /// Domain description
    domain: String,
    /// Predicates in this set
    predicates: Dict(String, Predicate),
    /// Relationships between predicates
    relationships: List(PredicateRelationship),
  )
}

/// Relationship between predicates
pub type PredicateRelationship {
  /// One predicate implies another
  ImpliesRelation(from: String, to: String)
  /// Predicates are mutually exclusive
  MutuallyExclusive(predicates: List(String))
  /// Predicates are jointly exhaustive
  JointlyExhaustive(predicates: List(String))
  /// One predicate is stronger than another
  StrongerThan(stronger: String, weaker: String)
}

/// A single prediction test case
pub type PredictionTestCase {
  PredictionTestCase(
    /// Unique identifier
    id: String,
    /// Human-readable name
    name: String,
    /// Description of what's being tested
    description: String,
    /// Category for organization
    category: TestCategory,
    /// Domain (e.g., "finance", "weather", "medical")
    domain: String,
    /// Premises (background knowledge + observations)
    premises: List(Proposition),
    /// The conclusion/prediction to test
    conclusion: Proposition,
    /// Expected validity outcome
    expected: ExpectedOutcome,
    /// Logic system to use for validation
    logic_system: LogicSystem,
    /// Confidence threshold for passing
    confidence_threshold: Float,
    /// Tags for filtering
    tags: List(String),
    /// Source or reference
    source: Option(String),
    /// Priority for execution ordering
    priority: TestPriority,
  )
}

/// Category of test case
pub type TestCategory {
  /// Deductive reasoning (premises guarantee conclusion)
  Deductive
  /// Inductive reasoning (premises support but don't guarantee)
  Inductive
  /// Abductive reasoning (inference to best explanation)
  Abductive
  /// Temporal prediction (future state inference)
  TemporalPrediction
  /// Causal inference (cause-effect reasoning)
  CausalInference
  /// Probabilistic prediction
  ProbabilisticPrediction
  /// Counterfactual reasoning
  Counterfactual
  /// Normative reasoning (what should be)
  Normative
  /// Epistemic reasoning (knowledge/belief)
  Epistemic
}

/// Expected outcome of a prediction test
pub type ExpectedOutcome {
  /// Prediction should be logically valid
  ExpectValid
  /// Prediction should be logically invalid
  ExpectInvalid(reason: String)
  /// Either outcome is acceptable (edge case)
  ExpectEither(explanation: String)
  /// Outcome is genuinely unknown
  ExpectUnknown
  /// Expect timeout (for complexity testing)
  ExpectTimeout
  /// Expect specific confidence range
  ExpectConfidence(min: Float, max: Float)
}

/// Test priority for execution ordering
pub type TestPriority {
  Critical
  High
  Normal
  Low
  Background
}

/// A suite of prediction tests
pub type PredictionTestSuite {
  PredictionTestSuite(
    /// Suite name
    name: String,
    /// Suite description
    description: String,
    /// Version identifier
    version: String,
    /// Test cases in this suite
    test_cases: List(PredictionTestCase),
    /// Predicate sets used by this suite
    predicate_sets: List(PredicateSet),
    /// Configuration for running the suite
    config: SuiteConfig,
    /// Metadata
    metadata: SuiteMetadata,
  )
}

/// Suite configuration
pub type SuiteConfig {
  SuiteConfig(
    /// Run tests in parallel
    parallel: Bool,
    /// Stop on first failure
    fail_fast: Bool,
    /// Timeout per test in milliseconds
    timeout_ms: Int,
    /// Number of retries for flaky tests
    retries: Int,
    /// Filter to specific categories
    category_filter: Option(List(TestCategory)),
    /// Filter to specific tags
    tag_filter: Option(List(String)),
    /// Minimum priority to run
    min_priority: TestPriority,
    /// Enable verbose output
    verbose: Bool,
  )
}

/// Suite metadata
pub type SuiteMetadata {
  SuiteMetadata(
    /// Author or source
    author: String,
    /// Creation date
    created: String,
    /// Last modified date
    modified: String,
    /// Related issues or tickets
    related_issues: List(Int),
  )
}

// =============================================================================
// Result Types
// =============================================================================

/// Result of a single prediction test
pub type PredictionTestResult {
  PredictionTestResult(
    /// Test case ID
    test_id: String,
    /// Test case name
    test_name: String,
    /// Whether the test passed
    passed: Bool,
    /// Actual validation result
    actual_result: ValidationResult,
    /// Expected outcome
    expected_outcome: ExpectedOutcome,
    /// Confidence score
    confidence: Float,
    /// Confidence factors
    confidence_factors: List(String),
    /// Duration in milliseconds
    duration_ms: Int,
    /// Tier used for validation
    tier_used: String,
    /// Countermodel if invalid
    countermodel: Option(String),
    /// Explanation of result
    explanation: String,
    /// Any warnings
    warnings: List(String),
  )
}

/// Result of running a prediction test suite
pub type PredictionSuiteResult {
  PredictionSuiteResult(
    /// Suite name
    suite_name: String,
    /// Total tests run
    total_tests: Int,
    /// Tests passed
    passed: Int,
    /// Tests failed
    failed: Int,
    /// Tests skipped
    skipped: Int,
    /// Tests with errors
    errors: Int,
    /// Individual test results
    test_results: List(PredictionTestResult),
    /// Overall pass rate
    pass_rate: Float,
    /// Total duration
    total_duration_ms: Int,
    /// Results by category
    category_breakdown: Dict(String, CategoryStats),
    /// Summary analysis
    summary: String,
  )
}

/// Statistics for a category
pub type CategoryStats {
  CategoryStats(total: Int, passed: Int, failed: Int, pass_rate: Float)
}

// =============================================================================
// Builder Functions
// =============================================================================

/// Create a new predicate
pub fn predicate(name: String, description: String) -> Predicate {
  Predicate(
    name: name,
    description: description,
    domain: "general",
    observable: True,
    value_type: BooleanPredicate,
  )
}

/// Set predicate domain
pub fn with_domain(pred: Predicate, domain: String) -> Predicate {
  Predicate(..pred, domain: domain)
}

/// Set predicate as non-observable
pub fn non_observable(pred: Predicate) -> Predicate {
  Predicate(..pred, observable: False)
}

/// Set predicate type
pub fn with_type(pred: Predicate, value_type: PredicateType) -> Predicate {
  Predicate(..pred, value_type: value_type)
}

/// Create a new predicate set
pub fn predicate_set(name: String) -> PredicateSet {
  PredicateSet(
    name: name,
    domain: name,
    predicates: dict.new(),
    relationships: [],
  )
}

/// Add a predicate to a set
pub fn add_predicate(
  set: PredicateSet,
  name: String,
  description: String,
) -> PredicateSet {
  let pred =
    Predicate(
      name: name,
      description: description,
      domain: set.domain,
      observable: True,
      value_type: BooleanPredicate,
    )
  PredicateSet(..set, predicates: dict.insert(set.predicates, name, pred))
}

/// Add a relationship to a predicate set
pub fn add_relationship(
  set: PredicateSet,
  rel: PredicateRelationship,
) -> PredicateSet {
  PredicateSet(..set, relationships: [rel, ..set.relationships])
}

/// Create a new prediction test case
pub fn prediction_test_case(id: String) -> PredictionTestCase {
  PredictionTestCase(
    id: id,
    name: id,
    description: "",
    category: Deductive,
    domain: "general",
    premises: [],
    conclusion: Atom("undefined"),
    expected: ExpectValid,
    logic_system: S5,
    confidence_threshold: 0.7,
    tags: [],
    source: None,
    priority: Normal,
  )
}

/// Set test case name
pub fn with_name(tc: PredictionTestCase, name: String) -> PredictionTestCase {
  PredictionTestCase(..tc, name: name)
}

/// Set test case description
pub fn with_description(
  tc: PredictionTestCase,
  desc: String,
) -> PredictionTestCase {
  PredictionTestCase(..tc, description: desc)
}

/// Set test category
pub fn with_category(
  tc: PredictionTestCase,
  cat: TestCategory,
) -> PredictionTestCase {
  PredictionTestCase(..tc, category: cat)
}

/// Set test domain
pub fn case_domain(tc: PredictionTestCase, domain: String) -> PredictionTestCase {
  PredictionTestCase(..tc, domain: domain)
}

/// Add a premise
pub fn with_premise(
  tc: PredictionTestCase,
  premise: Proposition,
) -> PredictionTestCase {
  PredictionTestCase(..tc, premises: list.append(tc.premises, [premise]))
}

/// Add multiple premises
pub fn with_premises(
  tc: PredictionTestCase,
  premises: List(Proposition),
) -> PredictionTestCase {
  PredictionTestCase(..tc, premises: list.append(tc.premises, premises))
}

/// Set the conclusion
pub fn with_conclusion(
  tc: PredictionTestCase,
  conclusion: Proposition,
) -> PredictionTestCase {
  PredictionTestCase(..tc, conclusion: conclusion)
}

/// Expect the test to be valid
pub fn expect_valid(tc: PredictionTestCase) -> PredictionTestCase {
  PredictionTestCase(..tc, expected: ExpectValid)
}

/// Expect the test to be invalid
pub fn expect_invalid(
  tc: PredictionTestCase,
  reason: String,
) -> PredictionTestCase {
  PredictionTestCase(..tc, expected: ExpectInvalid(reason))
}

/// Expect either outcome
pub fn expect_either(
  tc: PredictionTestCase,
  explanation: String,
) -> PredictionTestCase {
  PredictionTestCase(..tc, expected: ExpectEither(explanation))
}

/// Expect unknown outcome
pub fn expect_unknown(tc: PredictionTestCase) -> PredictionTestCase {
  PredictionTestCase(..tc, expected: ExpectUnknown)
}

/// Expect specific confidence range
pub fn expect_confidence(
  tc: PredictionTestCase,
  min: Float,
  max: Float,
) -> PredictionTestCase {
  PredictionTestCase(..tc, expected: ExpectConfidence(min, max))
}

/// Set logic system
pub fn with_logic_system(
  tc: PredictionTestCase,
  system: LogicSystem,
) -> PredictionTestCase {
  PredictionTestCase(..tc, logic_system: system)
}

/// Set confidence threshold
pub fn with_confidence_threshold(
  tc: PredictionTestCase,
  threshold: Float,
) -> PredictionTestCase {
  PredictionTestCase(..tc, confidence_threshold: threshold)
}

/// Add tags
pub fn with_tags(
  tc: PredictionTestCase,
  tags: List(String),
) -> PredictionTestCase {
  PredictionTestCase(..tc, tags: list.append(tc.tags, tags))
}

/// Set source reference
pub fn with_source(tc: PredictionTestCase, source: String) -> PredictionTestCase {
  PredictionTestCase(..tc, source: Some(source))
}

/// Set priority
pub fn with_priority(
  tc: PredictionTestCase,
  priority: TestPriority,
) -> PredictionTestCase {
  PredictionTestCase(..tc, priority: priority)
}

// =============================================================================
// Suite Builder Functions
// =============================================================================

/// Create a new test suite
pub fn test_suite(name: String) -> PredictionTestSuite {
  PredictionTestSuite(
    name: name,
    description: "",
    version: "1.0.0",
    test_cases: [],
    predicate_sets: [],
    config: default_suite_config(),
    metadata: SuiteMetadata(
      author: "unknown",
      created: "2026-01-17",
      modified: "2026-01-17",
      related_issues: [],
    ),
  )
}

/// Set suite description
pub fn suite_description(
  suite: PredictionTestSuite,
  desc: String,
) -> PredictionTestSuite {
  PredictionTestSuite(..suite, description: desc)
}

/// Add a test case to suite
pub fn add_test(
  suite: PredictionTestSuite,
  tc: PredictionTestCase,
) -> PredictionTestSuite {
  PredictionTestSuite(..suite, test_cases: list.append(suite.test_cases, [tc]))
}

/// Add multiple test cases
pub fn add_tests(
  suite: PredictionTestSuite,
  tests: List(PredictionTestCase),
) -> PredictionTestSuite {
  PredictionTestSuite(..suite, test_cases: list.append(suite.test_cases, tests))
}

/// Add a predicate set
pub fn add_predicate_set(
  suite: PredictionTestSuite,
  set: PredicateSet,
) -> PredictionTestSuite {
  PredictionTestSuite(
    ..suite,
    predicate_sets: list.append(suite.predicate_sets, [set]),
  )
}

/// Set suite configuration
pub fn with_config(
  suite: PredictionTestSuite,
  config: SuiteConfig,
) -> PredictionTestSuite {
  PredictionTestSuite(..suite, config: config)
}

/// Default suite configuration
pub fn default_suite_config() -> SuiteConfig {
  SuiteConfig(
    parallel: False,
    fail_fast: False,
    timeout_ms: 30_000,
    retries: 0,
    category_filter: None,
    tag_filter: None,
    min_priority: Background,
    verbose: False,
  )
}

/// Fast suite configuration
pub fn fast_suite_config() -> SuiteConfig {
  SuiteConfig(
    parallel: True,
    fail_fast: True,
    timeout_ms: 5000,
    retries: 0,
    category_filter: None,
    tag_filter: None,
    min_priority: Normal,
    verbose: False,
  )
}

/// Thorough suite configuration
pub fn thorough_suite_config() -> SuiteConfig {
  SuiteConfig(
    parallel: True,
    fail_fast: False,
    timeout_ms: 60_000,
    retries: 2,
    category_filter: None,
    tag_filter: None,
    min_priority: Background,
    verbose: True,
  )
}

// =============================================================================
// Test Execution
// =============================================================================

/// Run a single prediction test
pub fn run_prediction_test(
  tc: PredictionTestCase,
  config: ValidatorConfig,
) -> PredictionTestResult {
  // Create formalization
  let formalization =
    Formalization(
      id: tc.id,
      argument_id: tc.id <> "_arg",
      logic_system: tc.logic_system,
      premises: tc.premises,
      conclusion: tc.conclusion,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Create validator state and run validation
  let state = validator.new_state(config)
  let current_time = 0
  let #(_new_state, response) =
    validator.validate(state, formalization, current_time)

  // Build confidence context from response
  let tier_used = case response.tier_used {
    Some(t) -> t
    None -> Tier1Syntactic
  }
  let confidence_context = confidence.default_context(tier_used)

  // Calculate confidence
  let confidence_result =
    confidence.compute_confidence(response.result, confidence_context)

  // Determine if test passed
  let #(passed, explanation) =
    evaluate_outcome(tc.expected, response.result, confidence_result.score)

  // Extract tier string
  let tier_str = case response.tier_used {
    Some(t) -> tier_to_string(t)
    None -> "unknown"
  }

  // Extract countermodel
  let countermodel = case response.result {
    Invalid(cm) -> Some(cm)
    _ -> None
  }

  // Generate warnings
  let warnings = generate_warnings(tc, response, confidence_result)

  PredictionTestResult(
    test_id: tc.id,
    test_name: tc.name,
    passed: passed,
    actual_result: response.result,
    expected_outcome: tc.expected,
    confidence: confidence_result.score,
    confidence_factors: list.map(confidence_result.factors, fn(f) { f.factor }),
    duration_ms: response.duration_ms,
    tier_used: tier_str,
    countermodel: countermodel,
    explanation: explanation,
    warnings: warnings,
  )
}

/// Run a prediction test suite
pub fn run_prediction_suite(
  suite: PredictionTestSuite,
  config: ValidatorConfig,
) -> PredictionSuiteResult {
  // Filter tests by configuration
  let filtered_tests = filter_tests(suite.test_cases, suite.config)

  // Sort by priority
  let sorted_tests = sort_by_priority(filtered_tests)

  // Run tests
  let results =
    list.map(sorted_tests, fn(tc) { run_prediction_test(tc, config) })

  // Calculate statistics
  let passed = list.count(results, fn(r) { r.passed })
  let failed =
    list.count(results, fn(r) { !r.passed && !is_error_result(r.actual_result) })
  let errors = list.count(results, fn(r) { is_error_result(r.actual_result) })
  let total = list.length(results)
  let skipped = list.length(suite.test_cases) - total

  let pass_rate = case total {
    0 -> 0.0
    n -> int.to_float(passed) /. int.to_float(n) *. 100.0
  }

  let total_duration =
    results
    |> list.fold(0, fn(acc, r) { acc + r.duration_ms })

  // Category breakdown
  let category_breakdown = calculate_category_breakdown(results)

  // Generate summary
  let summary = generate_suite_summary(passed, failed, errors, skipped, total)

  PredictionSuiteResult(
    suite_name: suite.name,
    total_tests: total,
    passed: passed,
    failed: failed,
    skipped: skipped,
    errors: errors,
    test_results: results,
    pass_rate: pass_rate,
    total_duration_ms: total_duration,
    category_breakdown: category_breakdown,
    summary: summary,
  )
}

// =============================================================================
// Evaluation Helpers
// =============================================================================

fn evaluate_outcome(
  expected: ExpectedOutcome,
  actual: ValidationResult,
  confidence: Float,
) -> #(Bool, String) {
  case expected, actual {
    // Expected valid
    ExpectValid, Valid -> #(True, "Correctly validated as valid")
    ExpectValid, Invalid(cm) -> #(
      False,
      "Expected valid but got invalid. Countermodel: " <> cm,
    )
    ExpectValid, Unknown(reason) -> #(
      False,
      "Expected valid but got unknown: " <> reason,
    )
    ExpectValid, Timeout -> #(False, "Expected valid but timed out")
    ExpectValid, Error(e) -> #(False, "Expected valid but got error: " <> e)

    // Expected invalid
    ExpectInvalid(_), Invalid(_) -> #(True, "Correctly validated as invalid")
    ExpectInvalid(reason), Valid -> #(
      False,
      "Expected invalid (" <> reason <> ") but got valid",
    )
    ExpectInvalid(_), Unknown(reason) -> #(
      False,
      "Expected invalid but got unknown: " <> reason,
    )
    ExpectInvalid(_), Timeout -> #(False, "Expected invalid but timed out")
    ExpectInvalid(_), Error(e) -> #(
      False,
      "Expected invalid but got error: " <> e,
    )

    // Expected either
    ExpectEither(_), Valid -> #(True, "Either outcome acceptable - got valid")
    ExpectEither(_), Invalid(_) -> #(
      True,
      "Either outcome acceptable - got invalid",
    )
    ExpectEither(exp), Unknown(reason) -> #(
      False,
      "Expected valid or invalid (" <> exp <> ") but got unknown: " <> reason,
    )
    ExpectEither(_), Timeout -> #(
      False,
      "Expected valid or invalid but timed out",
    )
    ExpectEither(_), Error(e) -> #(
      False,
      "Expected valid or invalid but got error: " <> e,
    )

    // Expected unknown
    ExpectUnknown, Unknown(_) -> #(True, "Correctly returned unknown")
    ExpectUnknown, Valid -> #(False, "Expected unknown but got valid")
    ExpectUnknown, Invalid(_) -> #(False, "Expected unknown but got invalid")
    ExpectUnknown, Timeout -> #(True, "Unknown/timeout acceptable")
    ExpectUnknown, Error(_) -> #(True, "Unknown/error acceptable")

    // Expected timeout
    ExpectTimeout, Timeout -> #(True, "Correctly timed out")
    ExpectTimeout, _ -> #(False, "Expected timeout but completed")

    // Expected confidence range
    ExpectConfidence(min, max), _ ->
      case confidence >=. min && confidence <=. max {
        True -> #(
          True,
          "Confidence "
            <> float_to_string(confidence)
            <> " in expected range ["
            <> float_to_string(min)
            <> ", "
            <> float_to_string(max)
            <> "]",
        )
        False -> #(
          False,
          "Confidence "
            <> float_to_string(confidence)
            <> " outside expected range ["
            <> float_to_string(min)
            <> ", "
            <> float_to_string(max)
            <> "]",
        )
      }
  }
}

fn generate_warnings(
  tc: PredictionTestCase,
  response: validator.ValidationResponse,
  conf_result: ConfidenceResult,
) -> List(String) {
  let warnings = []

  // Low confidence warning
  let warnings = case conf_result.score <. tc.confidence_threshold {
    True -> [
      "Confidence "
        <> float_to_string(conf_result.score)
        <> " below threshold "
        <> float_to_string(tc.confidence_threshold),
      ..warnings
    ]
    False -> warnings
  }

  // Timeout warning
  let warnings = case response.duration_ms > 10_000 {
    True -> [
      "Slow validation: " <> int.to_string(response.duration_ms) <> "ms",
      ..warnings
    ]
    False -> warnings
  }

  // Cache miss for repeated test (from_cache field)
  let warnings = case response.from_cache {
    True -> warnings
    False -> warnings
  }

  warnings
}

fn filter_tests(
  tests: List(PredictionTestCase),
  config: SuiteConfig,
) -> List(PredictionTestCase) {
  tests
  |> list.filter(fn(t) {
    priority_value(t.priority) >= priority_value(config.min_priority)
  })
  |> list.filter(fn(t) {
    case config.category_filter {
      None -> True
      Some(cats) -> list.contains(cats, t.category)
    }
  })
  |> list.filter(fn(t) {
    case config.tag_filter {
      None -> True
      Some(tags) -> list.any(t.tags, fn(tag) { list.contains(tags, tag) })
    }
  })
}

fn sort_by_priority(tests: List(PredictionTestCase)) -> List(PredictionTestCase) {
  list.sort(tests, fn(a, b) {
    int.compare(priority_value(b.priority), priority_value(a.priority))
  })
}

fn priority_value(p: TestPriority) -> Int {
  case p {
    Critical -> 5
    High -> 4
    Normal -> 3
    Low -> 2
    Background -> 1
  }
}

fn is_error_result(result: ValidationResult) -> Bool {
  case result {
    Error(_) -> True
    Timeout -> True
    _ -> False
  }
}

fn calculate_category_breakdown(
  results: List(PredictionTestResult),
) -> Dict(String, CategoryStats) {
  // Group by category from test_id prefix (simplified)
  results
  |> list.group(fn(r) {
    case string.split(r.test_id, "_") {
      [cat, ..] -> cat
      [] -> "unknown"
    }
  })
  |> dict.map_values(fn(_key, group) {
    let total = list.length(group)
    let passed = list.count(group, fn(r) { r.passed })
    let failed = total - passed
    let pass_rate = case total {
      0 -> 0.0
      n -> int.to_float(passed) /. int.to_float(n) *. 100.0
    }
    CategoryStats(
      total: total,
      passed: passed,
      failed: failed,
      pass_rate: pass_rate,
    )
  })
}

fn generate_suite_summary(
  passed: Int,
  failed: Int,
  errors: Int,
  skipped: Int,
  total: Int,
) -> String {
  let pass_rate = case total {
    0 -> 0.0
    n -> int.to_float(passed) /. int.to_float(n) *. 100.0
  }

  let status = case failed == 0 && errors == 0 {
    True -> "ALL TESTS PASSED"
    False -> "SOME TESTS FAILED"
  }

  status
  <> ": "
  <> int.to_string(passed)
  <> " passed, "
  <> int.to_string(failed)
  <> " failed, "
  <> int.to_string(errors)
  <> " errors, "
  <> int.to_string(skipped)
  <> " skipped ("
  <> float_to_string(pass_rate)
  <> "% pass rate)"
}

fn tier_to_string(tier: ValidationTier) -> String {
  case tier {
    Tier1Syntactic -> "Tier1_Syntactic"
    Tier2TruthTable -> "Tier2_TruthTable"
    Tier3Z3 -> "Tier3_Z3"
  }
}

// =============================================================================
// Formatting and Reporting
// =============================================================================

/// Format a test result for display
pub fn format_test_result(result: PredictionTestResult) -> String {
  let status = case result.passed {
    True -> "[PASS]"
    False -> "[FAIL]"
  }

  let lines = [
    status <> " " <> result.test_name <> " (" <> result.test_id <> ")",
    "  Result: " <> format_validation_result(result.actual_result),
    "  Confidence: " <> float_to_string(result.confidence),
    "  Duration: " <> int.to_string(result.duration_ms) <> "ms",
    "  Tier: " <> result.tier_used,
    "  Explanation: " <> result.explanation,
  ]

  let lines = case result.countermodel {
    Some(cm) -> list.append(lines, ["  Countermodel: " <> cm])
    None -> lines
  }

  let lines = case result.warnings {
    [] -> lines
    ws ->
      list.append(lines, [
        "  Warnings: " <> string.join(ws, "; "),
      ])
  }

  string.join(lines, "\n")
}

/// Format a suite result for display
pub fn format_suite_result(result: PredictionSuiteResult) -> String {
  let header = [
    string.repeat("=", 70),
    "PREDICTION TEST SUITE: " <> result.suite_name,
    string.repeat("=", 70),
    "",
    "Summary: " <> result.summary,
    "Total Duration: " <> int.to_string(result.total_duration_ms) <> "ms",
    "",
    string.repeat("-", 70),
    "DETAILED RESULTS",
    string.repeat("-", 70),
    "",
  ]

  let test_results = list.map(result.test_results, format_test_result)

  let footer = [
    "",
    string.repeat("-", 70),
    "CATEGORY BREAKDOWN",
    string.repeat("-", 70),
  ]

  let category_lines =
    result.category_breakdown
    |> dict.to_list()
    |> list.map(fn(pair) {
      let #(cat, stats) = pair
      cat
      <> ": "
      <> int.to_string(stats.passed)
      <> "/"
      <> int.to_string(stats.total)
      <> " ("
      <> float_to_string(stats.pass_rate)
      <> "%)"
    })

  string.join(
    list.flatten([header, test_results, footer, category_lines]),
    "\n",
  )
}

fn format_validation_result(result: ValidationResult) -> String {
  case result {
    Valid -> "Valid"
    Invalid(cm) -> "Invalid: " <> string.slice(cm, 0, 50) <> "..."
    Unknown(reason) -> "Unknown: " <> reason
    Timeout -> "Timeout"
    Error(e) -> "Error: " <> e
  }
}

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let frac =
    float_truncate(float.absolute_value({ f -. int.to_float(whole) } *. 100.0))
  int.to_string(whole) <> "." <> pad_left(int.to_string(frac), 2, "0")
}

fn pad_left(s: String, len: Int, char: String) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left(char <> s, len, char)
  }
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int
