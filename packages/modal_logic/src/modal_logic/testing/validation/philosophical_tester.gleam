//// Philosophical Tester
////
//// This module provides a comprehensive testing framework that validates
//// inference rules against philosophical arguments from the corpus.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{type LogicSystem, type Proposition}
import modal_logic/rules/inference_rule.{
  type Bindings, type InferenceRule, type RuleApplicationResult, Applied,
  DerivationFailed, NoMatch, apply_rule, propositions_equal,
}
import modal_logic/rules/rule_store.{type RuleStore}
import modal_logic/testing/validation/argument_corpus.{
  type ArgumentCategory, type PhilosophicalArgument,
}

// ============ Core Types ============

/// Configuration for philosophical testing
pub type PhilosophicalTestConfig {
  PhilosophicalTestConfig(
    /// Include valid arguments
    test_valid: Bool,
    /// Include invalid arguments (fallacies)
    test_invalid: Bool,
    /// Specific categories to test
    categories: Option(List(ArgumentCategory)),
    /// Specific logic systems to test
    systems: Option(List(LogicSystem)),
    /// Maximum arguments to test (None = all)
    max_arguments: Option(Int),
    /// Generate detailed traces
    generate_traces: Bool,
    /// Fail fast on first error
    fail_fast: Bool,
  )
}

/// Default test configuration
pub fn default_config() -> PhilosophicalTestConfig {
  PhilosophicalTestConfig(
    test_valid: True,
    test_invalid: True,
    categories: None,
    systems: None,
    max_arguments: None,
    generate_traces: True,
    fail_fast: False,
  )
}

/// Configuration for testing only valid arguments
pub fn valid_only_config() -> PhilosophicalTestConfig {
  PhilosophicalTestConfig(
    ..default_config(),
    test_valid: True,
    test_invalid: False,
  )
}

/// Configuration for testing only invalid arguments (fallacies)
pub fn invalid_only_config() -> PhilosophicalTestConfig {
  PhilosophicalTestConfig(
    ..default_config(),
    test_valid: False,
    test_invalid: True,
  )
}

/// Result of testing a single argument
pub type ArgumentTestResult {
  ArgumentTestResult(
    /// The argument tested
    argument: PhilosophicalArgument,
    /// Whether the test passed
    passed: Bool,
    /// Expected validity
    expected_valid: Bool,
    /// Actual validation result
    actual_result: ValidationResult,
    /// Inference trace (if generated)
    trace: Option(InferenceTrace),
    /// Rules that successfully applied
    rules_applied: List(RuleApplication),
    /// Error message if test failed
    error_message: Option(String),
  )
}

/// Result of validation attempt
pub type ValidationResult {
  /// Successfully derived the conclusion
  Derived(conclusion: Proposition, steps: Int)
  /// Could not derive the conclusion
  NotDerived(reason: String)
  /// Found a contradiction
  Contradiction(explanation: String)
}

/// Record of a rule application
pub type RuleApplication {
  RuleApplication(
    /// Rule that was applied
    rule_id: String,
    /// Rule name
    rule_name: String,
    /// Premises used
    premises: List(Proposition),
    /// Conclusion derived
    conclusion: Proposition,
    /// Variable bindings
    bindings: Bindings,
  )
}

/// An inference trace showing derivation steps
pub type InferenceTrace {
  InferenceTrace(
    /// Starting premises
    initial_premises: List(Proposition),
    /// Target conclusion
    target_conclusion: Proposition,
    /// Steps taken
    steps: List(TraceStep),
    /// Whether target was reached
    target_reached: Bool,
    /// Total steps taken
    total_steps: Int,
  )
}

/// A single step in an inference trace
pub type TraceStep {
  TraceStep(
    /// Step number
    step_number: Int,
    /// Rule applied
    rule_id: String,
    /// Premises used in this step
    premises_used: List(Proposition),
    /// Conclusion of this step
    derived: Proposition,
    /// Explanation
    explanation: String,
  )
}

/// Overall test results
pub type PhilosophicalTestResult {
  PhilosophicalTestResult(
    /// Configuration used
    config: PhilosophicalTestConfig,
    /// Total arguments tested
    total_tested: Int,
    /// Arguments correctly validated
    correctly_validated: Int,
    /// Arguments incorrectly validated
    incorrectly_validated: Int,
    /// Individual argument results
    argument_results: List(ArgumentTestResult),
    /// Statistics by rule
    rule_statistics: Dict(String, RuleStatistic),
    /// Overall soundness assessment
    soundness_assessment: SoundnessAssessment,
  )
}

/// Statistics for a single rule
pub type RuleStatistic {
  RuleStatistic(
    /// Rule ID
    rule_id: String,
    /// Rule name
    rule_name: String,
    /// Times successfully applied
    successful_applications: Int,
    /// Times application attempted but failed
    failed_applications: Int,
    /// Arguments where this rule contributed
    contributed_to: Int,
  )
}

/// Soundness assessment
pub type SoundnessAssessment {
  SoundnessAssessment(
    /// Overall soundness score (0.0 - 1.0)
    score: Float,
    /// Assessment category
    category: SoundnessCategory,
    /// Detailed notes
    notes: List(String),
  )
}

/// Soundness categories
pub type SoundnessCategory {
  /// All tests passed
  FullySoundness
  /// Most tests passed (> 90%)
  MostlySoundness
  /// Significant failures (< 90%)
  PartialSoundness
  /// Many failures (< 50%)
  Unsound
}

// ============ Test Execution ============

/// Run philosophical tests with the given configuration
pub fn run_tests(
  store: RuleStore,
  config: PhilosophicalTestConfig,
) -> PhilosophicalTestResult {
  // Get arguments to test
  let arguments = select_arguments(config)

  // Initialize statistics
  let initial_stats: Dict(String, RuleStatistic) = dict.new()

  // Run tests
  let #(results, rule_stats) =
    list.fold(arguments, #([], initial_stats), fn(acc, arg) {
      let #(results_so_far, stats_so_far) = acc

      // Check fail-fast
      case config.fail_fast && has_failure(results_so_far) {
        True -> acc
        False -> {
          let #(result, new_stats) =
            test_argument(store, arg, config, stats_so_far)
          #([result, ..results_so_far], new_stats)
        }
      }
    })

  // Calculate totals
  let results_reversed = list.reverse(results)
  let total = list.length(results_reversed)
  let correct =
    list.filter(results_reversed, fn(r) { r.passed })
    |> list.length

  // Calculate soundness
  let assessment = assess_soundness(results_reversed)

  PhilosophicalTestResult(
    config: config,
    total_tested: total,
    correctly_validated: correct,
    incorrectly_validated: total - correct,
    argument_results: results_reversed,
    rule_statistics: rule_stats,
    soundness_assessment: assessment,
  )
}

/// Select arguments based on configuration
fn select_arguments(
  config: PhilosophicalTestConfig,
) -> List(PhilosophicalArgument) {
  let all = argument_corpus.all_arguments()

  // Filter by validity
  let filtered =
    list.filter(all, fn(arg) {
      case arg.is_valid, config.test_valid, config.test_invalid {
        True, True, _ -> True
        False, _, True -> True
        _, _, _ -> False
      }
    })

  // Filter by category
  let filtered_by_category = case config.categories {
    None -> filtered
    Some(categories) ->
      list.filter(filtered, fn(arg) { list.contains(categories, arg.category) })
  }

  // Filter by system
  let filtered_by_system = case config.systems {
    None -> filtered_by_category
    Some(systems) ->
      list.filter(filtered_by_category, fn(arg) {
        list.any(arg.valid_in, fn(sys) { list.contains(systems, sys) })
      })
  }

  // Apply max limit
  case config.max_arguments {
    None -> filtered_by_system
    Some(max) -> list.take(filtered_by_system, max)
  }
}

/// Check if any result is a failure
fn has_failure(results: List(ArgumentTestResult)) -> Bool {
  list.any(results, fn(r) { !r.passed })
}

/// Test a single argument
fn test_argument(
  store: RuleStore,
  arg: PhilosophicalArgument,
  config: PhilosophicalTestConfig,
  stats: Dict(String, RuleStatistic),
) -> #(ArgumentTestResult, Dict(String, RuleStatistic)) {
  // Get rules from store
  let rules = rule_store.list_rules(store)

  // Try to derive conclusion
  let #(validation_result, applications, new_stats) =
    attempt_derivation(arg.premises, arg.conclusion, rules, stats)

  // Generate trace if requested
  let trace = case config.generate_traces {
    True -> Some(generate_trace(arg.premises, arg.conclusion, applications))
    False -> None
  }

  // Determine if test passed
  let derivation_succeeded = case validation_result {
    Derived(_, _) -> True
    NotDerived(_) -> False
    Contradiction(_) -> False
  }

  // Test passes if:
  // - Valid argument and we derived the conclusion
  // - Invalid argument and we did NOT derive the conclusion
  let passed = case arg.is_valid {
    True -> derivation_succeeded
    False -> !derivation_succeeded
  }

  let error_message = case passed {
    True -> None
    False ->
      case arg.is_valid {
        True ->
          Some(
            "Expected to derive conclusion but failed: "
            <> format_validation(validation_result),
          )
        False ->
          Some("Expected NOT to derive conclusion (fallacy) but succeeded")
      }
  }

  let result =
    ArgumentTestResult(
      argument: arg,
      passed: passed,
      expected_valid: arg.is_valid,
      actual_result: validation_result,
      trace: trace,
      rules_applied: applications,
      error_message: error_message,
    )

  #(result, new_stats)
}

/// Attempt to derive the conclusion from premises using available rules
fn attempt_derivation(
  premises: List(Proposition),
  conclusion: Proposition,
  rules: List(InferenceRule),
  stats: Dict(String, RuleStatistic),
) -> #(ValidationResult, List(RuleApplication), Dict(String, RuleStatistic)) {
  // Check if conclusion is already in premises
  case list.any(premises, fn(p) { propositions_equal(p, conclusion) }) {
    True -> #(Derived(conclusion, 0), [], stats)
    False -> {
      // Try forward chaining with limited depth
      forward_chain(premises, conclusion, rules, stats, 0, 10, [])
    }
  }
}

/// Forward chaining derivation with depth limit
fn forward_chain(
  known: List(Proposition),
  target: Proposition,
  rules: List(InferenceRule),
  stats: Dict(String, RuleStatistic),
  depth: Int,
  max_depth: Int,
  applications: List(RuleApplication),
) -> #(ValidationResult, List(RuleApplication), Dict(String, RuleStatistic)) {
  // Check depth limit
  case depth >= max_depth {
    True -> #(NotDerived("Max depth reached"), applications, stats)
    False -> {
      // Check if target is in known facts
      case list.any(known, fn(k) { propositions_equal(k, target) }) {
        True -> #(Derived(target, depth), applications, stats)
        False -> {
          // Try applying each rule
          let #(new_facts, new_apps, new_stats) =
            try_all_rules(known, rules, stats)

          // Check if we derived anything new
          case new_facts {
            [] -> #(NotDerived("No more rules apply"), applications, new_stats)
            _ -> {
              // Add new facts and continue
              let updated_known = list.append(known, new_facts)
              let updated_apps = list.append(applications, new_apps)

              // Check if target is now known
              case
                list.any(new_facts, fn(f) { propositions_equal(f, target) })
              {
                True -> #(Derived(target, depth + 1), updated_apps, new_stats)
                False ->
                  forward_chain(
                    updated_known,
                    target,
                    rules,
                    new_stats,
                    depth + 1,
                    max_depth,
                    updated_apps,
                  )
              }
            }
          }
        }
      }
    }
  }
}

/// Try applying all rules to known facts
fn try_all_rules(
  known: List(Proposition),
  rules: List(InferenceRule),
  stats: Dict(String, RuleStatistic),
) -> #(List(Proposition), List(RuleApplication), Dict(String, RuleStatistic)) {
  list.fold(rules, #([], [], stats), fn(acc, rule) {
    let #(facts, apps, current_stats) = acc

    // Try this rule with different premise combinations
    let #(new_facts, new_apps, updated_stats) =
      try_rule_all_combos(known, rule, current_stats)

    #(list.append(facts, new_facts), list.append(apps, new_apps), updated_stats)
  })
}

/// Try a rule with all possible premise combinations
fn try_rule_all_combos(
  known: List(Proposition),
  rule: InferenceRule,
  stats: Dict(String, RuleStatistic),
) -> #(List(Proposition), List(RuleApplication), Dict(String, RuleStatistic)) {
  let premise_count = list.length(rule.premise_patterns)

  // Generate combinations of known facts
  let combinations = generate_combinations(known, premise_count)

  list.fold(combinations, #([], [], stats), fn(acc, combo) {
    let #(facts, apps, current_stats) = acc

    case apply_rule(rule, combo) {
      Applied(conclusion, bindings) -> {
        // Check if this is new
        case
          list.any(known, fn(k) { propositions_equal(k, conclusion) })
          || list.any(facts, fn(f) { propositions_equal(f, conclusion) })
        {
          True -> acc
          False -> {
            let app =
              RuleApplication(
                rule_id: rule.id,
                rule_name: rule.name,
                premises: combo,
                conclusion: conclusion,
                bindings: bindings,
              )
            let new_stats = update_stats(current_stats, rule, True)
            #([conclusion, ..facts], [app, ..apps], new_stats)
          }
        }
      }
      NoMatch(_) -> {
        let new_stats = update_stats(current_stats, rule, False)
        #(facts, apps, new_stats)
      }
      DerivationFailed(_) -> {
        let new_stats = update_stats(current_stats, rule, False)
        #(facts, apps, new_stats)
      }
    }
  })
}

/// Generate all combinations of n elements from a list
fn generate_combinations(
  items: List(Proposition),
  n: Int,
) -> List(List(Proposition)) {
  case n {
    0 -> [[]]
    1 -> list.map(items, fn(x) { [x] })
    2 -> {
      list.flat_map(items, fn(x) { list.map(items, fn(y) { [x, y] }) })
    }
    _ -> {
      // For larger n, use a simpler approach (limited combinations)
      case items {
        [] -> []
        [first, ..rest] -> {
          let with_first =
            generate_combinations(rest, n - 1)
            |> list.map(fn(combo) { [first, ..combo] })
          let without_first = generate_combinations(rest, n)
          list.append(with_first, without_first)
        }
      }
    }
  }
}

/// Update rule statistics
fn update_stats(
  stats: Dict(String, RuleStatistic),
  rule: InferenceRule,
  success: Bool,
) -> Dict(String, RuleStatistic) {
  let current = case dict.get(stats, rule.id) {
    Ok(s) -> s
    Error(_) ->
      RuleStatistic(
        rule_id: rule.id,
        rule_name: rule.name,
        successful_applications: 0,
        failed_applications: 0,
        contributed_to: 0,
      )
  }

  let updated = case success {
    True ->
      RuleStatistic(
        ..current,
        successful_applications: current.successful_applications + 1,
      )
    False ->
      RuleStatistic(
        ..current,
        failed_applications: current.failed_applications + 1,
      )
  }

  dict.insert(stats, rule.id, updated)
}

/// Generate an inference trace
fn generate_trace(
  premises: List(Proposition),
  target: Proposition,
  applications: List(RuleApplication),
) -> InferenceTrace {
  let steps =
    list.index_map(applications, fn(app, idx) {
      TraceStep(
        step_number: idx + 1,
        rule_id: app.rule_id,
        premises_used: app.premises,
        derived: app.conclusion,
        explanation: app.rule_name <> " applied to derive conclusion",
      )
    })

  let target_reached =
    list.any(applications, fn(app) {
      propositions_equal(app.conclusion, target)
    })

  InferenceTrace(
    initial_premises: premises,
    target_conclusion: target,
    steps: steps,
    target_reached: target_reached,
    total_steps: list.length(steps),
  )
}

/// Assess overall soundness from results
fn assess_soundness(results: List(ArgumentTestResult)) -> SoundnessAssessment {
  let total = list.length(results)
  let passed = list.length(list.filter(results, fn(r) { r.passed }))

  let score = case total {
    0 -> 1.0
    _ -> int.to_float(passed) /. int.to_float(total)
  }

  let category = case score {
    s if s == 1.0 -> FullySoundness
    s if s >=. 0.9 -> MostlySoundness
    s if s >=. 0.5 -> PartialSoundness
    _ -> Unsound
  }

  let notes = generate_soundness_notes(results, score)

  SoundnessAssessment(score: score, category: category, notes: notes)
}

/// Generate notes about soundness issues
fn generate_soundness_notes(
  results: List(ArgumentTestResult),
  score: Float,
) -> List(String) {
  let failures = list.filter(results, fn(r) { !r.passed })

  let notes = case score {
    s if s == 1.0 -> ["All tests passed successfully"]
    _ -> {
      list.map(failures, fn(f) {
        "Failed: "
        <> f.argument.name
        <> " - "
        <> option.unwrap(f.error_message, "Unknown error")
      })
    }
  }

  notes
}

// ============ Formatting Functions ============

/// Format validation result as string
fn format_validation(result: ValidationResult) -> String {
  case result {
    Derived(_, steps) -> "Derived in " <> int.to_string(steps) <> " steps"
    NotDerived(reason) -> "Not derived: " <> reason
    Contradiction(explanation) -> "Contradiction: " <> explanation
  }
}

/// Format test result as summary string
pub fn format_result(result: PhilosophicalTestResult) -> String {
  let score_str =
    int.to_string(float.round(result.soundness_assessment.score *. 100.0))

  string.concat([
    "Philosophical Test Results\n",
    "==========================\n",
    "Total Arguments: ",
    int.to_string(result.total_tested),
    "\n",
    "Correctly Validated: ",
    int.to_string(result.correctly_validated),
    "\n",
    "Incorrectly Validated: ",
    int.to_string(result.incorrectly_validated),
    "\n",
    "Soundness Score: ",
    score_str,
    "%\n",
    "Assessment: ",
    format_category(result.soundness_assessment.category),
    "\n",
  ])
}

/// Format soundness category
fn format_category(category: SoundnessCategory) -> String {
  case category {
    FullySoundness -> "Fully Sound"
    MostlySoundness -> "Mostly Sound"
    PartialSoundness -> "Partially Sound"
    Unsound -> "Unsound"
  }
}

/// Format inference trace
pub fn format_trace(trace: InferenceTrace) -> String {
  let steps_str =
    trace.steps
    |> list.map(fn(step) {
      "  Step "
      <> int.to_string(step.step_number)
      <> ": "
      <> step.rule_id
      <> " - "
      <> step.explanation
    })
    |> string.join("\n")

  string.concat([
    "Inference Trace\n",
    "---------------\n",
    "Initial Premises: ",
    int.to_string(list.length(trace.initial_premises)),
    "\n",
    "Target Reached: ",
    case trace.target_reached {
      True -> "Yes"
      False -> "No"
    },
    "\n",
    "Total Steps: ",
    int.to_string(trace.total_steps),
    "\n",
    "Steps:\n",
    steps_str,
    "\n",
  ])
}

/// Get failed tests
pub fn get_failures(result: PhilosophicalTestResult) -> List(ArgumentTestResult) {
  list.filter(result.argument_results, fn(r) { !r.passed })
}

/// Get passed tests
pub fn get_passes(result: PhilosophicalTestResult) -> List(ArgumentTestResult) {
  list.filter(result.argument_results, fn(r) { r.passed })
}

/// Calculate pass rate
pub fn pass_rate(result: PhilosophicalTestResult) -> Float {
  result.soundness_assessment.score
}
