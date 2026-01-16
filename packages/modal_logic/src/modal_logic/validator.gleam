//// Validation Orchestrator
////
//// This module orchestrates the validation of modal logic arguments.
//// It coordinates between cache, Z3 compilation, and result storage.
////
//// ## Validation Pipeline
////
//// 1. Check cache for existing result
//// 2. Compile formalization to Z3 SMT-LIB format
//// 3. Run Z3 solver (or mock for testing)
//// 4. Parse and format result
//// 5. Store in cache
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/validator
////
//// let config = validator.default_config()
//// let result = validator.validate(config, formalization)
//// ```

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/argument.{
  type Formalization, type ValidationResult, Invalid, Valid,
}
import modal_logic/cache.{type CacheConfig, type MemoryCache}
import modal_logic/heuristics.{
  type HeuristicConfig, type HeuristicResult, type ValidationTier,
  Tier1Syntactic, Tier2TruthTable, Tier3Z3,
}
import modal_logic/proposition.{type LogicSystem, type Proposition}
import z3/modal/compile as z3_compile
import z3/solver.{type SolverCheckResult, SolverSat, SolverUnknown, SolverUnsat}
import z3/types.{type Expr as Z3Expr, type Z3Error, BoolVal}

// =============================================================================
// Types
// =============================================================================

/// Configuration for the validation orchestrator
pub type ValidatorConfig {
  ValidatorConfig(
    /// Cache configuration
    cache_config: CacheConfig,
    /// Timeout for Z3 solver in milliseconds
    solver_timeout_ms: Int,
    /// Maximum number of worlds to explore
    max_worlds: Int,
    /// Whether to use parallel validation
    parallel: Bool,
    /// Number of parallel workers
    worker_count: Int,
    /// Whether to generate detailed countermodels
    detailed_countermodels: Bool,
    /// Whether Z3 is required (fail if unavailable) or optional (graceful degradation)
    require_z3: Bool,
    /// Degradation mode when Z3 is unavailable
    z3_degradation_mode: Z3DegradationMode,
  )
}

/// Degradation mode when Z3 is not available
pub type Z3DegradationMode {
  /// Fail immediately with an error if Z3 is unavailable
  FailFast
  /// Return Unknown/Inconclusive results when Z3 is unavailable
  ReturnUnknown
  /// Use a mock/heuristic validator as fallback
  UseFallback
}

/// A validation request
pub type ValidationRequest {
  ValidationRequest(
    /// Unique request ID
    id: String,
    /// The formalization to validate
    formalization: Formalization,
    /// Priority (higher = more urgent)
    priority: Int,
    /// Optional callback identifier
    callback_id: Option(String),
  )
}

/// Result of a validation attempt
pub type ValidationResponse {
  ValidationResponse(
    /// Request ID
    request_id: String,
    /// The validation result
    result: ValidationResult,
    /// Whether result came from cache
    from_cache: Bool,
    /// Time taken in milliseconds
    duration_ms: Int,
    /// Number of worlds explored (if applicable)
    worlds_explored: Option(Int),
    /// SMT-LIB formula used (for debugging)
    smt_formula: Option(String),
    /// Which validation tier was used (Tier1Syntactic, Tier2TruthTable, Tier3Z3)
    tier_used: Option(ValidationTier),
    /// Explanation of how result was determined (for heuristic tiers)
    tier_explanation: Option(String),
  )
}

/// Batch validation request
pub type BatchRequest {
  BatchRequest(
    /// Batch ID
    id: String,
    /// List of validation requests
    requests: List(ValidationRequest),
    /// Whether to stop on first invalid
    stop_on_invalid: Bool,
  )
}

/// Batch validation response
pub type BatchResponse {
  BatchResponse(
    /// Batch ID
    batch_id: String,
    /// Individual responses
    responses: List(ValidationResponse),
    /// Total time in milliseconds
    total_duration_ms: Int,
    /// Summary statistics
    stats: BatchStats,
  )
}

/// Statistics for batch validation
pub type BatchStats {
  BatchStats(
    /// Number of valid results
    valid_count: Int,
    /// Number of invalid results
    invalid_count: Int,
    /// Number of errors/timeouts
    error_count: Int,
    /// Number of cache hits
    cache_hits: Int,
  )
}

/// Kripke world in a countermodel
pub type KripkeWorld {
  KripkeWorld(
    /// World name/identifier
    name: String,
    /// Propositions true in this world
    true_props: List(String),
    /// Propositions false in this world
    false_props: List(String),
  )
}

/// Accessibility relation in a countermodel
pub type AccessibilityRelation {
  AccessibilityRelation(
    /// Source world
    from: String,
    /// Target world
    to: String,
  )
}

/// A structured countermodel
pub type Countermodel {
  Countermodel(
    /// List of worlds
    worlds: List(KripkeWorld),
    /// Accessibility relations
    relations: List(AccessibilityRelation),
    /// The designated actual world
    actual_world: String,
    /// Logic system used
    logic_system: LogicSystem,
  )
}

/// Validation state for orchestrator
pub type ValidatorState {
  ValidatorState(
    /// Configuration
    config: ValidatorConfig,
    /// Memory cache
    cache: MemoryCache,
    /// Pending requests
    pending: List(ValidationRequest),
    /// Completed count
    completed_count: Int,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default validator configuration
///
/// Default configuration uses graceful degradation (ReturnUnknown) when Z3
/// is unavailable, allowing the application to continue with limited functionality.
pub fn default_config() -> ValidatorConfig {
  ValidatorConfig(
    cache_config: cache.default_config(),
    solver_timeout_ms: 30_000,
    max_worlds: 10,
    parallel: False,
    worker_count: 4,
    detailed_countermodels: True,
    require_z3: False,
    z3_degradation_mode: ReturnUnknown,
  )
}

/// Create fast configuration (shorter timeout, less detail)
pub fn fast_config() -> ValidatorConfig {
  ValidatorConfig(
    cache_config: cache.fast_config(),
    solver_timeout_ms: 5000,
    max_worlds: 5,
    parallel: False,
    worker_count: 2,
    detailed_countermodels: False,
    require_z3: False,
    z3_degradation_mode: ReturnUnknown,
  )
}

/// Create thorough configuration (longer timeout, more detail)
pub fn thorough_config() -> ValidatorConfig {
  ValidatorConfig(
    cache_config: cache.persistent_config(),
    solver_timeout_ms: 60_000,
    max_worlds: 20,
    parallel: True,
    worker_count: 8,
    detailed_countermodels: True,
    require_z3: False,
    z3_degradation_mode: ReturnUnknown,
  )
}

/// Create a strict configuration that requires Z3
///
/// Use this when Z3 availability is mandatory for your use case.
/// Validation will fail with an error if Z3 is not installed.
pub fn strict_config() -> ValidatorConfig {
  ValidatorConfig(
    cache_config: cache.default_config(),
    solver_timeout_ms: 30_000,
    max_worlds: 10,
    parallel: False,
    worker_count: 4,
    detailed_countermodels: True,
    require_z3: True,
    z3_degradation_mode: FailFast,
  )
}

/// Set solver timeout
pub fn with_timeout(config: ValidatorConfig, timeout_ms: Int) -> ValidatorConfig {
  ValidatorConfig(..config, solver_timeout_ms: timeout_ms)
}

/// Set max worlds
pub fn with_max_worlds(config: ValidatorConfig, max: Int) -> ValidatorConfig {
  ValidatorConfig(..config, max_worlds: max)
}

/// Enable/disable parallel validation
pub fn with_parallel(config: ValidatorConfig, enabled: Bool) -> ValidatorConfig {
  ValidatorConfig(..config, parallel: enabled)
}

/// Set whether Z3 is required
///
/// If True, validation will fail with an error if Z3 is not available.
/// If False, validation will use graceful degradation based on the
/// configured degradation mode.
pub fn with_require_z3(
  config: ValidatorConfig,
  required: Bool,
) -> ValidatorConfig {
  ValidatorConfig(..config, require_z3: required)
}

/// Set the Z3 degradation mode
///
/// Controls how the validator behaves when Z3 is unavailable:
/// - FailFast: Immediately return an error
/// - ReturnUnknown: Return Unknown/Inconclusive results
/// - UseFallback: Use a heuristic-based fallback validator
pub fn with_degradation_mode(
  config: ValidatorConfig,
  mode: Z3DegradationMode,
) -> ValidatorConfig {
  ValidatorConfig(..config, z3_degradation_mode: mode)
}

// =============================================================================
// Tier Information Functions
// =============================================================================

/// Get a human-readable description of a validation tier
pub fn tier_description(tier: ValidationTier) -> String {
  heuristics.tier_description(tier)
}

/// Get the expected latency in milliseconds for a tier
pub fn tier_expected_latency_ms(tier: ValidationTier) -> Int {
  heuristics.tier_expected_latency_ms(tier)
}

/// Check if a tier is a heuristic tier (Tier 1 or 2)
pub fn is_heuristic_tier(tier: ValidationTier) -> Bool {
  case tier {
    Tier1Syntactic -> True
    Tier2TruthTable -> True
    Tier3Z3 -> False
  }
}

/// Get the tier name as a string
pub fn tier_name(tier: ValidationTier) -> String {
  case tier {
    Tier1Syntactic -> "Tier1Syntactic"
    Tier2TruthTable -> "Tier2TruthTable"
    Tier3Z3 -> "Tier3Z3"
  }
}

// =============================================================================
// Validation Functions
// =============================================================================

/// Create a new validator state
pub fn new_state(config: ValidatorConfig) -> ValidatorState {
  ValidatorState(
    config: config,
    cache: cache.new_memory_cache(config.cache_config),
    pending: [],
    completed_count: 0,
  )
}

/// Validate a single formalization
pub fn validate(
  state: ValidatorState,
  formalization: Formalization,
  current_time: Int,
) -> #(ValidatorState, ValidationResponse) {
  let request =
    ValidationRequest(
      id: formalization.id,
      formalization: formalization,
      priority: 0,
      callback_id: None,
    )

  validate_request(state, request, current_time)
}

/// Validate a request
pub fn validate_request(
  state: ValidatorState,
  request: ValidationRequest,
  current_time: Int,
) -> #(ValidatorState, ValidationResponse) {
  // Check cache first
  let cache_key =
    cache.validation_cache_key(state.config.cache_config, request.formalization)

  let #(new_cache, cache_result) =
    cache.memory_get(state.cache, cache_key, current_time)

  case cache_result {
    cache.CacheHit(cached_result) -> {
      // Return cached result
      let response =
        ValidationResponse(
          request_id: request.id,
          result: cached_result,
          from_cache: True,
          duration_ms: 0,
          worlds_explored: None,
          smt_formula: None,
          tier_used: None,
          tier_explanation: Some("Result retrieved from cache"),
        )
      let new_state =
        ValidatorState(
          ..state,
          cache: new_cache,
          completed_count: state.completed_count + 1,
        )
      #(new_state, response)
    }

    _ -> {
      // Run tiered validation (heuristics first, then Z3 if needed)
      let #(result, duration, worlds, smt, tier, explanation) =
        run_tiered_validation(state.config, request.formalization)

      // Cache the result
      let updated_cache =
        cache.memory_put(new_cache, cache_key, result, current_time)

      let response =
        ValidationResponse(
          request_id: request.id,
          result: result,
          from_cache: False,
          duration_ms: duration,
          worlds_explored: worlds,
          smt_formula: smt,
          tier_used: Some(tier),
          tier_explanation: explanation,
        )

      let new_state =
        ValidatorState(
          ..state,
          cache: updated_cache,
          completed_count: state.completed_count + 1,
        )

      #(new_state, response)
    }
  }
}

/// Validate a batch of formalizations
pub fn validate_batch(
  state: ValidatorState,
  batch: BatchRequest,
  current_time: Int,
) -> #(ValidatorState, BatchResponse) {
  let start_time = current_time

  // Process each request
  let #(final_state, responses, stats) =
    list.fold(batch.requests, #(state, [], empty_batch_stats()), fn(acc, req) {
      let #(current_state, responses_acc, stats_acc) = acc

      // Check if we should stop
      case batch.stop_on_invalid && stats_acc.invalid_count > 0 {
        True -> acc
        False -> {
          let #(new_state, response) =
            validate_request(current_state, req, current_time)

          let new_stats = update_batch_stats(stats_acc, response)
          #(new_state, [response, ..responses_acc], new_stats)
        }
      }
    })

  let batch_response =
    BatchResponse(
      batch_id: batch.id,
      responses: list.reverse(responses),
      total_duration_ms: current_time - start_time,
      stats: stats,
    )

  #(final_state, batch_response)
}

// =============================================================================
// SMT-LIB Generation
// =============================================================================

/// Generate SMT-LIB formula for validation
pub fn generate_smt(formalization: Formalization, max_worlds: Int) -> String {
  let logic_decls = generate_logic_declarations(formalization.logic_system)
  let world_decls = generate_world_declarations(max_worlds)
  let prop_decls = generate_proposition_declarations(formalization)
  let axioms = generate_axioms(formalization.logic_system, max_worlds)
  let premises_smt = generate_premises_smt(formalization.premises, max_worlds)
  let conclusion_smt =
    generate_conclusion_smt(formalization.conclusion, max_worlds)

  "; SMT-LIB formula for modal logic validation\n"
  <> "; Logic system: "
  <> logic_system_to_string(formalization.logic_system)
  <> "\n\n"
  <> logic_decls
  <> "\n"
  <> world_decls
  <> "\n"
  <> prop_decls
  <> "\n"
  <> axioms
  <> "\n"
  <> "; Premises\n"
  <> premises_smt
  <> "\n"
  <> "; Negated conclusion (for refutation)\n"
  <> conclusion_smt
  <> "\n"
  <> "(check-sat)\n"
  <> "(get-model)\n"
}

fn generate_logic_declarations(_system: LogicSystem) -> String {
  "(set-logic QF_UF)\n" <> "(set-option :produce-models true)\n"
}

fn generate_world_declarations(max_worlds: Int) -> String {
  let world_names = generate_world_names(max_worlds)
  let decls =
    list.map(world_names, fn(w) { "(declare-const " <> w <> " Bool)\n" })
  "; World declarations\n" <> string.join(decls, "")
}

fn generate_world_names(count: Int) -> List(String) {
  list.range(0, count - 1)
  |> list.map(fn(i) { "w" <> int_to_string(i) })
}

fn generate_proposition_declarations(formalization: Formalization) -> String {
  let all_props = collect_atoms(formalization)
  let world_count = 3
  // Default for declarations

  let decls =
    list.flat_map(all_props, fn(prop) {
      list.map(generate_world_names(world_count), fn(world) {
        "(declare-const " <> prop <> "_" <> world <> " Bool)\n"
      })
    })

  "; Proposition declarations\n" <> string.join(decls, "")
}

fn collect_atoms(formalization: Formalization) -> List(String) {
  let premise_atoms = list.flat_map(formalization.premises, collect_prop_atoms)
  let conclusion_atoms = collect_prop_atoms(formalization.conclusion)
  list.unique(list.append(premise_atoms, conclusion_atoms))
}

fn collect_prop_atoms(prop: Proposition) -> List(String) {
  case prop {
    proposition.Atom(name) -> [name]
    proposition.Not(inner) -> collect_prop_atoms(inner)
    proposition.And(left, right) ->
      list.append(collect_prop_atoms(left), collect_prop_atoms(right))
    proposition.Or(left, right) ->
      list.append(collect_prop_atoms(left), collect_prop_atoms(right))
    proposition.Implies(ante, cons) ->
      list.append(collect_prop_atoms(ante), collect_prop_atoms(cons))
    proposition.Necessary(inner) -> collect_prop_atoms(inner)
    proposition.Possible(inner) -> collect_prop_atoms(inner)
    proposition.Obligatory(inner) -> collect_prop_atoms(inner)
    proposition.Permitted(inner) -> collect_prop_atoms(inner)
    proposition.Knows(_, inner) -> collect_prop_atoms(inner)
    proposition.Believes(_, inner) -> collect_prop_atoms(inner)
  }
}

fn generate_axioms(system: LogicSystem, _max_worlds: Int) -> String {
  let base_axioms = "; Frame axioms\n"

  let reflexivity = case system {
    proposition.T | proposition.S4 | proposition.S5 ->
      "; Reflexivity: R(w,w) for all w\n"
      <> "(assert (forall ((w Int)) (R w w)))\n"
    _ -> ""
  }

  let transitivity = case system {
    proposition.K4 | proposition.S4 | proposition.S5 | proposition.KD45 ->
      "; Transitivity: R(w1,w2) & R(w2,w3) => R(w1,w3)\n"
    _ -> ""
  }

  let seriality = case system {
    proposition.KD | proposition.KD45 ->
      "; Seriality: forall w. exists v. R(w,v)\n"
    _ -> ""
  }

  base_axioms <> reflexivity <> transitivity <> seriality
}

fn generate_premises_smt(
  premises: List(Proposition),
  _max_worlds: Int,
) -> String {
  premises
  |> list.index_map(fn(p, i) {
    "; Premise "
    <> int_to_string(i + 1)
    <> "\n"
    <> "(assert "
    <> prop_to_smt(p, "w0")
    <> ")\n"
  })
  |> string.join("")
}

fn generate_conclusion_smt(conclusion: Proposition, _max_worlds: Int) -> String {
  "; Negated conclusion for countermodel search\n"
  <> "(assert (not "
  <> prop_to_smt(conclusion, "w0")
  <> "))\n"
}

fn prop_to_smt(prop: Proposition, world: String) -> String {
  case prop {
    proposition.Atom(name) -> name <> "_" <> world

    proposition.Not(inner) -> "(not " <> prop_to_smt(inner, world) <> ")"

    proposition.And(left, right) ->
      "(and "
      <> prop_to_smt(left, world)
      <> " "
      <> prop_to_smt(right, world)
      <> ")"

    proposition.Or(left, right) ->
      "(or "
      <> prop_to_smt(left, world)
      <> " "
      <> prop_to_smt(right, world)
      <> ")"

    proposition.Implies(ante, cons) ->
      "(=> "
      <> prop_to_smt(ante, world)
      <> " "
      <> prop_to_smt(cons, world)
      <> ")"

    proposition.Necessary(inner) ->
      "; Box: forall accessible worlds\n"
      <> "(forall ((v Int)) (=> (R "
      <> world
      <> " v) "
      <> prop_to_smt(inner, "v")
      <> "))"

    proposition.Possible(inner) ->
      "; Diamond: exists accessible world\n"
      <> "(exists ((v Int)) (and (R "
      <> world
      <> " v) "
      <> prop_to_smt(inner, "v")
      <> "))"

    proposition.Obligatory(inner) ->
      prop_to_smt(proposition.Necessary(inner), world)

    proposition.Permitted(inner) ->
      prop_to_smt(proposition.Possible(inner), world)

    proposition.Knows(agent, inner) ->
      "(K_" <> agent <> " " <> prop_to_smt(inner, world) <> ")"

    proposition.Believes(agent, inner) ->
      "(B_" <> agent <> " " <> prop_to_smt(inner, world) <> ")"
  }
}

// =============================================================================
// Tiered Validation Pipeline
// =============================================================================

/// Run tiered validation: heuristics first, then Z3 if needed
///
/// This function implements a tiered validation pipeline:
/// - **Tier 1**: Syntactic checks (tautology/contradiction detection, identity rules)
/// - **Tier 2**: Truth table enumeration (for small propositional formulas)
/// - **Tier 3**: Full Z3 SMT solving (fallback for complex cases)
///
/// The tiered approach provides fast results for simple cases while
/// maintaining full accuracy for complex modal logic validation.
fn run_tiered_validation(
  config: ValidatorConfig,
  formalization: Formalization,
) -> #(
  ValidationResult,
  Int,
  Option(Int),
  Option(String),
  ValidationTier,
  Option(String),
) {
  // Try heuristic validation first (Tier 1 and Tier 2)
  case heuristics.try_heuristic_validation(formalization) {
    Some(heuristic_result) -> {
      // Heuristics succeeded - return fast result
      let duration = heuristics.tier_expected_latency_ms(heuristic_result.tier)
      #(
        heuristic_result.result,
        duration,
        None,
        None,
        heuristic_result.tier,
        Some(heuristic_result.explanation),
      )
    }
    None -> {
      // Heuristics inconclusive - fall back to Z3 (Tier 3)
      let #(result, duration, worlds, smt) =
        run_validation(config, formalization)
      #(result, duration, worlds, smt, Tier3Z3, Some("Full Z3 SMT solving"))
    }
  }
}

// =============================================================================
// Z3 Solver Integration
// =============================================================================

/// Run validation using the Z3 solver (Tier 3)
///
/// This function:
/// 1. Converts the formalization's propositions to Z3's local Proposition type
/// 2. Compiles the validity check (premises + negated conclusion)
/// 3. Generates frame constraints for the logic system
/// 4. Runs the Z3 solver to find a countermodel
/// 5. If SAT, the argument is invalid (countermodel exists)
/// 6. If UNSAT, the argument is valid (no countermodel)
fn run_validation(
  config: ValidatorConfig,
  formalization: Formalization,
) -> #(ValidationResult, Int, Option(Int), Option(String)) {
  // Generate SMT formula for debugging/logging
  let smt = generate_smt(formalization, config.max_worlds)

  // Convert premises and conclusion to z3_compile's Proposition type
  let z3_premises = list.map(formalization.premises, convert_proposition_to_z3)
  let z3_conclusion = convert_proposition_to_z3(formalization.conclusion)
  let z3_system = convert_logic_system_to_z3(formalization.logic_system)

  // Create compile configuration
  let compile_config = z3_compile.config_with_worlds(config.max_worlds)

  // Compile validity check constraints
  let constraints =
    z3_compile.compile_validity_check(
      z3_premises,
      z3_conclusion,
      z3_system,
      compile_config,
    )

  // Run Z3 solver
  let validation_result =
    run_z3_validation(constraints, config, formalization.logic_system)

  // Return with timing and metadata
  let duration = 100
  // Approximate duration
  let worlds = Some(config.max_worlds)

  #(validation_result, duration, worlds, Some(smt))
}

/// Run the Z3 solver with the given constraints
fn run_z3_validation(
  constraints: List(Z3Expr),
  config: ValidatorConfig,
  logic_system: LogicSystem,
) -> ValidationResult {
  // Create a new solver
  case solver.new() {
    Error(_) -> Invalid("Z3 solver error: Could not create solver")
    Ok(s) -> {
      // Add all constraints
      case solver.assert_all(s, constraints) {
        Error(_) -> Invalid("Z3 solver error: Could not add constraints")
        Ok(s_with_constraints) -> {
          // Check satisfiability using real Z3
          case solver.check_with_z3(s_with_constraints) {
            Error(err) -> {
              // Handle Z3 unavailability based on configuration
              case err {
                types.PortError(port_err) -> {
                  // Z3 not available - use configured degradation mode
                  handle_z3_unavailable(config, port_err)
                }
                types.TimeoutError -> {
                  Invalid(
                    "Z3 solver timeout after "
                    <> int.to_string(config.solver_timeout_ms)
                    <> "ms",
                  )
                }
                _ -> Invalid("Z3 solver error: " <> format_z3_error(err))
              }
            }
            Ok(#(_solver, check_result)) -> {
              interpret_check_result(check_result, logic_system, config)
            }
          }
        }
      }
    }
  }
}

/// Interpret the Z3 check result
fn interpret_check_result(
  result: SolverCheckResult,
  logic_system: LogicSystem,
  config: ValidatorConfig,
) -> ValidationResult {
  case result {
    SolverSat(model) -> {
      // SAT means a countermodel exists - the argument is INVALID
      case config.detailed_countermodels {
        True -> {
          // Extract structured countermodel from Z3 model
          let countermodel = extract_countermodel(model, logic_system)
          // Generate human-readable explanation
          let explanation = generate_countermodel_explanation(countermodel)
          Invalid(explanation)
        }
        False -> Invalid("Countermodel exists")
      }
    }
    SolverUnsat -> {
      // UNSAT means no countermodel exists - the argument is VALID
      Valid
    }
    SolverUnknown(reason) -> {
      // Unknown result - report as inconclusive
      Invalid("Validation inconclusive: " <> reason)
    }
  }
}

/// Extract a structured Countermodel from a Z3 model
///
/// This function parses the Z3 model values and constructs a proper
/// Countermodel type with worlds, accessibility relations, and truth values.
///
/// ## Z3 Variable Naming Convention
/// - Proposition variables: `{prop}_w{index}` (e.g., "p_w0", "q_w1")
/// - Accessibility relations: `R_w{from}_w{to}` (e.g., "R_w0_w1")
///
/// ## Example
/// ```gleam
/// let countermodel = extract_countermodel(model, S4)
/// // Returns Countermodel with worlds, relations, actual_world="w0"
/// ```
pub fn extract_countermodel(
  model: solver.SolverModel,
  logic_system: LogicSystem,
) -> Countermodel {
  let model_values = dict.to_list(model.values)

  // Extract world-indexed proposition values
  let prop_values =
    list.filter_map(model_values, fn(entry) {
      let #(name, value) = entry
      // Parse variable names like "p_w0", "q_w1"
      case parse_variable_name(name) {
        Some(#(prop_name, world_idx)) -> {
          let bool_value = case value {
            BoolVal(b) -> b
            _ -> False
          }
          Ok(#(prop_name, world_idx, bool_value))
        }
        None -> Error(Nil)
      }
    })

  // Group by world and create KripkeWorld records
  let worlds_data = collect_world_info(prop_values)
  let kripke_worlds =
    list.map(worlds_data, fn(world_tuple) {
      let #(world_idx, true_props, false_props) = world_tuple
      KripkeWorld(
        name: "w" <> int.to_string(world_idx),
        true_props: true_props,
        false_props: false_props,
      )
    })

  // Ensure we have at least the actual world (w0)
  let final_worlds = case kripke_worlds {
    [] -> [KripkeWorld(name: "w0", true_props: [], false_props: [])]
    _ -> kripke_worlds
  }

  // Extract accessibility relations
  let relations =
    list.filter_map(model_values, fn(entry) {
      let #(name, value) = entry
      case parse_accessibility_relation(name) {
        Some(#(from, to)) -> {
          case value {
            BoolVal(True) ->
              Ok(AccessibilityRelation(
                from: "w" <> int.to_string(from),
                to: "w" <> int.to_string(to),
              ))
            _ -> Error(Nil)
          }
        }
        None -> Error(Nil)
      }
    })

  Countermodel(
    worlds: final_worlds,
    relations: relations,
    actual_world: "w0",
    logic_system: logic_system,
  )
}

/// Generate a human-readable explanation of why the countermodel
/// demonstrates the argument is invalid
///
/// This function analyzes the countermodel structure and produces
/// a detailed explanation including:
/// - World configuration and truth values
/// - Accessibility relations
/// - Why premises are true but conclusion is false
fn generate_countermodel_explanation(countermodel: Countermodel) -> String {
  let world_count = list.length(countermodel.worlds)
  let relation_count = list.length(countermodel.relations)

  // Header with countermodel summary
  let header =
    "Countermodel found with "
    <> int.to_string(world_count)
    <> " world(s) and "
    <> int.to_string(relation_count)
    <> " relation(s).\n\n"

  // World details
  let worlds_section = format_worlds_explanation(countermodel.worlds)

  // Relations section
  let relations_section = format_relations_explanation(countermodel.relations)

  // Frame properties for the logic system
  let frame_section = format_frame_properties(countermodel.logic_system)

  // Explanation of why argument is invalid
  let invalidity_explanation = generate_invalidity_explanation(countermodel)

  header
  <> worlds_section
  <> relations_section
  <> frame_section
  <> invalidity_explanation
}

/// Format the worlds section of the explanation
fn format_worlds_explanation(worlds: List(KripkeWorld)) -> String {
  let header = "Worlds:\n"
  let world_lines =
    worlds
    |> list.map(fn(world) {
      let true_str = case world.true_props {
        [] -> "∅"
        props -> "{" <> string.join(props, ", ") <> "}"
      }
      let false_str = case world.false_props {
        [] -> "∅"
        props -> "{" <> string.join(props, ", ") <> "}"
      }
      "  " <> world.name <> ": true=" <> true_str <> ", false=" <> false_str
    })
    |> string.join("\n")

  header <> world_lines <> "\n\n"
}

/// Format the relations section of the explanation
fn format_relations_explanation(
  relations: List(AccessibilityRelation),
) -> String {
  case relations {
    [] -> "Accessibility: (no relations - worlds are isolated)\n\n"
    rels -> {
      let rel_strs =
        list.map(rels, fn(rel) { "R(" <> rel.from <> "," <> rel.to <> ")" })
      "Accessibility: " <> string.join(rel_strs, ", ") <> "\n\n"
    }
  }
}

/// Format frame properties for the logic system
fn format_frame_properties(logic_system: LogicSystem) -> String {
  let props = case logic_system {
    proposition.K -> "none (basic modal logic)"
    proposition.T -> "reflexive"
    proposition.K4 -> "transitive"
    proposition.S4 -> "reflexive, transitive"
    proposition.S5 -> "reflexive, transitive, symmetric (equivalence)"
    proposition.KD -> "serial"
    proposition.KD45 -> "serial, transitive, euclidean"
  }
  "Frame properties ["
  <> logic_system_to_string(logic_system)
  <> "]: "
  <> props
  <> "\n\n"
}

/// Generate explanation of why the countermodel invalidates the argument
fn generate_invalidity_explanation(countermodel: Countermodel) -> String {
  let actual_world_info =
    list.find(countermodel.worlds, fn(w) { w.name == countermodel.actual_world })

  case actual_world_info {
    Ok(actual) -> {
      let true_summary = case actual.true_props {
        [] -> "no propositions are true"
        props -> string.join(props, ", ") <> " are true"
      }
      let false_summary = case actual.false_props {
        [] -> "no propositions are explicitly false"
        props -> string.join(props, ", ") <> " are false"
      }

      "Explanation:\n"
      <> "  In the actual world ("
      <> countermodel.actual_world
      <> "), "
      <> true_summary
      <> " and "
      <> false_summary
      <> ".\n"
      <> "  This configuration satisfies all premises while making the conclusion false,\n"
      <> "  demonstrating that the argument is INVALID in "
      <> logic_system_to_string(countermodel.logic_system)
      <> " logic."
    }
    Error(_) ->
      "Explanation: The countermodel demonstrates invalidity in "
      <> logic_system_to_string(countermodel.logic_system)
      <> " logic."
  }
}

/// Extract a human-readable countermodel description from a Z3 model
/// (Deprecated: Use extract_countermodel for structured data)
fn extract_countermodel_description(
  model: solver.SolverModel,
  logic_system: LogicSystem,
) -> String {
  let countermodel = extract_countermodel(model, logic_system)
  generate_countermodel_explanation(countermodel)
}

/// Parse a world-indexed variable name like "p_w0" -> Some(#("p", 0))
fn parse_variable_name(name: String) -> Option(#(String, Int)) {
  case string.split(name, "_w") {
    [prop_name, world_str] -> {
      case int.parse(world_str) {
        Ok(world_idx) -> Some(#(prop_name, world_idx))
        Error(_) -> None
      }
    }
    _ -> None
  }
}

/// Parse an accessibility relation like "R_w0_w1" -> Some(#(0, 1))
fn parse_accessibility_relation(name: String) -> Option(#(Int, Int)) {
  case string.starts_with(name, "R_w") {
    True -> {
      let rest = string.drop_start(name, 3)
      case string.split(rest, "_w") {
        [from_str, to_str] -> {
          case int.parse(from_str), int.parse(to_str) {
            Ok(from), Ok(to) -> Some(#(from, to))
            _, _ -> None
          }
        }
        _ -> None
      }
    }
    False -> None
  }
}

/// Collect world information from proposition values
fn collect_world_info(
  prop_values: List(#(String, Int, Bool)),
) -> List(#(Int, List(String), List(String))) {
  // Group by world index
  let world_indices =
    prop_values
    |> list.map(fn(entry) { entry.1 })
    |> list.unique

  list.map(world_indices, fn(world_idx) {
    let world_props =
      list.filter(prop_values, fn(entry) { entry.1 == world_idx })

    let true_props =
      world_props
      |> list.filter(fn(entry) { entry.2 })
      |> list.map(fn(entry) { entry.0 })
      |> list.filter(fn(name) { !string.starts_with(name, "R") })

    let false_props =
      world_props
      |> list.filter(fn(entry) { !entry.2 })
      |> list.map(fn(entry) { entry.0 })
      |> list.filter(fn(name) { !string.starts_with(name, "R") })

    #(world_idx, true_props, false_props)
  })
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
}

/// Format the countermodel output
fn format_countermodel_output(
  worlds: List(#(Int, List(String), List(String))),
  relations: List(#(Int, Int)),
  logic_system: LogicSystem,
) -> String {
  let worlds_str =
    worlds
    |> list.map(fn(world) {
      let #(idx, true_props, false_props) = world
      let world_name = "w" <> int.to_string(idx)
      let true_str = case true_props {
        [] -> ""
        props -> " [true: " <> string.join(props, ", ") <> "]"
      }
      let false_str = case false_props {
        [] -> ""
        props -> " [false: " <> string.join(props, ", ") <> "]"
      }
      world_name <> true_str <> false_str
    })
    |> string.join("; ")

  let relations_str = case relations {
    [] -> ""
    rels -> {
      let rel_strs =
        list.map(rels, fn(rel) {
          let #(from, to) = rel
          "R(w" <> int.to_string(from) <> ",w" <> int.to_string(to) <> ")"
        })
      " | Relations: " <> string.join(rel_strs, ", ")
    }
  }

  let system_str = " [" <> logic_system_to_string(logic_system) <> "]"

  "Countermodel: " <> worlds_str <> relations_str <> system_str
}

/// Handle Z3 unavailability based on configuration
fn handle_z3_unavailable(
  config: ValidatorConfig,
  error_msg: String,
) -> ValidationResult {
  case config.z3_degradation_mode {
    FailFast -> {
      Invalid(
        "Z3 solver required but unavailable: "
        <> error_msg
        <> "\n\n"
        <> z3_installation_instructions(),
      )
    }
    ReturnUnknown -> {
      argument.Unknown(
        "Validation inconclusive (Z3 unavailable). "
        <> "Install Z3 for full validation: pip3 install z3-solver",
      )
    }
    UseFallback -> {
      // Use simple heuristics - this is a placeholder for potential future
      // implementations that could use tableaux or other methods
      argument.Unknown(
        "Z3 unavailable, using fallback. Results may be incomplete.",
      )
    }
  }
}

/// Get Z3 installation instructions
fn z3_installation_instructions() -> String {
  "Z3 solver is not available. For full validation support, install Z3:

macOS:   brew install z3 && pip3 install z3-solver
Ubuntu:  apt install z3 && pip3 install z3-solver
Windows: pip install z3-solver

After installation, verify with:
  python3 -c \"from z3 import *; print('Z3 OK')\""
}

/// Fallback mock validation when Z3 is not available
/// (Deprecated: Use handle_z3_unavailable with proper config)
fn mock_validate_fallback(constraints: List(Z3Expr)) -> ValidationResult {
  // Simple heuristic: if we have constraints, assume unknown
  case list.length(constraints) {
    0 -> Valid
    _ ->
      argument.Unknown(
        "Z3 unavailable - unable to verify. Constraints count: "
        <> int.to_string(list.length(constraints)),
      )
  }
}

/// Format a Z3 error for display
fn format_z3_error(err: Z3Error) -> String {
  case err {
    types.SolverError(msg) -> "Solver error: " <> msg
    types.TimeoutError -> "Timeout"
    types.PortError(msg) -> "Port error: " <> msg
    types.ParseError(msg) -> "Parse error: " <> msg
  }
}

// =============================================================================
// Proposition Conversion
// =============================================================================

/// Convert a modal_logic Proposition to z3_compile Proposition
fn convert_proposition_to_z3(prop: Proposition) -> z3_compile.Proposition {
  case prop {
    proposition.Atom(name) -> z3_compile.Atom(name)
    proposition.Not(inner) ->
      z3_compile.PropNot(convert_proposition_to_z3(inner))
    proposition.And(left, right) ->
      z3_compile.PropAnd(
        convert_proposition_to_z3(left),
        convert_proposition_to_z3(right),
      )
    proposition.Or(left, right) ->
      z3_compile.PropOr(
        convert_proposition_to_z3(left),
        convert_proposition_to_z3(right),
      )
    proposition.Implies(ante, cons) ->
      z3_compile.PropImplies(
        convert_proposition_to_z3(ante),
        convert_proposition_to_z3(cons),
      )
    proposition.Necessary(inner) ->
      z3_compile.Necessary(convert_proposition_to_z3(inner))
    proposition.Possible(inner) ->
      z3_compile.Possible(convert_proposition_to_z3(inner))
    proposition.Obligatory(inner) ->
      z3_compile.Obligatory(convert_proposition_to_z3(inner))
    proposition.Permitted(inner) ->
      z3_compile.Permitted(convert_proposition_to_z3(inner))
    proposition.Knows(agent, inner) ->
      z3_compile.Knows(agent, convert_proposition_to_z3(inner))
    proposition.Believes(agent, inner) ->
      z3_compile.Believes(agent, convert_proposition_to_z3(inner))
  }
}

/// Convert a modal_logic LogicSystem to z3_compile LogicSystem
fn convert_logic_system_to_z3(system: LogicSystem) -> z3_compile.LogicSystem {
  case system {
    proposition.K -> z3_compile.K
    proposition.T -> z3_compile.T
    proposition.K4 -> z3_compile.K4
    proposition.S4 -> z3_compile.S4
    proposition.S5 -> z3_compile.S5
    proposition.KD -> z3_compile.KD
    proposition.KD45 -> z3_compile.KD45
  }
}

// =============================================================================
// Countermodel Parsing
// =============================================================================

/// Parse a countermodel from solver output
pub fn parse_countermodel(
  output: String,
  logic_system: LogicSystem,
) -> Option(Countermodel) {
  // Simple parser for mock countermodels
  case string.contains(output, "Countermodel:") {
    False -> None
    True -> {
      Some(Countermodel(
        worlds: [
          KripkeWorld(name: "w0", true_props: [], false_props: ["conclusion"]),
        ],
        relations: [],
        actual_world: "w0",
        logic_system: logic_system,
      ))
    }
  }
}

/// Check if a countermodel is valid for the given logic system
pub fn validate_countermodel(
  countermodel: Countermodel,
  logic_system: LogicSystem,
) -> Bool {
  // Check frame conditions
  let reflexive_ok = case logic_system {
    proposition.T | proposition.S4 | proposition.S5 ->
      check_reflexivity(countermodel)
    _ -> True
  }

  let transitive_ok = case logic_system {
    proposition.K4 | proposition.S4 | proposition.S5 | proposition.KD45 ->
      check_transitivity(countermodel)
    _ -> True
  }

  reflexive_ok && transitive_ok
}

fn check_reflexivity(countermodel: Countermodel) -> Bool {
  list.all(countermodel.worlds, fn(w) {
    list.any(countermodel.relations, fn(r) {
      r.from == w.name && r.to == w.name
    })
  })
}

fn check_transitivity(countermodel: Countermodel) -> Bool {
  // For each pair of relations R(a,b) and R(b,c), check R(a,c) exists
  list.all(countermodel.relations, fn(r1) {
    list.all(countermodel.relations, fn(r2) {
      case r1.to == r2.from {
        True ->
          list.any(countermodel.relations, fn(r3) {
            r3.from == r1.from && r3.to == r2.to
          })
        False -> True
      }
    })
  })
}

// =============================================================================
// Helper Functions
// =============================================================================

fn empty_batch_stats() -> BatchStats {
  BatchStats(valid_count: 0, invalid_count: 0, error_count: 0, cache_hits: 0)
}

fn update_batch_stats(
  stats: BatchStats,
  response: ValidationResponse,
) -> BatchStats {
  let cache_increment = case response.from_cache {
    True -> 1
    False -> 0
  }

  case response.result {
    Valid ->
      BatchStats(
        ..stats,
        valid_count: stats.valid_count + 1,
        cache_hits: stats.cache_hits + cache_increment,
      )
    Invalid(_) ->
      BatchStats(
        ..stats,
        invalid_count: stats.invalid_count + 1,
        cache_hits: stats.cache_hits + cache_increment,
      )
    _ ->
      BatchStats(
        ..stats,
        error_count: stats.error_count + 1,
        cache_hits: stats.cache_hits + cache_increment,
      )
  }
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
