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

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/argument.{
  type Formalization, type ValidationResult, Invalid, Valid,
}
import modal_logic/cache.{type CacheConfig, type MemoryCache}
import modal_logic/proposition.{type LogicSystem, type Proposition}

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
  )
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
pub fn default_config() -> ValidatorConfig {
  ValidatorConfig(
    cache_config: cache.default_config(),
    solver_timeout_ms: 30_000,
    max_worlds: 10,
    parallel: False,
    worker_count: 4,
    detailed_countermodels: True,
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
      // Run validation
      let #(result, duration, worlds, smt) =
        run_validation(state.config, request.formalization)

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
// Mock Validation (for testing without Z3)
// =============================================================================

/// Run validation (mock implementation for now)
fn run_validation(
  config: ValidatorConfig,
  formalization: Formalization,
) -> #(ValidationResult, Int, Option(Int), Option(String)) {
  // Generate SMT formula
  let smt = generate_smt(formalization, config.max_worlds)

  // Mock validation result based on formalization structure
  let result = mock_validate(formalization)

  // Mock timing
  let duration = 50
  let worlds = Some(3)

  #(result, duration, worlds, Some(smt))
}

/// Mock validation for testing
fn mock_validate(formalization: Formalization) -> ValidationResult {
  // Simple heuristic: check if conclusion appears in premises
  let premise_atoms = list.flat_map(formalization.premises, collect_prop_atoms)
  let conclusion_atoms = collect_prop_atoms(formalization.conclusion)

  let has_overlap =
    list.any(conclusion_atoms, fn(a) { list.contains(premise_atoms, a) })

  case has_overlap {
    True -> Valid
    False ->
      Invalid(
        "Countermodel: w0 where premises are true but conclusion is false",
      )
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
