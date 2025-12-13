//// Solver Configuration Module
////
//// This module provides comprehensive configuration options for the Z3 solver.
//// It exposes Z3's configuration parameters in a type-safe manner.
////
//// ## Configuration Options
////
//// - **Timeout**: Maximum solving time
//// - **Memory Limit**: Maximum memory usage
//// - **Random Seed**: For reproducible solving
//// - **Tactic Selection**: Solver strategy
//// - **Model Completion**: Behavior for undefined values
////
//// ## Usage
////
//// ```gleam
//// import z3/config.{Z3Config}
////
//// let cfg = config.new()
////   |> config.set_timeout(5000)
////   |> config.set_memory_limit(1024)
////   |> config.enable_model_completion()
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import z3/timeout.{type TimeoutConfig}

// =============================================================================
// Types
// =============================================================================

/// Comprehensive Z3 solver configuration
pub type Z3Config {
  Z3Config(
    /// Timeout configuration
    timeout: TimeoutConfig,
    /// Memory limit in megabytes (0 = unlimited)
    memory_limit_mb: Int,
    /// Random seed for reproducibility (None = use default)
    random_seed: Option(Int),
    /// Solver tactic/strategy
    tactic: SolverTactic,
    /// Model generation options
    model_options: ModelOptions,
    /// Proof generation options
    proof_options: ProofOptions,
    /// Core options (unsat core, etc.)
    core_options: CoreOptions,
    /// Additional custom parameters
    custom_params: Dict(String, ParamValue),
  )
}

/// Solver tactic/strategy selection
pub type SolverTactic {
  /// Default Z3 solver (auto-selects strategy)
  TacticDefault
  /// SAT solver optimized for propositional logic
  TacticSAT
  /// SMT solver for theory reasoning
  TacticSMT
  /// QF_LIA (quantifier-free linear integer arithmetic)
  TacticQFLIA
  /// QF_LRA (quantifier-free linear real arithmetic)
  TacticQFLRA
  /// QF_BV (quantifier-free bitvectors)
  TacticQFBV
  /// Custom tactic string
  TacticCustom(String)
}

/// Model generation configuration
pub type ModelOptions {
  ModelOptions(
    /// Generate models for satisfiable formulas
    generate_models: Bool,
    /// Complete model with default values for undefined constants
    model_completion: Bool,
    /// Validate models after generation
    model_validation: Bool,
  )
}

/// Proof generation configuration
pub type ProofOptions {
  ProofOptions(
    /// Generate proofs for unsatisfiable formulas
    generate_proofs: Bool,
    /// Proof format (when proofs are generated)
    proof_format: ProofFormat,
  )
}

/// Proof output format
pub type ProofFormat {
  ProofFormatDefault
  ProofFormatDot
  ProofFormatSMTLib2
}

/// Unsat core and related options
pub type CoreOptions {
  CoreOptions(
    /// Enable unsat core extraction
    unsat_core: Bool,
    /// Minimize the unsat core
    minimize_core: Bool,
    /// Maximum size of unsat core (0 = unlimited)
    max_core_size: Int,
  )
}

/// Parameter value type
pub type ParamValue {
  ParamBool(Bool)
  ParamInt(Int)
  ParamString(String)
  ParamFloat(Float)
}

// =============================================================================
// Configuration Constructors
// =============================================================================

/// Create a new default configuration
pub fn new() -> Z3Config {
  Z3Config(
    timeout: timeout.default_config(),
    memory_limit_mb: 0,
    random_seed: None,
    tactic: TacticDefault,
    model_options: default_model_options(),
    proof_options: default_proof_options(),
    core_options: default_core_options(),
    custom_params: dict.new(),
  )
}

/// Default model options
pub fn default_model_options() -> ModelOptions {
  ModelOptions(
    generate_models: True,
    model_completion: False,
    model_validation: False,
  )
}

/// Default proof options
pub fn default_proof_options() -> ProofOptions {
  ProofOptions(generate_proofs: False, proof_format: ProofFormatDefault)
}

/// Default core options
pub fn default_core_options() -> CoreOptions {
  CoreOptions(unsat_core: False, minimize_core: False, max_core_size: 0)
}

// =============================================================================
// Timeout Configuration
// =============================================================================

/// Set the timeout in milliseconds
pub fn set_timeout(config: Z3Config, timeout_ms: Int) -> Z3Config {
  Z3Config(..config, timeout: timeout.with_z3_timeout(timeout_ms))
}

/// Set the timeout configuration directly
pub fn set_timeout_config(
  config: Z3Config,
  timeout_config: TimeoutConfig,
) -> Z3Config {
  Z3Config(..config, timeout: timeout_config)
}

/// Get the current timeout configuration
pub fn get_timeout(config: Z3Config) -> TimeoutConfig {
  config.timeout
}

// =============================================================================
// Memory Configuration
// =============================================================================

/// Set the memory limit in megabytes
pub fn set_memory_limit(config: Z3Config, limit_mb: Int) -> Z3Config {
  Z3Config(..config, memory_limit_mb: max(0, limit_mb))
}

/// Remove the memory limit
pub fn remove_memory_limit(config: Z3Config) -> Z3Config {
  Z3Config(..config, memory_limit_mb: 0)
}

/// Get the memory limit
pub fn get_memory_limit(config: Z3Config) -> Int {
  config.memory_limit_mb
}

// =============================================================================
// Random Seed Configuration
// =============================================================================

/// Set the random seed for reproducibility
pub fn set_random_seed(config: Z3Config, seed: Int) -> Z3Config {
  Z3Config(..config, random_seed: Some(seed))
}

/// Clear the random seed (use default behavior)
pub fn clear_random_seed(config: Z3Config) -> Z3Config {
  Z3Config(..config, random_seed: None)
}

/// Get the random seed if set
pub fn get_random_seed(config: Z3Config) -> Option(Int) {
  config.random_seed
}

// =============================================================================
// Tactic Configuration
// =============================================================================

/// Set the solver tactic
pub fn set_tactic(config: Z3Config, tactic: SolverTactic) -> Z3Config {
  Z3Config(..config, tactic: tactic)
}

/// Get the current tactic
pub fn get_tactic(config: Z3Config) -> SolverTactic {
  config.tactic
}

/// Use the default tactic
pub fn use_default_tactic(config: Z3Config) -> Z3Config {
  set_tactic(config, TacticDefault)
}

/// Use the SAT tactic for propositional logic
pub fn use_sat_tactic(config: Z3Config) -> Z3Config {
  set_tactic(config, TacticSAT)
}

/// Use a custom tactic string
pub fn use_custom_tactic(config: Z3Config, tactic_str: String) -> Z3Config {
  set_tactic(config, TacticCustom(tactic_str))
}

// =============================================================================
// Model Options
// =============================================================================

/// Enable model generation
pub fn enable_models(config: Z3Config) -> Z3Config {
  let opts = config.model_options
  Z3Config(..config, model_options: ModelOptions(..opts, generate_models: True))
}

/// Disable model generation
pub fn disable_models(config: Z3Config) -> Z3Config {
  let opts = config.model_options
  Z3Config(
    ..config,
    model_options: ModelOptions(..opts, generate_models: False),
  )
}

/// Enable model completion (fill in undefined values)
pub fn enable_model_completion(config: Z3Config) -> Z3Config {
  let opts = config.model_options
  Z3Config(
    ..config,
    model_options: ModelOptions(..opts, model_completion: True),
  )
}

/// Disable model completion
pub fn disable_model_completion(config: Z3Config) -> Z3Config {
  let opts = config.model_options
  Z3Config(
    ..config,
    model_options: ModelOptions(..opts, model_completion: False),
  )
}

/// Enable model validation
pub fn enable_model_validation(config: Z3Config) -> Z3Config {
  let opts = config.model_options
  Z3Config(
    ..config,
    model_options: ModelOptions(..opts, model_validation: True),
  )
}

// =============================================================================
// Proof Options
// =============================================================================

/// Enable proof generation
pub fn enable_proofs(config: Z3Config) -> Z3Config {
  let opts = config.proof_options
  Z3Config(..config, proof_options: ProofOptions(..opts, generate_proofs: True))
}

/// Disable proof generation
pub fn disable_proofs(config: Z3Config) -> Z3Config {
  let opts = config.proof_options
  Z3Config(
    ..config,
    proof_options: ProofOptions(..opts, generate_proofs: False),
  )
}

/// Set proof format
pub fn set_proof_format(config: Z3Config, format: ProofFormat) -> Z3Config {
  let opts = config.proof_options
  Z3Config(..config, proof_options: ProofOptions(..opts, proof_format: format))
}

// =============================================================================
// Core Options
// =============================================================================

/// Enable unsat core extraction
pub fn enable_unsat_core(config: Z3Config) -> Z3Config {
  let opts = config.core_options
  Z3Config(..config, core_options: CoreOptions(..opts, unsat_core: True))
}

/// Disable unsat core extraction
pub fn disable_unsat_core(config: Z3Config) -> Z3Config {
  let opts = config.core_options
  Z3Config(..config, core_options: CoreOptions(..opts, unsat_core: False))
}

/// Enable core minimization
pub fn enable_core_minimization(config: Z3Config) -> Z3Config {
  let opts = config.core_options
  Z3Config(..config, core_options: CoreOptions(..opts, minimize_core: True))
}

/// Set maximum core size
pub fn set_max_core_size(config: Z3Config, max_size: Int) -> Z3Config {
  let opts = config.core_options
  Z3Config(
    ..config,
    core_options: CoreOptions(..opts, max_core_size: max(0, max_size)),
  )
}

// =============================================================================
// Custom Parameters
// =============================================================================

/// Set a custom boolean parameter
pub fn set_bool_param(config: Z3Config, name: String, value: Bool) -> Z3Config {
  Z3Config(
    ..config,
    custom_params: dict.insert(config.custom_params, name, ParamBool(value)),
  )
}

/// Set a custom integer parameter
pub fn set_int_param(config: Z3Config, name: String, value: Int) -> Z3Config {
  Z3Config(
    ..config,
    custom_params: dict.insert(config.custom_params, name, ParamInt(value)),
  )
}

/// Set a custom string parameter
pub fn set_string_param(
  config: Z3Config,
  name: String,
  value: String,
) -> Z3Config {
  Z3Config(
    ..config,
    custom_params: dict.insert(config.custom_params, name, ParamString(value)),
  )
}

/// Remove a custom parameter
pub fn remove_param(config: Z3Config, name: String) -> Z3Config {
  Z3Config(..config, custom_params: dict.delete(config.custom_params, name))
}

/// Get a custom parameter value
pub fn get_param(config: Z3Config, name: String) -> Option(ParamValue) {
  case dict.get(config.custom_params, name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

// =============================================================================
// Z3 Parameter Export
// =============================================================================

/// Convert configuration to Z3 solver parameters
pub fn to_z3_params(config: Z3Config) -> List(#(String, String)) {
  let timeout_params = timeout.to_z3_params(config.timeout)

  let memory_params = case config.memory_limit_mb {
    0 -> []
    mb -> [#("memory_max_size", int_to_string(mb * 1024 * 1024))]
  }

  let seed_params = case config.random_seed {
    None -> []
    Some(seed) -> [#("random_seed", int_to_string(seed))]
  }

  let model_params = [
    #("model", bool_to_string(config.model_options.generate_models)),
    #("model.completion", bool_to_string(config.model_options.model_completion)),
    #("model.validate", bool_to_string(config.model_options.model_validation)),
  ]

  let proof_params = [
    #("proof", bool_to_string(config.proof_options.generate_proofs)),
  ]

  let core_params = [
    #("unsat_core", bool_to_string(config.core_options.unsat_core)),
  ]

  let custom_params =
    dict.to_list(config.custom_params)
    |> list.map(fn(pair) {
      let #(name, value) = pair
      #(name, param_value_to_string(value))
    })

  list.flatten([
    timeout_params,
    memory_params,
    seed_params,
    model_params,
    proof_params,
    core_params,
    custom_params,
  ])
}

/// Get the tactic name for Z3
pub fn tactic_name(tactic: SolverTactic) -> String {
  case tactic {
    TacticDefault -> "default"
    TacticSAT -> "sat"
    TacticSMT -> "smt"
    TacticQFLIA -> "qflia"
    TacticQFLRA -> "qflra"
    TacticQFBV -> "qfbv"
    TacticCustom(name) -> name
  }
}

// =============================================================================
// Configuration Presets
// =============================================================================

/// Configuration preset for fast solving (short timeout, no extras)
pub fn preset_fast() -> Z3Config {
  new()
  |> set_timeout(1000)
  |> disable_proofs()
  |> disable_unsat_core()
}

/// Configuration preset for thorough solving
pub fn preset_thorough() -> Z3Config {
  new()
  |> set_timeout(30_000)
  |> enable_model_completion()
  |> enable_model_validation()
}

/// Configuration preset for debugging
pub fn preset_debug() -> Z3Config {
  new()
  |> set_timeout(60_000)
  |> enable_proofs()
  |> enable_unsat_core()
  |> enable_core_minimization()
  |> enable_model_validation()
}

/// Configuration preset for reproducible results
pub fn preset_reproducible(seed: Int) -> Z3Config {
  new()
  |> set_random_seed(seed)
}

/// Configuration preset for low memory environments
pub fn preset_low_memory() -> Z3Config {
  new()
  |> set_memory_limit(256)
  |> disable_proofs()
}

// =============================================================================
// Configuration Validation
// =============================================================================

/// Validate a configuration and return errors if any
pub fn validate(config: Z3Config) -> Result(Z3Config, List(String)) {
  let errors = []

  let errors = case config.memory_limit_mb < 0 {
    True -> ["Memory limit must be non-negative", ..errors]
    False -> errors
  }

  let errors = case timeout.validate_config(config.timeout) {
    Error(msg) -> [msg, ..errors]
    Ok(_) -> errors
  }

  case errors {
    [] -> Ok(config)
    errs -> Error(errs)
  }
}

/// Format configuration as a string for debugging
pub fn format(config: Z3Config) -> String {
  let timeout_str = case timeout.effective_timeout(config.timeout) {
    None -> "unlimited"
    Some(ms) -> int_to_string(ms) <> "ms"
  }

  let memory_str = case config.memory_limit_mb {
    0 -> "unlimited"
    mb -> int_to_string(mb) <> "MB"
  }

  "Z3Config{"
  <> "timeout="
  <> timeout_str
  <> ", memory="
  <> memory_str
  <> ", tactic="
  <> tactic_name(config.tactic)
  <> ", models="
  <> bool_to_string(config.model_options.generate_models)
  <> ", proofs="
  <> bool_to_string(config.proof_options.generate_proofs)
  <> ", unsat_core="
  <> bool_to_string(config.core_options.unsat_core)
  <> "}"
}

// =============================================================================
// Internal Helpers
// =============================================================================

fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
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

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn param_value_to_string(value: ParamValue) -> String {
  case value {
    ParamBool(b) -> bool_to_string(b)
    ParamInt(n) -> int_to_string(n)
    ParamString(s) -> s
    ParamFloat(f) -> float_to_string(f)
  }
}

fn float_to_string(f: Float) -> String {
  // Simple integer approximation for display
  let int_part = float_to_int(f)
  int_to_string(int_part)
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int
