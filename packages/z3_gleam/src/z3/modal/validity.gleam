//// Modal Validity Checker Module
////
//// This module provides a high-level interface for checking the validity
//// of modal formulas in various modal logic systems.
////
//// ## Validity Checking
////
//// A modal formula φ is valid in a modal system S if and only if
//// ¬φ is unsatisfiable in all S-frames. We check this by:
////
//// 1. Translating ¬φ to first-order logic
//// 2. Adding frame constraints for system S
//// 3. Checking satisfiability with Z3
////
//// If SAT: φ is invalid, and we extract a countermodel
//// If UNSAT: φ is valid
////
//// ## Usage
////
//// ```gleam
//// import z3/modal/validity
//// import z3/modal/kripke
//// import z3/modal/frame_conditions.{S4}
////
//// // Check if □p → p is valid in S4
//// let formula = kripke.modal_implies(kripke.box(kripke.atom("p")), kripke.atom("p"))
//// let result = validity.check_validity(formula, S4, 3)
////
//// case result {
////   validity.Valid -> io.println("Formula is valid in S4")
////   validity.Invalid(model) -> io.println("Countermodel: " <> countermodel.format(model))
////   validity.Unknown(reason) -> io.println("Unknown: " <> reason)
//// }
//// ```

import gleam/list
import gleam/option.{None, Some}
import z3/modal/countermodel.{type KripkeModel}
import z3/modal/frame_conditions.{type ModalSystem}
import z3/modal/kripke.{type KripkeContext, type ModalFormula}
import z3/solver.{type SolverCheckResult}
import z3/types.{type Z3Error}

// =============================================================================
// Types
// =============================================================================

/// Result of a validity check
pub type ValidityResult {
  /// The formula is valid in the given modal system
  Valid
  /// The formula is invalid, with a countermodel
  Invalid(countermodel: KripkeModel)
  /// The result could not be determined
  Unknown(reason: String)
}

/// Options for validity checking
pub type ValidityOptions {
  ValidityOptions(
    /// Maximum number of worlds to consider
    max_worlds: Int,
    /// Name for the initial world
    initial_world: String,
    /// Timeout in milliseconds (0 = no timeout)
    timeout_ms: Int,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Default validity checking options
pub fn default_options() -> ValidityOptions {
  ValidityOptions(max_worlds: 3, initial_world: "w0", timeout_ms: 0)
}

/// Create options with a specific world bound
pub fn with_max_worlds(n: Int) -> ValidityOptions {
  ValidityOptions(..default_options(), max_worlds: n)
}

/// Create options with a timeout
pub fn with_timeout(ms: Int) -> ValidityOptions {
  ValidityOptions(..default_options(), timeout_ms: ms)
}

// =============================================================================
// Main Validity Checking Interface
// =============================================================================

/// Check if a modal formula is valid in a given modal system
///
/// Uses bounded model checking with the specified number of worlds.
/// The formula is valid if no countermodel exists within the bound.
pub fn check_validity(
  formula: ModalFormula,
  system: ModalSystem,
  max_worlds: Int,
) -> ValidityResult {
  check_validity_with_options(formula, system, with_max_worlds(max_worlds))
}

/// Check validity with full options
pub fn check_validity_with_options(
  formula: ModalFormula,
  system: ModalSystem,
  options: ValidityOptions,
) -> ValidityResult {
  // Create Kripke context
  let ctx = kripke.new_context()

  // Generate world names
  let world_names = generate_world_names(options.max_worlds)

  // Get proposition names from formula
  let prop_names = kripke.get_atoms(formula)

  // To check validity of φ, we check satisfiability of ¬φ
  // If ¬φ is satisfiable, φ is invalid (we have a countermodel)
  // If ¬φ is unsatisfiable, φ is valid
  let negated_formula = kripke.modal_not(formula)

  // Translate negated formula at initial world
  let initial_world = kripke.world(ctx, options.initial_world)
  let #(translated, ctx) = kripke.translate(ctx, negated_formula, initial_world)

  // Get frame constraints for the modal system
  let frame_constraints =
    frame_conditions.get_constraints(system, ctx, world_names)

  // Combine all constraints
  let all_constraints = [translated, ..frame_constraints]

  // Create solver and check
  case solver.new() {
    Error(e) -> Unknown(error_to_string(e))
    Ok(s) -> {
      case solver.assert_all(s, all_constraints) {
        Error(e) -> Unknown(error_to_string(e))
        Ok(s) -> {
          case solver.check(s) {
            Error(e) -> Unknown(error_to_string(e))
            Ok(#(_, check_result)) -> {
              interpret_result(
                check_result,
                ctx,
                world_names,
                prop_names,
                options.initial_world,
              )
            }
          }
        }
      }
    }
  }
}

/// Check validity without extracting countermodel
pub fn is_valid(
  formula: ModalFormula,
  system: ModalSystem,
  max_worlds: Int,
) -> Bool {
  case check_validity(formula, system, max_worlds) {
    Valid -> True
    Invalid(_) -> False
    Unknown(_) -> False
  }
}

/// Check satisfiability of a modal formula
pub fn check_satisfiability(
  formula: ModalFormula,
  system: ModalSystem,
  max_worlds: Int,
) -> ValidityResult {
  // Formula is satisfiable iff its negation is not valid
  // So we check if the formula itself has a model
  let ctx = kripke.new_context()
  let world_names = generate_world_names(max_worlds)
  let prop_names = kripke.get_atoms(formula)
  let initial_world_name = "w0"
  let initial_world = kripke.world(ctx, initial_world_name)

  let #(translated, ctx) = kripke.translate(ctx, formula, initial_world)
  let frame_constraints =
    frame_conditions.get_constraints(system, ctx, world_names)
  let all_constraints = [translated, ..frame_constraints]

  case solver.new() {
    Error(e) -> Unknown(error_to_string(e))
    Ok(s) -> {
      case solver.assert_all(s, all_constraints) {
        Error(e) -> Unknown(error_to_string(e))
        Ok(s) -> {
          case solver.check(s) {
            Error(e) -> Unknown(error_to_string(e))
            Ok(#(_, check_result)) -> {
              // For satisfiability, SAT means satisfiable (return model as "Invalid")
              // UNSAT means unsatisfiable (return "Valid" meaning no model exists)
              case check_result {
                solver.SolverSat(model) -> {
                  let extracted =
                    countermodel.extract(
                      model,
                      ctx,
                      world_names,
                      prop_names,
                      initial_world_name,
                    )
                  case extracted {
                    countermodel.Extracted(km) -> Invalid(km)
                    _ -> Unknown("Failed to extract model")
                  }
                }
                solver.SolverUnsat -> Valid
                solver.SolverUnknown(reason) -> Unknown(reason)
              }
            }
          }
        }
      }
    }
  }
}

/// Check if a formula is satisfiable
pub fn is_satisfiable(
  formula: ModalFormula,
  system: ModalSystem,
  max_worlds: Int,
) -> Bool {
  case check_satisfiability(formula, system, max_worlds) {
    Invalid(_) -> True
    // Has a model
    Valid -> False
    // No model
    Unknown(_) -> False
  }
}

// =============================================================================
// Common Validity Checks
// =============================================================================

/// Check if formula is a tautology (valid in K, hence in all systems)
pub fn is_tautology(formula: ModalFormula, max_worlds: Int) -> Bool {
  is_valid(formula, frame_conditions.K, max_worlds)
}

/// Check if formula is a contradiction (unsatisfiable in all systems)
pub fn is_contradiction(formula: ModalFormula, max_worlds: Int) -> Bool {
  !is_satisfiable(formula, frame_conditions.K, max_worlds)
}

/// Check logical equivalence of two formulas
pub fn are_equivalent(
  f1: ModalFormula,
  f2: ModalFormula,
  system: ModalSystem,
  max_worlds: Int,
) -> Bool {
  // f1 ≡ f2 iff (f1 ↔ f2) is valid
  let biconditional = kripke.modal_iff(f1, f2)
  is_valid(biconditional, system, max_worlds)
}

/// Check if one formula implies another
pub fn implies(
  premise: ModalFormula,
  conclusion: ModalFormula,
  system: ModalSystem,
  max_worlds: Int,
) -> Bool {
  // premise → conclusion is valid iff premise → conclusion is true in all models
  let implication = kripke.modal_implies(premise, conclusion)
  is_valid(implication, system, max_worlds)
}

// =============================================================================
// Batch Checking
// =============================================================================

/// Check validity of multiple formulas, returning results for each
pub fn check_batch(
  formulas: List(ModalFormula),
  system: ModalSystem,
  max_worlds: Int,
) -> List(#(ModalFormula, ValidityResult)) {
  list.map(formulas, fn(f) { #(f, check_validity(f, system, max_worlds)) })
}

/// Filter valid formulas from a list
pub fn filter_valid(
  formulas: List(ModalFormula),
  system: ModalSystem,
  max_worlds: Int,
) -> List(ModalFormula) {
  list.filter(formulas, fn(f) { is_valid(f, system, max_worlds) })
}

/// Filter invalid formulas from a list
pub fn filter_invalid(
  formulas: List(ModalFormula),
  system: ModalSystem,
  max_worlds: Int,
) -> List(ModalFormula) {
  list.filter(formulas, fn(f) { !is_valid(f, system, max_worlds) })
}

// =============================================================================
// Result Utilities
// =============================================================================

/// Check if result indicates validity
pub fn is_valid_result(result: ValidityResult) -> Bool {
  case result {
    Valid -> True
    _ -> False
  }
}

/// Check if result indicates invalidity
pub fn is_invalid_result(result: ValidityResult) -> Bool {
  case result {
    Invalid(_) -> True
    _ -> False
  }
}

/// Check if result is unknown
pub fn is_unknown_result(result: ValidityResult) -> Bool {
  case result {
    Unknown(_) -> True
    _ -> False
  }
}

/// Get countermodel from result if available
pub fn get_countermodel(result: ValidityResult) -> option.Option(KripkeModel) {
  case result {
    Invalid(model) -> Some(model)
    _ -> None
  }
}

/// Format validity result as string
pub fn format_result(result: ValidityResult) -> String {
  case result {
    Valid -> "VALID"
    Invalid(model) -> "INVALID\n" <> countermodel.format(model)
    Unknown(reason) -> "UNKNOWN: " <> reason
  }
}

/// Format result with compact model display
pub fn format_result_compact(result: ValidityResult) -> String {
  case result {
    Valid -> "VALID"
    Invalid(model) -> "INVALID: " <> countermodel.format_compact(model)
    Unknown(reason) -> "UNKNOWN: " <> reason
  }
}

// =============================================================================
// Internal Helpers
// =============================================================================

/// Generate world names: w0, w1, w2, ...
fn generate_world_names(count: Int) -> List(String) {
  generate_world_names_helper(0, count, [])
  |> list.reverse
}

fn generate_world_names_helper(
  current: Int,
  max: Int,
  acc: List(String),
) -> List(String) {
  case current >= max {
    True -> acc
    False -> {
      let name = "w" <> int_to_string(current)
      generate_world_names_helper(current + 1, max, [name, ..acc])
    }
  }
}

/// Interpret solver result
fn interpret_result(
  check_result: SolverCheckResult,
  ctx: KripkeContext,
  world_names: List(String),
  prop_names: List(String),
  initial_world: String,
) -> ValidityResult {
  case check_result {
    solver.SolverSat(model) -> {
      // Satisfiable means the negation is satisfiable, so original is invalid
      let extracted =
        countermodel.extract(model, ctx, world_names, prop_names, initial_world)
      case extracted {
        countermodel.Extracted(km) -> Invalid(km)
        countermodel.ExtractionError(e) -> Unknown("Extraction failed: " <> e)
        countermodel.NoCountermodel -> Unknown("Unexpected: SAT but no model")
      }
    }
    solver.SolverUnsat -> Valid
    solver.SolverUnknown(reason) -> Unknown(reason)
  }
}

fn error_to_string(e: Z3Error) -> String {
  case e {
    types.SolverError(msg) -> "Solver error: " <> msg
    types.TimeoutError -> "Timeout"
    types.PortError(msg) -> "Port error: " <> msg
    types.ParseError(msg) -> "Parse error: " <> msg
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
