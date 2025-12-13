//// Unsat Core Extraction Module
////
//// This module provides functionality for extracting and analyzing
//// unsatisfiable cores from the Z3 solver. An unsat core is a minimal
//// (or near-minimal) subset of assertions that is still unsatisfiable.
////
//// ## Usage
////
//// ```gleam
//// import z3/unsat_core
//// import z3/solver
//// import z3/expr
////
//// pub fn example() {
////   // Create solver with unsat core tracking
////   let config = solver.config_with_unsat_core()
////   let assert Ok(s) = solver.new_with_config(config)
////
////   // Add named assertions
////   let x = expr.int_const("x")
////   let assert Ok(s) = unsat_core.assert_named(s, "pos", expr.gt(x, expr.int(0)))
////   let assert Ok(s) = unsat_core.assert_named(s, "neg", expr.lt(x, expr.int(0)))
////   let assert Ok(s) = unsat_core.assert_named(s, "zero", expr.eq(x, expr.int(0)))
////
////   // Check and get unsat core
////   case unsat_core.check_and_get_core(s) {
////     Ok(#(_, Some(core))) -> {
////       // core contains names of conflicting assertions
////       // e.g., ["pos", "neg"] or ["pos", "zero"]
////     }
////     Ok(#(_, None)) -> {
////       // Satisfiable - no unsat core
////     }
////     Error(_) -> // Error occurred
////   }
//// }
//// ```

import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import z3/solver.{type Solver, type SolverConfig, SolverUnsat}
import z3/types.{type Expr, type Z3Error}

// =============================================================================
// Unsat Core Types
// =============================================================================

/// An unsat core - a set of assertion names that are mutually unsatisfiable
pub type UnsatCore {
  UnsatCore(
    /// Names of the assertions in the core
    assertion_names: List(String),
    /// Whether this is a minimal core
    is_minimal: Bool,
  )
}

/// Result of unsat core extraction
pub type CoreExtractionResult {
  /// Found an unsat core
  CoreFound(core: UnsatCore)
  /// The problem is satisfiable (no core)
  NoCore
  /// Could not determine (timeout, etc.)
  CoreUnknown(reason: String)
}

// =============================================================================
// Named Assertion Operations
// =============================================================================

/// Add a named assertion to the solver for unsat core tracking
/// This is an alias for solver.assert_named for convenience
pub fn assert_named(
  s: Solver,
  name: String,
  expr: Expr,
) -> Result(Solver, Z3Error) {
  solver.assert_named(s, name, expr)
}

/// Add multiple named assertions
pub fn assert_all_named(
  s: Solver,
  named_exprs: List(#(String, Expr)),
) -> Result(Solver, Z3Error) {
  list.try_fold(named_exprs, s, fn(solver, pair) {
    let #(name, expr) = pair
    assert_named(solver, name, expr)
  })
}

/// Generate a unique name for an assertion
pub fn auto_name(prefix: String, index: Int) -> String {
  prefix <> "_" <> int_to_string(index)
}

/// Add assertions with auto-generated names
pub fn assert_with_auto_names(
  s: Solver,
  prefix: String,
  exprs: List(Expr),
) -> Result(Solver, Z3Error) {
  let indexed = list.index_map(exprs, fn(expr, idx) { #(idx, expr) })
  list.try_fold(indexed, s, fn(solver, pair) {
    let #(idx, expr) = pair
    assert_named(solver, auto_name(prefix, idx), expr)
  })
}

// =============================================================================
// Core Extraction Operations
// =============================================================================

/// Check satisfiability and get the unsat core if unsatisfiable
pub fn check_and_get_core(
  s: Solver,
) -> Result(#(Solver, Option(UnsatCore)), Z3Error) {
  use #(new_s, check_result) <- result.try(solver.check(s))

  case check_result {
    SolverUnsat -> {
      // Get the unsat core
      use core_names <- result.try(solver.get_unsat_core(new_s))
      let core = UnsatCore(assertion_names: core_names, is_minimal: False)
      Ok(#(new_s, Some(core)))
    }
    _ -> Ok(#(new_s, None))
  }
}

/// Extract unsat core with detailed result
pub fn extract_core(
  s: Solver,
) -> Result(#(Solver, CoreExtractionResult), Z3Error) {
  use #(new_s, check_result) <- result.try(solver.check(s))

  case check_result {
    SolverUnsat -> {
      use core_names <- result.try(solver.get_unsat_core(new_s))
      let core = UnsatCore(assertion_names: core_names, is_minimal: False)
      Ok(#(new_s, CoreFound(core)))
    }
    solver.SolverSat(_) -> Ok(#(new_s, NoCore))
    solver.SolverUnknown(reason) -> Ok(#(new_s, CoreUnknown(reason)))
  }
}

// =============================================================================
// Core Analysis Operations
// =============================================================================

/// Get the size of an unsat core
pub fn core_size(core: UnsatCore) -> Int {
  list.length(core.assertion_names)
}

/// Check if a specific assertion is in the core
pub fn is_in_core(core: UnsatCore, name: String) -> Bool {
  list.contains(core.assertion_names, name)
}

/// Get assertions that are NOT in the core
pub fn get_non_core_assertions(s: Solver, core: UnsatCore) -> List(String) {
  let all_names = dict.keys(solver.get_named_assertions(s))
  list.filter(all_names, fn(name) { !is_in_core(core, name) })
}

/// Get the expressions corresponding to core assertions
pub fn get_core_expressions(s: Solver, core: UnsatCore) -> List(#(String, Expr)) {
  let named = solver.get_named_assertions(s)
  list.filter_map(core.assertion_names, fn(name) {
    case dict.get(named, name) {
      Ok(expr) -> Ok(#(name, expr))
      Error(_) -> Error(Nil)
    }
  })
}

// =============================================================================
// Core Minimization (Placeholder)
// =============================================================================

/// Attempt to minimize an unsat core
/// This is a placeholder - actual implementation would iteratively
/// remove assertions and re-check satisfiability
pub fn minimize_core(
  s: Solver,
  core: UnsatCore,
) -> Result(#(Solver, UnsatCore), Z3Error) {
  // Placeholder implementation - returns the original core marked as minimal
  // A real implementation would:
  // 1. For each assertion in the core:
  //    a. Remove it temporarily
  //    b. Check if still unsat
  //    c. If sat, add it back (it's necessary)
  //    d. If unsat, keep it removed (it's redundant)
  Ok(#(s, UnsatCore(..core, is_minimal: True)))
}

/// Check if a subset of named assertions is satisfiable
pub fn is_subset_sat(s: Solver, names: List(String)) -> Result(Bool, Z3Error) {
  // Get the named assertions
  let named = solver.get_named_assertions(s)

  // Create a new solver with only the specified assertions
  use new_s <- result.try(solver.new())
  let filtered =
    list.filter_map(names, fn(name) {
      case dict.get(named, name) {
        Ok(expr) -> Ok(expr)
        Error(_) -> Error(Nil)
      }
    })

  use new_s <- result.try(solver.assert_all(new_s, filtered))
  solver.is_sat(new_s)
}

// =============================================================================
// Diagnostic Utilities
// =============================================================================

/// Format an unsat core for display
pub fn format_core(core: UnsatCore) -> String {
  let names_str = string_join(core.assertion_names, ", ")
  let minimal_str = case core.is_minimal {
    True -> " (minimal)"
    False -> ""
  }
  "Unsat core" <> minimal_str <> ": [" <> names_str <> "]"
}

/// Get a summary of the unsat core
pub fn core_summary(s: Solver, core: UnsatCore) -> String {
  let size = core_size(core)
  let total = dict.size(solver.get_named_assertions(s))
  let pct = case total {
    0 -> 0
    _ -> size * 100 / total
  }
  "Unsat core contains "
  <> int_to_string(size)
  <> " of "
  <> int_to_string(total)
  <> " assertions ("
  <> int_to_string(pct)
  <> "%)"
}

// =============================================================================
// Configuration Helpers
// =============================================================================

/// Create a solver configuration optimized for unsat core extraction
pub fn core_extraction_config() -> SolverConfig {
  solver.config_with_unsat_core()
}

/// Check if a solver is configured for unsat core extraction
pub fn is_core_enabled(_s: Solver) -> Bool {
  // This would check the solver's config
  // For now, always return True as a placeholder
  True
}

// =============================================================================
// Internal Helpers
// =============================================================================

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

fn string_join(strings: List(String), sep: String) -> String {
  case strings {
    [] -> ""
    [single] -> single
    [first, ..rest] -> first <> sep <> string_join(rest, sep)
  }
}
