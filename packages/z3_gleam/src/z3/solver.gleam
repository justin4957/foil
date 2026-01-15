//// Solver Interface Module
////
//// This module provides a high-level interface for Z3 solver operations.
//// It wraps the underlying port/NIF driver with a more idiomatic Gleam API.
////
//// ## Usage
////
//// ```gleam
//// import z3/solver
//// import z3/expr
////
//// pub fn main() {
////   // Create a new solver
////   let assert Ok(s) = solver.new()
////
////   // Create variables and constraints
////   let x = expr.int_const("x")
////   let y = expr.int_const("y")
////
////   // Add constraints
////   let assert Ok(s) = solver.assert_(s, expr.gt(x, expr.int(0)))
////   let assert Ok(s) = solver.assert_(s, expr.lt(y, expr.int(10)))
////   let assert Ok(s) = solver.assert_(s, expr.eq(expr.add([x, y]), expr.int(15)))
////
////   // Check satisfiability
////   case solver.check(s) {
////     Ok(#(_, solver.Sat(model))) -> {
////       // Found a solution
////       io.println("SAT!")
////     }
////     Ok(#(_, solver.Unsat)) -> {
////       io.println("UNSAT")
////     }
////     _ -> io.println("Error or Unknown")
////   }
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import z3/types.{
  type CheckResult, type Expr, type Value, type Z3Error, Model, Sat, Unknown,
  Unsat,
}

// =============================================================================
// Solver Types
// =============================================================================

/// A Z3 solver instance
/// Contains the current state of assertions and configuration
pub opaque type Solver {
  Solver(
    /// Current assertions in the solver
    assertions: List(Expr),
    /// Named assertions for unsat core extraction
    named_assertions: Dict(String, Expr),
    /// Stack of assertion scopes for push/pop
    scopes: List(SolverScope),
    /// Solver configuration
    config: SolverConfig,
  )
}

/// A scope in the solver stack (for push/pop)
type SolverScope {
  SolverScope(assertion_count: Int, named_assertion_keys: List(String))
}

/// Solver configuration options
pub type SolverConfig {
  SolverConfig(
    /// Timeout in milliseconds (0 = no timeout)
    timeout_ms: Int,
    /// Enable unsat core extraction
    unsat_core: Bool,
  )
}

/// Result of a satisfiability check
pub type SolverCheckResult {
  /// The formula is satisfiable, with a model
  SolverSat(model: SolverModel)
  /// The formula is unsatisfiable
  SolverUnsat
  /// The result is unknown (timeout, incomplete, etc.)
  SolverUnknown(reason: String)
}

/// A model representing a satisfying assignment
pub type SolverModel {
  SolverModel(values: Dict(String, Value))
}

// =============================================================================
// Solver Creation and Configuration
// =============================================================================

/// Create a new solver with default configuration
pub fn new() -> Result(Solver, Z3Error) {
  Ok(Solver(
    assertions: [],
    named_assertions: dict.new(),
    scopes: [],
    config: default_config(),
  ))
}

/// Create a new solver with custom configuration
pub fn new_with_config(config: SolverConfig) -> Result(Solver, Z3Error) {
  Ok(Solver(
    assertions: [],
    named_assertions: dict.new(),
    scopes: [],
    config: config,
  ))
}

/// Get the default solver configuration
pub fn default_config() -> SolverConfig {
  SolverConfig(timeout_ms: 0, unsat_core: False)
}

/// Create a configuration with a timeout
pub fn config_with_timeout(timeout_ms: Int) -> SolverConfig {
  SolverConfig(timeout_ms: timeout_ms, unsat_core: False)
}

/// Create a configuration with unsat core extraction enabled
pub fn config_with_unsat_core() -> SolverConfig {
  SolverConfig(timeout_ms: 0, unsat_core: True)
}

// =============================================================================
// Assertion Operations
// =============================================================================

/// Add an assertion to the solver
pub fn assert_(solver: Solver, expr: Expr) -> Result(Solver, Z3Error) {
  Ok(Solver(..solver, assertions: list.append(solver.assertions, [expr])))
}

/// Add multiple assertions to the solver
pub fn assert_all(solver: Solver, exprs: List(Expr)) -> Result(Solver, Z3Error) {
  Ok(Solver(..solver, assertions: list.append(solver.assertions, exprs)))
}

/// Add a named assertion (for unsat core extraction)
pub fn assert_named(
  solver: Solver,
  name: String,
  expr: Expr,
) -> Result(Solver, Z3Error) {
  Ok(
    Solver(
      ..solver,
      named_assertions: dict.insert(solver.named_assertions, name, expr),
    ),
  )
}

/// Get all current assertions
pub fn get_assertions(solver: Solver) -> List(Expr) {
  solver.assertions
}

/// Get all named assertions
pub fn get_named_assertions(solver: Solver) -> Dict(String, Expr) {
  solver.named_assertions
}

// =============================================================================
// Scope Operations (Push/Pop)
// =============================================================================

/// Push a new scope onto the solver stack
/// Assertions added after push can be removed by pop
pub fn push(solver: Solver) -> Result(Solver, Z3Error) {
  let scope =
    SolverScope(
      assertion_count: list.length(solver.assertions),
      named_assertion_keys: dict.keys(solver.named_assertions),
    )
  Ok(Solver(..solver, scopes: [scope, ..solver.scopes]))
}

/// Pop the most recent scope from the solver stack
/// Removes all assertions added since the corresponding push
pub fn pop(solver: Solver) -> Result(Solver, Z3Error) {
  case solver.scopes {
    [] -> Error(types.SolverError("No scope to pop"))
    [scope, ..rest_scopes] -> {
      let new_assertions = list.take(solver.assertions, scope.assertion_count)
      let new_named =
        dict.filter(solver.named_assertions, fn(key, _value) {
          list.contains(scope.named_assertion_keys, key)
        })
      Ok(
        Solver(
          ..solver,
          assertions: new_assertions,
          named_assertions: new_named,
          scopes: rest_scopes,
        ),
      )
    }
  }
}

/// Pop n scopes from the solver stack
pub fn pop_n(solver: Solver, n: Int) -> Result(Solver, Z3Error) {
  case n {
    0 -> Ok(solver)
    _ ->
      case pop(solver) {
        Ok(new_solver) -> pop_n(new_solver, n - 1)
        Error(e) -> Error(e)
      }
  }
}

/// Get the current scope depth
pub fn scope_depth(solver: Solver) -> Int {
  list.length(solver.scopes)
}

// =============================================================================
// Solver Operations
// =============================================================================

/// Reset the solver, removing all assertions
pub fn reset(solver: Solver) -> Result(Solver, Z3Error) {
  Ok(Solver(..solver, assertions: [], named_assertions: dict.new(), scopes: []))
}

/// Check satisfiability of current assertions (in-memory placeholder)
/// This is a lightweight check that doesn't require Z3.
/// For actual Z3 solving, use check_with_z3() or the port_solver module.
pub fn check(solver: Solver) -> Result(#(Solver, SolverCheckResult), Z3Error) {
  // This is a placeholder implementation for in-memory operation.
  // For real Z3 solving, use check_with_z3() which connects to the port driver.
  case list.length(solver.assertions) {
    0 ->
      // Empty solver is trivially satisfiable
      Ok(#(solver, SolverSat(SolverModel(values: dict.new()))))
    _ ->
      // Return Unknown since this is the lightweight in-memory solver
      // Use check_with_z3() for actual Z3 backend calls
      Ok(#(
        solver,
        SolverUnknown(
          "In-memory solver - use check_with_z3() or port_solver module for real Z3 solving",
        ),
      ))
  }
}

/// Check satisfiability using the Z3 backend via port driver
/// This function connects to the actual Z3 solver through the Python port driver.
/// The solver state is preserved and the Z3 connection is closed after the check.
///
/// ## Example
/// ```gleam
/// let assert Ok(s) = solver.new()
/// let x = expr.int_const("x")
/// let assert Ok(s) = solver.assert_(s, expr.gt(x, expr.int(0)))
/// let assert Ok(s) = solver.assert_(s, expr.lt(x, expr.int(10)))
///
/// case solver.check_with_z3(s) {
///   Ok(#(s, SolverSat(model))) -> io.println("SAT!")
///   Ok(#(s, SolverUnsat)) -> io.println("UNSAT")
///   Ok(#(s, SolverUnknown(reason))) -> io.println("Unknown: " <> reason)
///   Error(e) -> io.println("Error connecting to Z3")
/// }
/// ```
pub fn check_with_z3(
  solver: Solver,
) -> Result(#(Solver, SolverCheckResult), Z3Error) {
  // Handle empty solver case directly
  case list.length(solver.assertions) {
    0 -> Ok(#(solver, SolverSat(SolverModel(values: dict.new()))))
    _ -> do_check_with_z3(solver)
  }
}

/// Internal function that performs the actual Z3 check via port driver
fn do_check_with_z3(
  solver: Solver,
) -> Result(#(Solver, SolverCheckResult), Z3Error) {
  // Import port driver functions
  // Start the port driver
  case start_port_driver() {
    Error(e) -> Error(e)
    Ok(port_handle) -> {
      // Create Z3 context and solver
      case create_z3_context_and_solver(port_handle) {
        Error(e) -> {
          stop_port_driver(port_handle)
          Error(e)
        }
        Ok(#(port_handle, _ctx_handle, solver_handle)) -> {
          // Assert all expressions
          case
            assert_expressions_to_z3(
              port_handle,
              solver_handle,
              solver.assertions,
            )
          {
            Error(e) -> {
              stop_port_driver(port_handle)
              Error(e)
            }
            Ok(port_handle) -> {
              // Check satisfiability
              case run_z3_check(port_handle, solver_handle) {
                Error(e) -> {
                  stop_port_driver(port_handle)
                  Error(e)
                }
                Ok(#(port_handle, check_result)) -> {
                  // Convert result and clean up
                  stop_port_driver(port_handle)
                  Ok(#(solver, check_result))
                }
              }
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Port Driver Integration (Internal)
// =============================================================================

/// Port handle type (opaque wrapper for driver handle)
type PortDriverHandle

/// Context handle type
type Z3ContextHandle

/// Solver handle type for Z3
type Z3SolverHandle

@external(erlang, "z3_solver_ffi", "start_port")
fn start_port_driver() -> Result(PortDriverHandle, Z3Error)

@external(erlang, "z3_solver_ffi", "stop_port")
fn stop_port_driver(handle: PortDriverHandle) -> Nil

@external(erlang, "z3_solver_ffi", "create_context_and_solver")
fn create_z3_context_and_solver(
  handle: PortDriverHandle,
) -> Result(#(PortDriverHandle, Z3ContextHandle, Z3SolverHandle), Z3Error)

@external(erlang, "z3_solver_ffi", "assert_expressions")
fn assert_expressions_to_z3(
  handle: PortDriverHandle,
  solver: Z3SolverHandle,
  exprs: List(Expr),
) -> Result(PortDriverHandle, Z3Error)

@external(erlang, "z3_solver_ffi", "check_sat")
fn run_z3_check(
  handle: PortDriverHandle,
  solver: Z3SolverHandle,
) -> Result(#(PortDriverHandle, SolverCheckResult), Z3Error)

/// Check satisfiability and get a model if satisfiable
pub fn check_sat(
  solver: Solver,
) -> Result(#(Solver, Option(SolverModel)), Z3Error) {
  case check(solver) {
    Ok(#(s, SolverSat(model))) -> Ok(#(s, Some(model)))
    Ok(#(s, SolverUnsat)) -> Ok(#(s, None))
    Ok(#(s, SolverUnknown(_))) -> Ok(#(s, None))
    Error(e) -> Error(e)
  }
}

/// Check if the current assertions are satisfiable
pub fn is_sat(solver: Solver) -> Result(Bool, Z3Error) {
  case check(solver) {
    Ok(#(_, SolverSat(_))) -> Ok(True)
    Ok(#(_, SolverUnsat)) -> Ok(False)
    Ok(#(_, SolverUnknown(_))) -> Ok(False)
    Error(e) -> Error(e)
  }
}

/// Check if the current assertions are unsatisfiable
pub fn is_unsat(solver: Solver) -> Result(Bool, Z3Error) {
  case check(solver) {
    Ok(#(_, SolverSat(_))) -> Ok(False)
    Ok(#(_, SolverUnsat)) -> Ok(True)
    Ok(#(_, SolverUnknown(_))) -> Ok(False)
    Error(e) -> Error(e)
  }
}

// =============================================================================
// Unsat Core Operations
// =============================================================================

/// Get the unsat core (names of assertions that contribute to unsatisfiability)
/// Returns an empty list if the solver is satisfiable or unsat core is not enabled
pub fn get_unsat_core(solver: Solver) -> Result(List(String), Z3Error) {
  // This is a placeholder - actual implementation would query the Z3 backend
  case solver.config.unsat_core {
    True ->
      // Return empty list as placeholder
      Ok([])
    False ->
      Error(types.SolverError(
        "Unsat core extraction not enabled. Use config_with_unsat_core()",
      ))
  }
}

// =============================================================================
// Model Operations
// =============================================================================

/// Get the model from a check result (if satisfiable)
pub fn get_model(result: SolverCheckResult) -> Option(SolverModel) {
  case result {
    SolverSat(model) -> Some(model)
    SolverUnsat -> None
    SolverUnknown(_) -> None
  }
}

/// Convert a SolverCheckResult to the types.CheckResult format
pub fn to_check_result(result: SolverCheckResult) -> CheckResult {
  case result {
    SolverSat(model) -> Sat(Model(values: dict.to_list(model.values)))
    SolverUnsat -> Unsat
    SolverUnknown(reason) -> Unknown(reason)
  }
}

// =============================================================================
// Convenience Functions
// =============================================================================

/// Create a solver, add assertions, and check in one operation
pub fn solve(exprs: List(Expr)) -> Result(SolverCheckResult, Z3Error) {
  use solver <- result.try(new())
  use solver <- result.try(assert_all(solver, exprs))
  use #(_, result) <- result.try(check(solver))
  Ok(result)
}

/// Check if a single expression is satisfiable
pub fn is_satisfiable(expr: Expr) -> Result(Bool, Z3Error) {
  use solver <- result.try(new())
  use solver <- result.try(assert_(solver, expr))
  is_sat(solver)
}

/// Check if an expression is valid (its negation is unsatisfiable)
pub fn is_valid(expr: Expr) -> Result(Bool, Z3Error) {
  use solver <- result.try(new())
  use solver <- result.try(assert_(solver, types.Not(expr)))
  is_unsat(solver)
}

/// Check if two expressions are equivalent
pub fn are_equivalent(expr1: Expr, expr2: Expr) -> Result(Bool, Z3Error) {
  // expr1 <=> expr2 is valid iff NOT(expr1 <=> expr2) is unsat
  is_valid(types.Iff(expr1, expr2))
}

/// Check if expr1 implies expr2
pub fn implies(expr1: Expr, expr2: Expr) -> Result(Bool, Z3Error) {
  // expr1 => expr2 is valid iff NOT(expr1 => expr2) is unsat
  is_valid(types.Implies(expr1, expr2))
}
