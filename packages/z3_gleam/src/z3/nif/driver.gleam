//// Z3 NIF Driver
////
//// This module provides a NIF-based backend for Z3 operations.
//// It offers lower latency than the port-based driver but requires
//// the Z3 C library to be installed at build time.
////
//// ## Usage
////
//// ```gleam
//// import z3/nif/driver
////
//// // Check if NIF backend is available
//// case driver.is_available() {
////   True -> {
////     let assert Ok(ctx) = driver.new_context()
////     let assert Ok(solver) = driver.new_solver(ctx)
////     // ... use solver ...
////   }
////   False -> // Fall back to port driver
//// }
//// ```

import gleam/result
import z3/types.{type Z3Error, PortError, SolverError}

/// Opaque type representing a Z3 context (NIF resource)
pub type NifContext

/// Opaque type representing a Z3 solver (NIF resource)
pub type NifSolver

/// Opaque type representing a Z3 model (NIF resource)
pub type NifModel

/// Result of satisfiability check
pub type NifCheckResult {
  NifSat
  NifUnsat
  NifUnknown
}

// External NIF functions
@external(erlang, "z3_nif", "is_available")
fn nif_is_available() -> Bool

@external(erlang, "z3_nif", "version")
fn nif_version() -> Result(String, Nil)

@external(erlang, "z3_nif", "mk_context")
fn nif_mk_context() -> Result(NifContext, String)

@external(erlang, "z3_nif", "del_context")
fn nif_del_context(ctx: NifContext) -> Result(Nil, String)

@external(erlang, "z3_nif", "mk_solver")
fn nif_mk_solver(ctx: NifContext) -> Result(NifSolver, String)

@external(erlang, "z3_nif", "solver_check")
fn nif_solver_check(solver: NifSolver) -> Result(NifCheckResult, String)

@external(erlang, "z3_nif", "solver_get_model")
fn nif_solver_get_model(solver: NifSolver) -> Result(NifModel, String)

@external(erlang, "z3_nif", "solver_push")
fn nif_solver_push(solver: NifSolver) -> Result(Nil, String)

@external(erlang, "z3_nif", "solver_pop")
fn nif_solver_pop(solver: NifSolver, n: Int) -> Result(Nil, String)

@external(erlang, "z3_nif", "solver_reset")
fn nif_solver_reset(solver: NifSolver) -> Result(Nil, String)

@external(erlang, "z3_nif", "model_to_string")
fn nif_model_to_string(model: NifModel) -> Result(String, String)

/// Check if the NIF backend is available
/// Returns True if Z3 NIF was compiled and loaded successfully
pub fn is_available() -> Bool {
  nif_is_available()
}

/// Get the Z3 version string
/// Returns Error if NIF is not available
pub fn version() -> Result(String, Z3Error) {
  case is_available() {
    True ->
      nif_version()
      |> result.map_error(fn(_) { PortError("NIF not available") })
    False -> Error(PortError("Z3 NIF not available"))
  }
}

/// Create a new Z3 context
/// The context manages memory for all Z3 objects
pub fn new_context() -> Result(NifContext, Z3Error) {
  case is_available() {
    True ->
      nif_mk_context()
      |> result.map_error(fn(msg) { SolverError(msg) })
    False -> Error(PortError("Z3 NIF not available"))
  }
}

/// Delete a Z3 context and free its resources
/// Note: Resources are also cleaned up by garbage collection
pub fn delete_context(ctx: NifContext) -> Result(Nil, Z3Error) {
  nif_del_context(ctx)
  |> result.map_error(fn(msg) { SolverError(msg) })
}

/// Create a new solver for a context
pub fn new_solver(ctx: NifContext) -> Result(NifSolver, Z3Error) {
  nif_mk_solver(ctx)
  |> result.map_error(fn(msg) { SolverError(msg) })
}

/// Check the satisfiability of constraints in the solver
pub fn check(solver: NifSolver) -> Result(NifCheckResult, Z3Error) {
  nif_solver_check(solver)
  |> result.map_error(fn(msg) { SolverError(msg) })
}

/// Get the model after a satisfiable check
/// Must be called after check() returns NifSat
pub fn get_model(solver: NifSolver) -> Result(NifModel, Z3Error) {
  nif_solver_get_model(solver)
  |> result.map_error(fn(msg) { SolverError(msg) })
}

/// Push a new scope onto the solver stack
/// Allows assertions to be retracted later with pop()
pub fn push(solver: NifSolver) -> Result(Nil, Z3Error) {
  nif_solver_push(solver)
  |> result.map_error(fn(msg) { SolverError(msg) })
}

/// Pop n scopes from the solver stack
/// Retracts assertions made since the corresponding push()
pub fn pop(solver: NifSolver, n: Int) -> Result(Nil, Z3Error) {
  nif_solver_pop(solver, n)
  |> result.map_error(fn(msg) { SolverError(msg) })
}

/// Reset the solver, removing all assertions
pub fn reset(solver: NifSolver) -> Result(Nil, Z3Error) {
  nif_solver_reset(solver)
  |> result.map_error(fn(msg) { SolverError(msg) })
}

/// Convert a model to a human-readable string
pub fn model_to_string(model: NifModel) -> Result(String, Z3Error) {
  nif_model_to_string(model)
  |> result.map_error(fn(msg) { SolverError(msg) })
}
