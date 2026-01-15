//// Port-Connected Solver Module
////
//// This module provides a solver implementation that connects to the Z3 backend
//// via the port driver. It bridges the high-level Solver API with actual Z3 calls.
////
//// ## Architecture
////
//// ```
//// ┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
//// │   solver.gleam  │ ──▶ │  port_solver.gleam│ ──▶ │ port/driver.gleam│
//// │  (Solver type)  │     │  (Bridge layer)   │     │ (Port comms)    │
//// └─────────────────┘     └──────────────────┘     └─────────────────┘
////                                                          │
////                                                          ▼
////                                                  ┌─────────────────┐
////                                                  │ z3_driver.py    │
////                                                  │ (Python Z3)     │
////                                                  └─────────────────┘
//// ```
////
//// ## Usage
////
//// ```gleam
//// import z3/port_solver
//// import z3/solver
//// import z3/expr
////
//// // Create a port-connected solver
//// let assert Ok(ps) = port_solver.new()
////
//// // Add constraints
//// let x = expr.int_const("x")
//// let assert Ok(ps) = port_solver.assert_(ps, expr.gt(x, expr.int(0)))
//// let assert Ok(ps) = port_solver.assert_(ps, expr.lt(x, expr.int(10)))
////
//// // Check satisfiability (calls real Z3)
//// let assert Ok(#(ps, result)) = port_solver.check(ps)
////
//// // Clean up
//// port_solver.close(ps)
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import z3/compiler
import z3/port/driver.{type ContextHandle, type PortHandle, type SolverHandle}
import z3/solver.{
  type SolverCheckResult, type SolverModel, SolverModel, SolverSat,
  SolverUnknown, SolverUnsat,
}
import z3/types.{type Expr, type Value, type Z3Error, Model, Sat, Unknown, Unsat}

// =============================================================================
// Types
// =============================================================================

/// A port-connected solver that communicates with Z3 via the port driver
pub opaque type PortSolver {
  PortSolver(
    /// Handle to the port driver process
    port_handle: PortHandle,
    /// Handle to the Z3 context
    context_handle: ContextHandle,
    /// Handle to the Z3 solver
    solver_handle: SolverHandle,
    /// Current assertions (for tracking state)
    assertions: List(Expr),
    /// Named assertions for unsat core
    named_assertions: Dict(String, Expr),
    /// Scope stack for push/pop
    scope_stack: List(ScopeState),
  )
}

/// State saved when pushing a scope
type ScopeState {
  ScopeState(assertion_count: Int, named_keys: List(String))
}

/// Result type for port solver operations
pub type PortSolverResult(a) =
  Result(#(PortSolver, a), Z3Error)

// =============================================================================
// Solver Lifecycle
// =============================================================================

/// Create a new port-connected solver
/// This starts the Z3 port driver and creates a context and solver
pub fn new() -> Result(PortSolver, Z3Error) {
  // Start the port driver
  use port_handle <- result.try(driver.start())

  // Create a Z3 context
  use #(port_handle, context_handle) <- result.try(driver.new_context(
    port_handle,
  ))

  // Create a Z3 solver for the context
  use #(port_handle, solver_handle) <- result.try(driver.new_solver(
    port_handle,
    context_handle,
  ))

  Ok(
    PortSolver(
      port_handle: port_handle,
      context_handle: context_handle,
      solver_handle: solver_handle,
      assertions: [],
      named_assertions: dict.new(),
      scope_stack: [],
    ),
  )
}

/// Close the port solver and clean up resources
pub fn close(solver: PortSolver) -> Nil {
  driver.stop(solver.port_handle)
}

/// Check if the port solver is alive by pinging the driver
pub fn is_alive(solver: PortSolver) -> Result(PortSolver, Z3Error) {
  case driver.ping(solver.port_handle) {
    Ok(new_handle) -> Ok(PortSolver(..solver, port_handle: new_handle))
    Error(e) -> Error(e)
  }
}

// =============================================================================
// Assertion Operations
// =============================================================================

/// Add an assertion to the solver
pub fn assert_(solver: PortSolver, expr: Expr) -> Result(PortSolver, Z3Error) {
  // Compile the expression to JSON (validates it can be compiled)
  use _json <- result.try(compiler.compile(expr))

  // Send the assertion to Z3 via port driver
  use new_handle <- result.try(driver.assert_constraint(
    solver.port_handle,
    solver.solver_handle,
    expr,
  ))

  // Update local state
  Ok(
    PortSolver(
      ..solver,
      port_handle: new_handle,
      assertions: list.append(solver.assertions, [expr]),
    ),
  )
}

/// Add multiple assertions to the solver
pub fn assert_all(
  solver: PortSolver,
  exprs: List(Expr),
) -> Result(PortSolver, Z3Error) {
  list.fold(exprs, Ok(solver), fn(result, expr) {
    case result {
      Ok(s) -> assert_(s, expr)
      Error(e) -> Error(e)
    }
  })
}

/// Add a named assertion (for unsat core extraction)
pub fn assert_named(
  solver: PortSolver,
  name: String,
  expr: Expr,
) -> Result(PortSolver, Z3Error) {
  // First add the assertion normally
  use solver <- result.try(assert_(solver, expr))

  // Track the named assertion locally
  Ok(
    PortSolver(
      ..solver,
      named_assertions: dict.insert(solver.named_assertions, name, expr),
    ),
  )
}

/// Get all current assertions
pub fn get_assertions(solver: PortSolver) -> List(Expr) {
  solver.assertions
}

/// Get all named assertions
pub fn get_named_assertions(solver: PortSolver) -> Dict(String, Expr) {
  solver.named_assertions
}

// =============================================================================
// Scope Operations (Push/Pop)
// =============================================================================

/// Push a new scope onto the solver stack
pub fn push(solver: PortSolver) -> Result(PortSolver, Z3Error) {
  // Send push to Z3
  use new_handle <- result.try(driver.push(
    solver.port_handle,
    solver.solver_handle,
  ))

  // Save current state
  let scope_state =
    ScopeState(
      assertion_count: list.length(solver.assertions),
      named_keys: dict.keys(solver.named_assertions),
    )

  Ok(
    PortSolver(..solver, port_handle: new_handle, scope_stack: [
      scope_state,
      ..solver.scope_stack
    ]),
  )
}

/// Pop the most recent scope from the solver stack
pub fn pop(solver: PortSolver) -> Result(PortSolver, Z3Error) {
  case solver.scope_stack {
    [] -> Error(types.SolverError("No scope to pop"))
    [scope_state, ..rest] -> {
      // Send pop to Z3
      use new_handle <- result.try(driver.pop(
        solver.port_handle,
        solver.solver_handle,
        1,
      ))

      // Restore state
      let new_assertions =
        list.take(solver.assertions, scope_state.assertion_count)
      let new_named =
        dict.filter(solver.named_assertions, fn(key, _) {
          list.contains(scope_state.named_keys, key)
        })

      Ok(
        PortSolver(
          ..solver,
          port_handle: new_handle,
          assertions: new_assertions,
          named_assertions: new_named,
          scope_stack: rest,
        ),
      )
    }
  }
}

/// Pop n scopes from the solver stack
pub fn pop_n(solver: PortSolver, n: Int) -> Result(PortSolver, Z3Error) {
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
pub fn scope_depth(solver: PortSolver) -> Int {
  list.length(solver.scope_stack)
}

// =============================================================================
// Solver Operations
// =============================================================================

/// Reset the solver, removing all assertions
pub fn reset(solver: PortSolver) -> Result(PortSolver, Z3Error) {
  // Send reset to Z3
  use new_handle <- result.try(driver.reset(
    solver.port_handle,
    solver.solver_handle,
  ))

  Ok(
    PortSolver(
      ..solver,
      port_handle: new_handle,
      assertions: [],
      named_assertions: dict.new(),
      scope_stack: [],
    ),
  )
}

/// Check satisfiability of current assertions
/// This actually calls Z3 via the port driver
pub fn check(
  solver: PortSolver,
) -> Result(#(PortSolver, SolverCheckResult), Z3Error) {
  // Call the port driver to check satisfiability
  use #(new_handle, check_result) <- result.try(driver.check(
    solver.port_handle,
    solver.solver_handle,
  ))

  // Convert CheckResult to SolverCheckResult
  let solver_result = case check_result {
    Sat(Model(values)) -> {
      // Convert model values to dict
      let value_dict = dict.from_list(values)
      SolverSat(SolverModel(values: value_dict))
    }
    Unsat -> SolverUnsat
    Unknown(reason) -> SolverUnknown(reason)
  }

  Ok(#(PortSolver(..solver, port_handle: new_handle), solver_result))
}

/// Check satisfiability and get a model if satisfiable
pub fn check_sat(
  solver: PortSolver,
) -> Result(#(PortSolver, Option(SolverModel)), Z3Error) {
  use #(new_solver, result) <- result.try(check(solver))
  case result {
    SolverSat(model) -> Ok(#(new_solver, Some(model)))
    SolverUnsat -> Ok(#(new_solver, None))
    SolverUnknown(_) -> Ok(#(new_solver, None))
  }
}

/// Check if the current assertions are satisfiable
pub fn is_sat(solver: PortSolver) -> Result(#(PortSolver, Bool), Z3Error) {
  use #(new_solver, result) <- result.try(check(solver))
  case result {
    SolverSat(_) -> Ok(#(new_solver, True))
    SolverUnsat -> Ok(#(new_solver, False))
    SolverUnknown(_) -> Ok(#(new_solver, False))
  }
}

/// Check if the current assertions are unsatisfiable
pub fn is_unsat(solver: PortSolver) -> Result(#(PortSolver, Bool), Z3Error) {
  use #(new_solver, result) <- result.try(check(solver))
  case result {
    SolverSat(_) -> Ok(#(new_solver, False))
    SolverUnsat -> Ok(#(new_solver, True))
    SolverUnknown(_) -> Ok(#(new_solver, False))
  }
}

// =============================================================================
// Convenience Functions
// =============================================================================

/// Create a solver, add assertions, check, and return the result
/// The solver is closed automatically
pub fn solve(exprs: List(Expr)) -> Result(SolverCheckResult, Z3Error) {
  use solver <- result.try(new())
  use solver <- result.try(assert_all(solver, exprs))
  use #(solver, result) <- result.try(check(solver))
  close(solver)
  Ok(result)
}

/// Check if a single expression is satisfiable
pub fn is_satisfiable(expr: Expr) -> Result(Bool, Z3Error) {
  use solver <- result.try(new())
  use solver <- result.try(assert_(solver, expr))
  use #(solver, is_sat) <- result.try(is_sat(solver))
  close(solver)
  Ok(is_sat)
}

/// Check if an expression is valid (its negation is unsatisfiable)
pub fn is_valid(expr: Expr) -> Result(Bool, Z3Error) {
  use solver <- result.try(new())
  use solver <- result.try(assert_(solver, types.Not(expr)))
  use #(solver, is_unsat) <- result.try(is_unsat(solver))
  close(solver)
  Ok(is_unsat)
}

/// Check if two expressions are equivalent
pub fn are_equivalent(expr1: Expr, expr2: Expr) -> Result(Bool, Z3Error) {
  is_valid(types.Iff(expr1, expr2))
}

/// Check if expr1 implies expr2
pub fn implies(expr1: Expr, expr2: Expr) -> Result(Bool, Z3Error) {
  is_valid(types.Implies(expr1, expr2))
}

// =============================================================================
// Model Extraction
// =============================================================================

/// Get the model from the last satisfiable check
/// Must be called after a check() that returned SolverSat
pub fn get_model(
  solver: PortSolver,
) -> Result(#(PortSolver, Dict(String, Value)), Z3Error) {
  use #(new_handle, model_handle) <- result.try(driver.get_model(
    solver.port_handle,
    solver.solver_handle,
  ))

  let values = driver.model_values(model_handle)
  let value_dict = dict.from_list(values)

  Ok(#(PortSolver(..solver, port_handle: new_handle), value_dict))
}

// =============================================================================
// Interoperability with solver.gleam
// =============================================================================

/// Convert a PortSolver check result to a solver.Solver compatible result
/// This allows code using the high-level solver API to switch to port solver
pub fn to_solver_result(result: SolverCheckResult) -> solver.SolverCheckResult {
  result
}

/// Get statistics about the solver state
pub fn stats(solver: PortSolver) -> Dict(String, Int) {
  dict.from_list([
    #("assertions", list.length(solver.assertions)),
    #("named_assertions", dict.size(solver.named_assertions)),
    #("scope_depth", list.length(solver.scope_stack)),
  ])
}
