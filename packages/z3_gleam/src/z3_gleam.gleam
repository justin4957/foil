//// Z3 Gleam - Gleam bindings to the Z3 theorem prover
////
//// This library provides idiomatic Gleam bindings to Z3, enabling:
//// - SMT solving
//// - Constraint solving
//// - Modal logic verification via Kripke frame encoding
////
//// ## Modules
////
//// - `z3/expr` - Expression builder for creating Z3 expressions
//// - `z3/solver` - Solver interface for checking satisfiability
//// - `z3/model` - Model inspection for extracting satisfying assignments
//// - `z3/compiler` - Expression compilation to Z3 format
//// - `z3/unsat_core` - Unsat core extraction for debugging
//// - `z3/types` - Core type definitions
////
//// ## Quick Start
////
//// ```gleam
//// import z3/expr
//// import z3/solver
////
//// pub fn main() {
////   // Create variables
////   let x = expr.int_const("x")
////   let y = expr.int_const("y")
////
////   // Build constraints
////   let constraints = [
////     expr.gt(x, expr.int(0)),
////     expr.lt(y, expr.int(10)),
////     expr.eq(expr.add([x, y]), expr.int(15)),
////   ]
////
////   // Solve
////   case solver.solve(constraints) {
////     Ok(solver.SolverSat(model)) -> {
////       // Found a solution!
////     }
////     Ok(solver.SolverUnsat) -> {
////       // No solution exists
////     }
////     _ -> // Error or unknown
////   }
//// }
//// ```

import z3/types

// Re-export types for easier access
pub type Sort =
  types.Sort

pub type Expr =
  types.Expr

pub type CheckResult =
  types.CheckResult

pub type Model =
  types.Model

pub type Value =
  types.Value

pub type Z3Error =
  types.Z3Error

// Re-export sort constructors
pub const bool_sort = types.BoolSort

pub const int_sort = types.IntSort

pub const real_sort = types.RealSort

// Re-export value constructors
pub const bool_val = types.BoolVal

pub const int_val = types.IntVal

pub const real_val = types.RealVal

// Re-export check result constructors
pub const sat = types.Sat

pub const unsat = types.Unsat

pub const unknown = types.Unknown

// Re-export error constructors
pub const solver_error = types.SolverError

pub const timeout_error = types.TimeoutError

pub const port_error = types.PortError

pub const parse_error = types.ParseError
