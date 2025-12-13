//// Z3 Gleam - Gleam bindings to the Z3 theorem prover
////
//// This library provides idiomatic Gleam bindings to Z3, enabling:
//// - SMT solving
//// - Constraint solving
//// - Modal logic verification via Kripke frame encoding
////
//// ## Quick Start
////
//// ```gleam
//// import z3_gleam
//// import z3/types
////
//// // Coming soon: Basic usage example
//// ```

import z3/types

// Re-export types for easier access
pub type Sort = types.Sort
pub type Expr = types.Expr
pub type CheckResult = types.CheckResult
pub type Model = types.Model
pub type Value = types.Value
pub type Z3Error = types.Z3Error
