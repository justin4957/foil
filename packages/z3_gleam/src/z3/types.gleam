//// Core types for Z3 solver interface

/// Sort represents the type of an expression in Z3
pub type Sort {
  BoolSort
  IntSort
  RealSort
  UninterpretedSort(name: String)
  ArraySort(domain: Sort, range: Sort)
}

/// Expression represents a logical or mathematical expression
pub type Expr {
  // Literals
  BoolLit(Bool)
  IntLit(Int)
  RealLit(numerator: Int, denominator: Int)

  // Variables/Constants
  Const(name: String, sort: Sort)

  // Boolean operations
  And(List(Expr))
  Or(List(Expr))
  Not(Expr)
  Implies(Expr, Expr)
  Iff(Expr, Expr)

  // Control flow
  Ite(condition: Expr, then_branch: Expr, else_branch: Expr)

  // Quantifiers
  ForAll(vars: List(#(String, Sort)), body: Expr)
  Exists(vars: List(#(String, Sort)), body: Expr)

  // Arithmetic
  Add(List(Expr))
  Mul(List(Expr))
  Sub(Expr, Expr)
  Div(Expr, Expr)
  Neg(Expr)

  // Comparisons
  Eq(Expr, Expr)
  Lt(Expr, Expr)
  Le(Expr, Expr)
  Gt(Expr, Expr)
  Ge(Expr, Expr)
}

/// Result of a satisfiability check
pub type CheckResult {
  Sat(Model)
  Unsat
  Unknown(reason: String)
}

/// Model represents a satisfying assignment
/// Contains a list of variable-value pairs
pub type Model {
  Model(values: List(#(String, Value)))
}

/// Value represents a concrete value from a model
pub type Value {
  BoolVal(Bool)
  IntVal(Int)
  RealVal(Float)
  UnknownVal(String)
}

/// Error type for Z3 operations
pub type Z3Error {
  SolverError(String)
  TimeoutError
  PortError(String)
  ParseError(String)
}
