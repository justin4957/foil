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

  // Variables/Constants
  Const(name: String, sort: Sort)

  // Boolean operations
  And(List(Expr))
  Or(List(Expr))
  Not(Expr)
  Implies(Expr, Expr)
  Iff(Expr, Expr)

  // Quantifiers
  ForAll(vars: List(#(String, Sort)), body: Expr)
  Exists(vars: List(#(String, Sort)), body: Expr)

  // Arithmetic
  Add(List(Expr))
  Mul(List(Expr))
  Sub(Expr, Expr)
  Div(Expr, Expr)

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
pub type Model

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
