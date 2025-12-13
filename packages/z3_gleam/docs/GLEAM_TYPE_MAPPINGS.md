# Gleam Type Mappings for Z3 (B1.2)

This document describes how Z3 concepts map to Gleam types, providing a type-safe, idiomatic Gleam interface to Z3.

## Design Principles

1. **Type Safety**: Leverage Gleam's type system to prevent invalid Z3 operations at compile time
2. **Immutability**: All types are immutable, matching Gleam's functional paradigm
3. **Explicitness**: Error cases are explicit via Result types
4. **Composability**: Expression builders compose naturally using pipe operators

## Type Mappings

### Sort (Z3_sort)

Sorts represent Z3's type system. They're mapped to a sum type:

```gleam
/// Sort represents the type of an expression in Z3
pub type Sort {
  /// Boolean sort (true/false)
  BoolSort

  /// Arbitrary-precision integer sort
  IntSort

  /// Real number sort (rationals)
  RealSort

  /// Bit-vector sort with specified bit width
  BitVecSort(bits: Int)

  /// User-defined uninterpreted sort (for abstract types)
  UninterpretedSort(name: String)

  /// Array sort with domain and range sorts
  ArraySort(domain: Sort, range: Sort)

  /// Set sort (syntactic sugar for Array(T, Bool))
  SetSort(element: Sort)
}
```

**Rationale:**
- Sum types provide exhaustive pattern matching
- `BitVecSort` includes bit width for verification
- `SetSort` is a convenience wrapper

### Expr (Z3_ast)

Expressions are the core of Z3 formula construction:

```gleam
/// Expression represents a logical or mathematical expression
pub type Expr {
  // === Literals ===
  /// Boolean literal
  BoolLit(value: Bool)
  /// Integer literal
  IntLit(value: Int)
  /// Real number literal (as rational: numerator/denominator)
  RealLit(numerator: Int, denominator: Int)
  /// Bit-vector literal
  BitVecLit(value: Int, bits: Int)

  // === Variables/Constants ===
  /// Named constant with a sort
  Const(name: String, sort: Sort)

  // === Boolean Operations ===
  /// Conjunction (AND) - n-ary
  And(exprs: List(Expr))
  /// Disjunction (OR) - n-ary
  Or(exprs: List(Expr))
  /// Negation (NOT)
  Not(expr: Expr)
  /// Implication (=>)
  Implies(antecedent: Expr, consequent: Expr)
  /// Bi-implication (<=>)
  Iff(left: Expr, right: Expr)
  /// If-then-else
  Ite(condition: Expr, then_branch: Expr, else_branch: Expr)

  // === Quantifiers ===
  /// Universal quantification (forall)
  ForAll(
    /// Bound variables with names and sorts
    vars: List(#(String, Sort)),
    /// Body expression
    body: Expr,
  )
  /// Existential quantification (exists)
  Exists(
    vars: List(#(String, Sort)),
    body: Expr,
  )

  // === Arithmetic Operations ===
  /// Addition - n-ary
  Add(exprs: List(Expr))
  /// Subtraction - binary
  Sub(left: Expr, right: Expr)
  /// Multiplication - n-ary
  Mul(exprs: List(Expr))
  /// Division
  Div(numerator: Expr, denominator: Expr)
  /// Modulo (integer)
  Mod(left: Expr, right: Expr)
  /// Unary minus
  Neg(expr: Expr)

  // === Comparisons ===
  /// Equality
  Eq(left: Expr, right: Expr)
  /// Distinct (all different)
  Distinct(exprs: List(Expr))
  /// Less than
  Lt(left: Expr, right: Expr)
  /// Less than or equal
  Le(left: Expr, right: Expr)
  /// Greater than
  Gt(left: Expr, right: Expr)
  /// Greater than or equal
  Ge(left: Expr, right: Expr)

  // === Array Operations ===
  /// Array selection: arr[idx]
  Select(array: Expr, index: Expr)
  /// Array update: arr with [idx] = val
  Store(array: Expr, index: Expr, value: Expr)
  /// Constant array (all elements same value)
  ConstArray(sort: Sort, value: Expr)
}
```

**Rationale:**
- Recursive sum type allows arbitrary expression nesting
- N-ary operations (`And`, `Or`, `Add`, `Mul`) accept lists for convenience
- Named fields improve readability
- Array operations support SMT-LIB array theory

### CheckResult

Result of satisfiability checking:

```gleam
/// Result of a satisfiability check
pub type CheckResult {
  /// Formula is satisfiable; model provides witness
  Sat(model: Model)

  /// Formula is unsatisfiable
  Unsat

  /// Solver couldn't determine (timeout, resource limit)
  Unknown(reason: String)
}
```

**Rationale:**
- Three-valued logic matches Z3's `Z3_lbool`
- `Sat` includes the model for immediate access
- `Unknown` includes reason for debugging

### Model

Opaque type representing a satisfying assignment:

```gleam
/// Model represents a satisfying assignment from the solver
///
/// This is an opaque type - use model_eval and model_get_value
/// to extract information.
pub opaque type Model {
  Model(
    /// Internal handle to the model
    handle: ModelHandle,
    /// Constants defined in the model
    constants: List(#(String, Value)),
  )
}

/// Internal model handle (implementation-specific)
pub type ModelHandle
```

**Rationale:**
- Opaque type prevents invalid operations
- Caches constant values for efficiency

### Value

Concrete values extracted from models:

```gleam
/// Value represents a concrete value from a model
pub type Value {
  /// Boolean value
  BoolVal(value: Bool)

  /// Integer value (arbitrary precision via BigInt string)
  IntVal(value: Int)

  /// Real value as rational
  RealVal(numerator: Int, denominator: Int)

  /// Bit-vector value
  BitVecVal(value: Int, bits: Int)

  /// Array value (finite map representation)
  ArrayVal(
    /// Explicit mappings
    mappings: List(#(Value, Value)),
    /// Default value for unmapped indices
    default: Value,
  )

  /// Uninterpreted constant (by name)
  UninterpretedVal(name: String)

  /// Value couldn't be determined
  UnknownVal(reason: String)
}
```

**Rationale:**
- Covers all Z3 value types
- `ArrayVal` uses finite representation with default
- `UnknownVal` handles edge cases gracefully

### Z3Error

Error type for Z3 operations:

```gleam
/// Error type for Z3 operations
pub type Z3Error {
  /// Error from the Z3 solver itself
  SolverError(message: String)

  /// Operation timed out
  TimeoutError(timeout_ms: Int)

  /// Communication error with Z3 (Port/NIF)
  PortError(message: String)

  /// Failed to parse Z3 response
  ParseError(message: String)

  /// Invalid operation (type mismatch, etc.)
  InvalidOperationError(message: String)

  /// Z3 not available/not installed
  Z3NotFoundError
}
```

**Rationale:**
- Distinguishes error sources for appropriate handling
- All errors include descriptive messages

## Context Management

```gleam
/// Z3 context configuration
pub type Config {
  Config(
    /// Timeout in milliseconds (0 = no timeout)
    timeout_ms: Int,
    /// Enable proof generation
    proof: Bool,
    /// Enable model generation (default: true)
    model: Bool,
    /// Enable unsat core generation
    unsat_core: Bool,
  )
}

/// Default configuration
pub fn default_config() -> Config {
  Config(
    timeout_ms: 0,
    proof: False,
    model: True,
    unsat_core: False,
  )
}

/// Z3 context (opaque)
pub opaque type Context {
  Context(handle: ContextHandle)
}

pub type ContextHandle
```

## Solver Interface

```gleam
/// Solver state
pub opaque type Solver {
  Solver(
    context: Context,
    handle: SolverHandle,
  )
}

pub type SolverHandle

/// Create a new solver
pub fn new_solver(ctx: Context) -> Solver

/// Assert a constraint
pub fn assert(solver: Solver, expr: Expr) -> Solver

/// Check satisfiability
pub fn check(solver: Solver) -> Result(CheckResult, Z3Error)

/// Push a scope for incremental solving
pub fn push(solver: Solver) -> Solver

/// Pop n scopes
pub fn pop(solver: Solver, n: Int) -> Solver

/// Reset the solver
pub fn reset(solver: Solver) -> Solver
```

## Expression Builder API

Fluent API for building expressions:

```gleam
// === Variables ===
pub fn bool_const(name: String) -> Expr
pub fn int_const(name: String) -> Expr
pub fn real_const(name: String) -> Expr

// === Literals ===
pub fn bool(value: Bool) -> Expr
pub fn int(value: Int) -> Expr
pub fn real(num: Int, den: Int) -> Expr

// === Boolean Operations ===
pub fn and(exprs: List(Expr)) -> Expr
pub fn or(exprs: List(Expr)) -> Expr
pub fn not(expr: Expr) -> Expr
pub fn implies(a: Expr, b: Expr) -> Expr
pub fn iff(a: Expr, b: Expr) -> Expr

// === Arithmetic ===
pub fn add(exprs: List(Expr)) -> Expr
pub fn sub(a: Expr, b: Expr) -> Expr
pub fn mul(exprs: List(Expr)) -> Expr
pub fn div(a: Expr, b: Expr) -> Expr
pub fn neg(expr: Expr) -> Expr

// === Comparisons ===
pub fn eq(a: Expr, b: Expr) -> Expr
pub fn lt(a: Expr, b: Expr) -> Expr
pub fn le(a: Expr, b: Expr) -> Expr
pub fn gt(a: Expr, b: Expr) -> Expr
pub fn ge(a: Expr, b: Expr) -> Expr

// === Quantifiers ===
pub fn forall(vars: List(#(String, Sort)), body: Expr) -> Expr
pub fn exists(vars: List(#(String, Sort)), body: Expr) -> Expr
```

## Example Usage

```gleam
import z3_gleam.{bool_const, int_const, and, or, implies, eq, lt, check}
import z3_gleam/solver

pub fn example() {
  // Create solver
  let ctx = z3_gleam.new_context(default_config())
  let s = solver.new(ctx)

  // Create variables
  let x = int_const("x")
  let y = int_const("y")

  // Add constraints: x > 0 AND y > 0 AND x + y < 10
  let s = s
    |> solver.assert(gt(x, int(0)))
    |> solver.assert(gt(y, int(0)))
    |> solver.assert(lt(add([x, y]), int(10)))

  // Check satisfiability
  case solver.check(s) {
    Ok(Sat(model)) -> {
      // Extract values
      let x_val = model_get_value(model, "x")
      let y_val = model_get_value(model, "y")
      io.println("x = " <> value_to_string(x_val))
      io.println("y = " <> value_to_string(y_val))
    }
    Ok(Unsat) -> io.println("No solution exists")
    Ok(Unknown(reason)) -> io.println("Unknown: " <> reason)
    Error(err) -> io.println("Error: " <> error_to_string(err))
  }
}
```

## Type Safety Benefits

1. **Sort Checking**: Expression constructors encode sort constraints
2. **Exhaustive Matching**: Pattern matching on `CheckResult` ensures all cases handled
3. **No Null**: Option types instead of null pointers
4. **Immutable State**: Solver operations return new state

## Future Extensions

- **Tactics API**: For advanced proof strategies
- **Optimization**: `Z3_optimize` for optimization problems
- **Fixedpoint**: Datalog-style recursive queries
- **Interpolation**: Craig interpolants for verification
