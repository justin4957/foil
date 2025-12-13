//// Expression Builder Module
////
//// This module provides an idiomatic Gleam API for building Z3 expressions.
//// Expressions are pure Gleam data structures that can be compiled to Z3
//// internal representation separately.
////
//// ## Usage
////
//// ```gleam
//// import z3/expr
////
//// // Create variables
//// let x = expr.int_const("x")
//// let y = expr.int_const("y")
////
//// // Build expressions
//// let constraint = expr.and_([
////   expr.gt(x, expr.int(0)),
////   expr.lt(y, expr.int(10)),
////   expr.eq(expr.add([x, y]), expr.int(15)),
//// ])
//// ```

import gleam/list
import z3/types.{
  type Expr, type Sort, Add, And, ArraySort, BoolLit, BoolSort, Const, Div, Eq,
  Exists, ForAll, Ge, Gt, Iff, Implies, IntLit, IntSort, Ite, Le, Lt, Mul, Neg,
  Not, Or, RealLit, RealSort, Sub, UninterpretedSort,
}

// =============================================================================
// Sort Constructors
// =============================================================================

/// Create a boolean sort
pub fn bool_sort() -> Sort {
  BoolSort
}

/// Create an integer sort
pub fn int_sort() -> Sort {
  IntSort
}

/// Create a real number sort
pub fn real_sort() -> Sort {
  RealSort
}

/// Create an uninterpreted sort with the given name
pub fn uninterpreted_sort(name: String) -> Sort {
  UninterpretedSort(name)
}

/// Create an array sort with given domain and range sorts
pub fn array_sort(domain: Sort, range: Sort) -> Sort {
  ArraySort(domain, range)
}

// =============================================================================
// Literal Constructors
// =============================================================================

/// Create a boolean literal
pub fn bool(value: Bool) -> Expr {
  BoolLit(value)
}

/// Create a true literal
pub fn true_() -> Expr {
  BoolLit(True)
}

/// Create a false literal
pub fn false_() -> Expr {
  BoolLit(False)
}

/// Create an integer literal
pub fn int(value: Int) -> Expr {
  IntLit(value)
}

/// Create a real literal from numerator and denominator
pub fn real(numerator: Int, denominator: Int) -> Expr {
  RealLit(numerator, denominator)
}

/// Create a real literal from an integer (denominator = 1)
pub fn real_from_int(value: Int) -> Expr {
  RealLit(value, 1)
}

// =============================================================================
// Variable/Constant Constructors
// =============================================================================

/// Create a constant (variable) with the given name and sort
pub fn const_(name: String, sort: Sort) -> Expr {
  Const(name, sort)
}

/// Create a boolean constant (variable)
pub fn bool_const(name: String) -> Expr {
  Const(name, BoolSort)
}

/// Create an integer constant (variable)
pub fn int_const(name: String) -> Expr {
  Const(name, IntSort)
}

/// Create a real constant (variable)
pub fn real_const(name: String) -> Expr {
  Const(name, RealSort)
}

// =============================================================================
// Boolean Operations
// =============================================================================

/// Create a conjunction (AND) of expressions
/// Returns true if the list is empty
pub fn and_(exprs: List(Expr)) -> Expr {
  case exprs {
    [] -> BoolLit(True)
    [single] -> single
    _ -> And(exprs)
  }
}

/// Create a conjunction of two expressions
pub fn and2(left: Expr, right: Expr) -> Expr {
  And([left, right])
}

/// Create a disjunction (OR) of expressions
/// Returns false if the list is empty
pub fn or_(exprs: List(Expr)) -> Expr {
  case exprs {
    [] -> BoolLit(False)
    [single] -> single
    _ -> Or(exprs)
  }
}

/// Create a disjunction of two expressions
pub fn or2(left: Expr, right: Expr) -> Expr {
  Or([left, right])
}

/// Create a negation (NOT) of an expression
pub fn not_(expr: Expr) -> Expr {
  Not(expr)
}

/// Create an implication (left => right)
pub fn implies(antecedent: Expr, consequent: Expr) -> Expr {
  Implies(antecedent, consequent)
}

/// Create a bi-conditional (left <=> right)
pub fn iff(left: Expr, right: Expr) -> Expr {
  Iff(left, right)
}

/// Create an exclusive or (XOR) of two expressions
/// Defined as (a OR b) AND NOT (a AND b)
pub fn xor(left: Expr, right: Expr) -> Expr {
  And([Or([left, right]), Not(And([left, right]))])
}

// =============================================================================
// Conditional Expression
// =============================================================================

/// Create an if-then-else expression
pub fn ite(condition: Expr, then_branch: Expr, else_branch: Expr) -> Expr {
  Ite(condition, then_branch, else_branch)
}

// =============================================================================
// Arithmetic Operations
// =============================================================================

/// Create a sum of expressions
/// Returns 0 if the list is empty
pub fn add(exprs: List(Expr)) -> Expr {
  case exprs {
    [] -> IntLit(0)
    [single] -> single
    _ -> Add(exprs)
  }
}

/// Create a sum of two expressions
pub fn add2(left: Expr, right: Expr) -> Expr {
  Add([left, right])
}

/// Create a subtraction (left - right)
pub fn sub(left: Expr, right: Expr) -> Expr {
  Sub(left, right)
}

/// Create a product of expressions
/// Returns 1 if the list is empty
pub fn mul(exprs: List(Expr)) -> Expr {
  case exprs {
    [] -> IntLit(1)
    [single] -> single
    _ -> Mul(exprs)
  }
}

/// Create a product of two expressions
pub fn mul2(left: Expr, right: Expr) -> Expr {
  Mul([left, right])
}

/// Create a division (numerator / denominator)
pub fn div(numerator: Expr, denominator: Expr) -> Expr {
  Div(numerator, denominator)
}

/// Create the negation of an expression (-expr)
pub fn neg(expr: Expr) -> Expr {
  Neg(expr)
}

/// Create the absolute value of an expression
/// Defined as: if expr >= 0 then expr else -expr
pub fn abs(expr: Expr) -> Expr {
  Ite(Ge(expr, IntLit(0)), expr, Neg(expr))
}

/// Create minimum of two expressions
/// Defined as: if left <= right then left else right
pub fn min(left: Expr, right: Expr) -> Expr {
  Ite(Le(left, right), left, right)
}

/// Create maximum of two expressions
/// Defined as: if left >= right then left else right
pub fn max(left: Expr, right: Expr) -> Expr {
  Ite(Ge(left, right), left, right)
}

// =============================================================================
// Comparison Operations
// =============================================================================

/// Create an equality comparison (left == right)
pub fn eq(left: Expr, right: Expr) -> Expr {
  Eq(left, right)
}

/// Create a not-equal comparison (left != right)
pub fn neq(left: Expr, right: Expr) -> Expr {
  Not(Eq(left, right))
}

/// Create a less-than comparison (left < right)
pub fn lt(left: Expr, right: Expr) -> Expr {
  Lt(left, right)
}

/// Create a less-than-or-equal comparison (left <= right)
pub fn le(left: Expr, right: Expr) -> Expr {
  Le(left, right)
}

/// Create a greater-than comparison (left > right)
pub fn gt(left: Expr, right: Expr) -> Expr {
  Gt(left, right)
}

/// Create a greater-than-or-equal comparison (left >= right)
pub fn ge(left: Expr, right: Expr) -> Expr {
  Ge(left, right)
}

// =============================================================================
// Quantifiers
// =============================================================================

/// Create a universal quantifier (forall vars. body)
pub fn forall(vars: List(#(String, Sort)), body: Expr) -> Expr {
  ForAll(vars, body)
}

/// Create a universal quantifier with a single integer variable
pub fn forall_int(var_name: String, body: Expr) -> Expr {
  ForAll([#(var_name, IntSort)], body)
}

/// Create a universal quantifier with a single boolean variable
pub fn forall_bool(var_name: String, body: Expr) -> Expr {
  ForAll([#(var_name, BoolSort)], body)
}

/// Create an existential quantifier (exists vars. body)
pub fn exists(vars: List(#(String, Sort)), body: Expr) -> Expr {
  Exists(vars, body)
}

/// Create an existential quantifier with a single integer variable
pub fn exists_int(var_name: String, body: Expr) -> Expr {
  Exists([#(var_name, IntSort)], body)
}

/// Create an existential quantifier with a single boolean variable
pub fn exists_bool(var_name: String, body: Expr) -> Expr {
  Exists([#(var_name, BoolSort)], body)
}

// =============================================================================
// Convenience Functions
// =============================================================================

/// Create a constraint that a list of expressions are all distinct
/// distinct([a, b, c]) = (a != b) AND (a != c) AND (b != c)
pub fn distinct(exprs: List(Expr)) -> Expr {
  let pairs = list_pairs(exprs)
  let constraints =
    list.map(pairs, fn(pair) {
      let #(left, right) = pair
      Not(Eq(left, right))
    })
  and_(constraints)
}

/// Create a constraint that exactly one expression is true
pub fn exactly_one(exprs: List(Expr)) -> Expr {
  // At least one is true
  let at_least_one = or_(exprs)
  // At most one is true (no two are both true)
  let pairs = list_pairs(exprs)
  let at_most_one =
    and_(
      list.map(pairs, fn(pair) {
        let #(left, right) = pair
        Not(And([left, right]))
      }),
    )
  And([at_least_one, at_most_one])
}

/// Create a constraint that at most one expression is true
pub fn at_most_one(exprs: List(Expr)) -> Expr {
  let pairs = list_pairs(exprs)
  and_(
    list.map(pairs, fn(pair) {
      let #(left, right) = pair
      Not(And([left, right]))
    }),
  )
}

/// Create a constraint that at least one expression is true
pub fn at_least_one(exprs: List(Expr)) -> Expr {
  or_(exprs)
}

/// Create a range constraint: lower <= expr <= upper
pub fn in_range(expr: Expr, lower: Expr, upper: Expr) -> Expr {
  And([Ge(expr, lower), Le(expr, upper)])
}

/// Create a strict range constraint: lower < expr < upper
pub fn in_range_exclusive(expr: Expr, lower: Expr, upper: Expr) -> Expr {
  And([Gt(expr, lower), Lt(expr, upper)])
}

// =============================================================================
// Expression Utilities
// =============================================================================

/// Substitute a variable with an expression in the given expression
/// This is a simple syntactic substitution
pub fn substitute(expr: Expr, var_name: String, replacement: Expr) -> Expr {
  case expr {
    Const(name, _) if name == var_name -> replacement
    Const(_, _) -> expr
    BoolLit(_) -> expr
    IntLit(_) -> expr
    RealLit(_, _) -> expr
    And(exprs) -> And(list.map(exprs, substitute(_, var_name, replacement)))
    Or(exprs) -> Or(list.map(exprs, substitute(_, var_name, replacement)))
    Not(inner) -> Not(substitute(inner, var_name, replacement))
    Implies(a, b) ->
      Implies(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Iff(a, b) ->
      Iff(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Ite(cond, then_b, else_b) ->
      Ite(
        substitute(cond, var_name, replacement),
        substitute(then_b, var_name, replacement),
        substitute(else_b, var_name, replacement),
      )
    Add(exprs) -> Add(list.map(exprs, substitute(_, var_name, replacement)))
    Mul(exprs) -> Mul(list.map(exprs, substitute(_, var_name, replacement)))
    Sub(a, b) ->
      Sub(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Div(a, b) ->
      Div(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Neg(inner) -> Neg(substitute(inner, var_name, replacement))
    Eq(a, b) ->
      Eq(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Lt(a, b) ->
      Lt(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Le(a, b) ->
      Le(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Gt(a, b) ->
      Gt(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    Ge(a, b) ->
      Ge(
        substitute(a, var_name, replacement),
        substitute(b, var_name, replacement),
      )
    ForAll(vars, body) ->
      // Don't substitute if var_name is bound
      case list.any(vars, fn(v) { v.0 == var_name }) {
        True -> expr
        False -> ForAll(vars, substitute(body, var_name, replacement))
      }
    Exists(vars, body) ->
      // Don't substitute if var_name is bound
      case list.any(vars, fn(v) { v.0 == var_name }) {
        True -> expr
        False -> Exists(vars, substitute(body, var_name, replacement))
      }
  }
}

/// Get all free variable names in an expression
pub fn free_variables(expr: Expr) -> List(String) {
  free_vars_helper(expr, [])
  |> list.unique
}

// =============================================================================
// Internal Helper Functions
// =============================================================================

fn free_vars_helper(expr: Expr, bound: List(String)) -> List(String) {
  case expr {
    Const(name, _) ->
      case list.contains(bound, name) {
        True -> []
        False -> [name]
      }
    BoolLit(_) -> []
    IntLit(_) -> []
    RealLit(_, _) -> []
    And(exprs) -> list.flat_map(exprs, free_vars_helper(_, bound))
    Or(exprs) -> list.flat_map(exprs, free_vars_helper(_, bound))
    Not(inner) -> free_vars_helper(inner, bound)
    Implies(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Iff(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Ite(cond, then_b, else_b) ->
      list.flatten([
        free_vars_helper(cond, bound),
        free_vars_helper(then_b, bound),
        free_vars_helper(else_b, bound),
      ])
    Add(exprs) -> list.flat_map(exprs, free_vars_helper(_, bound))
    Mul(exprs) -> list.flat_map(exprs, free_vars_helper(_, bound))
    Sub(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Div(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Neg(inner) -> free_vars_helper(inner, bound)
    Eq(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Lt(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Le(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Gt(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    Ge(a, b) ->
      list.append(free_vars_helper(a, bound), free_vars_helper(b, bound))
    ForAll(vars, body) -> {
      let new_bound = list.append(bound, list.map(vars, fn(v) { v.0 }))
      free_vars_helper(body, new_bound)
    }
    Exists(vars, body) -> {
      let new_bound = list.append(bound, list.map(vars, fn(v) { v.0 }))
      free_vars_helper(body, new_bound)
    }
  }
}

/// Generate all pairs from a list (combinations of 2)
fn list_pairs(items: List(a)) -> List(#(a, a)) {
  case items {
    [] -> []
    [_] -> []
    [first, ..rest] -> {
      let pairs_with_first = list.map(rest, fn(item) { #(first, item) })
      list.append(pairs_with_first, list_pairs(rest))
    }
  }
}
