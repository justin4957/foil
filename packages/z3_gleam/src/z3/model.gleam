//// Model Inspection Module
////
//// This module provides functions for inspecting satisfying models
//// returned by the Z3 solver. It allows extracting values for variables
//// and converting models to various formats.
////
//// ## Usage
////
//// ```gleam
//// import z3/model
//// import z3/solver
//// import z3/expr
////
//// pub fn example() {
////   let assert Ok(s) = solver.new()
////   let x = expr.int_const("x")
////   let assert Ok(s) = solver.assert_(s, expr.eq(x, expr.int(42)))
////
////   case solver.check(s) {
////     Ok(#(_, solver.SolverSat(m))) -> {
////       // Get value of x
////       case model.get_int(m, "x") {
////         Some(42) -> io.println("x = 42")
////         _ -> io.println("unexpected value")
////       }
////     }
////     _ -> io.println("not sat")
////   }
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import z3/solver.{type SolverModel, SolverModel}
import z3/types.{
  type Expr, type Model, type Value, BoolVal, Const, IntVal, Model, RealVal,
  UnknownVal,
}

// =============================================================================
// Value Extraction
// =============================================================================

/// Get the value of a variable from the model
pub fn get_value(model: SolverModel, name: String) -> Option(Value) {
  let SolverModel(values) = model
  dict.get(values, name)
  |> option.from_result
}

/// Get an integer value from the model
pub fn get_int(model: SolverModel, name: String) -> Option(Int) {
  case get_value(model, name) {
    Some(IntVal(i)) -> Some(i)
    _ -> None
  }
}

/// Get a boolean value from the model
pub fn get_bool(model: SolverModel, name: String) -> Option(Bool) {
  case get_value(model, name) {
    Some(BoolVal(b)) -> Some(b)
    _ -> None
  }
}

/// Get a real value from the model
pub fn get_real(model: SolverModel, name: String) -> Option(Float) {
  case get_value(model, name) {
    Some(RealVal(r)) -> Some(r)
    _ -> None
  }
}

/// Get the string representation of a value
pub fn get_string(model: SolverModel, name: String) -> Option(String) {
  case get_value(model, name) {
    Some(value) -> Some(value_to_string(value))
    None -> None
  }
}

// =============================================================================
// Model Conversion
// =============================================================================

/// Convert the model to a dictionary of variable names to values
pub fn to_dict(model: SolverModel) -> Dict(String, Value) {
  let SolverModel(values) = model
  values
}

/// Convert the model to a list of (name, value) pairs
pub fn to_list(model: SolverModel) -> List(#(String, Value)) {
  let SolverModel(values) = model
  dict.to_list(values)
}

/// Convert the model to a types.Model
pub fn to_types_model(model: SolverModel) -> Model {
  Model(values: to_list(model))
}

/// Create a SolverModel from a types.Model
pub fn from_types_model(model: Model) -> SolverModel {
  let Model(values) = model
  SolverModel(dict.from_list(values))
}

/// Convert the model to a string representation
pub fn to_string(model: SolverModel) -> String {
  let pairs = to_list(model)
  let formatted =
    list.map(pairs, fn(pair) {
      let #(name, value) = pair
      name <> " = " <> value_to_string(value)
    })
  string.join(formatted, "\n")
}

// =============================================================================
// Model Queries
// =============================================================================

/// Get all variable names in the model
pub fn get_variable_names(model: SolverModel) -> List(String) {
  let SolverModel(values) = model
  dict.keys(values)
}

/// Check if the model contains a value for a variable
pub fn has_variable(model: SolverModel, name: String) -> Bool {
  let SolverModel(values) = model
  dict.has_key(values, name)
}

/// Get the number of variables in the model
pub fn size(model: SolverModel) -> Int {
  let SolverModel(values) = model
  dict.size(values)
}

/// Check if the model is empty
pub fn is_empty(model: SolverModel) -> Bool {
  size(model) == 0
}

// =============================================================================
// Expression Evaluation
// =============================================================================

/// Evaluate an expression using the values in the model
/// Returns the evaluated expression (simplified where possible)
pub fn eval(model: SolverModel, expr: Expr) -> Expr {
  case expr {
    Const(name, sort) ->
      case get_value(model, name) {
        Some(value) -> value_to_expr(value)
        None -> Const(name, sort)
      }

    types.BoolLit(_) -> expr
    types.IntLit(_) -> expr
    types.RealLit(_, _) -> expr

    types.And(exprs) -> {
      let evaled = list.map(exprs, eval(model, _))
      simplify_and(evaled)
    }

    types.Or(exprs) -> {
      let evaled = list.map(exprs, eval(model, _))
      simplify_or(evaled)
    }

    types.Not(inner) -> {
      let evaled = eval(model, inner)
      simplify_not(evaled)
    }

    types.Implies(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_implies(evaled_a, evaled_b)
    }

    types.Iff(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_iff(evaled_a, evaled_b)
    }

    types.Ite(cond, then_b, else_b) -> {
      let evaled_cond = eval(model, cond)
      case evaled_cond {
        types.BoolLit(True) -> eval(model, then_b)
        types.BoolLit(False) -> eval(model, else_b)
        _ -> types.Ite(evaled_cond, eval(model, then_b), eval(model, else_b))
      }
    }

    types.Add(exprs) -> {
      let evaled = list.map(exprs, eval(model, _))
      simplify_add(evaled)
    }

    types.Mul(exprs) -> {
      let evaled = list.map(exprs, eval(model, _))
      simplify_mul(evaled)
    }

    types.Sub(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_sub(evaled_a, evaled_b)
    }

    types.Div(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_div(evaled_a, evaled_b)
    }

    types.Neg(inner) -> {
      let evaled = eval(model, inner)
      simplify_neg(evaled)
    }

    types.Eq(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_eq(evaled_a, evaled_b)
    }

    types.Lt(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_lt(evaled_a, evaled_b)
    }

    types.Le(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_le(evaled_a, evaled_b)
    }

    types.Gt(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_gt(evaled_a, evaled_b)
    }

    types.Ge(a, b) -> {
      let evaled_a = eval(model, a)
      let evaled_b = eval(model, b)
      simplify_ge(evaled_a, evaled_b)
    }

    types.ForAll(vars, body) -> types.ForAll(vars, eval(model, body))
    types.Exists(vars, body) -> types.Exists(vars, eval(model, body))
  }
}

/// Evaluate an expression and extract a boolean result
pub fn eval_bool(model: SolverModel, expr: Expr) -> Option(Bool) {
  case eval(model, expr) {
    types.BoolLit(b) -> Some(b)
    _ -> None
  }
}

/// Evaluate an expression and extract an integer result
pub fn eval_int(model: SolverModel, expr: Expr) -> Option(Int) {
  case eval(model, expr) {
    types.IntLit(i) -> Some(i)
    _ -> None
  }
}

// =============================================================================
// Model Construction
// =============================================================================

/// Create an empty model
pub fn empty() -> SolverModel {
  SolverModel(dict.new())
}

/// Create a model from a list of (name, value) pairs
pub fn from_list(values: List(#(String, Value))) -> SolverModel {
  SolverModel(dict.from_list(values))
}

/// Add a value to the model
pub fn set_value(model: SolverModel, name: String, value: Value) -> SolverModel {
  let SolverModel(values) = model
  SolverModel(dict.insert(values, name, value))
}

/// Add an integer value to the model
pub fn set_int(model: SolverModel, name: String, value: Int) -> SolverModel {
  set_value(model, name, IntVal(value))
}

/// Add a boolean value to the model
pub fn set_bool(model: SolverModel, name: String, value: Bool) -> SolverModel {
  set_value(model, name, BoolVal(value))
}

/// Add a real value to the model
pub fn set_real(model: SolverModel, name: String, value: Float) -> SolverModel {
  set_value(model, name, RealVal(value))
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Convert a Value to its string representation
pub fn value_to_string(value: Value) -> String {
  case value {
    BoolVal(True) -> "true"
    BoolVal(False) -> "false"
    IntVal(i) -> int.to_string(i)
    RealVal(r) -> float.to_string(r)
    UnknownVal(s) -> s
  }
}

/// Convert a Value to an expression
fn value_to_expr(value: Value) -> Expr {
  case value {
    BoolVal(b) -> types.BoolLit(b)
    IntVal(i) -> types.IntLit(i)
    RealVal(r) -> {
      // Convert float to rational approximation
      let int_part = float.truncate(r)
      types.RealLit(int_part, 1)
    }
    UnknownVal(_) -> types.IntLit(0)
  }
}

// =============================================================================
// Simplification Helpers
// =============================================================================

fn simplify_and(exprs: List(Expr)) -> Expr {
  // Check if any is false
  case list.any(exprs, fn(e) { e == types.BoolLit(False) }) {
    True -> types.BoolLit(False)
    False -> {
      // Filter out true values
      let filtered = list.filter(exprs, fn(e) { e != types.BoolLit(True) })
      case filtered {
        [] -> types.BoolLit(True)
        [single] -> single
        _ -> types.And(filtered)
      }
    }
  }
}

fn simplify_or(exprs: List(Expr)) -> Expr {
  // Check if any is true
  case list.any(exprs, fn(e) { e == types.BoolLit(True) }) {
    True -> types.BoolLit(True)
    False -> {
      // Filter out false values
      let filtered = list.filter(exprs, fn(e) { e != types.BoolLit(False) })
      case filtered {
        [] -> types.BoolLit(False)
        [single] -> single
        _ -> types.Or(filtered)
      }
    }
  }
}

fn simplify_not(expr: Expr) -> Expr {
  case expr {
    types.BoolLit(True) -> types.BoolLit(False)
    types.BoolLit(False) -> types.BoolLit(True)
    types.Not(inner) -> inner
    _ -> types.Not(expr)
  }
}

fn simplify_implies(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.BoolLit(False), _ -> types.BoolLit(True)
    types.BoolLit(True), _ -> b
    _, types.BoolLit(True) -> types.BoolLit(True)
    _, _ -> types.Implies(a, b)
  }
}

fn simplify_iff(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.BoolLit(True), _ -> b
    types.BoolLit(False), _ -> simplify_not(b)
    _, types.BoolLit(True) -> a
    _, types.BoolLit(False) -> simplify_not(a)
    _, _ -> types.Iff(a, b)
  }
}

fn simplify_add(exprs: List(Expr)) -> Expr {
  // Sum up all integer literals
  let #(literals, rest) =
    list.partition(exprs, fn(e) {
      case e {
        types.IntLit(_) -> True
        _ -> False
      }
    })

  let sum =
    list.fold(literals, 0, fn(acc, e) {
      case e {
        types.IntLit(i) -> acc + i
        _ -> acc
      }
    })

  case rest, sum {
    [], _ -> types.IntLit(sum)
    _, 0 ->
      case rest {
        [single] -> single
        _ -> types.Add(rest)
      }
    _, _ -> types.Add(list.append(rest, [types.IntLit(sum)]))
  }
}

fn simplify_mul(exprs: List(Expr)) -> Expr {
  // Check for zero
  case list.any(exprs, fn(e) { e == types.IntLit(0) }) {
    True -> types.IntLit(0)
    False -> {
      // Multiply all integer literals
      let #(literals, rest) =
        list.partition(exprs, fn(e) {
          case e {
            types.IntLit(_) -> True
            _ -> False
          }
        })

      let product =
        list.fold(literals, 1, fn(acc, e) {
          case e {
            types.IntLit(i) -> acc * i
            _ -> acc
          }
        })

      case rest, product {
        [], _ -> types.IntLit(product)
        _, 1 ->
          case rest {
            [single] -> single
            _ -> types.Mul(rest)
          }
        _, _ -> types.Mul(list.append(rest, [types.IntLit(product)]))
      }
    }
  }
}

fn simplify_sub(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.IntLit(x), types.IntLit(y) -> types.IntLit(x - y)
    _, types.IntLit(0) -> a
    _, _ -> types.Sub(a, b)
  }
}

fn simplify_div(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.IntLit(x), types.IntLit(y) if y != 0 -> types.IntLit(x / y)
    _, types.IntLit(1) -> a
    _, _ -> types.Div(a, b)
  }
}

fn simplify_neg(expr: Expr) -> Expr {
  case expr {
    types.IntLit(i) -> types.IntLit(-i)
    types.Neg(inner) -> inner
    _ -> types.Neg(expr)
  }
}

fn simplify_eq(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.IntLit(x), types.IntLit(y) -> types.BoolLit(x == y)
    types.BoolLit(x), types.BoolLit(y) -> types.BoolLit(x == y)
    _, _ -> types.Eq(a, b)
  }
}

fn simplify_lt(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.IntLit(x), types.IntLit(y) -> types.BoolLit(x < y)
    _, _ -> types.Lt(a, b)
  }
}

fn simplify_le(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.IntLit(x), types.IntLit(y) -> types.BoolLit(x <= y)
    _, _ -> types.Le(a, b)
  }
}

fn simplify_gt(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.IntLit(x), types.IntLit(y) -> types.BoolLit(x > y)
    _, _ -> types.Gt(a, b)
  }
}

fn simplify_ge(a: Expr, b: Expr) -> Expr {
  case a, b {
    types.IntLit(x), types.IntLit(y) -> types.BoolLit(x >= y)
    _, _ -> types.Ge(a, b)
  }
}
