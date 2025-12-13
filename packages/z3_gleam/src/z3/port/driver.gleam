//// Port-based Z3 driver implementation
////
//// This module provides the Gleam side of the Port-based Z3 integration.
//// It communicates with a Python driver process via JSON over stdin/stdout.
////
//// ## Usage Example (Theoretical - requires z3-solver Python package)
////
//// ```gleam
//// import z3/port/driver
//// import z3/types.{Const, IntSort, Gt, IntLit, And}
////
//// pub fn main() {
////   // Start the Z3 port driver
////   let assert Ok(handle) = driver.start()
////
////   // Create a context and solver
////   let assert Ok(#(handle, ctx)) = driver.new_context(handle)
////   let assert Ok(#(handle, solver)) = driver.new_solver(handle, ctx)
////
////   // Add constraints: x > 0 AND x < 10
////   let x = Const("x", IntSort)
////   let constraint = And([Gt(x, IntLit(0)), types.Lt(x, IntLit(10))])
////   let assert Ok(handle) = driver.assert_constraint(handle, solver, constraint)
////
////   // Check satisfiability
////   let assert Ok(#(handle, result)) = driver.check(handle, solver)
////
////   // Clean up
////   driver.stop(handle)
//// }
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import z3/types.{
  type CheckResult, type Expr, type Sort, type Value, type Z3Error, Add, And,
  BoolLit, BoolSort, Const, Div, Eq, Exists, ForAll, Ge, Gt, Iff, Implies,
  IntLit, IntSort, Ite, Le, Lt, Mul, Neg, Not, Or, ParseError, PortError,
  RealSort, Sat, Sub, Unknown, Unsat,
}

// =============================================================================
// Port Handle Types
// =============================================================================

/// Handle to a running Z3 port driver
pub opaque type PortHandle {
  PortHandle(port: Port, next_id: Int)
}

/// Context handle (ID from the port)
pub opaque type ContextHandle {
  ContextHandle(id: Int)
}

/// Solver handle (ID from the port)
pub opaque type SolverHandle {
  SolverHandle(id: Int, context_id: Int)
}

/// Model handle (stores model data)
pub opaque type ModelHandle {
  ModelHandle(values: List(#(String, Value)))
}

/// Erlang port type
pub type Port

// =============================================================================
// Port Lifecycle
// =============================================================================

/// Start the Z3 port driver
pub fn start() -> Result(PortHandle, Z3Error) {
  let driver_path = get_driver_path()

  case open_port(driver_path) {
    Ok(port) -> {
      // Wait for ready signal
      case receive_ready(port) {
        Ok(_) -> Ok(PortHandle(port: port, next_id: 1))
        Error(err) -> Error(err)
      }
    }
    Error(reason) -> Error(PortError("Failed to start port: " <> reason))
  }
}

/// Stop the Z3 port driver
pub fn stop(handle: PortHandle) -> Nil {
  close_port(handle.port)
}

/// Ping the port to check it's alive
pub fn ping(handle: PortHandle) -> Result(PortHandle, Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([#("id", json.int(id)), #("cmd", json.string("ping"))])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(_) -> Ok(new_handle)
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// Context Operations
// =============================================================================

/// Create a new Z3 context
pub fn new_context(
  handle: PortHandle,
) -> Result(#(PortHandle, ContextHandle), Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([#("id", json.int(id)), #("cmd", json.string("new_context"))])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(response) ->
          case decode_context_id(response) {
            Ok(ctx_id) -> Ok(#(new_handle, ContextHandle(id: ctx_id)))
            Error(err) -> Error(err)
          }
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

/// Delete a context
pub fn delete_context(
  handle: PortHandle,
  ctx: ContextHandle,
) -> Result(PortHandle, Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("del_context")),
      #("context_id", json.int(ctx.id)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(_) -> Ok(new_handle)
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// Solver Operations
// =============================================================================

/// Create a new solver for a context
pub fn new_solver(
  handle: PortHandle,
  ctx: ContextHandle,
) -> Result(#(PortHandle, SolverHandle), Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("new_solver")),
      #("context_id", json.int(ctx.id)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(response) ->
          case decode_solver_id(response) {
            Ok(solver_id) ->
              Ok(#(new_handle, SolverHandle(id: solver_id, context_id: ctx.id)))
            Error(err) -> Error(err)
          }
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

/// Assert a constraint
pub fn assert_constraint(
  handle: PortHandle,
  solver: SolverHandle,
  expr: Expr,
) -> Result(PortHandle, Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("assert")),
      #("solver_id", json.int(solver.id)),
      #("context_id", json.int(solver.context_id)),
      #("expr", expr_to_json(expr)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(_) -> Ok(new_handle)
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

/// Check satisfiability
pub fn check(
  handle: PortHandle,
  solver: SolverHandle,
) -> Result(#(PortHandle, CheckResult), Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("check")),
      #("solver_id", json.int(solver.id)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(response) ->
          case decode_check_result(response, new_handle, solver) {
            Ok(#(h, result)) -> Ok(#(h, result))
            Error(err) -> Error(err)
          }
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

/// Push a scope for incremental solving
pub fn push(
  handle: PortHandle,
  solver: SolverHandle,
) -> Result(PortHandle, Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("push")),
      #("solver_id", json.int(solver.id)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(_) -> Ok(new_handle)
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

/// Pop n scopes
pub fn pop(
  handle: PortHandle,
  solver: SolverHandle,
  n: Int,
) -> Result(PortHandle, Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("pop")),
      #("solver_id", json.int(solver.id)),
      #("n", json.int(n)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(_) -> Ok(new_handle)
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

/// Reset the solver
pub fn reset(
  handle: PortHandle,
  solver: SolverHandle,
) -> Result(PortHandle, Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("reset")),
      #("solver_id", json.int(solver.id)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(_) -> Ok(new_handle)
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// Model Operations
// =============================================================================

/// Get the model from a sat result
pub fn get_model(
  handle: PortHandle,
  solver: SolverHandle,
) -> Result(#(PortHandle, ModelHandle), Z3Error) {
  let #(new_handle, id) = next_request_id(handle)
  let request =
    json.object([
      #("id", json.int(id)),
      #("cmd", json.string("get_model")),
      #("solver_id", json.int(solver.id)),
    ])

  case send_request(new_handle.port, request) {
    Ok(_) ->
      case receive_response(new_handle.port) {
        Ok(_response) -> {
          // For now, return empty model - full implementation would decode all values
          Ok(#(new_handle, ModelHandle(values: [])))
        }
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

/// Get a value from the model by variable name
pub fn model_get_value(model: ModelHandle, name: String) -> Option(Value) {
  list.find(model.values, fn(pair) { pair.0 == name })
  |> result.map(fn(pair) { pair.1 })
  |> option.from_result
}

/// Get all values from the model
pub fn model_values(model: ModelHandle) -> List(#(String, Value)) {
  model.values
}

// =============================================================================
// Expression to JSON
// =============================================================================

fn expr_to_json(expr: Expr) -> json.Json {
  case expr {
    BoolLit(value) ->
      json.object([
        #("type", json.string("bool_lit")),
        #("value", json.bool(value)),
      ])

    IntLit(value) ->
      json.object([
        #("type", json.string("int_lit")),
        #("value", json.int(value)),
      ])

    types.RealLit(num, denom) ->
      json.object([
        #("type", json.string("real_lit")),
        #("numerator", json.int(num)),
        #("denominator", json.int(denom)),
      ])

    Const(name, sort) ->
      json.object([
        #("type", json.string("const")),
        #("name", json.string(name)),
        #("sort", sort_to_json(sort)),
      ])

    And(exprs) ->
      json.object([
        #("type", json.string("and")),
        #("exprs", json.array(exprs, expr_to_json)),
      ])

    Or(exprs) ->
      json.object([
        #("type", json.string("or")),
        #("exprs", json.array(exprs, expr_to_json)),
      ])

    Not(inner) ->
      json.object([
        #("type", json.string("not")),
        #("expr", expr_to_json(inner)),
      ])

    Implies(a, b) ->
      json.object([
        #("type", json.string("implies")),
        #("antecedent", expr_to_json(a)),
        #("consequent", expr_to_json(b)),
      ])

    Iff(a, b) ->
      json.object([
        #("type", json.string("iff")),
        #("left", expr_to_json(a)),
        #("right", expr_to_json(b)),
      ])

    Add(exprs) ->
      json.object([
        #("type", json.string("add")),
        #("exprs", json.array(exprs, expr_to_json)),
      ])

    Sub(a, b) ->
      json.object([
        #("type", json.string("sub")),
        #("left", expr_to_json(a)),
        #("right", expr_to_json(b)),
      ])

    Mul(exprs) ->
      json.object([
        #("type", json.string("mul")),
        #("exprs", json.array(exprs, expr_to_json)),
      ])

    Div(a, b) ->
      json.object([
        #("type", json.string("div")),
        #("numerator", expr_to_json(a)),
        #("denominator", expr_to_json(b)),
      ])

    Neg(inner) ->
      json.object([
        #("type", json.string("neg")),
        #("expr", expr_to_json(inner)),
      ])

    Eq(a, b) ->
      json.object([
        #("type", json.string("eq")),
        #("left", expr_to_json(a)),
        #("right", expr_to_json(b)),
      ])

    Lt(a, b) ->
      json.object([
        #("type", json.string("lt")),
        #("left", expr_to_json(a)),
        #("right", expr_to_json(b)),
      ])

    Le(a, b) ->
      json.object([
        #("type", json.string("le")),
        #("left", expr_to_json(a)),
        #("right", expr_to_json(b)),
      ])

    Gt(a, b) ->
      json.object([
        #("type", json.string("gt")),
        #("left", expr_to_json(a)),
        #("right", expr_to_json(b)),
      ])

    Ge(a, b) ->
      json.object([
        #("type", json.string("ge")),
        #("left", expr_to_json(a)),
        #("right", expr_to_json(b)),
      ])

    ForAll(vars, body) ->
      json.object([
        #("type", json.string("forall")),
        #("vars", json.array(vars, var_to_json)),
        #("body", expr_to_json(body)),
      ])

    Exists(vars, body) ->
      json.object([
        #("type", json.string("exists")),
        #("vars", json.array(vars, var_to_json)),
        #("body", expr_to_json(body)),
      ])

    Ite(cond, then_branch, else_branch) ->
      json.object([
        #("type", json.string("ite")),
        #("condition", expr_to_json(cond)),
        #("then", expr_to_json(then_branch)),
        #("else", expr_to_json(else_branch)),
      ])
  }
}

fn sort_to_json(sort: Sort) -> json.Json {
  case sort {
    BoolSort -> json.string("bool")
    IntSort -> json.string("int")
    RealSort -> json.string("real")
    types.UninterpretedSort(name) ->
      json.object([
        #("type", json.string("uninterpreted")),
        #("name", json.string(name)),
      ])
    types.ArraySort(domain, range) ->
      json.object([
        #("type", json.string("array")),
        #("domain", sort_to_json(domain)),
        #("range", sort_to_json(range)),
      ])
  }
}

fn var_to_json(var: #(String, Sort)) -> json.Json {
  let #(name, sort) = var
  json.array([json.string(name), sort_to_json(sort)], fn(x) { x })
}

// =============================================================================
// Response Decoding (using Erlang FFI for compatibility)
// =============================================================================

fn decode_context_id(response: Dynamic) -> Result(Int, Z3Error) {
  case ffi_get_int_field(response, "context_id") {
    Ok(id) -> Ok(id)
    Error(_) -> Error(ParseError("Failed to decode context_id"))
  }
}

fn decode_solver_id(response: Dynamic) -> Result(Int, Z3Error) {
  case ffi_get_int_field(response, "solver_id") {
    Ok(id) -> Ok(id)
    Error(_) -> Error(ParseError("Failed to decode solver_id"))
  }
}

fn decode_check_result(
  response: Dynamic,
  handle: PortHandle,
  solver: SolverHandle,
) -> Result(#(PortHandle, CheckResult), Z3Error) {
  case ffi_get_string_field(response, "result") {
    Ok("sat") -> {
      // Get the model
      case get_model(handle, solver) {
        Ok(#(new_handle, _model)) -> {
          // Return with a placeholder Model (empty values for now)
          Ok(#(new_handle, Sat(types.Model(values: []))))
        }
        Error(err) -> Error(err)
      }
    }
    Ok("unsat") -> Ok(#(handle, Unsat))
    Ok("unknown") -> {
      let reason =
        ffi_get_string_field(response, "reason")
        |> result.unwrap("Unknown reason")
      Ok(#(handle, Unknown(reason)))
    }
    Ok(other) -> Error(ParseError("Unknown check result: " <> other))
    Error(_) -> Error(ParseError("Failed to decode check result"))
  }
}

// =============================================================================
// Port Communication
// =============================================================================

fn next_request_id(handle: PortHandle) -> #(PortHandle, Int) {
  let id = handle.next_id
  let new_handle = PortHandle(port: handle.port, next_id: id + 1)
  #(new_handle, id)
}

fn get_driver_path() -> String {
  // Get the path to the Python driver
  // In production, this would be in priv/port/z3_driver.py
  "python3 priv/port/z3_driver.py"
}

// Erlang port functions (external)
@external(erlang, "z3_port_ffi", "open_port")
fn open_port(command: String) -> Result(Port, String)

@external(erlang, "z3_port_ffi", "close_port")
fn close_port(port: Port) -> Nil

@external(erlang, "z3_port_ffi", "send_request")
fn send_request(port: Port, request: json.Json) -> Result(Nil, Z3Error)

@external(erlang, "z3_port_ffi", "receive_response")
fn receive_response(port: Port) -> Result(Dynamic, Z3Error)

@external(erlang, "z3_port_ffi", "receive_ready")
fn receive_ready(port: Port) -> Result(Nil, Z3Error)

// FFI helpers for dynamic field access
@external(erlang, "z3_port_ffi", "get_int_field")
fn ffi_get_int_field(data: Dynamic, field: String) -> Result(Int, Nil)

@external(erlang, "z3_port_ffi", "get_string_field")
fn ffi_get_string_field(data: Dynamic, field: String) -> Result(String, Nil)
