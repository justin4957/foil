//// Expression Compiler Module
////
//// This module compiles Gleam expressions to Z3's internal representation.
//// It handles variable declaration tracking and produces JSON suitable for
//// the port driver protocol.
////
//// ## Compilation Process
////
//// 1. Traverse the Gleam Expr tree
//// 2. Track variable declarations and sorts
//// 3. Generate Z3-compatible JSON representation
//// 4. Handle special cases like quantifiers
////
//// ## Usage
////
//// ```gleam
//// import z3/compiler
//// import z3/expr
////
//// // Build an expression
//// let x = expr.int_const("x")
//// let constraint = expr.gt(x, expr.int(0))
////
//// // Compile to JSON
//// let json = compiler.compile(constraint)
//// ```

import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import z3/types.{
  type Expr, type Sort, type Z3Error, Add, And, ArraySort, BoolLit, BoolSort,
  Const, Div, Eq, Exists, ForAll, Ge, Gt, Iff, Implies, IntLit, IntSort, Ite, Le,
  Lt, Mul, Neg, Not, Or, RealLit, RealSort, Sub, UninterpretedSort,
}

// =============================================================================
// Compiler Types
// =============================================================================

/// Compilation context tracking variables and their sorts
pub type CompilerContext {
  CompilerContext(
    /// Map of variable names to their sorts
    variables: Dict(String, Sort),
    /// Counter for generating fresh variable names
    fresh_counter: Int,
  )
}

/// Result of compilation
pub type CompileResult {
  CompileResult(
    /// The compiled JSON representation
    json: Json,
    /// Updated context with any new variable declarations
    context: CompilerContext,
  )
}

// =============================================================================
// Context Management
// =============================================================================

/// Create a new empty compiler context
pub fn new_context() -> CompilerContext {
  CompilerContext(variables: dict.new(), fresh_counter: 0)
}

/// Add a variable to the context
pub fn add_variable(
  ctx: CompilerContext,
  name: String,
  sort: Sort,
) -> CompilerContext {
  CompilerContext(..ctx, variables: dict.insert(ctx.variables, name, sort))
}

/// Get a variable's sort from the context
pub fn get_variable_sort(ctx: CompilerContext, name: String) -> Option(Sort) {
  dict.get(ctx.variables, name)
  |> option.from_result
}

/// Generate a fresh variable name
pub fn fresh_name(
  ctx: CompilerContext,
  prefix: String,
) -> #(String, CompilerContext) {
  let name = prefix <> "_" <> int_to_string(ctx.fresh_counter)
  let new_ctx = CompilerContext(..ctx, fresh_counter: ctx.fresh_counter + 1)
  #(name, new_ctx)
}

/// Get all declared variables
pub fn get_variables(ctx: CompilerContext) -> Dict(String, Sort) {
  ctx.variables
}

// =============================================================================
// Expression Compilation
// =============================================================================

/// Compile an expression to JSON with a fresh context
pub fn compile(expr: Expr) -> Result(Json, Z3Error) {
  let ctx = new_context()
  case compile_with_context(expr, ctx) {
    Ok(result) -> Ok(result.json)
    Error(e) -> Error(e)
  }
}

/// Compile an expression to JSON with an existing context
pub fn compile_with_context(
  expr: Expr,
  ctx: CompilerContext,
) -> Result(CompileResult, Z3Error) {
  case expr {
    BoolLit(value) ->
      Ok(CompileResult(
        json: json.object([
          #("type", json.string("bool_lit")),
          #("value", json.bool(value)),
        ]),
        context: ctx,
      ))

    IntLit(value) ->
      Ok(CompileResult(
        json: json.object([
          #("type", json.string("int_lit")),
          #("value", json.int(value)),
        ]),
        context: ctx,
      ))

    RealLit(num, denom) ->
      Ok(CompileResult(
        json: json.object([
          #("type", json.string("real_lit")),
          #("numerator", json.int(num)),
          #("denominator", json.int(denom)),
        ]),
        context: ctx,
      ))

    Const(name, sort) -> {
      let new_ctx = add_variable(ctx, name, sort)
      Ok(CompileResult(
        json: json.object([
          #("type", json.string("const")),
          #("name", json.string(name)),
          #("sort", compile_sort(sort)),
        ]),
        context: new_ctx,
      ))
    }

    And(exprs) -> compile_list_expr("and", "exprs", exprs, ctx)

    Or(exprs) -> compile_list_expr("or", "exprs", exprs, ctx)

    Not(inner) -> {
      use inner_result <- result.try(compile_with_context(inner, ctx))
      Ok(CompileResult(
        json: json.object([
          #("type", json.string("not")),
          #("expr", inner_result.json),
        ]),
        context: inner_result.context,
      ))
    }

    Implies(a, b) ->
      compile_binary_expr("implies", "antecedent", "consequent", a, b, ctx)

    Iff(a, b) -> compile_binary_expr("iff", "left", "right", a, b, ctx)

    Ite(cond, then_b, else_b) -> {
      use cond_result <- result.try(compile_with_context(cond, ctx))
      use then_result <- result.try(compile_with_context(
        then_b,
        cond_result.context,
      ))
      use else_result <- result.try(compile_with_context(
        else_b,
        then_result.context,
      ))
      Ok(CompileResult(
        json: json.object([
          #("type", json.string("ite")),
          #("condition", cond_result.json),
          #("then", then_result.json),
          #("else", else_result.json),
        ]),
        context: else_result.context,
      ))
    }

    Add(exprs) -> compile_list_expr("add", "exprs", exprs, ctx)

    Mul(exprs) -> compile_list_expr("mul", "exprs", exprs, ctx)

    Sub(a, b) -> compile_binary_expr("sub", "left", "right", a, b, ctx)

    Div(a, b) ->
      compile_binary_expr("div", "numerator", "denominator", a, b, ctx)

    Neg(inner) -> {
      use inner_result <- result.try(compile_with_context(inner, ctx))
      Ok(CompileResult(
        json: json.object([
          #("type", json.string("neg")),
          #("expr", inner_result.json),
        ]),
        context: inner_result.context,
      ))
    }

    Eq(a, b) -> compile_binary_expr("eq", "left", "right", a, b, ctx)

    Lt(a, b) -> compile_binary_expr("lt", "left", "right", a, b, ctx)

    Le(a, b) -> compile_binary_expr("le", "left", "right", a, b, ctx)

    Gt(a, b) -> compile_binary_expr("gt", "left", "right", a, b, ctx)

    Ge(a, b) -> compile_binary_expr("ge", "left", "right", a, b, ctx)

    ForAll(vars, body) -> compile_quantifier("forall", vars, body, ctx)

    Exists(vars, body) -> compile_quantifier("exists", vars, body, ctx)
  }
}

/// Compile a list of expressions to a JSON array
pub fn compile_list(
  exprs: List(Expr),
  ctx: CompilerContext,
) -> Result(#(List(Json), CompilerContext), Z3Error) {
  compile_list_helper(exprs, ctx, [])
}

// =============================================================================
// Sort Compilation
// =============================================================================

/// Compile a sort to JSON
pub fn compile_sort(sort: Sort) -> Json {
  case sort {
    BoolSort -> json.string("bool")
    IntSort -> json.string("int")
    RealSort -> json.string("real")
    UninterpretedSort(name) ->
      json.object([
        #("type", json.string("uninterpreted")),
        #("name", json.string(name)),
      ])
    ArraySort(domain, range) ->
      json.object([
        #("type", json.string("array")),
        #("domain", compile_sort(domain)),
        #("range", compile_sort(range)),
      ])
  }
}

/// Compile a sort name to a simple string for variable declarations
pub fn sort_to_string(sort: Sort) -> String {
  case sort {
    BoolSort -> "bool"
    IntSort -> "int"
    RealSort -> "real"
    UninterpretedSort(name) -> name
    ArraySort(_, _) -> "array"
  }
}

// =============================================================================
// Variable Declaration Extraction
// =============================================================================

/// Extract all variable declarations from an expression
pub fn extract_variables(expr: Expr) -> Dict(String, Sort) {
  extract_vars_helper(expr, dict.new())
}

fn extract_vars_helper(
  expr: Expr,
  acc: Dict(String, Sort),
) -> Dict(String, Sort) {
  case expr {
    Const(name, sort) -> dict.insert(acc, name, sort)
    BoolLit(_) -> acc
    IntLit(_) -> acc
    RealLit(_, _) -> acc
    And(exprs) -> list.fold(exprs, acc, fn(a, e) { extract_vars_helper(e, a) })
    Or(exprs) -> list.fold(exprs, acc, fn(a, e) { extract_vars_helper(e, a) })
    Not(inner) -> extract_vars_helper(inner, acc)
    Implies(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Iff(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Ite(c, t, e) ->
      extract_vars_helper(
        e,
        extract_vars_helper(t, extract_vars_helper(c, acc)),
      )
    Add(exprs) -> list.fold(exprs, acc, fn(a, e) { extract_vars_helper(e, a) })
    Mul(exprs) -> list.fold(exprs, acc, fn(a, e) { extract_vars_helper(e, a) })
    Sub(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Div(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Neg(inner) -> extract_vars_helper(inner, acc)
    Eq(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Lt(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Le(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Gt(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    Ge(a, b) -> extract_vars_helper(b, extract_vars_helper(a, acc))
    ForAll(vars, body) -> {
      // Add bound variables temporarily, then extract from body
      let bound_names = list.map(vars, fn(v) { v.0 })
      let body_vars = extract_vars_helper(body, acc)
      // Remove bound variables from result
      dict.filter(body_vars, fn(name, _) { !list.contains(bound_names, name) })
    }
    Exists(vars, body) -> {
      let bound_names = list.map(vars, fn(v) { v.0 })
      let body_vars = extract_vars_helper(body, acc)
      dict.filter(body_vars, fn(name, _) { !list.contains(bound_names, name) })
    }
  }
}

// =============================================================================
// JSON Builder Utilities
// =============================================================================

/// Build a JSON object for a solver command
pub fn build_command(
  cmd: String,
  request_id: Int,
  extra_fields: List(#(String, Json)),
) -> Json {
  json.object([
    #("id", json.int(request_id)),
    #("cmd", json.string(cmd)),
    ..extra_fields
  ])
}

/// Build an assert command
pub fn build_assert_command(
  request_id: Int,
  solver_id: Int,
  context_id: Int,
  expr_json: Json,
) -> Json {
  build_command("assert", request_id, [
    #("solver_id", json.int(solver_id)),
    #("context_id", json.int(context_id)),
    #("expr", expr_json),
  ])
}

/// Build a check command
pub fn build_check_command(request_id: Int, solver_id: Int) -> Json {
  build_command("check", request_id, [#("solver_id", json.int(solver_id))])
}

/// Build a push command
pub fn build_push_command(request_id: Int, solver_id: Int) -> Json {
  build_command("push", request_id, [#("solver_id", json.int(solver_id))])
}

/// Build a pop command
pub fn build_pop_command(request_id: Int, solver_id: Int, n: Int) -> Json {
  build_command("pop", request_id, [
    #("solver_id", json.int(solver_id)),
    #("n", json.int(n)),
  ])
}

// =============================================================================
// Internal Helper Functions
// =============================================================================

fn compile_binary_expr(
  type_name: String,
  left_name: String,
  right_name: String,
  left: Expr,
  right: Expr,
  ctx: CompilerContext,
) -> Result(CompileResult, Z3Error) {
  use left_result <- result.try(compile_with_context(left, ctx))
  use right_result <- result.try(compile_with_context(
    right,
    left_result.context,
  ))
  Ok(CompileResult(
    json: json.object([
      #("type", json.string(type_name)),
      #(left_name, left_result.json),
      #(right_name, right_result.json),
    ]),
    context: right_result.context,
  ))
}

fn compile_list_expr(
  type_name: String,
  list_name: String,
  exprs: List(Expr),
  ctx: CompilerContext,
) -> Result(CompileResult, Z3Error) {
  use #(json_list, new_ctx) <- result.try(compile_list(exprs, ctx))
  Ok(CompileResult(
    json: json.object([
      #("type", json.string(type_name)),
      #(list_name, json.array(json_list, fn(x) { x })),
    ]),
    context: new_ctx,
  ))
}

fn compile_quantifier(
  type_name: String,
  vars: List(#(String, Sort)),
  body: Expr,
  ctx: CompilerContext,
) -> Result(CompileResult, Z3Error) {
  // Add bound variables to context for body compilation
  let ctx_with_vars =
    list.fold(vars, ctx, fn(c, v) { add_variable(c, v.0, v.1) })

  use body_result <- result.try(compile_with_context(body, ctx_with_vars))

  let vars_json =
    json.array(vars, fn(v) {
      let #(name, sort) = v
      json.array([json.string(name), compile_sort(sort)], fn(x) { x })
    })

  Ok(CompileResult(
    json: json.object([
      #("type", json.string(type_name)),
      #("vars", vars_json),
      #("body", body_result.json),
    ]),
    context: body_result.context,
  ))
}

fn compile_list_helper(
  exprs: List(Expr),
  ctx: CompilerContext,
  acc: List(Json),
) -> Result(#(List(Json), CompilerContext), Z3Error) {
  case exprs {
    [] -> Ok(#(list.reverse(acc), ctx))
    [first, ..rest] -> {
      case compile_with_context(first, ctx) {
        Ok(result) ->
          compile_list_helper(rest, result.context, [result.json, ..acc])
        Error(e) -> Error(e)
      }
    }
  }
}

// Simple int to string helper (avoiding stdlib dependency issues)
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
