import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import z3/compiler
import z3/expr
import z3/model
import z3/solver
import z3/types.{
  Add, And, ArraySort, BoolLit, BoolSort, BoolVal, Const, Div, Eq, Exists,
  ForAll, Ge, Gt, Iff, Implies, IntLit, IntSort, IntVal, Ite, Le, Lt, Mul, Neg,
  Not, Or, ParseError, PortError, RealLit, RealSort, RealVal, SolverError, Sub,
  TimeoutError, UninterpretedSort, UnknownVal,
}
import z3/unsat_core

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// Sort Type Tests
// =============================================================================

pub fn bool_sort_test() {
  let sort = BoolSort
  let assert True = sort == BoolSort
}

pub fn int_sort_test() {
  let sort = IntSort
  let assert True = sort == IntSort
}

pub fn real_sort_test() {
  let sort = RealSort
  let assert True = sort == RealSort
}

pub fn uninterpreted_sort_test() {
  let sort = UninterpretedSort("Person")
  case sort {
    UninterpretedSort(name) -> {
      let assert True = name == "Person"
      Nil
    }
    _ -> panic as "Wrong sort type"
  }
}

pub fn array_sort_test() {
  let sort = ArraySort(IntSort, BoolSort)
  case sort {
    ArraySort(domain, range) -> {
      let assert True = domain == IntSort
      let assert True = range == BoolSort
      Nil
    }
    _ -> panic as "Wrong sort type"
  }
}

// =============================================================================
// Expression Type Tests
// =============================================================================

pub fn bool_literal_test() {
  let expr_true = BoolLit(True)
  let expr_false = BoolLit(False)

  case expr_true {
    BoolLit(val) -> {
      let assert True = val == True
      Nil
    }
    _ -> panic as "Wrong expression type"
  }

  case expr_false {
    BoolLit(val) -> {
      let assert True = val == False
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn int_literal_test() {
  let expr = IntLit(42)
  case expr {
    IntLit(val) -> {
      let assert True = val == 42
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn real_literal_test() {
  let expr = RealLit(numerator: 3, denominator: 2)
  case expr {
    RealLit(num, denom) -> {
      let assert True = num == 3
      let assert True = denom == 2
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn const_expression_test() {
  let x = Const("x", IntSort)
  case x {
    Const(name, sort) -> {
      let assert True = name == "x"
      let assert True = sort == IntSort
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn and_expression_test() {
  let a = Const("a", BoolSort)
  let b = Const("b", BoolSort)
  let expr = And([a, b])

  case expr {
    And(exprs) -> {
      let assert True = exprs == [a, b]
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn or_expression_test() {
  let a = Const("a", BoolSort)
  let b = Const("b", BoolSort)
  let expr = Or([a, b])

  case expr {
    Or(exprs) -> {
      let assert True = exprs == [a, b]
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn not_expression_test() {
  let a = Const("a", BoolSort)
  let expr = Not(a)

  case expr {
    Not(inner) -> {
      let assert True = inner == a
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn implies_expression_test() {
  let a = Const("a", BoolSort)
  let b = Const("b", BoolSort)
  let expr = Implies(a, b)

  case expr {
    Implies(ante, cons) -> {
      let assert True = ante == a
      let assert True = cons == b
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn iff_expression_test() {
  let a = Const("a", BoolSort)
  let b = Const("b", BoolSort)
  let expr = Iff(a, b)

  case expr {
    Iff(left, right) -> {
      let assert True = left == a
      let assert True = right == b
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn ite_expression_test() {
  let cond = Const("cond", BoolSort)
  let then_expr = IntLit(1)
  let else_expr = IntLit(0)
  let expr = Ite(cond, then_expr, else_expr)

  case expr {
    Ite(c, t, e) -> {
      let assert True = c == cond
      let assert True = t == then_expr
      let assert True = e == else_expr
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn add_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Add([x, y])

  case expr {
    Add(exprs) -> {
      let assert True = exprs == [x, y]
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn sub_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Sub(x, y)

  case expr {
    Sub(left, right) -> {
      let assert True = left == x
      let assert True = right == y
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn mul_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Mul([x, y])

  case expr {
    Mul(exprs) -> {
      let assert True = exprs == [x, y]
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn div_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Div(x, y)

  case expr {
    Div(num, denom) -> {
      let assert True = num == x
      let assert True = denom == y
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn neg_expression_test() {
  let x = Const("x", IntSort)
  let expr = Neg(x)

  case expr {
    Neg(inner) -> {
      let assert True = inner == x
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn eq_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Eq(x, y)

  case expr {
    Eq(left, right) -> {
      let assert True = left == x
      let assert True = right == y
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn lt_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Lt(x, y)

  case expr {
    Lt(left, right) -> {
      let assert True = left == x
      let assert True = right == y
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn le_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Le(x, y)

  case expr {
    Le(left, right) -> {
      let assert True = left == x
      let assert True = right == y
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn gt_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Gt(x, y)

  case expr {
    Gt(left, right) -> {
      let assert True = left == x
      let assert True = right == y
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn ge_expression_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Ge(x, y)

  case expr {
    Ge(left, right) -> {
      let assert True = left == x
      let assert True = right == y
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn forall_expression_test() {
  let x_var = #("x", IntSort)
  let body = Gt(Const("x", IntSort), IntLit(0))
  let expr = ForAll([x_var], body)

  case expr {
    ForAll(vars, inner_body) -> {
      let assert True = vars == [x_var]
      let assert True = inner_body == body
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn exists_expression_test() {
  let x_var = #("x", IntSort)
  let body = Eq(Const("x", IntSort), IntLit(42))
  let expr = Exists([x_var], body)

  case expr {
    Exists(vars, inner_body) -> {
      let assert True = vars == [x_var]
      let assert True = inner_body == body
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

// =============================================================================
// Value Type Tests
// =============================================================================

pub fn bool_value_test() {
  let val_true = BoolVal(True)
  let val_false = BoolVal(False)

  case val_true {
    BoolVal(v) -> {
      let assert True = v == True
      Nil
    }
    _ -> panic as "Wrong value type"
  }

  case val_false {
    BoolVal(v) -> {
      let assert True = v == False
      Nil
    }
    _ -> panic as "Wrong value type"
  }
}

pub fn int_value_test() {
  let val = IntVal(123)
  case val {
    IntVal(v) -> {
      let assert True = v == 123
      Nil
    }
    _ -> panic as "Wrong value type"
  }
}

pub fn real_value_test() {
  let val = RealVal(3.14)
  case val {
    RealVal(v) -> {
      let assert True = v == 3.14
      Nil
    }
    _ -> panic as "Wrong value type"
  }
}

pub fn unknown_value_test() {
  let val = UnknownVal("some_repr")
  case val {
    UnknownVal(repr) -> {
      let assert True = repr == "some_repr"
      Nil
    }
    _ -> panic as "Wrong value type"
  }
}

// =============================================================================
// Error Type Tests
// =============================================================================

pub fn solver_error_test() {
  let err = SolverError("Something went wrong")
  case err {
    SolverError(msg) -> {
      let assert True = msg == "Something went wrong"
      Nil
    }
    _ -> panic as "Wrong error type"
  }
}

pub fn timeout_error_test() {
  let err = TimeoutError
  let assert True = err == TimeoutError
}

pub fn port_error_test() {
  let err = PortError("Port died")
  case err {
    PortError(msg) -> {
      let assert True = msg == "Port died"
      Nil
    }
    _ -> panic as "Wrong error type"
  }
}

pub fn parse_error_test() {
  let err = ParseError("Invalid JSON")
  case err {
    ParseError(msg) -> {
      let assert True = msg == "Invalid JSON"
      Nil
    }
    _ -> panic as "Wrong error type"
  }
}

// =============================================================================
// Complex Expression Building Tests
// =============================================================================

pub fn nested_boolean_expression_test() {
  // (a AND b) OR (NOT c)
  let a = Const("a", BoolSort)
  let b = Const("b", BoolSort)
  let c = Const("c", BoolSort)

  let and_expr = And([a, b])
  let not_expr = Not(c)
  let or_expr = Or([and_expr, not_expr])

  case or_expr {
    Or([left, right]) -> {
      let assert True = left == and_expr
      let assert True = right == not_expr
      Nil
    }
    _ -> panic as "Wrong expression structure"
  }
}

pub fn arithmetic_constraint_test() {
  // x + y > z * 2
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let z = Const("z", IntSort)

  let sum = Add([x, y])
  let product = Mul([z, IntLit(2)])
  let constraint = Gt(sum, product)

  case constraint {
    Gt(left, right) -> {
      let assert True = left == sum
      let assert True = right == product
      Nil
    }
    _ -> panic as "Wrong expression structure"
  }
}

pub fn implies_chain_test() {
  // a => (b => c)
  let a = Const("a", BoolSort)
  let b = Const("b", BoolSort)
  let c = Const("c", BoolSort)

  let inner = Implies(b, c)
  let outer = Implies(a, inner)

  case outer {
    Implies(ante, cons) -> {
      let assert True = ante == a
      let assert True = cons == inner
      Nil
    }
    _ -> panic as "Wrong expression structure"
  }
}

pub fn ite_nested_test() {
  // if (x > 0) then (if (y > 0) then 1 else 2) else 3
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)

  let inner_cond = Gt(y, IntLit(0))
  let inner_ite = Ite(inner_cond, IntLit(1), IntLit(2))

  let outer_cond = Gt(x, IntLit(0))
  let outer_ite = Ite(outer_cond, inner_ite, IntLit(3))

  case outer_ite {
    Ite(cond, then_branch, else_branch) -> {
      let assert True = cond == outer_cond
      let assert True = then_branch == inner_ite
      let assert True = else_branch == IntLit(3)
      Nil
    }
    _ -> panic as "Wrong expression structure"
  }
}

// =============================================================================
// Expression Builder (expr) Module Tests
// =============================================================================

pub fn expr_int_const_test() {
  let x = expr.int_const("x")
  case x {
    Const(name, IntSort) -> {
      let assert True = name == "x"
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn expr_bool_const_test() {
  let p = expr.bool_const("p")
  case p {
    Const(name, BoolSort) -> {
      let assert True = name == "p"
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn expr_literals_test() {
  let assert True = expr.int(42) == IntLit(42)
  let assert True = expr.bool(True) == BoolLit(True)
  let assert True = expr.true_() == BoolLit(True)
  let assert True = expr.false_() == BoolLit(False)
  let assert True = expr.real(3, 2) == RealLit(3, 2)
}

pub fn expr_and_test() {
  let a = expr.bool_const("a")
  let b = expr.bool_const("b")
  let result = expr.and_([a, b])
  case result {
    And(exprs) -> {
      let assert True = list.length(exprs) == 2
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn expr_or_test() {
  let a = expr.bool_const("a")
  let b = expr.bool_const("b")
  let result = expr.or_([a, b])
  case result {
    Or(exprs) -> {
      let assert True = list.length(exprs) == 2
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn expr_not_test() {
  let a = expr.bool_const("a")
  let result = expr.not_(a)
  case result {
    Not(inner) -> {
      let assert True = inner == a
      Nil
    }
    _ -> panic as "Wrong expression type"
  }
}

pub fn expr_arithmetic_test() {
  let x = expr.int_const("x")
  let y = expr.int_const("y")

  // Test add
  case expr.add([x, y]) {
    Add(exprs) -> {
      let assert True = list.length(exprs) == 2
      Nil
    }
    _ -> panic as "Expected Add expression"
  }

  // Test sub
  case expr.sub(x, y) {
    Sub(left, right) -> {
      let assert True = left == x
      let assert True = right == y
      Nil
    }
    _ -> panic as "Expected Sub expression"
  }

  // Test mul
  case expr.mul([x, y]) {
    Mul(exprs) -> {
      let assert True = list.length(exprs) == 2
      Nil
    }
    _ -> panic as "Expected Mul expression"
  }

  // Test div
  case expr.div(x, y) {
    Div(num, denom) -> {
      let assert True = num == x
      let assert True = denom == y
      Nil
    }
    _ -> panic as "Expected Div expression"
  }
}

pub fn expr_comparisons_test() {
  let x = expr.int_const("x")
  let y = expr.int_const("y")

  let assert True = expr.eq(x, y) == Eq(x, y)
  let assert True = expr.lt(x, y) == Lt(x, y)
  let assert True = expr.le(x, y) == Le(x, y)
  let assert True = expr.gt(x, y) == Gt(x, y)
  let assert True = expr.ge(x, y) == Ge(x, y)
}

pub fn expr_distinct_test() {
  let a = expr.int_const("a")
  let b = expr.int_const("b")
  let c = expr.int_const("c")

  let result = expr.distinct([a, b, c])
  // distinct([a,b,c]) = (a != b) AND (a != c) AND (b != c)
  case result {
    And(_) -> Nil
    _ -> panic as "Expected And expression for distinct"
  }
}

pub fn expr_free_variables_test() {
  let x = expr.int_const("x")
  let y = expr.int_const("y")
  let constraint = expr.add([x, y])

  let vars = expr.free_variables(constraint)
  let assert True = list.contains(vars, "x")
  let assert True = list.contains(vars, "y")
  let assert True = list.length(vars) == 2
}

pub fn expr_substitute_test() {
  let x = expr.int_const("x")
  let y = expr.int_const("y")
  let constraint = expr.eq(x, expr.int(5))

  let substituted = expr.substitute(constraint, "x", y)
  let assert True = substituted == expr.eq(y, expr.int(5))
}

// =============================================================================
// Solver Module Tests
// =============================================================================

pub fn solver_new_test() {
  let assert Ok(s) = solver.new()
  let assert True = solver.get_assertions(s) == []
  let assert True = solver.scope_depth(s) == 0
}

pub fn solver_assert_test() {
  let assert Ok(s) = solver.new()
  let x = expr.int_const("x")
  let constraint = expr.gt(x, expr.int(0))

  let assert Ok(s) = solver.assert_(s, constraint)
  let assertions = solver.get_assertions(s)
  let assert True = list.length(assertions) == 1
}

pub fn solver_assert_all_test() {
  let assert Ok(s) = solver.new()
  let x = expr.int_const("x")
  let y = expr.int_const("y")
  let constraints = [
    expr.gt(x, expr.int(0)),
    expr.lt(y, expr.int(10)),
  ]

  let assert Ok(s) = solver.assert_all(s, constraints)
  let assertions = solver.get_assertions(s)
  let assert True = list.length(assertions) == 2
}

pub fn solver_push_pop_test() {
  let assert Ok(s) = solver.new()
  let x = expr.int_const("x")

  let assert Ok(s) = solver.assert_(s, expr.gt(x, expr.int(0)))
  let assert True = list.length(solver.get_assertions(s)) == 1

  let assert Ok(s) = solver.push(s)
  let assert True = solver.scope_depth(s) == 1

  let assert Ok(s) = solver.assert_(s, expr.lt(x, expr.int(10)))
  let assert True = list.length(solver.get_assertions(s)) == 2

  let assert Ok(s) = solver.pop(s)
  let assert True = solver.scope_depth(s) == 0
  let assert True = list.length(solver.get_assertions(s)) == 1
}

pub fn solver_reset_test() {
  let assert Ok(s) = solver.new()
  let x = expr.int_const("x")

  let assert Ok(s) = solver.assert_(s, expr.gt(x, expr.int(0)))
  let assert Ok(s) = solver.push(s)
  let assert Ok(s) = solver.reset(s)

  let assert True = solver.get_assertions(s) == []
  let assert True = solver.scope_depth(s) == 0
}

pub fn solver_check_empty_test() {
  let assert Ok(s) = solver.new()
  let assert Ok(#(_, result)) = solver.check(s)

  case result {
    solver.SolverSat(_) -> Nil
    _ -> panic as "Empty solver should be SAT"
  }
}

pub fn solver_named_assertions_test() {
  let assert Ok(s) = solver.new()
  let x = expr.int_const("x")

  let assert Ok(s) = solver.assert_named(s, "c1", expr.gt(x, expr.int(0)))
  let assert Ok(s) = solver.assert_named(s, "c2", expr.lt(x, expr.int(10)))

  let named = solver.get_named_assertions(s)
  let assert True = dict.size(named) == 2
}

// =============================================================================
// Model Module Tests
// =============================================================================

pub fn model_empty_test() {
  let m = model.empty()
  let assert True = model.is_empty(m)
  let assert True = model.size(m) == 0
}

pub fn model_from_list_test() {
  let m =
    model.from_list([
      #("x", IntVal(42)),
      #("y", IntVal(17)),
    ])

  let assert True = model.size(m) == 2
  let assert Some(42) = model.get_int(m, "x")
  let assert Some(17) = model.get_int(m, "y")
}

pub fn model_set_values_test() {
  let m = model.empty()
  let m = model.set_int(m, "x", 42)
  let m = model.set_bool(m, "flag", True)
  let m = model.set_real(m, "pi", 3.14)

  let assert Some(42) = model.get_int(m, "x")
  let assert Some(True) = model.get_bool(m, "flag")
  let assert Some(_) = model.get_real(m, "pi")
}

pub fn model_get_variable_names_test() {
  let m =
    model.from_list([
      #("a", IntVal(1)),
      #("b", IntVal(2)),
      #("c", IntVal(3)),
    ])

  let names = model.get_variable_names(m)
  let assert True = list.length(names) == 3
  let assert True = list.contains(names, "a")
  let assert True = list.contains(names, "b")
  let assert True = list.contains(names, "c")
}

pub fn model_has_variable_test() {
  let m = model.from_list([#("x", IntVal(42))])

  let assert True = model.has_variable(m, "x")
  let assert False = model.has_variable(m, "y")
}

pub fn model_to_dict_test() {
  let m =
    model.from_list([
      #("x", IntVal(1)),
      #("y", IntVal(2)),
    ])

  let d = model.to_dict(m)
  let assert True = dict.size(d) == 2
  let assert Ok(IntVal(1)) = dict.get(d, "x")
  let assert Ok(IntVal(2)) = dict.get(d, "y")
}

pub fn model_eval_test() {
  let m =
    model.from_list([
      #("x", IntVal(5)),
      #("y", IntVal(3)),
    ])

  // Simple evaluation
  let x = expr.int_const("x")
  let result = model.eval(m, x)
  let assert True = result == IntLit(5)

  // Arithmetic evaluation
  let sum = expr.add([expr.int_const("x"), expr.int_const("y")])
  let sum_result = model.eval(m, sum)
  let assert True = sum_result == IntLit(8)
}

pub fn model_eval_comparison_test() {
  let m =
    model.from_list([
      #("x", IntVal(10)),
    ])

  let x = expr.int_const("x")
  let cmp = expr.gt(x, expr.int(5))
  let result = model.eval(m, cmp)
  let assert True = result == BoolLit(True)
}

// =============================================================================
// Compiler Module Tests
// =============================================================================

pub fn compiler_compile_int_lit_test() {
  let expr = IntLit(42)
  let assert Ok(json_val) = compiler.compile(expr)
  let json_str = json.to_string(json_val)
  let assert True = json_str != ""
}

pub fn compiler_compile_bool_lit_test() {
  let expr = BoolLit(True)
  let assert Ok(json_val) = compiler.compile(expr)
  let json_str = json.to_string(json_val)
  let assert True = json_str != ""
}

pub fn compiler_compile_const_test() {
  let expr = Const("x", IntSort)
  let assert Ok(json_val) = compiler.compile(expr)
  let json_str = json.to_string(json_val)
  let assert True = json_str != ""
}

pub fn compiler_compile_and_test() {
  let a = BoolLit(True)
  let b = BoolLit(False)
  let expr = And([a, b])
  let assert Ok(json_val) = compiler.compile(expr)
  let json_str = json.to_string(json_val)
  let assert True = json_str != ""
}

pub fn compiler_compile_arithmetic_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let expr = Add([x, y])
  let assert Ok(json_val) = compiler.compile(expr)
  let json_str = json.to_string(json_val)
  let assert True = json_str != ""
}

pub fn compiler_extract_variables_test() {
  let x = Const("x", IntSort)
  let y = Const("y", IntSort)
  let z = Const("z", BoolSort)

  let expr = And([Gt(x, y), z])
  let vars = compiler.extract_variables(expr)

  let assert True = dict.size(vars) == 3
  let assert Ok(IntSort) = dict.get(vars, "x")
  let assert Ok(IntSort) = dict.get(vars, "y")
  let assert Ok(BoolSort) = dict.get(vars, "z")
}

pub fn compiler_context_test() {
  let ctx = compiler.new_context()
  let ctx = compiler.add_variable(ctx, "x", IntSort)
  let ctx = compiler.add_variable(ctx, "y", BoolSort)

  let vars = compiler.get_variables(ctx)
  let assert True = dict.size(vars) == 2

  let assert Some(IntSort) = compiler.get_variable_sort(ctx, "x")
  let assert Some(BoolSort) = compiler.get_variable_sort(ctx, "y")
  let assert None = compiler.get_variable_sort(ctx, "z")
}

pub fn compiler_fresh_name_test() {
  let ctx = compiler.new_context()
  let #(name1, ctx) = compiler.fresh_name(ctx, "tmp")
  let #(name2, _ctx) = compiler.fresh_name(ctx, "tmp")

  let assert True = name1 == "tmp_0"
  let assert True = name2 == "tmp_1"
}

pub fn compiler_compile_sort_test() {
  let int_json = compiler.compile_sort(IntSort)
  let assert True = json.to_string(int_json) == "\"int\""

  let bool_json = compiler.compile_sort(BoolSort)
  let assert True = json.to_string(bool_json) == "\"bool\""

  let real_json = compiler.compile_sort(RealSort)
  let assert True = json.to_string(real_json) == "\"real\""
}

// =============================================================================
// Unsat Core Module Tests
// =============================================================================

pub fn unsat_core_auto_name_test() {
  let name1 = unsat_core.auto_name("constraint", 0)
  let name2 = unsat_core.auto_name("constraint", 5)

  let assert True = name1 == "constraint_0"
  let assert True = name2 == "constraint_5"
}

pub fn unsat_core_types_test() {
  let core =
    unsat_core.UnsatCore(assertion_names: ["c1", "c2", "c3"], is_minimal: False)

  let assert True = unsat_core.core_size(core) == 3
  let assert True = unsat_core.is_in_core(core, "c1")
  let assert True = unsat_core.is_in_core(core, "c2")
  let assert False = unsat_core.is_in_core(core, "c4")
}

pub fn unsat_core_format_test() {
  let core = unsat_core.UnsatCore(assertion_names: ["a", "b"], is_minimal: True)

  let formatted = unsat_core.format_core(core)
  let assert True = formatted != ""
}

pub fn unsat_core_config_test() {
  let config = unsat_core.core_extraction_config()
  let assert Ok(s) = solver.new_with_config(config)
  let assert True = unsat_core.is_core_enabled(s)
}
