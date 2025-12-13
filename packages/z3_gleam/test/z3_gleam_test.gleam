import gleeunit
import z3/types.{
  Add, And, ArraySort, BoolLit, BoolSort, BoolVal, Const, Div, Eq, Exists,
  ForAll, Ge, Gt, Iff, Implies, IntLit, IntSort, IntVal, Ite, Le, Lt, Mul, Neg,
  Not, Or, ParseError, PortError, RealLit, RealSort, RealVal, SolverError, Sub,
  TimeoutError, UninterpretedSort, UnknownVal,
}

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
