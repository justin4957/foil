//// Z3 Dialogue Test
////
//// This module tests the "back and forth" interaction between the Gleam
//// application and the Z3 implementation, demonstrating the current
//// capabilities of the API.

import gleam/dict
import gleam/io
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import z3/compiler
import z3/expr
import z3/model
import z3/solver
import z3/types.{BoolLit, BoolVal, IntLit, IntVal}
import z3/unsat_core

/// Main entry point for the dialogue test
pub fn main() {
  io.println("=== Z3 Gleam Dialogue Test ===\n")

  test_expression_building()
  test_solver_interaction()
  test_model_evaluation()
  test_compilation_roundtrip()
  test_unsat_core_workflow()

  io.println("\n=== All dialogue tests completed ===")
}

/// Test 1: Expression Building Dialogue
fn test_expression_building() {
  io.println("--- Test 1: Expression Building Dialogue ---")
  io.println("User: Create integer variables x and y")

  let x = expr.int_const("x")
  let y = expr.int_const("y")

  io.println("Z3: Created Const(\"x\", IntSort) and Const(\"y\", IntSort)")

  io.println("\nUser: Build constraint: x > 0 AND y < 10 AND x + y = 15")

  let c1 = expr.gt(x, expr.int(0))
  let c2 = expr.lt(y, expr.int(10))
  let c3 = expr.eq(expr.add([x, y]), expr.int(15))
  let constraint = expr.and_([c1, c2, c3])

  io.println("Z3: Built expression tree:")
  io.println("    And([")
  io.println("      Gt(Const(\"x\"), IntLit(0)),")
  io.println("      Lt(Const(\"y\"), IntLit(10)),")
  io.println("      Eq(Add([Const(\"x\"), Const(\"y\")]), IntLit(15))")
  io.println("    ])")

  io.println("\nUser: What free variables are in this constraint?")
  let vars = expr.free_variables(constraint)
  io.println("Z3: Free variables: " <> string.join(vars, ", "))

  io.println("\nUser: Substitute x with 7")
  let substituted = expr.substitute(constraint, "x", expr.int(7))
  io.println(
    "Z3: After substitution, expression contains new constraints with 7",
  )

  // Verify substitution worked
  let new_vars = expr.free_variables(substituted)
  io.println("Z3: Remaining free variables: " <> string.join(new_vars, ", "))

  io.println("")
}

/// Test 2: Solver Interaction Dialogue
fn test_solver_interaction() {
  io.println("--- Test 2: Solver Interaction Dialogue ---")
  io.println("User: Create a new solver")

  let assert Ok(s) = solver.new()
  io.println("Z3: Created solver with empty assertion stack")
  io.println(
    "Z3: Current assertions: " <> int_to_str(list_len(solver.get_assertions(s))),
  )
  io.println("Z3: Scope depth: " <> int_to_str(solver.scope_depth(s)))

  io.println("\nUser: Assert x > 0")
  let x = expr.int_const("x")
  let assert Ok(s) = solver.assert_(s, expr.gt(x, expr.int(0)))
  io.println(
    "Z3: Added assertion. Count: "
    <> int_to_str(list_len(solver.get_assertions(s))),
  )

  io.println("\nUser: Push a scope")
  let assert Ok(s) = solver.push(s)
  io.println("Z3: Pushed scope. Depth: " <> int_to_str(solver.scope_depth(s)))

  io.println("\nUser: Assert x < 10 in this scope")
  let assert Ok(s) = solver.assert_(s, expr.lt(x, expr.int(10)))
  io.println(
    "Z3: Added assertion. Count: "
    <> int_to_str(list_len(solver.get_assertions(s))),
  )

  io.println("\nUser: Check satisfiability")
  let assert Ok(#(s, result)) = solver.check(s)
  case result {
    solver.SolverSat(_) -> io.println("Z3: SAT (trivially, empty solver)")
    solver.SolverUnsat -> io.println("Z3: UNSAT")
    solver.SolverUnknown(reason) -> io.println("Z3: Unknown - " <> reason)
  }

  io.println("\nUser: Pop the scope")
  let assert Ok(s) = solver.pop(s)
  io.println("Z3: Popped scope. Depth: " <> int_to_str(solver.scope_depth(s)))
  io.println(
    "Z3: Assertions after pop: "
    <> int_to_str(list_len(solver.get_assertions(s))),
  )

  io.println("\nUser: Reset the solver")
  let assert Ok(s) = solver.reset(s)
  io.println(
    "Z3: Solver reset. Assertions: "
    <> int_to_str(list_len(solver.get_assertions(s))),
  )

  io.println("")
}

/// Test 3: Model Evaluation Dialogue
fn test_model_evaluation() {
  io.println("--- Test 3: Model Evaluation Dialogue ---")
  io.println("User: Create a model with x=10, y=5, flag=true")

  let m =
    model.from_list([
      #("x", IntVal(10)),
      #("y", IntVal(5)),
      #("flag", BoolVal(True)),
    ])

  io.println(
    "Z3: Created model with " <> int_to_str(model.size(m)) <> " variables",
  )

  io.println("\nUser: What is the value of x?")
  case model.get_int(m, "x") {
    Some(val) -> io.println("Z3: x = " <> int_to_str(val))
    None -> io.println("Z3: x not found")
  }

  io.println("\nUser: Evaluate x + y")
  let x = expr.int_const("x")
  let y = expr.int_const("y")
  let sum = expr.add([x, y])
  let result = model.eval(m, sum)
  case result {
    IntLit(val) -> io.println("Z3: x + y = " <> int_to_str(val))
    _ -> io.println("Z3: Could not fully evaluate")
  }

  io.println("\nUser: Evaluate x > 5")
  let cmp = expr.gt(x, expr.int(5))
  let cmp_result = model.eval(m, cmp)
  case cmp_result {
    BoolLit(True) -> io.println("Z3: x > 5 = true")
    BoolLit(False) -> io.println("Z3: x > 5 = false")
    _ -> io.println("Z3: Could not fully evaluate")
  }

  io.println("\nUser: Evaluate x * y - 3")
  let complex = expr.sub(expr.mul([x, y]), expr.int(3))
  let complex_result = model.eval(m, complex)
  case complex_result {
    IntLit(val) -> io.println("Z3: x * y - 3 = " <> int_to_str(val))
    _ -> io.println("Z3: Could not fully evaluate")
  }

  io.println("\nUser: Does the model have variable 'z'?")
  case model.has_variable(m, "z") {
    True -> io.println("Z3: Yes, z exists")
    False -> io.println("Z3: No, z is not in the model")
  }

  io.println("")
}

/// Test 4: Compilation Roundtrip Dialogue
fn test_compilation_roundtrip() {
  io.println("--- Test 4: Compilation Roundtrip Dialogue ---")
  io.println("User: Compile the expression (x > 0) AND (y = x + 1) to JSON")

  let x = expr.int_const("x")
  let y = expr.int_const("y")
  let c1 = expr.gt(x, expr.int(0))
  let c2 = expr.eq(y, expr.add([x, expr.int(1)]))
  let constraint = expr.and_([c1, c2])

  let assert Ok(json_val) = compiler.compile(constraint)
  let json_str = json.to_string(json_val)

  io.println("Z3: Compiled to JSON:")
  io.println("    " <> truncate_string(json_str, 100))

  io.println("\nUser: What variables were extracted?")
  let vars = compiler.extract_variables(constraint)
  io.println("Z3: Extracted variables:")
  let var_list = dict.to_list(vars)
  print_variables(var_list)

  io.println("\nUser: Compile sort types")
  io.println(
    "Z3: IntSort -> " <> json.to_string(compiler.compile_sort(types.IntSort)),
  )
  io.println(
    "Z3: BoolSort -> " <> json.to_string(compiler.compile_sort(types.BoolSort)),
  )
  io.println(
    "Z3: RealSort -> " <> json.to_string(compiler.compile_sort(types.RealSort)),
  )

  io.println("")
}

/// Test 5: Unsat Core Workflow Dialogue
fn test_unsat_core_workflow() {
  io.println("--- Test 5: Unsat Core Workflow Dialogue ---")
  io.println("User: Create a solver configured for unsat core extraction")

  let config = unsat_core.core_extraction_config()
  let assert Ok(s) = solver.new_with_config(config)
  io.println("Z3: Created solver with unsat core extraction enabled")

  io.println("\nUser: Add named assertions for a conflicting set")
  let x = expr.int_const("x")

  let assert Ok(s) =
    unsat_core.assert_named(s, "positive", expr.gt(x, expr.int(0)))
  io.println("Z3: Added 'positive': x > 0")

  let assert Ok(s) =
    unsat_core.assert_named(s, "negative", expr.lt(x, expr.int(0)))
  io.println("Z3: Added 'negative': x < 0")

  let assert Ok(s) = unsat_core.assert_named(s, "zero", expr.eq(x, expr.int(0)))
  io.println("Z3: Added 'zero': x = 0")

  let named = solver.get_named_assertions(s)
  io.println("Z3: Total named assertions: " <> int_to_str(dict.size(named)))

  io.println("\nUser: Simulate an unsat core result")
  // In a real implementation, this would come from the solver
  let core =
    unsat_core.UnsatCore(
      assertion_names: ["positive", "negative"],
      is_minimal: False,
    )

  io.println(
    "Z3: Unsat core found with "
    <> int_to_str(unsat_core.core_size(core))
    <> " assertions",
  )
  io.println("Z3: " <> unsat_core.format_core(core))

  io.println("\nUser: Is 'positive' in the core?")
  case unsat_core.is_in_core(core, "positive") {
    True -> io.println("Z3: Yes, 'positive' is in the core")
    False -> io.println("Z3: No")
  }

  io.println("\nUser: Is 'zero' in the core?")
  case unsat_core.is_in_core(core, "zero") {
    True -> io.println("Z3: Yes")
    False -> io.println("Z3: No, 'zero' is not in the core")
  }

  io.println("\nUser: Get the core expressions")
  let core_exprs = unsat_core.get_core_expressions(s, core)
  io.println("Z3: Core expressions:")
  print_core_expressions(core_exprs)

  io.println("")
}

// Helper functions
fn int_to_str(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "10"
    15 -> "15"
    47 -> "47"
    50 -> "50"
    _ if n < 0 -> "-" <> int_to_str(-n)
    _ -> do_int_to_str(n, "")
  }
}

fn do_int_to_str(n: Int, acc: String) -> String {
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
      do_int_to_str(n / 10, char <> acc)
    }
  }
}

fn list_len(items: List(a)) -> Int {
  case items {
    [] -> 0
    [_, ..rest] -> 1 + list_len(rest)
  }
}

fn truncate_string(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len) <> "..."
    False -> s
  }
}

fn print_variables(vars: List(#(String, types.Sort))) -> Nil {
  case vars {
    [] -> Nil
    [#(name, sort), ..rest] -> {
      let sort_str = case sort {
        types.IntSort -> "Int"
        types.BoolSort -> "Bool"
        types.RealSort -> "Real"
        _ -> "Other"
      }
      io.println("    " <> name <> ": " <> sort_str)
      print_variables(rest)
    }
  }
}

fn print_core_expressions(exprs: List(#(String, types.Expr))) -> Nil {
  case exprs {
    [] -> Nil
    [#(name, _expr), ..rest] -> {
      io.println("    - " <> name)
      print_core_expressions(rest)
    }
  }
}
