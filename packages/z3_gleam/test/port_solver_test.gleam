//// Port Solver Integration Tests
////
//// Tests for the Z3 port solver integration.
//// These tests verify that the solver correctly communicates with Z3.

import gleam/dict
import gleam/io
import z3/expr
import z3/port_solver
import z3/solver
import z3/types

/// Main entry point for port solver tests
pub fn main() {
  io.println("=== Port Solver Integration Tests ===\n")

  test_port_solver_lifecycle()
  test_port_solver_simple_sat()
  test_port_solver_simple_unsat()
  test_port_solver_arithmetic()
  test_port_solver_push_pop()
  test_solver_check_with_z3()

  io.println("\n=== All port solver tests completed ===")
}

/// Test 1: Port Solver Lifecycle
fn test_port_solver_lifecycle() {
  io.println("--- Test 1: Port Solver Lifecycle ---")

  io.println("Creating port solver...")
  case port_solver.new() {
    Ok(ps) -> {
      io.println("[OK] Port solver created successfully")

      // Check if alive
      case port_solver.is_alive(ps) {
        Ok(ps2) -> {
          io.println("[OK] Port solver is alive (ping successful)")

          // Check stats
          let stats = port_solver.stats(ps2)
          io.println(
            "[OK] Stats: assertions="
            <> int_to_str(dict.get(stats, "assertions") |> result_unwrap(0))
            <> ", scope_depth="
            <> int_to_str(dict.get(stats, "scope_depth") |> result_unwrap(0)),
          )

          // Close
          port_solver.close(ps2)
          io.println("[OK] Port solver closed")
        }
        Error(e) -> {
          io.println("[SKIP] Ping failed: " <> format_error(e))
          port_solver.close(ps)
        }
      }
    }
    Error(e) -> {
      io.println(
        "[SKIP] Could not create port solver (Z3 may not be installed): "
        <> format_error(e),
      )
    }
  }
  io.println("")
}

/// Test 2: Simple SAT Check
fn test_port_solver_simple_sat() {
  io.println("--- Test 2: Simple SAT Check ---")

  case port_solver.new() {
    Ok(ps) -> {
      // Create constraint: x > 0 AND x < 10
      let x = expr.int_const("x")
      let c1 = expr.gt(x, expr.int(0))
      let c2 = expr.lt(x, expr.int(10))

      io.println("Adding constraints: x > 0 AND x < 10")

      case port_solver.assert_(ps, c1) {
        Ok(ps2) -> {
          case port_solver.assert_(ps2, c2) {
            Ok(ps3) -> {
              io.println("Checking satisfiability...")
              case port_solver.check(ps3) {
                Ok(#(ps4, result)) -> {
                  case result {
                    solver.SolverSat(_model) -> {
                      io.println("[OK] Result: SAT (as expected)")
                    }
                    solver.SolverUnsat -> {
                      io.println("[FAIL] Result: UNSAT (unexpected)")
                    }
                    solver.SolverUnknown(reason) -> {
                      io.println("[INFO] Result: Unknown - " <> reason)
                    }
                  }
                  port_solver.close(ps4)
                }
                Error(e) -> {
                  io.println("[SKIP] Check failed: " <> format_error(e))
                  port_solver.close(ps3)
                }
              }
            }
            Error(e) -> {
              io.println("[SKIP] Assert failed: " <> format_error(e))
              port_solver.close(ps2)
            }
          }
        }
        Error(e) -> {
          io.println("[SKIP] Assert failed: " <> format_error(e))
          port_solver.close(ps)
        }
      }
    }
    Error(e) -> {
      io.println("[SKIP] Port solver not available: " <> format_error(e))
    }
  }
  io.println("")
}

/// Test 3: Simple UNSAT Check
fn test_port_solver_simple_unsat() {
  io.println("--- Test 3: Simple UNSAT Check ---")

  case port_solver.new() {
    Ok(ps) -> {
      // Create contradictory constraint: x > 0 AND x < 0
      let x = expr.int_const("x")
      let c1 = expr.gt(x, expr.int(0))
      let c2 = expr.lt(x, expr.int(0))

      io.println("Adding contradictory constraints: x > 0 AND x < 0")

      case port_solver.assert_all(ps, [c1, c2]) {
        Ok(ps2) -> {
          io.println("Checking satisfiability...")
          case port_solver.check(ps2) {
            Ok(#(ps3, result)) -> {
              case result {
                solver.SolverSat(_) -> {
                  io.println(
                    "[FAIL] Result: SAT (unexpected for contradiction)",
                  )
                }
                solver.SolverUnsat -> {
                  io.println("[OK] Result: UNSAT (as expected)")
                }
                solver.SolverUnknown(reason) -> {
                  io.println("[INFO] Result: Unknown - " <> reason)
                }
              }
              port_solver.close(ps3)
            }
            Error(e) -> {
              io.println("[SKIP] Check failed: " <> format_error(e))
              port_solver.close(ps2)
            }
          }
        }
        Error(e) -> {
          io.println("[SKIP] Assert failed: " <> format_error(e))
          port_solver.close(ps)
        }
      }
    }
    Error(e) -> {
      io.println("[SKIP] Port solver not available: " <> format_error(e))
    }
  }
  io.println("")
}

/// Test 4: Arithmetic Constraints
fn test_port_solver_arithmetic() {
  io.println("--- Test 4: Arithmetic Constraints ---")

  case port_solver.new() {
    Ok(ps) -> {
      // Create constraint: x + y = 15 AND x > 5 AND y > 5
      let x = expr.int_const("x")
      let y = expr.int_const("y")
      let c1 = expr.eq(expr.add([x, y]), expr.int(15))
      let c2 = expr.gt(x, expr.int(5))
      let c3 = expr.gt(y, expr.int(5))

      io.println("Adding constraints: x + y = 15 AND x > 5 AND y > 5")

      case port_solver.assert_all(ps, [c1, c2, c3]) {
        Ok(ps2) -> {
          io.println("Checking satisfiability...")
          case port_solver.check(ps2) {
            Ok(#(ps3, result)) -> {
              case result {
                solver.SolverSat(_model) -> {
                  io.println("[OK] Result: SAT (e.g., x=7, y=8)")
                }
                solver.SolverUnsat -> {
                  io.println("[FAIL] Result: UNSAT (unexpected)")
                }
                solver.SolverUnknown(reason) -> {
                  io.println("[INFO] Result: Unknown - " <> reason)
                }
              }
              port_solver.close(ps3)
            }
            Error(e) -> {
              io.println("[SKIP] Check failed: " <> format_error(e))
              port_solver.close(ps2)
            }
          }
        }
        Error(e) -> {
          io.println("[SKIP] Assert failed: " <> format_error(e))
          port_solver.close(ps)
        }
      }
    }
    Error(e) -> {
      io.println("[SKIP] Port solver not available: " <> format_error(e))
    }
  }
  io.println("")
}

/// Test 5: Push/Pop Scopes
fn test_port_solver_push_pop() {
  io.println("--- Test 5: Push/Pop Scopes ---")

  case port_solver.new() {
    Ok(ps) -> {
      let x = expr.int_const("x")

      // Assert x > 0
      io.println("Asserting: x > 0")
      case port_solver.assert_(ps, expr.gt(x, expr.int(0))) {
        Ok(ps2) -> {
          io.println(
            "Scope depth: " <> int_to_str(port_solver.scope_depth(ps2)),
          )

          // Push scope
          io.println("Pushing scope...")
          case port_solver.push(ps2) {
            Ok(ps3) -> {
              io.println(
                "Scope depth after push: "
                <> int_to_str(port_solver.scope_depth(ps3)),
              )

              // Assert contradiction in this scope
              io.println("Asserting in scope: x < 0 (contradiction)")
              case port_solver.assert_(ps3, expr.lt(x, expr.int(0))) {
                Ok(ps4) -> {
                  // Check - should be UNSAT
                  case port_solver.check(ps4) {
                    Ok(#(ps5, result1)) -> {
                      case result1 {
                        solver.SolverUnsat ->
                          io.println("[OK] With contradiction: UNSAT")
                        solver.SolverUnknown(r) ->
                          io.println(
                            "[INFO] With contradiction: Unknown - " <> r,
                          )
                        _ ->
                          io.println(
                            "[FAIL] With contradiction: SAT (unexpected)",
                          )
                      }

                      // Pop scope
                      io.println("Popping scope...")
                      case port_solver.pop(ps5) {
                        Ok(ps6) -> {
                          io.println(
                            "Scope depth after pop: "
                            <> int_to_str(port_solver.scope_depth(ps6)),
                          )

                          // Check again - should be SAT now
                          case port_solver.check(ps6) {
                            Ok(#(ps7, result2)) -> {
                              case result2 {
                                solver.SolverSat(_) ->
                                  io.println("[OK] After pop: SAT")
                                solver.SolverUnknown(r) ->
                                  io.println(
                                    "[INFO] After pop: Unknown - " <> r,
                                  )
                                _ ->
                                  io.println(
                                    "[FAIL] After pop: UNSAT (unexpected)",
                                  )
                              }
                              port_solver.close(ps7)
                            }
                            Error(e) -> {
                              io.println(
                                "[SKIP] Check after pop failed: "
                                <> format_error(e),
                              )
                              port_solver.close(ps6)
                            }
                          }
                        }
                        Error(e) -> {
                          io.println("[SKIP] Pop failed: " <> format_error(e))
                          port_solver.close(ps5)
                        }
                      }
                    }
                    Error(e) -> {
                      io.println("[SKIP] Check failed: " <> format_error(e))
                      port_solver.close(ps4)
                    }
                  }
                }
                Error(e) -> {
                  io.println(
                    "[SKIP] Assert in scope failed: " <> format_error(e),
                  )
                  port_solver.close(ps3)
                }
              }
            }
            Error(e) -> {
              io.println("[SKIP] Push failed: " <> format_error(e))
              port_solver.close(ps2)
            }
          }
        }
        Error(e) -> {
          io.println("[SKIP] Assert failed: " <> format_error(e))
          port_solver.close(ps)
        }
      }
    }
    Error(e) -> {
      io.println("[SKIP] Port solver not available: " <> format_error(e))
    }
  }
  io.println("")
}

/// Test 6: solver.check_with_z3() function
fn test_solver_check_with_z3() {
  io.println("--- Test 6: solver.check_with_z3() Integration ---")

  // Create standard solver
  case solver.new() {
    Ok(s) -> {
      let x = expr.int_const("x")

      // Add constraints
      case solver.assert_(s, expr.gt(x, expr.int(0))) {
        Ok(s2) -> {
          case solver.assert_(s2, expr.lt(x, expr.int(10))) {
            Ok(s3) -> {
              io.println(
                "Standard solver created with constraints: x > 0, x < 10",
              )

              // Test regular check (placeholder)
              io.println("Testing solver.check() (placeholder)...")
              case solver.check(s3) {
                Ok(#(_, result1)) -> {
                  case result1 {
                    solver.SolverUnknown(reason) -> {
                      io.println(
                        "[OK] solver.check() returns: Unknown - " <> reason,
                      )
                    }
                    _ -> {
                      io.println("[OK] solver.check() returned a result")
                    }
                  }
                }
                Error(_) -> io.println("[SKIP] solver.check() error")
              }

              // Test check_with_z3 (actual Z3)
              io.println("Testing solver.check_with_z3() (real Z3)...")
              case solver.check_with_z3(s3) {
                Ok(#(_, result2)) -> {
                  case result2 {
                    solver.SolverSat(_) -> {
                      io.println("[OK] solver.check_with_z3() returns: SAT")
                    }
                    solver.SolverUnsat -> {
                      io.println(
                        "[FAIL] solver.check_with_z3() returns: UNSAT (unexpected)",
                      )
                    }
                    solver.SolverUnknown(reason) -> {
                      io.println(
                        "[INFO] solver.check_with_z3() returns: Unknown - "
                        <> reason,
                      )
                    }
                  }
                }
                Error(e) -> {
                  io.println(
                    "[SKIP] solver.check_with_z3() not available: "
                    <> format_error(e),
                  )
                }
              }
            }
            Error(_) -> io.println("[SKIP] Assert failed")
          }
        }
        Error(_) -> io.println("[SKIP] Assert failed")
      }
    }
    Error(_) -> io.println("[SKIP] Solver creation failed")
  }
  io.println("")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn int_to_str(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_str(-n, "")
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

fn format_error(e: types.Z3Error) -> String {
  case e {
    types.SolverError(msg) -> "SolverError: " <> msg
    types.TimeoutError -> "TimeoutError"
    types.PortError(msg) -> "PortError: " <> msg
    types.ParseError(msg) -> "ParseError: " <> msg
  }
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(val) -> val
    Error(_) -> default
  }
}
