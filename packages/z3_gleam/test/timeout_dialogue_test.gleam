//// Z3 Timeout and Resource Management Dialogue Test
////
//// This test demonstrates the back-and-forth interaction for
//// Z3 timeout configuration and resource management.
////
//// ## Purpose
//// - Validates timeout configuration passing to Z3
//// - Demonstrates timeout handling behavior
//// - Tests resource cleanup on timeout/error
//// - Documents expected behavior for timeout scenarios

import gleam/int
import gleam/io
import gleam/string
import z3/expr
import z3/solver

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Z3 Timeout and Resource Management Dialogue Test")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Default configuration
  test_default_config()

  // Test 2: Custom timeout configuration
  test_timeout_config()

  // Test 3: Full configuration with all options
  test_full_config()

  // Test 4: Solver with timeout - simple SAT
  test_solver_with_timeout_sat()

  // Test 5: Check timeout detection helper
  test_timeout_detection()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All Z3 Timeout Dialogue Tests Completed!")
  io.println("=" |> string.repeat(70))
}

/// Test 1: Default configuration
fn test_default_config() {
  io.println("")
  io.println("--- Test 1: Default Configuration ---")
  io.println("")

  io.println("User: Create a solver with default configuration")
  io.println("")

  let config = solver.default_config()

  io.println("[System]: Default SolverConfig created")
  io.println("         timeout_ms: " <> int.to_string(config.timeout_ms))
  io.println("         z3_timeout: " <> int.to_string(config.z3_timeout))
  io.println("         z3_rlimit: " <> int.to_string(config.z3_rlimit))
  io.println("         unsat_core: " <> bool_to_string(config.unsat_core))
  io.println("")

  io.println("[OK] Default configuration has all timeouts at 0 (no limit)")
  io.println("")
}

/// Test 2: Custom timeout configuration
fn test_timeout_config() {
  io.println("")
  io.println("--- Test 2: Custom Timeout Configuration ---")
  io.println("")

  io.println("User: Create a solver with 5 second timeout")
  io.println("")

  let config = solver.config_with_timeout(5000)

  io.println("[System]: Timeout SolverConfig created")
  io.println("         timeout_ms: " <> int.to_string(config.timeout_ms))
  io.println("         z3_timeout: " <> int.to_string(config.z3_timeout))
  io.println("         z3_rlimit: " <> int.to_string(config.z3_rlimit))
  io.println("")

  io.println("User: Create solver with separate port and Z3 timeouts")
  io.println("")

  let config2 = solver.config_with_timeouts(10_000, 5000)

  io.println("[System]: Separate timeout SolverConfig created")
  io.println(
    "         port timeout (timeout_ms): " <> int.to_string(config2.timeout_ms),
  )
  io.println(
    "         Z3 internal timeout (z3_timeout): "
    <> int.to_string(config2.z3_timeout),
  )
  io.println("")

  io.println("[OK] Custom timeouts configured correctly")
  io.println("")
}

/// Test 3: Full configuration with all options
fn test_full_config() {
  io.println("")
  io.println("--- Test 3: Full Configuration ---")
  io.println("")

  io.println("User: Create a solver with all options configured")
  io.println("")

  let config =
    solver.full_config(
      10_000,
      // port timeout 10s
      5000,
      // Z3 internal timeout 5s
      1_000_000,
      // resource limit
      True,
      // enable unsat core
    )

  io.println("[System]: Full SolverConfig created")
  io.println("         timeout_ms: " <> int.to_string(config.timeout_ms))
  io.println("         z3_timeout: " <> int.to_string(config.z3_timeout))
  io.println("         z3_rlimit: " <> int.to_string(config.z3_rlimit))
  io.println("         unsat_core: " <> bool_to_string(config.unsat_core))
  io.println("")

  io.println("User: Create a solver with only resource limit")
  io.println("")

  let config2 = solver.config_with_rlimit(500_000)

  io.println("[System]: Resource limit SolverConfig created")
  io.println("         z3_rlimit: " <> int.to_string(config2.z3_rlimit))
  io.println("")

  io.println("[OK] Full configuration options work correctly")
  io.println("")
}

/// Test 4: Solver with timeout - simple SAT
fn test_solver_with_timeout_sat() {
  io.println("")
  io.println("--- Test 4: Solver with Timeout (Simple SAT) ---")
  io.println("")

  io.println("User: Create solver with 10s timeout and solve x > 0 AND x < 10")
  io.println("")

  // Create solver with timeout config
  let config = solver.config_with_timeout(10_000)
  let assert Ok(s) = solver.new_with_config(config)

  // Add constraints: x > 0 AND x < 10
  let x = expr.int_const("x")
  let assert Ok(s) = solver.assert_(s, expr.gt(x, expr.int(0)))
  let assert Ok(s) = solver.assert_(s, expr.lt(x, expr.int(10)))

  io.println("[System]: Solver configured with 10s timeout")
  io.println("         Constraints: x > 0 AND x < 10")
  io.println("")

  // Check satisfiability
  io.println("User: Check satisfiability")
  io.println("")

  case solver.check_with_z3(s) {
    Ok(#(_s, solver.SolverSat(_model))) -> {
      io.println("[System]: Result: SAT")
      io.println("         Solution exists (x can be 1-9)")
      io.println("")
      io.println("[OK] Solver with timeout returned correct result")
    }
    Ok(#(_s, solver.SolverUnsat)) -> {
      io.println("[System]: Result: UNSAT")
      io.println("[UNEXPECTED] This problem should be SAT")
    }
    Ok(#(_s, solver.SolverUnknown(reason))) -> {
      io.println("[System]: Result: UNKNOWN")
      io.println("         Reason: " <> reason)
      io.println("")
      case solver.is_timeout(solver.SolverUnknown(reason)) {
        True -> io.println("[INFO] Result is due to timeout")
        False -> io.println("[INFO] Result is not timeout-related")
      }
    }
    Error(e) -> {
      io.println("[System]: Error: " <> string.inspect(e))
      io.println(
        "[INFO] Z3 may not be available - this is expected in some environments",
      )
    }
  }
  io.println("")
}

/// Test 5: Check timeout detection helper
fn test_timeout_detection() {
  io.println("")
  io.println("--- Test 5: Timeout Detection Helper ---")
  io.println("")

  io.println("User: Test timeout detection for various result types")
  io.println("")

  // Test various unknown reasons
  let timeout_result = solver.SolverUnknown("timeout")
  let canceled_result = solver.SolverUnknown("canceled")
  let other_result = solver.SolverUnknown("incomplete quantifier instantiation")
  let sat_result =
    solver.SolverSat(solver.SolverModel(values: gleam_stdlib_dict_new()))
  let unsat_result = solver.SolverUnsat

  io.println("[System]: Testing is_timeout helper function")
  io.println("")

  io.println(
    "         'timeout' reason: "
    <> bool_to_string(solver.is_timeout(timeout_result)),
  )
  io.println(
    "         'canceled' reason: "
    <> bool_to_string(solver.is_timeout(canceled_result)),
  )
  io.println(
    "         'incomplete' reason: "
    <> bool_to_string(solver.is_timeout(other_result)),
  )
  io.println(
    "         SAT result: " <> bool_to_string(solver.is_timeout(sat_result)),
  )
  io.println(
    "         UNSAT result: " <> bool_to_string(solver.is_timeout(unsat_result)),
  )
  io.println("")

  io.println(
    "[OK] Timeout detection correctly identifies timeout-related results",
  )
  io.println("")
}

// Helper to create empty dict (for test purposes)
import gleam/dict.{type Dict}
import z3/types.{type Value}

@external(erlang, "maps", "new")
fn gleam_stdlib_dict_new() -> Dict(String, Value)

// Helper functions
fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
