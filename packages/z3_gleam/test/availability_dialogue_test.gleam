//// Z3 Availability Detection Dialogue Test
////
//// This test demonstrates the back-and-forth interaction for
//// detecting Z3 availability and handling graceful degradation.
////
//// ## Purpose
//// - Validates Z3 availability detection
//// - Documents expected behavior for available/unavailable scenarios
//// - Tests installation instructions formatting
//// - Demonstrates graceful degradation modes

import gleam/io
import gleam/string
import z3/availability

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Z3 Availability Detection Dialogue Test")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Check Z3 availability
  test_check_z3_availability()

  // Test 2: Get detailed Z3 info
  test_get_z3_info()

  // Test 3: Installation instructions
  test_installation_instructions()

  // Test 4: Degradation modes
  test_degradation_modes()

  // Test 5: Simple boolean check
  test_simple_availability_check()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All Z3 Availability Dialogue Tests Completed!")
  io.println("=" |> string.repeat(70))
}

/// Test 1: Check Z3 availability
fn test_check_z3_availability() {
  io.println("")
  io.println("--- Test 1: Check Z3 Availability ---")
  io.println("")

  io.println("User: Check if Z3 solver is available")
  io.println("")

  let result = availability.check_z3()

  case result {
    availability.Available(version) -> {
      io.println("[System]: Z3 is AVAILABLE")
      io.println("         Version: " <> version)
      io.println("")
      io.println("[OK] Z3 detected successfully")
    }
    availability.NotAvailable(reason) -> {
      io.println("[System]: Z3 is NOT AVAILABLE")
      io.println("         Reason: " <> string.slice(reason, 0, 60) <> "...")
      io.println("")
      io.println(
        "[INFO] Z3 not installed - this is expected in some environments",
      )
    }
  }
  io.println("")
}

/// Test 2: Get detailed Z3 info
fn test_get_z3_info() {
  io.println("")
  io.println("--- Test 2: Get Detailed Z3 Info ---")
  io.println("")

  io.println("User: Get detailed Z3 information")
  io.println("")

  let info = availability.get_z3_info()

  io.println("[System]: Z3 Info")
  io.println("         Available: " <> bool_to_string(info.available))

  case info.version {
    option.Some(v) -> io.println("         Z3 Version: " <> v)
    option.None -> io.println("         Z3 Version: (not available)")
  }

  case info.python_version {
    option.Some(v) -> io.println("         Python Version: " <> v)
    option.None -> io.println("         Python Version: (not available)")
  }

  case info.driver_path {
    option.Some(p) -> io.println("         Driver Path: " <> p)
    option.None -> io.println("         Driver Path: (not found)")
  }

  case info.error {
    option.Some(e) -> io.println("         Error: " <> e)
    option.None -> Nil
  }

  io.println("")
  io.println("[OK] Detailed info retrieved")
  io.println("")
}

/// Test 3: Installation instructions
fn test_installation_instructions() {
  io.println("")
  io.println("--- Test 3: Installation Instructions ---")
  io.println("")

  io.println("User: Show Z3 installation instructions")
  io.println("")

  let instructions = availability.installation_instructions()

  io.println("[System]:")
  io.println(instructions)
  io.println("")

  io.println("User: Show brief error message")
  io.println("")

  let brief = availability.brief_error_message()
  io.println("[System]: " <> brief)
  io.println("")

  io.println("User: Show platform install command")
  io.println("")

  let cmd = availability.platform_install_command()
  io.println("[System]: " <> cmd)
  io.println("")

  io.println("[OK] Installation instructions displayed")
  io.println("")
}

/// Test 4: Degradation modes
fn test_degradation_modes() {
  io.println("")
  io.println("--- Test 4: Degradation Mode Messages ---")
  io.println("")

  io.println("User: What happens in FailFast mode?")
  io.println("")
  let fail_fast_msg = availability.degradation_message(availability.FailFast)
  io.println("[System]: " <> fail_fast_msg)
  io.println("")

  io.println("User: What happens in ReturnUnknown mode?")
  io.println("")
  let return_unknown_msg =
    availability.degradation_message(availability.ReturnUnknown)
  io.println("[System]: " <> return_unknown_msg)
  io.println("")

  io.println("User: What happens in UseExternalBinary mode?")
  io.println("")
  let external_msg =
    availability.degradation_message(availability.UseExternalBinary)
  io.println("[System]: " <> external_msg)
  io.println("")

  io.println("User: Get unavailable result message for FailFast")
  io.println("")
  let result_msg =
    availability.unavailable_result_message(
      availability.FailFast,
      "Z3 Python bindings not installed",
    )
  io.println("[System]: " <> result_msg)
  io.println("")

  io.println("[OK] Degradation mode messages displayed")
  io.println("")
}

/// Test 5: Simple boolean availability check
fn test_simple_availability_check() {
  io.println("")
  io.println("--- Test 5: Simple Boolean Check ---")
  io.println("")

  io.println("User: Is Z3 available? (yes/no)")
  io.println("")

  let is_available = availability.is_z3_available()

  case is_available {
    True -> {
      io.println("[System]: Yes, Z3 is available")

      case availability.get_z3_version() {
        option.Some(v) -> io.println("         Version: " <> v)
        option.None -> Nil
      }
    }
    False -> {
      io.println("[System]: No, Z3 is not available")
      io.println("         Use 'pip3 install z3-solver' to install")
    }
  }
  io.println("")

  io.println("[OK] Simple availability check completed")
  io.println("")
}

// Helper functions
import gleam/option

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
