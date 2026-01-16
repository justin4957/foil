//// Z3 Availability Detection Module
////
//// This module provides functions to detect whether Z3 is available
//// and to provide user-friendly error messages with installation instructions.
////
//// ## Usage
////
//// ```gleam
//// import z3/availability
////
//// pub fn main() {
////   case availability.check_z3() {
////     availability.Available(version) -> {
////       io.println("Z3 is available: " <> version)
////     }
////     availability.NotAvailable(reason) -> {
////       io.println("Z3 not available: " <> reason)
////       io.println(availability.installation_instructions())
////     }
////   }
//// }
//// ```

import gleam/option.{type Option, None, Some}
import gleam/result
import z3/types.{type Z3Error}

// =============================================================================
// Types
// =============================================================================

/// Z3 availability status
pub type Z3Availability {
  /// Z3 is available with the specified version
  Available(version: String)
  /// Z3 is not available with the reason why
  NotAvailable(reason: String)
}

/// Detailed availability information
pub type Z3Info {
  Z3Info(
    /// Whether Z3 is available
    available: Bool,
    /// Z3 version if available
    version: Option(String),
    /// Python version being used
    python_version: Option(String),
    /// Path to the Z3 driver script
    driver_path: Option(String),
    /// Error message if not available
    error: Option(String),
  )
}

/// Degradation mode for when Z3 is unavailable
pub type DegradationMode {
  /// Fail immediately with an error
  FailFast
  /// Return Unknown results for all checks
  ReturnUnknown
  /// Attempt to use external Z3 binary via SMT-LIB
  UseExternalBinary
}

// =============================================================================
// Availability Checking
// =============================================================================

/// Check if Z3 is available
///
/// This function attempts to start the Z3 port driver and check for availability.
/// It returns either the version string or an error reason.
///
/// ## Example
/// ```gleam
/// case check_z3() {
///   Available(version) -> io.println("Z3 v" <> version)
///   NotAvailable(reason) -> io.println("Error: " <> reason)
/// }
/// ```
pub fn check_z3() -> Z3Availability {
  case do_check_z3() {
    Ok(version) -> Available(version)
    Error(reason) -> NotAvailable(reason)
  }
}

/// Get detailed Z3 availability information
///
/// Returns comprehensive information about Z3 availability including
/// version, Python version, and driver path.
pub fn get_z3_info() -> Z3Info {
  case do_check_z3_detailed() {
    Ok(#(version, python_version, driver_path)) ->
      Z3Info(
        available: True,
        version: Some(version),
        python_version: Some(python_version),
        driver_path: Some(driver_path),
        error: None,
      )
    Error(reason) ->
      Z3Info(
        available: False,
        version: None,
        python_version: None,
        driver_path: None,
        error: Some(reason),
      )
  }
}

/// Check if Z3 is available (simple boolean check)
pub fn is_z3_available() -> Bool {
  case check_z3() {
    Available(_) -> True
    NotAvailable(_) -> False
  }
}

/// Get the Z3 version if available
pub fn get_z3_version() -> Option(String) {
  case check_z3() {
    Available(version) -> Some(version)
    NotAvailable(_) -> None
  }
}

// =============================================================================
// Installation Instructions
// =============================================================================

/// Get user-friendly installation instructions
///
/// Returns a multi-line string with installation instructions for
/// different operating systems.
pub fn installation_instructions() -> String {
  "Z3 solver is not available. For full validation support, install Z3:

macOS:
  brew install z3
  pip3 install z3-solver

Ubuntu/Debian:
  sudo apt install z3
  pip3 install z3-solver

Windows:
  pip install z3-solver

After installation, verify with:
  python3 -c \"from z3 import *; print('Z3 OK')\"

Note: Validation will return Unknown results until Z3 is installed."
}

/// Get a brief error message for Z3 unavailability
pub fn brief_error_message() -> String {
  "Z3 solver not available. Install with: pip3 install z3-solver"
}

/// Get platform-specific installation command
pub fn platform_install_command() -> String {
  // This is a simplified version - in practice you might detect the OS
  "pip3 install z3-solver"
}

// =============================================================================
// Degradation Mode Functions
// =============================================================================

/// Create a message for graceful degradation
pub fn degradation_message(mode: DegradationMode) -> String {
  case mode {
    FailFast ->
      "Z3 solver is required but not available. Please install Z3 to continue."
    ReturnUnknown ->
      "Z3 solver not available. Validation results will be marked as Unknown."
    UseExternalBinary ->
      "Z3 solver not available. Attempting to use external z3 binary."
  }
}

/// Get the result message for unavailable Z3 with specific mode
pub fn unavailable_result_message(
  mode: DegradationMode,
  reason: String,
) -> String {
  case mode {
    FailFast -> "Z3 required but unavailable: " <> reason
    ReturnUnknown -> "Validation inconclusive (Z3 unavailable): " <> reason
    UseExternalBinary -> "External Z3 fallback failed: " <> reason
  }
}

// =============================================================================
// FFI Functions
// =============================================================================

/// Internal function to check Z3 availability
@external(erlang, "z3_availability_ffi", "check_z3")
fn do_check_z3() -> Result(String, String)

/// Internal function to get detailed Z3 info
@external(erlang, "z3_availability_ffi", "check_z3_detailed")
fn do_check_z3_detailed() -> Result(#(String, String, String), String)
