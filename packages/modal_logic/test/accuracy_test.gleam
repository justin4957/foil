//// Entry point for accuracy testing
////
//// Run with: gleam run -m accuracy_test

import gleam/io
import modal_logic/testing/accuracy/accuracy_tests
import modal_logic/testing/fixtures/fixtures

pub fn main() {
  io.println("Running Accuracy Tests...")
  io.println("")

  // Get all fixtures
  let all_fixtures = fixtures.all_fixtures()

  // Run accuracy tests
  let results = accuracy_tests.run_accuracy_tests(all_fixtures)

  // Print report
  let report = accuracy_tests.format_report(results)
  io.println(report)
}
