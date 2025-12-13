//// Modal Logic Analyst - Main Application
////
//// This application integrates:
//// - LLM translation via anthropic_gleam
//// - Z3 validation via z3_gleam
//// - Core domain types from modal_logic
////
//// The execution loop:
//// 1. Translate natural language arguments to logical structures
//// 2. Validate formalizations using Z3
//// 3. Generate countermodels for invalid arguments
//// 4. Suggest repairs to strengthen arguments

import gleam/io
import modal_logic/proposition
import modal_logic/argument

pub fn main() -> Nil {
  io.println("Modal Logic Analyst v0.1.0")
  io.println("===========================")
  io.println("")
  io.println("Available packages:")
  io.println("  - anthropic_gleam: LLM client for Claude API")
  io.println("  - z3_gleam: Z3 theorem prover bindings")
  io.println("  - modal_logic: Core domain types")
  io.println("")
  io.println("Ready for development!")

  // Example: Create a simple proposition
  let example_proposition = proposition.Implies(
    proposition.Atom("rain"),
    proposition.Necessary(proposition.Atom("wet"))
  )

  io.println("")
  io.debug(example_proposition)
}
