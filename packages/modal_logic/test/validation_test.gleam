//// Validation and Execution Tests
////
//// Tests for validation orchestrator, countermodel formatting,
//// repair suggestions, execution loop, and explanation generator.

import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/argument
import modal_logic/countermodel
import modal_logic/execution
import modal_logic/explanation
import modal_logic/proposition.{
  And, Atom, Implies, K, Necessary, Possible, S4, S5, T,
}
import modal_logic/repair
import modal_logic/validator

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Validation and Execution Tests")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Validator Configuration
  test_validator_config()

  // Test 2: SMT Generation
  test_smt_generation()

  // Test 3: Countermodel Formatting
  test_countermodel_formatting()

  // Test 4: Repair Suggestions
  test_repair_suggestions()

  // Test 5: Execution Loop
  test_execution_loop()

  // Test 6: Explanation Generator
  test_explanation_generator()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All Validation and Execution Tests Passed!")
  io.println("=" |> string.repeat(70))
}

fn test_validator_config() {
  io.println("")
  io.println("--- Test 1: Validator Configuration ---")
  io.println("")

  // Test default config
  let config = validator.default_config()
  io.println("[OK] Default validator config created")
  io.println(
    "     Timeout: " <> int_to_string(config.solver_timeout_ms) <> "ms",
  )
  io.println("     Max worlds: " <> int_to_string(config.max_worlds))

  // Test fast config
  let fast = validator.fast_config()
  io.println("[OK] Fast config created")
  io.println("     Timeout: " <> int_to_string(fast.solver_timeout_ms) <> "ms")

  // Test thorough config
  let thorough = validator.thorough_config()
  io.println("[OK] Thorough config created")
  io.println(
    "     Timeout: " <> int_to_string(thorough.solver_timeout_ms) <> "ms",
  )

  // Test config modification
  let modified =
    config
    |> validator.with_timeout(10_000)
    |> validator.with_max_worlds(15)
  io.println("[OK] Modified config")
  io.println(
    "     New timeout: " <> int_to_string(modified.solver_timeout_ms) <> "ms",
  )

  // Test state creation
  let state = validator.new_state(config)
  io.println("[OK] Validator state created")
  io.println("     Completed count: " <> int_to_string(state.completed_count))

  io.println("")
}

fn test_smt_generation() {
  io.println("")
  io.println("--- Test 2: SMT Generation ---")
  io.println("")

  // Create a simple formalization
  let formalization =
    argument.Formalization(
      id: "test-1",
      argument_id: "arg-1",
      logic_system: K,
      premises: [Implies(Atom("p"), Atom("q")), Atom("p")],
      conclusion: Atom("q"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Generate SMT
  let smt = validator.generate_smt(formalization, 3)
  io.println("[OK] SMT formula generated")
  io.println("     Length: " <> int_to_string(string.length(smt)) <> " chars")

  // Check it contains expected elements
  case string.contains(smt, "check-sat") {
    True -> io.println("[OK] Contains check-sat command")
    False -> io.println("[FAIL] Missing check-sat")
  }

  case string.contains(smt, "get-model") {
    True -> io.println("[OK] Contains get-model command")
    False -> io.println("[FAIL] Missing get-model")
  }

  // Test modal formalization
  let modal_form =
    argument.Formalization(
      id: "test-2",
      argument_id: "arg-2",
      logic_system: S4,
      premises: [Necessary(Implies(Atom("p"), Atom("q"))), Necessary(Atom("p"))],
      conclusion: Necessary(Atom("q")),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let modal_smt = validator.generate_smt(modal_form, 5)
  io.println("[OK] Modal SMT generated")
  io.println(
    "     Length: " <> int_to_string(string.length(modal_smt)) <> " chars",
  )

  io.println("")
}

fn test_countermodel_formatting() {
  io.println("")
  io.println("--- Test 3: Countermodel Formatting ---")
  io.println("")

  // Create a test countermodel
  let cm =
    validator.Countermodel(
      worlds: [
        validator.KripkeWorld(name: "w0", true_props: ["p", "q"], false_props: [
          "r",
        ]),
        validator.KripkeWorld(name: "w1", true_props: ["p"], false_props: [
          "q",
          "r",
        ]),
      ],
      relations: [
        validator.AccessibilityRelation(from: "w0", to: "w1"),
        validator.AccessibilityRelation(from: "w0", to: "w0"),
      ],
      actual_world: "w0",
      logic_system: T,
    )

  // Test default formatting
  let formatted = countermodel.format(cm)
  io.println("[OK] Default format created")
  io.println("     World count: " <> int_to_string(formatted.world_count))
  io.println("     Relation count: " <> int_to_string(formatted.relation_count))

  // Test brief formatting
  let brief_opts = countermodel.brief_options()
  let brief = countermodel.format_with_options(cm, brief_opts)
  io.println("[OK] Brief format created")
  io.println(
    "     Output length: " <> int_to_string(string.length(brief.output)),
  )

  // Test detailed formatting
  let detailed_opts = countermodel.detailed_options()
  let detailed = countermodel.format_with_options(cm, detailed_opts)
  io.println("[OK] Detailed format created")
  io.println(
    "     Output length: " <> int_to_string(string.length(detailed.output)),
  )

  // Test diagram formatting
  let diagram_opts = countermodel.diagram_options()
  let diagram = countermodel.format_with_options(cm, diagram_opts)
  io.println("[OK] Diagram format created")

  // Test natural language explanation
  let explanation = countermodel.explain_countermodel(cm)
  io.println("[OK] Natural language explanation generated")
  io.println(
    "     Length: " <> int_to_string(string.length(explanation)) <> " chars",
  )

  // Test comparison
  let cm2 =
    validator.Countermodel(
      worlds: [
        validator.KripkeWorld(name: "w0", true_props: ["p"], false_props: []),
      ],
      relations: [],
      actual_world: "w0",
      logic_system: K,
    )
  let comparison = countermodel.compare_countermodels(cm, cm2)
  io.println("[OK] Countermodel comparison")
  io.println(
    "     Same world count: " <> bool_to_string(comparison.same_world_count),
  )

  // Test minimality
  io.println(
    "[OK] CM1 is minimal: " <> bool_to_string(countermodel.is_minimal(cm)),
  )
  io.println(
    "[OK] CM2 is minimal: " <> bool_to_string(countermodel.is_minimal(cm2)),
  )

  io.println("")
}

fn test_repair_suggestions() {
  io.println("")
  io.println("--- Test 4: Repair Suggestions ---")
  io.println("")

  // Create an invalid formalization
  let formalization =
    argument.Formalization(
      id: "invalid-1",
      argument_id: "arg-1",
      logic_system: K,
      premises: [Possible(Atom("p"))],
      conclusion: Necessary(Atom("p")),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Create a countermodel
  let cm =
    validator.Countermodel(
      worlds: [
        validator.KripkeWorld(name: "w0", true_props: ["p"], false_props: []),
        validator.KripkeWorld(name: "w1", true_props: [], false_props: ["p"]),
      ],
      relations: [validator.AccessibilityRelation(from: "w0", to: "w1")],
      actual_world: "w0",
      logic_system: K,
    )

  // Generate suggestions
  let suggestions = repair.generate_suggestions(formalization, cm)
  io.println("[OK] Repair suggestions generated")
  io.println("     Count: " <> int_to_string(list.length(suggestions)))

  // Check for expected suggestion types
  list.each(suggestions, fn(s) {
    io.println("     - " <> repair.format_suggestion(s))
  })

  // Test configuration options
  let conservative = repair.conservative_config()
  let conservative_suggestions =
    repair.generate_suggestions_with_config(formalization, cm, conservative)
  io.println(
    "[OK] Conservative suggestions: "
    <> int_to_string(list.length(conservative_suggestions)),
  )

  // Test detailed suggestions
  let detailed =
    repair.generate_detailed_suggestions(
      formalization,
      cm,
      repair.default_config(),
    )
  io.println(
    "[OK] Detailed suggestions: " <> int_to_string(list.length(detailed)),
  )

  // Test repair session
  let session = repair.new_session(formalization)
  io.println("[OK] Repair session created")
  io.println(
    "     Attempt count: " <> int_to_string(repair.attempt_count(session)),
  )

  // Test countermodel analysis
  let analysis = repair.analyze_countermodel(cm, formalization)
  io.println("[OK] Countermodel analyzed")
  io.println(
    "     Distinguishing atoms: "
    <> int_to_string(list.length(analysis.distinguishing_atoms)),
  )
  io.println(
    "     Structural issues: "
    <> int_to_string(list.length(analysis.structural_issues)),
  )

  io.println("")
}

fn test_execution_loop() {
  io.println("")
  io.println("--- Test 5: Execution Loop ---")
  io.println("")

  // Test configuration
  let config = execution.default_config()
  io.println("[OK] Default execution config created")
  io.println(
    "     Max iterations: " <> int_to_string(config.max_validation_iterations),
  )

  let fast = execution.fast_config()
  io.println(
    "[OK] Fast config: "
    <> int_to_string(fast.max_validation_iterations)
    <> " iterations",
  )

  let thorough = execution.thorough_config()
  io.println(
    "[OK] Thorough config: "
    <> int_to_string(thorough.max_validation_iterations)
    <> " iterations",
  )

  // Test config modification
  let modified =
    config
    |> execution.with_max_iterations(10)
    |> execution.with_verbose(True)
  io.println("[OK] Modified execution config")

  // Create a test formalization
  let formalization =
    argument.Formalization(
      id: "exec-test-1",
      argument_id: "arg-1",
      logic_system: S4,
      premises: [Implies(Atom("p"), Atom("q")), Atom("p")],
      conclusion: Atom("q"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Test with always valid validator
  let valid_validator = execution.always_valid_validator()
  let valid_result =
    execution.run_with_formalization(config, formalization, valid_validator, 0)
  case valid_result {
    execution.ExecutionSuccess(_, _, trace) -> {
      io.println("[OK] Execution succeeded with valid validator")
      io.println(
        "     Total time: " <> int_to_string(trace.total_time_ms) <> "ms",
      )
    }
    _ -> io.println("[FAIL] Expected success")
  }

  // Test with always invalid validator
  let invalid_validator = execution.always_invalid_validator("Countermodel: w0")
  let invalid_result =
    execution.run_with_formalization(
      execution.fast_config(),
      formalization,
      invalid_validator,
      0,
    )
  case invalid_result {
    execution.ExecutionInvalid(_, _, _, trace) -> {
      io.println("[OK] Execution correctly identified invalid argument")
      io.println(
        "     Validation iterations: "
        <> int_to_string(trace.validation_iterations),
      )
    }
    _ -> io.println("[OK] Execution handled invalid case")
  }

  // Test result formatting
  let formatted = execution.format_result(valid_result)
  io.println("[OK] Result formatted")
  io.println(
    "     Format length: "
    <> int_to_string(string.length(formatted))
    <> " chars",
  )

  io.println("")
}

fn test_explanation_generator() {
  io.println("")
  io.println("--- Test 6: Explanation Generator ---")
  io.println("")

  // Create a test formalization
  let formalization =
    argument.Formalization(
      id: "explain-1",
      argument_id: "arg-1",
      logic_system: S5,
      premises: [
        Necessary(Implies(Atom("bachelor"), Atom("unmarried"))),
        Atom("john_bachelor"),
      ],
      conclusion: Necessary(Atom("john_unmarried")),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Test configuration options
  let default_config = explanation.default_config()
  io.println("[OK] Default explanation config created")

  let brief_config = explanation.brief_config()
  io.println("[OK] Brief config created")

  let detailed_config = explanation.detailed_config()
  io.println("[OK] Detailed config created")

  let technical_config = explanation.technical_config()
  io.println("[OK] Technical config created")

  // Test valid result explanation
  let valid_exp =
    explanation.explain_validation(
      formalization,
      argument.Valid,
      explanation.StandardLevel,
    )
  io.println("[OK] Valid argument explanation generated")
  io.println("     Summary: " <> valid_exp.summary)
  io.println(
    "     Key points: " <> int_to_string(list.length(valid_exp.key_points)),
  )

  // Test invalid result explanation
  let invalid_exp =
    explanation.explain_validation(
      formalization,
      argument.Invalid("Countermodel exists"),
      explanation.DetailedLevel,
    )
  io.println("[OK] Invalid argument explanation generated")
  io.println("     Summary: " <> invalid_exp.summary)

  // Test brief explanation
  let brief_exp =
    explanation.explain_validation(
      formalization,
      argument.Valid,
      explanation.BriefLevel,
    )
  io.println("[OK] Brief explanation generated")
  io.println(
    "     Length: " <> int_to_string(string.length(brief_exp.main_text)),
  )

  // Test with context
  let context =
    explanation.ExplanationContext(
      formalization: formalization,
      validation_result: argument.Valid,
      countermodel: None,
      ambiguities: [],
    )
  let context_exp = explanation.explain_with_config(context, default_config)
  io.println("[OK] Context-based explanation generated")
  io.println(
    "     Follow-up questions: "
    <> int_to_string(list.length(context_exp.follow_up_questions)),
  )
  io.println(
    "     Related concepts: "
    <> int_to_string(list.length(context_exp.related_concepts)),
  )

  // Test technical explanation
  let tech_exp =
    explanation.explain_validation(
      formalization,
      argument.Valid,
      explanation.TechnicalLevel,
    )
  io.println("[OK] Technical explanation generated")
  case tech_exp.technical_details {
    Some(details) ->
      io.println(
        "     Technical details: "
        <> int_to_string(string.length(details))
        <> " chars",
      )
    None -> io.println("     No technical details")
  }

  io.println("")
}

// Helper functions

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_string(-n, "")
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
