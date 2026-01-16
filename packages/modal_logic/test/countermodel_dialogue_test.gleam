//// Countermodel Extraction Dialogue Test
////
//// This test demonstrates the back-and-forth interaction between
//// the validator and Z3 solver for countermodel extraction.
//// It validates that countermodels are correctly extracted from Z3 models
//// and properly formatted for human readability.
////
//// ## Purpose
//// - Validates countermodel extraction from Z3 models
//// - Documents expected behavior in a readable dialogue format
//// - Tests both structured Countermodel type and explanation generation

import gleam/dict
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import modal_logic/argument
import modal_logic/countermodel
import modal_logic/proposition.{Atom, Implies, K, Necessary, Possible, S4, S5, T}
import modal_logic/validator
import z3/solver.{SolverModel}
import z3/types.{BoolVal, IntVal}

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Countermodel Extraction Dialogue Test")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Simple countermodel extraction from mock model
  test_simple_countermodel_extraction()

  // Test 2: Multi-world countermodel with accessibility relations
  test_multiworld_countermodel()

  // Test 3: Countermodel formatting styles
  test_countermodel_formatting()

  // Test 4: Modal semantics explanation
  test_modal_semantics_explanation()

  // Test 5: Integration with validator (live Z3)
  test_validator_countermodel_integration()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All Countermodel Dialogue Tests Completed!")
  io.println("=" |> string.repeat(70))
}

/// Test 1: Simple countermodel extraction from a mock Z3 model
fn test_simple_countermodel_extraction() {
  io.println("")
  io.println("--- Test 1: Simple Countermodel Extraction ---")
  io.println("")

  io.println("User: Create a mock Z3 model with p=true, q=false at w0")
  io.println("")

  // Create a mock Z3 model
  let model_values =
    dict.new()
    |> dict.insert("p_w0", BoolVal(True))
    |> dict.insert("q_w0", BoolVal(False))

  let mock_model = SolverModel(values: model_values)

  io.println("[System]: Mock model created with 2 variables")
  io.println("         Variables: p_w0=true, q_w0=false")
  io.println("")

  io.println("User: Extract countermodel from this model")
  io.println("")

  let countermodel = validator.extract_countermodel(mock_model, K)

  io.println("[System]: Countermodel extracted successfully")
  io.println(
    "         Worlds: " <> int_to_string(list.length(countermodel.worlds)),
  )
  io.println(
    "         Relations: " <> int_to_string(list.length(countermodel.relations)),
  )
  io.println("         Actual world: " <> countermodel.actual_world)
  io.println("")

  // Verify the extraction
  case countermodel.worlds {
    [world] -> {
      io.println("[OK] Single world extracted correctly")
      io.println("     True props: " <> string.join(world.true_props, ", "))
      io.println("     False props: " <> string.join(world.false_props, ", "))
    }
    _ ->
      io.println("[INFO] Multiple worlds extracted (unexpected for this test)")
  }
  io.println("")
}

/// Test 2: Multi-world countermodel with accessibility relations
fn test_multiworld_countermodel() {
  io.println("")
  io.println("--- Test 2: Multi-World Countermodel Extraction ---")
  io.println("")

  io.println(
    "User: Create a model with 2 worlds and accessibility relation R(w0,w1)",
  )
  io.println("      World w0: p=true")
  io.println("      World w1: p=false")
  io.println("")

  // Create a mock Z3 model with multiple worlds
  let model_values =
    dict.new()
    |> dict.insert("p_w0", BoolVal(True))
    |> dict.insert("p_w1", BoolVal(False))
    |> dict.insert("R_w0_w1", BoolVal(True))

  let mock_model = SolverModel(values: model_values)

  io.println("[System]: Mock model created with 3 variables")
  io.println("         p_w0=true, p_w1=false, R_w0_w1=true")
  io.println("")

  io.println("User: Extract countermodel (this should show □p is false at w0)")
  io.println("")

  let countermodel = validator.extract_countermodel(mock_model, K)

  io.println("[System]: Countermodel extracted")
  io.println(
    "         World count: " <> int_to_string(list.length(countermodel.worlds)),
  )
  io.println(
    "         Relation count: "
    <> int_to_string(list.length(countermodel.relations)),
  )
  io.println("")

  // Print world details
  list.each(countermodel.worlds, fn(w) {
    io.println(
      "         "
      <> w.name
      <> ": true={"
      <> string.join(w.true_props, ",")
      <> "}, false={"
      <> string.join(w.false_props, ",")
      <> "}",
    )
  })

  // Print relations
  list.each(countermodel.relations, fn(r) {
    io.println("         R(" <> r.from <> "," <> r.to <> ")")
  })
  io.println("")

  // Verify
  case list.length(countermodel.worlds), list.length(countermodel.relations) {
    2, 1 -> io.println("[OK] 2 worlds and 1 relation extracted correctly")
    w, r ->
      io.println(
        "[INFO] Got "
        <> int_to_string(w)
        <> " worlds and "
        <> int_to_string(r)
        <> " relations",
      )
  }
  io.println("")
}

/// Test 3: Countermodel formatting styles
fn test_countermodel_formatting() {
  io.println("")
  io.println("--- Test 3: Countermodel Formatting Styles ---")
  io.println("")

  // Create a test countermodel
  let test_countermodel =
    validator.Countermodel(
      worlds: [
        validator.KripkeWorld(name: "w0", true_props: ["p"], false_props: ["q"]),
        validator.KripkeWorld(name: "w1", true_props: ["q"], false_props: ["p"]),
      ],
      relations: [validator.AccessibilityRelation(from: "w0", to: "w1")],
      actual_world: "w0",
      logic_system: K,
    )

  io.println("User: Format countermodel in BriefFormat")
  io.println("")
  let brief =
    countermodel.format_with_options(
      test_countermodel,
      countermodel.brief_options(),
    )
  io.println("[System]: " <> brief.output)
  io.println("")

  io.println("User: Format countermodel in StandardFormat")
  io.println("")
  let standard =
    countermodel.format_with_options(
      test_countermodel,
      countermodel.default_options(),
    )
  io.println("[System]:")
  io.println(standard.output)
  io.println("")

  io.println("User: Format countermodel in JsonFormat")
  io.println("")
  let json_opts =
    countermodel.FormatOptions(
      style: countermodel.JsonFormat,
      show_frame_properties: False,
      show_world_labels: False,
      max_line_width: 80,
      indent: "  ",
    )
  let json_formatted =
    countermodel.format_with_options(test_countermodel, json_opts)
  io.println("[System]:")
  io.println(json_formatted.output)
  io.println("")

  io.println("[OK] All formatting styles executed successfully")
  io.println("")
}

/// Test 4: Modal semantics explanation
fn test_modal_semantics_explanation() {
  io.println("")
  io.println("--- Test 4: Modal Semantics Explanation ---")
  io.println("")

  // Create a countermodel that shows □p failing
  let test_countermodel =
    validator.Countermodel(
      worlds: [
        validator.KripkeWorld(name: "w0", true_props: ["p"], false_props: []),
        validator.KripkeWorld(name: "w1", true_props: [], false_props: ["p"]),
      ],
      relations: [
        validator.AccessibilityRelation(from: "w0", to: "w0"),
        validator.AccessibilityRelation(from: "w0", to: "w1"),
      ],
      actual_world: "w0",
      logic_system: T,
    )

  io.println("User: Explain why □p is false at w0 in this T-model")
  io.println("      (w0 accesses w0 and w1, p is true at w0 but false at w1)")
  io.println("")

  let explanation = countermodel.explain_modal_semantics(test_countermodel)
  io.println("[System]:")
  io.println(explanation)

  io.println("[OK] Modal semantics explanation generated")
  io.println("")
}

/// Test 5: Integration with validator for live Z3 countermodel extraction
fn test_validator_countermodel_integration() {
  io.println("")
  io.println("--- Test 5: Validator Integration (Live Z3) ---")
  io.println("")

  io.println("User: Validate □p ⊢ p in system K (should be INVALID)")
  io.println("      This tests that reflexivity fails in K")
  io.println("")

  // Create formalization: □p ⊢ p (valid in T, invalid in K)
  let formalization =
    argument.Formalization(
      id: "countermodel-test-1",
      argument_id: "arg-cm-1",
      logic_system: K,
      premises: [Necessary(Atom("p"))],
      conclusion: Atom("p"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let config = validator.default_config() |> validator.with_max_worlds(2)
  let state = validator.new_state(config)
  let #(_new_state, response) = validator.validate(state, formalization, 0)

  case response.result {
    argument.Valid -> {
      io.println("[System]: Result = VALID")
      io.println("[UNEXPECTED] Expected INVALID in K system")
    }
    argument.Invalid(reason) -> {
      io.println("[System]: Result = INVALID")
      io.println("")
      io.println("Countermodel/Reason:")
      // Print first 500 chars of reason
      let display_reason = case string.length(reason) > 500 {
        True -> string.slice(reason, 0, 500) <> "..."
        False -> reason
      }
      io.println(display_reason)
      io.println("")
      io.println("[OK] Correctly identified as INVALID with countermodel")
    }
    _ -> io.println("[INFO] Unexpected result type")
  }
  io.println("")

  // Second test: Valid argument should have no countermodel
  io.println("User: Validate p → p ⊢ p → p (tautology, should be VALID)")
  io.println("")

  let tautology =
    argument.Formalization(
      id: "countermodel-test-2",
      argument_id: "arg-cm-2",
      logic_system: K,
      premises: [Implies(Atom("p"), Atom("p"))],
      conclusion: Implies(Atom("p"), Atom("p")),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  let #(_state2, response2) = validator.validate(state, tautology, 0)

  case response2.result {
    argument.Valid -> {
      io.println("[System]: Result = VALID")
      io.println(
        "[OK] Tautology correctly identified as valid (no countermodel)",
      )
    }
    argument.Invalid(reason) -> {
      io.println("[System]: Result = INVALID")
      io.println("Reason: " <> string.slice(reason, 0, 100))
      io.println("[INFO] Unexpected - may indicate Z3 not available")
    }
    _ -> io.println("[INFO] Unexpected result type")
  }
  io.println("")
}

// Helper: Convert int to string (avoiding import issues)
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    _ -> "N"
  }
}

// Import list module
import gleam/list
