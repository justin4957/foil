//// Countermodel Formatting
////
//// This module provides human-readable formatting of countermodels.
//// Countermodels are Kripke structures that demonstrate why an argument
//// is invalid by showing worlds where premises are true but conclusion is false.
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/countermodel
//// import modal_logic/validator.{Countermodel, KripkeWorld}
////
//// let formatted = countermodel.format(model, DetailedFormat)
//// ```

import gleam/list
import gleam/string
import modal_logic/proposition.{type LogicSystem}
import modal_logic/validator.{
  type AccessibilityRelation, type Countermodel, type KripkeWorld,
}

// =============================================================================
// Types
// =============================================================================

/// Format style for countermodel output
pub type FormatStyle {
  /// Brief one-line summary
  BriefFormat
  /// Standard multi-line format
  StandardFormat
  /// Detailed format with explanations
  DetailedFormat
  /// ASCII art diagram
  DiagramFormat
  /// JSON format for serialization
  JsonFormat
}

/// Formatting options
pub type FormatOptions {
  FormatOptions(
    /// Format style
    style: FormatStyle,
    /// Include frame property annotations
    show_frame_properties: Bool,
    /// Include world labels
    show_world_labels: Bool,
    /// Maximum line width for wrapping
    max_line_width: Int,
    /// Indentation string
    indent: String,
  )
}

/// A formatted countermodel with metadata
pub type FormattedCountermodel {
  FormattedCountermodel(
    /// The formatted string output
    output: String,
    /// Number of worlds
    world_count: Int,
    /// Number of relations
    relation_count: Int,
    /// Summary of why argument is invalid
    invalidity_reason: String,
  )
}

/// World evaluation showing truth values
pub type WorldEvaluation {
  WorldEvaluation(
    /// World name
    world: String,
    /// Propositions true in this world
    true_atoms: List(String),
    /// Propositions false in this world
    false_atoms: List(String),
    /// Whether premises hold in this world
    premises_hold: Bool,
    /// Whether conclusion holds in this world
    conclusion_holds: Bool,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default formatting options
pub fn default_options() -> FormatOptions {
  FormatOptions(
    style: StandardFormat,
    show_frame_properties: True,
    show_world_labels: True,
    max_line_width: 80,
    indent: "  ",
  )
}

/// Create brief formatting options
pub fn brief_options() -> FormatOptions {
  FormatOptions(
    style: BriefFormat,
    show_frame_properties: False,
    show_world_labels: False,
    max_line_width: 120,
    indent: "",
  )
}

/// Create detailed formatting options
pub fn detailed_options() -> FormatOptions {
  FormatOptions(
    style: DetailedFormat,
    show_frame_properties: True,
    show_world_labels: True,
    max_line_width: 80,
    indent: "  ",
  )
}

/// Create diagram formatting options
pub fn diagram_options() -> FormatOptions {
  FormatOptions(
    style: DiagramFormat,
    show_frame_properties: True,
    show_world_labels: True,
    max_line_width: 100,
    indent: "  ",
  )
}

// =============================================================================
// Formatting Functions
// =============================================================================

/// Format a countermodel with default options
pub fn format(countermodel: Countermodel) -> FormattedCountermodel {
  format_with_options(countermodel, default_options())
}

/// Format a countermodel with custom options
pub fn format_with_options(
  countermodel: Countermodel,
  options: FormatOptions,
) -> FormattedCountermodel {
  let output = case options.style {
    BriefFormat -> format_brief(countermodel)
    StandardFormat -> format_standard(countermodel, options)
    DetailedFormat -> format_detailed(countermodel, options)
    DiagramFormat -> format_diagram(countermodel, options)
    JsonFormat -> format_json(countermodel)
  }

  let reason = generate_invalidity_reason(countermodel)

  FormattedCountermodel(
    output: output,
    world_count: list.length(countermodel.worlds),
    relation_count: list.length(countermodel.relations),
    invalidity_reason: reason,
  )
}

/// Format as brief one-line summary
fn format_brief(countermodel: Countermodel) -> String {
  let world_count = list.length(countermodel.worlds)
  let relation_count = list.length(countermodel.relations)

  "Countermodel: "
  <> int_to_string(world_count)
  <> " world(s), "
  <> int_to_string(relation_count)
  <> " relation(s). "
  <> "Actual world: "
  <> countermodel.actual_world
  <> " where premises true, conclusion false."
}

/// Format as standard multi-line output
fn format_standard(countermodel: Countermodel, options: FormatOptions) -> String {
  let indent = options.indent
  let header = "=== Countermodel ===\n\n"

  let logic_info =
    "Logic System: "
    <> logic_system_to_string(countermodel.logic_system)
    <> "\n"
    <> "Actual World: "
    <> countermodel.actual_world
    <> "\n\n"

  let worlds_section =
    "Worlds:\n" <> format_worlds(countermodel.worlds, indent) <> "\n"

  let relations_section = case list.length(countermodel.relations) {
    0 -> "Relations: (none)\n"
    _ ->
      "Relations:\n" <> format_relations(countermodel.relations, indent) <> "\n"
  }

  let frame_props = case options.show_frame_properties {
    True -> format_frame_properties(countermodel, indent)
    False -> ""
  }

  header <> logic_info <> worlds_section <> relations_section <> frame_props
}

/// Format as detailed output with explanations
fn format_detailed(countermodel: Countermodel, options: FormatOptions) -> String {
  let indent = options.indent

  let header =
    "╔══════════════════════════════════════════════════════════════════════════════╗\n"
    <> "║                              COUNTERMODEL                                    ║\n"
    <> "╚══════════════════════════════════════════════════════════════════════════════╝\n\n"

  let explanation =
    "This countermodel demonstrates that the argument is INVALID.\n"
    <> "In the actual world ("
    <> countermodel.actual_world
    <> "), all premises are true but the conclusion is false.\n\n"

  let logic_info =
    "┌─ Logic System ─────────────────────────────────────────────────────────────────\n"
    <> "│ "
    <> logic_system_to_string(countermodel.logic_system)
    <> " - "
    <> logic_system_description(countermodel.logic_system)
    <> "\n"
    <> "└────────────────────────────────────────────────────────────────────────────────\n\n"

  let worlds_section =
    "┌─ Possible Worlds ──────────────────────────────────────────────────────────────\n"
    <> format_worlds_detailed(
      countermodel.worlds,
      countermodel.actual_world,
      indent,
    )
    <> "└────────────────────────────────────────────────────────────────────────────────\n\n"

  let relations_section =
    "┌─ Accessibility Relations ──────────────────────────────────────────────────────\n"
    <> format_relations_detailed(countermodel.relations, indent)
    <> "└────────────────────────────────────────────────────────────────────────────────\n\n"

  let frame_section =
    "┌─ Frame Properties ─────────────────────────────────────────────────────────────\n"
    <> format_frame_properties_detailed(countermodel, indent)
    <> "└────────────────────────────────────────────────────────────────────────────────\n"

  header
  <> explanation
  <> logic_info
  <> worlds_section
  <> relations_section
  <> frame_section
}

/// Format as ASCII diagram
fn format_diagram(countermodel: Countermodel, options: FormatOptions) -> String {
  let indent = options.indent
  let header = "Countermodel Diagram:\n\n"

  // Create simple world boxes
  let world_boxes =
    countermodel.worlds
    |> list.map(fn(w) {
      let marker = case w.name == countermodel.actual_world {
        True -> " ★"
        False -> ""
      }
      let props_str = case list.length(w.true_props) {
        0 -> "∅"
        _ -> string.join(w.true_props, ", ")
      }
      indent
      <> "┌───────────────┐\n"
      <> indent
      <> "│ "
      <> w.name
      <> marker
      <> string.repeat(" ", 13 - string.length(w.name) - string.length(marker))
      <> "│\n"
      <> indent
      <> "│ {"
      <> truncate_string(props_str, 10)
      <> "}"
      <> string.repeat(" ", 11 - string.length(truncate_string(props_str, 10)))
      <> "│\n"
      <> indent
      <> "└───────────────┘"
    })
    |> string.join("\n       │\n       ▼\n")

  let relations_str = case list.length(countermodel.relations) {
    0 -> "\n\n  No accessibility relations\n"
    _ ->
      "\n\n  Relations: "
      <> string.join(
        list.map(countermodel.relations, fn(r) { r.from <> " → " <> r.to }),
        ", ",
      )
      <> "\n"
  }

  let legend =
    "\n  Legend: ★ = actual world, {} = true propositions\n"
    <> "  Logic: "
    <> logic_system_to_string(countermodel.logic_system)
    <> "\n"

  header <> world_boxes <> relations_str <> legend
}

/// Format as JSON
fn format_json(countermodel: Countermodel) -> String {
  let worlds_json =
    countermodel.worlds
    |> list.map(fn(w) {
      "    {\n"
      <> "      \"name\": \""
      <> w.name
      <> "\",\n"
      <> "      \"true_props\": ["
      <> string.join(list.map(w.true_props, fn(p) { "\"" <> p <> "\"" }), ", ")
      <> "],\n"
      <> "      \"false_props\": ["
      <> string.join(list.map(w.false_props, fn(p) { "\"" <> p <> "\"" }), ", ")
      <> "]\n"
      <> "    }"
    })
    |> string.join(",\n")

  let relations_json =
    countermodel.relations
    |> list.map(fn(r) {
      "    {\"from\": \"" <> r.from <> "\", \"to\": \"" <> r.to <> "\"}"
    })
    |> string.join(",\n")

  "{\n"
  <> "  \"logic_system\": \""
  <> logic_system_to_string(countermodel.logic_system)
  <> "\",\n"
  <> "  \"actual_world\": \""
  <> countermodel.actual_world
  <> "\",\n"
  <> "  \"worlds\": [\n"
  <> worlds_json
  <> "\n  ],\n"
  <> "  \"relations\": [\n"
  <> relations_json
  <> "\n  ]\n"
  <> "}"
}

// =============================================================================
// World Formatting Helpers
// =============================================================================

fn format_worlds(worlds: List(KripkeWorld), indent: String) -> String {
  worlds
  |> list.map(fn(w) {
    let true_str = case list.length(w.true_props) {
      0 -> "∅"
      _ -> "{" <> string.join(w.true_props, ", ") <> "}"
    }
    let false_str = case list.length(w.false_props) {
      0 -> "∅"
      _ -> "{" <> string.join(w.false_props, ", ") <> "}"
    }
    indent <> w.name <> ": true=" <> true_str <> ", false=" <> false_str
  })
  |> string.join("\n")
}

fn format_worlds_detailed(
  worlds: List(KripkeWorld),
  actual: String,
  _indent: String,
) -> String {
  worlds
  |> list.map(fn(w) {
    let marker = case w.name == actual {
      True -> " ← ACTUAL WORLD"
      False -> ""
    }
    let true_str = case list.length(w.true_props) {
      0 -> "(no propositions true)"
      _ -> "True: " <> string.join(w.true_props, ", ")
    }
    let false_str = case list.length(w.false_props) {
      0 -> "(no propositions false)"
      _ -> "False: " <> string.join(w.false_props, ", ")
    }
    "│ "
    <> w.name
    <> marker
    <> "\n"
    <> "│   "
    <> true_str
    <> "\n"
    <> "│   "
    <> false_str
    <> "\n"
  })
  |> string.join("│\n")
}

// =============================================================================
// Relation Formatting Helpers
// =============================================================================

fn format_relations(
  relations: List(AccessibilityRelation),
  indent: String,
) -> String {
  relations
  |> list.map(fn(r) { indent <> r.from <> " → " <> r.to })
  |> string.join("\n")
}

fn format_relations_detailed(
  relations: List(AccessibilityRelation),
  _indent: String,
) -> String {
  case list.length(relations) {
    0 -> "│   (no accessibility relations - all worlds are isolated)\n"
    _ ->
      relations
      |> list.map(fn(r) {
        "│   R("
        <> r.from
        <> ", "
        <> r.to
        <> ") - "
        <> r.to
        <> " is accessible from "
        <> r.from
        <> "\n"
      })
      |> string.join("")
  }
}

// =============================================================================
// Frame Property Formatting
// =============================================================================

fn format_frame_properties(countermodel: Countermodel, indent: String) -> String {
  let props = get_frame_properties(countermodel.logic_system)
  case list.length(props) {
    0 -> ""
    _ ->
      "Frame Properties:\n"
      <> string.join(list.map(props, fn(p) { indent <> "• " <> p }), "\n")
      <> "\n"
  }
}

fn format_frame_properties_detailed(
  countermodel: Countermodel,
  _indent: String,
) -> String {
  let props = get_frame_properties_detailed(countermodel.logic_system)
  case list.length(props) {
    0 -> "│   (no special frame properties - basic modal logic K)\n"
    _ ->
      props
      |> list.map(fn(p) { "│   • " <> p <> "\n" })
      |> string.join("")
  }
}

fn get_frame_properties(system: LogicSystem) -> List(String) {
  case system {
    proposition.K -> []
    proposition.T -> ["Reflexive"]
    proposition.K4 -> ["Transitive"]
    proposition.S4 -> ["Reflexive", "Transitive"]
    proposition.S5 -> ["Reflexive", "Transitive", "Symmetric"]
    proposition.KD -> ["Serial"]
    proposition.KD45 -> ["Serial", "Transitive", "Euclidean"]
  }
}

fn get_frame_properties_detailed(system: LogicSystem) -> List(String) {
  case system {
    proposition.K -> []
    proposition.T -> ["Reflexivity: Every world can access itself (∀w: Rww)"]
    proposition.K4 -> [
      "Transitivity: If w1 accesses w2 and w2 accesses w3, then w1 accesses w3",
    ]
    proposition.S4 -> [
      "Reflexivity: Every world can access itself",
      "Transitivity: Accessibility is transitive",
    ]
    proposition.S5 -> [
      "Reflexivity: Every world can access itself",
      "Transitivity: Accessibility is transitive",
      "Symmetry: If w1 accesses w2, then w2 accesses w1",
    ]
    proposition.KD -> [
      "Seriality: Every world can access at least one world (∀w ∃v: Rwv)",
    ]
    proposition.KD45 -> [
      "Seriality: Every world can access at least one world",
      "Transitivity: Accessibility is transitive",
      "Euclidean: If w accesses v1 and v2, then v1 accesses v2",
    ]
  }
}

// =============================================================================
// Invalidity Reasoning
// =============================================================================

fn generate_invalidity_reason(countermodel: Countermodel) -> String {
  "In world "
  <> countermodel.actual_world
  <> ", all premises are satisfied but the conclusion fails. "
  <> "This demonstrates that the premises do not logically entail the conclusion "
  <> "in "
  <> logic_system_to_string(countermodel.logic_system)
  <> " logic."
}

fn logic_system_description(system: LogicSystem) -> String {
  case system {
    proposition.K -> "Basic modal logic (no frame conditions)"
    proposition.T -> "Reflexive frames (□p → p is valid)"
    proposition.K4 -> "Transitive frames (□p → □□p is valid)"
    proposition.S4 -> "Reflexive + transitive frames"
    proposition.S5 -> "Equivalence frames (universal accessibility)"
    proposition.KD -> "Serial frames (◇⊤ is valid)"
    proposition.KD45 -> "Deontic logic (serial + transitive + euclidean)"
  }
}

// =============================================================================
// Natural Language Generation
// =============================================================================

/// Generate natural language explanation of countermodel
pub fn explain_countermodel(countermodel: Countermodel) -> String {
  let world_count = list.length(countermodel.worlds)
  let intro = case world_count {
    1 ->
      "Consider a single possible world, " <> countermodel.actual_world <> ".\n"
    _ -> "Consider " <> int_to_string(world_count) <> " possible worlds.\n"
  }

  let actual_world =
    list.find(countermodel.worlds, fn(w) { w.name == countermodel.actual_world })

  let actual_desc = case actual_world {
    Ok(w) ->
      "In the actual world (" <> w.name <> "):\n" <> describe_world(w) <> "\n"
    Error(_) -> ""
  }

  let relations_desc = case list.length(countermodel.relations) {
    0 ->
      "There are no accessibility relations between worlds, meaning what is possible "
      <> "in one world has no bearing on other worlds.\n"
    n ->
      "There "
      <> case n {
        1 -> "is 1 accessibility relation"
        _ -> "are " <> int_to_string(n) <> " accessibility relations"
      }
      <> " between worlds.\n"
  }

  let conclusion =
    "\nThis configuration shows that even when all premises are true, "
    <> "the conclusion can be false - proving the argument invalid.\n"

  intro <> actual_desc <> relations_desc <> conclusion
}

/// Generate a detailed modal logic explanation for the countermodel
///
/// This explains how the countermodel works in terms of modal semantics:
/// - Which propositions are necessarily true (□p) - true in all accessible worlds
/// - Which propositions are possibly true (◇p) - true in at least one accessible world
/// - Why the specific world configuration invalidates the argument
pub fn explain_modal_semantics(countermodel: Countermodel) -> String {
  let world_count = list.length(countermodel.worlds)
  let relation_count = list.length(countermodel.relations)

  let header =
    "═══════════════════════════════════════════════════════════════════════\n"
    <> "                    MODAL COUNTERMODEL ANALYSIS\n"
    <> "═══════════════════════════════════════════════════════════════════════\n\n"

  let kripke_intro =
    "This Kripke model demonstrates invalidity:\n\n"
    <> "  • Worlds (W): {"
    <> string.join(list.map(countermodel.worlds, fn(w) { w.name }), ", ")
    <> "}\n"
    <> "  • Actual world (@): "
    <> countermodel.actual_world
    <> "\n"
    <> "  • Size: "
    <> int_to_string(world_count)
    <> " world(s), "
    <> int_to_string(relation_count)
    <> " relation(s)\n\n"

  let accessibility_section = case countermodel.relations {
    [] ->
      "Accessibility Relation R:\n"
      <> "  R = ∅ (empty - no world can access any other world)\n"
      <> "  This means □p is vacuously true and ◇p is always false.\n\n"
    rels -> {
      let rel_pairs =
        list.map(rels, fn(r) { "(" <> r.from <> "," <> r.to <> ")" })
      "Accessibility Relation R:\n"
      <> "  R = {"
      <> string.join(rel_pairs, ", ")
      <> "}\n"
      <> describe_accessibility_properties(countermodel)
      <> "\n"
    }
  }

  let valuation_section =
    "Valuation V (truth assignment per world):\n"
    <> string.join(
      list.map(countermodel.worlds, fn(w) {
        let true_str = case w.true_props {
          [] -> "∅"
          props -> "{" <> string.join(props, ", ") <> "}"
        }
        "  V(" <> w.name <> ") = " <> true_str
      }),
      "\n",
    )
    <> "\n\n"

  let modal_truth_section = explain_modal_truth_at_actual(countermodel)

  let frame_section =
    "Frame Properties ("
    <> logic_system_to_string(countermodel.logic_system)
    <> "):\n"
    <> format_frame_constraints(countermodel.logic_system)
    <> "\n"

  header
  <> kripke_intro
  <> accessibility_section
  <> valuation_section
  <> modal_truth_section
  <> frame_section
}

/// Describe accessibility relation properties
fn describe_accessibility_properties(countermodel: Countermodel) -> String {
  let is_reflexive = check_is_reflexive(countermodel)
  let is_transitive = check_is_transitive(countermodel)
  let is_symmetric = check_is_symmetric(countermodel)

  let props =
    [
      case is_reflexive {
        True -> "reflexive"
        False -> ""
      },
      case is_transitive {
        True -> "transitive"
        False -> ""
      },
      case is_symmetric {
        True -> "symmetric"
        False -> ""
      },
    ]
    |> list.filter(fn(s) { s != "" })

  case props {
    [] -> "  Properties: none (arbitrary relation)"
    p -> "  Properties: " <> string.join(p, ", ")
  }
}

/// Check if the accessibility relation is reflexive
fn check_is_reflexive(countermodel: Countermodel) -> Bool {
  list.all(countermodel.worlds, fn(w) {
    list.any(countermodel.relations, fn(r) {
      r.from == w.name && r.to == w.name
    })
  })
}

/// Check if the accessibility relation is transitive
fn check_is_transitive(countermodel: Countermodel) -> Bool {
  list.all(countermodel.relations, fn(r1) {
    list.all(countermodel.relations, fn(r2) {
      case r1.to == r2.from {
        True ->
          list.any(countermodel.relations, fn(r3) {
            r3.from == r1.from && r3.to == r2.to
          })
        False -> True
      }
    })
  })
}

/// Check if the accessibility relation is symmetric
fn check_is_symmetric(countermodel: Countermodel) -> Bool {
  list.all(countermodel.relations, fn(r) {
    list.any(countermodel.relations, fn(r2) {
      r2.from == r.to && r2.to == r.from
    })
  })
}

/// Explain modal truth at the actual world
fn explain_modal_truth_at_actual(countermodel: Countermodel) -> String {
  let actual_world_opt =
    list.find(countermodel.worlds, fn(w) { w.name == countermodel.actual_world })

  case actual_world_opt {
    Error(_) -> ""
    Ok(actual_world) -> {
      // Find accessible worlds from actual
      let accessible =
        countermodel.relations
        |> list.filter(fn(r) { r.from == countermodel.actual_world })
        |> list.map(fn(r) { r.to })

      let modal_analysis = case accessible {
        [] ->
          "  At "
          <> countermodel.actual_world
          <> ", no worlds are accessible.\n"
          <> "  Therefore: □φ is true for ALL φ (vacuously), and ◇φ is false for ALL φ.\n"
        worlds -> {
          "  From "
          <> countermodel.actual_world
          <> ", accessible worlds: {"
          <> string.join(worlds, ", ")
          <> "}\n"
          <> describe_modal_operators(actual_world, worlds, countermodel.worlds)
        }
      }

      "Modal Truth at Actual World:\n" <> modal_analysis <> "\n"
    }
  }
}

/// Describe modal operator semantics for given accessible worlds
fn describe_modal_operators(
  actual: KripkeWorld,
  accessible_names: List(String),
  all_worlds: List(KripkeWorld),
) -> String {
  // Find the accessible world data
  let accessible_worlds =
    list.filter(all_worlds, fn(w) { list.contains(accessible_names, w.name) })

  // Find propositions that are necessarily true (true in ALL accessible worlds)
  let all_props =
    list.flat_map(all_worlds, fn(w) { list.append(w.true_props, w.false_props) })
    |> list.unique

  let necessary_props =
    list.filter(all_props, fn(p) {
      list.all(accessible_worlds, fn(w) { list.contains(w.true_props, p) })
    })

  let possible_props =
    list.filter(all_props, fn(p) {
      list.any(accessible_worlds, fn(w) { list.contains(w.true_props, p) })
    })

  let necessary_str = case necessary_props {
    [] -> "  □-true: (none - no proposition is true in all accessible worlds)"
    props -> "  □-true: {" <> string.join(props, ", ") <> "}"
  }

  let possible_str = case possible_props {
    [] -> "  ◇-true: (none - no proposition is true in any accessible world)"
    props -> "  ◇-true: {" <> string.join(props, ", ") <> "}"
  }

  let actual_str = case actual.true_props {
    [] -> "  At " <> actual.name <> ": (no atomic propositions true)"
    props ->
      "  At " <> actual.name <> ": {" <> string.join(props, ", ") <> "} true"
  }

  actual_str <> "\n" <> necessary_str <> "\n" <> possible_str <> "\n"
}

/// Format frame constraints for the logic system
fn format_frame_constraints(system: LogicSystem) -> String {
  case system {
    proposition.K -> "  (no constraints - any accessibility relation is valid)"
    proposition.T -> "  ∀w: R(w,w)  [reflexivity - what is necessary is actual]"
    proposition.K4 ->
      "  ∀w,v,u: R(w,v) ∧ R(v,u) → R(w,u)  [transitivity - positive introspection]"
    proposition.S4 ->
      "  ∀w: R(w,w)  [reflexivity]\n"
      <> "  ∀w,v,u: R(w,v) ∧ R(v,u) → R(w,u)  [transitivity]"
    proposition.S5 ->
      "  R is an equivalence relation:\n"
      <> "    ∀w: R(w,w)  [reflexivity]\n"
      <> "    ∀w,v,u: R(w,v) ∧ R(v,u) → R(w,u)  [transitivity]\n"
      <> "    ∀w,v: R(w,v) → R(v,w)  [symmetry]"
    proposition.KD ->
      "  ∀w∃v: R(w,v)  [seriality - every world has an accessible world]"
    proposition.KD45 ->
      "  ∀w∃v: R(w,v)  [seriality]\n"
      <> "  ∀w,v,u: R(w,v) ∧ R(v,u) → R(w,u)  [transitivity]\n"
      <> "  ∀w,v,u: R(w,v) ∧ R(w,u) → R(v,u)  [euclidean]"
  }
}

fn describe_world(world: KripkeWorld) -> String {
  let true_desc = case list.length(world.true_props) {
    0 -> "  • No atomic propositions are true"
    _ -> "  • True: " <> string.join(world.true_props, ", ")
  }
  let false_desc = case list.length(world.false_props) {
    0 -> "  • No atomic propositions are explicitly false"
    _ -> "  • False: " <> string.join(world.false_props, ", ")
  }
  true_desc <> "\n" <> false_desc
}

// =============================================================================
// Comparison and Analysis
// =============================================================================

/// Compare two countermodels
pub fn compare_countermodels(
  cm1: Countermodel,
  cm2: Countermodel,
) -> CountermodelComparison {
  let same_logic = cm1.logic_system == cm2.logic_system
  let same_world_count = list.length(cm1.worlds) == list.length(cm2.worlds)
  let same_relation_count =
    list.length(cm1.relations) == list.length(cm2.relations)

  CountermodelComparison(
    same_logic_system: same_logic,
    same_world_count: same_world_count,
    same_relation_count: same_relation_count,
    cm1_worlds: list.length(cm1.worlds),
    cm2_worlds: list.length(cm2.worlds),
    cm1_relations: list.length(cm1.relations),
    cm2_relations: list.length(cm2.relations),
  )
}

/// Comparison result
pub type CountermodelComparison {
  CountermodelComparison(
    same_logic_system: Bool,
    same_world_count: Bool,
    same_relation_count: Bool,
    cm1_worlds: Int,
    cm2_worlds: Int,
    cm1_relations: Int,
    cm2_relations: Int,
  )
}

/// Get the minimal countermodel (fewest worlds/relations)
pub fn minimize_countermodel(countermodel: Countermodel) -> Countermodel {
  // For now, just return the original
  // A real implementation would remove unnecessary worlds/relations
  countermodel
}

/// Check if countermodel is minimal
pub fn is_minimal(countermodel: Countermodel) -> Bool {
  // Basic heuristic: single world with no relations is minimal
  list.length(countermodel.worlds) == 1 && countermodel.relations == []
}

// =============================================================================
// Helper Functions
// =============================================================================

fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    proposition.K -> "K"
    proposition.T -> "T"
    proposition.K4 -> "K4"
    proposition.S4 -> "S4"
    proposition.S5 -> "S5"
    proposition.KD -> "KD"
    proposition.KD45 -> "KD45"
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

fn truncate_string(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len - 2) <> ".."
    False -> s
  }
}
