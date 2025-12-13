//// Countermodel Extraction Module
////
//// This module extracts Kripke countermodels from Z3 satisfying assignments.
//// A countermodel demonstrates why a modal formula is not valid by providing
//// a Kripke frame where the formula fails.
////
//// ## KripkeModel Structure
////
//// A Kripke model consists of:
//// - A set of worlds (W)
//// - An accessibility relation (R ⊆ W × W)
//// - A valuation function (V: Prop → 2^W)
////
//// ## Usage
////
//// ```gleam
//// import z3/modal/countermodel
//// import z3/modal/kripke
//// import z3/solver
////
//// // After getting a SAT result from the solver
//// let kripke_model = countermodel.extract(solver_model, ctx, world_names, props)
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import z3/modal/kripke.{type KripkeContext}
import z3/solver.{type SolverModel, SolverModel}
import z3/types.{BoolVal}

// =============================================================================
// Types
// =============================================================================

/// A Kripke model (countermodel) for modal logic
pub type KripkeModel {
  KripkeModel(
    /// Set of world names
    worlds: List(String),
    /// Accessibility relation as a list of (from, to) pairs
    accessibility: List(#(String, String)),
    /// Valuation: maps proposition names to sets of worlds where they hold
    valuation: Dict(String, List(String)),
    /// The designated initial/actual world
    actual_world: String,
  )
}

/// Result of countermodel extraction
pub type ExtractionResult {
  /// Successfully extracted a countermodel
  Extracted(KripkeModel)
  /// No countermodel (formula is valid)
  NoCountermodel
  /// Extraction failed with reason
  ExtractionError(String)
}

// =============================================================================
// Countermodel Extraction
// =============================================================================

/// Extract a Kripke model from a Z3 solver model
pub fn extract(
  solver_model: SolverModel,
  ctx: KripkeContext,
  world_names: List(String),
  prop_names: List(String),
  actual_world: String,
) -> ExtractionResult {
  // Extract accessibility relation
  let accessibility = extract_accessibility(solver_model, ctx, world_names)

  // Extract valuation for each proposition
  let valuation = extract_valuation(solver_model, ctx, world_names, prop_names)

  Extracted(KripkeModel(
    worlds: world_names,
    accessibility: accessibility,
    valuation: valuation,
    actual_world: actual_world,
  ))
}

/// Extract the accessibility relation from the solver model
fn extract_accessibility(
  solver_model: SolverModel,
  ctx: KripkeContext,
  world_names: List(String),
) -> List(#(String, String)) {
  let all_pairs = list_all_pairs(world_names)
  list.filter(all_pairs, fn(pair) {
    let #(w1, w2) = pair
    is_accessible_in_model(solver_model, ctx, w1, w2)
  })
}

/// Check if w2 is accessible from w1 in the model
fn is_accessible_in_model(
  solver_model: SolverModel,
  ctx: KripkeContext,
  w1_name: String,
  w2_name: String,
) -> Bool {
  // The accessibility relation is encoded as R_w1_w2
  let var_name = ctx.accessibility_name <> "_" <> w1_name <> "_" <> w2_name
  get_bool_value(solver_model, var_name)
  |> option.unwrap(False)
}

/// Extract valuation for all propositions
fn extract_valuation(
  solver_model: SolverModel,
  _ctx: KripkeContext,
  world_names: List(String),
  prop_names: List(String),
) -> Dict(String, List(String)) {
  list.fold(prop_names, dict.new(), fn(acc, prop) {
    let worlds_where_true = extract_prop_worlds(solver_model, prop, world_names)
    dict.insert(acc, prop, worlds_where_true)
  })
}

/// Get worlds where a proposition holds
fn extract_prop_worlds(
  solver_model: SolverModel,
  prop_name: String,
  world_names: List(String),
) -> List(String) {
  list.filter(world_names, fn(w) {
    let var_name = prop_name <> "_" <> w
    get_bool_value(solver_model, var_name)
    |> option.unwrap(False)
  })
}

// =============================================================================
// Model Queries
// =============================================================================

/// Get all worlds in the model
pub fn get_worlds(model: KripkeModel) -> List(String) {
  model.worlds
}

/// Get the actual/initial world
pub fn get_actual_world(model: KripkeModel) -> String {
  model.actual_world
}

/// Check if w2 is accessible from w1
pub fn is_accessible(model: KripkeModel, from: String, to: String) -> Bool {
  list.contains(model.accessibility, #(from, to))
}

/// Get all worlds accessible from a given world
pub fn accessible_from(model: KripkeModel, world: String) -> List(String) {
  list.filter_map(model.accessibility, fn(pair) {
    let #(from, to) = pair
    case from == world {
      True -> Ok(to)
      False -> Error(Nil)
    }
  })
}

/// Get all worlds from which a given world is accessible
pub fn accessible_to(model: KripkeModel, world: String) -> List(String) {
  list.filter_map(model.accessibility, fn(pair) {
    let #(from, to) = pair
    case to == world {
      True -> Ok(from)
      False -> Error(Nil)
    }
  })
}

/// Check if a proposition holds at a world
pub fn holds_at(model: KripkeModel, prop: String, world: String) -> Bool {
  case dict.get(model.valuation, prop) {
    Ok(worlds) -> list.contains(worlds, world)
    Error(_) -> False
  }
}

/// Get all propositions that hold at a world
pub fn props_at_world(model: KripkeModel, world: String) -> List(String) {
  dict.fold(model.valuation, [], fn(acc, prop, worlds) {
    case list.contains(worlds, world) {
      True -> [prop, ..acc]
      False -> acc
    }
  })
}

/// Get all worlds where a proposition holds
pub fn worlds_where_holds(model: KripkeModel, prop: String) -> List(String) {
  case dict.get(model.valuation, prop) {
    Ok(worlds) -> worlds
    Error(_) -> []
  }
}

// =============================================================================
// Model Statistics
// =============================================================================

/// Get the number of worlds in the model
pub fn world_count(model: KripkeModel) -> Int {
  list.length(model.worlds)
}

/// Get the number of accessibility pairs
pub fn accessibility_count(model: KripkeModel) -> Int {
  list.length(model.accessibility)
}

/// Get the number of propositions
pub fn prop_count(model: KripkeModel) -> Int {
  dict.size(model.valuation)
}

// =============================================================================
// Model Formatting
// =============================================================================

/// Format the countermodel as a string for display
pub fn format(model: KripkeModel) -> String {
  let header = "Kripke Countermodel\n==================\n"

  let worlds_str = "Worlds: {" <> string.join(model.worlds, ", ") <> "}\n"

  let actual_str = "Actual world: " <> model.actual_world <> "\n"

  let access_str = format_accessibility(model)

  let valuation_str = format_valuation(model)

  header <> worlds_str <> actual_str <> access_str <> valuation_str
}

/// Format the accessibility relation
fn format_accessibility(model: KripkeModel) -> String {
  let header = "\nAccessibility relation R:\n"
  case model.accessibility {
    [] -> header <> "  (empty)\n"
    pairs -> {
      let formatted =
        list.map(pairs, fn(pair) {
          let #(from, to) = pair
          "  R(" <> from <> ", " <> to <> ")"
        })
      header <> string.join(formatted, "\n") <> "\n"
    }
  }
}

/// Format the valuation
fn format_valuation(model: KripkeModel) -> String {
  let header = "\nValuation V:\n"
  let props = dict.to_list(model.valuation)
  case props {
    [] -> header <> "  (no propositions)\n"
    _ -> {
      let formatted =
        list.map(props, fn(pair) {
          let #(prop, worlds) = pair
          "  V(" <> prop <> ") = {" <> string.join(worlds, ", ") <> "}"
        })
      header <> string.join(formatted, "\n") <> "\n"
    }
  }
}

/// Format a compact representation
pub fn format_compact(model: KripkeModel) -> String {
  let w = "W={" <> string.join(model.worlds, ",") <> "}"
  let r_pairs =
    list.map(model.accessibility, fn(p) {
      let #(from, to) = p
      "(" <> from <> "," <> to <> ")"
    })
  let r = "R={" <> string.join(r_pairs, ",") <> "}"
  let v_parts =
    list.map(dict.to_list(model.valuation), fn(p) {
      let #(prop, worlds) = p
      prop <> ":{" <> string.join(worlds, ",") <> "}"
    })
  let v = "V={" <> string.join(v_parts, ",") <> "}"
  w <> ", " <> r <> ", " <> v <> ", @" <> model.actual_world
}

// =============================================================================
// Model Construction (for testing)
// =============================================================================

/// Create an empty Kripke model
pub fn empty(actual_world: String) -> KripkeModel {
  KripkeModel(
    worlds: [actual_world],
    accessibility: [],
    valuation: dict.new(),
    actual_world: actual_world,
  )
}

/// Create a model with specified components
pub fn new(
  worlds: List(String),
  accessibility: List(#(String, String)),
  valuation: Dict(String, List(String)),
  actual_world: String,
) -> KripkeModel {
  KripkeModel(
    worlds: worlds,
    accessibility: accessibility,
    valuation: valuation,
    actual_world: actual_world,
  )
}

/// Add a world to the model
pub fn add_world(model: KripkeModel, world: String) -> KripkeModel {
  case list.contains(model.worlds, world) {
    True -> model
    False -> KripkeModel(..model, worlds: [world, ..model.worlds])
  }
}

/// Add an accessibility pair
pub fn add_accessibility(
  model: KripkeModel,
  from: String,
  to: String,
) -> KripkeModel {
  let pair = #(from, to)
  case list.contains(model.accessibility, pair) {
    True -> model
    False -> KripkeModel(..model, accessibility: [pair, ..model.accessibility])
  }
}

/// Set a proposition to hold at specified worlds
pub fn set_valuation(
  model: KripkeModel,
  prop: String,
  worlds: List(String),
) -> KripkeModel {
  KripkeModel(..model, valuation: dict.insert(model.valuation, prop, worlds))
}

/// Make a proposition true at a specific world
pub fn make_true_at(
  model: KripkeModel,
  prop: String,
  world: String,
) -> KripkeModel {
  let current_worlds = case dict.get(model.valuation, prop) {
    Ok(worlds) -> worlds
    Error(_) -> []
  }
  case list.contains(current_worlds, world) {
    True -> model
    False -> set_valuation(model, prop, [world, ..current_worlds])
  }
}

// =============================================================================
// Model Validation
// =============================================================================

/// Check if the model is well-formed
pub fn is_valid(model: KripkeModel) -> Bool {
  // Check actual world is in worlds
  let actual_in_worlds = list.contains(model.worlds, model.actual_world)

  // Check all accessibility pairs reference valid worlds
  let accessibility_valid =
    list.all(model.accessibility, fn(pair) {
      let #(from, to) = pair
      list.contains(model.worlds, from) && list.contains(model.worlds, to)
    })

  // Check all valuation worlds are valid
  let valuation_valid =
    dict.fold(model.valuation, True, fn(acc, _prop, worlds) {
      acc && list.all(worlds, fn(w) { list.contains(model.worlds, w) })
    })

  actual_in_worlds && accessibility_valid && valuation_valid
}

// =============================================================================
// Internal Helpers
// =============================================================================

/// Get a boolean value from the solver model
fn get_bool_value(model: SolverModel, name: String) -> Option(Bool) {
  let SolverModel(values) = model
  case dict.get(values, name) {
    Ok(BoolVal(b)) -> Some(b)
    _ -> None
  }
}

/// Generate all pairs (including self-pairs) from a list
fn list_all_pairs(items: List(a)) -> List(#(a, a)) {
  list.flat_map(items, fn(w1) { list.map(items, fn(w2) { #(w1, w2) }) })
}
