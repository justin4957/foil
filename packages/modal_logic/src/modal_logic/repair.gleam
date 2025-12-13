//// Repair Suggestion Generator
////
//// This module generates suggestions for repairing invalid modal logic arguments.
//// When an argument is found to be invalid, this module analyzes the countermodel
//// and proposes modifications that could make the argument valid.
////
//// ## Repair Strategies
////
//// 1. Add Missing Premise - Suggest premises that would block the countermodel
//// 2. Strengthen Premise - Make existing premises more specific
//// 3. Weaken Conclusion - Make the conclusion less demanding
//// 4. Change Logic System - Suggest a stronger logic where argument is valid
//// 5. Modify Modality - Resolve modal ambiguities
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/repair
////
//// let suggestions = repair.generate_suggestions(formalization, countermodel)
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string
import modal_logic/argument.{type Formalization, type RepairSuggestion}
import modal_logic/proposition.{type LogicSystem, type Proposition}
import modal_logic/validator.{type Countermodel}

// =============================================================================
// Types
// =============================================================================

/// Configuration for repair generation
pub type RepairConfig {
  RepairConfig(
    /// Maximum number of suggestions to generate
    max_suggestions: Int,
    /// Include premise additions
    include_additions: Bool,
    /// Include premise strengthening
    include_strengthening: Bool,
    /// Include conclusion weakening
    include_weakening: Bool,
    /// Include logic system changes
    include_logic_changes: Bool,
    /// Include modal modifications
    include_modality_changes: Bool,
    /// Minimum confidence threshold for suggestions
    min_confidence: Float,
  )
}

/// A detailed repair suggestion with analysis
pub type DetailedRepair {
  DetailedRepair(
    /// The repair suggestion
    suggestion: RepairSuggestion,
    /// How this repair blocks the countermodel
    blocking_explanation: String,
    /// Propositions affected by this repair
    affected_propositions: List(String),
    /// Whether this repair preserves the original intent
    preserves_intent: Bool,
    /// Risk assessment
    risk_level: RiskLevel,
  )
}

/// Risk level for a repair
pub type RiskLevel {
  /// Low risk - minor modification
  LowRisk
  /// Medium risk - significant change
  MediumRisk
  /// High risk - may change meaning substantially
  HighRisk
}

/// Analysis result for a countermodel
pub type CountermodelAnalysis {
  CountermodelAnalysis(
    /// Atoms that differ between actual world and others
    distinguishing_atoms: List(String),
    /// Missing relations that could validate argument
    missing_relations: List(#(String, String)),
    /// Worlds where conclusion fails
    conclusion_failure_worlds: List(String),
    /// Structural weaknesses in the argument
    structural_issues: List(String),
  )
}

/// Repair session tracking multiple attempts
pub type RepairSession {
  RepairSession(
    /// Original formalization
    original: Formalization,
    /// Attempted repairs
    attempts: List(RepairAttempt),
    /// Current best suggestion
    best_suggestion: Option(DetailedRepair),
  )
}

/// A single repair attempt
pub type RepairAttempt {
  RepairAttempt(
    /// The repair applied
    repair: RepairSuggestion,
    /// Whether it resulted in a valid argument
    succeeded: Bool,
    /// New countermodel if still invalid
    new_countermodel: Option(Countermodel),
  )
}

/// Internal repair candidate with original/suggested propositions
pub type RepairCandidate {
  RepairCandidate(
    repair_type: argument.RepairType,
    description: String,
    original: Option(Proposition),
    suggested: Option(Proposition),
    confidence: Float,
    explanation: String,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default repair configuration
pub fn default_config() -> RepairConfig {
  RepairConfig(
    max_suggestions: 5,
    include_additions: True,
    include_strengthening: True,
    include_weakening: True,
    include_logic_changes: True,
    include_modality_changes: True,
    min_confidence: 0.3,
  )
}

/// Create conservative repair configuration (minimal changes)
pub fn conservative_config() -> RepairConfig {
  RepairConfig(
    max_suggestions: 3,
    include_additions: True,
    include_strengthening: False,
    include_weakening: True,
    include_logic_changes: False,
    include_modality_changes: True,
    min_confidence: 0.5,
  )
}

/// Create aggressive repair configuration (more options)
pub fn aggressive_config() -> RepairConfig {
  RepairConfig(
    max_suggestions: 10,
    include_additions: True,
    include_strengthening: True,
    include_weakening: True,
    include_logic_changes: True,
    include_modality_changes: True,
    min_confidence: 0.2,
  )
}

// =============================================================================
// Main Repair Generation
// =============================================================================

/// Generate repair suggestions for an invalid argument
pub fn generate_suggestions(
  formalization: Formalization,
  countermodel: Countermodel,
) -> List(RepairSuggestion) {
  generate_suggestions_with_config(
    formalization,
    countermodel,
    default_config(),
  )
}

/// Generate repair suggestions with custom configuration
pub fn generate_suggestions_with_config(
  formalization: Formalization,
  countermodel: Countermodel,
  config: RepairConfig,
) -> List(RepairSuggestion) {
  let analysis = analyze_countermodel(countermodel, formalization)

  let candidates =
    []
    |> add_if(config.include_additions, fn(acc) {
      list.append(acc, generate_premise_additions(formalization, analysis))
    })
    |> add_if(config.include_strengthening, fn(acc) {
      list.append(acc, generate_premise_strengthening(formalization, analysis))
    })
    |> add_if(config.include_weakening, fn(acc) {
      list.append(acc, generate_conclusion_weakening(formalization, analysis))
    })
    |> add_if(config.include_logic_changes, fn(acc) {
      list.append(acc, generate_logic_changes(formalization, countermodel))
    })
    |> add_if(config.include_modality_changes, fn(acc) {
      list.append(acc, generate_modal_modifications(formalization, analysis))
    })

  candidates
  |> list.filter(fn(c) { c.confidence >=. config.min_confidence })
  |> list.sort(fn(a, b) { float_compare(b.confidence, a.confidence) })
  |> list.take(config.max_suggestions)
  |> list.index_map(fn(c, i) { candidate_to_suggestion(c, formalization, i) })
}

/// Generate detailed repair suggestions
pub fn generate_detailed_suggestions(
  formalization: Formalization,
  countermodel: Countermodel,
  config: RepairConfig,
) -> List(DetailedRepair) {
  let suggestions =
    generate_suggestions_with_config(formalization, countermodel, config)
  let analysis = analyze_countermodel(countermodel, formalization)

  list.map(suggestions, fn(s) { create_detailed_repair(s, analysis) })
}

// =============================================================================
// Countermodel Analysis
// =============================================================================

/// Analyze a countermodel to understand why the argument fails
pub fn analyze_countermodel(
  countermodel: Countermodel,
  formalization: Formalization,
) -> CountermodelAnalysis {
  let actual_world =
    list.find(countermodel.worlds, fn(w) { w.name == countermodel.actual_world })

  let actual_true = case actual_world {
    Ok(w) -> w.true_props
    Error(_) -> []
  }

  let other_true =
    countermodel.worlds
    |> list.filter(fn(w) { w.name != countermodel.actual_world })
    |> list.flat_map(fn(w) { w.true_props })
    |> list.unique

  let distinguishing =
    list.filter(actual_true, fn(a) { !list.contains(other_true, a) })

  // Find worlds where conclusion would need to hold
  let conclusion_atoms = collect_prop_atoms(formalization.conclusion)
  let failure_worlds =
    countermodel.worlds
    |> list.filter(fn(w) {
      list.any(conclusion_atoms, fn(a) { list.contains(w.false_props, a) })
    })
    |> list.map(fn(w) { w.name })

  // Identify missing relations based on logic system
  let missing_rels =
    identify_missing_relations(countermodel, formalization.logic_system)

  // Structural issues
  let issues = identify_structural_issues(formalization, countermodel)

  CountermodelAnalysis(
    distinguishing_atoms: distinguishing,
    missing_relations: missing_rels,
    conclusion_failure_worlds: failure_worlds,
    structural_issues: issues,
  )
}

fn identify_missing_relations(
  countermodel: Countermodel,
  logic_system: LogicSystem,
) -> List(#(String, String)) {
  let world_names = list.map(countermodel.worlds, fn(w) { w.name })

  // Check for missing reflexive relations in T, S4, S5
  let missing_reflexive = case logic_system {
    proposition.T | proposition.S4 | proposition.S5 ->
      world_names
      |> list.filter(fn(w) {
        !list.any(countermodel.relations, fn(r) { r.from == w && r.to == w })
      })
      |> list.map(fn(w) { #(w, w) })
    _ -> []
  }

  missing_reflexive
}

fn identify_structural_issues(
  formalization: Formalization,
  countermodel: Countermodel,
) -> List(String) {
  let issues = []

  // Check for ungrounded modalities
  let has_ungrounded_necessary =
    list.any(formalization.premises, fn(p) {
      case p {
        proposition.Necessary(_) -> True
        _ -> False
      }
    })
    && countermodel.relations == []

  let issues = case has_ungrounded_necessary {
    True -> ["Necessity claims with no accessibility relations"]
    False -> issues
  }

  // Check for modal scope issues
  let has_nested_modals = list.any(formalization.premises, has_nested_modality)

  let issues = case has_nested_modals {
    True -> ["Nested modal operators may cause scope ambiguity", ..issues]
    False -> issues
  }

  issues
}

fn has_nested_modality(prop: Proposition) -> Bool {
  case prop {
    proposition.Necessary(inner) -> is_modal(inner)
    proposition.Possible(inner) -> is_modal(inner)
    proposition.And(left, right) ->
      has_nested_modality(left) || has_nested_modality(right)
    proposition.Or(left, right) ->
      has_nested_modality(left) || has_nested_modality(right)
    proposition.Implies(ante, cons) ->
      has_nested_modality(ante) || has_nested_modality(cons)
    proposition.Not(inner) -> has_nested_modality(inner)
    _ -> False
  }
}

fn is_modal(prop: Proposition) -> Bool {
  case prop {
    proposition.Necessary(_) -> True
    proposition.Possible(_) -> True
    proposition.Obligatory(_) -> True
    proposition.Permitted(_) -> True
    proposition.Knows(_, _) -> True
    proposition.Believes(_, _) -> True
    _ -> False
  }
}

// =============================================================================
// Premise Addition Suggestions
// =============================================================================

fn generate_premise_additions(
  formalization: Formalization,
  analysis: CountermodelAnalysis,
) -> List(RepairCandidate) {
  // Suggest blocking premise for distinguishing atoms
  let blocking_suggestions =
    analysis.distinguishing_atoms
    |> list.map(fn(atom) {
      RepairCandidate(
        repair_type: argument.AddPremise,
        description: "Add premise asserting " <> atom,
        original: None,
        suggested: Some(proposition.Atom(atom)),
        confidence: 0.6,
        explanation: "Adding this premise would force "
          <> atom
          <> " to be true in the actual world",
      )
    })

  // Suggest necessary premise if conclusion has modality
  let modal_suggestions = case formalization.conclusion {
    proposition.Necessary(inner) -> [
      RepairCandidate(
        repair_type: argument.AddPremise,
        description: "Add necessary premise for inner proposition",
        original: None,
        suggested: Some(proposition.Necessary(inner)),
        confidence: 0.5,
        explanation: "Asserting necessity of the inner proposition",
      ),
    ]
    _ -> []
  }

  list.append(blocking_suggestions, modal_suggestions)
}

// =============================================================================
// Premise Strengthening Suggestions
// =============================================================================

fn generate_premise_strengthening(
  formalization: Formalization,
  _analysis: CountermodelAnalysis,
) -> List(RepairCandidate) {
  formalization.premises
  |> list.flat_map(fn(premise) {
    case premise {
      proposition.Implies(ante, cons) -> [
        RepairCandidate(
          repair_type: argument.StrengthenPremise,
          description: "Strengthen implication to biconditional",
          original: Some(premise),
          suggested: Some(proposition.And(
            proposition.Implies(ante, cons),
            proposition.Implies(cons, ante),
          )),
          confidence: 0.4,
          explanation: "Making the implication bidirectional may resolve the invalidity",
        ),
      ]
      proposition.Possible(inner) -> [
        RepairCandidate(
          repair_type: argument.StrengthenPremise,
          description: "Strengthen possibility to necessity",
          original: Some(premise),
          suggested: Some(proposition.Necessary(inner)),
          confidence: 0.5,
          explanation: "Replacing 'possibly' with 'necessarily' strengthens the premise",
        ),
      ]
      _ -> []
    }
  })
}

// =============================================================================
// Conclusion Weakening Suggestions
// =============================================================================

fn generate_conclusion_weakening(
  formalization: Formalization,
  _analysis: CountermodelAnalysis,
) -> List(RepairCandidate) {
  case formalization.conclusion {
    proposition.Necessary(inner) -> [
      RepairCandidate(
        repair_type: argument.WeakenConclusion,
        description: "Weaken necessary conclusion to possible",
        original: Some(formalization.conclusion),
        suggested: Some(proposition.Possible(inner)),
        confidence: 0.7,
        explanation: "The conclusion may only follow as a possibility, not a necessity",
      ),
      RepairCandidate(
        repair_type: argument.WeakenConclusion,
        description: "Remove modality from conclusion",
        original: Some(formalization.conclusion),
        suggested: Some(inner),
        confidence: 0.5,
        explanation: "The conclusion may follow without modal qualification",
      ),
    ]
    proposition.And(left, right) -> [
      RepairCandidate(
        repair_type: argument.WeakenConclusion,
        description: "Weaken conjunction to just first conjunct",
        original: Some(formalization.conclusion),
        suggested: Some(left),
        confidence: 0.4,
        explanation: "Perhaps only part of the conjunction follows",
      ),
      RepairCandidate(
        repair_type: argument.WeakenConclusion,
        description: "Weaken conjunction to just second conjunct",
        original: Some(formalization.conclusion),
        suggested: Some(right),
        confidence: 0.4,
        explanation: "Perhaps only part of the conjunction follows",
      ),
    ]
    _ -> []
  }
}

// =============================================================================
// Logic System Change Suggestions
// =============================================================================

fn generate_logic_changes(
  formalization: Formalization,
  _countermodel: Countermodel,
) -> List(RepairCandidate) {
  let current = formalization.logic_system
  let stronger_systems = get_stronger_systems(current)

  stronger_systems
  |> list.map(fn(system) {
    RepairCandidate(
      repair_type: argument.ChangeLogicSystem,
      description: "Consider using "
        <> logic_system_to_string(system)
        <> " logic",
      original: None,
      suggested: None,
      confidence: 0.3,
      explanation: logic_system_to_string(system)
        <> " has stronger frame conditions that may validate this argument",
    )
  })
}

fn get_stronger_systems(current: LogicSystem) -> List(LogicSystem) {
  case current {
    proposition.K -> [proposition.T, proposition.S4, proposition.S5]
    proposition.T -> [proposition.S4, proposition.S5]
    proposition.K4 -> [proposition.S4, proposition.S5]
    proposition.S4 -> [proposition.S5]
    proposition.S5 -> []
    proposition.KD -> [proposition.KD45]
    proposition.KD45 -> []
  }
}

// =============================================================================
// Modal Modification Suggestions
// =============================================================================

fn generate_modal_modifications(
  _formalization: Formalization,
  analysis: CountermodelAnalysis,
) -> List(RepairCandidate) {
  let has_scope_issues =
    list.any(analysis.structural_issues, fn(i) { string.contains(i, "scope") })

  case has_scope_issues {
    True -> [
      RepairCandidate(
        repair_type: argument.ModifyModality,
        description: "Clarify modal scope in premises",
        original: None,
        suggested: None,
        confidence: 0.5,
        explanation: "Nested modalities may have ambiguous scope; consider rephrasing",
      ),
    ]
    False -> []
  }
}

// =============================================================================
// Repair Application
// =============================================================================

/// Apply a repair to a formalization (based on repair type)
pub fn apply_repair(
  formalization: Formalization,
  repair: RepairSuggestion,
) -> Formalization {
  case repair.repaired_formalization {
    Some(repaired) -> repaired
    None -> formalization
  }
}

/// Check if a repair would be valid
pub fn validate_repair(
  _formalization: Formalization,
  repair: RepairSuggestion,
) -> Bool {
  case repair.repair_type {
    argument.AddPremise -> True
    argument.StrengthenPremise -> True
    argument.WeakenConclusion -> True
    argument.ChangeLogicSystem -> True
    argument.ModifyModality -> True
    argument.ResolveAmbiguity -> True
  }
}

// =============================================================================
// Repair Session Management
// =============================================================================

/// Create a new repair session
pub fn new_session(formalization: Formalization) -> RepairSession {
  RepairSession(original: formalization, attempts: [], best_suggestion: None)
}

/// Record a repair attempt
pub fn record_attempt(
  session: RepairSession,
  repair: RepairSuggestion,
  succeeded: Bool,
  new_countermodel: Option(Countermodel),
) -> RepairSession {
  let attempt =
    RepairAttempt(
      repair: repair,
      succeeded: succeeded,
      new_countermodel: new_countermodel,
    )

  RepairSession(..session, attempts: [attempt, ..session.attempts])
}

/// Get the number of attempts in a session
pub fn attempt_count(session: RepairSession) -> Int {
  list.length(session.attempts)
}

/// Check if any attempt succeeded
pub fn has_successful_attempt(session: RepairSession) -> Bool {
  list.any(session.attempts, fn(a) { a.succeeded })
}

// =============================================================================
// Formatting
// =============================================================================

/// Format a repair suggestion for display
pub fn format_suggestion(repair: RepairSuggestion) -> String {
  let type_str = case repair.repair_type {
    argument.AddPremise -> "[Add Premise]"
    argument.StrengthenPremise -> "[Strengthen]"
    argument.WeakenConclusion -> "[Weaken]"
    argument.ChangeLogicSystem -> "[Change Logic]"
    argument.ModifyModality -> "[Modify Modal]"
    argument.ResolveAmbiguity -> "[Resolve Ambiguity]"
  }

  let confidence_str =
    " (confidence: " <> format_confidence(repair.confidence) <> ")"

  type_str <> " " <> repair.description <> confidence_str
}

/// Format multiple suggestions
pub fn format_suggestions(repairs: List(RepairSuggestion)) -> String {
  case list.length(repairs) {
    0 -> "No repair suggestions available."
    n ->
      "Found "
      <> int_to_string(n)
      <> " repair suggestion(s):\n\n"
      <> string.join(
        list.index_map(repairs, fn(r, i) {
          int_to_string(i + 1) <> ". " <> format_suggestion(r)
        }),
        "\n\n",
      )
  }
}

fn format_confidence(c: Float) -> String {
  let percent = float_to_int(c *. 100.0)
  int_to_string(percent) <> "%"
}

// =============================================================================
// Helper Functions
// =============================================================================

fn candidate_to_suggestion(
  candidate: RepairCandidate,
  formalization: Formalization,
  index: Int,
) -> RepairSuggestion {
  // Build repaired formalization if we have a suggested proposition
  let repaired = case candidate.repair_type, candidate.suggested {
    argument.AddPremise, Some(prop) ->
      Some(
        argument.Formalization(
          ..formalization,
          premises: list.append(formalization.premises, [prop]),
        ),
      )
    argument.WeakenConclusion, Some(prop) ->
      Some(argument.Formalization(..formalization, conclusion: prop))
    argument.StrengthenPremise, Some(new_prop) ->
      case candidate.original {
        Some(old_prop) ->
          Some(
            argument.Formalization(
              ..formalization,
              premises: list.map(formalization.premises, fn(p) {
                case p == old_prop {
                  True -> new_prop
                  False -> p
                }
              }),
            ),
          )
        None -> None
      }
    _, _ -> None
  }

  argument.RepairSuggestion(
    id: "repair-" <> int_to_string(index),
    formalization_id: formalization.id,
    repair_type: candidate.repair_type,
    description: candidate.description,
    repaired_formalization: repaired,
    confidence: candidate.confidence,
    created_at: "",
  )
}

fn create_detailed_repair(
  suggestion: RepairSuggestion,
  _analysis: CountermodelAnalysis,
) -> DetailedRepair {
  let affected = case suggestion.repaired_formalization {
    Some(form) ->
      list.flat_map(form.premises, collect_prop_atoms)
      |> list.unique
    None -> []
  }

  let risk = case suggestion.repair_type {
    argument.AddPremise -> LowRisk
    argument.StrengthenPremise -> MediumRisk
    argument.WeakenConclusion -> MediumRisk
    argument.ChangeLogicSystem -> HighRisk
    argument.ModifyModality -> LowRisk
    argument.ResolveAmbiguity -> LowRisk
  }

  let preserves = case suggestion.repair_type {
    argument.AddPremise -> True
    argument.ModifyModality -> True
    argument.ResolveAmbiguity -> True
    _ -> False
  }

  DetailedRepair(
    suggestion: suggestion,
    blocking_explanation: suggestion.description,
    affected_propositions: affected,
    preserves_intent: preserves,
    risk_level: risk,
  )
}

fn collect_prop_atoms(prop: Proposition) -> List(String) {
  case prop {
    proposition.Atom(name) -> [name]
    proposition.Not(inner) -> collect_prop_atoms(inner)
    proposition.And(left, right) ->
      list.append(collect_prop_atoms(left), collect_prop_atoms(right))
    proposition.Or(left, right) ->
      list.append(collect_prop_atoms(left), collect_prop_atoms(right))
    proposition.Implies(ante, cons) ->
      list.append(collect_prop_atoms(ante), collect_prop_atoms(cons))
    proposition.Necessary(inner) -> collect_prop_atoms(inner)
    proposition.Possible(inner) -> collect_prop_atoms(inner)
    proposition.Obligatory(inner) -> collect_prop_atoms(inner)
    proposition.Permitted(inner) -> collect_prop_atoms(inner)
    proposition.Knows(_, inner) -> collect_prop_atoms(inner)
    proposition.Believes(_, inner) -> collect_prop_atoms(inner)
  }
}

fn add_if(
  list_val: List(a),
  condition: Bool,
  f: fn(List(a)) -> List(a),
) -> List(a) {
  case condition {
    True -> f(list_val)
    False -> list_val
  }
}

fn float_compare(a: Float, b: Float) -> order.Order {
  case a <. b {
    True -> order.Lt
    False ->
      case a >. b {
        True -> order.Gt
        False -> order.Eq
      }
  }
}

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

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int
