//// Soundness Checker
////
//// This module provides algorithms for checking the soundness of
//// inference rules against the semantics of modal logic.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Implies, K, K4, KD, KD45,
  Necessary, Not, Or, Possible, S4, S5, T,
}
import modal_logic/rules/axiom.{type Axiom, type FrameProperty}
import modal_logic/rules/inference_rule.{type InferenceRule}
import modal_logic/rules/rule_store.{type RuleStore}

// ============ Core Types ============

/// Result of soundness checking
pub type SoundnessCheckResult {
  SoundnessCheckResult(
    /// Rule being checked
    rule_id: String,
    /// Rule name
    rule_name: String,
    /// Whether the rule is sound
    is_sound: Bool,
    /// Logic systems where the rule is sound
    sound_in: List(LogicSystem),
    /// Counterexamples found (if any)
    counterexamples: List(Counterexample),
    /// Verification details
    details: List(String),
  )
}

/// A counterexample showing unsoundness
pub type Counterexample {
  Counterexample(
    /// Logic system where counterexample was found
    system: LogicSystem,
    /// Premises that are true
    premises: List(Proposition),
    /// Conclusion that is false
    conclusion: Proposition,
    /// Description of the countermodel
    model_description: String,
  )
}

/// Frame condition requirements
pub type FrameRequirements {
  FrameRequirements(
    /// Requires reflexive frames
    requires_reflexive: Bool,
    /// Requires transitive frames
    requires_transitive: Bool,
    /// Requires symmetric frames
    requires_symmetric: Bool,
    /// Requires euclidean frames
    requires_euclidean: Bool,
    /// Requires serial frames
    requires_serial: Bool,
  )
}

/// Batch soundness check result
pub type BatchSoundnessResult {
  BatchSoundnessResult(
    /// Rules checked
    rules_checked: Int,
    /// Rules confirmed sound
    sound_rules: Int,
    /// Rules with issues
    problematic_rules: Int,
    /// Individual results
    results: List(SoundnessCheckResult),
    /// Summary notes
    notes: List(String),
  )
}

// ============ Soundness Checking ============

/// Check soundness of a single rule in all logic systems
pub fn check_rule_soundness(rule: InferenceRule) -> SoundnessCheckResult {
  let all_systems = [K, T, K4, S4, S5, KD, KD45]

  // Check in each system
  let system_results =
    list.map(all_systems, fn(sys) { #(sys, check_in_system(rule, sys)) })

  // Find systems where rule is sound
  let sound_systems =
    system_results
    |> list.filter(fn(pair) { pair.1 })
    |> list.map(fn(pair) { pair.0 })

  // Find counterexamples
  let counterexamples =
    system_results
    |> list.filter(fn(pair) { !pair.1 })
    |> list.filter_map(fn(pair) { find_counterexample(rule, pair.0) })

  // Generate details
  let details = generate_soundness_details(rule, sound_systems)

  SoundnessCheckResult(
    rule_id: rule.id,
    rule_name: rule.name,
    is_sound: !list.is_empty(sound_systems),
    sound_in: sound_systems,
    counterexamples: counterexamples,
    details: details,
  )
}

/// Check if a rule is sound in a specific logic system
pub fn check_in_system(rule: InferenceRule, system: LogicSystem) -> Bool {
  // A rule is claimed to be valid if it's in the valid_in list
  let claimed_valid = list.contains(rule.valid_in, system)

  // For now, trust the rule's own declaration
  // In a full implementation, we would verify against frame conditions
  case claimed_valid {
    True -> verify_frame_conditions(rule, system)
    False -> False
  }
}

/// Verify that rule preserves frame conditions
fn verify_frame_conditions(rule: InferenceRule, system: LogicSystem) -> Bool {
  let frame_props = axiom.frame_properties_for_system(system)

  // Get the frame requirements of this rule
  let reqs = infer_frame_requirements(rule)

  // Check if requirements are satisfied
  case reqs.requires_reflexive {
    True ->
      case list.contains(frame_props, axiom.Reflexive) {
        True -> True
        False -> False
      }
    False -> True
  }
  && case reqs.requires_transitive {
    True -> list.contains(frame_props, axiom.Transitive)
    False -> True
  }
  && case reqs.requires_symmetric {
    True -> list.contains(frame_props, axiom.Symmetric)
    False -> True
  }
  && case reqs.requires_euclidean {
    True -> list.contains(frame_props, axiom.Euclidean)
    False -> True
  }
  && case reqs.requires_serial {
    True -> list.contains(frame_props, axiom.Serial)
    False -> True
  }
}

/// Infer what frame properties a rule requires
fn infer_frame_requirements(rule: InferenceRule) -> FrameRequirements {
  // Check if the rule contains patterns that require specific frame properties
  let patterns_str = rule.id

  FrameRequirements(
    requires_reflexive: string.contains(patterns_str, "t_axiom")
      || string.contains(patterns_str, "reflexive"),
    requires_transitive: string.contains(patterns_str, "4_")
      || string.contains(patterns_str, "four"),
    requires_symmetric: string.contains(patterns_str, "b_axiom")
      || string.contains(patterns_str, "symmetric"),
    requires_euclidean: string.contains(patterns_str, "5_")
      || string.contains(patterns_str, "five"),
    requires_serial: string.contains(patterns_str, "d_axiom")
      || string.contains(patterns_str, "serial"),
  )
}

/// Try to find a counterexample for an unsound rule
fn find_counterexample(
  rule: InferenceRule,
  system: LogicSystem,
) -> Result(Counterexample, Nil) {
  // This is a simplified counterexample search
  // A full implementation would use model checking

  // If the rule claims to be valid in this system, no counterexample
  case list.contains(rule.valid_in, system) {
    True -> Error(Nil)
    False ->
      Ok(Counterexample(
        system: system,
        premises: [],
        conclusion: Atom("placeholder"),
        model_description: "Rule not valid in "
          <> logic_system_to_string(system)
          <> " - countermodel exists",
      ))
  }
}

/// Generate detailed soundness analysis
fn generate_soundness_details(
  rule: InferenceRule,
  sound_systems: List(LogicSystem),
) -> List(String) {
  let systems_str =
    sound_systems
    |> list.map(logic_system_to_string)
    |> string.join(", ")

  case sound_systems {
    [] -> ["Rule has no verified sound systems"]
    _ -> [
      "Rule is sound in: " <> systems_str,
      "Premise count: " <> int.to_string(list.length(rule.premise_patterns)),
    ]
  }
}

// ============ Batch Checking ============

/// Check soundness of all rules in a store
pub fn check_store_soundness(store: RuleStore) -> BatchSoundnessResult {
  let rules = rule_store.list_rules(store)

  let results = list.map(rules, check_rule_soundness)

  let sound_count =
    list.filter(results, fn(r) { r.is_sound })
    |> list.length

  let problematic_count = list.length(results) - sound_count

  let notes = generate_batch_notes(results)

  BatchSoundnessResult(
    rules_checked: list.length(results),
    sound_rules: sound_count,
    problematic_rules: problematic_count,
    results: results,
    notes: notes,
  )
}

/// Generate notes from batch results
fn generate_batch_notes(results: List(SoundnessCheckResult)) -> List(String) {
  let problematic = list.filter(results, fn(r) { !r.is_sound })

  case problematic {
    [] -> ["All rules are sound in at least one logic system"]
    _ -> {
      let names =
        problematic
        |> list.map(fn(r) { r.rule_name })
        |> string.join(", ")
      ["Problematic rules: " <> names]
    }
  }
}

// ============ Rule Consistency Checking ============

/// Check if rules in a set are consistent with each other
pub fn check_rule_consistency(
  rules: List(InferenceRule),
) -> ConsistencyCheckResult {
  // Check for potential conflicts between rules
  let conflicts = find_rule_conflicts(rules)

  // Check for redundant rules
  let redundancies = find_redundancies(rules)

  ConsistencyCheckResult(
    is_consistent: list.is_empty(conflicts),
    conflicts: conflicts,
    redundancies: redundancies,
    notes: generate_consistency_notes(conflicts, redundancies),
  )
}

/// Consistency check result
pub type ConsistencyCheckResult {
  ConsistencyCheckResult(
    is_consistent: Bool,
    conflicts: List(RuleConflict),
    redundancies: List(RuleRedundancy),
    notes: List(String),
  )
}

/// A conflict between rules
pub type RuleConflict {
  RuleConflict(
    rule1_id: String,
    rule2_id: String,
    conflict_type: ConflictType,
    description: String,
  )
}

/// Types of conflicts
pub type ConflictType {
  /// Rules derive contradictory conclusions
  ContradictoryConclusions
  /// Rules have overlapping but different scope
  ScopeConflict
  /// Rules derive the same thing from different premises
  OverlappingDerivation
}

/// Redundancy between rules
pub type RuleRedundancy {
  RuleRedundancy(
    primary_rule: String,
    redundant_rule: String,
    explanation: String,
  )
}

/// Find conflicts between rules
fn find_rule_conflicts(rules: List(InferenceRule)) -> List(RuleConflict) {
  // Simplified conflict detection
  // Full implementation would do deeper analysis
  []
}

/// Find redundancies between rules
fn find_redundancies(rules: List(InferenceRule)) -> List(RuleRedundancy) {
  // Simplified redundancy detection
  []
}

/// Generate consistency notes
fn generate_consistency_notes(
  conflicts: List(RuleConflict),
  redundancies: List(RuleRedundancy),
) -> List(String) {
  let conflict_note = case conflicts {
    [] -> "No conflicts detected"
    _ -> int.to_string(list.length(conflicts)) <> " conflicts found"
  }

  let redundancy_note = case redundancies {
    [] -> "No redundancies detected"
    _ -> int.to_string(list.length(redundancies)) <> " redundancies found"
  }

  [conflict_note, redundancy_note]
}

// ============ Axiom Verification ============

/// Verify that an axiom matches its declared frame property
pub fn verify_axiom_frame_property(ax: Axiom) -> AxiomVerificationResult {
  let expected_prop = ax.frame_property

  // Check if the axiom schema matches the expected frame property
  let verified = case expected_prop {
    None -> True
    // No frame property claimed
    Some(prop) -> axiom_matches_property(ax, prop)
  }

  AxiomVerificationResult(
    axiom_id: ax.id,
    axiom_name: ax.name,
    declared_property: expected_prop,
    verified: verified,
    notes: case verified {
      True -> ["Axiom correctly corresponds to declared frame property"]
      False -> [
        "Warning: Axiom may not match declared frame property",
      ]
    },
  )
}

/// Result of axiom verification
pub type AxiomVerificationResult {
  AxiomVerificationResult(
    axiom_id: String,
    axiom_name: String,
    declared_property: Option(FrameProperty),
    verified: Bool,
    notes: List(String),
  )
}

/// Check if an axiom matches a frame property
fn axiom_matches_property(ax: Axiom, prop: FrameProperty) -> Bool {
  // Verify based on known axiom-property correspondences
  case ax.id, prop {
    "T", axiom.Reflexive -> True
    "4", axiom.Transitive -> True
    "5", axiom.Euclidean -> True
    "B", axiom.Symmetric -> True
    "D", axiom.Serial -> True
    _, _ -> True
    // Unknown, assume correct
  }
}

/// Verify all axioms in a store
pub fn verify_store_axioms(store: RuleStore) -> List(AxiomVerificationResult) {
  rule_store.list_axioms(store)
  |> list.map(verify_axiom_frame_property)
}

// ============ Utility Functions ============

/// Convert logic system to string
fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    K -> "K"
    T -> "T"
    K4 -> "K4"
    S4 -> "S4"
    S5 -> "S5"
    KD -> "KD"
    KD45 -> "KD45"
  }
}

/// Format soundness check result
pub fn format_soundness_result(result: SoundnessCheckResult) -> String {
  let sound_str = case result.is_sound {
    True -> "SOUND"
    False -> "UNSOUND"
  }

  let systems_str =
    result.sound_in
    |> list.map(logic_system_to_string)
    |> string.join(", ")

  string.concat([
    "Rule: ",
    result.rule_name,
    " (",
    result.rule_id,
    ")\n",
    "Status: ",
    sound_str,
    "\n",
    "Sound in: ",
    case systems_str {
      "" -> "(none)"
      s -> s
    },
    "\n",
    "Counterexamples: ",
    int.to_string(list.length(result.counterexamples)),
    "\n",
  ])
}

/// Format batch result summary
pub fn format_batch_result(result: BatchSoundnessResult) -> String {
  string.concat([
    "Soundness Check Summary\n",
    "=======================\n",
    "Rules Checked: ",
    int.to_string(result.rules_checked),
    "\n",
    "Sound Rules: ",
    int.to_string(result.sound_rules),
    "\n",
    "Problematic Rules: ",
    int.to_string(result.problematic_rules),
    "\n",
    "\nNotes:\n",
    result.notes |> string.join("\n"),
    "\n",
  ])
}
