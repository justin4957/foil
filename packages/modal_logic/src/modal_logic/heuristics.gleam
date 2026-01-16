//// Heuristic Validation for Fast Modal Logic Checking
////
//// This module provides tiered heuristic validation that can determine
//// validity for simple cases without invoking the full Z3 solver.
////
//// ## Validation Tiers
////
//// - **Tier 1: Syntactic Checks** (<1ms)
////   - Tautology detection (p ∨ ¬p, p → p)
////   - Contradiction detection (p ∧ ¬p)
////   - Identity patterns (conclusion equals premise)
////
//// - **Tier 2: Truth Table Analysis** (<50ms)
////   - Complete enumeration for small propositional formulas (≤5 variables)
////   - Pattern matching against known valid/invalid patterns
////
//// - **Tier 3: Full Z3 Solving** (fallback)
////   - Complete SMT solving with countermodel extraction
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/heuristics
////
//// case heuristics.try_heuristic_validation(formalization) {
////   Some(#(result, tier)) -> // Fast path succeeded
////   None -> // Fall through to Z3
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import modal_logic/argument.{
  type Formalization, type ValidationResult, Invalid, Valid,
}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, K4, KD,
  KD45, Knows, Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}

// =============================================================================
// Types
// =============================================================================

/// Which validation tier was used to determine the result
pub type ValidationTier {
  /// Tier 1: Syntactic pattern matching (<1ms)
  Tier1Syntactic
  /// Tier 2: Truth table enumeration (<50ms)
  Tier2TruthTable
  /// Tier 3: Full Z3 SMT solving
  Tier3Z3
}

/// Result of a heuristic validation attempt
pub type HeuristicResult {
  HeuristicResult(
    /// The validation result
    result: ValidationResult,
    /// Which tier determined the result
    tier: ValidationTier,
    /// Explanation of how the result was determined
    explanation: String,
  )
}

/// Configuration for heuristic validation
pub type HeuristicConfig {
  HeuristicConfig(
    /// Enable Tier 1 syntactic checks
    enable_tier1: Bool,
    /// Enable Tier 2 truth table analysis
    enable_tier2: Bool,
    /// Maximum variables for truth table analysis
    max_truth_table_vars: Int,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Default heuristic configuration
pub fn default_config() -> HeuristicConfig {
  HeuristicConfig(
    enable_tier1: True,
    enable_tier2: True,
    max_truth_table_vars: 5,
  )
}

/// Fast configuration (only Tier 1)
pub fn fast_config() -> HeuristicConfig {
  HeuristicConfig(
    enable_tier1: True,
    enable_tier2: False,
    max_truth_table_vars: 0,
  )
}

/// Full configuration (all tiers enabled, more variables)
pub fn full_config() -> HeuristicConfig {
  HeuristicConfig(
    enable_tier1: True,
    enable_tier2: True,
    max_truth_table_vars: 8,
  )
}

// =============================================================================
// Main Entry Point
// =============================================================================

/// Try to validate using heuristics before falling back to Z3
///
/// Returns Some(HeuristicResult) if heuristics can determine validity,
/// None if Z3 is needed.
pub fn try_heuristic_validation(
  formalization: Formalization,
) -> Option(HeuristicResult) {
  try_heuristic_validation_with_config(formalization, default_config())
}

/// Try heuristic validation with custom configuration
pub fn try_heuristic_validation_with_config(
  formalization: Formalization,
  config: HeuristicConfig,
) -> Option(HeuristicResult) {
  // Try Tier 1: Syntactic checks
  case config.enable_tier1 {
    True -> {
      case try_tier1_validation(formalization) {
        Some(result) -> Some(result)
        None -> {
          // Try Tier 2: Truth table
          case config.enable_tier2 {
            True -> try_tier2_validation(formalization, config)
            False -> None
          }
        }
      }
    }
    False -> {
      case config.enable_tier2 {
        True -> try_tier2_validation(formalization, config)
        False -> None
      }
    }
  }
}

// =============================================================================
// Tier 1: Syntactic Checks
// =============================================================================

/// Tier 1: Fast syntactic pattern matching
///
/// Checks for:
/// - Tautologies (always true patterns)
/// - Contradictions (always false patterns)
/// - Identity (conclusion is a premise)
/// - Trivial implications
fn try_tier1_validation(formalization: Formalization) -> Option(HeuristicResult) {
  // Check if conclusion is identical to a premise (immediate validity)
  case check_identity_validity(formalization) {
    Some(result) -> Some(result)
    None -> {
      // Check for tautological conclusion
      case check_tautology(formalization.conclusion) {
        Some(result) -> Some(result)
        None -> {
          // Check for contradictory premises
          case check_premise_contradiction(formalization.premises) {
            Some(result) -> Some(result)
            None -> {
              // Check for contradictory conclusion (argument valid if negation is contradiction)
              case check_conclusion_contradiction(formalization.conclusion) {
                Some(result) -> Some(result)
                None -> {
                  // Check modal axiom patterns
                  check_modal_axiom_patterns(formalization)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Check if conclusion is identical to one of the premises
fn check_identity_validity(
  formalization: Formalization,
) -> Option(HeuristicResult) {
  let conclusion = formalization.conclusion
  let is_premise =
    list.any(formalization.premises, fn(p) { propositions_equal(p, conclusion) })

  case is_premise {
    True ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "Conclusion is identical to a premise (identity rule)",
      ))
    False -> None
  }
}

/// Check if a proposition is a tautology
fn check_tautology(prop: Proposition) -> Option(HeuristicResult) {
  case is_tautology(prop) {
    True ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "Conclusion is a tautology",
      ))
    False -> None
  }
}

/// Check if proposition is a tautology pattern
fn is_tautology(prop: Proposition) -> Bool {
  case prop {
    // p ∨ ¬p (law of excluded middle)
    Or(p, Not(q)) -> propositions_equal(p, q)
    Or(Not(p), q) -> propositions_equal(p, q)

    // p → p (reflexivity of implication)
    Implies(p, q) -> {
      case propositions_equal(p, q) {
        True -> True
        False -> check_implication_tautology(p, q)
      }
    }

    // ¬(p ∧ ¬p) (non-contradiction)
    Not(And(p, Not(q))) -> propositions_equal(p, q)
    Not(And(Not(p), q)) -> propositions_equal(p, q)

    _ -> False
  }
}

/// Check implication-specific tautology patterns
fn check_implication_tautology(
  antecedent: Proposition,
  consequent: Proposition,
) -> Bool {
  case antecedent, consequent {
    // p → (q → p) (weakening)
    p, Implies(_, r) -> propositions_equal(p, r)

    // □p → □p, ◇p → ◇p (modal identity)
    Necessary(p), Necessary(q) -> propositions_equal(p, q)
    Possible(p), Possible(q) -> propositions_equal(p, q)

    _, _ -> False
  }
}

/// Check if premises contain a contradiction
fn check_premise_contradiction(
  premises: List(Proposition),
) -> Option(HeuristicResult) {
  // Check for explicit p and ¬p in premises
  let has_contradiction =
    list.any(premises, fn(p) {
      case p {
        Not(inner) -> list.any(premises, fn(q) { propositions_equal(inner, q) })
        _ ->
          list.any(premises, fn(q) {
            case q {
              Not(inner) -> propositions_equal(p, inner)
              _ -> False
            }
          })
      }
    })

  case has_contradiction {
    True ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "Premises contain a contradiction (p and ¬p), so any conclusion follows (ex falso quodlibet)",
      ))
    False -> None
  }
}

/// Check if conclusion is a contradiction (makes argument trivially valid
/// since its negation is a tautology)
fn check_conclusion_contradiction(
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  case is_contradiction(conclusion) {
    True ->
      Some(HeuristicResult(
        result: Invalid("Conclusion is a contradiction and can never be true"),
        tier: Tier1Syntactic,
        explanation: "Conclusion is a contradiction (p ∧ ¬p pattern)",
      ))
    False -> None
  }
}

/// Check if proposition is a contradiction pattern
fn is_contradiction(prop: Proposition) -> Bool {
  case prop {
    // p ∧ ¬p
    And(p, Not(q)) -> propositions_equal(p, q)
    And(Not(p), q) -> propositions_equal(p, q)

    // ¬(p ∨ ¬p) (denial of excluded middle)
    Not(Or(p, Not(q))) -> propositions_equal(p, q)
    Not(Or(Not(p), q)) -> propositions_equal(p, q)

    _ -> False
  }
}

/// Check for modal axiom patterns that are always valid/invalid in specific systems
fn check_modal_axiom_patterns(
  formalization: Formalization,
) -> Option(HeuristicResult) {
  let conclusion = formalization.conclusion
  let system = formalization.logic_system

  case conclusion {
    // T axiom: □p → p (valid in T, S4, S5; invalid in K, K4, KD, KD45)
    Implies(Necessary(p), q) -> {
      case propositions_equal(p, q) {
        True -> check_t_axiom(system)
        False -> check_other_implication_patterns(conclusion, system)
      }
    }

    // Dual: ◇p → ¬□¬p
    Implies(Possible(p), Not(Necessary(Not(q)))) -> {
      case propositions_equal(p, q) {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Modal dual: ◇p → ¬□¬p is valid in all modal logics",
          ))
        False -> None
      }
    }

    // Dual: ¬□¬p → ◇p
    Implies(Not(Necessary(Not(p))), Possible(q)) -> {
      case propositions_equal(p, q) {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Modal dual: ¬□¬p → ◇p is valid in all modal logics",
          ))
        False -> None
      }
    }

    _ -> None
  }
}

/// Check T axiom validity based on logic system
fn check_t_axiom(system: LogicSystem) -> Option(HeuristicResult) {
  case system {
    T | S4 | S5 ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "T axiom (□p → p) is valid in "
          <> logic_system_name(system)
          <> " due to reflexivity",
      ))
    K | K4 | KD | KD45 ->
      Some(HeuristicResult(
        result: Invalid(
          "T axiom (□p → p) requires reflexivity, which "
          <> logic_system_name(system)
          <> " lacks",
        ),
        tier: Tier1Syntactic,
        explanation: "T axiom requires reflexive frames",
      ))
  }
}

/// Check other implication patterns (4 axiom, 5 axiom, D axiom, K axiom)
fn check_other_implication_patterns(
  conclusion: Proposition,
  system: LogicSystem,
) -> Option(HeuristicResult) {
  case conclusion {
    // 4 axiom: □p → □□p
    Implies(Necessary(p), Necessary(Necessary(q))) -> {
      case propositions_equal(p, q) {
        True -> check_4_axiom(system)
        False -> None
      }
    }

    // 5 axiom: ◇p → □◇p
    Implies(Possible(p), Necessary(Possible(q))) -> {
      case propositions_equal(p, q) {
        True -> check_5_axiom(system)
        False -> None
      }
    }

    // D axiom: □p → ◇p
    Implies(Necessary(p), Possible(q)) -> {
      case propositions_equal(p, q) {
        True -> check_d_axiom(system)
        False -> None
      }
    }

    // K axiom: □(p → q) → (□p → □q)
    Implies(Necessary(Implies(p, q)), Implies(Necessary(r), Necessary(s))) -> {
      case propositions_equal(p, r) && propositions_equal(q, s) {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "K axiom (distribution) is valid in all normal modal logics",
          ))
        False -> None
      }
    }

    _ -> None
  }
}

/// Check 4 axiom validity based on logic system
fn check_4_axiom(system: LogicSystem) -> Option(HeuristicResult) {
  case system {
    K4 | S4 | S5 | KD45 ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "4 axiom (□p → □□p) is valid in "
          <> logic_system_name(system)
          <> " due to transitivity",
      ))
    _ -> None
  }
}

/// Check 5 axiom validity based on logic system
fn check_5_axiom(system: LogicSystem) -> Option(HeuristicResult) {
  case system {
    S5 | KD45 ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "5 axiom (◇p → □◇p) is valid in "
          <> logic_system_name(system)
          <> " due to euclidean property",
      ))
    _ -> None
  }
}

/// Check D axiom validity based on logic system
fn check_d_axiom(system: LogicSystem) -> Option(HeuristicResult) {
  case system {
    KD | KD45 | T | S4 | S5 ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "D axiom (□p → ◇p) is valid in "
          <> logic_system_name(system)
          <> " due to seriality",
      ))
    K | K4 -> None
  }
}

// =============================================================================
// Tier 2: Truth Table Analysis
// =============================================================================

/// Tier 2: Truth table enumeration for small propositional formulas
fn try_tier2_validation(
  formalization: Formalization,
  config: HeuristicConfig,
) -> Option(HeuristicResult) {
  // Check if formula is purely propositional (no modal operators)
  let has_modals = has_modal_operators(formalization)

  case has_modals {
    True -> None
    // Modal formulas need Z3
    False -> {
      // Collect all atoms
      let atoms = collect_all_atoms(formalization)
      let atom_count = set.size(atoms)

      // Only do truth table if small enough
      case atom_count <= config.max_truth_table_vars {
        True -> evaluate_truth_table(formalization, atoms)
        False -> None
      }
    }
  }
}

/// Check if formalization contains modal operators
fn has_modal_operators(formalization: Formalization) -> Bool {
  list.any(formalization.premises, proposition_has_modal)
  || proposition_has_modal(formalization.conclusion)
}

/// Check if a proposition contains modal operators
fn proposition_has_modal(prop: Proposition) -> Bool {
  case prop {
    Atom(_) -> False
    Not(p) -> proposition_has_modal(p)
    And(p, q) -> proposition_has_modal(p) || proposition_has_modal(q)
    Or(p, q) -> proposition_has_modal(p) || proposition_has_modal(q)
    Implies(p, q) -> proposition_has_modal(p) || proposition_has_modal(q)
    Necessary(_) -> True
    Possible(_) -> True
    Obligatory(_) -> True
    Permitted(_) -> True
    Knows(_, _) -> True
    Believes(_, _) -> True
  }
}

/// Collect all atoms from a formalization
fn collect_all_atoms(formalization: Formalization) -> Set(String) {
  let premise_atoms =
    list.flat_map(formalization.premises, fn(p) {
      set.to_list(collect_atoms_from_prop(p))
    })
  let conclusion_atoms =
    set.to_list(collect_atoms_from_prop(formalization.conclusion))

  set.from_list(list.append(premise_atoms, conclusion_atoms))
}

/// Collect atoms from a single proposition
fn collect_atoms_from_prop(prop: Proposition) -> Set(String) {
  case prop {
    Atom(name) -> set.from_list([name])
    Not(p) -> collect_atoms_from_prop(p)
    And(p, q) ->
      set.union(collect_atoms_from_prop(p), collect_atoms_from_prop(q))
    Or(p, q) ->
      set.union(collect_atoms_from_prop(p), collect_atoms_from_prop(q))
    Implies(p, q) ->
      set.union(collect_atoms_from_prop(p), collect_atoms_from_prop(q))
    Necessary(p) -> collect_atoms_from_prop(p)
    Possible(p) -> collect_atoms_from_prop(p)
    Obligatory(p) -> collect_atoms_from_prop(p)
    Permitted(p) -> collect_atoms_from_prop(p)
    Knows(_, p) -> collect_atoms_from_prop(p)
    Believes(_, p) -> collect_atoms_from_prop(p)
  }
}

/// Evaluate all truth table rows to determine validity
fn evaluate_truth_table(
  formalization: Formalization,
  atoms: Set(String),
) -> Option(HeuristicResult) {
  let atom_list = set.to_list(atoms)
  let assignments = generate_all_assignments(atom_list)

  // Check each assignment
  let results =
    list.map(assignments, fn(assignment) {
      check_assignment(formalization, assignment)
    })

  // If all assignments make the argument valid (premises true → conclusion true),
  // then the argument is valid
  let all_valid = list.all(results, fn(r) { r })

  // Find a counterexample if not all valid
  let counterexample =
    list.find(list.zip(assignments, results), fn(pair) {
      let #(_, valid) = pair
      !valid
    })

  case all_valid {
    True ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier2TruthTable,
        explanation: "Truth table analysis: argument valid for all "
          <> int.to_string(list.length(assignments))
          <> " truth assignments",
      ))
    False -> {
      case counterexample {
        Ok(#(assignment, _)) -> {
          let countermodel_str = format_assignment(assignment)
          Some(HeuristicResult(
            result: Invalid("Counterexample found: " <> countermodel_str),
            tier: Tier2TruthTable,
            explanation: "Truth table analysis found counterexample",
          ))
        }
        Error(_) ->
          Some(HeuristicResult(
            result: Invalid("Counterexample exists"),
            tier: Tier2TruthTable,
            explanation: "Truth table analysis found counterexample",
          ))
      }
    }
  }
}

/// Generate all possible truth assignments for atoms
fn generate_all_assignments(atoms: List(String)) -> List(Dict(String, Bool)) {
  case atoms {
    [] -> [dict.new()]
    [atom, ..rest] -> {
      let rest_assignments = generate_all_assignments(rest)
      list.flat_map(rest_assignments, fn(assignment) {
        [
          dict.insert(assignment, atom, True),
          dict.insert(assignment, atom, False),
        ]
      })
    }
  }
}

/// Check if an assignment makes the argument valid
/// (all premises true implies conclusion true)
fn check_assignment(
  formalization: Formalization,
  assignment: Dict(String, Bool),
) -> Bool {
  let premises_true =
    list.all(formalization.premises, fn(p) { evaluate_prop(p, assignment) })
  let conclusion_true = evaluate_prop(formalization.conclusion, assignment)

  // Argument is valid for this assignment if: premises false OR conclusion true
  !premises_true || conclusion_true
}

/// Evaluate a proposition under a truth assignment
fn evaluate_prop(prop: Proposition, assignment: Dict(String, Bool)) -> Bool {
  case prop {
    Atom(name) -> {
      case dict.get(assignment, name) {
        Ok(value) -> value
        Error(_) -> False
      }
    }
    Not(p) -> !evaluate_prop(p, assignment)
    And(p, q) -> evaluate_prop(p, assignment) && evaluate_prop(q, assignment)
    Or(p, q) -> evaluate_prop(p, assignment) || evaluate_prop(q, assignment)
    Implies(p, q) ->
      !evaluate_prop(p, assignment) || evaluate_prop(q, assignment)
    // Modal operators - should not reach here due to earlier check
    Necessary(_) -> True
    Possible(_) -> True
    Obligatory(_) -> True
    Permitted(_) -> True
    Knows(_, _) -> True
    Believes(_, _) -> True
  }
}

/// Format an assignment as a string
fn format_assignment(assignment: Dict(String, Bool)) -> String {
  let pairs =
    dict.to_list(assignment)
    |> list.map(fn(pair) {
      let #(name, value) = pair
      name <> "=" <> bool_to_string(value)
    })

  "{" <> string.join(pairs, ", ") <> "}"
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Check if two propositions are structurally equal
pub fn propositions_equal(p1: Proposition, p2: Proposition) -> Bool {
  case p1, p2 {
    Atom(a), Atom(b) -> a == b
    Not(a), Not(b) -> propositions_equal(a, b)
    And(a1, a2), And(b1, b2) ->
      propositions_equal(a1, b1) && propositions_equal(a2, b2)
    Or(a1, a2), Or(b1, b2) ->
      propositions_equal(a1, b1) && propositions_equal(a2, b2)
    Implies(a1, a2), Implies(b1, b2) ->
      propositions_equal(a1, b1) && propositions_equal(a2, b2)
    Necessary(a), Necessary(b) -> propositions_equal(a, b)
    Possible(a), Possible(b) -> propositions_equal(a, b)
    Obligatory(a), Obligatory(b) -> propositions_equal(a, b)
    Permitted(a), Permitted(b) -> propositions_equal(a, b)
    Knows(ag1, p1_inner), Knows(ag2, p2_inner) ->
      ag1 == ag2 && propositions_equal(p1_inner, p2_inner)
    Believes(ag1, p1_inner), Believes(ag2, p2_inner) ->
      ag1 == ag2 && propositions_equal(p1_inner, p2_inner)
    _, _ -> False
  }
}

/// Get the name of a logic system
fn logic_system_name(system: LogicSystem) -> String {
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

/// Get a description of what each tier does
pub fn tier_description(tier: ValidationTier) -> String {
  case tier {
    Tier1Syntactic ->
      "Syntactic pattern matching: tautology/contradiction detection, identity rules, modal axiom patterns"
    Tier2TruthTable ->
      "Truth table enumeration: complete evaluation of all truth assignments for propositional formulas"
    Tier3Z3 ->
      "Z3 SMT solving: full symbolic verification with countermodel extraction"
  }
}

/// Get the expected latency for a tier
pub fn tier_expected_latency_ms(tier: ValidationTier) -> Int {
  case tier {
    Tier1Syntactic -> 1
    Tier2TruthTable -> 50
    Tier3Z3 -> 2000
  }
}
