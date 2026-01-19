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
/// - Classical inference rules (modus ponens, modus tollens, etc.)
/// - Disjunctive and conjunctive patterns
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
                  // Check classical inference patterns (modus ponens, etc.)
                  case check_classical_inference_patterns(formalization) {
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

// =============================================================================
// Classical Inference Patterns (Tier 1 Fast Path)
// =============================================================================

/// Check for classical propositional inference patterns
/// These can be resolved syntactically without truth table enumeration
fn check_classical_inference_patterns(
  formalization: Formalization,
) -> Option(HeuristicResult) {
  let premises = formalization.premises
  let conclusion = formalization.conclusion

  // Check modus ponens: p → q, p ⊢ q
  case check_modus_ponens(premises, conclusion) {
    Some(result) -> Some(result)
    None -> {
      // Check modus tollens: p → q, ¬q ⊢ ¬p
      case check_modus_tollens(premises, conclusion) {
        Some(result) -> Some(result)
        None -> {
          // Check hypothetical syllogism: p → q, q → r ⊢ p → r
          case check_hypothetical_syllogism(premises, conclusion) {
            Some(result) -> Some(result)
            None -> {
              // Check disjunctive syllogism: p ∨ q, ¬p ⊢ q
              case check_disjunctive_syllogism(premises, conclusion) {
                Some(result) -> Some(result)
                None -> {
                  // Check conjunction elimination: p ∧ q ⊢ p or p ∧ q ⊢ q
                  case check_conjunction_elimination(premises, conclusion) {
                    Some(result) -> Some(result)
                    None -> {
                      // Check double negation elimination: ¬¬p ⊢ p
                      case
                        check_double_negation_elimination(premises, conclusion)
                      {
                        Some(result) -> Some(result)
                        None -> {
                          // Check conjunction introduction: p, q ⊢ p ∧ q
                          case
                            check_conjunction_introduction(premises, conclusion)
                          {
                            Some(result) -> Some(result)
                            None -> {
                              // Check disjunction introduction: p ⊢ p ∨ q
                              case
                                check_disjunction_introduction(
                                  premises,
                                  conclusion,
                                )
                              {
                                Some(result) -> Some(result)
                                None -> {
                                  // Check common fallacy patterns (invalid arguments)
                                  check_common_fallacy_patterns(
                                    premises,
                                    conclusion,
                                  )
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Check modus ponens pattern: p → q, p ⊢ q
fn check_modus_ponens(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Look for an implication p → q where q matches conclusion
  let implication_match =
    list.find(premises, fn(premise) {
      case premise {
        Implies(_, consequent) -> propositions_equal(consequent, conclusion)
        _ -> False
      }
    })

  case implication_match {
    Ok(Implies(antecedent, _)) -> {
      // Check if the antecedent is also a premise
      let has_antecedent =
        list.any(premises, fn(p) { propositions_equal(p, antecedent) })
      case has_antecedent {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Modus ponens: from p → q and p, conclude q",
          ))
        False -> None
      }
    }
    _ -> None
  }
}

/// Check modus tollens pattern: p → q, ¬q ⊢ ¬p
fn check_modus_tollens(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Conclusion should be ¬p
  case conclusion {
    Not(negated_antecedent) -> {
      // Look for an implication p → q where p matches negated_antecedent
      let implication_match =
        list.find(premises, fn(premise) {
          case premise {
            Implies(antecedent, _) ->
              propositions_equal(antecedent, negated_antecedent)
            _ -> False
          }
        })

      case implication_match {
        Ok(Implies(_, consequent)) -> {
          // Check if ¬q (negation of consequent) is a premise
          let has_negated_consequent =
            list.any(premises, fn(p) {
              case p {
                Not(inner) -> propositions_equal(inner, consequent)
                _ -> False
              }
            })
          case has_negated_consequent {
            True ->
              Some(HeuristicResult(
                result: Valid,
                tier: Tier1Syntactic,
                explanation: "Modus tollens: from p → q and ¬q, conclude ¬p",
              ))
            False -> None
          }
        }
        _ -> None
      }
    }
    _ -> None
  }
}

/// Check hypothetical syllogism: p → q, q → r ⊢ p → r
fn check_hypothetical_syllogism(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  case conclusion {
    Implies(p, r) -> {
      // Find all implications in premises
      let implications =
        list.filter_map(premises, fn(premise) {
          case premise {
            Implies(a, b) -> Ok(#(a, b))
            _ -> Error(Nil)
          }
        })

      // Check if there's a chain p → q and q → r
      let has_chain =
        list.any(implications, fn(impl1) {
          let #(a, b) = impl1
          case propositions_equal(a, p) {
            True -> {
              // Found p → q, now look for q → r
              list.any(implications, fn(impl2) {
                let #(c, d) = impl2
                propositions_equal(c, b) && propositions_equal(d, r)
              })
            }
            False -> False
          }
        })

      case has_chain {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Hypothetical syllogism: from p → q and q → r, conclude p → r",
          ))
        False -> None
      }
    }
    _ -> None
  }
}

/// Check disjunctive syllogism: p ∨ q, ¬p ⊢ q  or  p ∨ q, ¬q ⊢ p
fn check_disjunctive_syllogism(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Look for a disjunction that contains the conclusion
  let disjunction_match =
    list.find(premises, fn(premise) {
      case premise {
        Or(left, right) ->
          propositions_equal(left, conclusion)
          || propositions_equal(right, conclusion)
        _ -> False
      }
    })

  case disjunction_match {
    Ok(Or(left, right)) -> {
      // Determine which disjunct is the conclusion and check if negation of other is a premise
      let #(other_disjunct, _is_left) = case
        propositions_equal(left, conclusion)
      {
        True -> #(right, True)
        False -> #(left, False)
      }

      // Check if ¬other_disjunct is a premise
      let has_negation =
        list.any(premises, fn(p) {
          case p {
            Not(inner) -> propositions_equal(inner, other_disjunct)
            _ -> False
          }
        })

      case has_negation {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Disjunctive syllogism: from p ∨ q and ¬p, conclude q",
          ))
        False -> None
      }
    }
    _ -> None
  }
}

/// Check conjunction elimination: p ∧ q ⊢ p  or  p ∧ q ⊢ q
fn check_conjunction_elimination(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Look for a conjunction that contains the conclusion as a conjunct
  let conjunction_match =
    list.any(premises, fn(premise) {
      case premise {
        And(left, right) ->
          propositions_equal(left, conclusion)
          || propositions_equal(right, conclusion)
        _ -> False
      }
    })

  case conjunction_match {
    True ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "Conjunction elimination: from p ∧ q, conclude p (or q)",
      ))
    False -> None
  }
}

/// Check double negation elimination: ¬¬p ⊢ p
fn check_double_negation_elimination(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Check if ¬¬conclusion is a premise
  let has_double_negation =
    list.any(premises, fn(premise) {
      case premise {
        Not(Not(inner)) -> propositions_equal(inner, conclusion)
        _ -> False
      }
    })

  case has_double_negation {
    True ->
      Some(HeuristicResult(
        result: Valid,
        tier: Tier1Syntactic,
        explanation: "Double negation elimination: from ¬¬p, conclude p",
      ))
    False -> None
  }
}

/// Check conjunction introduction: p, q ⊢ p ∧ q
fn check_conjunction_introduction(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  case conclusion {
    And(left, right) -> {
      let has_left = list.any(premises, fn(p) { propositions_equal(p, left) })
      let has_right = list.any(premises, fn(p) { propositions_equal(p, right) })
      case has_left && has_right {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Conjunction introduction: from p and q, conclude p ∧ q",
          ))
        False -> None
      }
    }
    _ -> None
  }
}

/// Check disjunction introduction: p ⊢ p ∨ q  or  q ⊢ p ∨ q
fn check_disjunction_introduction(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  case conclusion {
    Or(left, right) -> {
      let has_left = list.any(premises, fn(p) { propositions_equal(p, left) })
      let has_right = list.any(premises, fn(p) { propositions_equal(p, right) })
      case has_left || has_right {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Disjunction introduction: from p, conclude p ∨ q",
          ))
        False -> None
      }
    }
    _ -> None
  }
}

// =============================================================================
// Common Fallacy Patterns (Tier 1 Invalid Detection)
// =============================================================================

/// Check for common fallacy patterns that are definitively invalid
/// This helps increase Tier 1 coverage by catching known-invalid patterns
fn check_common_fallacy_patterns(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Check affirming the consequent: p → q, q ⊢ p
  case check_affirming_consequent(premises, conclusion) {
    Some(result) -> Some(result)
    None -> {
      // Check denying the antecedent: p → q, ¬p ⊢ ¬q
      case check_denying_antecedent(premises, conclusion) {
        Some(result) -> Some(result)
        None -> {
          // Check affirming a disjunct: p ∨ q, p ⊢ ¬q
          check_affirming_disjunct(premises, conclusion)
        }
      }
    }
  }
}

/// Check affirming the consequent fallacy: p → q, q ⊢ p
fn check_affirming_consequent(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Look for an implication p → q where p matches conclusion
  let implication_match =
    list.find(premises, fn(premise) {
      case premise {
        Implies(antecedent, _) -> propositions_equal(antecedent, conclusion)
        _ -> False
      }
    })

  case implication_match {
    Ok(Implies(_, consequent)) -> {
      // Check if the consequent (not antecedent) is a premise
      let has_consequent_only =
        list.any(premises, fn(p) { propositions_equal(p, consequent) })
        && !list.any(premises, fn(p) { propositions_equal(p, conclusion) })

      case has_consequent_only {
        True ->
          Some(HeuristicResult(
            result: Invalid(
              "Affirming the consequent: from p → q and q, cannot conclude p",
            ),
            tier: Tier1Syntactic,
            explanation: "Fallacy detected: affirming the consequent",
          ))
        False -> None
      }
    }
    _ -> None
  }
}

/// Check denying the antecedent fallacy: p → q, ¬p ⊢ ¬q
fn check_denying_antecedent(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Conclusion should be ¬q
  case conclusion {
    Not(negated_consequent) -> {
      // Look for an implication p → q where q matches negated_consequent
      let implication_match =
        list.find(premises, fn(premise) {
          case premise {
            Implies(_, consequent) ->
              propositions_equal(consequent, negated_consequent)
            _ -> False
          }
        })

      case implication_match {
        Ok(Implies(antecedent, _)) -> {
          // Check if ¬p (negation of antecedent) is a premise but not other conditions
          let has_negated_antecedent =
            list.any(premises, fn(p) {
              case p {
                Not(inner) -> propositions_equal(inner, antecedent)
                _ -> False
              }
            })

          // Make sure q is not already negated in premises (that would be modus tollens)
          let has_negated_consequent =
            list.any(premises, fn(p) {
              case p {
                Not(inner) -> propositions_equal(inner, negated_consequent)
                _ -> False
              }
            })

          case has_negated_antecedent && !has_negated_consequent {
            True ->
              Some(HeuristicResult(
                result: Invalid(
                  "Denying the antecedent: from p → q and ¬p, cannot conclude ¬q",
                ),
                tier: Tier1Syntactic,
                explanation: "Fallacy detected: denying the antecedent",
              ))
            False -> None
          }
        }
        _ -> None
      }
    }
    _ -> None
  }
}

/// Check affirming a disjunct fallacy: p ∨ q, p ⊢ ¬q
fn check_affirming_disjunct(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Conclusion should be ¬q
  case conclusion {
    Not(negated) -> {
      // Look for a disjunction containing the negated conclusion
      let disjunction_match =
        list.find(premises, fn(premise) {
          case premise {
            Or(left, right) ->
              propositions_equal(left, negated)
              || propositions_equal(right, negated)
            _ -> False
          }
        })

      case disjunction_match {
        Ok(Or(left, right)) -> {
          // Determine which disjunct is the negated conclusion
          let other_disjunct = case propositions_equal(left, negated) {
            True -> right
            False -> left
          }

          // Check if the other disjunct is a premise (affirming it)
          let has_other_disjunct =
            list.any(premises, fn(p) { propositions_equal(p, other_disjunct) })

          case has_other_disjunct {
            True ->
              Some(HeuristicResult(
                result: Invalid(
                  "Affirming a disjunct: from p ∨ q and p, cannot conclude ¬q",
                ),
                tier: Tier1Syntactic,
                explanation: "Fallacy detected: affirming a disjunct (disjunction is inclusive)",
              ))
            False -> None
          }
        }
        _ -> None
      }
    }
    _ -> None
  }
}

/// Check for modal axiom patterns that are always valid/invalid in specific systems
fn check_modal_axiom_patterns(
  formalization: Formalization,
) -> Option(HeuristicResult) {
  let conclusion = formalization.conclusion
  let premises = formalization.premises
  let system = formalization.logic_system

  // First check premise-based modal inference patterns
  case check_modal_inference_patterns(premises, conclusion) {
    Some(result) -> Some(result)
    None -> {
      // Then check conclusion-only axiom patterns
      case conclusion {
        // T axiom: □p → p (valid in T, S4, S5; invalid in K, K4, KD, KD45)
        Implies(Necessary(p), q) -> {
          case propositions_equal(p, q) {
            True -> check_t_axiom(system)
            False -> check_other_implication_patterns(conclusion, system)
          }
        }

        // 5 axiom: ◇p → □◇p (valid in S5, KD45)
        Implies(Possible(p), Necessary(Possible(q))) -> {
          case propositions_equal(p, q) {
            True -> check_5_axiom(system)
            False -> None
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
  }
}

/// Check modal inference patterns based on premises and conclusion
fn check_modal_inference_patterns(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // K distribution: □(p → q), □p ⊢ □q
  case check_k_distribution(premises, conclusion) {
    Some(result) -> Some(result)
    None -> {
      // Modal modus ponens: □(p → q), p ⊢ q (in reflexive systems)
      case check_modal_modus_ponens(premises, conclusion) {
        Some(result) -> Some(result)
        None -> {
          // Necessitation from tautology premise
          check_necessitation_from_tautology(premises, conclusion)
        }
      }
    }
  }
}

/// Check K distribution rule: □(p → q), □p ⊢ □q
fn check_k_distribution(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  case conclusion {
    Necessary(inner_conclusion) -> {
      // Look for □(p → q) where q = inner_conclusion
      let boxed_implication =
        list.find(premises, fn(premise) {
          case premise {
            Necessary(Implies(_, consequent)) ->
              propositions_equal(consequent, inner_conclusion)
            _ -> False
          }
        })

      case boxed_implication {
        Ok(Necessary(Implies(antecedent, _))) -> {
          // Look for □p where p = antecedent
          let has_boxed_antecedent =
            list.any(premises, fn(p) {
              case p {
                Necessary(inner) -> propositions_equal(inner, antecedent)
                _ -> False
              }
            })

          case has_boxed_antecedent {
            True ->
              Some(HeuristicResult(
                result: Valid,
                tier: Tier1Syntactic,
                explanation: "K distribution: from □(p → q) and □p, conclude □q",
              ))
            False -> None
          }
        }
        _ -> None
      }
    }
    _ -> None
  }
}

/// Check modal modus ponens pattern for conclusions
fn check_modal_modus_ponens(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  // Look for □(p → conclusion) and p in premises
  let boxed_implication =
    list.find(premises, fn(premise) {
      case premise {
        Necessary(Implies(_, consequent)) ->
          propositions_equal(consequent, conclusion)
        _ -> False
      }
    })

  case boxed_implication {
    Ok(Necessary(Implies(antecedent, _))) -> {
      // Check if antecedent is a premise
      let has_antecedent =
        list.any(premises, fn(p) { propositions_equal(p, antecedent) })

      case has_antecedent {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Modal modus ponens: from □(p → q) and p, conclude q",
          ))
        False -> None
      }
    }
    _ -> None
  }
}

/// Check necessitation from a tautology: if p is a tautology premise, □p is valid
fn check_necessitation_from_tautology(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Option(HeuristicResult) {
  case conclusion {
    Necessary(inner) -> {
      // Check if inner is identical to a premise that is a tautology
      let has_tautology_premise =
        list.any(premises, fn(p) {
          propositions_equal(p, inner) && is_tautology(inner)
        })

      case has_tautology_premise {
        True ->
          Some(HeuristicResult(
            result: Valid,
            tier: Tier1Syntactic,
            explanation: "Necessitation: tautologies are necessarily true",
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
    // Probabilistic operators are considered modal
    proposition.Probable(_) -> True
    proposition.ProbAtLeast(_, _) -> True
    proposition.ProbAtMost(_, _) -> True
    proposition.ProbExact(_, _) -> True
    proposition.ProbRange(_, _, _) -> True
    proposition.CondProb(_, _, _) -> True
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
    // Probabilistic operators
    proposition.Probable(p) -> collect_atoms_from_prop(p)
    proposition.ProbAtLeast(p, _) -> collect_atoms_from_prop(p)
    proposition.ProbAtMost(p, _) -> collect_atoms_from_prop(p)
    proposition.ProbExact(p, _) -> collect_atoms_from_prop(p)
    proposition.ProbRange(p, _, _) -> collect_atoms_from_prop(p)
    proposition.CondProb(cons, ante, _) ->
      set.union(collect_atoms_from_prop(cons), collect_atoms_from_prop(ante))
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
    // Probabilistic operators - should not reach here due to earlier check
    proposition.Probable(_) -> True
    proposition.ProbAtLeast(_, _) -> True
    proposition.ProbAtMost(_, _) -> True
    proposition.ProbExact(_, _) -> True
    proposition.ProbRange(_, _, _) -> True
    proposition.CondProb(_, _, _) -> True
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
