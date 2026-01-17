//// Fallacy Detection Module
////
//// This module provides automatic detection of common logical fallacies
//// with clear explanations and fix suggestions for prediction accuracy
//// assessment.
////
//// ## Purpose
//// - Detect named fallacies in reasoning
//// - Provide human-readable explanations
//// - Suggest how to fix/avoid fallacies
//// - Classify severity of detected issues
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/fallacy
////
//// let analysis = fallacy.analyze_formalization(formalization, countermodel)
//// let report = fallacy.format_analysis(analysis, DetailedLevel)
//// ```

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
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

/// Severity of a detected fallacy
pub type Severity {
  /// Invalidates entire argument
  Critical
  /// Significant flaw
  Major
  /// Weakness but not fatal
  Minor
  /// Potential issue to consider
  Warning
}

/// Types of logical fallacies
pub type FallacyType {
  // Propositional fallacies (formal)
  /// Affirming the consequent: If P then Q, Q, therefore P
  AffirmingConsequent
  /// Denying the antecedent: If P then Q, not P, therefore not Q
  DenyingAntecedent
  /// Affirming a disjunct: P or Q, P, therefore not Q
  AffirmingDisjunct

  // Modal fallacies
  /// □(p → q) confused with (□p → □q)
  ModalScopeConfusion
  /// □p confused with ◇p
  NecessityPossibilitySwap
  /// Using T-axiom in non-reflexive system
  FramePropertyViolation

  // Epistemic fallacies
  /// K confused with B
  KnowledgeBeliefConfusion
  /// Kp but ¬p
  FactiveViolation
  /// Invalid positive/negative introspection
  IntrospectionViolation

  // Structural fallacies
  /// Conclusion is assumed in premises
  CircularReasoning
  /// Conclusion doesn't follow from premises
  NonSequitur
  /// Treating different things as equivalent
  FalseEquivalence

  // Probabilistic fallacies
  /// Ignoring base rates
  BaseRateNeglect
  /// Only seeking confirming evidence
  ConfirmationBias

  // Syllogistic fallacies
  /// Middle term not distributed
  UndistributedMiddle
  /// Four different terms used
  FourTermsFallacy
}

/// A detected fallacy with full details
pub type DetectedFallacy {
  DetectedFallacy(
    /// Type of fallacy
    fallacy_type: FallacyType,
    /// Human-readable name
    name: String,
    /// Description of the fallacy
    description: String,
    /// Where in the argument it occurs
    location: FallacyLocation,
    /// Severity of the issue
    severity: Severity,
    /// Example of this fallacy
    example: String,
    /// Suggestion for fixing
    fix_suggestion: String,
    /// Involved propositions
    involved_propositions: List(Proposition),
    /// Confidence score (0.0 to 1.0)
    confidence: Float,
  )
}

/// Location of a fallacy in the argument
pub type FallacyLocation {
  /// In the premises
  InPremises(indices: List(Int))
  /// In the conclusion
  InConclusion
  /// In the relationship between premises and conclusion
  InInference
  /// Throughout the argument
  Throughout
}

/// Quality assessment of reasoning
pub type ReasoningQuality {
  ReasoningQuality(
    /// Overall score (0.0 to 1.0)
    overall_score: Float,
    /// Score breakdown by category
    category_scores: List(#(String, Float)),
    /// Quality level
    level: QualityLevel,
    /// Summary description
    summary: String,
  )
}

/// Quality levels for reasoning
pub type QualityLevel {
  /// No significant issues
  Excellent
  /// Minor issues only
  Good
  /// Some concerning issues
  Fair
  /// Major issues present
  Poor
  /// Critical flaws detected
  Flawed
}

/// Complete fallacy analysis result
pub type FallacyAnalysis {
  FallacyAnalysis(
    /// Detected fallacies
    detected_fallacies: List(DetectedFallacy),
    /// Reasoning quality assessment
    reasoning_quality: ReasoningQuality,
    /// Improvement suggestions
    suggestions: List(String),
    /// Whether the argument should be rejected
    should_reject: Bool,
  )
}

/// Configuration for fallacy analysis
pub type FallacyConfig {
  FallacyConfig(
    /// Minimum confidence to report a fallacy
    min_confidence: Float,
    /// Include minor/warning severity
    include_minor: Bool,
    /// Maximum fallacies to report
    max_fallacies: Int,
    /// Check modal-specific fallacies
    check_modal: Bool,
    /// Check epistemic-specific fallacies
    check_epistemic: Bool,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Default analysis configuration
pub fn default_config() -> FallacyConfig {
  FallacyConfig(
    min_confidence: 0.6,
    include_minor: True,
    max_fallacies: 10,
    check_modal: True,
    check_epistemic: True,
  )
}

/// Strict configuration (higher thresholds)
pub fn strict_config() -> FallacyConfig {
  FallacyConfig(
    min_confidence: 0.8,
    include_minor: False,
    max_fallacies: 5,
    check_modal: True,
    check_epistemic: True,
  )
}

/// Comprehensive configuration
pub fn comprehensive_config() -> FallacyConfig {
  FallacyConfig(
    min_confidence: 0.4,
    include_minor: True,
    max_fallacies: 20,
    check_modal: True,
    check_epistemic: True,
  )
}

// =============================================================================
// Main Analysis Functions
// =============================================================================

/// Analyze a formalization for fallacies
pub fn analyze_formalization(
  formalization: Formalization,
  config: FallacyConfig,
) -> FallacyAnalysis {
  let premises = formalization.premises
  let conclusion = formalization.conclusion
  let system = formalization.logic_system

  // Detect various fallacy types
  let propositional_fallacies =
    detect_propositional_fallacies(premises, conclusion)
  let modal_fallacies = case config.check_modal {
    True -> detect_modal_fallacies(premises, conclusion, system)
    False -> []
  }
  let epistemic_fallacies = case config.check_epistemic {
    True -> detect_epistemic_fallacies(premises, conclusion)
    False -> []
  }
  let structural_fallacies = detect_structural_fallacies(premises, conclusion)

  // Combine and filter
  let all_fallacies =
    list.flatten([
      propositional_fallacies,
      modal_fallacies,
      epistemic_fallacies,
      structural_fallacies,
    ])
    |> list.filter(fn(f: DetectedFallacy) {
      f.confidence >=. config.min_confidence
    })
    |> list.filter(fn(f: DetectedFallacy) {
      case config.include_minor {
        True -> True
        False ->
          case f.severity {
            Minor | Warning -> False
            _ -> True
          }
      }
    })
    |> list.sort(fn(a: DetectedFallacy, b: DetectedFallacy) {
      float.compare(b.confidence, a.confidence)
    })
    |> list.take(config.max_fallacies)

  // Calculate quality
  let quality = assess_reasoning_quality(all_fallacies)

  // Generate suggestions
  let suggestions = generate_suggestions(all_fallacies)

  // Determine if argument should be rejected
  let should_reject =
    list.any(all_fallacies, fn(f: DetectedFallacy) {
      case f.severity {
        Critical -> True
        _ -> False
      }
    })

  FallacyAnalysis(
    detected_fallacies: all_fallacies,
    reasoning_quality: quality,
    suggestions: suggestions,
    should_reject: should_reject,
  )
}

/// Analyze with validation result for additional context
pub fn analyze_with_result(
  formalization: Formalization,
  result: ValidationResult,
  config: FallacyConfig,
) -> FallacyAnalysis {
  let base_analysis = analyze_formalization(formalization, config)

  // If invalid, check for common invalid patterns
  case result {
    Invalid(countermodel) -> {
      let additional = analyze_countermodel_hints(countermodel, formalization)
      FallacyAnalysis(
        ..base_analysis,
        detected_fallacies: list.append(
            base_analysis.detected_fallacies,
            additional,
          )
          |> list.unique()
          |> list.take(config.max_fallacies),
      )
    }
    _ -> base_analysis
  }
}

// =============================================================================
// Propositional Fallacy Detection
// =============================================================================

/// Detect propositional logic fallacies
fn detect_propositional_fallacies(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  list.flatten([
    detect_affirming_consequent(premises, conclusion),
    detect_denying_antecedent(premises, conclusion),
    detect_affirming_disjunct(premises, conclusion),
  ])
}

/// Detect affirming the consequent: p → q, q ⊢ p
fn detect_affirming_consequent(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  // Look for pattern: Implies(a, b) in premises, b in premises, conclusion = a
  let implications =
    list.filter_map(premises, fn(p) {
      case p {
        Implies(ant, cons) -> Ok(#(ant, cons))
        _ -> Error(Nil)
      }
    })

  list.filter_map(implications, fn(impl) {
    let #(antecedent, consequent) = impl
    // Check if consequent appears as premise and conclusion is antecedent
    let consequent_in_premises =
      list.any(premises, fn(p) { propositions_match(p, consequent) })
    let conclusion_is_antecedent = propositions_match(conclusion, antecedent)

    case consequent_in_premises && conclusion_is_antecedent {
      True ->
        Ok(DetectedFallacy(
          fallacy_type: AffirmingConsequent,
          name: "Affirming the Consequent",
          description: "Incorrectly concluding the antecedent from the consequent of a conditional",
          location: InInference,
          severity: Critical,
          example: "If it rains, the ground is wet. The ground is wet. Therefore, it rained.",
          fix_suggestion: "The consequent being true doesn't guarantee the antecedent. Consider other possible causes.",
          involved_propositions: [
            Implies(antecedent, consequent),
            consequent,
            conclusion,
          ],
          confidence: 0.95,
        ))
      False -> Error(Nil)
    }
  })
}

/// Detect denying the antecedent: p → q, ¬p ⊢ ¬q
fn detect_denying_antecedent(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  let implications =
    list.filter_map(premises, fn(p) {
      case p {
        Implies(ant, cons) -> Ok(#(ant, cons))
        _ -> Error(Nil)
      }
    })

  list.filter_map(implications, fn(impl) {
    let #(antecedent, consequent) = impl
    // Check for ¬antecedent in premises and conclusion = ¬consequent
    let negated_ant_in_premises =
      list.any(premises, fn(p) {
        case p {
          Not(inner) -> propositions_match(inner, antecedent)
          _ -> False
        }
      })

    let conclusion_is_negated_cons = case conclusion {
      Not(inner) -> propositions_match(inner, consequent)
      _ -> False
    }

    case negated_ant_in_premises && conclusion_is_negated_cons {
      True ->
        Ok(DetectedFallacy(
          fallacy_type: DenyingAntecedent,
          name: "Denying the Antecedent",
          description: "Incorrectly concluding the negation of the consequent from the negation of the antecedent",
          location: InInference,
          severity: Critical,
          example: "If you study, you'll pass. You didn't study. Therefore, you won't pass.",
          fix_suggestion: "The antecedent being false doesn't determine the consequent. Other conditions might make it true.",
          involved_propositions: [
            Implies(antecedent, consequent),
            Not(antecedent),
            conclusion,
          ],
          confidence: 0.95,
        ))
      False -> Error(Nil)
    }
  })
}

/// Detect affirming a disjunct: p ∨ q, p ⊢ ¬q
fn detect_affirming_disjunct(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  let disjunctions =
    list.filter_map(premises, fn(p) {
      case p {
        Or(left, right) -> Ok(#(left, right))
        _ -> Error(Nil)
      }
    })

  list.filter_map(disjunctions, fn(disj) {
    let #(left, right) = disj
    // Check for left in premises and conclusion = ¬right (or vice versa)
    let left_in_premises =
      list.any(premises, fn(p) { propositions_match(p, left) })

    let conclusion_negates_right = case conclusion {
      Not(inner) -> propositions_match(inner, right)
      _ -> False
    }

    case left_in_premises && conclusion_negates_right {
      True ->
        Ok(DetectedFallacy(
          fallacy_type: AffirmingDisjunct,
          name: "Affirming a Disjunct",
          description: "Incorrectly concluding that one disjunct being true means the other must be false",
          location: InInference,
          severity: Major,
          example: "Either it's raining or the sprinklers are on. It's raining. Therefore, the sprinklers aren't on.",
          fix_suggestion: "In inclusive disjunction, both disjuncts can be true. Consider whether exclusive 'or' was intended.",
          involved_propositions: [Or(left, right), left, conclusion],
          confidence: 0.85,
        ))
      False -> Error(Nil)
    }
  })
}

// =============================================================================
// Modal Fallacy Detection
// =============================================================================

/// Detect modal logic fallacies
fn detect_modal_fallacies(
  premises: List(Proposition),
  conclusion: Proposition,
  system: LogicSystem,
) -> List(DetectedFallacy) {
  list.flatten([
    detect_modal_scope_confusion(premises, conclusion, system),
    detect_necessity_possibility_swap(premises, conclusion),
    detect_frame_property_violation(premises, conclusion, system),
  ])
}

/// Detect modal scope confusion: □(p → q) confused with □p → □q
fn detect_modal_scope_confusion(
  premises: List(Proposition),
  conclusion: Proposition,
  system: LogicSystem,
) -> List(DetectedFallacy) {
  // In system K, □(p → q) does NOT entail □p → □q
  // Look for Necessary(Implies(a, b)) in premises with Implies(Necessary(a), Necessary(b)) as conclusion
  let boxed_implications =
    list.filter_map(premises, fn(p) {
      case p {
        Necessary(Implies(a, b)) -> Ok(#(a, b))
        _ -> Error(Nil)
      }
    })

  case conclusion {
    Implies(Necessary(a), Necessary(b)) -> {
      let is_scope_confusion =
        list.any(boxed_implications, fn(pair) {
          let #(pa, pb) = pair
          propositions_match(pa, a) && propositions_match(pb, b)
        })

      case is_scope_confusion && system == K {
        True -> [
          DetectedFallacy(
            fallacy_type: ModalScopeConfusion,
            name: "Modal Scope Confusion",
            description: "Confusing □(p → q) with (□p → □q) - the distribution axiom only holds in certain systems",
            location: InInference,
            severity: Major,
            example: "Necessarily, if it rains then streets are wet. Therefore, if necessarily it rains, then necessarily streets are wet.",
            fix_suggestion: "In basic modal logic K, necessity doesn't distribute over implication this way. Consider if a stronger system (T, S4, S5) is needed.",
            involved_propositions: [Necessary(Implies(a, b)), conclusion],
            confidence: 0.9,
          ),
        ]
        False -> []
      }
    }
    _ -> []
  }
}

/// Detect necessity-possibility swap: ◇p confused with □p
fn detect_necessity_possibility_swap(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  // Check if concluding necessity from possibility
  let possibilities =
    list.filter_map(premises, fn(p) {
      case p {
        Possible(inner) -> Ok(inner)
        _ -> Error(Nil)
      }
    })

  case conclusion {
    Necessary(inner) -> {
      let swap_detected =
        list.any(possibilities, fn(poss_inner) {
          propositions_match(poss_inner, inner)
        })

      case swap_detected {
        True -> [
          DetectedFallacy(
            fallacy_type: NecessityPossibilitySwap,
            name: "Necessity-Possibility Swap",
            description: "Incorrectly treating possibility (◇) as necessity (□)",
            location: InInference,
            severity: Critical,
            example: "It's possible that it will rain. Therefore, it's necessary that it will rain.",
            fix_suggestion: "Possibility only means something can be true in some world; necessity requires truth in all worlds.",
            involved_propositions: [Possible(inner), conclusion],
            confidence: 0.95,
          ),
        ]
        False -> []
      }
    }
    _ -> []
  }
}

/// Detect frame property violations (e.g., using T-axiom in non-reflexive system)
fn detect_frame_property_violation(
  premises: List(Proposition),
  conclusion: Proposition,
  system: LogicSystem,
) -> List(DetectedFallacy) {
  // T-axiom: □p → p requires reflexivity (system T or stronger)
  // Check if trying to derive p from □p in K
  let necessities =
    list.filter_map(premises, fn(p) {
      case p {
        Necessary(inner) -> Ok(inner)
        _ -> Error(Nil)
      }
    })

  let t_axiom_violation =
    system == K
    && list.any(necessities, fn(nec_inner) {
      propositions_match(nec_inner, conclusion)
    })

  case t_axiom_violation {
    True -> [
      DetectedFallacy(
        fallacy_type: FramePropertyViolation,
        name: "Frame Property Violation",
        description: "Using the T-axiom (□p → p) in a non-reflexive system like K",
        location: InInference,
        severity: Major,
        example: "In system K: Necessarily p. Therefore, p. (Invalid - K lacks reflexivity)",
        fix_suggestion: "The T-axiom requires reflexivity. Use system T, S4, or S5 instead of K.",
        involved_propositions: [Necessary(conclusion), conclusion],
        confidence: 0.85,
      ),
    ]
    False -> []
  }
}

// =============================================================================
// Epistemic Fallacy Detection
// =============================================================================

/// Detect epistemic logic fallacies
fn detect_epistemic_fallacies(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  list.flatten([
    detect_knowledge_belief_confusion(premises, conclusion),
    detect_factive_violation(premises, conclusion),
  ])
}

/// Detect confusion between knowledge and belief
fn detect_knowledge_belief_confusion(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  // Check if deriving Knows from Believes
  let beliefs =
    list.filter_map(premises, fn(p) {
      case p {
        Believes(agent, inner) -> Ok(#(agent, inner))
        _ -> Error(Nil)
      }
    })

  case conclusion {
    Knows(agent, inner) -> {
      let confusion_detected =
        list.any(beliefs, fn(b) {
          let #(b_agent, b_inner) = b
          b_agent == agent && propositions_match(b_inner, inner)
        })

      case confusion_detected {
        True -> [
          DetectedFallacy(
            fallacy_type: KnowledgeBeliefConfusion,
            name: "Knowledge-Belief Confusion",
            description: "Incorrectly treating belief as knowledge - knowledge requires truth, belief does not",
            location: InInference,
            severity: Major,
            example: "Alice believes it's raining. Therefore, Alice knows it's raining.",
            fix_suggestion: "Knowledge is factive (Kp implies p), but belief is not. Additional evidence of truth is needed.",
            involved_propositions: [Believes(agent, inner), conclusion],
            confidence: 0.9,
          ),
        ]
        False -> []
      }
    }
    _ -> []
  }
}

/// Detect factive violation (knowing something false)
fn detect_factive_violation(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  // Check for Knows(a, p) in premises and ¬p in premises or conclusion
  let knowledge_claims =
    list.filter_map(premises, fn(p) {
      case p {
        Knows(agent, inner) -> Ok(#(agent, inner))
        _ -> Error(Nil)
      }
    })

  list.filter_map(knowledge_claims, fn(k) {
    let #(agent, known_prop) = k
    // Check if negation of known prop appears
    let negation_in_premises =
      list.any(premises, fn(p) {
        case p {
          Not(inner) -> propositions_match(inner, known_prop)
          _ -> False
        }
      })

    let negation_is_conclusion = case conclusion {
      Not(inner) -> propositions_match(inner, known_prop)
      _ -> False
    }

    case negation_in_premises || negation_is_conclusion {
      True ->
        Ok(DetectedFallacy(
          fallacy_type: FactiveViolation,
          name: "Factive Violation",
          description: "Claiming knowledge of something that is false - knowledge requires truth",
          location: Throughout,
          severity: Critical,
          example: "Alice knows that p. But ¬p is also asserted.",
          fix_suggestion: "Knowledge is factive: one cannot know something false. Revise to use 'believes' or correct the factual claim.",
          involved_propositions: [Knows(agent, known_prop), Not(known_prop)],
          confidence: 0.95,
        ))
      False -> Error(Nil)
    }
  })
}

// =============================================================================
// Structural Fallacy Detection
// =============================================================================

/// Detect structural fallacies
fn detect_structural_fallacies(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  list.flatten([
    detect_circular_reasoning(premises, conclusion),
    detect_non_sequitur(premises, conclusion),
  ])
}

/// Detect circular reasoning (conclusion appears in premises)
fn detect_circular_reasoning(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  let conclusion_in_premises =
    list.any(premises, fn(p) {
      propositions_match(p, conclusion)
      || propositions_very_similar(p, conclusion)
    })

  case conclusion_in_premises {
    True -> [
      DetectedFallacy(
        fallacy_type: CircularReasoning,
        name: "Circular Reasoning",
        description: "The conclusion appears in (or is equivalent to) one of the premises",
        location: Throughout,
        severity: Critical,
        example: "A is true because B is true. B is true because A is true.",
        fix_suggestion: "Provide independent evidence for the conclusion that doesn't assume it.",
        involved_propositions: [conclusion],
        confidence: 0.9,
      ),
    ]
    False -> []
  }
}

/// Detect non-sequitur (conclusion doesn't follow)
fn detect_non_sequitur(
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(DetectedFallacy) {
  // Check if conclusion shares no atoms with premises
  let premise_atoms =
    premises
    |> list.flat_map(extract_atoms)
    |> list.unique()

  let conclusion_atoms =
    conclusion
    |> extract_atoms
    |> list.unique()

  let shared_atoms =
    list.filter(conclusion_atoms, fn(a) { list.contains(premise_atoms, a) })

  case list.is_empty(shared_atoms) && !list.is_empty(conclusion_atoms) {
    True -> [
      DetectedFallacy(
        fallacy_type: NonSequitur,
        name: "Non Sequitur",
        description: "The conclusion appears unconnected to the premises - no shared terms",
        location: InInference,
        severity: Major,
        example: "The sky is blue. Therefore, pizza is delicious.",
        fix_suggestion: "Ensure the conclusion follows logically from the premises with clear inferential steps.",
        involved_propositions: list.append(premises, [conclusion]),
        confidence: 0.7,
      ),
    ]
    False -> []
  }
}

// =============================================================================
// Countermodel Analysis
// =============================================================================

/// Extract additional fallacy hints from countermodel
fn analyze_countermodel_hints(
  countermodel: String,
  formalization: Formalization,
) -> List(DetectedFallacy) {
  // Analyze countermodel string for common patterns
  let has_multiple_worlds =
    string.contains(countermodel, "w1") || string.contains(countermodel, "w2")
  let has_accessibility = string.contains(countermodel, "Access")

  case has_multiple_worlds && has_accessibility {
    True ->
      // Modal countermodel suggests modal reasoning issue
      case formalization.logic_system {
        K -> [
          DetectedFallacy(
            fallacy_type: FramePropertyViolation,
            name: "Modal Frame Issue",
            description: "Countermodel shows the argument fails due to modal accessibility constraints",
            location: InInference,
            severity: Major,
            example: "The argument assumes frame properties not available in the chosen logic system",
            fix_suggestion: "Consider using a stronger modal system with appropriate frame properties",
            involved_propositions: [],
            confidence: 0.7,
          ),
        ]
        _ -> []
      }
    False -> []
  }
}

// =============================================================================
// Quality Assessment
// =============================================================================

/// Assess overall reasoning quality
fn assess_reasoning_quality(
  fallacies: List(DetectedFallacy),
) -> ReasoningQuality {
  let critical_count = list.count(fallacies, fn(f) { f.severity == Critical })
  let major_count = list.count(fallacies, fn(f) { f.severity == Major })
  let minor_count = list.count(fallacies, fn(f) { f.severity == Minor })
  let warning_count = list.count(fallacies, fn(f) { f.severity == Warning })

  // Calculate score (1.0 = perfect, 0.0 = terrible)
  let penalty =
    int.to_float(critical_count)
    *. 0.4
    +. int.to_float(major_count)
    *. 0.2
    +. int.to_float(minor_count)
    *. 0.05
    +. int.to_float(warning_count)
    *. 0.02

  let score = float.max(0.0, 1.0 -. penalty)

  let level = case score {
    s if s >=. 0.9 -> Excellent
    s if s >=. 0.7 -> Good
    s if s >=. 0.5 -> Fair
    s if s >=. 0.3 -> Poor
    _ -> Flawed
  }

  let summary = case level {
    Excellent -> "Reasoning is sound with no significant issues detected"
    Good -> "Reasoning is generally sound with minor concerns"
    Fair -> "Reasoning has some issues that should be addressed"
    Poor -> "Reasoning has significant problems"
    Flawed -> "Reasoning contains critical logical errors"
  }

  ReasoningQuality(
    overall_score: score,
    category_scores: [
      #("Propositional Logic", case critical_count {
        0 -> 1.0
        _ -> 0.3
      }),
      #("Modal Logic", case major_count {
        0 -> 1.0
        _ -> 0.5
      }),
      #("Structure", case list.length(fallacies) {
        0 -> 1.0
        n if n <= 2 -> 0.7
        _ -> 0.4
      }),
    ],
    level: level,
    summary: summary,
  )
}

/// Generate improvement suggestions
fn generate_suggestions(fallacies: List(DetectedFallacy)) -> List(String) {
  fallacies
  |> list.map(fn(f) { f.fix_suggestion })
  |> list.unique()
  |> list.take(5)
}

// =============================================================================
// Formatting
// =============================================================================

/// Format a complete fallacy analysis
pub fn format_analysis(analysis: FallacyAnalysis, detailed: Bool) -> String {
  let fallacy_count = list.length(analysis.detected_fallacies)

  let header = case fallacy_count {
    0 -> "No logical fallacies detected."
    1 -> "1 logical fallacy detected."
    n -> int.to_string(n) <> " logical fallacies detected."
  }

  let quality_line =
    "Reasoning Quality: "
    <> quality_level_to_string(analysis.reasoning_quality.level)
    <> " ("
    <> format_percentage(analysis.reasoning_quality.overall_score)
    <> ")"

  let fallacy_lines = case detailed {
    True ->
      list.map(analysis.detected_fallacies, format_fallacy_detailed)
      |> string.join("\n\n")
    False ->
      list.map(analysis.detected_fallacies, format_fallacy_brief)
      |> string.join("\n")
  }

  let suggestion_lines = case list.is_empty(analysis.suggestions) {
    True -> ""
    False ->
      "\nSuggestions:\n"
      <> {
        analysis.suggestions
        |> list.map(fn(s) { "• " <> s })
        |> string.join("\n")
      }
  }

  string.join([header, quality_line, fallacy_lines, suggestion_lines], "\n\n")
}

/// Format a single fallacy briefly
fn format_fallacy_brief(fallacy: DetectedFallacy) -> String {
  let severity_marker = case fallacy.severity {
    Critical -> "[CRITICAL]"
    Major -> "[MAJOR]"
    Minor -> "[minor]"
    Warning -> "[warning]"
  }

  severity_marker
  <> " "
  <> fallacy.name
  <> " (confidence: "
  <> format_percentage(fallacy.confidence)
  <> ")"
}

/// Format a single fallacy in detail
fn format_fallacy_detailed(fallacy: DetectedFallacy) -> String {
  let severity_marker = case fallacy.severity {
    Critical -> "🔴 CRITICAL"
    Major -> "🟠 MAJOR"
    Minor -> "🟡 Minor"
    Warning -> "⚪ Warning"
  }

  string.join(
    [
      severity_marker <> ": " <> fallacy.name,
      "  Description: " <> fallacy.description,
      "  Example: " <> fallacy.example,
      "  Fix: " <> fallacy.fix_suggestion,
      "  Confidence: " <> format_percentage(fallacy.confidence),
    ],
    "\n",
  )
}

/// Convert quality level to string
fn quality_level_to_string(level: QualityLevel) -> String {
  case level {
    Excellent -> "Excellent"
    Good -> "Good"
    Fair -> "Fair"
    Poor -> "Poor"
    Flawed -> "Flawed"
  }
}

/// Format a float as percentage
fn format_percentage(value: Float) -> String {
  let pct = value *. 100.0
  let rounded = float.round(pct)
  int.to_string(rounded) <> "%"
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Check if two propositions match structurally
fn propositions_match(p1: Proposition, p2: Proposition) -> Bool {
  case p1, p2 {
    Atom(a), Atom(b) -> a == b
    Not(a), Not(b) -> propositions_match(a, b)
    And(a1, a2), And(b1, b2) ->
      propositions_match(a1, b1) && propositions_match(a2, b2)
    Or(a1, a2), Or(b1, b2) ->
      propositions_match(a1, b1) && propositions_match(a2, b2)
    Implies(a1, a2), Implies(b1, b2) ->
      propositions_match(a1, b1) && propositions_match(a2, b2)
    Necessary(a), Necessary(b) -> propositions_match(a, b)
    Possible(a), Possible(b) -> propositions_match(a, b)
    Knows(ag1, a), Knows(ag2, b) -> ag1 == ag2 && propositions_match(a, b)
    Believes(ag1, a), Believes(ag2, b) -> ag1 == ag2 && propositions_match(a, b)
    Obligatory(a), Obligatory(b) -> propositions_match(a, b)
    Permitted(a), Permitted(b) -> propositions_match(a, b)
    _, _ -> False
  }
}

/// Check if propositions are very similar (for circular reasoning)
fn propositions_very_similar(p1: Proposition, p2: Proposition) -> Bool {
  // Check for common patterns of disguised identity
  case p1, p2 {
    // Double negation
    Not(Not(a)), b -> propositions_match(a, b)
    a, Not(Not(b)) -> propositions_match(a, b)
    // Implication with true antecedent
    Implies(Atom("true"), a), b -> propositions_match(a, b)
    a, Implies(Atom("true"), b) -> propositions_match(a, b)
    // Direct match (default case)
    a, b -> propositions_match(a, b)
  }
}

/// Extract all atoms from a proposition
fn extract_atoms(prop: Proposition) -> List(String) {
  case prop {
    Atom(name) -> [name]
    Not(inner) -> extract_atoms(inner)
    And(left, right) -> list.append(extract_atoms(left), extract_atoms(right))
    Or(left, right) -> list.append(extract_atoms(left), extract_atoms(right))
    Implies(left, right) ->
      list.append(extract_atoms(left), extract_atoms(right))
    Necessary(inner) -> extract_atoms(inner)
    Possible(inner) -> extract_atoms(inner)
    Knows(_, inner) -> extract_atoms(inner)
    Believes(_, inner) -> extract_atoms(inner)
    Obligatory(inner) -> extract_atoms(inner)
    Permitted(inner) -> extract_atoms(inner)
    // Probabilistic operators
    proposition.Probable(inner) -> extract_atoms(inner)
    proposition.ProbAtLeast(inner, _) -> extract_atoms(inner)
    proposition.ProbAtMost(inner, _) -> extract_atoms(inner)
    proposition.ProbExact(inner, _) -> extract_atoms(inner)
    proposition.ProbRange(inner, _, _) -> extract_atoms(inner)
    proposition.CondProb(cons, ante, _) ->
      list.append(extract_atoms(cons), extract_atoms(ante))
  }
}

// =============================================================================
// Convenience Functions for Common Patterns
// =============================================================================

/// Check if argument exhibits affirming the consequent pattern
pub fn is_affirming_consequent(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Bool {
  !list.is_empty(detect_affirming_consequent(premises, conclusion))
}

/// Check if argument exhibits denying the antecedent pattern
pub fn is_denying_antecedent(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Bool {
  !list.is_empty(detect_denying_antecedent(premises, conclusion))
}

/// Check if argument has any modal fallacies
pub fn has_modal_fallacy(
  premises: List(Proposition),
  conclusion: Proposition,
  system: LogicSystem,
) -> Bool {
  !list.is_empty(detect_modal_fallacies(premises, conclusion, system))
}

/// Check if argument has any epistemic fallacies
pub fn has_epistemic_fallacy(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Bool {
  !list.is_empty(detect_epistemic_fallacies(premises, conclusion))
}

/// Get severity as string
pub fn severity_to_string(severity: Severity) -> String {
  case severity {
    Critical -> "Critical"
    Major -> "Major"
    Minor -> "Minor"
    Warning -> "Warning"
  }
}

/// Get fallacy type as string
pub fn fallacy_type_to_string(fallacy_type: FallacyType) -> String {
  case fallacy_type {
    AffirmingConsequent -> "Affirming the Consequent"
    DenyingAntecedent -> "Denying the Antecedent"
    AffirmingDisjunct -> "Affirming a Disjunct"
    ModalScopeConfusion -> "Modal Scope Confusion"
    NecessityPossibilitySwap -> "Necessity-Possibility Swap"
    FramePropertyViolation -> "Frame Property Violation"
    KnowledgeBeliefConfusion -> "Knowledge-Belief Confusion"
    FactiveViolation -> "Factive Violation"
    IntrospectionViolation -> "Introspection Violation"
    CircularReasoning -> "Circular Reasoning"
    NonSequitur -> "Non Sequitur"
    FalseEquivalence -> "False Equivalence"
    BaseRateNeglect -> "Base Rate Neglect"
    ConfirmationBias -> "Confirmation Bias"
    UndistributedMiddle -> "Undistributed Middle"
    FourTermsFallacy -> "Four Terms Fallacy"
  }
}
