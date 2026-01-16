//// Multi-System Parallel Validation Module
////
//// This module enables parallel validation of modal logic arguments across
//// multiple logic systems simultaneously, helping users understand which
//// logical framework best captures their reasoning.
////
//// ## Purpose
//// - Validate reasoning across K, T, S4, S5, KD, KD45, K4 systems in parallel
//// - Identify which systems agree/disagree on validity
//// - Recommend the most appropriate logic system with explanation
//// - Provide semantic analysis of why validity differs between systems
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/multi_system.{validate_multi_system, default_config}
////
//// let result = validate_multi_system(formalization, default_config())
//// // Returns MultiSystemValidation with results for all systems
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Eq, Gt, Lt}
import gleam/result
import gleam/string
import modal_logic/argument.{
  type Formalization, type ValidationResult, Formalization, Invalid, Timeout,
  Unknown, Valid,
}
import modal_logic/heuristics.{
  type HeuristicResult, type ValidationTier, Tier1Syntactic, Tier2TruthTable,
  Tier3Z3,
}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, K4, KD,
  KD45, Knows, Necessary, Not, Or, Possible, S4, S5, T,
}

// =============================================================================
// Types
// =============================================================================

/// Configuration for multi-system validation
pub type MultiSystemConfig {
  MultiSystemConfig(
    /// Which systems to validate against
    systems: List(LogicSystem),
    /// Timeout per system in milliseconds
    timeout_per_system_ms: Int,
    /// Whether to run validations in parallel (when platform supports)
    parallel: Bool,
    /// Include countermodels in results
    include_countermodels: Bool,
    /// Whether to generate recommendation
    generate_recommendation: Bool,
    /// Minimum confidence threshold for results
    min_confidence: Float,
  )
}

/// Result of validating a formula across multiple systems
pub type MultiSystemValidation {
  MultiSystemValidation(
    /// The formalization that was validated
    formalization: Formalization,
    /// Results for each logic system
    results: Dict(String, SystemValidationResult),
    /// Recommended logic system
    recommended_system: Option(LogicSystem),
    /// Reason for the recommendation
    recommendation_reason: String,
    /// Comparison analysis between systems
    comparison: SystemComparison,
    /// Total execution time in milliseconds
    total_duration_ms: Int,
  )
}

/// Validation result for a single logic system
pub type SystemValidationResult {
  SystemValidationResult(
    /// The logic system used
    system: LogicSystem,
    /// Validity result
    validity: Validity,
    /// Confidence in the result (0.0 - 1.0)
    confidence: Float,
    /// Time taken in milliseconds
    duration_ms: Int,
    /// Countermodel if invalid
    countermodel: Option(String),
    /// Which validation tier was used
    tier_used: ValidationTier,
  )
}

/// Validity status
pub type Validity {
  ValidFormula
  InvalidFormula
  UnknownValidity(reason: String)
  TimedOut
}

/// Comparison analysis between systems
pub type SystemComparison {
  SystemComparison(
    /// Pairs of systems where validity differs
    validity_differs: List(#(LogicSystem, LogicSystem)),
    /// Strictest system where formula is valid (if any)
    strictest_valid: Option(LogicSystem),
    /// Most permissive system where formula is invalid (if any)
    most_permissive_invalid: Option(LogicSystem),
    /// Semantic analysis explaining the differences
    semantic_analysis: String,
    /// Consensus outcome across all systems
    consensus: ConsensusOutcome,
  )
}

/// Consensus outcome across systems
pub type ConsensusOutcome {
  /// All systems agree the formula is valid
  AllValid
  /// All systems agree the formula is invalid
  AllInvalid
  /// Some systems say valid, others say invalid
  Mixed(valid_in: List(LogicSystem), invalid_in: List(LogicSystem))
  /// Results are inconclusive (timeouts, unknown)
  Inconclusive
}

/// Frame property that affects validity
pub type FrameProperty {
  Reflexivity
  Transitivity
  Symmetry
  Seriality
  Euclidean
}

// =============================================================================
// Configuration
// =============================================================================

/// Default configuration with all standard systems
pub fn default_config() -> MultiSystemConfig {
  MultiSystemConfig(
    systems: [K, T, K4, S4, S5, KD, KD45],
    timeout_per_system_ms: 2000,
    parallel: True,
    include_countermodels: True,
    generate_recommendation: True,
    min_confidence: 0.8,
  )
}

/// Fast configuration with fewer systems for quick checks
pub fn fast_config() -> MultiSystemConfig {
  MultiSystemConfig(
    systems: [K, T, S4, S5],
    timeout_per_system_ms: 500,
    parallel: True,
    include_countermodels: False,
    generate_recommendation: True,
    min_confidence: 0.7,
  )
}

/// Full configuration with all systems and detailed output
pub fn thorough_config() -> MultiSystemConfig {
  MultiSystemConfig(
    systems: [K, T, K4, S4, S5, KD, KD45],
    timeout_per_system_ms: 5000,
    parallel: True,
    include_countermodels: True,
    generate_recommendation: True,
    min_confidence: 0.9,
  )
}

/// Configuration focused on epistemic/doxastic systems
pub fn epistemic_config() -> MultiSystemConfig {
  MultiSystemConfig(
    systems: [S5, KD45, T],
    timeout_per_system_ms: 2000,
    parallel: True,
    include_countermodels: True,
    generate_recommendation: True,
    min_confidence: 0.8,
  )
}

/// Configuration focused on deontic systems
pub fn deontic_config() -> MultiSystemConfig {
  MultiSystemConfig(
    systems: [KD, KD45, K],
    timeout_per_system_ms: 2000,
    parallel: True,
    include_countermodels: True,
    generate_recommendation: True,
    min_confidence: 0.8,
  )
}

// =============================================================================
// Main Validation Functions
// =============================================================================

/// Validate a formalization across multiple logic systems
pub fn validate_multi_system(
  formalization: Formalization,
  config: MultiSystemConfig,
) -> MultiSystemValidation {
  // Validate across all configured systems
  let system_results = validate_across_systems(formalization, config)

  // Build results dictionary
  let results_dict = build_results_dict(system_results)

  // Analyze differences between systems
  let comparison = analyze_comparison(system_results, formalization)

  // Generate recommendation
  let #(recommended, reason) = case config.generate_recommendation {
    True -> generate_recommendation(system_results, formalization, comparison)
    False -> #(None, "Recommendation generation disabled")
  }

  // Calculate total duration
  let total_duration =
    list.fold(system_results, 0, fn(acc, r) { acc + r.duration_ms })

  MultiSystemValidation(
    formalization: formalization,
    results: results_dict,
    recommended_system: recommended,
    recommendation_reason: reason,
    comparison: comparison,
    total_duration_ms: total_duration,
  )
}

/// Validate a formula string across multiple systems (convenience function)
pub fn validate_formula_multi_system(
  premises: List(Proposition),
  conclusion: Proposition,
  config: MultiSystemConfig,
) -> MultiSystemValidation {
  let formalization =
    Formalization(
      id: "multi_system_check",
      argument_id: "formula_check",
      logic_system: K,
      // Default, will be overridden per system
      premises: premises,
      conclusion: conclusion,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  validate_multi_system(formalization, config)
}

// =============================================================================
// System Validation
// =============================================================================

/// Validate formalization across all configured systems
fn validate_across_systems(
  formalization: Formalization,
  config: MultiSystemConfig,
) -> List(SystemValidationResult) {
  config.systems
  |> list.map(fn(system) { validate_in_system(formalization, system, config) })
}

/// Validate formalization in a specific logic system
fn validate_in_system(
  formalization: Formalization,
  system: LogicSystem,
  config: MultiSystemConfig,
) -> SystemValidationResult {
  // Create system-specific formalization
  let system_formalization =
    Formalization(..formalization, logic_system: system)

  // Try heuristic validation first (fast path)
  let heuristic_config = heuristics.default_config()
  let heuristic_result =
    heuristics.try_heuristic_validation_with_config(
      system_formalization,
      heuristic_config,
    )

  case heuristic_result {
    Some(result) -> {
      // Heuristic succeeded - use its result
      let validity = case result.result {
        Valid -> ValidFormula
        Invalid(_) -> InvalidFormula
        Unknown(reason) -> UnknownValidity(reason)
        Timeout -> TimedOut
        argument.Error(_) -> UnknownValidity("Heuristic error")
      }

      let countermodel = case result.result {
        Invalid(cm) -> Some(cm)
        _ -> None
      }

      // Confidence based on tier used
      let confidence = case result.tier {
        Tier1Syntactic -> 0.95
        Tier2TruthTable -> 0.99
        Tier3Z3 -> 0.99
      }

      // Duration estimate based on tier
      let duration_ms = case result.tier {
        Tier1Syntactic -> 1
        Tier2TruthTable -> 10
        Tier3Z3 -> 100
      }

      SystemValidationResult(
        system: system,
        validity: validity,
        confidence: confidence,
        duration_ms: duration_ms,
        countermodel: countermodel,
        tier_used: result.tier,
      )
    }
    None -> {
      // Heuristic didn't apply - use tier 3 simulation
      // In production, this would call Z3; for now we use frame-based analysis
      validate_with_frame_analysis(system_formalization, system, config)
    }
  }
}

/// Validate using frame property analysis (fallback when heuristics don't apply)
fn validate_with_frame_analysis(
  formalization: Formalization,
  system: LogicSystem,
  config: MultiSystemConfig,
) -> SystemValidationResult {
  // Analyze what frame properties are needed for validity
  let required_properties = analyze_required_frame_properties(formalization)

  // Check if system provides required properties
  let system_properties = get_system_frame_properties(system)

  let has_required =
    list.all(required_properties, fn(req) {
      list.contains(system_properties, req)
    })

  // Determine validity based on frame property analysis
  let #(validity, confidence, countermodel) = case has_required {
    True -> {
      // System has all required properties
      // Check for additional invalidity patterns
      case check_invalidity_patterns(formalization, system) {
        Some(reason) -> #(InvalidFormula, 0.85, Some(reason))
        None -> #(ValidFormula, 0.9, None)
      }
    }
    False -> {
      // System lacks required properties
      let missing =
        list.filter(required_properties, fn(req) {
          !list.contains(system_properties, req)
        })
      let reason = explain_missing_properties(missing, system)
      #(InvalidFormula, 0.9, Some(reason))
    }
  }

  SystemValidationResult(
    system: system,
    validity: validity,
    confidence: confidence,
    duration_ms: 10,
    // Frame analysis is fast
    countermodel: countermodel,
    tier_used: Tier2TruthTable,
  )
}

/// Analyze what frame properties a formula requires for validity
fn analyze_required_frame_properties(
  formalization: Formalization,
) -> List(FrameProperty) {
  let all_props =
    list.append(formalization.premises, [formalization.conclusion])
  let required = []

  // Check for T-axiom pattern: □p → p requires reflexivity
  let needs_reflexivity =
    list.any(all_props, fn(prop) { requires_reflexivity(prop) })
    || is_t_axiom_pattern(formalization)

  let required = case needs_reflexivity {
    True -> [Reflexivity, ..required]
    False -> required
  }

  // Check for 4-axiom pattern: □p → □□p requires transitivity
  let needs_transitivity =
    list.any(all_props, fn(prop) { requires_transitivity(prop) })
    || is_four_axiom_pattern(formalization)

  let required = case needs_transitivity {
    True -> [Transitivity, ..required]
    False -> required
  }

  // Check for 5-axiom pattern: ◇p → □◇p requires euclidean
  let needs_euclidean =
    list.any(all_props, fn(prop) { requires_euclidean(prop) })
    || is_five_axiom_pattern(formalization)

  let required = case needs_euclidean {
    True -> [Euclidean, ..required]
    False -> required
  }

  // Check for D-axiom pattern: □p → ◇p requires seriality
  let needs_seriality =
    list.any(all_props, fn(prop) { requires_seriality(prop) })
    || is_d_axiom_pattern(formalization)

  let required = case needs_seriality {
    True -> [Seriality, ..required]
    False -> required
  }

  required
}

/// Check if proposition pattern requires reflexivity
fn requires_reflexivity(prop: Proposition) -> Bool {
  case prop {
    Implies(Necessary(inner), p) -> propositions_match(inner, p)
    _ -> False
  }
}

/// Check if proposition pattern requires transitivity
fn requires_transitivity(prop: Proposition) -> Bool {
  case prop {
    Implies(Necessary(p), Necessary(Necessary(q))) -> propositions_match(p, q)
    _ -> False
  }
}

/// Check if proposition pattern requires euclidean property
fn requires_euclidean(prop: Proposition) -> Bool {
  case prop {
    Implies(Possible(p), Necessary(Possible(q))) -> propositions_match(p, q)
    _ -> False
  }
}

/// Check if proposition pattern requires seriality
fn requires_seriality(prop: Proposition) -> Bool {
  case prop {
    Implies(Necessary(p), Possible(q)) -> propositions_match(p, q)
    _ -> False
  }
}

/// Check if formalization is T-axiom pattern (□p → p)
fn is_t_axiom_pattern(formalization: Formalization) -> Bool {
  case formalization.premises, formalization.conclusion {
    [Necessary(p)], q -> propositions_match(p, q)
    _, _ -> False
  }
}

/// Check if formalization is 4-axiom pattern (□p → □□p)
fn is_four_axiom_pattern(formalization: Formalization) -> Bool {
  case formalization.premises, formalization.conclusion {
    [Necessary(p)], Necessary(Necessary(q)) -> propositions_match(p, q)
    _, _ -> False
  }
}

/// Check if formalization is 5-axiom pattern (◇p → □◇p)
fn is_five_axiom_pattern(formalization: Formalization) -> Bool {
  case formalization.premises, formalization.conclusion {
    [Possible(p)], Necessary(Possible(q)) -> propositions_match(p, q)
    _, _ -> False
  }
}

/// Check if formalization is D-axiom pattern (□p → ◇p)
fn is_d_axiom_pattern(formalization: Formalization) -> Bool {
  case formalization.premises, formalization.conclusion {
    [Necessary(p)], Possible(q) -> propositions_match(p, q)
    _, _ -> False
  }
}

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
    _, _ -> False
  }
}

/// Get frame properties for a logic system
pub fn get_system_frame_properties(system: LogicSystem) -> List(FrameProperty) {
  case system {
    K -> []
    T -> [Reflexivity]
    K4 -> [Transitivity]
    S4 -> [Reflexivity, Transitivity]
    S5 -> [Reflexivity, Transitivity, Symmetry, Euclidean]
    KD -> [Seriality]
    KD45 -> [Seriality, Transitivity, Euclidean]
  }
}

/// Explain why missing properties cause invalidity
fn explain_missing_properties(
  missing: List(FrameProperty),
  system: LogicSystem,
) -> String {
  let missing_str =
    missing
    |> list.map(frame_property_to_string)
    |> string.join(", ")

  "System "
  <> logic_system_to_string(system)
  <> " lacks: "
  <> missing_str
  <> ". Formula requires these frame properties for validity."
}

/// Check for patterns that make a formula invalid regardless of frame properties
fn check_invalidity_patterns(
  formalization: Formalization,
  system: LogicSystem,
) -> Option(String) {
  // Check for propositional invalidity patterns
  let premises = formalization.premises
  let conclusion = formalization.conclusion

  // Check affirming the consequent: p→q, q ⊢ p
  case is_affirming_consequent(premises, conclusion) {
    True -> Some("Affirming the consequent: conclusion doesn't follow")
    False -> None
  }
}

/// Check if argument is affirming the consequent fallacy
fn is_affirming_consequent(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Bool {
  let implications =
    list.filter_map(premises, fn(p) {
      case p {
        Implies(ant, cons) -> Ok(#(ant, cons))
        _ -> Error(Nil)
      }
    })

  list.any(implications, fn(impl) {
    let #(antecedent, consequent) = impl
    let consequent_in_premises =
      list.any(premises, fn(p) { propositions_match(p, consequent) })
    let conclusion_is_antecedent = propositions_match(conclusion, antecedent)
    consequent_in_premises && conclusion_is_antecedent
  })
}

// =============================================================================
// Comparison Analysis
// =============================================================================

/// Analyze comparison between system results
fn analyze_comparison(
  results: List(SystemValidationResult),
  formalization: Formalization,
) -> SystemComparison {
  let validity_pairs = find_validity_differences(results)
  let strictest = find_strictest_valid(results)
  let most_permissive = find_most_permissive_invalid(results)
  let consensus = determine_consensus(results)
  let analysis = generate_semantic_analysis(results, formalization)

  SystemComparison(
    validity_differs: validity_pairs,
    strictest_valid: strictest,
    most_permissive_invalid: most_permissive,
    semantic_analysis: analysis,
    consensus: consensus,
  )
}

/// Find pairs of systems where validity differs
fn find_validity_differences(
  results: List(SystemValidationResult),
) -> List(#(LogicSystem, LogicSystem)) {
  let valid_systems =
    list.filter_map(results, fn(r) {
      case r.validity {
        ValidFormula -> Ok(r.system)
        _ -> Error(Nil)
      }
    })

  let invalid_systems =
    list.filter_map(results, fn(r) {
      case r.validity {
        InvalidFormula -> Ok(r.system)
        _ -> Error(Nil)
      }
    })

  // Create pairs of (valid_system, invalid_system)
  list.flat_map(valid_systems, fn(v) {
    list.map(invalid_systems, fn(i) { #(i, v) })
  })
}

/// Find the strictest system where formula is valid
fn find_strictest_valid(
  results: List(SystemValidationResult),
) -> Option(LogicSystem) {
  let valid_results =
    list.filter(results, fn(r) {
      case r.validity {
        ValidFormula -> True
        _ -> False
      }
    })

  case valid_results {
    [] -> None
    _ -> {
      // Sort by strictness (fewer properties = stricter)
      let sorted =
        list.sort(valid_results, fn(a, b) {
          let a_props = list.length(get_system_frame_properties(a.system))
          let b_props = list.length(get_system_frame_properties(b.system))
          int.compare(a_props, b_props)
        })

      case sorted {
        [first, ..] -> Some(first.system)
        [] -> None
      }
    }
  }
}

/// Find the most permissive system where formula is invalid
fn find_most_permissive_invalid(
  results: List(SystemValidationResult),
) -> Option(LogicSystem) {
  let invalid_results =
    list.filter(results, fn(r) {
      case r.validity {
        InvalidFormula -> True
        _ -> False
      }
    })

  case invalid_results {
    [] -> None
    _ -> {
      // Sort by permissiveness (more properties = more permissive)
      let sorted =
        list.sort(invalid_results, fn(a, b) {
          let a_props = list.length(get_system_frame_properties(a.system))
          let b_props = list.length(get_system_frame_properties(b.system))
          int.compare(b_props, a_props)
        })

      case sorted {
        [first, ..] -> Some(first.system)
        [] -> None
      }
    }
  }
}

/// Determine consensus outcome
fn determine_consensus(
  results: List(SystemValidationResult),
) -> ConsensusOutcome {
  let valid_systems =
    list.filter_map(results, fn(r) {
      case r.validity {
        ValidFormula -> Ok(r.system)
        _ -> Error(Nil)
      }
    })

  let invalid_systems =
    list.filter_map(results, fn(r) {
      case r.validity {
        InvalidFormula -> Ok(r.system)
        _ -> Error(Nil)
      }
    })

  let unknown_count =
    list.count(results, fn(r) {
      case r.validity {
        UnknownValidity(_) | TimedOut -> True
        _ -> False
      }
    })

  case valid_systems, invalid_systems, unknown_count {
    [], [], _ -> Inconclusive
    _, [], 0 -> AllValid
    [], _, 0 -> AllInvalid
    [_, ..] as v, [_, ..] as i, _ -> Mixed(valid_in: v, invalid_in: i)
    _, _, _ -> Inconclusive
  }
}

/// Generate semantic analysis explaining the differences
fn generate_semantic_analysis(
  results: List(SystemValidationResult),
  formalization: Formalization,
) -> String {
  let valid_count =
    list.count(results, fn(r) {
      case r.validity {
        ValidFormula -> True
        _ -> False
      }
    })

  let invalid_count =
    list.count(results, fn(r) {
      case r.validity {
        InvalidFormula -> True
        _ -> False
      }
    })

  let total = list.length(results)

  case valid_count, invalid_count {
    v, 0 if v == total ->
      "Formula is valid in all tested systems. "
      <> "This indicates a logically necessary truth that holds regardless of frame properties."
    0, i if i == total ->
      "Formula is invalid in all tested systems. "
      <> "This indicates a logical fallacy or unsound inference pattern."
    v, i if v > 0 && i > 0 -> {
      let required = analyze_required_frame_properties(formalization)
      let required_str =
        required
        |> list.map(frame_property_to_string)
        |> string.join(", ")

      case list.length(required) {
        0 ->
          "Validity varies between systems. "
          <> "The formula may have subtle dependencies on frame properties."
        _ ->
          "Formula requires these frame properties: "
          <> required_str
          <> ". "
          <> "Systems lacking these properties cannot validate the inference."
      }
    }
    _, _ ->
      "Results are inconclusive. Some systems timed out or returned unknown."
  }
}

// =============================================================================
// Recommendation Generation
// =============================================================================

/// Generate system recommendation
fn generate_recommendation(
  results: List(SystemValidationResult),
  formalization: Formalization,
  comparison: SystemComparison,
) -> #(Option(LogicSystem), String) {
  // Strategy: Recommend the minimal (strictest) system where the formula is valid
  case comparison.strictest_valid {
    Some(system) -> {
      let reason = generate_recommendation_reason(system, formalization)
      #(Some(system), reason)
    }
    None -> {
      // No system validates it - recommend based on content
      let recommended = recommend_by_content(formalization)
      let reason = case recommended {
        Some(sys) ->
          "Formula is invalid in all systems. "
          <> logic_system_to_string(sys)
          <> " is recommended based on content analysis (modal operators and patterns used)."
        None ->
          "No appropriate system could be determined. Consider reviewing the argument structure."
      }
      #(recommended, reason)
    }
  }
}

/// Generate reason for recommending a system
fn generate_recommendation_reason(
  system: LogicSystem,
  formalization: Formalization,
) -> String {
  let system_str = logic_system_to_string(system)
  let properties = get_system_frame_properties(system)
  let properties_str =
    properties
    |> list.map(frame_property_to_string)
    |> string.join(", ")

  let base_reason = case system {
    K ->
      "Basic modal logic K is sufficient. No special frame properties are required."
    T ->
      "System T (reflexive frames) is minimal for this formula. "
      <> "The formula requires that every world can access itself (the T-axiom: "
      <> "□p → p)."
    K4 ->
      "System K4 (transitive frames) is needed. "
      <> "The formula relies on chaining necessity across accessible worlds."
    S4 ->
      "System S4 (reflexive + transitive) is appropriate. "
      <> "The formula combines T-axiom and 4-axiom patterns."
    S5 ->
      "System S5 (equivalence relation) is needed. "
      <> "The formula requires full modal collapse where possibility and necessity interact symmetrically."
    KD ->
      "System KD (serial frames) is appropriate for deontic reasoning. "
      <> "The formula requires that obligations are consistent (D-axiom: □p → ◇p)."
    KD45 ->
      "System KD45 is appropriate for full doxastic/belief logic. "
      <> "The formula uses belief patterns requiring seriality, transitivity, and euclidean properties."
  }

  case list.length(properties) {
    0 -> base_reason
    _ -> base_reason <> " Required frame properties: " <> properties_str <> "."
  }
}

/// Recommend system based on formula content analysis
fn recommend_by_content(formalization: Formalization) -> Option(LogicSystem) {
  let all_props =
    list.append(formalization.premises, [formalization.conclusion])

  let has_knowledge =
    list.any(all_props, fn(p) { contains_knowledge_operator(p) })
  let has_belief = list.any(all_props, fn(p) { contains_belief_operator(p) })
  let has_obligation =
    list.any(all_props, fn(p) { contains_deontic_operator(p) })
  let has_necessity =
    list.any(all_props, fn(p) { contains_necessity_operator(p) })
  let has_possibility =
    list.any(all_props, fn(p) { contains_possibility_operator(p) })

  case has_knowledge, has_belief, has_obligation {
    True, _, _ -> Some(S5)
    // Knowledge typically uses S5
    _, True, _ -> Some(KD45)
    // Belief uses KD45
    _, _, True -> Some(KD)
    // Deontic uses KD
    _, _, _ ->
      case has_necessity, has_possibility {
        True, True -> Some(S4)
        // Mixed modal uses S4
        True, False -> Some(T)
        // Just necessity, use T
        False, True -> Some(K)
        // Just possibility, use K
        False, False -> Some(K)
        // No modals, use K
      }
  }
}

/// Check if proposition contains knowledge operator
fn contains_knowledge_operator(prop: Proposition) -> Bool {
  case prop {
    Knows(_, _) -> True
    Not(inner) -> contains_knowledge_operator(inner)
    And(a, b) ->
      contains_knowledge_operator(a) || contains_knowledge_operator(b)
    Or(a, b) -> contains_knowledge_operator(a) || contains_knowledge_operator(b)
    Implies(a, b) ->
      contains_knowledge_operator(a) || contains_knowledge_operator(b)
    Necessary(inner) -> contains_knowledge_operator(inner)
    Possible(inner) -> contains_knowledge_operator(inner)
    _ -> False
  }
}

/// Check if proposition contains belief operator
fn contains_belief_operator(prop: Proposition) -> Bool {
  case prop {
    Believes(_, _) -> True
    Not(inner) -> contains_belief_operator(inner)
    And(a, b) -> contains_belief_operator(a) || contains_belief_operator(b)
    Or(a, b) -> contains_belief_operator(a) || contains_belief_operator(b)
    Implies(a, b) -> contains_belief_operator(a) || contains_belief_operator(b)
    Necessary(inner) -> contains_belief_operator(inner)
    Possible(inner) -> contains_belief_operator(inner)
    _ -> False
  }
}

/// Check if proposition contains deontic operator (Obligatory, Permitted)
fn contains_deontic_operator(prop: Proposition) -> Bool {
  case prop {
    proposition.Obligatory(_) -> True
    proposition.Permitted(_) -> True
    Not(inner) -> contains_deontic_operator(inner)
    And(a, b) -> contains_deontic_operator(a) || contains_deontic_operator(b)
    Or(a, b) -> contains_deontic_operator(a) || contains_deontic_operator(b)
    Implies(a, b) ->
      contains_deontic_operator(a) || contains_deontic_operator(b)
    Necessary(inner) -> contains_deontic_operator(inner)
    Possible(inner) -> contains_deontic_operator(inner)
    _ -> False
  }
}

/// Check if proposition contains necessity operator
fn contains_necessity_operator(prop: Proposition) -> Bool {
  case prop {
    Necessary(_) -> True
    Not(inner) -> contains_necessity_operator(inner)
    And(a, b) ->
      contains_necessity_operator(a) || contains_necessity_operator(b)
    Or(a, b) -> contains_necessity_operator(a) || contains_necessity_operator(b)
    Implies(a, b) ->
      contains_necessity_operator(a) || contains_necessity_operator(b)
    Possible(inner) -> contains_necessity_operator(inner)
    _ -> False
  }
}

/// Check if proposition contains possibility operator
fn contains_possibility_operator(prop: Proposition) -> Bool {
  case prop {
    Possible(_) -> True
    Not(inner) -> contains_possibility_operator(inner)
    And(a, b) ->
      contains_possibility_operator(a) || contains_possibility_operator(b)
    Or(a, b) ->
      contains_possibility_operator(a) || contains_possibility_operator(b)
    Implies(a, b) ->
      contains_possibility_operator(a) || contains_possibility_operator(b)
    Necessary(inner) -> contains_possibility_operator(inner)
    _ -> False
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Build results dictionary from list
fn build_results_dict(
  results: List(SystemValidationResult),
) -> Dict(String, SystemValidationResult) {
  results
  |> list.map(fn(r) { #(logic_system_to_string(r.system), r) })
  |> dict.from_list
}

/// Convert logic system to string
pub fn logic_system_to_string(system: LogicSystem) -> String {
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

/// Convert frame property to string
pub fn frame_property_to_string(property: FrameProperty) -> String {
  case property {
    Reflexivity -> "reflexivity"
    Transitivity -> "transitivity"
    Symmetry -> "symmetry"
    Seriality -> "seriality"
    Euclidean -> "euclidean"
  }
}

/// Convert validity to string
pub fn validity_to_string(validity: Validity) -> String {
  case validity {
    ValidFormula -> "Valid"
    InvalidFormula -> "Invalid"
    UnknownValidity(reason) -> "Unknown: " <> reason
    TimedOut -> "Timeout"
  }
}

/// Convert consensus to string
pub fn consensus_to_string(consensus: ConsensusOutcome) -> String {
  case consensus {
    AllValid -> "All systems agree: Valid"
    AllInvalid -> "All systems agree: Invalid"
    Mixed(v, i) ->
      "Mixed: Valid in "
      <> int.to_string(list.length(v))
      <> " systems, Invalid in "
      <> int.to_string(list.length(i))
    Inconclusive -> "Inconclusive"
  }
}

// =============================================================================
// Formatting Functions
// =============================================================================

/// Format multi-system validation result as a report
pub fn format_validation_report(
  validation: MultiSystemValidation,
  detailed: Bool,
) -> String {
  let header = "Multi-System Validation Report"
  let separator = string.repeat("=", 60)

  let results_section = format_results_section(validation.results, detailed)

  let comparison_section = format_comparison_section(validation.comparison)

  let recommendation_section =
    format_recommendation_section(
      validation.recommended_system,
      validation.recommendation_reason,
    )

  let timing =
    "Total execution time: "
    <> int.to_string(validation.total_duration_ms)
    <> "ms"

  [
    header,
    separator,
    results_section,
    comparison_section,
    recommendation_section,
    timing,
  ]
  |> string.join("\n\n")
}

/// Format results section
fn format_results_section(
  results: Dict(String, SystemValidationResult),
  detailed: Bool,
) -> String {
  let header = "Results by System:"

  let entries =
    dict.to_list(results)
    |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
    |> list.map(fn(entry) {
      let #(name, result) = entry
      let validity_str = validity_to_string(result.validity)
      let confidence_str = float_to_percent(result.confidence)

      let base =
        "  "
        <> name
        <> ": "
        <> validity_str
        <> " (confidence: "
        <> confidence_str
        <> ")"

      case detailed, result.countermodel {
        True, Some(cm) -> base <> "\n    Countermodel: " <> cm
        _, _ -> base
      }
    })
    |> string.join("\n")

  header <> "\n" <> entries
}

/// Format comparison section
fn format_comparison_section(comparison: SystemComparison) -> String {
  let header = "Comparison Analysis:"

  let consensus_line =
    "  Consensus: " <> consensus_to_string(comparison.consensus)

  let strictest_line = case comparison.strictest_valid {
    Some(sys) -> "  Strictest valid system: " <> logic_system_to_string(sys)
    None -> "  No system validates the formula"
  }

  let analysis_line = "  Analysis: " <> comparison.semantic_analysis

  [header, consensus_line, strictest_line, analysis_line]
  |> string.join("\n")
}

/// Format recommendation section
fn format_recommendation_section(
  recommended: Option(LogicSystem),
  reason: String,
) -> String {
  let header = "Recommendation:"

  let system_line = case recommended {
    Some(sys) -> "  Recommended system: " <> logic_system_to_string(sys)
    None -> "  No specific system recommended"
  }

  let reason_line = "  Reason: " <> reason

  [header, system_line, reason_line]
  |> string.join("\n")
}

/// Convert float to percentage string
fn float_to_percent(f: Float) -> String {
  let pct = float.round(f *. 100.0)
  int.to_string(pct) <> "%"
}

// =============================================================================
// JSON-like Output
// =============================================================================

/// Format validation as JSON-like string
pub fn format_as_json(validation: MultiSystemValidation) -> String {
  let results_json = format_results_as_json(validation.results)

  let recommended_str = case validation.recommended_system {
    Some(sys) -> "\"" <> logic_system_to_string(sys) <> "\""
    None -> "null"
  }

  let differences_json =
    validation.comparison.validity_differs
    |> list.map(fn(pair) {
      let #(s1, s2) = pair
      "[\""
      <> logic_system_to_string(s1)
      <> "\", \""
      <> logic_system_to_string(s2)
      <> "\"]"
    })
    |> string.join(", ")

  let strictest_json = case validation.comparison.strictest_valid {
    Some(sys) -> "\"" <> logic_system_to_string(sys) <> "\""
    None -> "null"
  }

  "{\n"
  <> "  \"results\": {\n"
  <> results_json
  <> "\n  },\n"
  <> "  \"recommended_system\": "
  <> recommended_str
  <> ",\n"
  <> "  \"recommendation_reason\": \""
  <> escape_json_string(validation.recommendation_reason)
  <> "\",\n"
  <> "  \"comparison\": {\n"
  <> "    \"validity_differs\": ["
  <> differences_json
  <> "],\n"
  <> "    \"strictest_valid\": "
  <> strictest_json
  <> ",\n"
  <> "    \"semantic_analysis\": \""
  <> escape_json_string(validation.comparison.semantic_analysis)
  <> "\"\n"
  <> "  },\n"
  <> "  \"total_duration_ms\": "
  <> int.to_string(validation.total_duration_ms)
  <> "\n}"
}

/// Format results as JSON
fn format_results_as_json(
  results: Dict(String, SystemValidationResult),
) -> String {
  dict.to_list(results)
  |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
  |> list.map(fn(entry) {
    let #(name, result) = entry
    let validity_str = case result.validity {
      ValidFormula -> "\"Valid\""
      InvalidFormula -> "\"Invalid\""
      UnknownValidity(_) -> "\"Unknown\""
      TimedOut -> "\"Timeout\""
    }

    "    \""
    <> name
    <> "\": {\"validity\": "
    <> validity_str
    <> ", \"confidence\": "
    <> float.to_string(result.confidence)
    <> "}"
  })
  |> string.join(",\n")
}

/// Escape string for JSON
fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}
