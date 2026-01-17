//// Probabilistic Modal Logic Support
////
//// This module extends modal logic with probabilistic operators for reasoning
//// about uncertainty, likelihood, and conditional probabilities.
////
//// ## Overview
////
//// Probabilistic modal logic combines standard modal logic with probability
//// measures over possible worlds, enabling reasoning about statements like:
//// - "The stock will probably rise" (Probable)
//// - "There's at least 70% chance of rain" (ProbAtLeast)
//// - "If the market is up, 90% chance stock rises" (CondProb)
////
//// ## Semantics
////
//// Probabilistic Kripke frames extend standard frames with probability measures:
//// - Frame = (W, R, μ) where μ: W → [0,1] is a probability distribution
//// - w ⊨ Probable(φ) iff Σ{μ(v) : v ∈ R(w) and v ⊨ φ} > 0.5
//// - w ⊨ ProbAtLeast(φ, t) iff Σ{μ(v) : v ∈ R(w) and v ⊨ φ} ≥ t
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/probabilistic
//// import modal_logic/proposition.{Probable, ProbAtLeast, CondProb, Atom}
////
//// // Create a probabilistic argument
//// let market_up = Atom("market_up")
//// let stock_up = Atom("stock_up")
////
//// let premises = [
////   ProbAtLeast(market_up, 0.7),           // P(market_up) >= 0.7
////   CondProb(stock_up, market_up, 0.9),    // P(stock_up|market_up) = 0.9
//// ]
//// let conclusion = Probable(stock_up)      // P(stock_up) > 0.5
////
//// // Validate
//// let result = probabilistic.validate_probabilistic(premises, conclusion)
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/argument.{type Formalization, type ValidationResult}
import modal_logic/proposition.{
  type Proposition, And, Atom, Believes, CondProb, Implies, Knows, Necessary,
  Not, Obligatory, Or, Permitted, Possible, ProbAtLeast, ProbAtMost, ProbExact,
  ProbRange, Probable,
}

// =============================================================================
// Types
// =============================================================================

/// Result of probabilistic validation
pub type ProbabilisticResult {
  ProbabilisticResult(
    /// Whether the argument is valid
    valid: Bool,
    /// Computed probability bounds
    bounds: ProbabilityBounds,
    /// Explanation of the reasoning
    explanation: String,
    /// Confidence in the result
    confidence: Float,
    /// Validation method used
    method: ValidationMethod,
  )
}

/// Probability bounds for a proposition
pub type ProbabilityBounds {
  ProbabilityBounds(
    /// Lower bound on probability
    lower: Float,
    /// Upper bound on probability
    upper: Float,
    /// Whether bounds are exact
    exact: Bool,
  )
}

/// Method used for validation
pub type ValidationMethod {
  /// Direct probability calculation
  DirectCalculation
  /// Probability chain rule
  ChainRule
  /// Bayes' theorem application
  BayesTheorem
  /// Interval arithmetic
  IntervalArithmetic
  /// Constraint propagation
  ConstraintPropagation
}

/// Constraint on a probability variable
pub type ProbConstraint {
  /// P(φ) >= value
  AtLeast(proposition: String, value: Float)
  /// P(φ) <= value
  AtMost(proposition: String, value: Float)
  /// P(φ) = value
  Exactly(proposition: String, value: Float)
  /// P(φ|ψ) = value
  Conditional(consequent: String, antecedent: String, value: Float)
  /// low <= P(φ) <= high
  Range(proposition: String, low: Float, high: Float)
}

/// State for probabilistic reasoning
pub type ProbabilisticState {
  ProbabilisticState(
    /// Known probability constraints
    constraints: List(ProbConstraint),
    /// Computed bounds for propositions
    bounds: Dict(String, ProbabilityBounds),
    /// Conditional probability table
    conditionals: Dict(#(String, String), Float),
  )
}

/// Configuration for probabilistic validation
pub type ProbabilisticConfig {
  ProbabilisticConfig(
    /// Tolerance for floating point comparisons
    tolerance: Float,
    /// Maximum iterations for constraint propagation
    max_iterations: Int,
    /// Whether to use interval arithmetic
    use_intervals: Bool,
    /// Default probability for unknown propositions
    default_probability: Float,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Default configuration for probabilistic validation
pub fn default_config() -> ProbabilisticConfig {
  ProbabilisticConfig(
    tolerance: 0.0001,
    max_iterations: 100,
    use_intervals: True,
    default_probability: 0.5,
  )
}

/// Strict configuration with tighter tolerances
pub fn strict_config() -> ProbabilisticConfig {
  ProbabilisticConfig(
    tolerance: 0.00001,
    max_iterations: 200,
    use_intervals: True,
    default_probability: 0.5,
  )
}

/// Fast configuration for quick validation
pub fn fast_config() -> ProbabilisticConfig {
  ProbabilisticConfig(
    tolerance: 0.001,
    max_iterations: 50,
    use_intervals: False,
    default_probability: 0.5,
  )
}

// =============================================================================
// Main Validation Functions
// =============================================================================

/// Validate a probabilistic argument
pub fn validate_probabilistic(
  premises: List(Proposition),
  conclusion: Proposition,
) -> ProbabilisticResult {
  validate_probabilistic_with_config(premises, conclusion, default_config())
}

/// Validate a probabilistic argument with custom configuration
pub fn validate_probabilistic_with_config(
  premises: List(Proposition),
  conclusion: Proposition,
  config: ProbabilisticConfig,
) -> ProbabilisticResult {
  // Extract constraints from premises
  let state = extract_constraints(premises)

  // Propagate constraints to compute bounds
  let propagated_state = propagate_constraints(state, config)

  // Check if conclusion is entailed
  let #(valid, bounds, method) =
    check_conclusion(conclusion, propagated_state, config)

  // Generate explanation
  let explanation = generate_explanation(premises, conclusion, valid, method)

  // Compute confidence based on bound tightness
  let confidence = compute_confidence(bounds)

  ProbabilisticResult(
    valid: valid,
    bounds: bounds,
    explanation: explanation,
    confidence: confidence,
    method: method,
  )
}

/// Quick check if a proposition involves probabilities
pub fn is_probabilistic(prop: Proposition) -> Bool {
  case prop {
    Probable(_) -> True
    ProbAtLeast(_, _) -> True
    ProbAtMost(_, _) -> True
    ProbExact(_, _) -> True
    ProbRange(_, _, _) -> True
    CondProb(_, _, _) -> True
    And(p1, p2) -> is_probabilistic(p1) || is_probabilistic(p2)
    Or(p1, p2) -> is_probabilistic(p1) || is_probabilistic(p2)
    Implies(p1, p2) -> is_probabilistic(p1) || is_probabilistic(p2)
    Not(p) -> is_probabilistic(p)
    Necessary(p) -> is_probabilistic(p)
    Possible(p) -> is_probabilistic(p)
    _ -> False
  }
}

/// Check if any premises or conclusion involve probabilities
pub fn has_probabilistic_content(
  premises: List(Proposition),
  conclusion: Proposition,
) -> Bool {
  is_probabilistic(conclusion) || list.any(premises, is_probabilistic)
}

// =============================================================================
// Constraint Extraction
// =============================================================================

/// Extract probability constraints from premises
fn extract_constraints(premises: List(Proposition)) -> ProbabilisticState {
  let constraints =
    premises
    |> list.flat_map(extract_constraint_from_proposition)

  ProbabilisticState(
    constraints: constraints,
    bounds: dict.new(),
    conditionals: dict.new(),
  )
}

/// Extract constraints from a single proposition
fn extract_constraint_from_proposition(
  prop: Proposition,
) -> List(ProbConstraint) {
  case prop {
    Probable(inner) -> {
      let key = proposition_key(inner)
      [AtLeast(key, 0.5001)]
    }

    ProbAtLeast(inner, threshold) -> {
      let key = proposition_key(inner)
      [AtLeast(key, threshold)]
    }

    ProbAtMost(inner, threshold) -> {
      let key = proposition_key(inner)
      [AtMost(key, threshold)]
    }

    ProbExact(inner, value) -> {
      let key = proposition_key(inner)
      [Exactly(key, value)]
    }

    ProbRange(inner, low, high) -> {
      let key = proposition_key(inner)
      [Range(key, low, high)]
    }

    CondProb(consequent, antecedent, value) -> {
      let cons_key = proposition_key(consequent)
      let ante_key = proposition_key(antecedent)
      [Conditional(cons_key, ante_key, value)]
    }

    And(p1, p2) ->
      list.append(
        extract_constraint_from_proposition(p1),
        extract_constraint_from_proposition(p2),
      )

    _ -> []
  }
}

/// Generate a unique key for a proposition
fn proposition_key(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "not_" <> proposition_key(inner)
    And(p1, p2) -> "and_" <> proposition_key(p1) <> "_" <> proposition_key(p2)
    Or(p1, p2) -> "or_" <> proposition_key(p1) <> "_" <> proposition_key(p2)
    Implies(p1, p2) ->
      "implies_" <> proposition_key(p1) <> "_" <> proposition_key(p2)
    Necessary(inner) -> "necessary_" <> proposition_key(inner)
    Possible(inner) -> "possible_" <> proposition_key(inner)
    Probable(inner) -> "probable_" <> proposition_key(inner)
    ProbAtLeast(inner, t) ->
      "prob_at_least_" <> proposition_key(inner) <> "_" <> float.to_string(t)
    ProbAtMost(inner, t) ->
      "prob_at_most_" <> proposition_key(inner) <> "_" <> float.to_string(t)
    ProbExact(inner, p) ->
      "prob_exact_" <> proposition_key(inner) <> "_" <> float.to_string(p)
    ProbRange(inner, l, h) ->
      "prob_range_"
      <> proposition_key(inner)
      <> "_"
      <> float.to_string(l)
      <> "_"
      <> float.to_string(h)
    CondProb(cons, ante, p) ->
      "cond_prob_"
      <> proposition_key(cons)
      <> "_given_"
      <> proposition_key(ante)
      <> "_"
      <> float.to_string(p)
    Obligatory(inner) -> "obligatory_" <> proposition_key(inner)
    Permitted(inner) -> "permitted_" <> proposition_key(inner)
    Knows(agent, inner) -> "knows_" <> agent <> "_" <> proposition_key(inner)
    Believes(agent, inner) ->
      "believes_" <> agent <> "_" <> proposition_key(inner)
  }
}

// =============================================================================
// Constraint Propagation
// =============================================================================

/// Propagate constraints to compute probability bounds
fn propagate_constraints(
  state: ProbabilisticState,
  config: ProbabilisticConfig,
) -> ProbabilisticState {
  // Initialize bounds from direct constraints
  let initial_bounds = initialize_bounds(state.constraints)

  // Store conditionals
  let conditionals = extract_conditionals(state.constraints)

  // Iteratively propagate constraints
  let final_bounds =
    propagate_iterations(
      initial_bounds,
      state.constraints,
      conditionals,
      config,
      0,
    )

  ProbabilisticState(
    constraints: state.constraints,
    bounds: final_bounds,
    conditionals: conditionals,
  )
}

/// Initialize bounds from direct constraints
fn initialize_bounds(
  constraints: List(ProbConstraint),
) -> Dict(String, ProbabilityBounds) {
  list.fold(constraints, dict.new(), fn(acc, constraint) {
    case constraint {
      AtLeast(prop, value) -> {
        let current = dict.get(acc, prop) |> result.unwrap(default_bounds())
        let new_bounds =
          ProbabilityBounds(
            lower: float.max(current.lower, value),
            upper: current.upper,
            exact: False,
          )
        dict.insert(acc, prop, new_bounds)
      }

      AtMost(prop, value) -> {
        let current = dict.get(acc, prop) |> result.unwrap(default_bounds())
        let new_bounds =
          ProbabilityBounds(
            lower: current.lower,
            upper: float.min(current.upper, value),
            exact: False,
          )
        dict.insert(acc, prop, new_bounds)
      }

      Exactly(prop, value) -> {
        dict.insert(
          acc,
          prop,
          ProbabilityBounds(lower: value, upper: value, exact: True),
        )
      }

      Range(prop, low, high) -> {
        let current = dict.get(acc, prop) |> result.unwrap(default_bounds())
        let new_bounds =
          ProbabilityBounds(
            lower: float.max(current.lower, low),
            upper: float.min(current.upper, high),
            exact: False,
          )
        dict.insert(acc, prop, new_bounds)
      }

      Conditional(_, _, _) ->
        // Conditionals are handled separately
        acc
    }
  })
}

/// Extract conditional probabilities
fn extract_conditionals(
  constraints: List(ProbConstraint),
) -> Dict(#(String, String), Float) {
  list.fold(constraints, dict.new(), fn(acc, constraint) {
    case constraint {
      Conditional(cons, ante, value) -> dict.insert(acc, #(cons, ante), value)
      _ -> acc
    }
  })
}

/// Default bounds for unknown propositions
fn default_bounds() -> ProbabilityBounds {
  ProbabilityBounds(lower: 0.0, upper: 1.0, exact: False)
}

/// Iteratively propagate constraints
fn propagate_iterations(
  bounds: Dict(String, ProbabilityBounds),
  constraints: List(ProbConstraint),
  conditionals: Dict(#(String, String), Float),
  config: ProbabilisticConfig,
  iteration: Int,
) -> Dict(String, ProbabilityBounds) {
  case iteration >= config.max_iterations {
    True -> bounds
    False -> {
      // Apply chain rule propagation
      let new_bounds = apply_chain_rule(bounds, conditionals, config)

      // Check for convergence
      case bounds_converged(bounds, new_bounds, config.tolerance) {
        True -> new_bounds
        False ->
          propagate_iterations(
            new_bounds,
            constraints,
            conditionals,
            config,
            iteration + 1,
          )
      }
    }
  }
}

/// Apply probability chain rule: P(A) >= P(A|B) * P(B)
fn apply_chain_rule(
  bounds: Dict(String, ProbabilityBounds),
  conditionals: Dict(#(String, String), Float),
  config: ProbabilisticConfig,
) -> Dict(String, ProbabilityBounds) {
  dict.fold(conditionals, bounds, fn(acc, key, cond_prob) {
    let #(consequent, antecedent) = key

    // Get bounds for antecedent
    let ante_bounds =
      dict.get(acc, antecedent) |> result.unwrap(default_bounds())

    // Compute implied lower bound: P(A) >= P(A|B) * P(B)_lower
    let implied_lower = cond_prob *. ante_bounds.lower

    // Get current bounds for consequent
    let cons_bounds =
      dict.get(acc, consequent) |> result.unwrap(default_bounds())

    // Update if implied lower is higher
    case implied_lower >. cons_bounds.lower +. config.tolerance {
      True -> {
        let new_bounds =
          ProbabilityBounds(
            lower: implied_lower,
            upper: cons_bounds.upper,
            exact: False,
          )
        dict.insert(acc, consequent, new_bounds)
      }
      False -> acc
    }
  })
}

/// Check if bounds have converged
fn bounds_converged(
  old_bounds: Dict(String, ProbabilityBounds),
  new_bounds: Dict(String, ProbabilityBounds),
  tolerance: Float,
) -> Bool {
  let old_list = dict.to_list(old_bounds)
  let new_list = dict.to_list(new_bounds)

  case list.length(old_list) == list.length(new_list) {
    False -> False
    True -> {
      list.all(old_list, fn(pair) {
        let #(key, old_bound) = pair
        case dict.get(new_bounds, key) {
          Ok(new_bound) ->
            float.absolute_value(old_bound.lower -. new_bound.lower)
            <. tolerance
            && float.absolute_value(old_bound.upper -. new_bound.upper)
            <. tolerance
          Error(_) -> False
        }
      })
    }
  }
}

// =============================================================================
// Conclusion Checking
// =============================================================================

/// Check if conclusion is entailed by the propagated state
fn check_conclusion(
  conclusion: Proposition,
  state: ProbabilisticState,
  config: ProbabilisticConfig,
) -> #(Bool, ProbabilityBounds, ValidationMethod) {
  case conclusion {
    Probable(inner) -> {
      let key = proposition_key(inner)
      let bounds =
        dict.get(state.bounds, key) |> result.unwrap(default_bounds())

      // Probable(φ) is true if lower bound > 0.5
      let valid = bounds.lower >. 0.5 +. config.tolerance
      #(valid, bounds, DirectCalculation)
    }

    ProbAtLeast(inner, threshold) -> {
      let key = proposition_key(inner)
      let bounds =
        dict.get(state.bounds, key) |> result.unwrap(default_bounds())

      // ProbAtLeast(φ, t) is true if lower bound >= t
      let valid = bounds.lower >=. threshold -. config.tolerance
      #(valid, bounds, DirectCalculation)
    }

    ProbAtMost(inner, threshold) -> {
      let key = proposition_key(inner)
      let bounds =
        dict.get(state.bounds, key) |> result.unwrap(default_bounds())

      // ProbAtMost(φ, t) is true if upper bound <= t
      let valid = bounds.upper <=. threshold +. config.tolerance
      #(valid, bounds, DirectCalculation)
    }

    ProbExact(inner, value) -> {
      let key = proposition_key(inner)
      let bounds =
        dict.get(state.bounds, key) |> result.unwrap(default_bounds())

      // ProbExact(φ, p) is true if bounds contain p exactly
      let valid =
        bounds.exact
        && float.absolute_value(bounds.lower -. value) <. config.tolerance
      #(valid, bounds, DirectCalculation)
    }

    ProbRange(inner, low, high) -> {
      let key = proposition_key(inner)
      let bounds =
        dict.get(state.bounds, key) |> result.unwrap(default_bounds())

      // ProbRange(φ, l, h) is true if bounds are within [l, h]
      let valid =
        bounds.lower >=. low -. config.tolerance
        && bounds.upper <=. high +. config.tolerance
      #(valid, bounds, IntervalArithmetic)
    }

    // For non-probabilistic conclusions, check if they follow from prob constraints
    _ -> {
      // Default to unknown bounds and not valid for probabilistic entailment
      let bounds = default_bounds()
      #(False, bounds, DirectCalculation)
    }
  }
}

// =============================================================================
// Explanation Generation
// =============================================================================

/// Generate explanation for the validation result
fn generate_explanation(
  premises: List(Proposition),
  conclusion: Proposition,
  valid: Bool,
  method: ValidationMethod,
) -> String {
  let method_name = case method {
    DirectCalculation -> "direct probability calculation"
    ChainRule -> "probability chain rule"
    BayesTheorem -> "Bayes' theorem"
    IntervalArithmetic -> "interval arithmetic"
    ConstraintPropagation -> "constraint propagation"
  }

  let validity_str = case valid {
    True -> "VALID"
    False -> "INVALID"
  }

  let premise_count = list.length(premises)

  validity_str
  <> " - Determined by "
  <> method_name
  <> " from "
  <> int.to_string(premise_count)
  <> " premise(s)"
}

/// Compute confidence based on bound tightness
fn compute_confidence(bounds: ProbabilityBounds) -> Float {
  case bounds.exact {
    True -> 1.0
    False -> {
      // Confidence is higher when bounds are tighter
      let width = bounds.upper -. bounds.lower
      case width <. 0.01 {
        True -> 0.99
        False ->
          case width <. 0.1 {
            True -> 0.9
            False ->
              case width <. 0.3 {
                True -> 0.8
                False -> 0.5 +. { { 1.0 -. width } *. 0.3 }
              }
          }
      }
    }
  }
}

// =============================================================================
// Probability Bound Arithmetic
// =============================================================================

/// Add probability bounds: P(A) + P(B) for independent events
pub fn add_bounds(
  a: ProbabilityBounds,
  b: ProbabilityBounds,
) -> ProbabilityBounds {
  ProbabilityBounds(
    lower: float.min(1.0, a.lower +. b.lower),
    upper: float.min(1.0, a.upper +. b.upper),
    exact: a.exact && b.exact,
  )
}

/// Multiply probability bounds: P(A) * P(B) for independent events
pub fn multiply_bounds(
  a: ProbabilityBounds,
  b: ProbabilityBounds,
) -> ProbabilityBounds {
  ProbabilityBounds(
    lower: a.lower *. b.lower,
    upper: a.upper *. b.upper,
    exact: a.exact && b.exact,
  )
}

/// Complement bounds: P(not A) = 1 - P(A)
pub fn complement_bounds(bounds: ProbabilityBounds) -> ProbabilityBounds {
  ProbabilityBounds(
    lower: 1.0 -. bounds.upper,
    upper: 1.0 -. bounds.lower,
    exact: bounds.exact,
  )
}

/// Intersect bounds: tightest interval containing both
pub fn intersect_bounds(
  a: ProbabilityBounds,
  b: ProbabilityBounds,
) -> ProbabilityBounds {
  ProbabilityBounds(
    lower: float.max(a.lower, b.lower),
    upper: float.min(a.upper, b.upper),
    exact: a.exact && b.exact && a.lower == b.lower && a.upper == b.upper,
  )
}

/// Check if bounds are valid (lower <= upper)
pub fn bounds_valid(bounds: ProbabilityBounds) -> Bool {
  bounds.lower <=. bounds.upper
}

/// Check if probability value is in bounds
pub fn in_bounds(value: Float, bounds: ProbabilityBounds) -> Bool {
  value >=. bounds.lower && value <=. bounds.upper
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Create a Probable proposition
pub fn probable(prop: Proposition) -> Proposition {
  Probable(prop)
}

/// Create a ProbAtLeast proposition
pub fn prob_at_least(prop: Proposition, threshold: Float) -> Proposition {
  ProbAtLeast(prop, threshold)
}

/// Create a ProbAtMost proposition
pub fn prob_at_most(prop: Proposition, threshold: Float) -> Proposition {
  ProbAtMost(prop, threshold)
}

/// Create a ProbExact proposition
pub fn prob_exact(prop: Proposition, probability: Float) -> Proposition {
  ProbExact(prop, probability)
}

/// Create a ProbRange proposition
pub fn prob_range(prop: Proposition, low: Float, high: Float) -> Proposition {
  ProbRange(prop, low, high)
}

/// Create a CondProb proposition
pub fn cond_prob(
  consequent: Proposition,
  antecedent: Proposition,
  probability: Float,
) -> Proposition {
  CondProb(consequent, antecedent, probability)
}

/// Format probability bounds as string
pub fn format_bounds(bounds: ProbabilityBounds) -> String {
  case bounds.exact {
    True -> "P = " <> float.to_string(bounds.lower)
    False ->
      float.to_string(bounds.lower)
      <> " <= P <= "
      <> float.to_string(bounds.upper)
  }
}

/// Format a probabilistic proposition as string
pub fn format_probabilistic(prop: Proposition) -> String {
  case prop {
    Probable(inner) -> "Probable(" <> format_inner(inner) <> ")"
    ProbAtLeast(inner, t) ->
      "P(" <> format_inner(inner) <> ") >= " <> float.to_string(t)
    ProbAtMost(inner, t) ->
      "P(" <> format_inner(inner) <> ") <= " <> float.to_string(t)
    ProbExact(inner, p) ->
      "P(" <> format_inner(inner) <> ") = " <> float.to_string(p)
    ProbRange(inner, l, h) ->
      float.to_string(l)
      <> " <= P("
      <> format_inner(inner)
      <> ") <= "
      <> float.to_string(h)
    CondProb(cons, ante, p) ->
      "P("
      <> format_inner(cons)
      <> " | "
      <> format_inner(ante)
      <> ") = "
      <> float.to_string(p)
    _ -> "non-probabilistic"
  }
}

/// Format inner proposition (simplified)
fn format_inner(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "not " <> format_inner(inner)
    And(p1, p2) -> format_inner(p1) <> " and " <> format_inner(p2)
    Or(p1, p2) -> format_inner(p1) <> " or " <> format_inner(p2)
    _ -> "<complex>"
  }
}

/// Check if a probability value is valid (between 0 and 1)
pub fn is_valid_probability(p: Float) -> Bool {
  p >=. 0.0 && p <=. 1.0
}

/// Clamp a value to valid probability range
pub fn clamp_probability(p: Float) -> Float {
  float.max(0.0, float.min(1.0, p))
}
