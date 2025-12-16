//// Inference Rule Types for Modal Logic
////
//// This module defines the core types for modal logic inference rules,
//// including patterns for matching propositions and rule metadata.

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, Knows,
  Necessary, Not, Obligatory, Or, Permitted, Possible,
}

/// A pattern for matching propositions with variable binding
pub type PropositionPattern {
  /// Match any atom and bind to variable name
  AnyAtom(variable: String)
  /// Match a specific atom value
  SpecificAtom(name: String)
  /// Match negation pattern
  PatternNot(inner: PropositionPattern)
  /// Match conjunction pattern
  PatternAnd(left: PropositionPattern, right: PropositionPattern)
  /// Match disjunction pattern
  PatternOr(left: PropositionPattern, right: PropositionPattern)
  /// Match implication pattern
  PatternImplies(antecedent: PropositionPattern, consequent: PropositionPattern)
  /// Match necessity pattern
  PatternNecessary(inner: PropositionPattern)
  /// Match possibility pattern
  PatternPossible(inner: PropositionPattern)
  /// Match obligation pattern
  PatternObligatory(inner: PropositionPattern)
  /// Match permission pattern
  PatternPermitted(inner: PropositionPattern)
  /// Match knowledge pattern with agent variable
  PatternKnows(agent_var: String, inner: PropositionPattern)
  /// Match belief pattern with agent variable
  PatternBelieves(agent_var: String, inner: PropositionPattern)
  /// Match any proposition and bind to variable
  Wildcard(variable: String)
}

/// An inference rule that derives conclusions from premises
pub type InferenceRule {
  InferenceRule(
    /// Unique identifier
    id: String,
    /// Human-readable name
    name: String,
    /// Description of what the rule does
    description: String,
    /// Premise patterns to match
    premise_patterns: List(PropositionPattern),
    /// Conclusion pattern to derive
    conclusion_pattern: PropositionPattern,
    /// Logic systems where this rule is sound
    valid_in: List(LogicSystem),
    /// Rule metadata
    metadata: RuleMetadata,
  )
}

/// Metadata about a rule
pub type RuleMetadata {
  RuleMetadata(
    /// Version number
    version: Int,
    /// When the rule was created
    created_at: Option(String),
    /// When the rule was last updated
    updated_at: Option(String),
    /// Author of the rule
    author: Option(String),
    /// Source reference
    source: Option(String),
    /// Tags for categorization
    tags: List(String),
    /// Usage examples
    examples: List(RuleExample),
  )
}

/// An example of rule application
pub type RuleExample {
  RuleExample(
    /// Description of the example
    description: String,
    /// Input premises
    premises: List(Proposition),
    /// Expected conclusion
    conclusion: Proposition,
    /// Whether the rule should succeed
    should_succeed: Bool,
  )
}

/// Result of applying a rule
pub type RuleApplicationResult {
  /// Rule successfully applied with derived conclusion
  Applied(conclusion: Proposition, bindings: Dict(String, Proposition))
  /// Rule did not match the given premises
  NoMatch(reason: String)
  /// Rule matched but failed to derive conclusion
  DerivationFailed(reason: String)
}

/// Variable bindings from pattern matching
pub type Bindings =
  Dict(String, Proposition)

/// Create empty bindings
pub fn empty_bindings() -> Bindings {
  dict.new()
}

/// Match a proposition against a pattern, returning bindings if successful
pub fn match_pattern(
  pattern: PropositionPattern,
  prop: Proposition,
  bindings: Bindings,
) -> Option(Bindings) {
  case pattern, prop {
    // Wildcard matches anything
    Wildcard(var), _ -> {
      case dict.get(bindings, var) {
        Ok(existing) -> {
          case propositions_equal(existing, prop) {
            True -> Some(bindings)
            False -> None
          }
        }
        Error(_) -> Some(dict.insert(bindings, var, prop))
      }
    }

    // AnyAtom matches any atom
    AnyAtom(var), Atom(name) -> {
      case dict.get(bindings, var) {
        Ok(existing) -> {
          case existing {
            Atom(existing_name) if existing_name == name -> Some(bindings)
            _ -> None
          }
        }
        Error(_) -> Some(dict.insert(bindings, var, Atom(name)))
      }
    }
    AnyAtom(_), _ -> None

    // SpecificAtom matches exact atom
    SpecificAtom(expected), Atom(actual) if expected == actual -> Some(bindings)
    SpecificAtom(_), _ -> None

    // Not pattern
    PatternNot(inner_pat), Not(inner_prop) ->
      match_pattern(inner_pat, inner_prop, bindings)
    PatternNot(_), _ -> None

    // And pattern
    PatternAnd(left_pat, right_pat), And(left_prop, right_prop) -> {
      case match_pattern(left_pat, left_prop, bindings) {
        Some(new_bindings) -> match_pattern(right_pat, right_prop, new_bindings)
        None -> None
      }
    }
    PatternAnd(_, _), _ -> None

    // Or pattern
    PatternOr(left_pat, right_pat), Or(left_prop, right_prop) -> {
      case match_pattern(left_pat, left_prop, bindings) {
        Some(new_bindings) -> match_pattern(right_pat, right_prop, new_bindings)
        None -> None
      }
    }
    PatternOr(_, _), _ -> None

    // Implies pattern
    PatternImplies(ante_pat, cons_pat), Implies(ante_prop, cons_prop) -> {
      case match_pattern(ante_pat, ante_prop, bindings) {
        Some(new_bindings) -> match_pattern(cons_pat, cons_prop, new_bindings)
        None -> None
      }
    }
    PatternImplies(_, _), _ -> None

    // Necessary pattern
    PatternNecessary(inner_pat), Necessary(inner_prop) ->
      match_pattern(inner_pat, inner_prop, bindings)
    PatternNecessary(_), _ -> None

    // Possible pattern
    PatternPossible(inner_pat), Possible(inner_prop) ->
      match_pattern(inner_pat, inner_prop, bindings)
    PatternPossible(_), _ -> None

    // Obligatory pattern
    PatternObligatory(inner_pat), Obligatory(inner_prop) ->
      match_pattern(inner_pat, inner_prop, bindings)
    PatternObligatory(_), _ -> None

    // Permitted pattern
    PatternPermitted(inner_pat), Permitted(inner_prop) ->
      match_pattern(inner_pat, inner_prop, bindings)
    PatternPermitted(_), _ -> None

    // Knows pattern
    PatternKnows(agent_var, inner_pat), Knows(agent, inner_prop) -> {
      let agent_bindings = dict.insert(bindings, agent_var, Atom(agent))
      match_pattern(inner_pat, inner_prop, agent_bindings)
    }
    PatternKnows(_, _), _ -> None

    // Believes pattern
    PatternBelieves(agent_var, inner_pat), Believes(agent, inner_prop) -> {
      let agent_bindings = dict.insert(bindings, agent_var, Atom(agent))
      match_pattern(inner_pat, inner_prop, agent_bindings)
    }
    PatternBelieves(_, _), _ -> None
  }
}

/// Check if two propositions are structurally equal
pub fn propositions_equal(a: Proposition, b: Proposition) -> Bool {
  case a, b {
    Atom(n1), Atom(n2) -> n1 == n2
    Not(p1), Not(p2) -> propositions_equal(p1, p2)
    And(l1, r1), And(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    Or(l1, r1), Or(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    Implies(l1, r1), Implies(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    Necessary(p1), Necessary(p2) -> propositions_equal(p1, p2)
    Possible(p1), Possible(p2) -> propositions_equal(p1, p2)
    Obligatory(p1), Obligatory(p2) -> propositions_equal(p1, p2)
    Permitted(p1), Permitted(p2) -> propositions_equal(p1, p2)
    Knows(a1, p1), Knows(a2, p2) -> a1 == a2 && propositions_equal(p1, p2)
    Believes(a1, p1), Believes(a2, p2) -> a1 == a2 && propositions_equal(p1, p2)
    _, _ -> False
  }
}

/// Substitute bindings into a pattern to create a proposition
pub fn substitute(
  pattern: PropositionPattern,
  bindings: Bindings,
) -> Option(Proposition) {
  case pattern {
    Wildcard(var) -> {
      case dict.get(bindings, var) {
        Ok(prop) -> Some(prop)
        Error(_) -> None
      }
    }

    AnyAtom(var) -> {
      case dict.get(bindings, var) {
        Ok(prop) -> Some(prop)
        Error(_) -> None
      }
    }

    SpecificAtom(name) -> Some(Atom(name))

    PatternNot(inner) -> {
      case substitute(inner, bindings) {
        Some(prop) -> Some(Not(prop))
        None -> None
      }
    }

    PatternAnd(left, right) -> {
      case substitute(left, bindings), substitute(right, bindings) {
        Some(l), Some(r) -> Some(And(l, r))
        _, _ -> None
      }
    }

    PatternOr(left, right) -> {
      case substitute(left, bindings), substitute(right, bindings) {
        Some(l), Some(r) -> Some(Or(l, r))
        _, _ -> None
      }
    }

    PatternImplies(ante, cons) -> {
      case substitute(ante, bindings), substitute(cons, bindings) {
        Some(a), Some(c) -> Some(Implies(a, c))
        _, _ -> None
      }
    }

    PatternNecessary(inner) -> {
      case substitute(inner, bindings) {
        Some(prop) -> Some(Necessary(prop))
        None -> None
      }
    }

    PatternPossible(inner) -> {
      case substitute(inner, bindings) {
        Some(prop) -> Some(Possible(prop))
        None -> None
      }
    }

    PatternObligatory(inner) -> {
      case substitute(inner, bindings) {
        Some(prop) -> Some(Obligatory(prop))
        None -> None
      }
    }

    PatternPermitted(inner) -> {
      case substitute(inner, bindings) {
        Some(prop) -> Some(Permitted(prop))
        None -> None
      }
    }

    PatternKnows(agent_var, inner) -> {
      case dict.get(bindings, agent_var), substitute(inner, bindings) {
        Ok(Atom(agent)), Some(prop) -> Some(Knows(agent, prop))
        _, _ -> None
      }
    }

    PatternBelieves(agent_var, inner) -> {
      case dict.get(bindings, agent_var), substitute(inner, bindings) {
        Ok(Atom(agent)), Some(prop) -> Some(Believes(agent, prop))
        _, _ -> None
      }
    }
  }
}

/// Apply an inference rule to a list of premises
pub fn apply_rule(
  rule: InferenceRule,
  premises: List(Proposition),
) -> RuleApplicationResult {
  // Check if number of premises matches (or rule has no premise patterns)
  case
    list.length(rule.premise_patterns) == list.length(premises)
    || rule.premise_patterns == []
  {
    False ->
      NoMatch(
        reason: "Expected "
        <> int_to_string(list.length(rule.premise_patterns))
        <> " premises, got "
        <> int_to_string(list.length(premises)),
      )
    True -> {
      // Try to match all premise patterns
      let match_result =
        match_all_patterns(rule.premise_patterns, premises, empty_bindings())

      case match_result {
        None ->
          NoMatch(
            reason: "Premises do not match rule patterns for " <> rule.name,
          )
        Some(bindings) -> {
          // Substitute bindings into conclusion pattern
          case substitute(rule.conclusion_pattern, bindings) {
            Some(conclusion) ->
              Applied(conclusion: conclusion, bindings: bindings)
            None ->
              DerivationFailed(
                reason: "Could not derive conclusion from bindings",
              )
          }
        }
      }
    }
  }
}

/// Match all patterns against all premises
fn match_all_patterns(
  patterns: List(PropositionPattern),
  premises: List(Proposition),
  bindings: Bindings,
) -> Option(Bindings) {
  case patterns, premises {
    [], [] -> Some(bindings)
    [pat, ..rest_pats], [prem, ..rest_prems] -> {
      case match_pattern(pat, prem, bindings) {
        Some(new_bindings) ->
          match_all_patterns(rest_pats, rest_prems, new_bindings)
        None -> None
      }
    }
    _, _ -> None
  }
}

/// Simple int to string for error messages
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

/// Check if rule is valid in a given logic system
pub fn is_valid_in(rule: InferenceRule, system: LogicSystem) -> Bool {
  list.contains(rule.valid_in, system)
}

/// Get all logic systems where rule is valid
pub fn valid_systems(rule: InferenceRule) -> List(LogicSystem) {
  rule.valid_in
}

/// Create default metadata
pub fn default_metadata() -> RuleMetadata {
  RuleMetadata(
    version: 1,
    created_at: None,
    updated_at: None,
    author: None,
    source: None,
    tags: [],
    examples: [],
  )
}
