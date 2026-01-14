//// Formula Pattern Library
////
//// Provides reusable formula patterns and auto-suggestion capabilities
//// for common modal logic formulas across different categories.

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Pattern category
pub type PatternCategory {
  Epistemic
  Deontic
  Temporal
  Alethic
  Classical
}

/// Complexity level
pub type ComplexityLevel {
  Simple
  Medium
  Complex
}

/// Formula pattern
pub type Pattern {
  Pattern(
    id: String,
    name: String,
    formula: String,
    description: String,
    category: PatternCategory,
    tags: List(String),
    complexity: ComplexityLevel,
    use_cases: List(String),
    related_patterns: List(String),
  )
}

/// Pattern suggestion with relevance score
pub type Suggestion {
  Suggestion(pattern: Pattern, relevance_score: Float, match_reason: String)
}

// =============================================================================
// Pattern Database
// =============================================================================

/// Get all available patterns
pub fn all_patterns() -> List(Pattern) {
  list.flatten([
    epistemic_patterns(),
    deontic_patterns(),
    temporal_patterns(),
    alethic_patterns(),
    classical_patterns(),
  ])
}

/// Epistemic logic patterns (knowledge and belief)
pub fn epistemic_patterns() -> List(Pattern) {
  [
    Pattern(
      id: "ep001",
      name: "Knowledge Implies Truth",
      formula: "Knows(agent, p) → p",
      description: "If an agent knows p, then p is true (truth axiom for knowledge)",
      category: Epistemic,
      tags: ["knowledge", "truth", "factivity"],
      complexity: Simple,
      use_cases: ["Epistemic logic", "Agent reasoning", "Knowledge bases"],
      related_patterns: ["ep002", "ep003"],
    ),
    Pattern(
      id: "ep002",
      name: "Positive Introspection",
      formula: "Knows(agent, p) → Knows(agent, Knows(agent, p))",
      description: "If an agent knows p, they know that they know p (positive introspection)",
      category: Epistemic,
      tags: ["knowledge", "introspection", "self-awareness"],
      complexity: Medium,
      use_cases: ["Epistemic logic", "Self-aware agents", "Metacognition"],
      related_patterns: ["ep001", "ep003"],
    ),
    Pattern(
      id: "ep003",
      name: "Negative Introspection",
      formula: "¬Knows(agent, p) → Knows(agent, ¬Knows(agent, p))",
      description: "If an agent doesn't know p, they know they don't know p",
      category: Epistemic,
      tags: ["knowledge", "introspection", "ignorance"],
      complexity: Medium,
      use_cases: ["Epistemic logic", "Awareness of ignorance"],
      related_patterns: ["ep001", "ep002"],
    ),
    Pattern(
      id: "ep004",
      name: "Common Knowledge",
      formula: "Knows(a1, p) ∧ Knows(a2, p) → Knows(a1, Knows(a2, p))",
      description: "If two agents know p, the first knows that the second knows p",
      category: Epistemic,
      tags: ["knowledge", "common-knowledge", "multi-agent"],
      complexity: Complex,
      use_cases: [
        "Multi-agent systems",
        "Distributed knowledge",
        "Coordination",
      ],
      related_patterns: ["ep001", "ep002"],
    ),
    Pattern(
      id: "ep005",
      name: "Belief Consistency",
      formula: "Believes(agent, p) ∧ Believes(agent, p → q) → Believes(agent, q)",
      description: "If an agent believes p and believes p implies q, they believe q",
      category: Epistemic,
      tags: ["belief", "consistency", "inference"],
      complexity: Medium,
      use_cases: ["Belief systems", "Reasoning under uncertainty"],
      related_patterns: ["ep006"],
    ),
    Pattern(
      id: "ep006",
      name: "Knowledge Implies Belief",
      formula: "Knows(agent, p) → Believes(agent, p)",
      description: "If an agent knows something, they believe it",
      category: Epistemic,
      tags: ["knowledge", "belief", "relationship"],
      complexity: Simple,
      use_cases: ["Epistemic logic", "Knowledge vs belief distinction"],
      related_patterns: ["ep001", "ep005"],
    ),
  ]
}

/// Deontic logic patterns (obligation and permission)
pub fn deontic_patterns() -> List(Pattern) {
  [
    Pattern(
      id: "dp001",
      name: "Obligation Implies Permission",
      formula: "Obligatory(p) → Permitted(p)",
      description: "If something is obligatory, it is permitted",
      category: Deontic,
      tags: ["obligation", "permission", "deontic"],
      complexity: Simple,
      use_cases: ["Moral reasoning", "Legal systems", "Normative ethics"],
      related_patterns: ["dp002", "dp003"],
    ),
    Pattern(
      id: "dp002",
      name: "No Contradictory Obligations",
      formula: "¬(Obligatory(p) ∧ Obligatory(¬p))",
      description: "It cannot be both obligatory to do p and obligatory not to do p",
      category: Deontic,
      tags: ["obligation", "consistency", "contradiction"],
      complexity: Simple,
      use_cases: ["Consistent normative systems", "Legal reasoning"],
      related_patterns: ["dp001", "dp003"],
    ),
    Pattern(
      id: "dp003",
      name: "Prohibition",
      formula: "Obligatory(¬p)",
      description: "It is obligatory that p does not hold (prohibition)",
      category: Deontic,
      tags: ["obligation", "prohibition", "negation"],
      complexity: Simple,
      use_cases: ["Moral rules", "Legal prohibitions"],
      related_patterns: ["dp001", "dp004"],
    ),
    Pattern(
      id: "dp004",
      name: "Permission",
      formula: "Permitted(p) ↔ ¬Obligatory(¬p)",
      description: "Something is permitted iff it's not obligatory not to do it",
      category: Deontic,
      tags: ["permission", "obligation", "duality"],
      complexity: Medium,
      use_cases: ["Deontic logic", "Permission systems"],
      related_patterns: ["dp001", "dp003"],
    ),
    Pattern(
      id: "dp005",
      name: "Ought Implies Can",
      formula: "Obligatory(p) → Possible(p)",
      description: "If something is obligatory, it must be possible",
      category: Deontic,
      tags: ["obligation", "possibility", "practical"],
      complexity: Medium,
      use_cases: ["Practical ethics", "Realistic obligations"],
      related_patterns: ["dp001"],
    ),
  ]
}

/// Temporal logic patterns
pub fn temporal_patterns() -> List(Pattern) {
  [
    Pattern(
      id: "tp001",
      name: "Always Implies Now",
      formula: "□p → p",
      description: "If p is always true, then p is true now (in system T)",
      category: Temporal,
      tags: ["always", "present", "reflexivity"],
      complexity: Simple,
      use_cases: ["Temporal reasoning", "System T"],
      related_patterns: ["tp002", "ap001"],
    ),
    Pattern(
      id: "tp002",
      name: "Eventually Consistent",
      formula: "◇p → ◇(p ∧ ◇p)",
      description: "If p is eventually true, it's eventually true and eventually true again",
      category: Temporal,
      tags: ["eventually", "consistency"],
      complexity: Medium,
      use_cases: ["Distributed systems", "Eventual consistency"],
      related_patterns: ["tp001"],
    ),
  ]
}

/// Alethic logic patterns (necessity and possibility)
pub fn alethic_patterns() -> List(Pattern) {
  [
    Pattern(
      id: "ap001",
      name: "Necessity Implies Truth",
      formula: "□p → p",
      description: "If p is necessary, then p is true (axiom T)",
      category: Alethic,
      tags: ["necessity", "truth", "reflexivity"],
      complexity: Simple,
      use_cases: ["System T", "Alethic modality", "Metaphysics"],
      related_patterns: ["ap002", "ap003"],
    ),
    Pattern(
      id: "ap002",
      name: "Necessity of Necessity",
      formula: "□p → □□p",
      description: "If p is necessary, then it's necessarily necessary (axiom 4)",
      category: Alethic,
      tags: ["necessity", "transitivity"],
      complexity: Medium,
      use_cases: ["System K4", "System S4", "Provability logic"],
      related_patterns: ["ap001", "ap003"],
    ),
    Pattern(
      id: "ap003",
      name: "Possibility Implies Necessary Possibility",
      formula: "◇p → □◇p",
      description: "If p is possible, then it's necessarily possible (axiom 5)",
      category: Alethic,
      tags: ["possibility", "necessity", "euclidean"],
      complexity: Medium,
      use_cases: ["System S5", "Metaphysical modality"],
      related_patterns: ["ap001", "ap002"],
    ),
    Pattern(
      id: "ap004",
      name: "Duality of Necessity and Possibility",
      formula: "□p ↔ ¬◇¬p",
      description: "p is necessary iff it's not possible that not-p",
      category: Alethic,
      tags: ["necessity", "possibility", "duality", "equivalence"],
      complexity: Medium,
      use_cases: ["All modal systems", "Modal duality"],
      related_patterns: ["ap005"],
    ),
    Pattern(
      id: "ap005",
      name: "Possibility as Dual of Necessity",
      formula: "◇p ↔ ¬□¬p",
      description: "p is possible iff it's not necessary that not-p",
      category: Alethic,
      tags: ["possibility", "necessity", "duality", "equivalence"],
      complexity: Medium,
      use_cases: ["All modal systems", "Modal duality"],
      related_patterns: ["ap004"],
    ),
  ]
}

/// Classical logic patterns
pub fn classical_patterns() -> List(Pattern) {
  [
    Pattern(
      id: "cp001",
      name: "Modus Ponens",
      formula: "p ∧ (p → q) → q",
      description: "If p is true and p implies q, then q is true",
      category: Classical,
      tags: ["implication", "inference", "classical"],
      complexity: Simple,
      use_cases: ["All logic systems", "Basic reasoning"],
      related_patterns: ["cp002", "cp003"],
    ),
    Pattern(
      id: "cp002",
      name: "Modus Tollens",
      formula: "¬q ∧ (p → q) → ¬p",
      description: "If q is false and p implies q, then p is false",
      category: Classical,
      tags: ["implication", "negation", "inference"],
      complexity: Simple,
      use_cases: ["All logic systems", "Proof by contrapositive"],
      related_patterns: ["cp001", "cp003"],
    ),
    Pattern(
      id: "cp003",
      name: "Hypothetical Syllogism",
      formula: "(p → q) ∧ (q → r) → (p → r)",
      description: "If p implies q and q implies r, then p implies r",
      category: Classical,
      tags: ["implication", "transitivity", "chaining"],
      complexity: Simple,
      use_cases: ["All logic systems", "Chain reasoning"],
      related_patterns: ["cp001", "cp002"],
    ),
    Pattern(
      id: "cp004",
      name: "Disjunctive Syllogism",
      formula: "(p ∨ q) ∧ ¬p → q",
      description: "If p or q is true, and p is false, then q is true",
      category: Classical,
      tags: ["disjunction", "negation", "elimination"],
      complexity: Simple,
      use_cases: ["All logic systems", "Case analysis"],
      related_patterns: ["cp001"],
    ),
    Pattern(
      id: "cp005",
      name: "De Morgan's Law (Conjunction)",
      formula: "¬(p ∧ q) ↔ (¬p ∨ ¬q)",
      description: "Not (p and q) is equivalent to (not-p or not-q)",
      category: Classical,
      tags: ["negation", "conjunction", "disjunction", "equivalence"],
      complexity: Simple,
      use_cases: ["All logic systems", "Negation normal form"],
      related_patterns: ["cp006"],
    ),
    Pattern(
      id: "cp006",
      name: "De Morgan's Law (Disjunction)",
      formula: "¬(p ∨ q) ↔ (¬p ∧ ¬q)",
      description: "Not (p or q) is equivalent to (not-p and not-q)",
      category: Classical,
      tags: ["negation", "conjunction", "disjunction", "equivalence"],
      complexity: Simple,
      use_cases: ["All logic systems", "Negation normal form"],
      related_patterns: ["cp005"],
    ),
    Pattern(
      id: "cp007",
      name: "Law of Excluded Middle",
      formula: "p ∨ ¬p",
      description: "Either p is true or not-p is true (no middle ground)",
      category: Classical,
      tags: ["disjunction", "negation", "tautology"],
      complexity: Simple,
      use_cases: ["Classical logic", "Proof by cases"],
      related_patterns: [],
    ),
  ]
}

// =============================================================================
// Pattern Search and Filtering
// =============================================================================

/// Get patterns by category
pub fn by_category(category: PatternCategory) -> List(Pattern) {
  all_patterns()
  |> list.filter(fn(p) { p.category == category })
}

/// Get patterns by tag
pub fn by_tag(tag: String) -> List(Pattern) {
  all_patterns()
  |> list.filter(fn(p) { list.contains(p.tags, tag) })
}

/// Get patterns by complexity
pub fn by_complexity(complexity: ComplexityLevel) -> List(Pattern) {
  all_patterns()
  |> list.filter(fn(p) { p.complexity == complexity })
}

/// Get pattern by ID
pub fn by_id(id: String) -> Option(Pattern) {
  all_patterns()
  |> list.find(fn(p) { p.id == id })
  |> option.from_result
}

/// Search patterns by name or description
pub fn search(query: String) -> List(Pattern) {
  let lower_query = string.lowercase(query)

  all_patterns()
  |> list.filter(fn(p) {
    string.contains(string.lowercase(p.name), lower_query)
    || string.contains(string.lowercase(p.description), lower_query)
  })
}

// =============================================================================
// Auto-Suggestion Engine
// =============================================================================

/// Suggest patterns based on partial formula
pub fn suggest(
  partial_formula: String,
  max_suggestions: Int,
) -> List(Suggestion) {
  let normalized = normalize_formula(partial_formula)

  all_patterns()
  |> list.map(fn(pattern) {
    let score = calculate_relevance(normalized, pattern)
    let reason = match_reason(normalized, pattern)
    Suggestion(pattern: pattern, relevance_score: score, match_reason: reason)
  })
  |> list.filter(fn(s) { s.relevance_score >. 0.0 })
  |> list.sort(fn(a, b) { float.compare(b.relevance_score, a.relevance_score) })
  |> list.take(max_suggestions)
}

/// Suggest patterns by category
pub fn suggest_by_category(
  category: PatternCategory,
  partial_formula: String,
  max_suggestions: Int,
) -> List(Suggestion) {
  let patterns = by_category(category)
  let normalized = normalize_formula(partial_formula)

  patterns
  |> list.map(fn(pattern) {
    let score = calculate_relevance(normalized, pattern)
    let reason = match_reason(normalized, pattern)
    Suggestion(pattern: pattern, relevance_score: score, match_reason: reason)
  })
  |> list.filter(fn(s) { s.relevance_score >. 0.0 })
  |> list.sort(fn(a, b) { float.compare(b.relevance_score, a.relevance_score) })
  |> list.take(max_suggestions)
}

// =============================================================================
// Pattern Matching Algorithms
// =============================================================================

/// Normalize formula for comparison
fn normalize_formula(formula: String) -> String {
  formula
  |> string.lowercase
  |> string.trim
  |> string.replace("→", "->")
  |> string.replace("∧", "/\\")
  |> string.replace("∨", "\\/")
  |> string.replace("¬", "~")
  |> string.replace("□", "box")
  |> string.replace("◇", "diamond")
  |> string.replace("↔", "<->")
}

/// Calculate relevance score between partial formula and pattern
fn calculate_relevance(normalized_partial: String, pattern: Pattern) -> Float {
  let normalized_pattern = normalize_formula(pattern.formula)

  // Exact match
  case normalized_partial == normalized_pattern {
    True -> 1.0
    False -> {
      // Substring match
      let substring_score = case
        string.contains(normalized_pattern, normalized_partial)
      {
        True -> 0.8
        False -> 0.0
      }

      // Keyword overlap
      let keyword_score = keyword_overlap_score(normalized_partial, pattern)

      // Tag match
      let tag_score = tag_match_score(normalized_partial, pattern)

      // Take maximum score
      float.max(substring_score, float.max(keyword_score, tag_score))
    }
  }
}

/// Calculate keyword overlap score
fn keyword_overlap_score(normalized_partial: String, pattern: Pattern) -> Float {
  let keywords = [
    "knows",
    "believes",
    "obligatory",
    "permitted",
    "box",
    "diamond",
  ]

  let partial_keywords =
    keywords
    |> list.filter(fn(kw) { string.contains(normalized_partial, kw) })

  let pattern_keywords =
    keywords
    |> list.filter(fn(kw) {
      string.contains(normalize_formula(pattern.formula), kw)
    })

  case list.length(partial_keywords) {
    0 -> 0.0
    partial_count -> {
      let overlap =
        partial_keywords
        |> list.filter(fn(kw) { list.contains(pattern_keywords, kw) })
        |> list.length

      int.to_float(overlap) /. int.to_float(partial_count) *. 0.6
    }
  }
}

/// Calculate tag match score
fn tag_match_score(normalized_partial: String, pattern: Pattern) -> Float {
  let matching_tags =
    pattern.tags
    |> list.filter(fn(tag) { string.contains(normalized_partial, tag) })
    |> list.length

  case matching_tags {
    0 -> 0.0
    count ->
      int.to_float(count) /. int.to_float(list.length(pattern.tags)) *. 0.5
  }
}

/// Determine match reason for suggestion
fn match_reason(normalized_partial: String, pattern: Pattern) -> String {
  let normalized_pattern = normalize_formula(pattern.formula)

  case normalized_partial == normalized_pattern {
    True -> "Exact match"
    False ->
      case string.contains(normalized_pattern, normalized_partial) {
        True -> "Contains your input"
        False -> {
          // Check for keyword matches
          let keywords = ["knows", "believes", "obligatory", "permitted"]
          let matching =
            keywords
            |> list.filter(fn(kw) { string.contains(normalized_partial, kw) })

          case list.length(matching) > 0 {
            True -> "Similar operators"
            False -> "Related pattern"
          }
        }
      }
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get count of patterns by category
pub fn count_by_category() -> List(#(PatternCategory, Int)) {
  [
    #(Epistemic, list.length(epistemic_patterns())),
    #(Deontic, list.length(deontic_patterns())),
    #(Temporal, list.length(temporal_patterns())),
    #(Alethic, list.length(alethic_patterns())),
    #(Classical, list.length(classical_patterns())),
  ]
}

/// Get total pattern count
pub fn count() -> Int {
  list.length(all_patterns())
}

/// Convert category to string
pub fn category_to_string(category: PatternCategory) -> String {
  case category {
    Epistemic -> "epistemic"
    Deontic -> "deontic"
    Temporal -> "temporal"
    Alethic -> "alethic"
    Classical -> "classical"
  }
}

/// Parse category from string
pub fn string_to_category(s: String) -> Option(PatternCategory) {
  case string.lowercase(s) {
    "epistemic" -> Some(Epistemic)
    "deontic" -> Some(Deontic)
    "temporal" -> Some(Temporal)
    "alethic" -> Some(Alethic)
    "classical" -> Some(Classical)
    _ -> None
  }
}

/// Convert complexity to string
pub fn complexity_to_string(complexity: ComplexityLevel) -> String {
  case complexity {
    Simple -> "simple"
    Medium -> "medium"
    Complex -> "complex"
  }
}
