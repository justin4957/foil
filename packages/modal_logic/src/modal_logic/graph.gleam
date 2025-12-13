//// Argument Graph Queries
////
//// This module provides graph-oriented queries for analyzing relationships
//// between arguments and formalizations.
////
//// ## Features
////
//// - **Formalization Comparison**: Compare structural similarity of formalizations
//// - **Cross-Argument Analysis**: Find arguments with shared premises or conclusions
//// - **Inference Pattern Tracking**: Identify and track common reasoning patterns
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/graph
////
//// let similar = graph.find_similar_formalizations(f1, formalizations, 0.7)
//// let related = graph.find_related_arguments(arg, arguments)
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import modal_logic/argument.{type Argument, type Formalization}
import modal_logic/proposition.{type LogicSystem, type Proposition}

// =============================================================================
// Types
// =============================================================================

/// Relationship between two arguments
pub type ArgumentRelationship {
  /// First argument supports the second
  Supports
  /// Arguments contradict each other
  Contradicts
  /// First argument specializes (is more specific than) the second
  Specializes
  /// First argument generalizes (is more general than) the second
  Generalizes
  /// Arguments share similar logical structure
  SimilarStructure
  /// Arguments share one or more premises
  SharedPremise
  /// Arguments share the same conclusion
  SharedConclusion
}

/// A relationship edge between two arguments
pub type ArgumentEdge {
  ArgumentEdge(
    source_id: String,
    target_id: String,
    relationship: ArgumentRelationship,
    strength: Float,
    metadata: Dict(String, String),
  )
}

/// A node in the argument graph
pub type ArgumentNode {
  ArgumentNode(
    argument_id: String,
    formalization_count: Int,
    validation_status: Option(String),
    centrality_score: Float,
  )
}

/// An argument graph for analysis
pub type ArgumentGraph {
  ArgumentGraph(
    nodes: Dict(String, ArgumentNode),
    edges: List(ArgumentEdge),
    metadata: Dict(String, String),
  )
}

/// Similarity result between two formalizations
pub type SimilarityResult {
  SimilarityResult(
    formalization_id: String,
    similarity_score: Float,
    matching_premises: Int,
    matching_conclusion: Bool,
    same_logic_system: Bool,
  )
}

/// An inference pattern extracted from formalizations
pub type InferencePattern {
  InferencePattern(
    id: String,
    name: String,
    description: String,
    pattern_structure: PatternStructure,
    logic_systems: List(LogicSystem),
    example_count: Int,
  )
}

/// Structure of an inference pattern
pub type PatternStructure {
  PatternStructure(
    premise_patterns: List(PropositionPattern),
    conclusion_pattern: PropositionPattern,
  )
}

/// A pattern for matching propositions
pub type PropositionPattern {
  /// Match any atom
  AnyAtom
  /// Match specific atom by name
  SpecificAtom(name: String)
  /// Match negation of pattern
  NegationOf(PropositionPattern)
  /// Match conjunction of patterns
  ConjunctionOf(PropositionPattern, PropositionPattern)
  /// Match disjunction of patterns
  DisjunctionOf(PropositionPattern, PropositionPattern)
  /// Match implication of patterns
  ImplicationOf(PropositionPattern, PropositionPattern)
  /// Match necessity of pattern
  NecessityOf(PropositionPattern)
  /// Match possibility of pattern
  PossibilityOf(PropositionPattern)
  /// Match any proposition (wildcard)
  AnyProposition
}

/// Query result for cross-argument analysis
pub type CrossArgumentResult {
  CrossArgumentResult(
    argument_id: String,
    relationship: ArgumentRelationship,
    shared_elements: List(String),
    confidence: Float,
  )
}

// =============================================================================
// Formalization Comparison
// =============================================================================

/// Calculate similarity score between two formalizations
pub fn formalization_similarity(f1: Formalization, f2: Formalization) -> Float {
  let logic_score = case f1.logic_system == f2.logic_system {
    True -> 0.3
    False -> 0.0
  }

  let premise_score = premise_similarity(f1.premises, f2.premises) *. 0.4
  let conclusion_score =
    proposition_similarity(f1.conclusion, f2.conclusion) *. 0.3

  logic_score +. premise_score +. conclusion_score
}

/// Find similar formalizations above a threshold
pub fn find_similar_formalizations(
  target: Formalization,
  candidates: List(Formalization),
  threshold: Float,
) -> List(SimilarityResult) {
  candidates
  |> list.filter(fn(f) { f.id != target.id })
  |> list.map(fn(f) {
    let score = formalization_similarity(target, f)
    let matching = count_matching_premises(target.premises, f.premises)
    let same_conclusion =
      proposition_similarity(target.conclusion, f.conclusion) >. 0.9
    SimilarityResult(
      formalization_id: f.id,
      similarity_score: score,
      matching_premises: matching,
      matching_conclusion: same_conclusion,
      same_logic_system: target.logic_system == f.logic_system,
    )
  })
  |> list.filter(fn(r) { r.similarity_score >=. threshold })
  |> list.sort(fn(a, b) {
    float.compare(b.similarity_score, a.similarity_score)
  })
}

/// Compare two sets of premises
pub fn premise_similarity(
  premises1: List(Proposition),
  premises2: List(Proposition),
) -> Float {
  let set1 = proposition_set(premises1)
  let set2 = proposition_set(premises2)

  let intersection_size = set.size(set.intersection(set1, set2))
  let union_size = set.size(set.union(set1, set2))

  case union_size {
    0 -> 1.0
    // Both empty
    _ -> int_to_float(intersection_size) /. int_to_float(union_size)
  }
}

/// Calculate similarity between two propositions
pub fn proposition_similarity(p1: Proposition, p2: Proposition) -> Float {
  case p1, p2 {
    proposition.Atom(n1), proposition.Atom(n2) ->
      case n1 == n2 {
        True -> 1.0
        False -> 0.0
      }

    proposition.Not(inner1), proposition.Not(inner2) ->
      proposition_similarity(inner1, inner2)

    proposition.And(l1, r1), proposition.And(l2, r2) -> {
      let score1 =
        { proposition_similarity(l1, l2) +. proposition_similarity(r1, r2) }
        /. 2.0
      let score2 =
        { proposition_similarity(l1, r2) +. proposition_similarity(r1, l2) }
        /. 2.0
      float_max(score1, score2)
    }

    proposition.Or(l1, r1), proposition.Or(l2, r2) -> {
      let score1 =
        { proposition_similarity(l1, l2) +. proposition_similarity(r1, r2) }
        /. 2.0
      let score2 =
        { proposition_similarity(l1, r2) +. proposition_similarity(r1, l2) }
        /. 2.0
      float_max(score1, score2)
    }

    proposition.Implies(a1, c1), proposition.Implies(a2, c2) ->
      { proposition_similarity(a1, a2) +. proposition_similarity(c1, c2) }
      /. 2.0

    proposition.Necessary(i1), proposition.Necessary(i2) ->
      proposition_similarity(i1, i2)

    proposition.Possible(i1), proposition.Possible(i2) ->
      proposition_similarity(i1, i2)

    proposition.Obligatory(i1), proposition.Obligatory(i2) ->
      proposition_similarity(i1, i2)

    proposition.Permitted(i1), proposition.Permitted(i2) ->
      proposition_similarity(i1, i2)

    proposition.Knows(ag1, i1), proposition.Knows(ag2, i2) ->
      case ag1 == ag2 {
        True -> proposition_similarity(i1, i2)
        False -> proposition_similarity(i1, i2) *. 0.5
      }

    proposition.Believes(ag1, i1), proposition.Believes(ag2, i2) ->
      case ag1 == ag2 {
        True -> proposition_similarity(i1, i2)
        False -> proposition_similarity(i1, i2) *. 0.5
      }

    _, _ -> 0.0
  }
}

// =============================================================================
// Cross-Argument Analysis
// =============================================================================

/// Find arguments related to the target
pub fn find_related_arguments(
  target: Argument,
  candidates: List(Argument),
) -> List(CrossArgumentResult) {
  candidates
  |> list.filter(fn(a) { a.id != target.id })
  |> list.flat_map(fn(candidate) { analyze_relationship(target, candidate) })
  |> list.sort(fn(a, b) { float.compare(b.confidence, a.confidence) })
}

/// Analyze relationship between two arguments
pub fn analyze_relationship(
  arg1: Argument,
  arg2: Argument,
) -> List(CrossArgumentResult) {
  let results = []

  // Check for shared premises
  let shared_premises = find_shared_premises(arg1, arg2)
  let results = case shared_premises {
    [] -> results
    premises -> [
      CrossArgumentResult(
        argument_id: arg2.id,
        relationship: SharedPremise,
        shared_elements: premises,
        confidence: int_to_float(list.length(premises))
          /. int_to_float(max_premise_count(arg1, arg2)),
      ),
      ..results
    ]
  }

  // Check for shared conclusions
  let shared_conclusions = find_shared_conclusions(arg1, arg2)
  let results = case shared_conclusions {
    [] -> results
    conclusions -> [
      CrossArgumentResult(
        argument_id: arg2.id,
        relationship: SharedConclusion,
        shared_elements: conclusions,
        confidence: 0.8,
      ),
      ..results
    ]
  }

  // Check structural similarity
  let structural_score = structural_similarity(arg1, arg2)
  let results = case structural_score >. 0.5 {
    True -> [
      CrossArgumentResult(
        argument_id: arg2.id,
        relationship: SimilarStructure,
        shared_elements: [],
        confidence: structural_score,
      ),
      ..results
    ]
    False -> results
  }

  results
}

/// Find premises shared between two arguments
pub fn find_shared_premises(arg1: Argument, arg2: Argument) -> List(String) {
  let premises1 = extract_all_premises(arg1)
  let premises2 = extract_all_premises(arg2)

  premises1
  |> list.filter(fn(p1) {
    list.any(premises2, fn(p2) { proposition_similarity(p1, p2) >. 0.9 })
  })
  |> list.map(proposition_to_string)
}

/// Find conclusions shared between two arguments
pub fn find_shared_conclusions(arg1: Argument, arg2: Argument) -> List(String) {
  let conclusions1 = extract_all_conclusions(arg1)
  let conclusions2 = extract_all_conclusions(arg2)

  conclusions1
  |> list.filter(fn(c1) {
    list.any(conclusions2, fn(c2) { proposition_similarity(c1, c2) >. 0.9 })
  })
  |> list.map(proposition_to_string)
}

/// Calculate structural similarity between arguments
pub fn structural_similarity(arg1: Argument, arg2: Argument) -> Float {
  case arg1.formalizations, arg2.formalizations {
    [], _ -> 0.0
    _, [] -> 0.0
    f1s, f2s -> {
      let similarities =
        list.flat_map(f1s, fn(f1) {
          list.map(f2s, fn(f2) { formalization_similarity(f1, f2) })
        })
      case similarities {
        [] -> 0.0
        sims -> list_max(sims)
      }
    }
  }
}

// =============================================================================
// Inference Pattern Tracking
// =============================================================================

/// Extract inference pattern from a formalization
pub fn extract_pattern(formalization: Formalization) -> PatternStructure {
  PatternStructure(
    premise_patterns: list.map(formalization.premises, proposition_to_pattern),
    conclusion_pattern: proposition_to_pattern(formalization.conclusion),
  )
}

/// Convert a proposition to a pattern
pub fn proposition_to_pattern(prop: Proposition) -> PropositionPattern {
  case prop {
    proposition.Atom(name) -> SpecificAtom(name)
    proposition.Not(inner) -> NegationOf(proposition_to_pattern(inner))
    proposition.And(l, r) ->
      ConjunctionOf(proposition_to_pattern(l), proposition_to_pattern(r))
    proposition.Or(l, r) ->
      DisjunctionOf(proposition_to_pattern(l), proposition_to_pattern(r))
    proposition.Implies(a, c) ->
      ImplicationOf(proposition_to_pattern(a), proposition_to_pattern(c))
    proposition.Necessary(inner) -> NecessityOf(proposition_to_pattern(inner))
    proposition.Possible(inner) -> PossibilityOf(proposition_to_pattern(inner))
    proposition.Obligatory(inner) -> proposition_to_pattern(inner)
    // Simplify for pattern matching
    proposition.Permitted(inner) -> proposition_to_pattern(inner)
    proposition.Knows(_, inner) -> proposition_to_pattern(inner)
    proposition.Believes(_, inner) -> proposition_to_pattern(inner)
  }
}

/// Generalize a pattern by replacing specific atoms with wildcards
pub fn generalize_pattern(pattern: PatternStructure) -> PatternStructure {
  PatternStructure(
    premise_patterns: list.map(
      pattern.premise_patterns,
      generalize_prop_pattern,
    ),
    conclusion_pattern: generalize_prop_pattern(pattern.conclusion_pattern),
  )
}

fn generalize_prop_pattern(pattern: PropositionPattern) -> PropositionPattern {
  case pattern {
    SpecificAtom(_) -> AnyAtom
    AnyAtom -> AnyAtom
    AnyProposition -> AnyProposition
    NegationOf(inner) -> NegationOf(generalize_prop_pattern(inner))
    ConjunctionOf(l, r) ->
      ConjunctionOf(generalize_prop_pattern(l), generalize_prop_pattern(r))
    DisjunctionOf(l, r) ->
      DisjunctionOf(generalize_prop_pattern(l), generalize_prop_pattern(r))
    ImplicationOf(a, c) ->
      ImplicationOf(generalize_prop_pattern(a), generalize_prop_pattern(c))
    NecessityOf(inner) -> NecessityOf(generalize_prop_pattern(inner))
    PossibilityOf(inner) -> PossibilityOf(generalize_prop_pattern(inner))
  }
}

/// Check if a formalization matches a pattern
pub fn matches_pattern(
  formalization: Formalization,
  pattern: PatternStructure,
) -> Bool {
  let premise_match =
    list.length(formalization.premises) == list.length(pattern.premise_patterns)
    && list.all(
      list.zip(formalization.premises, pattern.premise_patterns),
      fn(pair) {
        let #(prop, pat) = pair
        proposition_matches_pattern(prop, pat)
      },
    )

  let conclusion_match =
    proposition_matches_pattern(
      formalization.conclusion,
      pattern.conclusion_pattern,
    )

  premise_match && conclusion_match
}

/// Check if a proposition matches a pattern
pub fn proposition_matches_pattern(
  prop: Proposition,
  pattern: PropositionPattern,
) -> Bool {
  case pattern {
    AnyProposition -> True
    AnyAtom ->
      case prop {
        proposition.Atom(_) -> True
        _ -> False
      }
    SpecificAtom(name) ->
      case prop {
        proposition.Atom(n) -> n == name
        _ -> False
      }
    NegationOf(inner) ->
      case prop {
        proposition.Not(p) -> proposition_matches_pattern(p, inner)
        _ -> False
      }
    ConjunctionOf(l, r) ->
      case prop {
        proposition.And(pl, pr) ->
          {
            proposition_matches_pattern(pl, l)
            && proposition_matches_pattern(pr, r)
          }
          || {
            proposition_matches_pattern(pl, r)
            && proposition_matches_pattern(pr, l)
          }
        _ -> False
      }
    DisjunctionOf(l, r) ->
      case prop {
        proposition.Or(pl, pr) ->
          {
            proposition_matches_pattern(pl, l)
            && proposition_matches_pattern(pr, r)
          }
          || {
            proposition_matches_pattern(pl, r)
            && proposition_matches_pattern(pr, l)
          }
        _ -> False
      }
    ImplicationOf(a, c) ->
      case prop {
        proposition.Implies(pa, pc) ->
          proposition_matches_pattern(pa, a)
          && proposition_matches_pattern(pc, c)
        _ -> False
      }
    NecessityOf(inner) ->
      case prop {
        proposition.Necessary(p) -> proposition_matches_pattern(p, inner)
        _ -> False
      }
    PossibilityOf(inner) ->
      case prop {
        proposition.Possible(p) -> proposition_matches_pattern(p, inner)
        _ -> False
      }
  }
}

/// Identify common patterns in a set of formalizations
pub fn identify_common_patterns(
  formalizations: List(Formalization),
) -> List(#(PatternStructure, Int)) {
  let patterns =
    formalizations
    |> list.map(fn(f) { generalize_pattern(extract_pattern(f)) })

  // Count occurrences of each pattern, storing #(pattern, count)
  let pattern_counts: Dict(String, #(PatternStructure, Int)) =
    patterns
    |> list.fold(dict.new(), fn(counts, pattern) {
      let key = pattern_to_string(pattern)
      let current_count = case dict.get(counts, key) {
        Ok(#(_, c)) -> c
        Error(_) -> 0
      }
      dict.insert(counts, key, #(pattern, current_count + 1))
    })

  // Filter to patterns that appear more than once and sort by count
  pattern_counts
  |> dict.values
  |> list.filter(fn(pair: #(PatternStructure, Int)) { pair.1 > 1 })
  |> list.sort(fn(a: #(PatternStructure, Int), b: #(PatternStructure, Int)) {
    int.compare(b.1, a.1)
  })
}

// =============================================================================
// Graph Construction
// =============================================================================

/// Build an argument graph from a list of arguments
pub fn build_graph(arguments: List(Argument)) -> ArgumentGraph {
  let nodes =
    arguments
    |> list.map(fn(arg) {
      let status = get_validation_status(arg)
      let node =
        ArgumentNode(
          argument_id: arg.id,
          formalization_count: list.length(arg.formalizations),
          validation_status: status,
          centrality_score: 0.0,
        )
      #(arg.id, node)
    })
    |> dict.from_list

  let edges =
    arguments
    |> list.flat_map(fn(arg1) {
      list.flat_map(arguments, fn(arg2) {
        case arg1.id != arg2.id {
          True -> build_edges(arg1, arg2)
          False -> []
        }
      })
    })
    |> deduplicate_edges

  ArgumentGraph(nodes: nodes, edges: edges, metadata: dict.new())
}

/// Build edges between two arguments
fn build_edges(arg1: Argument, arg2: Argument) -> List(ArgumentEdge) {
  let relationships = analyze_relationship(arg1, arg2)

  relationships
  |> list.map(fn(r) {
    ArgumentEdge(
      source_id: arg1.id,
      target_id: r.argument_id,
      relationship: r.relationship,
      strength: r.confidence,
      metadata: dict.new(),
    )
  })
}

/// Remove duplicate edges (keep the one with higher strength)
fn deduplicate_edges(edges: List(ArgumentEdge)) -> List(ArgumentEdge) {
  let edge_map: Dict(String, ArgumentEdge) =
    list.fold(edges, dict.new(), fn(acc, edge) {
      let key =
        edge.source_id
        <> "->"
        <> edge.target_id
        <> ":"
        <> relationship_to_string(edge.relationship)
      let existing = dict.get(acc, key)
      case existing {
        Ok(ArgumentEdge(strength: existing_strength, ..)) ->
          case edge.strength >. existing_strength {
            True -> dict.insert(acc, key, edge)
            False -> acc
          }
        Error(_) -> dict.insert(acc, key, edge)
      }
    })
  dict.values(edge_map)
}

/// Calculate centrality scores for all nodes
pub fn calculate_centrality(graph: ArgumentGraph) -> ArgumentGraph {
  let edge_counts =
    graph.edges
    |> list.fold(dict.new(), fn(acc, edge) {
      let source_count = result.unwrap(dict.get(acc, edge.source_id), 0)
      let target_count = result.unwrap(dict.get(acc, edge.target_id), 0)
      acc
      |> dict.insert(edge.source_id, source_count + 1)
      |> dict.insert(edge.target_id, target_count + 1)
    })

  let max_count =
    edge_counts
    |> dict.values
    |> list.fold(1, int.max)

  let updated_nodes =
    graph.nodes
    |> dict.map_values(fn(id, node) {
      let count = result.unwrap(dict.get(edge_counts, id), 0)
      let score = int_to_float(count) /. int_to_float(max_count)
      ArgumentNode(..node, centrality_score: score)
    })

  ArgumentGraph(..graph, nodes: updated_nodes)
}

/// Get nodes with highest centrality
pub fn get_central_nodes(graph: ArgumentGraph, count: Int) -> List(ArgumentNode) {
  graph.nodes
  |> dict.values
  |> list.sort(fn(a, b) {
    float.compare(b.centrality_score, a.centrality_score)
  })
  |> list.take(count)
}

/// Get all edges for a node
pub fn get_node_edges(
  graph: ArgumentGraph,
  node_id: String,
) -> List(ArgumentEdge) {
  graph.edges
  |> list.filter(fn(e) { e.source_id == node_id || e.target_id == node_id })
}

// =============================================================================
// SQL Query Builders for Graph Operations
// =============================================================================

/// Build SQL to find similar formalizations
pub fn build_similar_formalizations_sql() -> String {
  "SELECT * FROM find_similar_formalizations($1, $2)"
}

/// Build SQL to get argument relationships
pub fn build_argument_relationships_sql() -> String {
  "SELECT ar.*,
          a1.natural_language as source_text,
          a2.natural_language as target_text
   FROM argument_relationships ar
   JOIN arguments a1 ON ar.source_argument_id = a1.id
   JOIN arguments a2 ON ar.target_argument_id = a2.id
   WHERE ar.source_argument_id = $1 OR ar.target_argument_id = $1
   ORDER BY ar.created_at DESC"
}

/// Build SQL to insert argument relationship
pub fn build_insert_relationship_sql() -> String {
  "INSERT INTO argument_relationships (source_argument_id, target_argument_id, relationship_type, metadata)
   VALUES ($1, $2, $3, $4::jsonb)
   ON CONFLICT (source_argument_id, target_argument_id, relationship_type)
   DO UPDATE SET metadata = $4::jsonb
   RETURNING id"
}

/// Build SQL to get inference patterns
pub fn build_inference_patterns_sql() -> String {
  "SELECT ip.*, COUNT(fp.formalization_id) as usage_count
   FROM inference_patterns ip
   LEFT JOIN formalization_patterns fp ON ip.id = fp.pattern_id
   GROUP BY ip.id
   ORDER BY usage_count DESC"
}

/// Build SQL to find formalizations by pattern
pub fn build_formalizations_by_pattern_sql() -> String {
  "SELECT f.*
   FROM formalizations f
   JOIN formalization_patterns fp ON f.id = fp.formalization_id
   WHERE fp.pattern_id = $1
   ORDER BY fp.match_confidence DESC"
}

// =============================================================================
// Internal Helpers
// =============================================================================

fn proposition_set(props: List(Proposition)) -> Set(String) {
  props
  |> list.map(proposition_to_string)
  |> set.from_list
}

fn count_matching_premises(
  premises1: List(Proposition),
  premises2: List(Proposition),
) -> Int {
  premises1
  |> list.filter(fn(p1) {
    list.any(premises2, fn(p2) { proposition_similarity(p1, p2) >. 0.9 })
  })
  |> list.length
}

fn extract_all_premises(arg: Argument) -> List(Proposition) {
  arg.formalizations
  |> list.flat_map(fn(f) { f.premises })
}

fn extract_all_conclusions(arg: Argument) -> List(Proposition) {
  arg.formalizations
  |> list.map(fn(f) { f.conclusion })
}

fn max_premise_count(arg1: Argument, arg2: Argument) -> Int {
  let count1 =
    arg1.formalizations
    |> list.map(fn(f) { list.length(f.premises) })
    |> list.fold(0, int.max)
  let count2 =
    arg2.formalizations
    |> list.map(fn(f) { list.length(f.premises) })
    |> list.fold(0, int.max)
  int.max(count1, int.max(count2, 1))
}

fn get_validation_status(arg: Argument) -> Option(String) {
  arg.formalizations
  |> list.filter_map(fn(f) { option.to_result(f.validation, Nil) })
  |> list.first
  |> result.map(fn(v) {
    case v {
      argument.Valid -> "valid"
      argument.Invalid(_) -> "invalid"
      argument.Timeout -> "timeout"
      argument.Error(_) -> "error"
    }
  })
  |> option.from_result
}

fn relationship_to_string(rel: ArgumentRelationship) -> String {
  case rel {
    Supports -> "supports"
    Contradicts -> "contradicts"
    Specializes -> "specializes"
    Generalizes -> "generalizes"
    SimilarStructure -> "similar_structure"
    SharedPremise -> "shared_premise"
    SharedConclusion -> "shared_conclusion"
  }
}

fn proposition_to_string(prop: Proposition) -> String {
  case prop {
    proposition.Atom(name) -> name
    proposition.Not(inner) -> "~" <> proposition_to_string(inner)
    proposition.And(l, r) ->
      "("
      <> proposition_to_string(l)
      <> " & "
      <> proposition_to_string(r)
      <> ")"
    proposition.Or(l, r) ->
      "("
      <> proposition_to_string(l)
      <> " | "
      <> proposition_to_string(r)
      <> ")"
    proposition.Implies(a, c) ->
      "("
      <> proposition_to_string(a)
      <> " -> "
      <> proposition_to_string(c)
      <> ")"
    proposition.Necessary(inner) -> "[]" <> proposition_to_string(inner)
    proposition.Possible(inner) -> "<>" <> proposition_to_string(inner)
    proposition.Obligatory(inner) -> "O" <> proposition_to_string(inner)
    proposition.Permitted(inner) -> "P" <> proposition_to_string(inner)
    proposition.Knows(agent, inner) ->
      "K_" <> agent <> "(" <> proposition_to_string(inner) <> ")"
    proposition.Believes(agent, inner) ->
      "B_" <> agent <> "(" <> proposition_to_string(inner) <> ")"
  }
}

fn pattern_to_string(pattern: PatternStructure) -> String {
  let premises =
    pattern.premise_patterns
    |> list.map(prop_pattern_to_string)
    |> string.join(", ")
  premises <> " |- " <> prop_pattern_to_string(pattern.conclusion_pattern)
}

fn prop_pattern_to_string(pattern: PropositionPattern) -> String {
  case pattern {
    AnyAtom -> "?"
    AnyProposition -> "*"
    SpecificAtom(name) -> name
    NegationOf(inner) -> "~" <> prop_pattern_to_string(inner)
    ConjunctionOf(l, r) ->
      "("
      <> prop_pattern_to_string(l)
      <> "&"
      <> prop_pattern_to_string(r)
      <> ")"
    DisjunctionOf(l, r) ->
      "("
      <> prop_pattern_to_string(l)
      <> "|"
      <> prop_pattern_to_string(r)
      <> ")"
    ImplicationOf(a, c) ->
      "("
      <> prop_pattern_to_string(a)
      <> "->"
      <> prop_pattern_to_string(c)
      <> ")"
    NecessityOf(inner) -> "[]" <> prop_pattern_to_string(inner)
    PossibilityOf(inner) -> "<>" <> prop_pattern_to_string(inner)
  }
}

fn list_max(l: List(Float)) -> Float {
  case l {
    [] -> 0.0
    [x] -> x
    [x, ..rest] -> float_max(x, list_max(rest))
  }
}

fn float_max(a: Float, b: Float) -> Float {
  case a >. b {
    True -> a
    False -> b
  }
}

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
