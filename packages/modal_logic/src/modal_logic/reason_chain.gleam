//// Reason Chain Formalization
////
//// This module provides infrastructure for parsing natural language reasoning
//// chains into structured formats that can be formalized into modal logic.
////
//// ## Overview
////
//// Prediction reasoning often comes as natural language like:
//// > "The stock will rise because if earnings beat expectations, stock prices
//// > typically increase, and we know earnings beat expectations."
////
//// This module:
//// 1. Parses such text into a structured ReasonChain
//// 2. Classifies each reason by type (factual, causal, modal, epistemic, deontic)
//// 3. Detects implicit assumptions
//// 4. Generates formal Arguments for validation
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/reason_chain.{parse_reason_chain, formalize_chain}
////
//// let chain = parse_reason_chain("The project will succeed because...")
//// let argument = formalize_chain(chain)
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/argument.{type Formalization, Formalization}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, KD, Knows,
  Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}

// =============================================================================
// Types
// =============================================================================

/// A complete reason chain extracted from natural language
pub type ReasonChain {
  ReasonChain(
    /// The main claim or conclusion
    claim: String,
    /// Supporting reasons
    reasons: List(Reason),
    /// Detected implicit assumptions
    implicit_assumptions: List(String),
    /// Original natural language text
    original_text: String,
    /// Confidence in the extraction (0.0 to 1.0)
    extraction_confidence: Float,
  )
}

/// A single reason in the chain
pub type Reason {
  Reason(
    /// The statement of the reason
    statement: String,
    /// Classification of the reason type
    reason_type: ReasonType,
    /// What this reason supports (claim or another reason)
    supports: String,
    /// Confidence in the classification (0.0 to 1.0)
    confidence: Float,
    /// Modal indicators found in the reason
    modal_indicators: List(ModalIndicator),
  )
}

/// Types of reasons in a reasoning chain
pub type ReasonType {
  /// Factual assertion: "Earnings beat expectations"
  Factual
  /// Causal relationship: "If X then Y"
  Causal
  /// Modal assertion: "Necessarily", "Possibly"
  Modal
  /// Epistemic assertion: "We know", "It's believed"
  Epistemic
  /// Deontic assertion: "Should", "Must" (obligation/permission)
  Deontic
  /// Mixed type with multiple characteristics
  Mixed(List(ReasonType))
}

/// Modal indicators found in text
pub type ModalIndicator {
  ModalIndicator(
    /// The word or phrase
    text: String,
    /// Position in the reason
    position: Int,
    /// Category of modal
    category: ModalCategory,
    /// Strength of the modal (0.0 to 1.0)
    strength: Float,
  )
}

/// Categories of modal expressions
pub type ModalCategory {
  /// Alethic modality (necessity, possibility)
  Alethic
  /// Deontic modality (obligation, permission)
  DeonticModal
  /// Epistemic modality (knowledge, belief)
  EpistemicModal
  /// Temporal modality (always, sometimes)
  Temporal
  /// Conditional relationships
  Conditional
  /// Probabilistic expressions
  Probabilistic
}

/// Result of formalizing a reason chain
pub type FormalizationResult {
  FormalizationResult(
    /// The formalized argument
    formalization: Formalization,
    /// Mapping of reasons to premises
    reason_to_premise_map: List(#(String, Proposition)),
    /// Recommended logic system with explanation
    recommended_system: LogicSystem,
    /// Explanation of why this system was chosen
    system_explanation: String,
    /// Logical gaps identified
    gaps: List(LogicalGap),
  )
}

/// A logical gap in the reasoning
pub type LogicalGap {
  LogicalGap(
    /// Description of the gap
    description: String,
    /// Which reasons are involved
    involved_reasons: List(String),
    /// Severity (0.0 = minor, 1.0 = critical)
    severity: Float,
    /// Suggested fix
    suggested_fix: Option(String),
  )
}

/// Configuration for reason chain parsing
pub type ParseConfig {
  ParseConfig(
    /// Minimum confidence for reason extraction
    min_reason_confidence: Float,
    /// Whether to detect implicit assumptions
    detect_assumptions: Bool,
    /// Maximum number of reasons to extract
    max_reasons: Int,
    /// Whether to classify mixed types
    allow_mixed_types: Bool,
    /// Default logic system if none detected
    default_logic_system: LogicSystem,
  )
}

/// Error types for reason chain processing
pub type ReasonChainError {
  /// Failed to parse the input text
  ParseError(message: String)
  /// No claim could be identified
  NoClaimError
  /// No supporting reasons found
  NoReasonsError
  /// Formalization failed
  FormalizationError(message: String)
  /// Logic system could not be determined
  LogicSystemError(message: String)
  /// JSON parsing error
  JsonError(message: String)
}

// =============================================================================
// Configuration
// =============================================================================

/// Default configuration for reason chain parsing
pub fn default_config() -> ParseConfig {
  ParseConfig(
    min_reason_confidence: 0.5,
    detect_assumptions: True,
    max_reasons: 10,
    allow_mixed_types: True,
    default_logic_system: K,
  )
}

/// Strict configuration requiring higher confidence
pub fn strict_config() -> ParseConfig {
  ParseConfig(
    min_reason_confidence: 0.75,
    detect_assumptions: True,
    max_reasons: 5,
    allow_mixed_types: False,
    default_logic_system: S4,
  )
}

/// Fast configuration for quick analysis
pub fn fast_config() -> ParseConfig {
  ParseConfig(
    min_reason_confidence: 0.3,
    detect_assumptions: False,
    max_reasons: 20,
    allow_mixed_types: True,
    default_logic_system: K,
  )
}

// =============================================================================
// Parsing Functions
// =============================================================================

/// Parse natural language into a reason chain
///
/// This function extracts the claim, supporting reasons, and implicit
/// assumptions from natural language reasoning.
///
/// ## Example
/// ```gleam
/// let text = "The project will succeed because the team is experienced."
/// let result = parse_reason_chain(text, default_config())
/// ```
pub fn parse_reason_chain(
  text: String,
  config: ParseConfig,
) -> Result(ReasonChain, ReasonChainError) {
  // Extract claim (conclusion)
  use claim <- result.try(extract_claim(text))

  // Extract supporting reasons
  use reasons <- result.try(extract_reasons(text, claim, config))

  // Detect implicit assumptions
  let assumptions = case config.detect_assumptions {
    True -> detect_implicit_assumptions(text, reasons)
    False -> []
  }

  // Calculate overall extraction confidence
  let confidence = calculate_extraction_confidence(reasons)

  Ok(ReasonChain(
    claim: claim,
    reasons: reasons,
    implicit_assumptions: assumptions,
    original_text: text,
    extraction_confidence: confidence,
  ))
}

/// Extract the main claim from text
fn extract_claim(text: String) -> Result(String, ReasonChainError) {
  // Look for claim indicators
  let claim_patterns = [
    #("will ", " because"),
    #("therefore ", ""),
    #("so ", ""),
    #("thus ", ""),
    #("hence ", ""),
    #("consequently ", ""),
    #("", " because"),
    #("", " since"),
  ]

  // Try each pattern
  let result =
    list.find_map(claim_patterns, fn(pattern) {
      let #(prefix, suffix) = pattern
      extract_claim_with_pattern(text, prefix, suffix)
    })

  case result {
    Ok(claim) -> Ok(claim)
    Error(_) -> {
      // Fall back to first sentence as claim
      case string.split(text, ".") {
        [first, ..] -> Ok(string.trim(first))
        [] -> Error(NoClaimError)
      }
    }
  }
}

/// Extract claim using a specific pattern
fn extract_claim_with_pattern(
  text: String,
  prefix: String,
  suffix: String,
) -> Result(String, Nil) {
  let lower_text = string.lowercase(text)

  let prefix_pos = case prefix {
    "" -> Ok(0)
    p -> find_substring_position(lower_text, p)
  }

  let suffix_pos = case suffix {
    "" -> Ok(string.length(text))
    s -> find_substring_position(lower_text, s)
  }

  case prefix_pos, suffix_pos {
    Ok(start), Ok(end) if end > start -> {
      let claim =
        text
        |> string.slice(
          start + string.length(prefix),
          end - start - string.length(prefix),
        )
        |> string.trim()
      case claim {
        "" -> Error(Nil)
        c -> Ok(c)
      }
    }
    _, _ -> Error(Nil)
  }
}

/// Find position of substring
fn find_substring_position(text: String, pattern: String) -> Result(Int, Nil) {
  find_substring_position_helper(text, pattern, 0)
}

fn find_substring_position_helper(
  text: String,
  pattern: String,
  pos: Int,
) -> Result(Int, Nil) {
  let pattern_len = string.length(pattern)
  let text_len = string.length(text)

  case pos + pattern_len > text_len {
    True -> Error(Nil)
    False -> {
      let slice = string.slice(text, pos, pattern_len)
      case string.lowercase(slice) == string.lowercase(pattern) {
        True -> Ok(pos)
        False -> find_substring_position_helper(text, pattern, pos + 1)
      }
    }
  }
}

/// Extract supporting reasons from text
fn extract_reasons(
  text: String,
  claim: String,
  config: ParseConfig,
) -> Result(List(Reason), ReasonChainError) {
  // Reason connectors to look for
  let connectors = [
    "because", "since", "as", "given that", "due to", "owing to", "for", "and",
    "also", "moreover", "furthermore", "in addition", "additionally",
  ]

  // Extract text after "because" or similar
  let reasons_text = extract_reasons_text(text, connectors)

  case reasons_text {
    "" -> Error(NoReasonsError)
    rt -> {
      // Split into individual reasons
      let reason_parts = split_into_reasons(rt, connectors)

      // Classify each reason
      let reasons =
        reason_parts
        |> list.filter_map(fn(part) {
          let statement = string.trim(part)
          case statement {
            "" -> Error(Nil)
            s -> {
              let #(reason_type, indicators) = classify_reason(s)
              let confidence = calculate_reason_confidence(s, reason_type)
              case confidence >=. config.min_reason_confidence {
                True ->
                  Ok(Reason(
                    statement: s,
                    reason_type: reason_type,
                    supports: claim,
                    confidence: confidence,
                    modal_indicators: indicators,
                  ))
                False -> Error(Nil)
              }
            }
          }
        })
        |> list.take(config.max_reasons)

      case reasons {
        [] -> Error(NoReasonsError)
        rs -> Ok(rs)
      }
    }
  }
}

/// Extract the portion of text containing reasons
fn extract_reasons_text(text: String, connectors: List(String)) -> String {
  let lower_text = string.lowercase(text)

  // Find the first connector and extract everything after
  let result =
    list.find_map(connectors, fn(connector) {
      case find_substring_position(lower_text, connector) {
        Ok(pos) -> {
          let after = string.drop_start(text, pos + string.length(connector))
          Ok(string.trim(after))
        }
        Error(_) -> Error(Nil)
      }
    })

  case result {
    Ok(rt) -> rt
    Error(_) -> ""
  }
}

/// Split reasons text into individual reasons
fn split_into_reasons(text: String, connectors: List(String)) -> List(String) {
  // Split on commas and connectors
  let parts =
    text
    |> string.split(",")
    |> list.flat_map(fn(part) { string.split(part, " and ") })
    |> list.flat_map(fn(part) { string.split(part, " also ") })
    |> list.flat_map(fn(part) { string.split(part, ".") })
    |> list.map(string.trim)
    |> list.filter(fn(s) { s != "" })

  parts
}

/// Classify a reason by type
fn classify_reason(statement: String) -> #(ReasonType, List(ModalIndicator)) {
  let lower = string.lowercase(statement)
  let indicators = extract_modal_indicators(statement)

  // Check for different reason types
  let is_causal = is_causal_statement(lower)
  let is_epistemic = is_epistemic_statement(lower)
  let is_deontic = is_deontic_statement(lower)
  let is_modal = is_modal_statement(lower)

  let types = []
  let types = case is_causal {
    True -> [Causal, ..types]
    False -> types
  }
  let types = case is_epistemic {
    True -> [Epistemic, ..types]
    False -> types
  }
  let types = case is_deontic {
    True -> [Deontic, ..types]
    False -> types
  }
  let types = case is_modal {
    True -> [Modal, ..types]
    False -> types
  }

  let reason_type = case types {
    [] -> Factual
    [single] -> single
    multiple -> Mixed(multiple)
  }

  #(reason_type, indicators)
}

/// Check if statement expresses causality
fn is_causal_statement(lower: String) -> Bool {
  let causal_patterns = [
    "if ", " then ", "leads to", "causes", "results in", "implies", "typically",
    "usually", "often", "generally", "tend to", "because of", "due to", "leads",
    "result",
  ]
  list.any(causal_patterns, fn(p) { string.contains(lower, p) })
}

/// Check if statement is epistemic
fn is_epistemic_statement(lower: String) -> Bool {
  let epistemic_patterns = [
    "know", "believe", "think", "aware", "certain", "sure", "we know",
    "it is known", "evidence", "understand",
  ]
  list.any(epistemic_patterns, fn(p) { string.contains(lower, p) })
}

/// Check if statement is deontic
fn is_deontic_statement(lower: String) -> Bool {
  let deontic_patterns = [
    "should", "must", "ought", "obligat", "permit", "allow", "require",
    "forbidden", "duty", "right to",
  ]
  list.any(deontic_patterns, fn(p) { string.contains(lower, p) })
}

/// Check if statement is modal
fn is_modal_statement(lower: String) -> Bool {
  let modal_patterns = [
    "necessar", "possible", "might", "could", "would", "always", "never",
    "sometimes", "must be", "can be",
  ]
  list.any(modal_patterns, fn(p) { string.contains(lower, p) })
}

/// Extract modal indicators from text
fn extract_modal_indicators(text: String) -> List(ModalIndicator) {
  let modal_words = [
    #("necessarily", Alethic, 1.0),
    #("possibly", Alethic, 0.7),
    #("must", Alethic, 0.9),
    #("might", Alethic, 0.5),
    #("could", Alethic, 0.5),
    #("would", Alethic, 0.6),
    #("know", EpistemicModal, 0.9),
    #("believe", EpistemicModal, 0.7),
    #("think", EpistemicModal, 0.6),
    #("should", DeonticModal, 0.8),
    #("ought", DeonticModal, 0.8),
    #("permitted", DeonticModal, 0.7),
    #("obligated", DeonticModal, 0.9),
    #("always", Temporal, 0.9),
    #("sometimes", Temporal, 0.5),
    #("never", Temporal, 0.9),
    #("if", Conditional, 0.8),
    #("typically", Probabilistic, 0.7),
    #("usually", Probabilistic, 0.7),
    #("often", Probabilistic, 0.6),
    #("rarely", Probabilistic, 0.3),
  ]

  let lower = string.lowercase(text)
  list.filter_map(modal_words, fn(word_info) {
    let #(word, category, strength) = word_info
    case find_substring_position(lower, word) {
      Ok(pos) ->
        Ok(ModalIndicator(
          text: word,
          position: pos,
          category: category,
          strength: strength,
        ))
      Error(_) -> Error(Nil)
    }
  })
}

/// Calculate confidence for a single reason
fn calculate_reason_confidence(
  statement: String,
  reason_type: ReasonType,
) -> Float {
  let base_confidence = 0.6

  // Longer statements tend to be more specific
  let length_bonus = case string.length(statement) {
    n if n > 50 -> 0.1
    n if n > 20 -> 0.05
    _ -> 0.0
  }

  // Type-specific adjustments
  let type_adjustment = case reason_type {
    Factual -> 0.1
    Causal -> 0.15
    Epistemic -> 0.1
    Deontic -> 0.1
    Modal -> 0.05
    Mixed(_) -> 0.0
  }

  float.min(1.0, base_confidence +. length_bonus +. type_adjustment)
}

/// Calculate overall extraction confidence
fn calculate_extraction_confidence(reasons: List(Reason)) -> Float {
  case reasons {
    [] -> 0.0
    rs -> {
      let total = list.fold(rs, 0.0, fn(acc, r) { acc +. r.confidence })
      total /. int.to_float(list.length(rs))
    }
  }
}

/// Detect implicit assumptions in the reasoning
fn detect_implicit_assumptions(
  text: String,
  reasons: List(Reason),
) -> List(String) {
  let assumptions = []

  // Check for probabilistic language that implies assumptions
  let lower = string.lowercase(text)

  let assumptions = case
    string.contains(lower, "usually")
    || string.contains(lower, "typically")
    || string.contains(lower, "often")
  {
    True -> [
      "Probabilistic statements are interpreted as modal necessity",
      ..assumptions
    ]
    False -> assumptions
  }

  // Check for causal claims without explicit universality
  let has_causal =
    list.any(reasons, fn(r) {
      case r.reason_type {
        Causal -> True
        Mixed(types) -> list.any(types, fn(t) { t == Causal })
        _ -> False
      }
    })

  let assumptions = case has_causal && !string.contains(lower, "always") {
    True -> [
      "Causal relationships are assumed to hold universally",
      ..assumptions
    ]
    False -> assumptions
  }

  // Check for epistemic claims
  let has_epistemic =
    list.any(reasons, fn(r) {
      case r.reason_type {
        Epistemic -> True
        Mixed(types) -> list.any(types, fn(t) { t == Epistemic })
        _ -> False
      }
    })

  let assumptions = case has_epistemic {
    True -> [
      "Knowledge claims are assumed to be true and justified",
      ..assumptions
    ]
    False -> assumptions
  }

  assumptions
}

// =============================================================================
// Formalization Functions
// =============================================================================

/// Formalize a reason chain into a logical argument
pub fn formalize_chain(
  chain: ReasonChain,
  config: ParseConfig,
) -> Result(FormalizationResult, ReasonChainError) {
  // Determine recommended logic system
  let #(system, explanation) = determine_logic_system(chain, config)

  // Build premises from reasons
  let premises_with_map =
    chain.reasons
    |> list.map(fn(reason) {
      let prop = reason_to_proposition(reason, chain.claim)
      #(reason.statement, prop)
    })

  let premises = list.map(premises_with_map, fn(pair) { pair.1 })

  // Build conclusion
  let conclusion = claim_to_proposition(chain.claim)

  // Identify logical gaps
  let gaps = identify_logical_gaps(chain, premises)

  // Create formalization
  let formalization =
    Formalization(
      id: generate_formalization_id(chain),
      argument_id: "reason-chain",
      logic_system: system,
      premises: premises,
      conclusion: conclusion,
      assumptions: chain.implicit_assumptions,
      validation: None,
      created_at: None,
      updated_at: None,
    )

  Ok(FormalizationResult(
    formalization: formalization,
    reason_to_premise_map: premises_with_map,
    recommended_system: system,
    system_explanation: explanation,
    gaps: gaps,
  ))
}

/// Determine the most appropriate logic system
fn determine_logic_system(
  chain: ReasonChain,
  config: ParseConfig,
) -> #(LogicSystem, String) {
  let all_indicators =
    chain.reasons
    |> list.flat_map(fn(r) { r.modal_indicators })

  let has_epistemic =
    list.any(all_indicators, fn(i) { i.category == EpistemicModal })
  let has_deontic =
    list.any(all_indicators, fn(i) { i.category == DeonticModal })
  let has_temporal = list.any(all_indicators, fn(i) { i.category == Temporal })
  let has_alethic = list.any(all_indicators, fn(i) { i.category == Alethic })

  // Determine system based on modal types present
  case has_epistemic, has_deontic, has_temporal, has_alethic {
    True, _, _, _ -> #(
      S5,
      "S5 recommended for epistemic reasoning (knowledge satisfies positive introspection)",
    )
    _, True, _, _ -> #(
      KD,
      "KD recommended for deontic reasoning (obligations are serial/consistent)",
    )
    _, _, True, _ -> #(
      S4,
      "S4 recommended for temporal reasoning (reflexive and transitive)",
    )
    _, _, _, True -> #(
      T,
      "T recommended for alethic reasoning (reflexive frames)",
    )
    _, _, _, _ -> #(
      config.default_logic_system,
      "Default system K used (no specific modal patterns detected)",
    )
  }
}

/// Convert a reason to a proposition
fn reason_to_proposition(reason: Reason, claim: String) -> Proposition {
  let base_prop = statement_to_atom(reason.statement)
  let claim_prop = statement_to_atom(claim)

  case reason.reason_type {
    Factual -> base_prop
    Causal ->
      // Causal: □(reason → claim)
      Necessary(Implies(base_prop, claim_prop))
    Modal -> Necessary(base_prop)
    Epistemic ->
      // Epistemic: K(reason)
      Knows("agent", base_prop)
    Deontic ->
      // Deontic: O(reason)
      Obligatory(base_prop)
    Mixed(types) -> {
      // Apply the first applicable type
      case types {
        [Causal, ..] -> Necessary(Implies(base_prop, claim_prop))
        [Epistemic, ..] -> Knows("agent", base_prop)
        [Deontic, ..] -> Obligatory(base_prop)
        [Modal, ..] -> Necessary(base_prop)
        _ -> base_prop
      }
    }
  }
}

/// Convert a claim to a proposition
fn claim_to_proposition(claim: String) -> Proposition {
  statement_to_atom(claim)
}

/// Convert a natural language statement to an atom
fn statement_to_atom(statement: String) -> Proposition {
  // Create a normalized atom name
  let normalized =
    statement
    |> string.lowercase()
    |> string.replace(" ", "_")
    |> string.replace(",", "")
    |> string.replace(".", "")
    |> string.replace("'", "")
    |> string.replace("\"", "")
    |> string.slice(0, 50)
  // Limit length

  Atom(normalized)
}

/// Generate a unique formalization ID
fn generate_formalization_id(chain: ReasonChain) -> String {
  let claim_hash =
    chain.claim
    |> string.slice(0, 10)
    |> string.replace(" ", "-")
  "rc-" <> claim_hash
}

/// Identify logical gaps in the reasoning
fn identify_logical_gaps(
  chain: ReasonChain,
  premises: List(Proposition),
) -> List(LogicalGap) {
  let gaps = []

  // Check for missing links
  let gaps = case list.length(chain.reasons) < 2 {
    True -> [
      LogicalGap(
        description: "Single reason may not fully support the claim",
        involved_reasons: list.map(chain.reasons, fn(r) { r.statement }),
        severity: 0.3,
        suggested_fix: Some("Consider adding intermediate reasoning steps"),
      ),
      ..gaps
    ]
    False -> gaps
  }

  // Check for probabilistic-to-definite jumps
  let has_probabilistic =
    chain.reasons
    |> list.flat_map(fn(r) { r.modal_indicators })
    |> list.any(fn(i) { i.category == Probabilistic })

  let gaps = case has_probabilistic {
    True -> [
      LogicalGap(
        description: "Probabilistic reasoning used to support definite conclusion",
        involved_reasons: list.filter_map(chain.reasons, fn(r) {
          case
            list.any(r.modal_indicators, fn(i) { i.category == Probabilistic })
          {
            True -> Ok(r.statement)
            False -> Error(Nil)
          }
        }),
        severity: 0.5,
        suggested_fix: Some(
          "Consider qualifying the conclusion or strengthening the premises",
        ),
      ),
      ..gaps
    ]
    False -> gaps
  }

  gaps
}

// =============================================================================
// Formatting and Serialization
// =============================================================================

/// Format a reason type as a string
pub fn reason_type_to_string(reason_type: ReasonType) -> String {
  case reason_type {
    Factual -> "Factual"
    Causal -> "Causal"
    Modal -> "Modal"
    Epistemic -> "Epistemic"
    Deontic -> "Deontic"
    Mixed(types) ->
      "Mixed("
      <> string.join(list.map(types, reason_type_to_string), ", ")
      <> ")"
  }
}

/// Format a modal category as a string
pub fn modal_category_to_string(category: ModalCategory) -> String {
  case category {
    Alethic -> "Alethic"
    DeonticModal -> "Deontic"
    EpistemicModal -> "Epistemic"
    Temporal -> "Temporal"
    Conditional -> "Conditional"
    Probabilistic -> "Probabilistic"
  }
}

/// Format a reason chain for display
pub fn format_reason_chain(chain: ReasonChain) -> String {
  let claim_str = "Claim: " <> chain.claim <> "\n"

  let reasons_str =
    chain.reasons
    |> list.index_map(fn(r, i) {
      "  "
      <> int.to_string(i + 1)
      <> ". "
      <> r.statement
      <> " ["
      <> reason_type_to_string(r.reason_type)
      <> "] (confidence: "
      <> float_to_percent(r.confidence)
      <> ")"
    })
    |> string.join("\n")

  let assumptions_str = case chain.implicit_assumptions {
    [] -> ""
    asms ->
      "\n\nImplicit Assumptions:\n"
      <> string.join(list.map(asms, fn(a) { "  - " <> a }), "\n")
  }

  claim_str
  <> "\nReasons:\n"
  <> reasons_str
  <> assumptions_str
  <> "\n\nExtraction Confidence: "
  <> float_to_percent(chain.extraction_confidence)
}

/// Format a formalization result for display
pub fn format_formalization_result(result: FormalizationResult) -> String {
  let system_str =
    "Recommended Logic System: "
    <> logic_system_to_string(result.recommended_system)
    <> "\n"
    <> "Explanation: "
    <> result.system_explanation
    <> "\n"

  let premises_str =
    result.formalization.premises
    |> list.index_map(fn(p, i) {
      "  P" <> int.to_string(i + 1) <> ": " <> format_proposition(p)
    })
    |> string.join("\n")

  let conclusion_str =
    "  C: " <> format_proposition(result.formalization.conclusion)

  let gaps_str = case result.gaps {
    [] -> ""
    gs ->
      "\n\nLogical Gaps:\n"
      <> string.join(
        list.map(gs, fn(g) {
          "  - "
          <> g.description
          <> " (severity: "
          <> float_to_percent(g.severity)
          <> ")"
        }),
        "\n",
      )
  }

  system_str
  <> "\nPremises:\n"
  <> premises_str
  <> "\n\nConclusion:\n"
  <> conclusion_str
  <> gaps_str
}

/// Convert logic system to string
fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    K -> "K"
    T -> "T"
    proposition.K4 -> "K4"
    S4 -> "S4"
    S5 -> "S5"
    KD -> "KD"
    proposition.KD45 -> "KD45"
  }
}

/// Format a proposition for display
fn format_proposition(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "¬" <> format_proposition(inner)
    And(l, r) ->
      "(" <> format_proposition(l) <> " ∧ " <> format_proposition(r) <> ")"
    Or(l, r) ->
      "(" <> format_proposition(l) <> " ∨ " <> format_proposition(r) <> ")"
    Implies(l, r) ->
      "(" <> format_proposition(l) <> " → " <> format_proposition(r) <> ")"
    Necessary(inner) -> "□" <> format_proposition(inner)
    Possible(inner) -> "◇" <> format_proposition(inner)
    Obligatory(inner) -> "O" <> format_proposition(inner)
    Permitted(inner) -> "P" <> format_proposition(inner)
    Knows(agent, inner) ->
      "K_" <> agent <> "(" <> format_proposition(inner) <> ")"
    Believes(agent, inner) ->
      "B_" <> agent <> "(" <> format_proposition(inner) <> ")"
  }
}

fn float_to_percent(f: Float) -> String {
  let whole = float_truncate(f *. 100.0)
  int.to_string(whole) <> "%"
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int

// =============================================================================
// JSON Serialization
// =============================================================================

/// Convert a reason chain to JSON string
pub fn reason_chain_to_json(chain: ReasonChain) -> String {
  let reasons_json =
    chain.reasons
    |> list.map(reason_to_json)
    |> string.join(",\n    ")

  let assumptions_json =
    chain.implicit_assumptions
    |> list.map(fn(a) { "\"" <> escape_json(a) <> "\"" })
    |> string.join(", ")

  "{\n"
  <> "  \"claim\": \""
  <> escape_json(chain.claim)
  <> "\",\n"
  <> "  \"reasons\": [\n    "
  <> reasons_json
  <> "\n  ],\n"
  <> "  \"implicit_assumptions\": ["
  <> assumptions_json
  <> "],\n"
  <> "  \"extraction_confidence\": "
  <> float.to_string(chain.extraction_confidence)
  <> "\n}"
}

/// Convert a single reason to JSON
fn reason_to_json(reason: Reason) -> String {
  let indicators_json =
    reason.modal_indicators
    |> list.map(indicator_to_json)
    |> string.join(", ")

  "{\n"
  <> "      \"statement\": \""
  <> escape_json(reason.statement)
  <> "\",\n"
  <> "      \"type\": \""
  <> reason_type_to_string(reason.reason_type)
  <> "\",\n"
  <> "      \"supports\": \""
  <> escape_json(reason.supports)
  <> "\",\n"
  <> "      \"confidence\": "
  <> float.to_string(reason.confidence)
  <> ",\n"
  <> "      \"modal_indicators\": ["
  <> indicators_json
  <> "]\n"
  <> "    }"
}

/// Convert modal indicator to JSON
fn indicator_to_json(indicator: ModalIndicator) -> String {
  "{"
  <> "\"text\": \""
  <> indicator.text
  <> "\", "
  <> "\"category\": \""
  <> modal_category_to_string(indicator.category)
  <> "\", "
  <> "\"strength\": "
  <> float.to_string(indicator.strength)
  <> "}"
}

/// Convert formalization result to JSON
pub fn formalization_result_to_json(result: FormalizationResult) -> String {
  let premises_json =
    result.formalization.premises
    |> list.map(fn(p) { "\"" <> escape_json(format_proposition(p)) <> "\"" })
    |> string.join(", ")

  let gaps_json =
    result.gaps
    |> list.map(gap_to_json)
    |> string.join(",\n    ")

  "{\n"
  <> "  \"premises\": ["
  <> premises_json
  <> "],\n"
  <> "  \"conclusion\": \""
  <> escape_json(format_proposition(result.formalization.conclusion))
  <> "\",\n"
  <> "  \"recommended_system\": \""
  <> logic_system_to_string(result.recommended_system)
  <> "\",\n"
  <> "  \"system_explanation\": \""
  <> escape_json(result.system_explanation)
  <> "\",\n"
  <> "  \"gaps\": [\n    "
  <> gaps_json
  <> "\n  ]\n"
  <> "}"
}

/// Convert logical gap to JSON
fn gap_to_json(gap: LogicalGap) -> String {
  let fix_json = case gap.suggested_fix {
    Some(fix) -> "\"" <> escape_json(fix) <> "\""
    None -> "null"
  }

  "{"
  <> "\"description\": \""
  <> escape_json(gap.description)
  <> "\", "
  <> "\"severity\": "
  <> float.to_string(gap.severity)
  <> ", "
  <> "\"suggested_fix\": "
  <> fix_json
  <> "}"
}

/// Escape a string for JSON
fn escape_json(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}
