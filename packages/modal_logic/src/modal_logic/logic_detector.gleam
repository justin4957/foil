//// Logic System Detection
////
//// This module determines the appropriate modal logic system for arguments.
//// It uses a combination of:
//// - Heuristic analysis of modal words
//// - LLM suggestions
//// - User overrides
////
//// ## Logic System Properties
////
//// - K: Base modal logic (no special properties)
//// - T: Reflexive (□p → p, what's necessary is actual)
//// - K4: Transitive (□p → □□p, necessity iterates)
//// - S4: Reflexive + Transitive
//// - S5: Equivalence relation (symmetric accessibility)
//// - KD: Serial (deontic, no conflicts: ¬(□p ∧ □¬p))
//// - KD45: Full deontic logic
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/logic_detector
////
//// let detection = logic_detector.detect_from_text(argument_text)
//// case detection.recommended {
////   Some(system) -> use_system(system)
////   None -> ask_user()
//// }
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/proposition.{type LogicSystem, type Proposition}

// =============================================================================
// Types
// =============================================================================

/// Result of logic system detection
pub type LogicDetection {
  LogicDetection(
    /// Recommended logic system (if confident)
    recommended: Option(LogicSystem),
    /// All possible systems with scores
    candidates: List(LogicCandidate),
    /// Modal words found in the text
    modal_words: List(ModalWord),
    /// Detected modal features
    features: List(ModalFeature),
    /// Overall confidence (0.0 to 1.0)
    confidence: Float,
    /// Reasoning for the recommendation
    reasoning: String,
  )
}

/// A candidate logic system with score
pub type LogicCandidate {
  LogicCandidate(
    /// The logic system
    system: LogicSystem,
    /// Score (0.0 to 1.0)
    score: Float,
    /// Reasons for this score
    reasons: List(String),
  )
}

/// A modal word found in text
pub type ModalWord {
  ModalWord(
    /// The word or phrase
    word: String,
    /// Category of modal
    category: ModalCategory,
    /// Typical strength (necessity vs possibility)
    strength: ModalStrength,
    /// Position in text (if available)
    position: Option(Int),
  )
}

/// Categories of modal expressions
pub type ModalCategory {
  /// Necessity/possibility (must, can, possibly)
  Alethic
  /// Obligation/permission (should, may, ought)
  Deontic
  /// Knowledge/belief (knows, believes, thinks)
  Epistemic
  /// Time-based (always, sometimes, eventually)
  Temporal
  /// Conditional/hypothetical
  Conditional
}

/// Strength of modal expression
pub type ModalStrength {
  /// Strong necessity (must, necessarily)
  Necessity
  /// Weak necessity (should, ought)
  WeakNecessity
  /// Strong possibility (can, possible)
  Possibility
  /// Weak possibility (might, perhaps)
  WeakPossibility
}

/// Modal features detected in an argument
pub type ModalFeature {
  /// Nested modalities (□□p, ◇□p)
  NestedModality
  /// Mixed modal types (alethic + deontic)
  MixedModality
  /// Reflexive patterns (what is necessary is actual)
  ReflexivePattern
  /// Transitive patterns (iterated necessity)
  TransitivePattern
  /// Symmetric patterns (if possible necessary, then necessary)
  SymmetricPattern
  /// Deontic patterns (ought implies can)
  DeonticPattern
  /// Epistemic patterns (knowledge implies truth)
  EpistemicPattern
}

/// User override for logic system
pub type LogicOverride {
  /// No override, use detection
  NoOverride
  /// Suggest but allow change
  SuggestSystem(LogicSystem)
  /// Require specific system
  RequireSystem(LogicSystem)
}

// =============================================================================
// Detection from Text
// =============================================================================

/// Detect logic system from natural language text
pub fn detect_from_text(text: String) -> LogicDetection {
  let lower_text = string.lowercase(text)
  let modal_words = extract_modal_words(lower_text)
  let features = detect_features(modal_words)
  let candidates = score_candidates(modal_words, features)

  let #(recommended, confidence) = case candidates {
    [] -> #(None, 0.0)
    [first, ..rest] -> {
      // Recommend if score is significantly higher than alternatives
      case rest {
        [] -> #(Some(first.system), first.score)
        [second, ..] ->
          case first.score -. second.score >. 0.2 {
            True -> #(Some(first.system), first.score)
            False -> #(None, first.score *. 0.7)
          }
      }
    }
  }

  let reasoning = build_reasoning(modal_words, features, recommended)

  LogicDetection(
    recommended: recommended,
    candidates: candidates,
    modal_words: modal_words,
    features: features,
    confidence: confidence,
    reasoning: reasoning,
  )
}

/// Detect logic system from propositions
pub fn detect_from_propositions(
  premises: List(Proposition),
  conclusion: Proposition,
) -> LogicDetection {
  let all_props = [conclusion, ..premises]
  let features = detect_features_from_propositions(all_props)
  let modal_words = []
  // No text to analyze
  let candidates = score_candidates_from_features(features)

  let #(recommended, confidence) = case candidates {
    [] -> #(Some(proposition.K), 0.5)
    // Default to K
    [first, ..rest] -> {
      case rest {
        [] -> #(Some(first.system), first.score)
        [second, ..] ->
          case first.score -. second.score >. 0.15 {
            True -> #(Some(first.system), first.score)
            False -> #(None, first.score *. 0.6)
          }
      }
    }
  }

  let reasoning = build_reasoning(modal_words, features, recommended)

  LogicDetection(
    recommended: recommended,
    candidates: candidates,
    modal_words: modal_words,
    features: features,
    confidence: confidence,
    reasoning: reasoning,
  )
}

// =============================================================================
// Modal Word Extraction
// =============================================================================

/// Extract modal words from text
fn extract_modal_words(text: String) -> List(ModalWord) {
  let word_patterns = get_modal_patterns()

  list.filter_map(word_patterns, fn(pattern) {
    let #(word, category, strength) = pattern
    case string.contains(text, word) {
      True -> Ok(ModalWord(word, category, strength, None))
      False -> Error(Nil)
    }
  })
}

/// Get modal word patterns
fn get_modal_patterns() -> List(#(String, ModalCategory, ModalStrength)) {
  [
    // Alethic necessity
    #("necessarily", Alethic, Necessity),
    #("must be", Alethic, Necessity),
    #("has to be", Alethic, Necessity),
    #("cannot be otherwise", Alethic, Necessity),
    #("it is necessary that", Alethic, Necessity),
    #("inevitably", Alethic, Necessity),
    // Alethic possibility
    #("possibly", Alethic, Possibility),
    #("can be", Alethic, Possibility),
    #("could be", Alethic, Possibility),
    #("might be", Alethic, WeakPossibility),
    #("it is possible that", Alethic, Possibility),
    #("perhaps", Alethic, WeakPossibility),
    #("maybe", Alethic, WeakPossibility),
    // Deontic obligation
    #("ought to", Deontic, WeakNecessity),
    #("should", Deontic, WeakNecessity),
    #("must", Deontic, Necessity),
    #("obliged to", Deontic, Necessity),
    #("required to", Deontic, Necessity),
    #("duty to", Deontic, Necessity),
    // Deontic permission
    #("may", Deontic, Possibility),
    #("permitted to", Deontic, Possibility),
    #("allowed to", Deontic, Possibility),
    #("can", Deontic, Possibility),
    #("entitled to", Deontic, Possibility),
    // Epistemic knowledge
    #("knows that", Epistemic, Necessity),
    #("is aware that", Epistemic, Necessity),
    #("is certain that", Epistemic, Necessity),
    // Epistemic belief
    #("believes that", Epistemic, WeakNecessity),
    #("thinks that", Epistemic, WeakNecessity),
    #("suspects that", Epistemic, WeakPossibility),
    // Temporal
    #("always", Temporal, Necessity),
    #("never", Temporal, Necessity),
    #("sometimes", Temporal, Possibility),
    #("eventually", Temporal, Possibility),
    #("until", Temporal, Possibility),
    // Conditional
    #("if", Conditional, Possibility),
    #("would", Conditional, WeakNecessity),
    #("could", Conditional, Possibility),
  ]
}

// =============================================================================
// Feature Detection
// =============================================================================

/// Detect modal features from words
fn detect_features(modal_words: List(ModalWord)) -> List(ModalFeature) {
  let categories =
    list.map(modal_words, fn(w) { w.category })
    |> list.unique

  let features = []

  // Check for mixed modality
  let features = case list.length(categories) > 1 {
    True -> [MixedModality, ..features]
    False -> features
  }

  // Check for deontic patterns
  let has_deontic = list.any(modal_words, fn(w) { w.category == Deontic })
  let features = case has_deontic {
    True -> [DeonticPattern, ..features]
    False -> features
  }

  // Check for epistemic patterns
  let has_epistemic = list.any(modal_words, fn(w) { w.category == Epistemic })
  let features = case has_epistemic {
    True -> [EpistemicPattern, ..features]
    False -> features
  }

  features
}

/// Detect features from propositions
fn detect_features_from_propositions(
  props: List(Proposition),
) -> List(ModalFeature) {
  let features = list.flat_map(props, analyze_proposition)
  list.unique(features)
}

/// Analyze a single proposition for features
fn analyze_proposition(prop: Proposition) -> List(ModalFeature) {
  case prop {
    proposition.Atom(_) -> []

    proposition.Not(inner) -> analyze_proposition(inner)

    proposition.And(left, right) ->
      list.append(analyze_proposition(left), analyze_proposition(right))

    proposition.Or(left, right) ->
      list.append(analyze_proposition(left), analyze_proposition(right))

    proposition.Implies(ante, cons) ->
      list.append(analyze_proposition(ante), analyze_proposition(cons))

    proposition.Necessary(inner) -> {
      let inner_features = analyze_proposition(inner)
      case inner {
        proposition.Necessary(_) -> [
          NestedModality,
          TransitivePattern,
          ..inner_features
        ]
        proposition.Possible(_) -> [NestedModality, ..inner_features]
        _ -> inner_features
      }
    }

    proposition.Possible(inner) -> {
      let inner_features = analyze_proposition(inner)
      case inner {
        proposition.Necessary(_) -> [
          NestedModality,
          SymmetricPattern,
          ..inner_features
        ]
        proposition.Possible(_) -> [NestedModality, ..inner_features]
        _ -> inner_features
      }
    }

    proposition.Obligatory(inner) -> [
      DeonticPattern,
      ..analyze_proposition(inner)
    ]

    proposition.Permitted(inner) -> [
      DeonticPattern,
      ..analyze_proposition(inner)
    ]

    proposition.Knows(_, inner) -> [
      EpistemicPattern,
      ..analyze_proposition(inner)
    ]

    proposition.Believes(_, inner) -> [
      EpistemicPattern,
      ..analyze_proposition(inner)
    ]
  }
}

// =============================================================================
// Candidate Scoring
// =============================================================================

/// Score logic system candidates based on words and features
fn score_candidates(
  modal_words: List(ModalWord),
  features: List(ModalFeature),
) -> List(LogicCandidate) {
  let systems = [
    proposition.K,
    proposition.T,
    proposition.K4,
    proposition.S4,
    proposition.S5,
    proposition.KD,
    proposition.KD45,
  ]

  systems
  |> list.map(fn(system) { score_system(system, modal_words, features) })
  |> list.sort(fn(a, b) {
    case a.score >. b.score {
      True -> order_lt()
      False ->
        case a.score <. b.score {
          True -> order_gt()
          False -> order_eq()
        }
    }
  })
}

/// Score candidates from features only
fn score_candidates_from_features(
  features: List(ModalFeature),
) -> List(LogicCandidate) {
  score_candidates([], features)
}

/// Score a specific logic system
fn score_system(
  system: LogicSystem,
  modal_words: List(ModalWord),
  features: List(ModalFeature),
) -> LogicCandidate {
  let #(base_score, reasons) = case system {
    proposition.K -> #(0.3, ["Base modal logic, minimal assumptions"])

    proposition.T -> {
      let reasons = ["Reflexive: what's necessary is actual"]
      let score = case list.any(features, fn(f) { f == ReflexivePattern }) {
        True -> 0.7
        False -> 0.4
      }
      #(score, reasons)
    }

    proposition.K4 -> {
      let reasons = ["Transitive: necessity iterates"]
      let score = case list.any(features, fn(f) { f == TransitivePattern }) {
        True -> 0.7
        False -> 0.35
      }
      #(score, reasons)
    }

    proposition.S4 -> {
      let has_transitive = list.any(features, fn(f) { f == TransitivePattern })
      let has_reflexive = list.any(features, fn(f) { f == ReflexivePattern })
      let score = case has_transitive && has_reflexive {
        True -> 0.85
        False ->
          case has_transitive || has_reflexive {
            True -> 0.55
            False -> 0.4
          }
      }
      #(score, ["Reflexive + Transitive"])
    }

    proposition.S5 -> {
      let has_symmetric = list.any(features, fn(f) { f == SymmetricPattern })
      let score = case has_symmetric {
        True -> 0.8
        False -> 0.35
      }
      #(score, ["Equivalence relation, symmetric necessity"])
    }

    proposition.KD -> {
      let has_deontic = list.any(features, fn(f) { f == DeonticPattern })
      let has_deontic_words =
        list.any(modal_words, fn(w) { w.category == Deontic })
      let score = case has_deontic || has_deontic_words {
        True -> 0.8
        False -> 0.2
      }
      #(score, ["Deontic logic, serial accessibility"])
    }

    proposition.KD45 -> {
      let has_deontic = list.any(features, fn(f) { f == DeonticPattern })
      let has_deontic_words =
        list.any(modal_words, fn(w) { w.category == Deontic })
      let has_nested = list.any(features, fn(f) { f == NestedModality })
      let score = case has_deontic || has_deontic_words {
        True ->
          case has_nested {
            True -> 0.85
            False -> 0.6
          }
        False -> 0.15
      }
      #(score, ["Full deontic logic with introspection"])
    }
  }

  // Adjust for epistemic content
  let has_epistemic = list.any(features, fn(f) { f == EpistemicPattern })
  let adjusted_score = case has_epistemic {
    True ->
      case system {
        proposition.S5 -> base_score +. 0.1
        proposition.T -> base_score +. 0.05
        _ -> base_score
      }
    False -> base_score
  }

  LogicCandidate(system: system, score: adjusted_score, reasons: reasons)
}

// =============================================================================
// Override Application
// =============================================================================

/// Apply user override to detection result
pub fn apply_override(
  detection: LogicDetection,
  override: LogicOverride,
) -> LogicDetection {
  case override {
    NoOverride -> detection

    SuggestSystem(system) -> {
      // Boost the suggested system's score
      let new_candidates =
        list.map(detection.candidates, fn(c) {
          case c.system == system {
            True ->
              LogicCandidate(
                ..c,
                score: min_float(1.0, c.score +. 0.3),
                reasons: ["User suggested", ..c.reasons],
              )
            False -> c
          }
        })
      LogicDetection(
        ..detection,
        candidates: new_candidates,
        recommended: Some(system),
        reasoning: detection.reasoning
          <> " (User suggested "
          <> logic_system_to_string(system)
          <> ")",
      )
    }

    RequireSystem(system) -> {
      LogicDetection(
        ..detection,
        recommended: Some(system),
        confidence: 1.0,
        reasoning: "User required " <> logic_system_to_string(system),
      )
    }
  }
}

/// Merge detection from text with detection from propositions
pub fn merge_detections(
  text_detection: LogicDetection,
  prop_detection: LogicDetection,
) -> LogicDetection {
  // Combine modal words and features
  let modal_words = text_detection.modal_words
  let features =
    list.unique(list.append(text_detection.features, prop_detection.features))

  // Average candidate scores
  let merged_candidates =
    list.map(text_detection.candidates, fn(tc) {
      let prop_score =
        list.find(prop_detection.candidates, fn(pc) { pc.system == tc.system })
        |> result.map(fn(pc) { pc.score })
        |> result.unwrap(0.0)

      let avg_score = { tc.score +. prop_score } /. 2.0
      LogicCandidate(..tc, score: avg_score)
    })
    |> list.sort(fn(a, b) {
      case a.score >. b.score {
        True -> order_lt()
        False ->
          case a.score <. b.score {
            True -> order_gt()
            False -> order_eq()
          }
      }
    })

  let #(recommended, confidence) = case merged_candidates {
    [] -> #(None, 0.0)
    [first, ..rest] -> {
      case rest {
        [] -> #(Some(first.system), first.score)
        [second, ..] ->
          case first.score -. second.score >. 0.15 {
            True -> #(Some(first.system), first.score)
            False -> #(None, first.score *. 0.6)
          }
      }
    }
  }

  LogicDetection(
    recommended: recommended,
    candidates: merged_candidates,
    modal_words: modal_words,
    features: features,
    confidence: confidence,
    reasoning: "Merged detection from text and proposition analysis",
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

fn build_reasoning(
  modal_words: List(ModalWord),
  features: List(ModalFeature),
  recommended: Option(LogicSystem),
) -> String {
  let word_summary = case modal_words {
    [] -> "No modal words detected. "
    words -> {
      let word_strs = list.map(words, fn(w) { "'" <> w.word <> "'" })
      "Found modal words: " <> string.join(word_strs, ", ") <> ". "
    }
  }

  let feature_summary = case features {
    [] -> "No special modal features detected. "
    feats -> {
      let feat_strs = list.map(feats, feature_to_string)
      "Detected features: " <> string.join(feat_strs, ", ") <> ". "
    }
  }

  let recommendation = case recommended {
    None -> "No clear recommendation; multiple systems may apply."
    Some(system) -> "Recommended: " <> logic_system_to_string(system) <> "."
  }

  word_summary <> feature_summary <> recommendation
}

fn feature_to_string(feature: ModalFeature) -> String {
  case feature {
    NestedModality -> "nested modality"
    MixedModality -> "mixed modality types"
    ReflexivePattern -> "reflexive pattern"
    TransitivePattern -> "transitive pattern"
    SymmetricPattern -> "symmetric pattern"
    DeonticPattern -> "deontic reasoning"
    EpistemicPattern -> "epistemic reasoning"
  }
}

fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    proposition.K -> "K"
    proposition.T -> "T"
    proposition.K4 -> "K4"
    proposition.S4 -> "S4"
    proposition.S5 -> "S5"
    proposition.KD -> "KD"
    proposition.KD45 -> "KD45"
  }
}

fn min_float(a: Float, b: Float) -> Float {
  case a <. b {
    True -> a
    False -> b
  }
}

// Order helpers for sorting (descending by score)
fn order_lt() -> order.Order {
  order.Lt
}

fn order_gt() -> order.Order {
  order.Gt
}

fn order_eq() -> order.Order {
  order.Eq
}

// Import order type
import gleam/order
