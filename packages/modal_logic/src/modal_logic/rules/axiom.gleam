//// Modal Logic Axiom Definitions
////
//// This module defines the standard modal logic axioms (K, T, 4, 5, D, B)
//// and their corresponding frame properties.

import gleam/list
import gleam/option.{type Option, None, Some}
import modal_logic/proposition.{type LogicSystem, K, K4, KD, KD45, S4, S5, T}
import modal_logic/rules/inference_rule.{
  type PropositionPattern, type RuleMetadata, AnyAtom, PatternImplies,
  PatternNecessary, PatternNot, PatternPossible, Wildcard, default_metadata,
}

/// Frame property corresponding to an axiom
pub type FrameProperty {
  /// Reflexive: R(w,w) for all w
  Reflexive
  /// Transitive: R(w,v) ∧ R(v,u) → R(w,u)
  Transitive
  /// Symmetric: R(w,v) → R(v,w)
  Symmetric
  /// Euclidean: R(w,v) ∧ R(w,u) → R(v,u)
  Euclidean
  /// Serial: ∀w. ∃v. R(w,v)
  Serial
}

/// A modal logic axiom schema
pub type Axiom {
  Axiom(
    /// Unique identifier (e.g., "K", "T", "4", "5", "D", "B")
    id: String,
    /// Human-readable name
    name: String,
    /// Description of what the axiom asserts
    description: String,
    /// Axiom schema as proposition pattern
    schema: PropositionPattern,
    /// Logic systems that include this axiom
    included_in: List(LogicSystem),
    /// Frame property this axiom corresponds to
    frame_property: Option(FrameProperty),
    /// Additional metadata
    metadata: RuleMetadata,
  )
}

// ============ Standard Modal Logic Axioms ============

/// K Axiom: □(p → q) → (□p → □q)
/// The distribution axiom - necessity distributes over implication
pub fn k_axiom() -> Axiom {
  Axiom(
    id: "K",
    name: "Distribution Axiom",
    description: "If it is necessary that p implies q, then if p is necessary, q is necessary",
    schema: PatternImplies(
      PatternNecessary(PatternImplies(AnyAtom("p"), AnyAtom("q"))),
      PatternImplies(
        PatternNecessary(AnyAtom("p")),
        PatternNecessary(AnyAtom("q")),
      ),
    ),
    included_in: [K, T, K4, S4, S5, KD, KD45],
    frame_property: None,
    metadata: default_metadata(),
  )
}

/// T Axiom: □p → p
/// The reflexivity axiom - what is necessary is actual
pub fn t_axiom() -> Axiom {
  Axiom(
    id: "T",
    name: "Reflexivity Axiom",
    description: "If p is necessary, then p is actual",
    schema: PatternImplies(PatternNecessary(AnyAtom("p")), AnyAtom("p")),
    included_in: [T, S4, S5],
    frame_property: Some(Reflexive),
    metadata: default_metadata(),
  )
}

/// 4 Axiom: □p → □□p
/// The transitivity axiom - positive introspection
pub fn axiom_4() -> Axiom {
  Axiom(
    id: "4",
    name: "Positive Introspection",
    description: "If p is necessary, then it is necessary that p is necessary",
    schema: PatternImplies(
      PatternNecessary(AnyAtom("p")),
      PatternNecessary(PatternNecessary(AnyAtom("p"))),
    ),
    included_in: [K4, S4, S5, KD45],
    frame_property: Some(Transitive),
    metadata: default_metadata(),
  )
}

/// 5 Axiom: ◇p → □◇p
/// The Euclidean axiom - what is possible is necessarily possible
pub fn axiom_5() -> Axiom {
  Axiom(
    id: "5",
    name: "Euclidean Axiom",
    description: "If p is possible, then it is necessary that p is possible",
    schema: PatternImplies(
      PatternPossible(AnyAtom("p")),
      PatternNecessary(PatternPossible(AnyAtom("p"))),
    ),
    included_in: [S5, KD45],
    frame_property: Some(Euclidean),
    metadata: default_metadata(),
  )
}

/// D Axiom: □p → ◇p
/// The seriality axiom - deontic consistency
pub fn d_axiom() -> Axiom {
  Axiom(
    id: "D",
    name: "Seriality Axiom",
    description: "If p is necessary, then p is possible (no dead ends)",
    schema: PatternImplies(
      PatternNecessary(AnyAtom("p")),
      PatternPossible(AnyAtom("p")),
    ),
    included_in: [KD, KD45],
    frame_property: Some(Serial),
    metadata: default_metadata(),
  )
}

/// B Axiom: p → □◇p
/// The symmetry axiom - what is actual is necessarily possible
pub fn b_axiom() -> Axiom {
  Axiom(
    id: "B",
    name: "Symmetry Axiom",
    description: "If p is actual, then it is necessary that p is possible",
    schema: PatternImplies(
      AnyAtom("p"),
      PatternNecessary(PatternPossible(AnyAtom("p"))),
    ),
    included_in: [S5],
    frame_property: Some(Symmetric),
    metadata: default_metadata(),
  )
}

// ============ Derived Axiom Schemas ============

/// Dual: ◇p ↔ ¬□¬p
/// Possibility is dual to necessity
pub fn possibility_dual() -> Axiom {
  Axiom(
    id: "DUAL",
    name: "Possibility Dual",
    description: "p is possible if and only if not-p is not necessary",
    schema: PatternImplies(
      PatternPossible(AnyAtom("p")),
      PatternNot(PatternNecessary(PatternNot(AnyAtom("p")))),
    ),
    included_in: [K, T, K4, S4, S5, KD, KD45],
    frame_property: None,
    metadata: default_metadata(),
  )
}

/// Necessitation Rule: If ⊢ p, then ⊢ □p
/// Valid formulas are necessary
pub fn necessitation_schema() -> Axiom {
  Axiom(
    id: "NEC",
    name: "Necessitation",
    description: "If p is a theorem (provable), then □p is a theorem",
    schema: PatternImplies(
      Wildcard("theorem"),
      PatternNecessary(Wildcard("theorem")),
    ),
    included_in: [K, T, K4, S4, S5, KD, KD45],
    frame_property: None,
    metadata: default_metadata(),
  )
}

// ============ Epistemic Axiom Schemas ============

/// Knowledge entails truth: Kp → p
pub fn knowledge_truth() -> Axiom {
  Axiom(
    id: "KT",
    name: "Knowledge Truth",
    description: "If an agent knows p, then p is true",
    schema: PatternImplies(PatternNecessary(AnyAtom("p")), AnyAtom("p")),
    included_in: [T, S4, S5],
    frame_property: Some(Reflexive),
    metadata: default_metadata(),
  )
}

/// Positive introspection: Kp → KKp
pub fn positive_introspection() -> Axiom {
  Axiom(
    id: "KK",
    name: "Positive Introspection",
    description: "If an agent knows p, they know that they know p",
    schema: PatternImplies(
      PatternNecessary(AnyAtom("p")),
      PatternNecessary(PatternNecessary(AnyAtom("p"))),
    ),
    included_in: [S4, S5],
    frame_property: Some(Transitive),
    metadata: default_metadata(),
  )
}

/// Negative introspection: ¬Kp → K¬Kp
pub fn negative_introspection() -> Axiom {
  Axiom(
    id: "NK",
    name: "Negative Introspection",
    description: "If an agent doesn't know p, they know they don't know p",
    schema: PatternImplies(
      PatternNot(PatternNecessary(AnyAtom("p"))),
      PatternNecessary(PatternNot(PatternNecessary(AnyAtom("p")))),
    ),
    included_in: [S5],
    frame_property: Some(Euclidean),
    metadata: default_metadata(),
  )
}

// ============ Deontic Axiom Schemas ============

/// Deontic consistency: Op → Pp (obligation implies permission)
pub fn deontic_consistency() -> Axiom {
  Axiom(
    id: "DC",
    name: "Deontic Consistency",
    description: "If p is obligatory, then p is permitted",
    schema: PatternImplies(
      PatternNecessary(AnyAtom("p")),
      PatternPossible(AnyAtom("p")),
    ),
    included_in: [KD, KD45],
    frame_property: Some(Serial),
    metadata: default_metadata(),
  )
}

// ============ Utility Functions ============

/// Get all standard axioms
pub fn all_standard_axioms() -> List(Axiom) {
  [
    k_axiom(),
    t_axiom(),
    axiom_4(),
    axiom_5(),
    d_axiom(),
    b_axiom(),
    possibility_dual(),
    necessitation_schema(),
  ]
}

/// Get all epistemic axioms
pub fn all_epistemic_axioms() -> List(Axiom) {
  [knowledge_truth(), positive_introspection(), negative_introspection()]
}

/// Get all deontic axioms
pub fn all_deontic_axioms() -> List(Axiom) {
  [d_axiom(), deontic_consistency()]
}

/// Get axioms for a specific logic system
pub fn axioms_for_system(system: LogicSystem) -> List(Axiom) {
  all_standard_axioms()
  |> list.filter(fn(ax) { list.contains(ax.included_in, system) })
}

/// Get the frame property for a logic system
pub fn frame_properties_for_system(system: LogicSystem) -> List(FrameProperty) {
  case system {
    K -> []
    T -> [Reflexive]
    K4 -> [Transitive]
    S4 -> [Reflexive, Transitive]
    S5 -> [Reflexive, Transitive, Symmetric, Euclidean]
    KD -> [Serial]
    KD45 -> [Serial, Transitive, Euclidean]
  }
}

/// Check if an axiom is included in a logic system
pub fn is_axiom_in_system(axiom: Axiom, system: LogicSystem) -> Bool {
  list.contains(axiom.included_in, system)
}

/// Get axiom by ID
pub fn get_axiom_by_id(id: String) -> Option(Axiom) {
  case id {
    "K" -> Some(k_axiom())
    "T" -> Some(t_axiom())
    "4" -> Some(axiom_4())
    "5" -> Some(axiom_5())
    "D" -> Some(d_axiom())
    "B" -> Some(b_axiom())
    "DUAL" -> Some(possibility_dual())
    "NEC" -> Some(necessitation_schema())
    "KT" -> Some(knowledge_truth())
    "KK" -> Some(positive_introspection())
    "NK" -> Some(negative_introspection())
    "DC" -> Some(deontic_consistency())
    _ -> None
  }
}

/// Format axiom as string
pub fn axiom_to_string(axiom: Axiom) -> String {
  axiom.id <> " (" <> axiom.name <> "): " <> axiom.description
}

/// Format frame property as string
pub fn frame_property_to_string(prop: FrameProperty) -> String {
  case prop {
    Reflexive -> "Reflexive (R(w,w) for all w)"
    Transitive -> "Transitive (R(w,v) ∧ R(v,u) → R(w,u))"
    Symmetric -> "Symmetric (R(w,v) → R(v,w))"
    Euclidean -> "Euclidean (R(w,v) ∧ R(w,u) → R(v,u))"
    Serial -> "Serial (∀w. ∃v. R(w,v))"
  }
}
