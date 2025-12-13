//// Frame Condition Constraints Module
////
//// This module provides constraints for different modal logic systems.
//// Each system is characterized by properties of the accessibility relation:
////
//// - K: No constraints (basic modal logic)
//// - T: Reflexive (R(w,w) for all w)
//// - K4: Transitive (R(w,v) ∧ R(v,u) → R(w,u))
//// - S4: Reflexive + Transitive
//// - S5: Reflexive + Symmetric + Transitive (equivalence relation)
//// - KD: Serial (∀w. ∃v. R(w,v))
//// - KD45: Serial + Transitive + Euclidean
////
//// ## Usage
////
//// ```gleam
//// import z3/modal/frame_conditions.{S4, get_constraints}
//// import z3/modal/kripke
////
//// let ctx = kripke.new_context()
//// let worlds = ["w0", "w1", "w2"]
//// let constraints = get_constraints(S4, ctx, worlds)
//// ```

import gleam/list
import z3/modal/kripke.{type KripkeContext}
import z3/types.{type Expr, And, Exists, ForAll, Implies, Or}

// =============================================================================
// Modal System Types
// =============================================================================

/// Modal logic systems defined by frame conditions
pub type ModalSystem {
  /// K - Basic modal logic (no frame conditions)
  K
  /// T - Reflexive frames (□p → p is valid)
  T
  /// K4 - Transitive frames (□p → □□p is valid)
  K4
  /// S4 - Reflexive + Transitive (□p → p, □p → □□p)
  S4
  /// S5 - Equivalence relation (universal modality)
  S5
  /// KD - Serial frames (□p → ◇p is valid)
  KD
  /// KD45 - Serial + Transitive + Euclidean (doxastic logic)
  KD45
  /// B - Reflexive + Symmetric (□p → ◇□p)
  B
  /// KB - Symmetric
  KB
}

/// Frame property types
pub type FrameProperty {
  /// R(w, w) for all worlds
  Reflexivity
  /// R(w, v) → R(v, w)
  Symmetry
  /// R(w, v) ∧ R(v, u) → R(w, u)
  Transitivity
  /// ∀w. ∃v. R(w, v)
  Seriality
  /// R(w, v) ∧ R(w, u) → R(v, u)
  Euclidean
}

// =============================================================================
// System Properties
// =============================================================================

/// Get the frame properties required by a modal system
pub fn system_properties(system: ModalSystem) -> List(FrameProperty) {
  case system {
    K -> []
    T -> [Reflexivity]
    K4 -> [Transitivity]
    S4 -> [Reflexivity, Transitivity]
    S5 -> [Reflexivity, Symmetry, Transitivity]
    KD -> [Seriality]
    KD45 -> [Seriality, Transitivity, Euclidean]
    B -> [Reflexivity, Symmetry]
    KB -> [Symmetry]
  }
}

/// Get a human-readable description of a modal system
pub fn system_description(system: ModalSystem) -> String {
  case system {
    K -> "K (basic modal logic)"
    T -> "T (reflexive frames)"
    K4 -> "K4 (transitive frames)"
    S4 -> "S4 (reflexive + transitive)"
    S5 -> "S5 (equivalence relation)"
    KD -> "KD (serial frames)"
    KD45 -> "KD45 (serial + transitive + euclidean)"
    B -> "B (reflexive + symmetric)"
    KB -> "KB (symmetric frames)"
  }
}

/// Get a human-readable description of a frame property
pub fn property_description(prop: FrameProperty) -> String {
  case prop {
    Reflexivity -> "Reflexivity: R(w, w)"
    Symmetry -> "Symmetry: R(w, v) -> R(v, w)"
    Transitivity -> "Transitivity: R(w, v) & R(v, u) -> R(w, u)"
    Seriality -> "Seriality: forall w. exists v. R(w, v)"
    Euclidean -> "Euclidean: R(w, v) & R(w, u) -> R(v, u)"
  }
}

// =============================================================================
// Constraint Generation
// =============================================================================

/// Get all frame constraints for a modal system given a set of world names
pub fn get_constraints(
  system: ModalSystem,
  ctx: KripkeContext,
  world_names: List(String),
) -> List(Expr) {
  let properties = system_properties(system)
  list.flat_map(properties, fn(prop) {
    property_constraints(prop, ctx, world_names)
  })
}

/// Get constraints for a single frame property
pub fn property_constraints(
  prop: FrameProperty,
  ctx: KripkeContext,
  world_names: List(String),
) -> List(Expr) {
  case prop {
    Reflexivity -> reflexivity_constraints(ctx, world_names)
    Symmetry -> symmetry_constraints(ctx, world_names)
    Transitivity -> transitivity_constraints(ctx, world_names)
    Seriality -> seriality_constraints(ctx, world_names)
    Euclidean -> euclidean_constraints(ctx, world_names)
  }
}

/// Get a single universal constraint for a property (using quantifiers)
pub fn get_universal_constraint(prop: FrameProperty, ctx: KripkeContext) -> Expr {
  let world_sort = kripke.world_sort(ctx)
  case prop {
    Reflexivity -> {
      // ∀w. R(w, w)
      let w = kripke.world(ctx, "w")
      let body = kripke.accessible(ctx, w, w)
      ForAll([#("w", world_sort)], body)
    }

    Symmetry -> {
      // ∀w, v. R(w, v) → R(v, w)
      let w = kripke.world(ctx, "w")
      let v = kripke.world(ctx, "v")
      let premise = kripke.accessible(ctx, w, v)
      let conclusion = kripke.accessible(ctx, v, w)
      ForAll(
        [#("w", world_sort), #("v", world_sort)],
        Implies(premise, conclusion),
      )
    }

    Transitivity -> {
      // ∀w, v, u. R(w, v) ∧ R(v, u) → R(w, u)
      let w = kripke.world(ctx, "w")
      let v = kripke.world(ctx, "v")
      let u = kripke.world(ctx, "u")
      let premise =
        And([kripke.accessible(ctx, w, v), kripke.accessible(ctx, v, u)])
      let conclusion = kripke.accessible(ctx, w, u)
      ForAll(
        [#("w", world_sort), #("v", world_sort), #("u", world_sort)],
        Implies(premise, conclusion),
      )
    }

    Seriality -> {
      // ∀w. ∃v. R(w, v)
      let w = kripke.world(ctx, "w")
      let v = kripke.world(ctx, "v")
      let body = kripke.accessible(ctx, w, v)
      ForAll([#("w", world_sort)], Exists([#("v", world_sort)], body))
    }

    Euclidean -> {
      // ∀w, v, u. R(w, v) ∧ R(w, u) → R(v, u)
      let w = kripke.world(ctx, "w")
      let v = kripke.world(ctx, "v")
      let u = kripke.world(ctx, "u")
      let premise =
        And([kripke.accessible(ctx, w, v), kripke.accessible(ctx, w, u)])
      let conclusion = kripke.accessible(ctx, v, u)
      ForAll(
        [#("w", world_sort), #("v", world_sort), #("u", world_sort)],
        Implies(premise, conclusion),
      )
    }
  }
}

/// Get all universal constraints for a modal system
pub fn get_universal_constraints(
  system: ModalSystem,
  ctx: KripkeContext,
) -> List(Expr) {
  let properties = system_properties(system)
  list.map(properties, fn(prop) { get_universal_constraint(prop, ctx) })
}

// =============================================================================
// Ground Constraint Generation (for finite worlds)
// =============================================================================

/// Generate reflexivity constraints: R(w, w) for each world
fn reflexivity_constraints(
  ctx: KripkeContext,
  world_names: List(String),
) -> List(Expr) {
  list.map(world_names, fn(w_name) {
    let w = kripke.world(ctx, w_name)
    kripke.accessible(ctx, w, w)
  })
}

/// Generate symmetry constraints: R(w, v) → R(v, w) for all pairs
fn symmetry_constraints(
  ctx: KripkeContext,
  world_names: List(String),
) -> List(Expr) {
  let pairs = list_pairs(world_names)
  list.flat_map(pairs, fn(pair) {
    let #(w_name, v_name) = pair
    let w = kripke.world(ctx, w_name)
    let v = kripke.world(ctx, v_name)
    // R(w, v) → R(v, w) and R(v, w) → R(w, v)
    [
      Implies(kripke.accessible(ctx, w, v), kripke.accessible(ctx, v, w)),
      Implies(kripke.accessible(ctx, v, w), kripke.accessible(ctx, w, v)),
    ]
  })
}

/// Generate transitivity constraints: R(w, v) ∧ R(v, u) → R(w, u) for all triples
fn transitivity_constraints(
  ctx: KripkeContext,
  world_names: List(String),
) -> List(Expr) {
  let triples = list_triples(world_names)
  list.map(triples, fn(triple) {
    let #(w_name, v_name, u_name) = triple
    let w = kripke.world(ctx, w_name)
    let v = kripke.world(ctx, v_name)
    let u = kripke.world(ctx, u_name)
    let premise =
      And([kripke.accessible(ctx, w, v), kripke.accessible(ctx, v, u)])
    let conclusion = kripke.accessible(ctx, w, u)
    Implies(premise, conclusion)
  })
}

/// Generate seriality constraints: ∃v. R(w, v) for each world
/// In finite setting: R(w, w0) ∨ R(w, w1) ∨ ... for each w
fn seriality_constraints(
  ctx: KripkeContext,
  world_names: List(String),
) -> List(Expr) {
  list.map(world_names, fn(w_name) {
    let w = kripke.world(ctx, w_name)
    let accessible_to_any =
      list.map(world_names, fn(v_name) {
        let v = kripke.world(ctx, v_name)
        kripke.accessible(ctx, w, v)
      })
    Or(accessible_to_any)
  })
}

/// Generate euclidean constraints: R(w, v) ∧ R(w, u) → R(v, u) for all triples
fn euclidean_constraints(
  ctx: KripkeContext,
  world_names: List(String),
) -> List(Expr) {
  let triples = list_triples(world_names)
  list.map(triples, fn(triple) {
    let #(w_name, v_name, u_name) = triple
    let w = kripke.world(ctx, w_name)
    let v = kripke.world(ctx, v_name)
    let u = kripke.world(ctx, u_name)
    let premise =
      And([kripke.accessible(ctx, w, v), kripke.accessible(ctx, w, u)])
    let conclusion = kripke.accessible(ctx, v, u)
    Implies(premise, conclusion)
  })
}

// =============================================================================
// Axiom Schemas
// =============================================================================

/// Get characteristic axiom schemas for a modal system as modal formulas
pub fn characteristic_axioms(system: ModalSystem) -> List(String) {
  case system {
    K -> ["K: □(p → q) → (□p → □q)"]
    T -> ["K", "T: □p → p"]
    K4 -> ["K", "4: □p → □□p"]
    S4 -> ["K", "T: □p → p", "4: □p → □□p"]
    S5 -> ["K", "T: □p → p", "5: ◇p → □◇p"]
    KD -> ["K", "D: □p → ◇p"]
    KD45 -> ["K", "D: □p → ◇p", "4: □p → □□p", "5: ◇p → □◇p"]
    B -> ["K", "T: □p → p", "B: p → □◇p"]
    KB -> ["K", "B: p → □◇p"]
  }
}

// =============================================================================
// Validation
// =============================================================================

/// Check if a system implies another (by frame property inclusion)
pub fn system_implies(stronger: ModalSystem, weaker: ModalSystem) -> Bool {
  let stronger_props = system_properties(stronger)
  let weaker_props = system_properties(weaker)
  list.all(weaker_props, fn(prop) { list.contains(stronger_props, prop) })
}

/// Get the strongest common system for two systems
pub fn common_system(s1: ModalSystem, s2: ModalSystem) -> ModalSystem {
  // Return K as the common base if no stronger common system
  case system_implies(s1, s2) {
    True -> s1
    False ->
      case system_implies(s2, s1) {
        True -> s2
        False -> K
      }
  }
}

// =============================================================================
// Internal Helpers
// =============================================================================

/// Generate all pairs from a list
fn list_pairs(items: List(a)) -> List(#(a, a)) {
  case items {
    [] -> []
    [first, ..rest] -> {
      let pairs_with_first = list.map(rest, fn(item) { #(first, item) })
      list.append(pairs_with_first, list_pairs(rest))
    }
  }
}

/// Generate all triples from a list (with repetition allowed)
fn list_triples(items: List(a)) -> List(#(a, a, a)) {
  list.flat_map(items, fn(w) {
    list.flat_map(items, fn(v) { list.map(items, fn(u) { #(w, v, u) }) })
  })
}
