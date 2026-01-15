//// Modal Logic to Z3 Expression Compilation Module
////
//// This module provides functions to translate modal logic propositions
//// and formalizations into Z3 expressions using Kripke semantics.
////
//// ## Kripke Semantics Encoding
////
//// Modal formulas are encoded using a bounded Kripke model:
//// - Worlds are represented as integers (0 to max_worlds-1)
//// - World 0 is the "actual" world where the argument is evaluated
//// - Accessibility relation R(w1, w2) is encoded as boolean variables
//// - Propositions are world-indexed: p_w0, p_w1, etc.
////
//// ## Modal Operators
////
//// - □p (Necessary p): For all accessible worlds w', p holds at w'
//// - ◇p (Possible p): There exists an accessible world w' where p holds
////
//// ## Usage
////
//// ```gleam
//// import z3/modal/compile
//// import modal_logic/proposition.{Necessary, Atom, And, K, S4}
////
//// // Compile a proposition at world 0
//// let prop = Necessary(Atom("p"))
//// let z3_expr = compile.compile_proposition(prop, 0, 3)
////
//// // Generate frame constraints for S4
//// let constraints = compile.compile_frame_constraints(S4, 3)
//// ```

import gleam/int
import gleam/list
import z3/types.{type Expr, And, BoolSort, Const, Implies, Not, Or}

// =============================================================================
// Proposition Compilation
// =============================================================================

/// Proposition type from modal_logic (replicated here to avoid circular deps)
pub type Proposition {
  Atom(String)
  PropNot(Proposition)
  PropAnd(Proposition, Proposition)
  PropOr(Proposition, Proposition)
  PropImplies(Proposition, Proposition)
  Necessary(Proposition)
  Possible(Proposition)
  Obligatory(Proposition)
  Permitted(Proposition)
  Knows(agent: String, proposition: Proposition)
  Believes(agent: String, proposition: Proposition)
}

/// Logic system type
pub type LogicSystem {
  K
  T
  K4
  S4
  S5
  KD
  KD45
}

/// Compile a modal proposition to a Z3 expression at a given world
///
/// The proposition is evaluated at the specified world index.
/// Modal operators (□, ◇) quantify over accessible worlds.
pub fn compile_proposition(
  prop: Proposition,
  world: Int,
  max_worlds: Int,
) -> Expr {
  case prop {
    Atom(name) -> {
      // Atomic propositions are world-indexed boolean variables
      let var_name = name <> "_w" <> int.to_string(world)
      Const(var_name, BoolSort)
    }

    PropNot(inner) -> {
      Not(compile_proposition(inner, world, max_worlds))
    }

    PropAnd(left, right) -> {
      And([
        compile_proposition(left, world, max_worlds),
        compile_proposition(right, world, max_worlds),
      ])
    }

    PropOr(left, right) -> {
      Or([
        compile_proposition(left, world, max_worlds),
        compile_proposition(right, world, max_worlds),
      ])
    }

    PropImplies(antecedent, consequent) -> {
      Implies(
        compile_proposition(antecedent, world, max_worlds),
        compile_proposition(consequent, world, max_worlds),
      )
    }

    Necessary(inner) -> {
      // □p at world w means: for all w' accessible from w, p holds at w'
      // Encoded as: R(w,0) → p_0 ∧ R(w,1) → p_1 ∧ ... ∧ R(w,n-1) → p_{n-1}
      let world_indices = list.range(0, max_worlds - 1)
      let implications =
        list.map(world_indices, fn(target_world) {
          let accessibility = accessibility_var(world, target_world)
          let inner_at_target =
            compile_proposition(inner, target_world, max_worlds)
          Implies(accessibility, inner_at_target)
        })
      And(implications)
    }

    Possible(inner) -> {
      // ◇p at world w means: there exists w' accessible from w where p holds
      // Encoded as: (R(w,0) ∧ p_0) ∨ (R(w,1) ∧ p_1) ∨ ... ∨ (R(w,n-1) ∧ p_{n-1})
      let world_indices = list.range(0, max_worlds - 1)
      let disjuncts =
        list.map(world_indices, fn(target_world) {
          let accessibility = accessibility_var(world, target_world)
          let inner_at_target =
            compile_proposition(inner, target_world, max_worlds)
          And([accessibility, inner_at_target])
        })
      Or(disjuncts)
    }

    Obligatory(inner) -> {
      // Deontic obligation uses same encoding as necessity
      compile_proposition(Necessary(inner), world, max_worlds)
    }

    Permitted(inner) -> {
      // Deontic permission uses same encoding as possibility
      compile_proposition(Possible(inner), world, max_worlds)
    }

    Knows(agent, inner) -> {
      // Epistemic knowledge with agent-indexed accessibility
      let world_indices = list.range(0, max_worlds - 1)
      let implications =
        list.map(world_indices, fn(target_world) {
          let accessibility =
            epistemic_accessibility_var(agent, world, target_world)
          let inner_at_target =
            compile_proposition(inner, target_world, max_worlds)
          Implies(accessibility, inner_at_target)
        })
      And(implications)
    }

    Believes(agent, inner) -> {
      // Epistemic belief - same encoding structure as knowledge
      let world_indices = list.range(0, max_worlds - 1)
      let implications =
        list.map(world_indices, fn(target_world) {
          let accessibility =
            doxastic_accessibility_var(agent, world, target_world)
          let inner_at_target =
            compile_proposition(inner, target_world, max_worlds)
          Implies(accessibility, inner_at_target)
        })
      And(implications)
    }
  }
}

// =============================================================================
// Frame Constraints
// =============================================================================

/// Compile frame constraints for a logic system
///
/// Returns a list of Z3 expressions that constrain the accessibility relation
/// according to the frame properties of the logic system.
pub fn compile_frame_constraints(
  system: LogicSystem,
  max_worlds: Int,
) -> List(Expr) {
  case system {
    K -> {
      // K: Base modal logic - no frame constraints
      []
    }

    T -> {
      // T: Reflexive (□p → p)
      reflexivity_constraints(max_worlds)
    }

    K4 -> {
      // K4: Transitive (□p → □□p)
      transitivity_constraints(max_worlds)
    }

    S4 -> {
      // S4: Reflexive + Transitive
      list.append(
        reflexivity_constraints(max_worlds),
        transitivity_constraints(max_worlds),
      )
    }

    S5 -> {
      // S5: Reflexive + Symmetric + Transitive (equivalence relation)
      list.flatten([
        reflexivity_constraints(max_worlds),
        symmetry_constraints(max_worlds),
        transitivity_constraints(max_worlds),
      ])
    }

    KD -> {
      // KD: Serial (□p → ◇p)
      seriality_constraints(max_worlds)
    }

    KD45 -> {
      // KD45: Serial + Transitive + Euclidean
      list.flatten([
        seriality_constraints(max_worlds),
        transitivity_constraints(max_worlds),
        euclidean_constraints(max_worlds),
      ])
    }
  }
}

/// Reflexivity: For all w, R(w, w)
fn reflexivity_constraints(max_worlds: Int) -> List(Expr) {
  list.range(0, max_worlds - 1)
  |> list.map(fn(w) { accessibility_var(w, w) })
}

/// Transitivity: For all w1, w2, w3: R(w1, w2) ∧ R(w2, w3) → R(w1, w3)
fn transitivity_constraints(max_worlds: Int) -> List(Expr) {
  let worlds = list.range(0, max_worlds - 1)

  list.flat_map(worlds, fn(w1) {
    list.flat_map(worlds, fn(w2) {
      list.map(worlds, fn(w3) {
        // R(w1, w2) ∧ R(w2, w3) → R(w1, w3)
        Implies(
          And([accessibility_var(w1, w2), accessibility_var(w2, w3)]),
          accessibility_var(w1, w3),
        )
      })
    })
  })
}

/// Symmetry: For all w1, w2: R(w1, w2) → R(w2, w1)
fn symmetry_constraints(max_worlds: Int) -> List(Expr) {
  let worlds = list.range(0, max_worlds - 1)

  list.flat_map(worlds, fn(w1) {
    list.map(worlds, fn(w2) {
      // R(w1, w2) → R(w2, w1)
      Implies(accessibility_var(w1, w2), accessibility_var(w2, w1))
    })
  })
}

/// Seriality: For all w, there exists w' such that R(w, w')
fn seriality_constraints(max_worlds: Int) -> List(Expr) {
  let worlds = list.range(0, max_worlds - 1)

  list.map(worlds, fn(w) {
    // For each world w, at least one world is accessible
    let accessible_worlds =
      list.map(worlds, fn(w2) { accessibility_var(w, w2) })
    Or(accessible_worlds)
  })
}

/// Euclidean: For all w1, w2, w3: R(w1, w2) ∧ R(w1, w3) → R(w2, w3)
fn euclidean_constraints(max_worlds: Int) -> List(Expr) {
  let worlds = list.range(0, max_worlds - 1)

  list.flat_map(worlds, fn(w1) {
    list.flat_map(worlds, fn(w2) {
      list.map(worlds, fn(w3) {
        // R(w1, w2) ∧ R(w1, w3) → R(w2, w3)
        Implies(
          And([accessibility_var(w1, w2), accessibility_var(w1, w3)]),
          accessibility_var(w2, w3),
        )
      })
    })
  })
}

// =============================================================================
// Formalization Compilation
// =============================================================================

/// Configuration for compilation
pub type CompileConfig {
  CompileConfig(
    /// Maximum number of worlds to consider
    max_worlds: Int,
    /// The world where the argument is evaluated (usually 0)
    actual_world: Int,
  )
}

/// Default compilation configuration
pub fn default_config() -> CompileConfig {
  CompileConfig(max_worlds: 3, actual_world: 0)
}

/// Create a configuration with specified max worlds
pub fn config_with_worlds(max_worlds: Int) -> CompileConfig {
  CompileConfig(max_worlds: max_worlds, actual_world: 0)
}

/// Compile a formalization to Z3 constraints
///
/// A formalization is valid if there is no model where:
/// - All premises are true at the actual world
/// - The conclusion is false at the actual world
/// - The frame constraints are satisfied
///
/// Returns a list of constraints that, if satisfiable, provide a countermodel.
pub fn compile_validity_check(
  premises: List(Proposition),
  conclusion: Proposition,
  system: LogicSystem,
  config: CompileConfig,
) -> List(Expr) {
  let max_worlds = config.max_worlds
  let actual_world = config.actual_world

  // Frame constraints
  let frame_constraints = compile_frame_constraints(system, max_worlds)

  // Premises must be true at the actual world
  let premise_constraints =
    list.map(premises, fn(premise) {
      compile_proposition(premise, actual_world, max_worlds)
    })

  // Conclusion must be FALSE at the actual world (we're looking for countermodel)
  let negated_conclusion =
    Not(compile_proposition(conclusion, actual_world, max_worlds))

  // Combine all constraints
  list.flatten([frame_constraints, premise_constraints, [negated_conclusion]])
}

/// Compile a satisfiability check for a proposition
///
/// Returns constraints that are satisfiable iff the proposition is satisfiable.
pub fn compile_satisfiability_check(
  prop: Proposition,
  system: LogicSystem,
  config: CompileConfig,
) -> List(Expr) {
  let max_worlds = config.max_worlds
  let actual_world = config.actual_world

  // Frame constraints
  let frame_constraints = compile_frame_constraints(system, max_worlds)

  // Proposition must be true at the actual world
  let prop_constraint = compile_proposition(prop, actual_world, max_worlds)

  list.append(frame_constraints, [prop_constraint])
}

// =============================================================================
// Variable Naming Helpers
// =============================================================================

/// Create an accessibility relation variable R(from, to)
fn accessibility_var(from_world: Int, to_world: Int) -> Expr {
  let name =
    "R_w" <> int.to_string(from_world) <> "_w" <> int.to_string(to_world)
  Const(name, BoolSort)
}

/// Create an epistemic accessibility variable for an agent
fn epistemic_accessibility_var(
  agent: String,
  from_world: Int,
  to_world: Int,
) -> Expr {
  let name =
    "K_"
    <> agent
    <> "_w"
    <> int.to_string(from_world)
    <> "_w"
    <> int.to_string(to_world)
  Const(name, BoolSort)
}

/// Create a doxastic accessibility variable for an agent
fn doxastic_accessibility_var(
  agent: String,
  from_world: Int,
  to_world: Int,
) -> Expr {
  let name =
    "B_"
    <> agent
    <> "_w"
    <> int.to_string(from_world)
    <> "_w"
    <> int.to_string(to_world)
  Const(name, BoolSort)
}

// =============================================================================
// Proposition Conversion Utilities
// =============================================================================

/// Convert a modal_logic/proposition.Proposition to our local Proposition type
/// This is needed because we can't import from modal_logic to avoid circular deps
pub fn from_external_proposition(_external: Dynamic) -> Proposition {
  // This would need proper implementation based on the external type
  // For now, return a placeholder
  Atom("placeholder")
}

/// Type alias for external proposition (to be replaced with proper type)
pub type Dynamic

/// Get all atomic proposition names from a proposition
pub fn collect_atoms(prop: Proposition) -> List(String) {
  case prop {
    Atom(name) -> [name]
    PropNot(inner) -> collect_atoms(inner)
    PropAnd(left, right) ->
      list.append(collect_atoms(left), collect_atoms(right))
    PropOr(left, right) ->
      list.append(collect_atoms(left), collect_atoms(right))
    PropImplies(left, right) ->
      list.append(collect_atoms(left), collect_atoms(right))
    Necessary(inner) -> collect_atoms(inner)
    Possible(inner) -> collect_atoms(inner)
    Obligatory(inner) -> collect_atoms(inner)
    Permitted(inner) -> collect_atoms(inner)
    Knows(_, inner) -> collect_atoms(inner)
    Believes(_, inner) -> collect_atoms(inner)
  }
  |> list.unique
}

/// Count modal depth (maximum nesting of modal operators)
pub fn modal_depth(prop: Proposition) -> Int {
  case prop {
    Atom(_) -> 0
    PropNot(inner) -> modal_depth(inner)
    PropAnd(left, right) -> int.max(modal_depth(left), modal_depth(right))
    PropOr(left, right) -> int.max(modal_depth(left), modal_depth(right))
    PropImplies(left, right) -> int.max(modal_depth(left), modal_depth(right))
    Necessary(inner) -> 1 + modal_depth(inner)
    Possible(inner) -> 1 + modal_depth(inner)
    Obligatory(inner) -> 1 + modal_depth(inner)
    Permitted(inner) -> 1 + modal_depth(inner)
    Knows(_, inner) -> 1 + modal_depth(inner)
    Believes(_, inner) -> 1 + modal_depth(inner)
  }
}

/// Suggest number of worlds based on modal depth
pub fn suggest_max_worlds(prop: Proposition) -> Int {
  // A rough heuristic: depth + 2 worlds is usually sufficient for small formulas
  let depth = modal_depth(prop)
  int.max(2, depth + 2)
}
