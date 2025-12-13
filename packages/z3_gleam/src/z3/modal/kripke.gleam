//// Kripke Frame Encoding Module
////
//// This module provides utilities for encoding modal logic in Z3 using
//// the standard translation to first-order logic. It defines:
//// - World sort (uninterpreted sort for possible worlds)
//// - Accessibility relation R(w1, w2)
//// - Proposition predicates P(w)
////
//// ## Standard Translation
////
//// Modal formulas are translated to first-order logic:
//// - □φ becomes ∀w'. R(w,w') → φ[w'/w]
//// - ◇φ becomes ∃w'. R(w,w') ∧ φ[w'/w]
//// - Atomic p becomes P(w)
////
//// ## Usage
////
//// ```gleam
//// import z3/modal/kripke
////
//// // Create a Kripke frame context
//// let ctx = kripke.new_context()
////
//// // Create world variables
//// let w0 = kripke.world("w0")
//// let w1 = kripke.world("w1")
////
//// // Create accessibility relation
//// let accessible = kripke.accessible(ctx, w0, w1)
////
//// // Create proposition at a world
//// let p_at_w0 = kripke.prop_at(ctx, "p", w0)
//// ```

import gleam/list
import z3/types.{
  type Expr, type Sort, And, Const, Exists, ForAll, Implies, Not, Or,
  UninterpretedSort,
}

// =============================================================================
// Types
// =============================================================================

/// Context for Kripke frame encoding
/// Tracks declared propositions and world variables
pub type KripkeContext {
  KripkeContext(
    /// Counter for generating fresh world names
    world_counter: Int,
    /// Declared proposition names
    propositions: List(String),
    /// World sort name (for consistency)
    world_sort_name: String,
    /// Accessibility relation name
    accessibility_name: String,
  )
}

/// A modal formula in abstract syntax
pub type ModalFormula {
  /// Atomic proposition
  Atom(name: String)
  /// Negation
  ModalNot(ModalFormula)
  /// Conjunction
  ModalAnd(List(ModalFormula))
  /// Disjunction
  ModalOr(List(ModalFormula))
  /// Implication
  ModalImplies(ModalFormula, ModalFormula)
  /// Biconditional
  ModalIff(ModalFormula, ModalFormula)
  /// Necessity (box)
  Box(ModalFormula)
  /// Possibility (diamond)
  Diamond(ModalFormula)
  /// True constant
  ModalTrue
  /// False constant
  ModalFalse
}

// =============================================================================
// Context Management
// =============================================================================

/// Create a new Kripke context with default settings
pub fn new_context() -> KripkeContext {
  KripkeContext(
    world_counter: 0,
    propositions: [],
    world_sort_name: "World",
    accessibility_name: "R",
  )
}

/// Create a context with custom names
pub fn new_context_with_names(
  world_sort: String,
  accessibility: String,
) -> KripkeContext {
  KripkeContext(
    world_counter: 0,
    propositions: [],
    world_sort_name: world_sort,
    accessibility_name: accessibility,
  )
}

/// Register a proposition in the context
pub fn register_proposition(ctx: KripkeContext, name: String) -> KripkeContext {
  case list.contains(ctx.propositions, name) {
    True -> ctx
    False -> KripkeContext(..ctx, propositions: [name, ..ctx.propositions])
  }
}

/// Register multiple propositions
pub fn register_propositions(
  ctx: KripkeContext,
  names: List(String),
) -> KripkeContext {
  list.fold(names, ctx, register_proposition)
}

/// Generate a fresh world variable name
pub fn fresh_world(ctx: KripkeContext) -> #(String, KripkeContext) {
  let name = "w_" <> int_to_string(ctx.world_counter)
  let new_ctx = KripkeContext(..ctx, world_counter: ctx.world_counter + 1)
  #(name, new_ctx)
}

// =============================================================================
// Sort and Variable Constructors
// =============================================================================

/// Get the World sort
pub fn world_sort(ctx: KripkeContext) -> Sort {
  UninterpretedSort(ctx.world_sort_name)
}

/// Create a world variable
pub fn world(ctx: KripkeContext, name: String) -> Expr {
  Const(name, world_sort(ctx))
}

/// Create a named world constant (e.g., the initial world w0)
pub fn world_const(ctx: KripkeContext, name: String) -> Expr {
  Const(name, world_sort(ctx))
}

// =============================================================================
// Accessibility Relation
// =============================================================================

/// Create an accessibility relation predicate R(w1, w2)
/// This creates an uninterpreted function application
pub fn accessible(ctx: KripkeContext, w1: Expr, w2: Expr) -> Expr {
  // R(w1, w2) is encoded as an uninterpreted predicate
  // We use a naming convention: R_w1_w2 as a boolean constant
  // In a full implementation, this would be a function application
  let r_name = ctx.accessibility_name
  make_relation_app(r_name, w1, w2)
}

/// Check if w2 is accessible from w1 (alias for accessible)
pub fn is_accessible(ctx: KripkeContext, from: Expr, to: Expr) -> Expr {
  accessible(ctx, from, to)
}

// =============================================================================
// Proposition Predicates
// =============================================================================

/// Create a proposition predicate P(w) - proposition holds at world w
pub fn prop_at(_ctx: KripkeContext, prop_name: String, world_expr: Expr) -> Expr {
  // P(w) is encoded as an uninterpreted predicate
  make_prop_app(prop_name, world_expr)
}

/// Create a negated proposition at a world
pub fn not_prop_at(
  ctx: KripkeContext,
  prop_name: String,
  world_expr: Expr,
) -> Expr {
  Not(prop_at(ctx, prop_name, world_expr))
}

// =============================================================================
// Standard Translation
// =============================================================================

/// Translate a modal formula to first-order logic at a given world
/// This is the standard translation from modal logic to FOL
pub fn translate(
  ctx: KripkeContext,
  formula: ModalFormula,
  current_world: Expr,
) -> #(Expr, KripkeContext) {
  case formula {
    Atom(name) -> {
      let ctx = register_proposition(ctx, name)
      #(prop_at(ctx, name, current_world), ctx)
    }

    ModalTrue -> #(types.BoolLit(True), ctx)

    ModalFalse -> #(types.BoolLit(False), ctx)

    ModalNot(inner) -> {
      let #(translated, ctx) = translate(ctx, inner, current_world)
      #(Not(translated), ctx)
    }

    ModalAnd(formulas) -> {
      let #(translated_list, ctx) = translate_list(ctx, formulas, current_world)
      #(And(translated_list), ctx)
    }

    ModalOr(formulas) -> {
      let #(translated_list, ctx) = translate_list(ctx, formulas, current_world)
      #(Or(translated_list), ctx)
    }

    ModalImplies(antecedent, consequent) -> {
      let #(trans_ante, ctx) = translate(ctx, antecedent, current_world)
      let #(trans_cons, ctx) = translate(ctx, consequent, current_world)
      #(Implies(trans_ante, trans_cons), ctx)
    }

    ModalIff(left, right) -> {
      let #(trans_left, ctx) = translate(ctx, left, current_world)
      let #(trans_right, ctx) = translate(ctx, right, current_world)
      #(types.Iff(trans_left, trans_right), ctx)
    }

    Box(inner) -> {
      // □φ = ∀w'. R(w, w') → φ[w']
      let #(fresh_name, ctx) = fresh_world(ctx)
      let fresh_world_expr = world(ctx, fresh_name)
      let #(translated_inner, ctx) = translate(ctx, inner, fresh_world_expr)
      let accessibility = accessible(ctx, current_world, fresh_world_expr)
      let body = Implies(accessibility, translated_inner)
      #(ForAll([#(fresh_name, world_sort(ctx))], body), ctx)
    }

    Diamond(inner) -> {
      // ◇φ = ∃w'. R(w, w') ∧ φ[w']
      let #(fresh_name, ctx) = fresh_world(ctx)
      let fresh_world_expr = world(ctx, fresh_name)
      let #(translated_inner, ctx) = translate(ctx, inner, fresh_world_expr)
      let accessibility = accessible(ctx, current_world, fresh_world_expr)
      let body = And([accessibility, translated_inner])
      #(Exists([#(fresh_name, world_sort(ctx))], body), ctx)
    }
  }
}

/// Translate a list of modal formulas
fn translate_list(
  ctx: KripkeContext,
  formulas: List(ModalFormula),
  current_world: Expr,
) -> #(List(Expr), KripkeContext) {
  case formulas {
    [] -> #([], ctx)
    [first, ..rest] -> {
      let #(translated_first, ctx) = translate(ctx, first, current_world)
      let #(translated_rest, ctx) = translate_list(ctx, rest, current_world)
      #([translated_first, ..translated_rest], ctx)
    }
  }
}

/// Translate a formula and create an assertion that it holds at the initial world
pub fn translate_at_initial(
  ctx: KripkeContext,
  formula: ModalFormula,
  initial_world_name: String,
) -> #(Expr, KripkeContext) {
  let initial_world = world(ctx, initial_world_name)
  translate(ctx, formula, initial_world)
}

// =============================================================================
// Modal Formula Constructors (Convenience)
// =============================================================================

/// Create an atomic proposition
pub fn atom(name: String) -> ModalFormula {
  Atom(name)
}

/// Create a negation
pub fn modal_not(formula: ModalFormula) -> ModalFormula {
  ModalNot(formula)
}

/// Create a conjunction
pub fn modal_and(formulas: List(ModalFormula)) -> ModalFormula {
  case formulas {
    [] -> ModalTrue
    [single] -> single
    _ -> ModalAnd(formulas)
  }
}

/// Create a conjunction of two formulas
pub fn modal_and2(left: ModalFormula, right: ModalFormula) -> ModalFormula {
  ModalAnd([left, right])
}

/// Create a disjunction
pub fn modal_or(formulas: List(ModalFormula)) -> ModalFormula {
  case formulas {
    [] -> ModalFalse
    [single] -> single
    _ -> ModalOr(formulas)
  }
}

/// Create a disjunction of two formulas
pub fn modal_or2(left: ModalFormula, right: ModalFormula) -> ModalFormula {
  ModalOr([left, right])
}

/// Create an implication
pub fn modal_implies(
  antecedent: ModalFormula,
  consequent: ModalFormula,
) -> ModalFormula {
  ModalImplies(antecedent, consequent)
}

/// Create a biconditional
pub fn modal_iff(left: ModalFormula, right: ModalFormula) -> ModalFormula {
  ModalIff(left, right)
}

/// Create a necessity (box) formula
pub fn box(formula: ModalFormula) -> ModalFormula {
  Box(formula)
}

/// Create a possibility (diamond) formula
pub fn diamond(formula: ModalFormula) -> ModalFormula {
  Diamond(formula)
}

/// Create nested boxes: □□...□φ (n times)
pub fn boxes(formula: ModalFormula, n: Int) -> ModalFormula {
  case n {
    _ if n <= 0 -> formula
    1 -> Box(formula)
    _ -> Box(boxes(formula, n - 1))
  }
}

/// Create nested diamonds: ◇◇...◇φ (n times)
pub fn diamonds(formula: ModalFormula, n: Int) -> ModalFormula {
  case n {
    _ if n <= 0 -> formula
    1 -> Diamond(formula)
    _ -> Diamond(diamonds(formula, n - 1))
  }
}

// =============================================================================
// Formula Utilities
// =============================================================================

/// Get all atomic propositions in a formula
pub fn get_atoms(formula: ModalFormula) -> List(String) {
  get_atoms_helper(formula, [])
  |> list.unique
}

fn get_atoms_helper(formula: ModalFormula, acc: List(String)) -> List(String) {
  case formula {
    Atom(name) -> [name, ..acc]
    ModalNot(inner) -> get_atoms_helper(inner, acc)
    ModalAnd(formulas) ->
      list.fold(formulas, acc, fn(a, f) { get_atoms_helper(f, a) })
    ModalOr(formulas) ->
      list.fold(formulas, acc, fn(a, f) { get_atoms_helper(f, a) })
    ModalImplies(a, b) -> get_atoms_helper(b, get_atoms_helper(a, acc))
    ModalIff(a, b) -> get_atoms_helper(b, get_atoms_helper(a, acc))
    Box(inner) -> get_atoms_helper(inner, acc)
    Diamond(inner) -> get_atoms_helper(inner, acc)
    ModalTrue -> acc
    ModalFalse -> acc
  }
}

/// Get the modal depth of a formula (max nesting of □ and ◇)
pub fn modal_depth(formula: ModalFormula) -> Int {
  case formula {
    Atom(_) -> 0
    ModalTrue -> 0
    ModalFalse -> 0
    ModalNot(inner) -> modal_depth(inner)
    ModalAnd(formulas) -> max_depth(formulas)
    ModalOr(formulas) -> max_depth(formulas)
    ModalImplies(a, b) -> max(modal_depth(a), modal_depth(b))
    ModalIff(a, b) -> max(modal_depth(a), modal_depth(b))
    Box(inner) -> 1 + modal_depth(inner)
    Diamond(inner) -> 1 + modal_depth(inner)
  }
}

fn max_depth(formulas: List(ModalFormula)) -> Int {
  case formulas {
    [] -> 0
    [first, ..rest] -> max(modal_depth(first), max_depth(rest))
  }
}

fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

// =============================================================================
// Internal Helpers
// =============================================================================

/// Create a relation application R(w1, w2)
/// Encoded as a unique boolean constant based on the relation and arguments
fn make_relation_app(relation_name: String, arg1: Expr, arg2: Expr) -> Expr {
  // For now, we encode R(w1, w2) as a boolean constant with a derived name
  // A more sophisticated implementation would use uninterpreted functions
  let name =
    relation_name <> "_" <> expr_to_name(arg1) <> "_" <> expr_to_name(arg2)
  Const(name, types.BoolSort)
}

/// Create a proposition application P(w)
fn make_prop_app(prop_name: String, world_expr: Expr) -> Expr {
  let name = prop_name <> "_" <> expr_to_name(world_expr)
  Const(name, types.BoolSort)
}

/// Extract a name from a world expression
fn expr_to_name(e: Expr) -> String {
  case e {
    Const(name, _) -> name
    _ -> "expr"
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_string(-n, "")
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
