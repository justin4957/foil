//// Modal Compile Module Tests
////
//// Tests for the modal logic to Z3 expression compilation module.

import gleam/list
import gleeunit/should
import z3/modal/compile.{
  Atom, Believes, CompileConfig, K, K4, KD, KD45, Knows, Necessary, Obligatory,
  Permitted, Possible, PropAnd, PropImplies, PropNot, PropOr, S4, S5, T,
}
import z3/types.{And, BoolSort, Const, Implies, Not, Or}

// =============================================================================
// Proposition Compilation Tests
// =============================================================================

pub fn compile_atom_test() {
  // Atomic proposition at world 0 should be p_w0
  let prop = Atom("p")
  let result = compile.compile_proposition(prop, 0, 3)

  // Should be a Const with name "p_w0" and BoolSort
  case result {
    Const("p_w0", BoolSort) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_atom_world_1_test() {
  // Atomic proposition at world 1 should be p_w1
  let prop = Atom("q")
  let result = compile.compile_proposition(prop, 1, 3)

  case result {
    Const("q_w1", BoolSort) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_not_test() {
  // Not(p) at world 0 should be Not(p_w0)
  let prop = PropNot(Atom("p"))
  let result = compile.compile_proposition(prop, 0, 3)

  case result {
    Not(Const("p_w0", BoolSort)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_and_test() {
  // And(p, q) at world 0 should be And([p_w0, q_w0])
  let prop = PropAnd(Atom("p"), Atom("q"))
  let result = compile.compile_proposition(prop, 0, 3)

  case result {
    And([Const("p_w0", BoolSort), Const("q_w0", BoolSort)]) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_or_test() {
  // Or(p, q) at world 0 should be Or([p_w0, q_w0])
  let prop = PropOr(Atom("p"), Atom("q"))
  let result = compile.compile_proposition(prop, 0, 3)

  case result {
    Or([Const("p_w0", BoolSort), Const("q_w0", BoolSort)]) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_implies_test() {
  // Implies(p, q) at world 0 should be Implies(p_w0, q_w0)
  let prop = PropImplies(Atom("p"), Atom("q"))
  let result = compile.compile_proposition(prop, 0, 3)

  case result {
    Implies(Const("p_w0", BoolSort), Const("q_w0", BoolSort)) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_necessary_test() {
  // Necessary(p) at world 0 should be And of implications for all accessible worlds
  let prop = Necessary(Atom("p"))
  let result = compile.compile_proposition(prop, 0, 2)

  // With 2 worlds, should be And([R_w0_w0 -> p_w0, R_w0_w1 -> p_w1])
  case result {
    And(implications) -> {
      list.length(implications)
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn compile_possible_test() {
  // Possible(p) at world 0 should be Or of conjunctions for all accessible worlds
  let prop = Possible(Atom("p"))
  let result = compile.compile_proposition(prop, 0, 2)

  // With 2 worlds, should be Or([R_w0_w0 & p_w0, R_w0_w1 & p_w1])
  case result {
    Or(disjuncts) -> {
      list.length(disjuncts)
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn compile_obligatory_test() {
  // Obligatory is encoded as Necessary
  let prop = Obligatory(Atom("p"))
  let result = compile.compile_proposition(prop, 0, 2)

  case result {
    And(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_permitted_test() {
  // Permitted is encoded as Possible
  let prop = Permitted(Atom("p"))
  let result = compile.compile_proposition(prop, 0, 2)

  case result {
    Or(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn compile_knows_test() {
  // Knows uses agent-indexed accessibility
  let prop = Knows("alice", Atom("p"))
  let result = compile.compile_proposition(prop, 0, 2)

  case result {
    And(implications) -> {
      list.length(implications)
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn compile_believes_test() {
  // Believes uses agent-indexed doxastic accessibility
  let prop = Believes("bob", Atom("p"))
  let result = compile.compile_proposition(prop, 0, 2)

  case result {
    And(implications) -> {
      list.length(implications)
      |> should.equal(2)
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Frame Constraint Tests
// =============================================================================

pub fn frame_constraints_k_test() {
  // K has no frame constraints
  let constraints = compile.compile_frame_constraints(K, 3)
  list.length(constraints)
  |> should.equal(0)
}

pub fn frame_constraints_t_test() {
  // T has reflexivity constraints: R(w, w) for each world
  let constraints = compile.compile_frame_constraints(T, 3)
  list.length(constraints)
  |> should.equal(3)
}

pub fn frame_constraints_k4_test() {
  // K4 has transitivity constraints: 3^3 = 27 constraints for 3 worlds
  let constraints = compile.compile_frame_constraints(K4, 3)
  list.length(constraints)
  |> should.equal(27)
}

pub fn frame_constraints_s4_test() {
  // S4 has reflexivity (3) + transitivity (27) = 30 constraints
  let constraints = compile.compile_frame_constraints(S4, 3)
  list.length(constraints)
  |> should.equal(30)
}

pub fn frame_constraints_s5_test() {
  // S5 has reflexivity (3) + symmetry (9) + transitivity (27) = 39 constraints
  let constraints = compile.compile_frame_constraints(S5, 3)
  list.length(constraints)
  |> should.equal(39)
}

pub fn frame_constraints_kd_test() {
  // KD has seriality constraints: 1 per world = 3 constraints
  let constraints = compile.compile_frame_constraints(KD, 3)
  list.length(constraints)
  |> should.equal(3)
}

pub fn frame_constraints_kd45_test() {
  // KD45 has seriality (3) + transitivity (27) + euclidean (27) = 57 constraints
  let constraints = compile.compile_frame_constraints(KD45, 3)
  list.length(constraints)
  |> should.equal(57)
}

// =============================================================================
// Configuration Tests
// =============================================================================

pub fn default_config_test() {
  let config = compile.default_config()
  config.max_worlds
  |> should.equal(3)
  config.actual_world
  |> should.equal(0)
}

pub fn config_with_worlds_test() {
  let config = compile.config_with_worlds(5)
  config.max_worlds
  |> should.equal(5)
  config.actual_world
  |> should.equal(0)
}

// =============================================================================
// Validity Check Tests
// =============================================================================

pub fn validity_check_empty_premises_test() {
  // Validity check with no premises and conclusion p
  let config = CompileConfig(max_worlds: 2, actual_world: 0)
  let constraints = compile.compile_validity_check([], Atom("p"), K, config)

  // Should have: frame constraints (0 for K) + negated conclusion (1)
  list.length(constraints)
  |> should.equal(1)
}

pub fn validity_check_with_premises_test() {
  // Validity check with premise p and conclusion q
  let config = CompileConfig(max_worlds: 2, actual_world: 0)
  let constraints =
    compile.compile_validity_check([Atom("p")], Atom("q"), K, config)

  // Should have: frame constraints (0) + premises (1) + negated conclusion (1) = 2
  list.length(constraints)
  |> should.equal(2)
}

pub fn validity_check_s4_test() {
  // Validity check in S4 (should include frame constraints)
  let config = CompileConfig(max_worlds: 2, actual_world: 0)
  let constraints =
    compile.compile_validity_check([Atom("p")], Atom("q"), S4, config)

  // Should have: reflexivity (2) + transitivity (8) + premise (1) + negated conclusion (1) = 12
  list.length(constraints)
  |> should.equal(12)
}

// =============================================================================
// Satisfiability Check Tests
// =============================================================================

pub fn satisfiability_check_test() {
  let config = CompileConfig(max_worlds: 2, actual_world: 0)
  let constraints = compile.compile_satisfiability_check(Atom("p"), K, config)

  // Should have: frame constraints (0) + proposition (1) = 1
  list.length(constraints)
  |> should.equal(1)
}

pub fn satisfiability_check_s4_test() {
  let config = CompileConfig(max_worlds: 2, actual_world: 0)
  let constraints = compile.compile_satisfiability_check(Atom("p"), S4, config)

  // Should have: reflexivity (2) + transitivity (8) + proposition (1) = 11
  list.length(constraints)
  |> should.equal(11)
}

// =============================================================================
// Utility Function Tests
// =============================================================================

pub fn collect_atoms_single_test() {
  let atoms = compile.collect_atoms(Atom("p"))
  list.length(atoms)
  |> should.equal(1)
  list.contains(atoms, "p")
  |> should.be_true
}

pub fn collect_atoms_complex_test() {
  // Necessary(p & q) should collect both p and q
  let prop = Necessary(PropAnd(Atom("p"), Atom("q")))
  let atoms = compile.collect_atoms(prop)
  list.length(atoms)
  |> should.equal(2)
  list.contains(atoms, "p")
  |> should.be_true
  list.contains(atoms, "q")
  |> should.be_true
}

pub fn collect_atoms_duplicate_test() {
  // p & p should only return one "p" (unique)
  let prop = PropAnd(Atom("p"), Atom("p"))
  let atoms = compile.collect_atoms(prop)
  list.length(atoms)
  |> should.equal(1)
}

pub fn modal_depth_atom_test() {
  let depth = compile.modal_depth(Atom("p"))
  depth
  |> should.equal(0)
}

pub fn modal_depth_necessary_test() {
  let depth = compile.modal_depth(Necessary(Atom("p")))
  depth
  |> should.equal(1)
}

pub fn modal_depth_nested_test() {
  // Necessary(Possible(p)) has depth 2
  let depth = compile.modal_depth(Necessary(Possible(Atom("p"))))
  depth
  |> should.equal(2)
}

pub fn modal_depth_and_test() {
  // Max of (Necessary(p), Atom(q)) = max(1, 0) = 1
  let depth = compile.modal_depth(PropAnd(Necessary(Atom("p")), Atom("q")))
  depth
  |> should.equal(1)
}

pub fn suggest_max_worlds_atom_test() {
  let suggested = compile.suggest_max_worlds(Atom("p"))
  // depth 0 + 2 = 2
  suggested
  |> should.equal(2)
}

pub fn suggest_max_worlds_nested_test() {
  // Depth 2 + 2 = 4
  let suggested = compile.suggest_max_worlds(Necessary(Possible(Atom("p"))))
  suggested
  |> should.equal(4)
}
