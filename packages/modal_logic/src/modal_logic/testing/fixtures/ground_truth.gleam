//// Curated Ground-Truth Test Fixtures
////
//// This module provides independently verified test fixtures with known
//// validity for measuring accuracy against real logic problems rather than
//// synthetic generated data.
////
//// ## Categories
//// - Propositional: Classical argument forms (modus ponens, syllogisms, etc.)
//// - Modal K: Distribution axiom, necessitation rule
//// - Modal T: Reflexivity axiom (□p → p)
//// - Modal S4: Transitivity (□p → □□p) with reflexivity
//// - Modal S5: Euclidean property, epistemic logic
//// - Deontic: Obligation/permission with D-axiom
//// - Epistemic: Knowledge distribution, positive introspection
//// - Cross-system: Valid in one system, invalid in another
//// - Deep nesting: Multiple levels of modal operators
//// - Tier boundary: Cases at the edge of Tier 1/Tier 2 coverage
////
//// ## Source/Justification
//// Each fixture documents its source (standard textbook result, axiom schema,
//// well-known fallacy, etc.) in the `source` field.

import gleam/list
import gleam/option.{None, Some}
import modal_logic/proposition.{
  And, Atom, Believes, Implies, K, K4, KD, KD45, Knows, Necessary, Not,
  Obligatory, Or, Permitted, Possible, S4, S5, T,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture, TestFixture}
import modal_logic/testing/test_config.{
  ClassicArgument, Easy, EdgeCase, ExpectedEither, ExpectedInvalid,
  ExpectedValid, Hard, Medium, ModalTheorem, Research,
}

/// All curated ground-truth fixtures (50+ cases).
///
/// These are independently verified test cases from standard modal logic
/// textbooks and well-established results. Every expected validity has
/// a documented justification.
pub fn all_ground_truth_fixtures() -> List(TestFixture) {
  list.flatten([
    propositional_fixtures(),
    modal_k_fixtures(),
    modal_t_fixtures(),
    modal_s4_fixtures(),
    modal_s5_fixtures(),
    deontic_fixtures(),
    epistemic_fixtures(),
    cross_system_fixtures(),
    deep_nesting_fixtures(),
    tier_boundary_fixtures(),
  ])
}

/// Number of curated ground-truth fixtures.
pub fn ground_truth_count() -> Int {
  list.length(all_ground_truth_fixtures())
}

// =============================================================================
// Propositional Logic (valid in all systems)
// =============================================================================

fn propositional_fixtures() -> List(TestFixture) {
  [
    // 1. Modus ponens
    TestFixture(
      id: "gt-prop-modus-ponens",
      name: "Modus Ponens",
      category: ClassicArgument,
      natural_language: "If P then Q. P. Therefore Q.",
      expected_logic_system: K,
      expected_premises: [Implies(Atom("P"), Atom("Q")), Atom("P")],
      expected_conclusion: Atom("Q"),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "propositional", "valid"],
      source: Some("Standard propositional logic — modus ponens"),
    ),
    // 2. Modus tollens
    TestFixture(
      id: "gt-prop-modus-tollens",
      name: "Modus Tollens",
      category: ClassicArgument,
      natural_language: "If P then Q. Not Q. Therefore not P.",
      expected_logic_system: K,
      expected_premises: [Implies(Atom("P"), Atom("Q")), Not(Atom("Q"))],
      expected_conclusion: Not(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "propositional", "valid"],
      source: Some("Standard propositional logic — modus tollens"),
    ),
    // 3. Hypothetical syllogism
    TestFixture(
      id: "gt-prop-hypothetical-syllogism",
      name: "Hypothetical Syllogism",
      category: ClassicArgument,
      natural_language: "If P then Q. If Q then R. Therefore if P then R.",
      expected_logic_system: K,
      expected_premises: [
        Implies(Atom("P"), Atom("Q")),
        Implies(Atom("Q"), Atom("R")),
      ],
      expected_conclusion: Implies(Atom("P"), Atom("R")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "propositional", "valid"],
      source: Some("Standard propositional logic — hypothetical syllogism"),
    ),
    // 4. Disjunctive syllogism
    TestFixture(
      id: "gt-prop-disjunctive-syllogism",
      name: "Disjunctive Syllogism",
      category: ClassicArgument,
      natural_language: "P or Q. Not P. Therefore Q.",
      expected_logic_system: K,
      expected_premises: [Or(Atom("P"), Atom("Q")), Not(Atom("P"))],
      expected_conclusion: Atom("Q"),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "propositional", "valid"],
      source: Some("Standard propositional logic — disjunctive syllogism"),
    ),
    // 5. Affirming the consequent (INVALID)
    TestFixture(
      id: "gt-prop-affirming-consequent",
      name: "Affirming the Consequent",
      category: ClassicArgument,
      natural_language: "If P then Q. Q. Therefore P.",
      expected_logic_system: K,
      expected_premises: [Implies(Atom("P"), Atom("Q")), Atom("Q")],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedInvalid(Some(
        "Formal fallacy: affirming the consequent",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "propositional", "invalid", "fallacy"],
      source: Some("Standard fallacy — affirming the consequent"),
    ),
    // 6. Denying the antecedent (INVALID)
    TestFixture(
      id: "gt-prop-denying-antecedent",
      name: "Denying the Antecedent",
      category: ClassicArgument,
      natural_language: "If P then Q. Not P. Therefore not Q.",
      expected_logic_system: K,
      expected_premises: [Implies(Atom("P"), Atom("Q")), Not(Atom("P"))],
      expected_conclusion: Not(Atom("Q")),
      expected_validity: ExpectedInvalid(Some(
        "Formal fallacy: denying the antecedent",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "propositional", "invalid", "fallacy"],
      source: Some("Standard fallacy — denying the antecedent"),
    ),
    // 7. Constructive dilemma
    TestFixture(
      id: "gt-prop-constructive-dilemma",
      name: "Constructive Dilemma",
      category: ClassicArgument,
      natural_language: "If P then Q. If R then S. P or R. Therefore Q or S.",
      expected_logic_system: K,
      expected_premises: [
        Implies(Atom("P"), Atom("Q")),
        Implies(Atom("R"), Atom("S")),
        Or(Atom("P"), Atom("R")),
      ],
      expected_conclusion: Or(Atom("Q"), Atom("S")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "propositional", "valid"],
      source: Some("Standard propositional logic — constructive dilemma"),
    ),
    // 8. Ex falso quodlibet (explosion)
    TestFixture(
      id: "gt-prop-explosion",
      name: "Ex Falso Quodlibet",
      category: ClassicArgument,
      natural_language: "P. Not P. Therefore Q.",
      expected_logic_system: K,
      expected_premises: [Atom("P"), Not(Atom("P"))],
      expected_conclusion: Atom("Q"),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "propositional", "valid", "explosion"],
      source: Some("Classical logic — principle of explosion"),
    ),
  ]
}

// =============================================================================
// Modal K (Base modal logic)
// =============================================================================

fn modal_k_fixtures() -> List(TestFixture) {
  [
    // 9. K-axiom (distribution axiom)
    TestFixture(
      id: "gt-k-distribution",
      name: "K Distribution Axiom",
      category: ModalTheorem,
      natural_language: "Necessarily if P then Q. Necessarily P. Therefore necessarily Q.",
      expected_logic_system: K,
      expected_premises: [
        Necessary(Implies(Atom("P"), Atom("Q"))),
        Necessary(Atom("P")),
      ],
      expected_conclusion: Necessary(Atom("Q")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "k", "valid", "axiom"],
      source: Some("K-axiom schema: □(P→Q) → (□P → □Q)"),
    ),
    // 10. Necessitation of tautology
    TestFixture(
      id: "gt-k-necessitation-tautology",
      name: "Necessitation of Tautology",
      category: ModalTheorem,
      natural_language: "Therefore it is necessarily the case that P or not P.",
      expected_logic_system: K,
      expected_premises: [],
      expected_conclusion: Necessary(Or(Atom("P"), Not(Atom("P")))),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "k", "valid", "necessitation"],
      source: Some("Necessitation rule: if ⊢ φ then ⊢ □φ"),
    ),
    // 11. Modal distribution with possibility (INVALID in K)
    TestFixture(
      id: "gt-k-invalid-possible-distribution",
      name: "K: ◇P, ◇Q does not entail ◇(P∧Q)",
      category: ModalTheorem,
      natural_language: "It is possible that P. It is possible that Q. Therefore it is possible that P and Q.",
      expected_logic_system: K,
      expected_premises: [Possible(Atom("P")), Possible(Atom("Q"))],
      expected_conclusion: Possible(And(Atom("P"), Atom("Q"))),
      expected_validity: ExpectedInvalid(Some(
        "◇P ∧ ◇Q does not entail ◇(P∧Q) — P and Q may be possible in different worlds",
      )),
      difficulty: Medium,
      tags: ["ground-truth", "modal", "k", "invalid"],
      source: Some("Standard Kripke semantics — possibility non-distribution"),
    ),
    // 12. Necessity distributes over conjunction
    TestFixture(
      id: "gt-k-necessity-conjunction",
      name: "K: □(P∧Q) entails □P ∧ □Q",
      category: ModalTheorem,
      natural_language: "Necessarily P and Q. Therefore necessarily P.",
      expected_logic_system: K,
      expected_premises: [Necessary(And(Atom("P"), Atom("Q")))],
      expected_conclusion: Necessary(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "k", "valid"],
      source: Some("□(P∧Q) → □P is a theorem of K"),
    ),
    // 13. Necessity does not entail truth in K (INVALID)
    TestFixture(
      id: "gt-k-invalid-nec-to-truth",
      name: "K: □P does not entail P",
      category: ModalTheorem,
      natural_language: "P is necessary. Therefore P is true.",
      expected_logic_system: K,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedInvalid(Some(
        "□P → P requires reflexivity (T-axiom), which K lacks",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "modal", "k", "invalid", "cross-system"],
      source: Some("K lacks T-axiom; □P ⊬ P in K"),
    ),
  ]
}

// =============================================================================
// Modal T (Reflexive frames)
// =============================================================================

fn modal_t_fixtures() -> List(TestFixture) {
  [
    // 14. T-axiom: □P → P
    TestFixture(
      id: "gt-t-axiom",
      name: "T-Axiom: Necessity Implies Truth",
      category: ModalTheorem,
      natural_language: "P is necessarily true. Therefore P is true.",
      expected_logic_system: T,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "t", "valid", "axiom"],
      source: Some("T-axiom schema: □P → P (reflexive frames)"),
    ),
    // 15. T: truth implies possibility
    TestFixture(
      id: "gt-t-truth-implies-possibility",
      name: "T: P implies ◇P",
      category: ModalTheorem,
      natural_language: "P is true. Therefore P is possible.",
      expected_logic_system: T,
      expected_premises: [Atom("P")],
      expected_conclusion: Possible(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "t", "valid"],
      source: Some("P → ◇P is a theorem of T (dual of T-axiom)"),
    ),
    // 16. T: □P → □□P fails (INVALID — no transitivity)
    TestFixture(
      id: "gt-t-invalid-4-axiom",
      name: "T: 4-Axiom Fails",
      category: ModalTheorem,
      natural_language: "P is necessarily true. Therefore P is necessarily necessarily true.",
      expected_logic_system: T,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Necessary(Necessary(Atom("P"))),
      expected_validity: ExpectedInvalid(Some(
        "□P → □□P requires transitivity (4-axiom), which T lacks",
      )),
      difficulty: Medium,
      tags: ["ground-truth", "modal", "t", "invalid", "cross-system"],
      source: Some("T lacks 4-axiom; □P ⊬ □□P in T"),
    ),
    // 17. T with K distribution
    TestFixture(
      id: "gt-t-k-distribution",
      name: "T: K+T Combined",
      category: ModalTheorem,
      natural_language: "Necessarily if P then Q. Necessarily P. Therefore Q.",
      expected_logic_system: T,
      expected_premises: [
        Necessary(Implies(Atom("P"), Atom("Q"))),
        Necessary(Atom("P")),
      ],
      expected_conclusion: Atom("Q"),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "modal", "t", "valid"],
      source: Some("K-axiom gives □Q, T-axiom gives Q from □Q"),
    ),
  ]
}

// =============================================================================
// Modal S4 (Reflexive + Transitive)
// =============================================================================

fn modal_s4_fixtures() -> List(TestFixture) {
  [
    // 18. 4-axiom: □P → □□P
    TestFixture(
      id: "gt-s4-4-axiom",
      name: "S4: 4-Axiom Valid",
      category: ModalTheorem,
      natural_language: "P is necessarily true. Therefore P is necessarily necessarily true.",
      expected_logic_system: S4,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Necessary(Necessary(Atom("P"))),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "s4", "valid", "axiom"],
      source: Some("4-axiom schema: □P → □□P (transitive frames)"),
    ),
    // 19. S4: ◇◇P → ◇P
    TestFixture(
      id: "gt-s4-possible-collapse",
      name: "S4: ◇◇P Implies ◇P",
      category: ModalTheorem,
      natural_language: "It is possibly possibly true that P. Therefore P is possible.",
      expected_logic_system: S4,
      expected_premises: [Possible(Possible(Atom("P")))],
      expected_conclusion: Possible(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "modal", "s4", "valid"],
      source: Some("In S4: ◇◇P → ◇P (dual of 4-axiom)"),
    ),
    // 20. S4: 5-axiom fails (INVALID — no euclidean)
    TestFixture(
      id: "gt-s4-invalid-5-axiom",
      name: "S4: 5-Axiom Fails",
      category: ModalTheorem,
      natural_language: "P is possibly true. Therefore P is necessarily possible.",
      expected_logic_system: S4,
      expected_premises: [Possible(Atom("P"))],
      expected_conclusion: Necessary(Possible(Atom("P"))),
      expected_validity: ExpectedInvalid(Some(
        "◇P → □◇P requires euclidean property (5-axiom), which S4 lacks",
      )),
      difficulty: Medium,
      tags: ["ground-truth", "modal", "s4", "invalid", "cross-system"],
      source: Some("S4 lacks 5-axiom; ◇P ⊬ □◇P in S4"),
    ),
    // 21. S4: T-axiom still holds
    TestFixture(
      id: "gt-s4-t-axiom-holds",
      name: "S4: T-Axiom Holds",
      category: ModalTheorem,
      natural_language: "P is necessarily true. Therefore P is true.",
      expected_logic_system: S4,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "s4", "valid"],
      source: Some("S4 includes T, so □P → P holds"),
    ),
  ]
}

// =============================================================================
// Modal S5 (Equivalence relation)
// =============================================================================

fn modal_s5_fixtures() -> List(TestFixture) {
  [
    // 22. 5-axiom: ◇P → □◇P
    TestFixture(
      id: "gt-s5-5-axiom",
      name: "S5: 5-Axiom Valid",
      category: ModalTheorem,
      natural_language: "P is possible. Therefore P is necessarily possible.",
      expected_logic_system: S5,
      expected_premises: [Possible(Atom("P"))],
      expected_conclusion: Necessary(Possible(Atom("P"))),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "modal", "s5", "valid", "axiom"],
      source: Some("5-axiom schema: ◇P → □◇P (euclidean frames)"),
    ),
    // 23. S5: ◇□P → □P
    TestFixture(
      id: "gt-s5-possible-necessary-collapse",
      name: "S5: ◇□P Implies □P",
      category: ModalTheorem,
      natural_language: "It is possibly necessarily true that P. Therefore P is necessary.",
      expected_logic_system: S5,
      expected_premises: [Possible(Necessary(Atom("P")))],
      expected_conclusion: Necessary(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Hard,
      tags: ["ground-truth", "modal", "s5", "valid"],
      source: Some("In S5: ◇□P → □P (characteristic theorem)"),
    ),
    // 24. S5: ◇P does not entail □P (INVALID even in S5)
    TestFixture(
      id: "gt-s5-invalid-possible-to-necessary",
      name: "S5: ◇P Does Not Entail □P",
      category: ModalTheorem,
      natural_language: "P is possible. Therefore P is necessary.",
      expected_logic_system: S5,
      expected_premises: [Possible(Atom("P"))],
      expected_conclusion: Necessary(Atom("P")),
      expected_validity: ExpectedInvalid(Some(
        "◇P → □P is not valid in any normal modal logic",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "modal", "s5", "invalid"],
      source: Some("◇P ⊬ □P in S5 (or any normal modal logic)"),
    ),
    // 25. S5 iteration collapse: □□P ↔ □P
    TestFixture(
      id: "gt-s5-iteration-collapse",
      name: "S5: Iterated Necessity Collapse",
      category: ModalTheorem,
      natural_language: "It is necessarily necessarily true that P. Therefore P is necessary.",
      expected_logic_system: S5,
      expected_premises: [Necessary(Necessary(Atom("P")))],
      expected_conclusion: Necessary(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "s5", "valid"],
      source: Some("In S5: □□P → □P (iterated modalities collapse)"),
    ),
    // 26. S5 possibility iteration: ◇◇P → ◇P
    TestFixture(
      id: "gt-s5-possibility-iteration",
      name: "S5: Iterated Possibility Collapse",
      category: ModalTheorem,
      natural_language: "It is possibly possibly true that P. Therefore P is possible.",
      expected_logic_system: S5,
      expected_premises: [Possible(Possible(Atom("P")))],
      expected_conclusion: Possible(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "modal", "s5", "valid"],
      source: Some("In S5: ◇◇P → ◇P (inherited from S4)"),
    ),
  ]
}

// =============================================================================
// Deontic Logic (KD)
// =============================================================================

fn deontic_fixtures() -> List(TestFixture) {
  [
    // 27. D-axiom: O(P) → P(P) (obligation implies permission)
    TestFixture(
      id: "gt-kd-d-axiom",
      name: "KD: D-Axiom (Obligation Implies Permission)",
      category: ModalTheorem,
      natural_language: "It is obligatory that P. Therefore P is permitted.",
      expected_logic_system: KD,
      expected_premises: [Obligatory(Atom("P"))],
      expected_conclusion: Permitted(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "deontic", "kd", "valid", "axiom"],
      source: Some("D-axiom schema: O(P) → P(P) (serial frames)"),
    ),
    // 28. Deontic distribution
    TestFixture(
      id: "gt-kd-distribution",
      name: "KD: Obligation Distribution",
      category: ModalTheorem,
      natural_language: "It is obligatory that if P then Q. It is obligatory that P. Therefore it is obligatory that Q.",
      expected_logic_system: KD,
      expected_premises: [
        Obligatory(Implies(Atom("P"), Atom("Q"))),
        Obligatory(Atom("P")),
      ],
      expected_conclusion: Obligatory(Atom("Q")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "deontic", "kd", "valid"],
      source: Some("K-axiom for deontic: O(P→Q) → (O(P) → O(Q))"),
    ),
    // 29. Permission does not imply obligation (INVALID)
    TestFixture(
      id: "gt-kd-invalid-permission-to-obligation",
      name: "KD: Permission Does Not Imply Obligation",
      category: ClassicArgument,
      natural_language: "P is permitted. Therefore P is obligatory.",
      expected_logic_system: KD,
      expected_premises: [Permitted(Atom("P"))],
      expected_conclusion: Obligatory(Atom("P")),
      expected_validity: ExpectedInvalid(Some(
        "P(P) does not entail O(P) — permission is weaker than obligation",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "deontic", "kd", "invalid"],
      source: Some("Standard deontic logic — P(P) ⊬ O(P)"),
    ),
    // 30. Obligation does not imply truth (INVALID in KD)
    TestFixture(
      id: "gt-kd-invalid-obligation-to-truth",
      name: "KD: Obligation Does Not Imply Truth",
      category: ClassicArgument,
      natural_language: "It is obligatory that P. Therefore P is true.",
      expected_logic_system: KD,
      expected_premises: [Obligatory(Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedInvalid(Some(
        "O(P) does not entail P — deontic logic lacks veridicality",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "deontic", "kd", "invalid"],
      source: Some("KD lacks T-axiom for O; obligations can be violated"),
    ),
  ]
}

// =============================================================================
// Epistemic Logic (S5 with Knows)
// =============================================================================

fn epistemic_fixtures() -> List(TestFixture) {
  [
    // 31. Knowledge distribution
    TestFixture(
      id: "gt-epistemic-distribution",
      name: "Epistemic: Knowledge Distribution",
      category: ClassicArgument,
      natural_language: "Alice knows that if P then Q. Alice knows P. Therefore Alice knows Q.",
      expected_logic_system: S5,
      expected_premises: [
        Knows("alice", Implies(Atom("P"), Atom("Q"))),
        Knows("alice", Atom("P")),
      ],
      expected_conclusion: Knows("alice", Atom("Q")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "epistemic", "s5", "valid"],
      source: Some("K-axiom for knowledge: K(P→Q) → (K(P) → K(Q))"),
    ),
    // 32. Knowledge implies truth (veridicality)
    TestFixture(
      id: "gt-epistemic-veridicality",
      name: "Epistemic: Knowledge Implies Truth",
      category: ClassicArgument,
      natural_language: "Alice knows that P. Therefore P is true.",
      expected_logic_system: S5,
      expected_premises: [Knows("alice", Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "epistemic", "s5", "valid"],
      source: Some("T-axiom for knowledge: K(P) → P (veridicality)"),
    ),
    // 33. Positive introspection: K(P) → K(K(P))
    TestFixture(
      id: "gt-epistemic-positive-introspection",
      name: "Epistemic: Positive Introspection",
      category: ModalTheorem,
      natural_language: "Alice knows P. Therefore Alice knows that she knows P.",
      expected_logic_system: S5,
      expected_premises: [Knows("alice", Atom("P"))],
      expected_conclusion: Knows("alice", Knows("alice", Atom("P"))),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "epistemic", "s5", "valid", "introspection"],
      source: Some("4-axiom for knowledge: K(P) → K(K(P))"),
    ),
    // 34. Belief does not imply truth (INVALID)
    TestFixture(
      id: "gt-epistemic-belief-not-veridical",
      name: "Epistemic: Belief Does Not Imply Truth",
      category: ClassicArgument,
      natural_language: "Bob believes P. Therefore P is true.",
      expected_logic_system: KD45,
      expected_premises: [Believes("bob", Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedInvalid(Some(
        "Belief lacks veridicality; B(P) ⊬ P in KD45",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "epistemic", "kd45", "invalid"],
      source: Some("KD45 lacks T-axiom; beliefs can be false"),
    ),
    // 35. Belief consistency (D-axiom for belief)
    TestFixture(
      id: "gt-epistemic-belief-consistency",
      name: "Epistemic: Belief Consistency",
      category: ModalTheorem,
      natural_language: "Bob believes P. Therefore Bob does not believe not-P.",
      expected_logic_system: KD45,
      expected_premises: [Believes("bob", Atom("P"))],
      expected_conclusion: Not(Believes("bob", Not(Atom("P")))),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "epistemic", "kd45", "valid"],
      source: Some("D-axiom for belief: B(P) → ¬B(¬P)"),
    ),
  ]
}

// =============================================================================
// Cross-System Boundary Cases
// =============================================================================

fn cross_system_fixtures() -> List(TestFixture) {
  [
    // 36. Valid in T but not K: □P → P
    TestFixture(
      id: "gt-cross-t-not-k-reflexivity",
      name: "Cross: □P → P (Valid T, Invalid K)",
      category: EdgeCase,
      natural_language: "P is necessarily true. Therefore P. (Valid in T, invalid in K.)",
      expected_logic_system: K,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedInvalid(Some(
        "□P → P requires reflexivity; invalid in K",
      )),
      difficulty: Medium,
      tags: ["ground-truth", "cross-system", "k", "invalid"],
      source: Some("T-axiom is not a theorem of K"),
    ),
    // 37. Valid in S4 but not T: □P → □□P
    TestFixture(
      id: "gt-cross-s4-not-t-transitivity",
      name: "Cross: □P → □□P (Valid S4, Invalid T)",
      category: EdgeCase,
      natural_language: "P is necessary. Therefore P is necessarily necessary. (Valid in S4, invalid in T.)",
      expected_logic_system: T,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Necessary(Necessary(Atom("P"))),
      expected_validity: ExpectedInvalid(Some(
        "□P → □□P requires transitivity; invalid in T",
      )),
      difficulty: Medium,
      tags: ["ground-truth", "cross-system", "t", "invalid"],
      source: Some("4-axiom is not a theorem of T"),
    ),
    // 38. Valid in S5 but not S4: ◇P → □◇P
    TestFixture(
      id: "gt-cross-s5-not-s4-euclidean",
      name: "Cross: ◇P → □◇P (Valid S5, Invalid S4)",
      category: EdgeCase,
      natural_language: "P is possible. Therefore P is necessarily possible. (Valid in S5, invalid in S4.)",
      expected_logic_system: S4,
      expected_premises: [Possible(Atom("P"))],
      expected_conclusion: Necessary(Possible(Atom("P"))),
      expected_validity: ExpectedInvalid(Some(
        "◇P → □◇P requires euclidean; invalid in S4",
      )),
      difficulty: Medium,
      tags: ["ground-truth", "cross-system", "s4", "invalid"],
      source: Some("5-axiom is not a theorem of S4"),
    ),
    // 39. Valid in K4 but not K: □P → □□P
    TestFixture(
      id: "gt-cross-k4-not-k-transitivity",
      name: "Cross: □P → □□P (Valid K4, Invalid K)",
      category: EdgeCase,
      natural_language: "P is necessary. Therefore P is necessarily necessary. (Valid in K4, invalid in K.)",
      expected_logic_system: K,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Necessary(Necessary(Atom("P"))),
      expected_validity: ExpectedInvalid(Some(
        "□P → □□P requires transitivity; invalid in K",
      )),
      difficulty: Medium,
      tags: ["ground-truth", "cross-system", "k", "invalid"],
      source: Some("4-axiom is not a theorem of K"),
    ),
    // 40. Non-sequitur (INVALID in all systems)
    TestFixture(
      id: "gt-cross-nonsequitur",
      name: "Cross: Non-Sequitur (Invalid Everywhere)",
      category: ClassicArgument,
      natural_language: "P is necessary. Therefore Q is necessary.",
      expected_logic_system: K,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Necessary(Atom("Q")),
      expected_validity: ExpectedInvalid(Some(
        "□P ⊬ □Q — no logical connection between P and Q",
      )),
      difficulty: Easy,
      tags: ["ground-truth", "cross-system", "invalid", "all-systems"],
      source: Some("Non-sequitur — invalid in all normal modal logics"),
    ),
  ]
}

// =============================================================================
// Deep Nesting Cases
// =============================================================================

fn deep_nesting_fixtures() -> List(TestFixture) {
  [
    // 41. Triple necessity: □□□P → □P in S4
    TestFixture(
      id: "gt-deep-triple-necessity-s4",
      name: "Deep: □□□P → □P in S4",
      category: EdgeCase,
      natural_language: "It is necessarily necessarily necessarily true that P. Therefore P is necessary.",
      expected_logic_system: S4,
      expected_premises: [Necessary(Necessary(Necessary(Atom("P"))))],
      expected_conclusion: Necessary(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Hard,
      tags: ["ground-truth", "deep-nesting", "s4", "valid"],
      source: Some("In S4: iterated □ collapses via 4-axiom + T-axiom"),
    ),
    // 42. □◇□P → □P in S5
    TestFixture(
      id: "gt-deep-mixed-modality-s5",
      name: "Deep: □◇□P → □P in S5",
      category: EdgeCase,
      natural_language: "It is necessarily possibly necessarily true that P. Therefore P is necessary.",
      expected_logic_system: S5,
      expected_premises: [Necessary(Possible(Necessary(Atom("P"))))],
      expected_conclusion: Necessary(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Hard,
      tags: ["ground-truth", "deep-nesting", "s5", "valid"],
      source: Some("In S5: □◇□P → □P via ◇□P → □P and monotonicity"),
    ),
    // 43. ◇□◇□P → ◇P in S5
    TestFixture(
      id: "gt-deep-alternating-s5",
      name: "Deep: ◇□◇□P → ◇P in S5",
      category: EdgeCase,
      natural_language: "It is possibly necessarily possibly necessarily true that P. Therefore P is possible.",
      expected_logic_system: S5,
      expected_premises: [
        Possible(Necessary(Possible(Necessary(Atom("P"))))),
      ],
      expected_conclusion: Possible(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Hard,
      tags: ["ground-truth", "deep-nesting", "s5", "valid"],
      source: Some("In S5: all iterated modalities collapse to outermost"),
    ),
    // 44. □□□P → P in T (INVALID — needs transitivity for intermediate collapse)
    TestFixture(
      id: "gt-deep-triple-necessity-t-invalid",
      name: "Deep: □□□P → P in T (Not Valid)",
      category: EdgeCase,
      natural_language: "It is necessarily necessarily necessarily true that P. Therefore P is true.",
      expected_logic_system: T,
      expected_premises: [Necessary(Necessary(Necessary(Atom("P"))))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedEither(
        "In T, □P → P gives □□P → □P → P, so □□□P → □□P → □P → P holds. Valid by iterated T-axiom application.",
      ),
      difficulty: Research,
      tags: ["ground-truth", "deep-nesting", "t", "research"],
      source: Some(
        "T-axiom applies at each level: □□□P → □□P → □P → P — valid in T",
      ),
    ),
    // 45. Deeply nested necessity with different atoms (INVALID)
    TestFixture(
      id: "gt-deep-different-atoms",
      name: "Deep: □□P, □□Q ⊬ □(P∧Q) in K",
      category: EdgeCase,
      natural_language: "P is necessarily necessarily true. Q is necessarily necessarily true. Therefore P and Q are necessarily true together.",
      expected_logic_system: K,
      expected_premises: [
        Necessary(Necessary(Atom("P"))),
        Necessary(Necessary(Atom("Q"))),
      ],
      expected_conclusion: Necessary(And(Atom("P"), Atom("Q"))),
      expected_validity: ExpectedEither(
        "In K: □□P and □□Q give □P and □Q via double application, then □(P∧Q) from monotonicity. Validity depends on derivation.",
      ),
      difficulty: Research,
      tags: ["ground-truth", "deep-nesting", "k", "research"],
      source: Some("Depends on K-axiom application strategy"),
    ),
  ]
}

// =============================================================================
// Tier Boundary Cases (at the edge of Tier 1/Tier 2 coverage)
// =============================================================================

fn tier_boundary_fixtures() -> List(TestFixture) {
  [
    // 46. Simple identity (pure Tier 1)
    TestFixture(
      id: "gt-tier-identity",
      name: "Tier: P ⊢ P (Pure Tier 1)",
      category: ClassicArgument,
      natural_language: "P. Therefore P.",
      expected_logic_system: K,
      expected_premises: [Atom("P")],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "tier-boundary", "tier1", "valid"],
      source: Some("Identity — trivially valid"),
    ),
    // 47. Requires truth table (Tier 2)
    TestFixture(
      id: "gt-tier-contraposition",
      name: "Tier: Contraposition (Tier 2)",
      category: ClassicArgument,
      natural_language: "If P then Q. Therefore if not Q then not P.",
      expected_logic_system: K,
      expected_premises: [Implies(Atom("P"), Atom("Q"))],
      expected_conclusion: Implies(Not(Atom("Q")), Not(Atom("P"))),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "tier-boundary", "tier2", "valid"],
      source: Some("Contraposition — requires truth table evaluation"),
    ),
    // 48. Multi-premise complex (Tier 2 boundary)
    TestFixture(
      id: "gt-tier-complex-propositional",
      name: "Tier: Complex Propositional (Tier 2)",
      category: ClassicArgument,
      natural_language: "P or Q. If P then R. If Q then R. Therefore R.",
      expected_logic_system: K,
      expected_premises: [
        Or(Atom("P"), Atom("Q")),
        Implies(Atom("P"), Atom("R")),
        Implies(Atom("Q"), Atom("R")),
      ],
      expected_conclusion: Atom("R"),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "tier-boundary", "tier2", "valid"],
      source: Some("Proof by cases — valid propositional argument"),
    ),
    // 49. Modal + propositional mix (Tier 1/2 boundary)
    TestFixture(
      id: "gt-tier-modal-propositional-mix",
      name: "Tier: □(P→Q), P ⊢ Q (Tier Boundary)",
      category: ModalTheorem,
      natural_language: "Necessarily if P then Q. P. Therefore Q.",
      expected_logic_system: T,
      expected_premises: [Necessary(Implies(Atom("P"), Atom("Q"))), Atom("P")],
      expected_conclusion: Atom("Q"),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["ground-truth", "tier-boundary", "valid"],
      source: Some("In T: □(P→Q) gives P→Q by T-axiom, then Q by modus ponens"),
    ),
    // 50. Requires countermodel (invalid, needs Tier 3 ideally)
    TestFixture(
      id: "gt-tier-countermodel-needed",
      name: "Tier: Countermodel Required (Tier 3)",
      category: EdgeCase,
      natural_language: "P implies Q. Q implies R. Therefore R implies P.",
      expected_logic_system: K,
      expected_premises: [
        Implies(Atom("P"), Atom("Q")),
        Implies(Atom("Q"), Atom("R")),
      ],
      expected_conclusion: Implies(Atom("R"), Atom("P")),
      expected_validity: ExpectedInvalid(Some(
        "Countermodel: P=F, Q=F, R=T satisfies premises but not conclusion",
      )),
      difficulty: Medium,
      tags: [
        "ground-truth",
        "tier-boundary",
        "tier3",
        "invalid",
        "countermodel",
      ],
      source: Some("Invalid — countermodel exists with P=F, Q=F, R=T"),
    ),
    // 51. Conjunction introduction
    TestFixture(
      id: "gt-tier-conjunction-intro",
      name: "Tier: Conjunction Introduction",
      category: ClassicArgument,
      natural_language: "P. Q. Therefore P and Q.",
      expected_logic_system: K,
      expected_premises: [Atom("P"), Atom("Q")],
      expected_conclusion: And(Atom("P"), Atom("Q")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["ground-truth", "tier-boundary", "tier1", "valid"],
      source: Some("Conjunction introduction — trivially valid"),
    ),
    // 52. Invalid modal with propositional structure
    TestFixture(
      id: "gt-tier-modal-invalid",
      name: "Tier: □P ∨ □Q ⊬ □(P∨Q) reversal",
      category: EdgeCase,
      natural_language: "P or Q is necessarily true. Therefore P is necessary or Q is necessary.",
      expected_logic_system: K,
      expected_premises: [Necessary(Or(Atom("P"), Atom("Q")))],
      expected_conclusion: Or(Necessary(Atom("P")), Necessary(Atom("Q"))),
      expected_validity: ExpectedInvalid(Some(
        "□(P∨Q) ⊬ □P ∨ □Q — necessity does not distribute over disjunction",
      )),
      difficulty: Hard,
      tags: ["ground-truth", "tier-boundary", "modal", "invalid"],
      source: Some("Well-known: □ distributes over ∧ but not ∨"),
    ),
  ]
}
