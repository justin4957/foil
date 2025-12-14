//// Core fixture types and loader for autonomous testing

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, KD, Knows,
  Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}
import modal_logic/testing/test_config.{
  type Difficulty, type ExpectedValidity, type FixtureCategory, ClassicArgument,
  Easy, EdgeCase, ExpectedEither, ExpectedInvalid, ExpectedValid, Hard, Medium,
  ModalTheorem, RegressionCase, Research, Trivial,
}

/// A test fixture representing a modal logic argument
pub type TestFixture {
  TestFixture(
    /// Unique identifier
    id: String,
    /// Human-readable name
    name: String,
    /// Category for filtering
    category: FixtureCategory,
    /// Natural language statement of the argument
    natural_language: String,
    /// Expected logic system
    expected_logic_system: LogicSystem,
    /// Expected premises after formalization
    expected_premises: List(Proposition),
    /// Expected conclusion after formalization
    expected_conclusion: Proposition,
    /// Whether the argument should be valid
    expected_validity: ExpectedValidity,
    /// Difficulty level
    difficulty: Difficulty,
    /// Tags for filtering
    tags: List(String),
    /// Source reference (paper, textbook, etc.)
    source: Option(String),
  )
}

/// Result of loading fixtures
pub type FixtureLoadResult {
  FixturesLoaded(fixtures: List(TestFixture), count: Int)
  LoadError(message: String)
}

/// Get all embedded fixtures
pub fn all_fixtures() -> List(TestFixture) {
  list.flatten([
    classic_argument_fixtures(),
    modal_theorem_fixtures(),
    edge_case_fixtures(),
  ])
}

/// Get fixtures by category
pub fn fixtures_by_category(category: FixtureCategory) -> List(TestFixture) {
  all_fixtures()
  |> list.filter(fn(f) { fixture_category_matches(f.category, category) })
}

/// Get fixtures by difficulty
pub fn fixtures_by_difficulty(difficulty: Difficulty) -> List(TestFixture) {
  all_fixtures()
  |> list.filter(fn(f) { f.difficulty == difficulty })
}

/// Get fixtures by tag
pub fn fixtures_by_tag(tag: String) -> List(TestFixture) {
  all_fixtures()
  |> list.filter(fn(f) { list.contains(f.tags, tag) })
}

/// Get fixtures by logic system
pub fn fixtures_by_logic_system(system: LogicSystem) -> List(TestFixture) {
  all_fixtures()
  |> list.filter(fn(f) { f.expected_logic_system == system })
}

/// Get fixture by ID
pub fn fixture_by_id(id: String) -> Option(TestFixture) {
  all_fixtures()
  |> list.find(fn(f) { f.id == id })
  |> option.from_result
}

/// Check if categories match
fn fixture_category_matches(
  actual: FixtureCategory,
  expected: FixtureCategory,
) -> Bool {
  case actual, expected {
    ClassicArgument, ClassicArgument -> True
    ModalTheorem, ModalTheorem -> True
    EdgeCase, EdgeCase -> True
    RegressionCase, RegressionCase -> True
    test_config.ExternalDataset(a), test_config.ExternalDataset(b) -> a == b
    _, _ -> False
  }
}

/// Classic philosophical argument fixtures
pub fn classic_argument_fixtures() -> List(TestFixture) {
  [
    // Modal Modus Ponens - fundamental valid argument
    TestFixture(
      id: "classic-modal-modus-ponens",
      name: "Modal Modus Ponens",
      category: ClassicArgument,
      natural_language: "Necessarily, if it rains then the ground is wet. It necessarily rains. Therefore, necessarily the ground is wet.",
      expected_logic_system: K,
      expected_premises: [
        Necessary(Implies(Atom("rains"), Atom("ground_wet"))),
        Necessary(Atom("rains")),
      ],
      expected_conclusion: Necessary(Atom("ground_wet")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["valid", "modus-ponens", "necessity"],
      source: Some("Standard modal logic"),
    ),
    // Invalid: Possibility to Necessity
    TestFixture(
      id: "classic-possibility-to-necessity",
      name: "Possibility to Necessity (Invalid)",
      category: ClassicArgument,
      natural_language: "It is possible that it rains. Therefore, it necessarily rains.",
      expected_logic_system: K,
      expected_premises: [Possible(Atom("rains"))],
      expected_conclusion: Necessary(Atom("rains")),
      expected_validity: ExpectedInvalid(Some(
        "Countermodel with world where rains is false",
      )),
      difficulty: Easy,
      tags: ["invalid", "fallacy", "modal-confusion"],
      source: Some("Standard modal logic"),
    ),
    // Deontic Modus Ponens
    TestFixture(
      id: "classic-deontic-modus-ponens",
      name: "Deontic Modus Ponens",
      category: ClassicArgument,
      natural_language: "You ought to keep your promises. If you keep your promises, you ought to be honest. Therefore, you ought to be honest.",
      expected_logic_system: KD,
      expected_premises: [
        Obligatory(Atom("keep_promises")),
        Implies(Atom("keep_promises"), Obligatory(Atom("be_honest"))),
      ],
      expected_conclusion: Obligatory(Atom("be_honest")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["valid", "deontic", "obligation"],
      source: Some("Deontic logic"),
    ),
    // Epistemic Closure
    TestFixture(
      id: "classic-epistemic-closure",
      name: "Epistemic Closure",
      category: ClassicArgument,
      natural_language: "Alice knows that if it is raining, then the ground is wet. Alice knows that it is raining. Therefore, Alice knows that the ground is wet.",
      expected_logic_system: S5,
      expected_premises: [
        Knows("alice", Implies(Atom("raining"), Atom("ground_wet"))),
        Knows("alice", Atom("raining")),
      ],
      expected_conclusion: Knows("alice", Atom("ground_wet")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["valid", "epistemic", "knowledge", "closure"],
      source: Some("Epistemic logic"),
    ),
    // Belief Distribution (may fail in some systems)
    TestFixture(
      id: "classic-belief-distribution",
      name: "Belief Distribution",
      category: ClassicArgument,
      natural_language: "Bob believes that it is sunny and warm. Therefore, Bob believes that it is sunny.",
      expected_logic_system: K,
      expected_premises: [
        Believes("bob", And(Atom("sunny"), Atom("warm"))),
      ],
      expected_conclusion: Believes("bob", Atom("sunny")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["valid", "belief", "distribution"],
      source: Some("Doxastic logic"),
    ),
    // S5 Characteristic: Possibility implies Necessary Possibility
    TestFixture(
      id: "classic-s5-characteristic",
      name: "S5 Characteristic Axiom",
      category: ClassicArgument,
      natural_language: "It is possible that there exists a perfect being. Therefore, it is necessarily possible that there exists a perfect being.",
      expected_logic_system: S5,
      expected_premises: [Possible(Atom("perfect_being_exists"))],
      expected_conclusion: Necessary(Possible(Atom("perfect_being_exists"))),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["valid", "s5", "modal-axiom"],
      source: Some("S5 modal logic"),
    ),
    // Permission from Non-Obligation
    TestFixture(
      id: "classic-permission-non-obligation",
      name: "Permission from Non-Obligation",
      category: ClassicArgument,
      natural_language: "It is not obligatory that you work on Sunday. Therefore, it is permitted that you do not work on Sunday.",
      expected_logic_system: KD,
      expected_premises: [Not(Obligatory(Atom("work_sunday")))],
      expected_conclusion: Permitted(Not(Atom("work_sunday"))),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["valid", "deontic", "permission", "obligation"],
      source: Some("Deontic logic"),
    ),
    // Invalid: Ought Implies Can (controversial)
    TestFixture(
      id: "classic-ought-implies-can",
      name: "Ought Implies Can",
      category: ClassicArgument,
      natural_language: "You ought to save the drowning child. Therefore, you can save the drowning child.",
      expected_logic_system: KD,
      expected_premises: [Obligatory(Atom("save_child"))],
      expected_conclusion: Possible(Atom("save_child")),
      expected_validity: ExpectedEither(
        "Depends on whether deontic logic includes ought-implies-can principle",
      ),
      difficulty: Hard,
      tags: ["controversial", "deontic", "ought-implies-can"],
      source: Some("Kant's ethics"),
    ),
    // Necessity Distribution
    TestFixture(
      id: "classic-necessity-distribution",
      name: "Necessity Distribution",
      category: ClassicArgument,
      natural_language: "Necessarily, if Socrates is a man then Socrates is mortal. Socrates is necessarily a man. Therefore, Socrates is necessarily mortal.",
      expected_logic_system: K,
      expected_premises: [
        Necessary(Implies(Atom("socrates_man"), Atom("socrates_mortal"))),
        Necessary(Atom("socrates_man")),
      ],
      expected_conclusion: Necessary(Atom("socrates_mortal")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["valid", "k-axiom", "distribution"],
      source: Some("Modal logic textbook"),
    ),
    // Invalid in K, Valid in T: Necessity implies Actuality
    TestFixture(
      id: "classic-t-axiom",
      name: "T Axiom (Necessity Implies Actuality)",
      category: ClassicArgument,
      natural_language: "It is necessarily true that 2+2=4. Therefore, 2+2=4.",
      expected_logic_system: T,
      expected_premises: [Necessary(Atom("two_plus_two_equals_four"))],
      expected_conclusion: Atom("two_plus_two_equals_four"),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["valid", "t-axiom", "reflexivity"],
      source: Some("T modal logic"),
    ),
  ]
}

/// Standard modal logic theorem fixtures
pub fn modal_theorem_fixtures() -> List(TestFixture) {
  [
    // K Axiom
    TestFixture(
      id: "theorem-k-axiom",
      name: "K Axiom",
      category: ModalTheorem,
      natural_language: "If it is necessary that P implies Q, then if it is necessary that P, then it is necessary that Q.",
      expected_logic_system: K,
      expected_premises: [
        Necessary(Implies(Atom("P"), Atom("Q"))),
        Necessary(Atom("P")),
      ],
      expected_conclusion: Necessary(Atom("Q")),
      expected_validity: ExpectedValid,
      difficulty: Trivial,
      tags: ["theorem", "k-axiom", "fundamental"],
      source: Some("Modal logic"),
    ),
    // T Axiom
    TestFixture(
      id: "theorem-t-axiom",
      name: "T Axiom",
      category: ModalTheorem,
      natural_language: "If P is necessary, then P is true.",
      expected_logic_system: T,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Atom("P"),
      expected_validity: ExpectedValid,
      difficulty: Trivial,
      tags: ["theorem", "t-axiom", "reflexive"],
      source: Some("T modal logic"),
    ),
    // 4 Axiom (S4)
    TestFixture(
      id: "theorem-4-axiom",
      name: "4 Axiom (Positive Introspection)",
      category: ModalTheorem,
      natural_language: "If P is necessary, then it is necessary that P is necessary.",
      expected_logic_system: S4,
      expected_premises: [Necessary(Atom("P"))],
      expected_conclusion: Necessary(Necessary(Atom("P"))),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["theorem", "4-axiom", "transitive", "s4"],
      source: Some("S4 modal logic"),
    ),
    // 5 Axiom (S5)
    TestFixture(
      id: "theorem-5-axiom",
      name: "5 Axiom (S5 Characteristic)",
      category: ModalTheorem,
      natural_language: "If P is possible, then P is necessarily possible.",
      expected_logic_system: S5,
      expected_premises: [Possible(Atom("P"))],
      expected_conclusion: Necessary(Possible(Atom("P"))),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["theorem", "5-axiom", "euclidean", "s5"],
      source: Some("S5 modal logic"),
    ),
    // D Axiom (Deontic)
    TestFixture(
      id: "theorem-d-axiom",
      name: "D Axiom (Seriality)",
      category: ModalTheorem,
      natural_language: "If P is obligatory, then P is permitted.",
      expected_logic_system: KD,
      expected_premises: [Obligatory(Atom("P"))],
      expected_conclusion: Permitted(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["theorem", "d-axiom", "serial", "deontic"],
      source: Some("Deontic logic"),
    ),
    // B Axiom
    TestFixture(
      id: "theorem-b-axiom",
      name: "B Axiom (Symmetry)",
      category: ModalTheorem,
      natural_language: "If P is true, then P is necessarily possible.",
      expected_logic_system: S5,
      expected_premises: [Atom("P")],
      expected_conclusion: Necessary(Possible(Atom("P"))),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["theorem", "b-axiom", "symmetric", "s5"],
      source: Some("B modal logic"),
    ),
    // Dual of Necessity
    TestFixture(
      id: "theorem-dual-necessity",
      name: "Dual: Not Possible Not = Necessary",
      category: ModalTheorem,
      natural_language: "It is not possible that P is false. Therefore, P is necessary.",
      expected_logic_system: K,
      expected_premises: [Not(Possible(Not(Atom("P"))))],
      expected_conclusion: Necessary(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["theorem", "dual", "modal-equivalence"],
      source: Some("Modal logic"),
    ),
    // Dual of Possibility
    TestFixture(
      id: "theorem-dual-possibility",
      name: "Dual: Not Necessary Not = Possible",
      category: ModalTheorem,
      natural_language: "It is not necessary that P is false. Therefore, P is possible.",
      expected_logic_system: K,
      expected_premises: [Not(Necessary(Not(Atom("P"))))],
      expected_conclusion: Possible(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["theorem", "dual", "modal-equivalence"],
      source: Some("Modal logic"),
    ),
    // Distribution over Conjunction
    TestFixture(
      id: "theorem-distribution-conjunction",
      name: "Distribution over Conjunction",
      category: ModalTheorem,
      natural_language: "It is necessary that P and Q. Therefore, it is necessary that P, and it is necessary that Q.",
      expected_logic_system: K,
      expected_premises: [Necessary(And(Atom("P"), Atom("Q")))],
      expected_conclusion: And(Necessary(Atom("P")), Necessary(Atom("Q"))),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["theorem", "distribution", "conjunction"],
      source: Some("Modal logic"),
    ),
    // Invalid: Distribution over Disjunction
    TestFixture(
      id: "theorem-distribution-disjunction-invalid",
      name: "Distribution over Disjunction (Invalid)",
      category: ModalTheorem,
      natural_language: "It is necessary that P or Q. Therefore, it is necessary that P, or it is necessary that Q.",
      expected_logic_system: K,
      expected_premises: [Necessary(Or(Atom("P"), Atom("Q")))],
      expected_conclusion: Or(Necessary(Atom("P")), Necessary(Atom("Q"))),
      expected_validity: ExpectedInvalid(Some(
        "Countermodel: world where P true, another where Q true",
      )),
      difficulty: Medium,
      tags: ["invalid", "distribution", "disjunction"],
      source: Some("Modal logic"),
    ),
  ]
}

/// Edge case fixtures for boundary testing
pub fn edge_case_fixtures() -> List(TestFixture) {
  [
    // Deeply nested modalities
    TestFixture(
      id: "edge-deeply-nested",
      name: "Deeply Nested Modalities",
      category: EdgeCase,
      natural_language: "It is necessarily possibly necessarily possibly true that P.",
      expected_logic_system: S5,
      expected_premises: [],
      expected_conclusion: Necessary(Possible(Necessary(Possible(Atom("P"))))),
      expected_validity: ExpectedEither(
        "In S5, this reduces to Possible(P) due to equivalence relation",
      ),
      difficulty: Hard,
      tags: ["edge-case", "nested", "s5"],
      source: None,
    ),
    // Contradictory premises
    TestFixture(
      id: "edge-contradictory-premises",
      name: "Contradictory Premises",
      category: EdgeCase,
      natural_language: "P is necessary. P is impossible. Therefore, Q is necessary.",
      expected_logic_system: K,
      expected_premises: [Necessary(Atom("P")), Necessary(Not(Atom("P")))],
      expected_conclusion: Necessary(Atom("Q")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["edge-case", "contradiction", "explosion"],
      source: Some("Classical logic"),
    ),
    // Tautological conclusion
    TestFixture(
      id: "edge-tautology",
      name: "Tautological Conclusion",
      category: EdgeCase,
      natural_language: "P or not P is necessary.",
      expected_logic_system: K,
      expected_premises: [],
      expected_conclusion: Necessary(Or(Atom("P"), Not(Atom("P")))),
      expected_validity: ExpectedValid,
      difficulty: Easy,
      tags: ["edge-case", "tautology"],
      source: Some("Classical logic"),
    ),
    // Self-referential (handled structurally)
    TestFixture(
      id: "edge-iteration-collapse",
      name: "Modal Iteration Collapse (S5)",
      category: EdgeCase,
      natural_language: "If possibly possibly P, then possibly P.",
      expected_logic_system: S5,
      expected_premises: [Possible(Possible(Atom("P")))],
      expected_conclusion: Possible(Atom("P")),
      expected_validity: ExpectedValid,
      difficulty: Medium,
      tags: ["edge-case", "iteration", "s5"],
      source: Some("S5 modal logic"),
    ),
    // Mixed modalities
    TestFixture(
      id: "edge-mixed-modalities",
      name: "Mixed Alethic and Deontic",
      category: EdgeCase,
      natural_language: "It is necessary that you ought to tell the truth. Therefore, it is obligatory that you tell the truth.",
      expected_logic_system: KD,
      expected_premises: [Necessary(Obligatory(Atom("tell_truth")))],
      expected_conclusion: Obligatory(Atom("tell_truth")),
      expected_validity: ExpectedEither(
        "Depends on interaction between alethic and deontic modalities",
      ),
      difficulty: Research,
      tags: ["edge-case", "mixed-modality", "alethic", "deontic"],
      source: None,
    ),
    // Many premises
    TestFixture(
      id: "edge-many-premises",
      name: "Many Premises",
      category: EdgeCase,
      natural_language: "P1 is necessary. P2 is necessary. P3 is necessary. P4 is necessary. P5 is necessary. P1 and P2 and P3 and P4 and P5 implies Q. Therefore, Q is necessary.",
      expected_logic_system: K,
      expected_premises: [
        Necessary(Atom("P1")),
        Necessary(Atom("P2")),
        Necessary(Atom("P3")),
        Necessary(Atom("P4")),
        Necessary(Atom("P5")),
        Necessary(Implies(
          And(
            Atom("P1"),
            And(Atom("P2"), And(Atom("P3"), And(Atom("P4"), Atom("P5")))),
          ),
          Atom("Q"),
        )),
      ],
      expected_conclusion: Necessary(Atom("Q")),
      expected_validity: ExpectedValid,
      difficulty: Hard,
      tags: ["edge-case", "many-premises"],
      source: None,
    ),
  ]
}

/// Convert fixture to string representation
pub fn fixture_to_string(fixture: TestFixture) -> String {
  string.concat([
    "TestFixture(",
    fixture.id,
    ": ",
    fixture.name,
    " [",
    test_config.category_to_string(fixture.category),
    "] ",
    test_config.difficulty_to_string(fixture.difficulty),
    ")",
  ])
}

/// Get count of fixtures by category
pub fn count_by_category() -> List(#(FixtureCategory, Int)) {
  [
    #(ClassicArgument, list.length(classic_argument_fixtures())),
    #(ModalTheorem, list.length(modal_theorem_fixtures())),
    #(EdgeCase, list.length(edge_case_fixtures())),
  ]
}
