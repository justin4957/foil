//// Argument Corpus
////
//// This module provides an embedded corpus of classic philosophical
//// arguments for testing modal logic inference rules.

import gleam/list
import gleam/option.{type Option, None, Some}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Implies, K, KD, Knows,
  Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}

// ============ Core Types ============

/// A philosophical argument for testing
pub type PhilosophicalArgument {
  PhilosophicalArgument(
    /// Unique identifier
    id: String,
    /// Name of the argument
    name: String,
    /// Category of argument (e.g., "modal", "epistemic", "deontic")
    category: ArgumentCategory,
    /// Natural language description
    description: String,
    /// The premises in formal notation
    premises: List(Proposition),
    /// The conclusion
    conclusion: Proposition,
    /// Whether the argument is valid
    is_valid: Bool,
    /// Logic systems where this argument is valid
    valid_in: List(LogicSystem),
    /// Source reference
    source: Option(String),
    /// Tags for categorization
    tags: List(String),
  )
}

/// Categories of philosophical arguments
pub type ArgumentCategory {
  /// Pure modal arguments
  Modal
  /// Epistemic (knowledge) arguments
  Epistemic
  /// Deontic (obligation/permission) arguments
  Deontic
  /// Classical propositional logic
  Classical
  /// Syllogistic reasoning
  Syllogistic
  /// Arguments that demonstrate fallacies
  Fallacy
  /// Famous historical arguments
  Historical
}

// ============ Classic Modal Arguments ============

/// Classic modal logic arguments
pub fn modal_arguments() -> List(PhilosophicalArgument) {
  [
    // Necessity Distribution (K axiom application)
    PhilosophicalArgument(
      id: "modal_k_distribution",
      name: "Necessity Distribution",
      category: Modal,
      description: "If it is necessary that p implies q, and p is necessary, then q is necessary",
      premises: [
        Necessary(Implies(Atom("p"), Atom("q"))),
        Necessary(Atom("p")),
      ],
      conclusion: Necessary(Atom("q")),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("Basic Modal Logic"),
      tags: ["k_axiom", "distribution"],
    ),
    // T Axiom Application
    PhilosophicalArgument(
      id: "modal_t_application",
      name: "Necessity Entails Actuality",
      category: Modal,
      description: "What is necessary is actual (T axiom)",
      premises: [Necessary(Atom("p"))],
      conclusion: Atom("p"),
      is_valid: True,
      valid_in: [T, S4, S5],
      source: Some("Basic Modal Logic"),
      tags: ["t_axiom", "reflexivity"],
    ),
    // 4 Axiom Application
    PhilosophicalArgument(
      id: "modal_4_application",
      name: "Positive Introspection",
      category: Modal,
      description: "If p is necessary, then it is necessary that p is necessary",
      premises: [Necessary(Atom("p"))],
      conclusion: Necessary(Necessary(Atom("p"))),
      is_valid: True,
      valid_in: [S4, S5],
      source: Some("Basic Modal Logic"),
      tags: ["4_axiom", "introspection"],
    ),
    // 5 Axiom Application
    PhilosophicalArgument(
      id: "modal_5_application",
      name: "Possibility Necessitation",
      category: Modal,
      description: "If p is possible, then it is necessary that p is possible",
      premises: [Possible(Atom("p"))],
      conclusion: Necessary(Possible(Atom("p"))),
      is_valid: True,
      valid_in: [S5],
      source: Some("Basic Modal Logic"),
      tags: ["5_axiom", "euclidean"],
    ),
    // Modal Modus Tollens
    PhilosophicalArgument(
      id: "modal_modus_tollens",
      name: "Modal Modus Tollens",
      category: Modal,
      description: "From necessary implication and impossibility of consequent, derive impossibility of antecedent",
      premises: [
        Necessary(Implies(Atom("p"), Atom("q"))),
        Not(Possible(Atom("q"))),
      ],
      conclusion: Not(Possible(Atom("p"))),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("Modal Logic"),
      tags: ["modus_tollens", "derived"],
    ),
    // Possibility from Actuality (converse of T is invalid in K)
    PhilosophicalArgument(
      id: "modal_actuality_to_possibility",
      name: "Actuality to Possibility",
      category: Modal,
      description: "What is actual is possible (in T, S4, S5)",
      premises: [Atom("p")],
      conclusion: Possible(Atom("p")),
      is_valid: True,
      valid_in: [T, S4, S5],
      source: Some("Basic Modal Logic"),
      tags: ["derived", "possibility"],
    ),
  ]
}

// ============ Epistemic Arguments ============

/// Epistemic logic arguments
pub fn epistemic_arguments() -> List(PhilosophicalArgument) {
  [
    // Knowledge entails truth
    PhilosophicalArgument(
      id: "epistemic_knowledge_truth",
      name: "Knowledge Entails Truth",
      category: Epistemic,
      description: "If Alice knows that p, then p is true (factivity of knowledge)",
      premises: [Knows("alice", Atom("p"))],
      conclusion: Atom("p"),
      is_valid: True,
      valid_in: [T, S4, S5],
      source: Some("Epistemic Logic"),
      tags: ["knowledge", "factivity"],
    ),
    // Positive Introspection (KK principle)
    PhilosophicalArgument(
      id: "epistemic_kk_principle",
      name: "KK Principle",
      category: Epistemic,
      description: "If Alice knows p, she knows that she knows p",
      premises: [Knows("alice", Atom("p"))],
      conclusion: Knows("alice", Knows("alice", Atom("p"))),
      is_valid: True,
      valid_in: [S4, S5],
      source: Some("Epistemic Logic - Hintikka"),
      tags: ["knowledge", "introspection", "kk_principle"],
    ),
    // Knowledge distribution
    PhilosophicalArgument(
      id: "epistemic_distribution",
      name: "Knowledge Distribution",
      category: Epistemic,
      description: "If Alice knows p implies q and knows p, she knows q",
      premises: [
        Knows("alice", Implies(Atom("p"), Atom("q"))),
        Knows("alice", Atom("p")),
      ],
      conclusion: Knows("alice", Atom("q")),
      is_valid: True,
      valid_in: [K, T, S4, S5],
      source: Some("Epistemic Logic"),
      tags: ["knowledge", "distribution"],
    ),
    // Moore's Paradox (knowledge version)
    PhilosophicalArgument(
      id: "epistemic_moore_paradox",
      name: "Moore's Paradox",
      category: Epistemic,
      description: "It is impossible to know 'p and I don't know p'",
      premises: [Knows("alice", And(Atom("p"), Not(Knows("alice", Atom("p")))))],
      conclusion: Atom("contradiction"),
      is_valid: False,
      valid_in: [],
      source: Some("G.E. Moore"),
      tags: ["knowledge", "paradox", "self-reference"],
    ),
  ]
}

// ============ Deontic Arguments ============

/// Deontic logic arguments
pub fn deontic_arguments() -> List(PhilosophicalArgument) {
  [
    // Obligation implies permission
    PhilosophicalArgument(
      id: "deontic_obligation_permission",
      name: "Obligation Implies Permission",
      category: Deontic,
      description: "If p is obligatory, then p is permitted (D axiom)",
      premises: [Obligatory(Atom("p"))],
      conclusion: Permitted(Atom("p")),
      is_valid: True,
      valid_in: [KD],
      source: Some("Standard Deontic Logic"),
      tags: ["obligation", "permission", "d_axiom"],
    ),
    // Deontic Distribution
    PhilosophicalArgument(
      id: "deontic_distribution",
      name: "Deontic Distribution",
      category: Deontic,
      description: "If it is obligatory that p implies q, and p is obligatory, then q is obligatory",
      premises: [
        Obligatory(Implies(Atom("p"), Atom("q"))),
        Obligatory(Atom("p")),
      ],
      conclusion: Obligatory(Atom("q")),
      is_valid: True,
      valid_in: [KD],
      source: Some("Standard Deontic Logic"),
      tags: ["obligation", "distribution"],
    ),
    // Forbidden = Not Permitted
    PhilosophicalArgument(
      id: "deontic_forbidden",
      name: "Forbidden Definition",
      category: Deontic,
      description: "Something is forbidden iff it is not permitted",
      premises: [Not(Permitted(Atom("p")))],
      conclusion: Obligatory(Not(Atom("p"))),
      is_valid: True,
      valid_in: [KD],
      source: Some("Standard Deontic Logic"),
      tags: ["forbidden", "permission", "definition"],
    ),
    // Ross's Paradox (problematic inference)
    PhilosophicalArgument(
      id: "deontic_ross_paradox",
      name: "Ross's Paradox",
      category: Deontic,
      description: "From 'you ought to mail the letter', derive 'you ought to mail or burn the letter'",
      premises: [Obligatory(Atom("mail_letter"))],
      conclusion: Obligatory(Or(Atom("mail_letter"), Atom("burn_letter"))),
      is_valid: True,
      // Valid in SDL but counterintuitive
      valid_in: [KD],
      source: Some("Alf Ross (1941)"),
      tags: ["paradox", "obligation", "disjunction"],
    ),
  ]
}

// ============ Classical Arguments ============

/// Classical propositional logic arguments
pub fn classical_arguments() -> List(PhilosophicalArgument) {
  [
    // Modus Ponens
    PhilosophicalArgument(
      id: "classical_modus_ponens",
      name: "Modus Ponens",
      category: Classical,
      description: "From p and p implies q, derive q",
      premises: [Atom("p"), Implies(Atom("p"), Atom("q"))],
      conclusion: Atom("q"),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("Classical Logic"),
      tags: ["fundamental", "implication"],
    ),
    // Modus Tollens
    PhilosophicalArgument(
      id: "classical_modus_tollens",
      name: "Modus Tollens",
      category: Classical,
      description: "From p implies q and not q, derive not p",
      premises: [Implies(Atom("p"), Atom("q")), Not(Atom("q"))],
      conclusion: Not(Atom("p")),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("Classical Logic"),
      tags: ["fundamental", "implication"],
    ),
    // Hypothetical Syllogism
    PhilosophicalArgument(
      id: "classical_hypothetical_syllogism",
      name: "Hypothetical Syllogism",
      category: Classical,
      description: "From p implies q and q implies r, derive p implies r",
      premises: [
        Implies(Atom("p"), Atom("q")),
        Implies(Atom("q"), Atom("r")),
      ],
      conclusion: Implies(Atom("p"), Atom("r")),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("Classical Logic"),
      tags: ["syllogism", "transitivity"],
    ),
    // Disjunctive Syllogism
    PhilosophicalArgument(
      id: "classical_disjunctive_syllogism",
      name: "Disjunctive Syllogism",
      category: Classical,
      description: "From p or q and not p, derive q",
      premises: [Or(Atom("p"), Atom("q")), Not(Atom("p"))],
      conclusion: Atom("q"),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("Classical Logic"),
      tags: ["syllogism", "disjunction"],
    ),
    // Conjunction Introduction
    PhilosophicalArgument(
      id: "classical_conjunction_intro",
      name: "Conjunction Introduction",
      category: Classical,
      description: "From p and q separately, derive p and q together",
      premises: [Atom("p"), Atom("q")],
      conclusion: And(Atom("p"), Atom("q")),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("Classical Logic"),
      tags: ["fundamental", "conjunction"],
    ),
  ]
}

// ============ Historical Arguments ============

/// Famous historical philosophical arguments
pub fn historical_arguments() -> List(PhilosophicalArgument) {
  [
    // Anselm's Ontological Argument (simplified modal version)
    PhilosophicalArgument(
      id: "historical_ontological",
      name: "Ontological Argument (Modal Version)",
      category: Historical,
      description: "Plantinga's modal ontological argument: if God's existence is possible, then God necessarily exists",
      premises: [
        // If God exists, God exists necessarily
        Implies(Atom("god_exists"), Necessary(Atom("god_exists"))),
        // God's existence is possible
        Possible(Atom("god_exists")),
      ],
      conclusion: Necessary(Atom("god_exists")),
      is_valid: True,
      valid_in: [S5],
      source: Some("Alvin Plantinga, The Nature of Necessity"),
      tags: ["ontological", "god", "existence"],
    ),
    // Socratic Argument (All men are mortal)
    PhilosophicalArgument(
      id: "historical_socrates",
      name: "Socratic Syllogism",
      category: Historical,
      description: "All men are mortal, Socrates is a man, therefore Socrates is mortal",
      premises: [
        // All men are mortal (represented as: being a man implies being mortal)
        Necessary(Implies(Atom("man"), Atom("mortal"))),
        // Socrates is a man
        Atom("socrates_is_man"),
        // Socrates being a man implies he has property 'man'
        Implies(Atom("socrates_is_man"), Atom("man")),
      ],
      conclusion: Atom("mortal"),
      is_valid: True,
      valid_in: [K, T, S4, S5],
      source: Some("Aristotle (attributed)"),
      tags: ["syllogism", "mortality", "famous"],
    ),
    // Cogito (simplified)
    PhilosophicalArgument(
      id: "historical_cogito",
      name: "Cogito Ergo Sum",
      category: Historical,
      description: "I think, therefore I exist - if I am thinking, I must exist",
      premises: [
        // If I am thinking, then I exist
        Necessary(Implies(Atom("thinking"), Atom("existing"))),
        // I am thinking
        Atom("thinking"),
      ],
      conclusion: Atom("existing"),
      is_valid: True,
      valid_in: [K, T, S4, S5, KD],
      source: Some("René Descartes, Meditations"),
      tags: ["existence", "thought", "famous"],
    ),
  ]
}

// ============ Fallacy Examples ============

/// Examples of logical fallacies (invalid arguments)
pub fn fallacy_arguments() -> List(PhilosophicalArgument) {
  [
    // Affirming the Consequent
    PhilosophicalArgument(
      id: "fallacy_affirming_consequent",
      name: "Affirming the Consequent",
      category: Fallacy,
      description: "Invalid: From p implies q and q, incorrectly concluding p",
      premises: [Implies(Atom("p"), Atom("q")), Atom("q")],
      conclusion: Atom("p"),
      is_valid: False,
      valid_in: [],
      source: Some("Logic Fallacies"),
      tags: ["fallacy", "invalid", "implication"],
    ),
    // Denying the Antecedent
    PhilosophicalArgument(
      id: "fallacy_denying_antecedent",
      name: "Denying the Antecedent",
      category: Fallacy,
      description: "Invalid: From p implies q and not p, incorrectly concluding not q",
      premises: [Implies(Atom("p"), Atom("q")), Not(Atom("p"))],
      conclusion: Not(Atom("q")),
      is_valid: False,
      valid_in: [],
      source: Some("Logic Fallacies"),
      tags: ["fallacy", "invalid", "implication"],
    ),
    // Modal Fallacy: Necessity from Possibility
    PhilosophicalArgument(
      id: "fallacy_necessity_from_possibility",
      name: "Necessity from Possibility",
      category: Fallacy,
      description: "Invalid: From possibly p, incorrectly concluding necessarily p",
      premises: [Possible(Atom("p"))],
      conclusion: Necessary(Atom("p")),
      is_valid: False,
      valid_in: [],
      source: Some("Modal Logic Fallacies"),
      tags: ["fallacy", "modal", "invalid"],
    ),
    // Modal Scope Fallacy
    PhilosophicalArgument(
      id: "fallacy_modal_scope",
      name: "Modal Scope Fallacy",
      category: Fallacy,
      description: "Invalid: Confusing □(p → q) with (□p → q)",
      premises: [
        Necessary(Implies(Atom("bachelor"), Atom("unmarried"))),
        Atom("bachelor"),
      ],
      conclusion: Necessary(Atom("unmarried")),
      is_valid: False,
      valid_in: [],
      source: Some("Modal Logic"),
      tags: ["fallacy", "modal", "scope"],
    ),
  ]
}

// ============ Utility Functions ============

/// Get all arguments in the corpus
pub fn all_arguments() -> List(PhilosophicalArgument) {
  list.flatten([
    modal_arguments(),
    epistemic_arguments(),
    deontic_arguments(),
    classical_arguments(),
    historical_arguments(),
    fallacy_arguments(),
  ])
}

/// Get arguments by category
pub fn arguments_by_category(
  category: ArgumentCategory,
) -> List(PhilosophicalArgument) {
  all_arguments()
  |> list.filter(fn(arg) { arg.category == category })
}

/// Get valid arguments only
pub fn valid_arguments() -> List(PhilosophicalArgument) {
  all_arguments()
  |> list.filter(fn(arg) { arg.is_valid })
}

/// Get invalid arguments (fallacies)
pub fn invalid_arguments() -> List(PhilosophicalArgument) {
  all_arguments()
  |> list.filter(fn(arg) { !arg.is_valid })
}

/// Get arguments valid in a specific logic system
pub fn arguments_for_system(system: LogicSystem) -> List(PhilosophicalArgument) {
  all_arguments()
  |> list.filter(fn(arg) { list.contains(arg.valid_in, system) })
}

/// Get arguments by tag
pub fn arguments_by_tag(tag: String) -> List(PhilosophicalArgument) {
  all_arguments()
  |> list.filter(fn(arg) { list.contains(arg.tags, tag) })
}

/// Get argument by ID
pub fn get_argument(id: String) -> Option(PhilosophicalArgument) {
  all_arguments()
  |> list.find(fn(arg) { arg.id == id })
  |> option.from_result
}

/// Count arguments by category
pub fn count_by_category() -> List(#(ArgumentCategory, Int)) {
  [
    #(Modal, list.length(arguments_by_category(Modal))),
    #(Epistemic, list.length(arguments_by_category(Epistemic))),
    #(Deontic, list.length(arguments_by_category(Deontic))),
    #(Classical, list.length(arguments_by_category(Classical))),
    #(Historical, list.length(arguments_by_category(Historical))),
    #(Fallacy, list.length(arguments_by_category(Fallacy))),
    #(Syllogistic, list.length(arguments_by_category(Syllogistic))),
  ]
}

/// Format category as string
pub fn category_to_string(category: ArgumentCategory) -> String {
  case category {
    Modal -> "Modal"
    Epistemic -> "Epistemic"
    Deontic -> "Deontic"
    Classical -> "Classical"
    Syllogistic -> "Syllogistic"
    Fallacy -> "Fallacy"
    Historical -> "Historical"
  }
}

/// Get corpus statistics
pub fn corpus_statistics() -> CorpusStatistics {
  let all = all_arguments()
  CorpusStatistics(
    total_arguments: list.length(all),
    valid_arguments: list.length(valid_arguments()),
    invalid_arguments: list.length(invalid_arguments()),
    by_category: count_by_category(),
  )
}

/// Corpus statistics
pub type CorpusStatistics {
  CorpusStatistics(
    total_arguments: Int,
    valid_arguments: Int,
    invalid_arguments: Int,
    by_category: List(#(ArgumentCategory, Int)),
  )
}
