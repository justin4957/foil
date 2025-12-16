//// LogiQA 2.0 Dataset Adapter
////
//// This module provides specialized handling for the LogiQA 2.0 dataset
//// which contains ~35k premise-hypothesis pairs for logical reasoning.
////
//// Dataset: https://github.com/lgw863/LogiQA-dataset
//// Paper: https://arxiv.org/abs/2007.08124

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, Knows,
  Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}
import modal_logic/testing/external/huggingface_loader.{
  type DatasetExample, type ExampleLabel, Contradiction, DatasetExample,
  Entailment, Neutral, Unknown,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture, TestFixture}
import modal_logic/testing/test_config.{
  Easy, ExpectedEither, ExpectedInvalid, ExpectedValid, ExternalDataset, Hard,
  Medium, Unknown as UnknownValidity,
}

// ============ Core Types ============

/// LogiQA-specific configuration
pub type LogiQAConfig {
  LogiQAConfig(
    /// Maximum examples to load
    max_examples: Int,
    /// Filter by reasoning type
    reasoning_type_filter: Option(ReasoningType),
    /// Minimum confidence threshold for translation
    min_confidence: Float,
    /// Include multi-choice context
    include_context: Bool,
    /// Cache duration in seconds
    cache_duration: Int,
  )
}

/// Types of logical reasoning in LogiQA
pub type ReasoningType {
  /// Categorical syllogisms
  Categorical
  /// Sufficient condition reasoning
  SufficientCondition
  /// Necessary condition reasoning
  NecessaryCondition
  /// Disjunctive reasoning
  Disjunctive
  /// Conjunctive reasoning
  Conjunctive
  /// Causal reasoning
  Causal
  /// Analogy-based reasoning
  Analogy
  /// Mixed or complex reasoning
  Mixed
}

/// A LogiQA example with premise-hypothesis structure
pub type LogiQAExample {
  LogiQAExample(
    /// Unique identifier
    id: String,
    /// Context passage
    context: String,
    /// Question stem
    question: String,
    /// Multiple choice options
    options: List(String),
    /// Correct answer index (0-based)
    correct_answer: Int,
    /// Reasoning type
    reasoning_type: ReasoningType,
    /// Extracted premise-hypothesis pairs
    premise_hypothesis_pairs: List(PremiseHypothesisPair),
  )
}

/// A premise-hypothesis pair extracted from LogiQA
pub type PremiseHypothesisPair {
  PremiseHypothesisPair(
    /// Premises from context
    premises: List(String),
    /// Hypothesis (from question + answer)
    hypothesis: String,
    /// Whether this is the correct answer
    is_correct: Bool,
    /// Entailment label
    label: ExampleLabel,
  )
}

/// Translation result
pub type LogiQATranslation {
  LogiQATranslation(
    /// Original example ID
    original_id: String,
    /// Translated premises
    translated_premises: List(Proposition),
    /// Translated hypothesis
    translated_hypothesis: Proposition,
    /// Detected logic system
    logic_system: LogicSystem,
    /// Translation confidence
    confidence: Float,
    /// Detected reasoning patterns
    patterns: List(ReasoningPattern),
  )
}

/// Patterns detected in reasoning
pub type ReasoningPattern {
  /// Modus ponens pattern
  ModusPonens
  /// Modus tollens pattern
  ModusTollens
  /// Hypothetical syllogism
  HypotheticalSyllogism
  /// Disjunctive syllogism
  DisjunctiveSyllogism
  /// Conditional reasoning
  ConditionalReasoning
  /// Negation handling
  NegationPattern
  /// Universal instantiation
  UniversalInstantiation
  /// Modal reasoning
  ModalReasoning
  /// Epistemic reasoning
  EpistemicReasoning
  /// Deontic reasoning
  DeonticReasoning
}

// ============ Configuration ============

/// Default LogiQA configuration
pub fn default_config() -> LogiQAConfig {
  LogiQAConfig(
    max_examples: 100,
    reasoning_type_filter: None,
    min_confidence: 0.6,
    include_context: True,
    cache_duration: 604_800,
  )
}

/// Configuration for categorical reasoning only
pub fn categorical_config() -> LogiQAConfig {
  LogiQAConfig(..default_config(), reasoning_type_filter: Some(Categorical))
}

/// Configuration for conditional reasoning
pub fn conditional_config() -> LogiQAConfig {
  LogiQAConfig(
    ..default_config(),
    reasoning_type_filter: Some(SufficientCondition),
  )
}

/// Configuration for full dataset
pub fn comprehensive_config() -> LogiQAConfig {
  LogiQAConfig(
    max_examples: 500,
    reasoning_type_filter: None,
    min_confidence: 0.5,
    include_context: True,
    cache_duration: 604_800,
  )
}

// ============ Mock LogiQA Examples ============

/// Extended LogiQA examples representing dataset structure
pub fn extended_logiqa_examples() -> List(LogiQAExample) {
  [
    // Categorical reasoning
    LogiQAExample(
      id: "logiqa_cat_1",
      context: "All managers in the company have MBAs. John is a manager in the company.",
      question: "Which of the following must be true?",
      options: [
        "John has an MBA",
        "John does not have an MBA",
        "Some managers do not have MBAs",
        "Cannot be determined",
      ],
      correct_answer: 0,
      reasoning_type: Categorical,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "All managers in the company have MBAs",
            "John is a manager in the company",
          ],
          hypothesis: "John has an MBA",
          is_correct: True,
          label: Entailment,
        ),
        PremiseHypothesisPair(
          premises: [
            "All managers in the company have MBAs",
            "John is a manager in the company",
          ],
          hypothesis: "John does not have an MBA",
          is_correct: False,
          label: Contradiction,
        ),
      ],
    ),
    // Sufficient condition reasoning
    LogiQAExample(
      id: "logiqa_suf_1",
      context: "If a student passes all exams, they will graduate. Mary passed all her exams.",
      question: "What can we conclude?",
      options: [
        "Mary will graduate",
        "Mary might not graduate",
        "Only Mary will graduate",
        "Mary passed some exams",
      ],
      correct_answer: 0,
      reasoning_type: SufficientCondition,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "If a student passes all exams, they will graduate",
            "Mary passed all her exams",
          ],
          hypothesis: "Mary will graduate",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Necessary condition reasoning
    LogiQAExample(
      id: "logiqa_nec_1",
      context: "One can only enter the building if they have a valid ID. Tom entered the building.",
      question: "What must be true?",
      options: [
        "Tom has a valid ID",
        "Tom does not need an ID",
        "The building has no security",
        "Tom is an employee",
      ],
      correct_answer: 0,
      reasoning_type: NecessaryCondition,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "One can only enter the building if they have a valid ID",
            "Tom entered the building",
          ],
          hypothesis: "Tom has a valid ID",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Disjunctive reasoning
    LogiQAExample(
      id: "logiqa_dis_1",
      context: "Either the project will be completed on time, or the budget will be exceeded. The project was not completed on time.",
      question: "What follows?",
      options: [
        "The budget was exceeded",
        "The project was successful",
        "The budget was not exceeded",
        "Nothing can be concluded",
      ],
      correct_answer: 0,
      reasoning_type: Disjunctive,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "Either the project will be completed on time, or the budget will be exceeded",
            "The project was not completed on time",
          ],
          hypothesis: "The budget was exceeded",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Causal reasoning
    LogiQAExample(
      id: "logiqa_cau_1",
      context: "Whenever it rains heavily, the river floods. It rained heavily last night.",
      question: "What can be concluded?",
      options: [
        "The river flooded",
        "The river did not flood",
        "It might rain again",
        "The dam broke",
      ],
      correct_answer: 0,
      reasoning_type: Causal,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "Whenever it rains heavily, the river floods",
            "It rained heavily last night",
          ],
          hypothesis: "The river flooded",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Modal reasoning example
    LogiQAExample(
      id: "logiqa_mod_1",
      context: "It is impossible for a perpetual motion machine to exist. Someone claims to have built a perpetual motion machine.",
      question: "What must be true?",
      options: [
        "The claim is false",
        "The machine works",
        "Physics is wrong",
        "Cannot determine",
      ],
      correct_answer: 0,
      reasoning_type: Mixed,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "It is impossible for a perpetual motion machine to exist",
            "Someone claims to have built a perpetual motion machine",
          ],
          hypothesis: "The claim is false",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Epistemic reasoning
    LogiQAExample(
      id: "logiqa_epi_1",
      context: "Alice knows that Bob is at the party. If Alice knows something, she will act on it.",
      question: "What follows?",
      options: [
        "Alice will act on Bob being at the party",
        "Bob does not know Alice",
        "Alice is not at the party",
        "Bob will leave",
      ],
      correct_answer: 0,
      reasoning_type: Mixed,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "Alice knows that Bob is at the party",
            "If Alice knows something, she will act on it",
          ],
          hypothesis: "Alice will act on Bob being at the party",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Deontic reasoning
    LogiQAExample(
      id: "logiqa_deo_1",
      context: "Doctors ought to help patients. Sarah is a doctor.",
      question: "What is required?",
      options: [
        "Sarah ought to help patients",
        "Sarah must not help",
        "Patients help doctors",
        "Nothing is required",
      ],
      correct_answer: 0,
      reasoning_type: Mixed,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: ["Doctors ought to help patients", "Sarah is a doctor"],
          hypothesis: "Sarah ought to help patients",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Modus tollens
    LogiQAExample(
      id: "logiqa_mt_1",
      context: "If the alarm sounds, there is danger. There is no danger.",
      question: "What can we conclude?",
      options: [
        "The alarm did not sound",
        "The alarm sounded",
        "There might be danger",
        "The alarm is broken",
      ],
      correct_answer: 0,
      reasoning_type: SufficientCondition,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "If the alarm sounds, there is danger",
            "There is no danger",
          ],
          hypothesis: "The alarm did not sound",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
    // Hypothetical syllogism
    LogiQAExample(
      id: "logiqa_hs_1",
      context: "If it rains, the ground gets wet. If the ground gets wet, the roads are slippery. It is raining.",
      question: "What follows?",
      options: [
        "The roads are slippery",
        "The roads are dry",
        "It will stop raining",
        "Nothing can be concluded",
      ],
      correct_answer: 0,
      reasoning_type: SufficientCondition,
      premise_hypothesis_pairs: [
        PremiseHypothesisPair(
          premises: [
            "If it rains, the ground gets wet",
            "If the ground gets wet, the roads are slippery",
            "It is raining",
          ],
          hypothesis: "The roads are slippery",
          is_correct: True,
          label: Entailment,
        ),
      ],
    ),
  ]
}

// ============ Translation Functions ============

/// Translate a LogiQA example to modal logic
pub fn translate_example(
  example: LogiQAExample,
  config: LogiQAConfig,
) -> List(LogiQATranslation) {
  example.premise_hypothesis_pairs
  |> list.filter(fn(pair) { pair.is_correct || config.include_context })
  |> list.map(fn(pair) {
    translate_pair(example.id, pair, example.reasoning_type)
  })
  |> list.filter(fn(t) { t.confidence >=. config.min_confidence })
}

/// Translate a premise-hypothesis pair
fn translate_pair(
  example_id: String,
  pair: PremiseHypothesisPair,
  reasoning_type: ReasoningType,
) -> LogiQATranslation {
  let patterns = detect_reasoning_patterns(pair.premises, pair.hypothesis)
  let logic_system =
    detect_logic_system(pair.premises, pair.hypothesis, patterns)

  let translated_premises =
    pair.premises
    |> list.index_map(fn(p, idx) { translate_sentence(p, idx, patterns) })

  let translated_hypothesis =
    translate_sentence(pair.hypothesis, list.length(pair.premises), patterns)

  let confidence = calculate_confidence(patterns, reasoning_type)

  LogiQATranslation(
    original_id: example_id,
    translated_premises: translated_premises,
    translated_hypothesis: translated_hypothesis,
    logic_system: logic_system,
    confidence: confidence,
    patterns: patterns,
  )
}

/// Detect reasoning patterns in text
fn detect_reasoning_patterns(
  premises: List(String),
  hypothesis: String,
) -> List(ReasoningPattern) {
  let all_text =
    list.append(premises, [hypothesis])
    |> string.join(" ")
    |> string.lowercase

  let patterns = []

  // Modus ponens detection: If P then Q, P => Q
  let patterns = case
    string.contains(all_text, "if ") && string.contains(all_text, " then ")
  {
    True -> [ModusPonens, ..patterns]
    False -> patterns
  }

  // Conditional reasoning
  let patterns = case
    string.contains(all_text, "whenever")
    || string.contains(all_text, "only if")
  {
    True -> [ConditionalReasoning, ..patterns]
    False -> patterns
  }

  // Disjunctive syllogism
  let patterns = case
    string.contains(all_text, "either") && string.contains(all_text, " or ")
  {
    True -> [DisjunctiveSyllogism, ..patterns]
    False -> patterns
  }

  // Negation
  let patterns = case
    string.contains(all_text, " not ")
    || string.contains(all_text, "no ")
    || string.contains(all_text, "never")
  {
    True -> [NegationPattern, ..patterns]
    False -> patterns
  }

  // Universal instantiation
  let patterns = case
    string.contains(all_text, "all ")
    || string.contains(all_text, "every ")
    || string.contains(all_text, "any ")
  {
    True -> [UniversalInstantiation, ..patterns]
    False -> patterns
  }

  // Modal reasoning
  let patterns = case
    string.contains(all_text, "must")
    || string.contains(all_text, "necessarily")
    || string.contains(all_text, "impossible")
    || string.contains(all_text, "possible")
  {
    True -> [ModalReasoning, ..patterns]
    False -> patterns
  }

  // Epistemic reasoning
  let patterns = case
    string.contains(all_text, "knows")
    || string.contains(all_text, "believes")
    || string.contains(all_text, "thinks")
  {
    True -> [EpistemicReasoning, ..patterns]
    False -> patterns
  }

  // Deontic reasoning
  let patterns = case
    string.contains(all_text, "ought")
    || string.contains(all_text, "should")
    || string.contains(all_text, "permitted")
    || string.contains(all_text, "forbidden")
  {
    True -> [DeonticReasoning, ..patterns]
    False -> patterns
  }

  patterns
}

/// Detect appropriate logic system
fn detect_logic_system(
  premises: List(String),
  hypothesis: String,
  patterns: List(ReasoningPattern),
) -> LogicSystem {
  case list.contains(patterns, EpistemicReasoning) {
    True -> S5
    False ->
      case list.contains(patterns, DeonticReasoning) {
        True -> proposition.KD
        False ->
          case list.contains(patterns, ModalReasoning) {
            True -> S4
            False ->
              case list.contains(patterns, UniversalInstantiation) {
                True -> T
                False -> K
              }
          }
      }
  }
}

/// Translate a sentence to a proposition
fn translate_sentence(
  sentence: String,
  index: Int,
  patterns: List(ReasoningPattern),
) -> Proposition {
  let lower = string.lowercase(sentence)
  let base_atom = "q" <> int.to_string(index)

  // Check for universal: "All X are Y"
  case
    string.starts_with(lower, "all ") || string.starts_with(lower, "every ")
  {
    True -> translate_universal(lower, base_atom)
    False ->
      case string.starts_with(lower, "if ") {
        True -> translate_conditional(lower, base_atom)
        False ->
          case string.starts_with(lower, "either ") {
            True -> translate_disjunction(lower, base_atom)
            False ->
              case string.contains(lower, " knows ") {
                True -> translate_epistemic(lower, base_atom)
                False ->
                  case
                    string.contains(lower, "ought")
                    || string.contains(lower, "should")
                  {
                    True -> translate_deontic(lower, base_atom)
                    False ->
                      case string.contains(lower, "impossible") {
                        True -> translate_impossibility(lower, base_atom)
                        False ->
                          case
                            string.contains(lower, " not ")
                            || string.contains(lower, "no ")
                          {
                            True -> translate_negation(lower, base_atom)
                            False ->
                              case string.contains(lower, "must") {
                                True -> translate_necessity(lower, base_atom)
                                False -> Atom(base_atom)
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

/// Translate universal: "All X are Y" -> □(X → Y)
fn translate_universal(sentence: String, base_atom: String) -> Proposition {
  let parts =
    sentence
    |> string.replace("all ", "")
    |> string.replace("every ", "")
    |> string.split(" are ")

  case parts {
    [subject, predicate] ->
      Necessary(Implies(Atom(clean_term(subject)), Atom(clean_term(predicate))))
    _ -> Atom(base_atom)
  }
}

/// Translate conditional: "If X then Y" -> X → Y
fn translate_conditional(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("if ", "")
    |> string.replace(", then ", " then ")
    |> string.replace(",", "")

  let parts = string.split(cleaned, " then ")
  case parts {
    [ant, cons] -> Implies(Atom(clean_term(ant)), Atom(clean_term(cons)))
    _ -> Atom(base_atom)
  }
}

/// Translate disjunction: "Either X or Y" -> X ∨ Y
fn translate_disjunction(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("either ", "")
    |> string.replace(".", "")

  let parts = string.split(cleaned, " or ")
  case parts {
    [left, right] -> Or(Atom(clean_term(left)), Atom(clean_term(right)))
    _ -> Atom(base_atom)
  }
}

/// Translate epistemic: "X knows Y" -> K_x(Y)
fn translate_epistemic(sentence: String, base_atom: String) -> Proposition {
  let parts = string.split(sentence, " knows ")
  case parts {
    [agent, content] -> Knows(clean_term(agent), Atom(clean_term(content)))
    _ -> Atom(base_atom)
  }
}

/// Translate deontic: "X ought Y" -> O(Y)
fn translate_deontic(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("ought to ", "")
    |> string.replace("should ", "")
    |> clean_term

  Obligatory(Atom(cleaned))
}

/// Translate impossibility: "It is impossible X" -> □¬X
fn translate_impossibility(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("it is impossible for ", "")
    |> string.replace("it is impossible that ", "")
    |> string.replace("impossible for ", "")
    |> string.replace(" to exist", "")
    |> clean_term

  Necessary(Not(Atom(cleaned)))
}

/// Translate negation
fn translate_negation(sentence: String, base_atom: String) -> Proposition {
  case string.contains(sentence, " not ") {
    True -> {
      let cleaned =
        sentence
        |> string.replace(" not ", " ")
        |> clean_term
      Not(Atom(cleaned))
    }
    False ->
      case string.contains(sentence, "no ") {
        True -> {
          let cleaned =
            sentence
            |> string.replace("there is no ", "")
            |> string.replace("no ", "")
            |> clean_term
          Not(Atom(cleaned))
        }
        False -> Not(Atom(base_atom))
      }
  }
}

/// Translate necessity: "X must Y" -> □Y
fn translate_necessity(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("must be ", "")
    |> string.replace("must ", "")
    |> clean_term

  Necessary(Atom(cleaned))
}

/// Clean a term for use as atom name
fn clean_term(term: String) -> String {
  term
  |> string.trim
  |> string.replace(" ", "_")
  |> string.replace(".", "")
  |> string.replace(",", "")
  |> string.replace("'", "")
  |> string.lowercase
}

/// Calculate translation confidence
fn calculate_confidence(
  patterns: List(ReasoningPattern),
  reasoning_type: ReasoningType,
) -> Float {
  let base = 0.65
  let pattern_bonus = int.to_float(list.length(patterns)) *. 0.08

  // Bonus for well-structured reasoning types
  let type_bonus = case reasoning_type {
    Categorical -> 0.1
    SufficientCondition -> 0.1
    NecessaryCondition -> 0.1
    Disjunctive -> 0.08
    _ -> 0.0
  }

  let total = base +. pattern_bonus +. type_bonus
  case total >. 1.0 {
    True -> 1.0
    False -> total
  }
}

// ============ Conversion to Fixtures ============

/// Convert LogiQA translation to test fixture
pub fn translation_to_fixture(
  translation: LogiQATranslation,
  original_label: ExampleLabel,
  reasoning_type: ReasoningType,
) -> TestFixture {
  let validity = case original_label {
    Entailment -> ExpectedValid
    Contradiction -> ExpectedInvalid(None)
    Neutral -> ExpectedEither("Multiple interpretations possible")
    Unknown -> UnknownValidity
  }

  let difficulty = case translation.confidence {
    c if c >=. 0.85 -> Easy
    c if c >=. 0.7 -> Medium
    _ -> Hard
  }

  let tags =
    translation.patterns
    |> list.map(pattern_to_string)
    |> list.append(["logiqa", reasoning_type_to_string(reasoning_type)])

  TestFixture(
    id: translation.original_id <> "_translated",
    name: "LogiQA: " <> reasoning_type_to_string(reasoning_type),
    category: ExternalDataset(source: "LogiQA 2.0"),
    natural_language: "",
    expected_logic_system: translation.logic_system,
    expected_premises: translation.translated_premises,
    expected_conclusion: translation.translated_hypothesis,
    expected_validity: validity,
    difficulty: difficulty,
    tags: tags,
    source: Some("LogiQA:" <> translation.original_id),
  )
}

/// Convert pattern to string
fn pattern_to_string(pattern: ReasoningPattern) -> String {
  case pattern {
    ModusPonens -> "modus_ponens"
    ModusTollens -> "modus_tollens"
    HypotheticalSyllogism -> "hypothetical_syllogism"
    DisjunctiveSyllogism -> "disjunctive_syllogism"
    ConditionalReasoning -> "conditional"
    NegationPattern -> "negation"
    UniversalInstantiation -> "universal"
    ModalReasoning -> "modal"
    EpistemicReasoning -> "epistemic"
    DeonticReasoning -> "deontic"
  }
}

/// Convert reasoning type to string
pub fn reasoning_type_to_string(rt: ReasoningType) -> String {
  case rt {
    Categorical -> "categorical"
    SufficientCondition -> "sufficient_condition"
    NecessaryCondition -> "necessary_condition"
    Disjunctive -> "disjunctive"
    Conjunctive -> "conjunctive"
    Causal -> "causal"
    Analogy -> "analogy"
    Mixed -> "mixed"
  }
}

// ============ Main Interface ============

/// Get all LogiQA fixtures
pub fn get_all_fixtures(config: LogiQAConfig) -> List(TestFixture) {
  extended_logiqa_examples()
  |> filter_examples(config)
  |> list.take(config.max_examples)
  |> list.flat_map(fn(example) {
    translate_example(example, config)
    |> list.filter_map(fn(translation) {
      let pair =
        list.find(example.premise_hypothesis_pairs, fn(p) { p.is_correct })
      case pair {
        Ok(p) ->
          Ok(translation_to_fixture(
            translation,
            p.label,
            example.reasoning_type,
          ))
        Error(_) -> Error(Nil)
      }
    })
  })
}

/// Filter examples by config
fn filter_examples(
  examples: List(LogiQAExample),
  config: LogiQAConfig,
) -> List(LogiQAExample) {
  case config.reasoning_type_filter {
    None -> examples
    Some(rt) -> list.filter(examples, fn(ex) { ex.reasoning_type == rt })
  }
}

/// Get mock fixtures (for testing)
pub fn get_mock_fixtures() -> List(TestFixture) {
  get_all_fixtures(default_config())
}

/// Get statistics about LogiQA dataset
pub fn dataset_statistics() -> LogiQAStatistics {
  let examples = extended_logiqa_examples()

  let by_type =
    examples
    |> list.group(fn(ex) { reasoning_type_to_string(ex.reasoning_type) })
    |> dict.map_values(fn(_key, group) { list.length(group) })

  let total_pairs =
    examples
    |> list.map(fn(ex) { list.length(ex.premise_hypothesis_pairs) })
    |> list.fold(0, fn(acc, n) { acc + n })

  LogiQAStatistics(
    total_examples: list.length(examples),
    total_pairs: total_pairs,
    by_reasoning_type: by_type,
    average_premises: calculate_average_premises(examples),
  )
}

/// LogiQA statistics
pub type LogiQAStatistics {
  LogiQAStatistics(
    total_examples: Int,
    total_pairs: Int,
    by_reasoning_type: Dict(String, Int),
    average_premises: Float,
  )
}

/// Calculate average premise count
fn calculate_average_premises(examples: List(LogiQAExample)) -> Float {
  case examples {
    [] -> 0.0
    _ -> {
      let total =
        examples
        |> list.flat_map(fn(ex) { ex.premise_hypothesis_pairs })
        |> list.map(fn(pair) { list.length(pair.premises) })
        |> list.fold(0, fn(acc, n) { acc + n })
      let count =
        examples
        |> list.flat_map(fn(ex) { ex.premise_hypothesis_pairs })
        |> list.length
      case count {
        0 -> 0.0
        _ -> int.to_float(total) /. int.to_float(count)
      }
    }
  }
}

/// Convert DatasetExample to LogiQAExample (for huggingface loader compatibility)
pub fn from_dataset_example(example: DatasetExample) -> LogiQAExample {
  LogiQAExample(
    id: example.id,
    context: string.join(example.premises, " "),
    question: "",
    options: [example.conclusion],
    correct_answer: 0,
    reasoning_type: detect_reasoning_type_from_text(
      string.join(example.premises, " ") <> " " <> example.conclusion,
    ),
    premise_hypothesis_pairs: [
      PremiseHypothesisPair(
        premises: example.premises,
        hypothesis: example.conclusion,
        is_correct: True,
        label: example.label,
      ),
    ],
  )
}

/// Detect reasoning type from text
fn detect_reasoning_type_from_text(text: String) -> ReasoningType {
  let lower = string.lowercase(text)
  case string.contains(lower, "all ") || string.contains(lower, "every ") {
    True -> Categorical
    False ->
      case string.contains(lower, "if ") {
        True -> SufficientCondition
        False ->
          case string.contains(lower, "only if") {
            True -> NecessaryCondition
            False ->
              case string.contains(lower, "either") {
                True -> Disjunctive
                False ->
                  case string.contains(lower, "whenever") {
                    True -> Causal
                    False -> Mixed
                  }
              }
          }
      }
  }
}
