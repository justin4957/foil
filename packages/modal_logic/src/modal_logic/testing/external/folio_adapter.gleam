//// FOLIO Dataset Adapter
////
//// This module provides specialized handling for the FOLIO dataset
//// (First Order Logic Inference Open-domain) with enhanced translation
//// from FOL to modal logic.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Implies, K, Necessary, Not, Or,
  Possible, S5, T,
}
import modal_logic/testing/external/huggingface_loader.{
  type DatasetExample, type ExampleLabel, Contradiction, DatasetExample,
  Entailment, Neutral, Unknown,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture, TestFixture}
import modal_logic/testing/test_config.{
  type Difficulty, type ExpectedValidity, Easy, ExpectedEither, ExpectedInvalid,
  ExpectedValid, ExternalDataset, Hard, Medium, Unknown as UnknownValidity,
}

// ============ Core Types ============

/// FOLIO-specific configuration
pub type FOLIOConfig {
  FOLIOConfig(
    /// Enable modal translation
    translate_to_modal: Bool,
    /// Include universal quantification as necessity
    universal_as_necessity: Bool,
    /// Minimum premise count
    min_premises: Int,
    /// Maximum premise count
    max_premises: Int,
    /// Filter by label
    label_filter: Option(ExampleLabel),
  )
}

/// A translated FOLIO example
pub type FOLIOTranslation {
  FOLIOTranslation(
    /// Original example ID
    original_id: String,
    /// Original premises (natural language)
    original_premises: List(String),
    /// Original conclusion
    original_conclusion: String,
    /// Translated premises
    translated_premises: List(Proposition),
    /// Translated conclusion
    translated_conclusion: Proposition,
    /// Translation confidence
    confidence: Float,
    /// Translation notes
    notes: List(String),
    /// Detected patterns
    detected_patterns: List(FOLIOPattern),
  )
}

/// Patterns detected in FOLIO examples
pub type FOLIOPattern {
  /// Universal quantification: "All X are Y"
  UniversalQuantification
  /// Existential quantification: "Some X are Y"
  ExistentialQuantification
  /// Conditional: "If X then Y"
  Conditional
  /// Negation: "No X" or "not"
  Negation
  /// Modal: "necessarily", "possibly"
  Modal
  /// Conjunction: "and"
  Conjunction
  /// Disjunction: "or"
  Disjunction
}

// ============ Configuration ============

/// Default FOLIO configuration
pub fn default_config() -> FOLIOConfig {
  FOLIOConfig(
    translate_to_modal: True,
    universal_as_necessity: True,
    min_premises: 1,
    max_premises: 10,
    label_filter: None,
  )
}

/// Configuration for entailment examples only
pub fn entailment_config() -> FOLIOConfig {
  FOLIOConfig(..default_config(), label_filter: Some(Entailment))
}

/// Configuration for contradiction examples only
pub fn contradiction_config() -> FOLIOConfig {
  FOLIOConfig(..default_config(), label_filter: Some(Contradiction))
}

// ============ Enhanced FOLIO Examples ============

/// Get extended FOLIO examples with richer content
pub fn extended_folio_examples() -> List(DatasetExample) {
  [
    // Syllogistic reasoning
    DatasetExample(
      id: "folio_ext_1",
      premises: [
        "All philosophers are thinkers.",
        "Socrates is a philosopher.",
      ],
      conclusion: "Socrates is a thinker.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([#("type", "syllogism"), #("difficulty", "easy")]),
    ),
    DatasetExample(
      id: "folio_ext_2",
      premises: [
        "No reptiles are mammals.",
        "All snakes are reptiles.",
        "Some animals are snakes.",
      ],
      conclusion: "Some animals are not mammals.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([
        #("type", "syllogism"),
        #("difficulty", "medium"),
      ]),
    ),
    // Modal reasoning
    DatasetExample(
      id: "folio_ext_3",
      premises: [
        "Necessarily, all mathematicians understand logic.",
        "Alice is a mathematician.",
      ],
      conclusion: "Alice necessarily understands logic.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([#("type", "modal"), #("difficulty", "medium")]),
    ),
    DatasetExample(
      id: "folio_ext_4",
      premises: [
        "It is possible that there exists intelligent life elsewhere.",
        "What is possible is not necessarily false.",
      ],
      conclusion: "It is not necessarily false that there exists intelligent life elsewhere.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([#("type", "modal"), #("difficulty", "hard")]),
    ),
    // Conditional reasoning
    DatasetExample(
      id: "folio_ext_5",
      premises: [
        "If someone is a doctor, they have medical training.",
        "If someone has medical training, they studied biology.",
        "Alice is a doctor.",
      ],
      conclusion: "Alice studied biology.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([
        #("type", "conditional"),
        #("difficulty", "medium"),
      ]),
    ),
    // Disjunctive reasoning
    DatasetExample(
      id: "folio_ext_6",
      premises: [
        "Either the butler or the gardener committed the crime.",
        "The butler has an alibi.",
      ],
      conclusion: "The gardener committed the crime.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([
        #("type", "disjunctive"),
        #("difficulty", "easy"),
      ]),
    ),
    // Negation patterns
    DatasetExample(
      id: "folio_ext_7",
      premises: [
        "No birds are mammals.",
        "Penguins are birds.",
      ],
      conclusion: "Penguins are not mammals.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([#("type", "negation"), #("difficulty", "easy")]),
    ),
    // Invalid inference (fallacy)
    DatasetExample(
      id: "folio_ext_8",
      premises: [
        "If it rains, the ground is wet.",
        "The ground is wet.",
      ],
      conclusion: "It rained.",
      label: Neutral,
      source_format: "folio_extended",
      metadata: dict.from_list([
        #("type", "fallacy"),
        #("fallacy_type", "affirming_consequent"),
      ]),
    ),
    // Contradiction detection
    DatasetExample(
      id: "folio_ext_9",
      premises: [
        "All cats are mammals.",
        "Fluffy is a cat.",
        "Fluffy is not a mammal.",
      ],
      conclusion: "Fluffy is both a mammal and not a mammal.",
      label: Contradiction,
      source_format: "folio_extended",
      metadata: dict.from_list([#("type", "contradiction")]),
    ),
    // Complex multi-step reasoning
    DatasetExample(
      id: "folio_ext_10",
      premises: [
        "All engineers understand mathematics.",
        "All people who understand mathematics can solve logic puzzles.",
        "Some engineers are also artists.",
        "Bob is an engineer and an artist.",
      ],
      conclusion: "Bob can solve logic puzzles.",
      label: Entailment,
      source_format: "folio_extended",
      metadata: dict.from_list([
        #("type", "multi_step"),
        #("difficulty", "hard"),
      ]),
    ),
  ]
}

// ============ Translation Functions ============

/// Translate a FOLIO example to modal logic
pub fn translate_example(
  example: DatasetExample,
  config: FOLIOConfig,
) -> FOLIOTranslation {
  let patterns = detect_patterns(example)

  let translated_premises =
    example.premises
    |> list.index_map(fn(premise, idx) {
      translate_sentence(premise, idx, config, patterns)
    })

  let translated_conclusion =
    translate_sentence(
      example.conclusion,
      list.length(example.premises),
      config,
      patterns,
    )

  let confidence = calculate_translation_confidence(patterns, example)
  let notes = generate_translation_notes(patterns, example)

  FOLIOTranslation(
    original_id: example.id,
    original_premises: example.premises,
    original_conclusion: example.conclusion,
    translated_premises: translated_premises,
    translated_conclusion: translated_conclusion,
    confidence: confidence,
    notes: notes,
    detected_patterns: patterns,
  )
}

/// Detect logical patterns in an example
fn detect_patterns(example: DatasetExample) -> List(FOLIOPattern) {
  let all_text =
    example.premises
    |> list.append([example.conclusion])
    |> string.join(" ")
    |> string.lowercase

  let patterns = []

  let patterns = case
    string.contains(all_text, "all ")
    || string.contains(all_text, "every ")
  {
    True -> [UniversalQuantification, ..patterns]
    False -> patterns
  }

  let patterns = case
    string.contains(all_text, "some ")
    || string.contains(all_text, "there exists")
  {
    True -> [ExistentialQuantification, ..patterns]
    False -> patterns
  }

  let patterns = case
    string.contains(all_text, "if ")
    || string.contains(all_text, "implies")
  {
    True -> [Conditional, ..patterns]
    False -> patterns
  }

  let patterns = case
    string.contains(all_text, "no ")
    || string.contains(all_text, " not ")
    || string.contains(all_text, "n't")
  {
    True -> [Negation, ..patterns]
    False -> patterns
  }

  let patterns = case
    string.contains(all_text, "necessarily")
    || string.contains(all_text, "possibly")
    || string.contains(all_text, "must be")
  {
    True -> [Modal, ..patterns]
    False -> patterns
  }

  let patterns = case string.contains(all_text, " and ") {
    True -> [Conjunction, ..patterns]
    False -> patterns
  }

  let patterns = case
    string.contains(all_text, " or ")
    || string.contains(all_text, "either")
  {
    True -> [Disjunction, ..patterns]
    False -> patterns
  }

  patterns
}

/// Translate a single sentence to a proposition
fn translate_sentence(
  sentence: String,
  index: Int,
  config: FOLIOConfig,
  patterns: List(FOLIOPattern),
) -> Proposition {
  let lower = string.lowercase(sentence)
  let base_atom = "p" <> int.to_string(index)

  // Check for universal quantification (translate to necessity if configured)
  case
    config.universal_as_necessity
    && { string.starts_with(lower, "all ") || string.starts_with(lower, "every ") }
  {
    True -> translate_universal(lower, base_atom)
    False ->
      case string.starts_with(lower, "some ") {
        True -> translate_existential(lower, base_atom)
        False ->
          case
            string.starts_with(lower, "if ")
            || string.contains(lower, " implies ")
          {
            True -> translate_conditional(lower, base_atom)
            False ->
              case
                string.starts_with(lower, "no ")
                || string.contains(lower, " not ")
              {
                True -> translate_negation(lower, base_atom)
                False ->
                  case string.contains(lower, "necessarily") {
                    True -> translate_necessity(lower, base_atom)
                    False ->
                      case string.contains(lower, "possibly") {
                        True -> translate_possibility(lower, base_atom)
                        False ->
                          case
                            string.contains(lower, " or ")
                            || string.contains(lower, "either")
                          {
                            True -> translate_disjunction(lower, base_atom)
                            False -> Atom(base_atom)
                          }
                      }
                  }
              }
          }
      }
  }
}

/// Translate universal quantification: "All X are Y" -> □(X → Y)
fn translate_universal(sentence: String, base_atom: String) -> Proposition {
  let parts =
    sentence
    |> string.replace("all ", "")
    |> string.replace("every ", "")
    |> string.split(" are ")

  case parts {
    [subject, predicate] ->
      Necessary(
        Implies(
          Atom(clean_term(subject)),
          Atom(clean_term(predicate)),
        ),
      )
    _ -> Atom(base_atom)
  }
}

/// Translate existential quantification: "Some X are Y" -> ◇(X ∧ Y)
fn translate_existential(sentence: String, base_atom: String) -> Proposition {
  let parts =
    sentence
    |> string.replace("some ", "")
    |> string.split(" are ")

  case parts {
    [subject, predicate] ->
      Possible(
        And(
          Atom(clean_term(subject)),
          Atom(clean_term(predicate)),
        ),
      )
    _ -> Atom(base_atom)
  }
}

/// Translate conditional: "If X then Y" -> X → Y
fn translate_conditional(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("if ", "")
    |> string.replace(", then ", " then ")

  let parts = string.split(cleaned, " then ")
  case parts {
    [antecedent, consequent] ->
      Implies(
        Atom(clean_term(antecedent)),
        Atom(clean_term(consequent)),
      )
    _ -> {
      // Try splitting on "implies"
      let parts2 = string.split(sentence, " implies ")
      case parts2 {
        [a, c] -> Implies(Atom(clean_term(a)), Atom(clean_term(c)))
        _ -> Atom(base_atom)
      }
    }
  }
}

/// Translate negation
fn translate_negation(sentence: String, base_atom: String) -> Proposition {
  case string.starts_with(string.lowercase(sentence), "no ") {
    True -> {
      let parts =
        sentence
        |> string.replace("No ", "")
        |> string.replace("no ", "")
        |> string.split(" are ")

      case parts {
        [subject, predicate] ->
          Not(And(Atom(clean_term(subject)), Atom(clean_term(predicate))))
        _ -> Not(Atom(base_atom))
      }
    }
    False -> {
      // Contains "not"
      let cleaned =
        sentence
        |> string.replace(" not ", "_NOT_")
        |> clean_term

      Not(Atom(cleaned))
    }
  }
}

/// Translate necessity: "necessarily X" -> □X
fn translate_necessity(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("necessarily", "")
    |> string.replace(",", "")
    |> clean_term

  Necessary(Atom(cleaned))
}

/// Translate possibility: "possibly X" -> ◇X
fn translate_possibility(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("possibly", "")
    |> string.replace(",", "")
    |> clean_term

  Possible(Atom(cleaned))
}

/// Translate disjunction: "X or Y" -> X ∨ Y
fn translate_disjunction(sentence: String, base_atom: String) -> Proposition {
  let cleaned =
    sentence
    |> string.replace("either ", "")

  let parts = string.split(cleaned, " or ")
  case parts {
    [left, right] -> Or(Atom(clean_term(left)), Atom(clean_term(right)))
    _ -> Atom(base_atom)
  }
}

/// Clean a term for use as atom name
fn clean_term(term: String) -> String {
  term
  |> string.trim
  |> string.replace(" ", "_")
  |> string.replace(".", "")
  |> string.replace(",", "")
  |> string.lowercase
}

/// Calculate translation confidence
fn calculate_translation_confidence(
  patterns: List(FOLIOPattern),
  example: DatasetExample,
) -> Float {
  let base_confidence = 0.7

  // More patterns = higher confidence
  let pattern_bonus = int.to_float(list.length(patterns)) *. 0.05

  // Known metadata types increase confidence
  let metadata_bonus = case dict.get(example.metadata, "type") {
    Ok(_) -> 0.1
    Error(_) -> 0.0
  }

  let total = base_confidence +. pattern_bonus +. metadata_bonus

  case total >. 1.0 {
    True -> 1.0
    False -> total
  }
}

/// Generate translation notes
fn generate_translation_notes(
  patterns: List(FOLIOPattern),
  example: DatasetExample,
) -> List(String) {
  let pattern_notes =
    patterns
    |> list.map(fn(p) {
      case p {
        UniversalQuantification -> "Detected universal quantification"
        ExistentialQuantification -> "Detected existential quantification"
        Conditional -> "Detected conditional structure"
        Negation -> "Detected negation"
        Modal -> "Detected modal expression"
        Conjunction -> "Detected conjunction"
        Disjunction -> "Detected disjunction"
      }
    })

  let label_note = case example.label {
    Entailment -> ["Expected: Valid entailment"]
    Contradiction -> ["Expected: Contradiction"]
    Neutral -> ["Expected: Neutral/ambiguous"]
    Unknown -> ["Expected: Unknown"]
  }

  list.append(pattern_notes, label_note)
}

// ============ Conversion to Fixtures ============

/// Convert FOLIO translation to test fixture
pub fn translation_to_fixture(
  translation: FOLIOTranslation,
  original_label: ExampleLabel,
) -> TestFixture {
  let validity = case original_label {
    Entailment -> ExpectedValid
    Contradiction -> ExpectedInvalid(None)
    Neutral -> ExpectedEither("Multiple interpretations possible")
    Unknown -> UnknownValidity
  }

  let difficulty = case translation.confidence {
    c if c >=. 0.9 -> Easy
    c if c >=. 0.7 -> Medium
    _ -> Hard
  }

  let tags =
    translation.detected_patterns
    |> list.map(pattern_to_string)
    |> list.append(["folio", "translated"])

  let natural_language =
    translation.original_premises
    |> list.append(["Therefore, " <> translation.original_conclusion])
    |> string.join(" ")

  TestFixture(
    id: translation.original_id <> "_translated",
    name: "FOLIO: " <> string.slice(translation.original_conclusion, 0, 40),
    category: ExternalDataset(source: "FOLIO"),
    natural_language: natural_language,
    expected_logic_system: detect_logic_system(translation),
    expected_premises: translation.translated_premises,
    expected_conclusion: translation.translated_conclusion,
    expected_validity: validity,
    difficulty: difficulty,
    tags: tags,
    source: Some("FOLIO:" <> translation.original_id),
  )
}

/// Detect appropriate logic system from translation
fn detect_logic_system(translation: FOLIOTranslation) -> LogicSystem {
  case list.contains(translation.detected_patterns, Modal) {
    True -> S5
    False ->
      case list.contains(translation.detected_patterns, UniversalQuantification) {
        True -> T
        False -> K
      }
  }
}

/// Convert pattern to string
fn pattern_to_string(pattern: FOLIOPattern) -> String {
  case pattern {
    UniversalQuantification -> "universal"
    ExistentialQuantification -> "existential"
    Conditional -> "conditional"
    Negation -> "negation"
    Modal -> "modal"
    Conjunction -> "conjunction"
    Disjunction -> "disjunction"
  }
}

// ============ Main Interface ============

/// Get all FOLIO fixtures
pub fn get_all_fixtures(config: FOLIOConfig) -> List(TestFixture) {
  extended_folio_examples()
  |> filter_examples(config)
  |> list.map(fn(example) {
    let translation = translate_example(example, config)
    translation_to_fixture(translation, example.label)
  })
}

/// Filter examples based on configuration
fn filter_examples(
  examples: List(DatasetExample),
  config: FOLIOConfig,
) -> List(DatasetExample) {
  examples
  |> list.filter(fn(ex) {
    let premise_count = list.length(ex.premises)
    premise_count >= config.min_premises && premise_count <= config.max_premises
  })
  |> list.filter(fn(ex) {
    case config.label_filter {
      None -> True
      Some(label) -> ex.label == label
    }
  })
}

/// Get statistics about FOLIO dataset
pub fn dataset_statistics() -> FOLIOStatistics {
  let examples = extended_folio_examples()

  let by_label =
    examples
    |> list.group(fn(ex) { label_to_string(ex.label) })
    |> dict.map_values(fn(_key, group) { list.length(group) })

  let by_type =
    examples
    |> list.filter_map(fn(ex) {
      case dict.get(ex.metadata, "type") {
        Ok(t) -> Ok(t)
        Error(_) -> Error(Nil)
      }
    })
    |> list.group(fn(t) { t })
    |> dict.map_values(fn(_key, group) { list.length(group) })

  FOLIOStatistics(
    total_examples: list.length(examples),
    by_label: by_label,
    by_type: by_type,
    average_premises: calculate_average_premises(examples),
  )
}

/// FOLIO statistics
pub type FOLIOStatistics {
  FOLIOStatistics(
    total_examples: Int,
    by_label: Dict(String, Int),
    by_type: Dict(String, Int),
    average_premises: Float,
  )
}

/// Convert label to string
fn label_to_string(label: ExampleLabel) -> String {
  case label {
    Entailment -> "entailment"
    Contradiction -> "contradiction"
    Neutral -> "neutral"
    Unknown -> "unknown"
  }
}

/// Calculate average premise count
fn calculate_average_premises(examples: List(DatasetExample)) -> Float {
  case examples {
    [] -> 0.0
    _ -> {
      let total =
        list.fold(examples, 0, fn(acc, ex) {
          acc + list.length(ex.premises)
        })
      int.to_float(total) /. int.to_float(list.length(examples))
    }
  }
}
