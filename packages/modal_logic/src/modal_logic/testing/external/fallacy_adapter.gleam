//// Logical Fallacy Dataset Adapter
////
//// This module provides integration with logical fallacy datasets for
//// negative testing - validating that invalid arguments are correctly
//// identified as invalid.
////
//// Primary dataset: Logic Fallacy Dataset (~2,449 examples)
//// Source: https://github.com/causalNLP/logical-fallacy

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Implies, K, Necessary, Not, Or,
  Possible, T,
}
import modal_logic/testing/external/huggingface_loader.{
  type DatasetExample, type ExampleLabel, Contradiction, DatasetExample,
  Entailment, Neutral, Unknown,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture, TestFixture}
import modal_logic/testing/test_config.{
  Easy, ExpectedEither, ExpectedInvalid, ExternalDataset, Hard, Medium,
}

// ============ Core Types ============

/// Fallacy dataset configuration
pub type FallacyConfig {
  FallacyConfig(
    /// Maximum examples to load
    max_examples: Int,
    /// Filter by fallacy type
    fallacy_type_filter: Option(FallacyType),
    /// Include formal fallacies only
    formal_only: Bool,
    /// Minimum confidence for translations
    min_confidence: Float,
    /// Cache duration in seconds
    cache_duration: Int,
  )
}

/// Types of logical fallacies
pub type FallacyType {
  // Formal fallacies (invalid logical structure)
  /// Affirming the consequent: If P then Q, Q, therefore P
  AffirmingConsequent
  /// Denying the antecedent: If P then Q, not P, therefore not Q
  DenyingAntecedent
  /// Affirming a disjunct: P or Q, P, therefore not Q
  AffirmingDisjunct
  /// Undistributed middle: All A are B, All C are B, therefore All A are C
  UndistributedMiddle
  /// Illicit major: All A are B, No C are A, therefore No C are B
  IllicitMajor
  /// Illicit minor: All A are B, All A are C, therefore All C are B
  IllicitMinor
  /// Four terms fallacy
  FourTerms
  /// Existential fallacy
  ExistentialFallacy
  // Informal fallacies (content/context issues)
  /// Ad hominem: attacking the person rather than the argument
  AdHominem
  /// Straw man: misrepresenting the opponent's argument
  StrawMan
  /// Appeal to authority: using authority as evidence
  AppealToAuthority
  /// Appeal to emotion: using emotion instead of logic
  AppealToEmotion
  /// False dilemma: presenting only two options when more exist
  FalseDilemma
  /// Slippery slope: claiming one event leads to extreme consequences
  SlipperySlope
  /// Circular reasoning: conclusion is assumed in premises
  CircularReasoning
  /// Hasty generalization: drawing conclusions from insufficient evidence
  HastyGeneralization
  /// Red herring: introducing irrelevant topic
  RedHerring
  /// Equivocation: using ambiguous terms
  Equivocation
}

/// A fallacy example
pub type FallacyExample {
  FallacyExample(
    /// Unique identifier
    id: String,
    /// The fallacious argument text
    argument_text: String,
    /// Extracted premises
    premises: List(String),
    /// Extracted conclusion
    conclusion: String,
    /// Type of fallacy
    fallacy_type: FallacyType,
    /// Is this a formal fallacy?
    is_formal: Bool,
    /// Explanation of why it's fallacious
    explanation: String,
    /// Source metadata
    metadata: Dict(String, String),
  )
}

/// Translation result for fallacy
pub type FallacyTranslation {
  FallacyTranslation(
    /// Original example ID
    original_id: String,
    /// Fallacy type
    fallacy_type: FallacyType,
    /// Translated premises
    translated_premises: List(Proposition),
    /// Translated (invalid) conclusion
    translated_conclusion: Proposition,
    /// Logic system for evaluation
    logic_system: LogicSystem,
    /// Translation confidence
    confidence: Float,
    /// Why the argument is invalid
    invalidity_reason: String,
  )
}

// ============ Configuration ============

/// Default fallacy configuration
pub fn default_config() -> FallacyConfig {
  FallacyConfig(
    max_examples: 100,
    fallacy_type_filter: None,
    formal_only: False,
    min_confidence: 0.6,
    cache_duration: 604_800,
  )
}

/// Configuration for formal fallacies only
pub fn formal_fallacy_config() -> FallacyConfig {
  FallacyConfig(..default_config(), formal_only: True)
}

/// Configuration for specific fallacy type
pub fn type_config(fallacy_type: FallacyType) -> FallacyConfig {
  FallacyConfig(..default_config(), fallacy_type_filter: Some(fallacy_type))
}

/// Comprehensive configuration
pub fn comprehensive_config() -> FallacyConfig {
  FallacyConfig(
    max_examples: 500,
    fallacy_type_filter: None,
    formal_only: False,
    min_confidence: 0.5,
    cache_duration: 604_800,
  )
}

// ============ Mock Fallacy Examples ============

/// Extended fallacy examples covering major types
pub fn extended_fallacy_examples() -> List(FallacyExample) {
  [
    // Affirming the consequent
    FallacyExample(
      id: "fallacy_ac_1",
      argument_text: "If it rains, the ground is wet. The ground is wet. Therefore, it rained.",
      premises: ["If it rains, the ground is wet", "The ground is wet"],
      conclusion: "It rained",
      fallacy_type: AffirmingConsequent,
      is_formal: True,
      explanation: "The ground could be wet for other reasons (sprinklers, spill)",
      metadata: dict.from_list([#("category", "formal")]),
    ),
    FallacyExample(
      id: "fallacy_ac_2",
      argument_text: "All dogs are mammals. Fluffy is a mammal. Therefore, Fluffy is a dog.",
      premises: ["All dogs are mammals", "Fluffy is a mammal"],
      conclusion: "Fluffy is a dog",
      fallacy_type: AffirmingConsequent,
      is_formal: True,
      explanation: "Fluffy could be any mammal, not necessarily a dog",
      metadata: dict.from_list([#("category", "formal")]),
    ),
    // Denying the antecedent
    FallacyExample(
      id: "fallacy_da_1",
      argument_text: "If you study hard, you will pass. You did not study hard. Therefore, you will not pass.",
      premises: ["If you study hard, you will pass", "You did not study hard"],
      conclusion: "You will not pass",
      fallacy_type: DenyingAntecedent,
      is_formal: True,
      explanation: "One might pass through other means (natural talent, luck)",
      metadata: dict.from_list([#("category", "formal")]),
    ),
    FallacyExample(
      id: "fallacy_da_2",
      argument_text: "If it's a bird, it can fly. This is not a bird. Therefore, it cannot fly.",
      premises: ["If it's a bird, it can fly", "This is not a bird"],
      conclusion: "It cannot fly",
      fallacy_type: DenyingAntecedent,
      is_formal: True,
      explanation: "Non-birds like bats and insects can also fly",
      metadata: dict.from_list([#("category", "formal")]),
    ),
    // Affirming a disjunct
    FallacyExample(
      id: "fallacy_ad_1",
      argument_text: "Either John is at home or John is at work. John is at home. Therefore, John is not at work.",
      premises: [
        "Either John is at home or John is at work",
        "John is at home",
      ],
      conclusion: "John is not at work",
      fallacy_type: AffirmingDisjunct,
      is_formal: True,
      explanation: "The 'or' could be inclusive - John could be at both places at different times",
      metadata: dict.from_list([#("category", "formal")]),
    ),
    // Undistributed middle
    FallacyExample(
      id: "fallacy_um_1",
      argument_text: "All cats are animals. All dogs are animals. Therefore, all cats are dogs.",
      premises: ["All cats are animals", "All dogs are animals"],
      conclusion: "All cats are dogs",
      fallacy_type: UndistributedMiddle,
      is_formal: True,
      explanation: "The middle term 'animals' is not distributed in either premise",
      metadata: dict.from_list([#("category", "formal")]),
    ),
    FallacyExample(
      id: "fallacy_um_2",
      argument_text: "All humans are mortal. All Greeks are mortal. Therefore, all humans are Greeks.",
      premises: ["All humans are mortal", "All Greeks are mortal"],
      conclusion: "All humans are Greeks",
      fallacy_type: UndistributedMiddle,
      is_formal: True,
      explanation: "Being mortal doesn't establish a connection between humans and Greeks",
      metadata: dict.from_list([#("category", "formal")]),
    ),
    // False dilemma
    FallacyExample(
      id: "fallacy_fd_1",
      argument_text: "You're either with us or against us. You're not with us. Therefore, you're against us.",
      premises: ["You're either with us or against us", "You're not with us"],
      conclusion: "You're against us",
      fallacy_type: FalseDilemma,
      is_formal: False,
      explanation: "There are other options: neutral, undecided, or conditionally supportive",
      metadata: dict.from_list([#("category", "informal")]),
    ),
    // Slippery slope
    FallacyExample(
      id: "fallacy_ss_1",
      argument_text: "If we allow students to redo one test, they'll want to redo all tests. Eventually, grades will become meaningless. Therefore, we shouldn't allow any test redos.",
      premises: [
        "If we allow students to redo one test, they'll want to redo all tests",
        "Eventually, grades will become meaningless",
      ],
      conclusion: "We shouldn't allow any test redos",
      fallacy_type: SlipperySlope,
      is_formal: False,
      explanation: "The chain of consequences is not inevitable",
      metadata: dict.from_list([#("category", "informal")]),
    ),
    // Circular reasoning
    FallacyExample(
      id: "fallacy_cr_1",
      argument_text: "The Bible is true because it's the word of God. We know it's the word of God because the Bible says so.",
      premises: [
        "The Bible is true because it's the word of God",
        "The Bible says it's the word of God",
      ],
      conclusion: "The Bible is true",
      fallacy_type: CircularReasoning,
      is_formal: False,
      explanation: "The conclusion is assumed in the premises",
      metadata: dict.from_list([#("category", "informal")]),
    ),
    // Ad hominem
    FallacyExample(
      id: "fallacy_ah_1",
      argument_text: "Dr. Smith argues that climate change is real. But Dr. Smith once failed a chemistry exam. Therefore, climate change is not real.",
      premises: [
        "Dr. Smith argues that climate change is real",
        "Dr. Smith once failed a chemistry exam",
      ],
      conclusion: "Climate change is not real",
      fallacy_type: AdHominem,
      is_formal: False,
      explanation: "The person's character doesn't affect the validity of their argument",
      metadata: dict.from_list([#("category", "informal")]),
    ),
    // Hasty generalization
    FallacyExample(
      id: "fallacy_hg_1",
      argument_text: "I met two people from that city and they were both rude. Therefore, everyone from that city is rude.",
      premises: [
        "I met two people from that city",
        "Both of them were rude",
      ],
      conclusion: "Everyone from that city is rude",
      fallacy_type: HastyGeneralization,
      is_formal: False,
      explanation: "Sample size of two is insufficient to generalize to an entire population",
      metadata: dict.from_list([#("category", "informal")]),
    ),
  ]
}

// ============ Translation Functions ============

/// Translate a fallacy example to modal logic
pub fn translate_example(
  example: FallacyExample,
  config: FallacyConfig,
) -> FallacyTranslation {
  let translated_premises =
    example.premises
    |> list.index_map(fn(p, idx) {
      translate_premise(p, idx, example.fallacy_type)
    })

  let translated_conclusion =
    translate_conclusion(
      example.conclusion,
      list.length(example.premises),
      example.fallacy_type,
    )

  let logic_system = detect_logic_system(example)
  let confidence = calculate_confidence(example)

  FallacyTranslation(
    original_id: example.id,
    fallacy_type: example.fallacy_type,
    translated_premises: translated_premises,
    translated_conclusion: translated_conclusion,
    logic_system: logic_system,
    confidence: confidence,
    invalidity_reason: example.explanation,
  )
}

/// Translate a premise based on fallacy type
fn translate_premise(
  premise: String,
  index: Int,
  fallacy_type: FallacyType,
) -> Proposition {
  let lower = string.lowercase(premise)
  let base_atom = "f" <> int.to_string(index)

  case fallacy_type {
    AffirmingConsequent | DenyingAntecedent -> {
      // These involve conditionals
      case string.starts_with(lower, "if ") {
        True -> translate_conditional(lower, base_atom)
        False -> translate_simple(lower, base_atom)
      }
    }
    AffirmingDisjunct | FalseDilemma -> {
      // These involve disjunctions
      case string.contains(lower, " or ") {
        True -> translate_disjunction(lower, base_atom)
        False -> translate_simple(lower, base_atom)
      }
    }
    UndistributedMiddle | IllicitMajor | IllicitMinor -> {
      // These involve universals
      case string.starts_with(lower, "all ") {
        True -> translate_universal(lower, base_atom)
        False -> translate_simple(lower, base_atom)
      }
    }
    _ -> translate_simple(lower, base_atom)
  }
}

/// Translate the conclusion
fn translate_conclusion(
  conclusion: String,
  premise_count: Int,
  fallacy_type: FallacyType,
) -> Proposition {
  let lower = string.lowercase(conclusion)
  let base_atom = "c" <> int.to_string(premise_count)

  translate_simple(lower, base_atom)
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
    _ -> {
      // Try comma separator
      let parts2 = string.split(cleaned, ", ")
      case parts2 {
        [ant, cons] -> Implies(Atom(clean_term(ant)), Atom(clean_term(cons)))
        _ -> Atom(base_atom)
      }
    }
  }
}

/// Translate disjunction: "X or Y" -> X ∨ Y
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

/// Translate universal: "All X are Y" -> □(X → Y)
fn translate_universal(sentence: String, base_atom: String) -> Proposition {
  let parts =
    sentence
    |> string.replace("all ", "")
    |> string.split(" are ")

  case parts {
    [subject, predicate] ->
      Necessary(Implies(Atom(clean_term(subject)), Atom(clean_term(predicate))))
    _ -> Atom(base_atom)
  }
}

/// Translate simple sentence
fn translate_simple(sentence: String, base_atom: String) -> Proposition {
  case
    string.contains(sentence, " not ") || string.starts_with(sentence, "not ")
  {
    True -> {
      let cleaned =
        sentence
        |> string.replace(" not ", " ")
        |> string.replace("not ", "")
        |> clean_term
      Not(Atom(cleaned))
    }
    False -> Atom(clean_term(sentence))
  }
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

/// Detect logic system for fallacy
fn detect_logic_system(example: FallacyExample) -> LogicSystem {
  case example.is_formal {
    True ->
      case example.fallacy_type {
        UndistributedMiddle | IllicitMajor | IllicitMinor -> T
        _ -> K
      }
    False -> K
  }
}

/// Calculate translation confidence
fn calculate_confidence(example: FallacyExample) -> Float {
  let base = case example.is_formal {
    True -> 0.8
    False -> 0.65
  }

  // Bonus for well-documented examples
  let metadata_bonus = case dict.get(example.metadata, "category") {
    Ok(_) -> 0.05
    Error(_) -> 0.0
  }

  base +. metadata_bonus
}

// ============ Conversion to Fixtures ============

/// Convert fallacy translation to test fixture
pub fn translation_to_fixture(translation: FallacyTranslation) -> TestFixture {
  let difficulty = case translation.confidence {
    c if c >=. 0.8 -> Easy
    c if c >=. 0.65 -> Medium
    _ -> Hard
  }

  let tags = [
    "fallacy",
    fallacy_type_to_string(translation.fallacy_type),
    "invalid",
    "negative_test",
  ]

  TestFixture(
    id: translation.original_id <> "_fallacy",
    name: "Fallacy: " <> fallacy_type_to_string(translation.fallacy_type),
    category: ExternalDataset(source: "Logic Fallacy Dataset"),
    natural_language: translation.invalidity_reason,
    expected_logic_system: translation.logic_system,
    expected_premises: translation.translated_premises,
    expected_conclusion: translation.translated_conclusion,
    expected_validity: ExpectedInvalid(Some(translation.invalidity_reason)),
    difficulty: difficulty,
    tags: tags,
    source: Some("Fallacy:" <> translation.original_id),
  )
}

/// Convert fallacy type to string
pub fn fallacy_type_to_string(ft: FallacyType) -> String {
  case ft {
    AffirmingConsequent -> "affirming_consequent"
    DenyingAntecedent -> "denying_antecedent"
    AffirmingDisjunct -> "affirming_disjunct"
    UndistributedMiddle -> "undistributed_middle"
    IllicitMajor -> "illicit_major"
    IllicitMinor -> "illicit_minor"
    FourTerms -> "four_terms"
    ExistentialFallacy -> "existential_fallacy"
    AdHominem -> "ad_hominem"
    StrawMan -> "straw_man"
    AppealToAuthority -> "appeal_to_authority"
    AppealToEmotion -> "appeal_to_emotion"
    FalseDilemma -> "false_dilemma"
    SlipperySlope -> "slippery_slope"
    CircularReasoning -> "circular_reasoning"
    HastyGeneralization -> "hasty_generalization"
    RedHerring -> "red_herring"
    Equivocation -> "equivocation"
  }
}

/// Check if fallacy type is formal
pub fn is_formal_fallacy(ft: FallacyType) -> Bool {
  case ft {
    AffirmingConsequent
    | DenyingAntecedent
    | AffirmingDisjunct
    | UndistributedMiddle
    | IllicitMajor
    | IllicitMinor
    | FourTerms
    | ExistentialFallacy -> True
    _ -> False
  }
}

// ============ Main Interface ============

/// Get all fallacy fixtures
pub fn get_all_fixtures(config: FallacyConfig) -> List(TestFixture) {
  extended_fallacy_examples()
  |> filter_examples(config)
  |> list.take(config.max_examples)
  |> list.map(fn(example) {
    let translation = translate_example(example, config)
    translation_to_fixture(translation)
  })
  |> list.filter(fn(fixture) {
    case fixture.expected_validity {
      ExpectedInvalid(_) -> True
      _ -> False
    }
  })
}

/// Filter examples by config
fn filter_examples(
  examples: List(FallacyExample),
  config: FallacyConfig,
) -> List(FallacyExample) {
  examples
  |> list.filter(fn(ex) {
    case config.formal_only {
      True -> ex.is_formal
      False -> True
    }
  })
  |> list.filter(fn(ex) {
    case config.fallacy_type_filter {
      None -> True
      Some(ft) -> ex.fallacy_type == ft
    }
  })
}

/// Get mock fixtures (for testing)
pub fn get_mock_fixtures() -> List(TestFixture) {
  get_all_fixtures(default_config())
}

/// Get formal fallacy fixtures only
pub fn get_formal_fixtures() -> List(TestFixture) {
  get_all_fixtures(formal_fallacy_config())
}

/// Get statistics about fallacy dataset
pub fn dataset_statistics() -> FallacyStatistics {
  let examples = extended_fallacy_examples()

  let by_type =
    examples
    |> list.group(fn(ex) { fallacy_type_to_string(ex.fallacy_type) })
    |> dict.map_values(fn(_key, group) { list.length(group) })

  let formal_count = list.count(examples, fn(ex) { ex.is_formal })
  let informal_count = list.length(examples) - formal_count

  FallacyStatistics(
    total_examples: list.length(examples),
    formal_fallacies: formal_count,
    informal_fallacies: informal_count,
    by_type: by_type,
  )
}

/// Fallacy statistics
pub type FallacyStatistics {
  FallacyStatistics(
    total_examples: Int,
    formal_fallacies: Int,
    informal_fallacies: Int,
    by_type: Dict(String, Int),
  )
}

/// Convert DatasetExample to FallacyExample (for HuggingFace loader compatibility)
pub fn from_dataset_example(
  example: DatasetExample,
  fallacy_type: FallacyType,
) -> FallacyExample {
  FallacyExample(
    id: example.id,
    argument_text: string.join(example.premises, " ")
      <> " Therefore, "
      <> example.conclusion,
    premises: example.premises,
    conclusion: example.conclusion,
    fallacy_type: fallacy_type,
    is_formal: is_formal_fallacy(fallacy_type),
    explanation: "Detected as " <> fallacy_type_to_string(fallacy_type),
    metadata: example.metadata,
  )
}

/// List all formal fallacy types
pub fn formal_fallacy_types() -> List(FallacyType) {
  [
    AffirmingConsequent,
    DenyingAntecedent,
    AffirmingDisjunct,
    UndistributedMiddle,
    IllicitMajor,
    IllicitMinor,
    FourTerms,
    ExistentialFallacy,
  ]
}

/// List all informal fallacy types
pub fn informal_fallacy_types() -> List(FallacyType) {
  [
    AdHominem,
    StrawMan,
    AppealToAuthority,
    AppealToEmotion,
    FalseDilemma,
    SlipperySlope,
    CircularReasoning,
    HastyGeneralization,
    RedHerring,
    Equivocation,
  ]
}
