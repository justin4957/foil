//// Dataset Adapter
////
//// This module transforms external dataset examples into TestFixture format
//// for use in the autonomous testing system.

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/proposition.{
  type Proposition, type LogicSystem, And, Atom, Implies, K, Knows,
  Necessary, Not, Obligatory, Or, Possible, S5, T,
}
import modal_logic/testing/external/huggingface_loader.{
  type DatasetExample, type ExampleLabel, type HuggingFaceDataset, Contradiction,
  Entailment, Neutral, Unknown,
}
import modal_logic/testing/fixtures/fixtures.{type TestFixture, TestFixture}
import modal_logic/testing/test_config.{
  type Difficulty, type ExpectedValidity, Easy,
  ExpectedEither, ExpectedInvalid, ExpectedValid, ExternalDataset, Hard, Medium,
  Unknown as UnknownValidity,
}

/// Convert a HuggingFace dataset to test fixtures
pub fn dataset_to_fixtures(dataset: HuggingFaceDataset) -> List(TestFixture) {
  dataset.examples
  |> list.map(fn(example) { example_to_fixture(example, dataset.name) })
}

/// Convert a single example to a test fixture
pub fn example_to_fixture(example: DatasetExample, source: String) -> TestFixture {
  // Combine premises into natural language
  let natural_language =
    list.append(example.premises, [
      "Therefore, " <> example.conclusion,
    ])
    |> string.join(" ")

  // Determine expected validity from label
  let expected_validity = label_to_validity(example.label)

  // Detect logic system from content
  let logic_system = detect_logic_system(natural_language)

  // Parse to propositions (simplified - would use LLM in production)
  let #(premises, conclusion) = parse_example(example)

  // Determine difficulty based on premise count and complexity
  let difficulty = estimate_difficulty(example)

  // Extract tags from metadata
  let tags = extract_tags(example)

  TestFixture(
    id: example.id,
    name: generate_fixture_name(example),
    category: ExternalDataset(source: source),
    natural_language: natural_language,
    expected_logic_system: logic_system,
    expected_premises: premises,
    expected_conclusion: conclusion,
    expected_validity: expected_validity,
    difficulty: difficulty,
    tags: tags,
    source: Some(source),
  )
}

/// Convert label to expected validity
fn label_to_validity(label: ExampleLabel) -> ExpectedValidity {
  case label {
    Entailment -> ExpectedValid
    Contradiction -> ExpectedInvalid(None)
    Neutral -> ExpectedEither("Multiple valid interpretations possible")
    Unknown -> UnknownValidity
  }
}

/// Detect logic system from natural language content
fn detect_logic_system(text: String) -> LogicSystem {
  let lower = string.lowercase(text)

  // Check for modal indicators
  let has_necessity =
    string.contains(lower, "necessarily")
    || string.contains(lower, "must be")
    || string.contains(lower, "impossible")
  let has_possibility =
    string.contains(lower, "possibly")
    || string.contains(lower, "might be")
    || string.contains(lower, "could be")
  let has_knowledge =
    string.contains(lower, "knows")
    || string.contains(lower, "knowledge")
    || string.contains(lower, "believes")
  let has_deontic =
    string.contains(lower, "ought")
    || string.contains(lower, "should")
    || string.contains(lower, "permitted")
    || string.contains(lower, "obligatory")

  case has_necessity, has_possibility, has_knowledge, has_deontic {
    True, True, _, _ -> S5
    // Strong modal
    True, False, _, _ -> T
    // Necessity-focused
    _, _, True, _ -> S5
    // Epistemic typically uses S5
    _, _, _, True -> proposition.KD
    // Deontic uses KD
    _, _, _, _ -> K
    // Default to minimal modal logic
  }
}

/// Parse example to propositions (simplified heuristic parsing)
fn parse_example(example: DatasetExample) -> #(List(Proposition), Proposition) {
  let premises =
    example.premises
    |> list.index_map(fn(premise, idx) { parse_sentence(premise, idx) })

  let conclusion = parse_sentence(example.conclusion, list.length(example.premises))

  #(premises, conclusion)
}

/// Parse a single sentence to a proposition (heuristic approach)
fn parse_sentence(sentence: String, index: Int) -> Proposition {
  let lower = string.lowercase(sentence)
  let base_atom = "p" <> int.to_string(index)

  // Detect logical structure
  case detect_sentence_structure(lower) {
    Universal(subject, predicate) ->
      Implies(Atom(subject), Atom(predicate))
    Existential(subject, predicate) ->
      And(Atom(subject), Atom(predicate))
    Conditional(antecedent, consequent) ->
      Implies(Atom(antecedent), Atom(consequent))
    Negation(inner) ->
      Not(Atom(inner))
    Necessity(inner) ->
      Necessary(Atom(inner))
    Possibility(inner) ->
      Possible(Atom(inner))
    Deontic(inner) ->
      Obligatory(Atom(inner))
    Epistemic(agent, inner) ->
      Knows(agent, Atom(inner))
    Disjunction(left, right) ->
      Or(Atom(left), Atom(right))
    Simple ->
      Atom(base_atom)
  }
}

/// Detected sentence structure
type SentenceStructure {
  Universal(subject: String, predicate: String)
  Existential(subject: String, predicate: String)
  Conditional(antecedent: String, consequent: String)
  Negation(inner: String)
  Necessity(inner: String)
  Possibility(inner: String)
  Deontic(inner: String)
  Epistemic(agent: String, inner: String)
  Disjunction(left: String, right: String)
  Simple
}

/// Detect sentence structure from text
fn detect_sentence_structure(text: String) -> SentenceStructure {
  // Check for universal quantification
  case string.starts_with(text, "all ") {
    True -> {
      // Try to extract "All X are Y"
      let parts = string.split(text, " are ")
      case parts {
        [subject, predicate] -> {
          let subject_clean =
            subject
            |> string.replace("all ", "")
            |> string.trim
          Universal(subject: subject_clean, predicate: string.trim(predicate))
        }
        _ -> Simple
      }
    }
    False ->
      // Check for existential
      case string.starts_with(text, "some ") {
        True -> {
          let parts = string.split(text, " are ")
          case parts {
            [subject, predicate] -> {
              let subject_clean =
                subject
                |> string.replace("some ", "")
                |> string.trim
              Existential(subject: subject_clean, predicate: string.trim(predicate))
            }
            _ -> Simple
          }
        }
        False ->
          // Check for conditional
          case string.starts_with(text, "if ") {
            True -> {
              let parts = string.split(text, " then ")
              case parts {
                [ant, cons] -> {
                  let ant_clean =
                    ant
                    |> string.replace("if ", "")
                    |> string.trim
                  Conditional(antecedent: ant_clean, consequent: string.trim(cons))
                }
                _ -> Simple
              }
            }
            False ->
              // Check for negation
              case
                string.contains(text, " not ")
                || string.starts_with(text, "no ")
              {
                True -> {
                  let inner =
                    text
                    |> string.replace(" not ", " ")
                    |> string.replace("no ", "")
                    |> string.trim
                  Negation(inner: inner)
                }
                False ->
                  // Check for necessity
                  case string.contains(text, "necessarily") {
                    True -> {
                      let inner =
                        text
                        |> string.replace("necessarily", "")
                        |> string.replace(",", "")
                        |> string.trim
                      Necessity(inner: inner)
                    }
                    False ->
                      // Check for possibility
                      case string.contains(text, "possibly") {
                        True -> {
                          let inner =
                            text
                            |> string.replace("possibly", "")
                            |> string.replace(",", "")
                            |> string.trim
                          Possibility(inner: inner)
                        }
                        False ->
                          // Check for deontic
                          case
                            string.contains(text, "ought")
                            || string.contains(text, "should")
                          {
                            True -> {
                              let inner =
                                text
                                |> string.replace("ought to", "")
                                |> string.replace("should", "")
                                |> string.trim
                              Deontic(inner: inner)
                            }
                            False ->
                              // Check for epistemic
                              case string.contains(text, " knows ") {
                                True -> {
                                  let parts = string.split(text, " knows ")
                                  case parts {
                                    [agent, content] ->
                                      Epistemic(
                                        agent: string.trim(agent),
                                        inner: string.trim(content),
                                      )
                                    _ -> Simple
                                  }
                                }
                                False ->
                                  // Check for disjunction
                                  case string.contains(text, " or ") {
                                    True -> {
                                      let parts = string.split(text, " or ")
                                      case parts {
                                        [left, right] ->
                                          Disjunction(
                                            left: string.trim(left),
                                            right: string.trim(right),
                                          )
                                        _ -> Simple
                                      }
                                    }
                                    False -> Simple
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

/// Estimate difficulty based on example complexity
fn estimate_difficulty(example: DatasetExample) -> Difficulty {
  let premise_count = list.length(example.premises)
  let total_length =
    example.premises
    |> list.map(string.length)
    |> list.fold(0, fn(acc, len) { acc + len })

  case premise_count, total_length {
    n, _ if n <= 2 -> Easy
    n, l if n <= 3 && l < 200 -> Medium
    _, _ -> Hard
  }
}

/// Extract tags from example metadata
fn extract_tags(example: DatasetExample) -> List(String) {
  let base_tags = case example.label {
    Entailment -> ["valid"]
    Contradiction -> ["invalid", "contradiction"]
    Neutral -> ["neutral", "ambiguous"]
    Unknown -> ["unknown"]
  }

  // Would extract additional tags from metadata in full implementation
  base_tags
}

/// Generate a descriptive fixture name
fn generate_fixture_name(example: DatasetExample) -> String {
  let label_str = case example.label {
    Entailment -> "Valid"
    Contradiction -> "Invalid"
    Neutral -> "Neutral"
    Unknown -> "Unknown"
  }

  // Take first few words of conclusion
  let conclusion_preview =
    example.conclusion
    |> string.split(" ")
    |> list.take(4)
    |> string.join(" ")

  label_str <> ": " <> conclusion_preview <> "..."
}

/// Filter fixtures by expected validity
pub fn filter_by_validity(
  fixtures: List(TestFixture),
  validity: ExpectedValidity,
) -> List(TestFixture) {
  list.filter(fixtures, fn(f) {
    case f.expected_validity, validity {
      ExpectedValid, ExpectedValid -> True
      ExpectedInvalid(_), ExpectedInvalid(_) -> True
      ExpectedEither(_), ExpectedEither(_) -> True
      UnknownValidity, UnknownValidity -> True
      _, _ -> False
    }
  })
}

/// Filter fixtures by logic system
pub fn filter_by_logic_system(
  fixtures: List(TestFixture),
  system: LogicSystem,
) -> List(TestFixture) {
  list.filter(fixtures, fn(f) { f.expected_logic_system == system })
}

/// Get fixture count by source
pub fn count_by_source(fixtures: List(TestFixture)) -> List(#(String, Int)) {
  fixtures
  |> list.group(fn(f) {
    case f.source {
      Some(s) -> s
      None -> "embedded"
    }
  })
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(source, group) = pair
    #(source, list.length(group))
  })
}
