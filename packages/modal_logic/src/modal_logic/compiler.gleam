//// Structure Compiler for LLM Output
////
//// This module compiles JSON output from the LLM translation into
//// internal Proposition types. It validates the structure and builds
//// the Proposition AST.
////
//// ## Compilation Pipeline
////
//// 1. Parse JSON string into typed structures
//// 2. Validate structure against expected schema
//// 3. Build Proposition AST from validated structure
//// 4. Generate Formalization with metadata
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/compiler
////
//// let result = compiler.compile_translation(json_string)
//// case result {
////   Ok(translation) -> use_translation(translation)
////   Error(errors) -> handle_errors(errors)
//// }
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/argument.{type Ambiguity, type Formalization}
import modal_logic/proposition.{type LogicSystem, type Proposition}

// =============================================================================
// Types
// =============================================================================

/// A compiled translation result
pub type CompiledTranslation {
  CompiledTranslation(
    /// Extracted premises as propositions
    premises: List(PremiseInfo),
    /// Extracted conclusion
    conclusion: ConclusionInfo,
    /// Recommended logic system
    logic_system: LogicSystem,
    /// Confidence score from LLM
    confidence: Float,
    /// Detected ambiguities
    ambiguities: List(Ambiguity),
    /// Assumptions made during formalization
    assumptions: List(String),
    /// Additional notes from the LLM
    notes: Option(String),
  )
}

/// Information about a compiled premise
pub type PremiseInfo {
  PremiseInfo(
    /// Natural language text
    natural_text: String,
    /// Formal proposition
    formal: Proposition,
    /// Source sentence in original argument
    source_sentence: Option(String),
  )
}

/// Information about a compiled conclusion
pub type ConclusionInfo {
  ConclusionInfo(
    /// Natural language text
    natural_text: String,
    /// Formal proposition
    formal: Proposition,
    /// Source sentence in original argument
    source_sentence: Option(String),
  )
}

/// Compilation error types
pub type CompileError {
  /// JSON parsing failed
  JsonParseError(message: String)
  /// Required field missing
  MissingField(field_name: String)
  /// Field has wrong type
  TypeError(field_name: String, expected: String, got: String)
  /// Invalid proposition structure
  InvalidProposition(description: String)
  /// Unknown logic system
  UnknownLogicSystem(name: String)
  /// Invalid ambiguity type
  InvalidAmbiguityType(value: String)
  /// Validation error
  ValidationError(message: String)
  /// Multiple errors occurred
  MultipleErrors(errors: List(CompileError))
}

/// Result type for compilation operations
pub type CompileResult(a) =
  Result(a, CompileError)

// =============================================================================
// Internal JSON Types (for decoding)
// =============================================================================

/// JSON representation of a premise
type JsonPremise {
  JsonPremise(
    natural_text: String,
    formal: JsonProposition,
    source_sentence: Option(String),
  )
}

/// JSON representation of conclusion
type JsonConclusion {
  JsonConclusion(
    natural_text: String,
    formal: JsonProposition,
    source_sentence: Option(String),
  )
}

/// JSON representation of a proposition
type JsonProposition {
  JsonAtom(name: String)
  JsonNot(operand: JsonProposition)
  JsonAnd(left: JsonProposition, right: JsonProposition)
  JsonOr(left: JsonProposition, right: JsonProposition)
  JsonImplies(left: JsonProposition, right: JsonProposition)
  JsonNecessary(operand: JsonProposition)
  JsonPossible(operand: JsonProposition)
  JsonObligatory(operand: JsonProposition)
  JsonPermitted(operand: JsonProposition)
  JsonKnows(agent: String, operand: JsonProposition)
  JsonBelieves(agent: String, operand: JsonProposition)
}

/// JSON representation of ambiguity
type JsonAmbiguity {
  JsonAmbiguity(
    amb_type: String,
    term: Option(String),
    description: String,
    alternatives: List(String),
  )
}

/// JSON representation of full translation
type JsonTranslation {
  JsonTranslation(
    premises: List(JsonPremise),
    conclusion: JsonConclusion,
    logic_system: String,
    confidence: Float,
    ambiguities: List(JsonAmbiguity),
    assumptions: List(String),
    notes: Option(String),
  )
}

// =============================================================================
// Main Compilation Functions
// =============================================================================

/// Compile a JSON translation response into a CompiledTranslation
pub fn compile_translation(
  json_string: String,
) -> CompileResult(CompiledTranslation) {
  case json.parse(json_string, translation_decoder()) {
    Error(_) -> Error(JsonParseError("Failed to parse JSON"))
    Ok(json_translation) -> compile_json_translation(json_translation)
  }
}

/// Compile from a dynamic value (for direct use)
pub fn compile_proposition(value: Dynamic) -> CompileResult(Proposition) {
  case decode.run(value, proposition_decoder()) {
    Error(_) -> Error(InvalidProposition("Failed to decode proposition"))
    Ok(json_prop) -> Ok(json_proposition_to_proposition(json_prop))
  }
}

/// Compile JSON translation to final result
fn compile_json_translation(
  jt: JsonTranslation,
) -> CompileResult(CompiledTranslation) {
  // Parse logic system
  use logic_system <- result.try(parse_logic_system(jt.logic_system))

  // Convert premises
  let premises =
    list.map(jt.premises, fn(jp) {
      PremiseInfo(
        natural_text: jp.natural_text,
        formal: json_proposition_to_proposition(jp.formal),
        source_sentence: jp.source_sentence,
      )
    })

  // Convert conclusion
  let conclusion =
    ConclusionInfo(
      natural_text: jt.conclusion.natural_text,
      formal: json_proposition_to_proposition(jt.conclusion.formal),
      source_sentence: jt.conclusion.source_sentence,
    )

  // Convert ambiguities
  let ambiguities =
    list.filter_map(jt.ambiguities, fn(ja) {
      case json_ambiguity_to_ambiguity(ja) {
        Ok(a) -> Ok(a)
        Error(_) -> Error(Nil)
      }
    })

  Ok(CompiledTranslation(
    premises: premises,
    conclusion: conclusion,
    logic_system: logic_system,
    confidence: jt.confidence,
    ambiguities: ambiguities,
    assumptions: jt.assumptions,
    notes: jt.notes,
  ))
}

// =============================================================================
// Decoders
// =============================================================================

/// Decoder for full translation
fn translation_decoder() -> decode.Decoder(JsonTranslation) {
  use premises <- decode.field("premises", decode.list(premise_decoder()))
  use conclusion <- decode.field("conclusion", conclusion_decoder())
  use logic_system <- decode.field("logic_system", decode.string)
  use confidence <- decode.field("confidence", float_or_int_decoder())
  use ambiguities <- decode.optional_field(
    "ambiguities",
    [],
    decode.list(ambiguity_decoder()),
  )
  use assumptions <- decode.optional_field(
    "assumptions",
    [],
    decode.list(decode.string),
  )
  use notes <- decode.optional_field("notes", "", decode.string)

  decode.success(
    JsonTranslation(
      premises: premises,
      conclusion: conclusion,
      logic_system: logic_system,
      confidence: confidence,
      ambiguities: ambiguities,
      assumptions: assumptions,
      notes: case notes {
        "" -> None
        s -> Some(s)
      },
    ),
  )
}

/// Decoder for premise
fn premise_decoder() -> decode.Decoder(JsonPremise) {
  use natural_text <- decode.field("natural_text", decode.string)
  use formal <- decode.field("formal", proposition_decoder())
  use source_sentence <- decode.optional_field(
    "source_sentence",
    "",
    decode.string,
  )

  decode.success(
    JsonPremise(
      natural_text: natural_text,
      formal: formal,
      source_sentence: case source_sentence {
        "" -> None
        s -> Some(s)
      },
    ),
  )
}

/// Decoder for conclusion
fn conclusion_decoder() -> decode.Decoder(JsonConclusion) {
  use natural_text <- decode.field("natural_text", decode.string)
  use formal <- decode.field("formal", proposition_decoder())
  use source_sentence <- decode.optional_field(
    "source_sentence",
    "",
    decode.string,
  )

  decode.success(
    JsonConclusion(
      natural_text: natural_text,
      formal: formal,
      source_sentence: case source_sentence {
        "" -> None
        s -> Some(s)
      },
    ),
  )
}

/// Decoder for proposition (recursive)
fn proposition_decoder() -> decode.Decoder(JsonProposition) {
  use prop_type <- decode.field("type", decode.string)

  case prop_type {
    "atom" -> atom_decoder()
    "not" -> not_decoder()
    "and" -> and_decoder()
    "or" -> or_decoder()
    "implies" -> implies_decoder()
    "necessary" -> necessary_decoder()
    "possible" -> possible_decoder()
    "obligatory" -> obligatory_decoder()
    "permitted" -> permitted_decoder()
    "knows" -> knows_decoder()
    "believes" -> believes_decoder()
    _ -> decode.failure(JsonAtom("error"), "valid proposition type")
  }
}

fn atom_decoder() -> decode.Decoder(JsonProposition) {
  use name <- decode.field("name", decode.string)
  decode.success(JsonAtom(name))
}

fn not_decoder() -> decode.Decoder(JsonProposition) {
  use operand <- decode.field("operand", decode.recursive(proposition_decoder))
  decode.success(JsonNot(operand))
}

fn and_decoder() -> decode.Decoder(JsonProposition) {
  use left <- decode.field("left", decode.recursive(proposition_decoder))
  use right <- decode.field("right", decode.recursive(proposition_decoder))
  decode.success(JsonAnd(left, right))
}

fn or_decoder() -> decode.Decoder(JsonProposition) {
  use left <- decode.field("left", decode.recursive(proposition_decoder))
  use right <- decode.field("right", decode.recursive(proposition_decoder))
  decode.success(JsonOr(left, right))
}

fn implies_decoder() -> decode.Decoder(JsonProposition) {
  use left <- decode.field("left", decode.recursive(proposition_decoder))
  use right <- decode.field("right", decode.recursive(proposition_decoder))
  decode.success(JsonImplies(left, right))
}

fn necessary_decoder() -> decode.Decoder(JsonProposition) {
  use operand <- decode.field("operand", decode.recursive(proposition_decoder))
  decode.success(JsonNecessary(operand))
}

fn possible_decoder() -> decode.Decoder(JsonProposition) {
  use operand <- decode.field("operand", decode.recursive(proposition_decoder))
  decode.success(JsonPossible(operand))
}

fn obligatory_decoder() -> decode.Decoder(JsonProposition) {
  use operand <- decode.field("operand", decode.recursive(proposition_decoder))
  decode.success(JsonObligatory(operand))
}

fn permitted_decoder() -> decode.Decoder(JsonProposition) {
  use operand <- decode.field("operand", decode.recursive(proposition_decoder))
  decode.success(JsonPermitted(operand))
}

fn knows_decoder() -> decode.Decoder(JsonProposition) {
  use agent <- decode.field("agent", decode.string)
  use operand <- decode.field("operand", decode.recursive(proposition_decoder))
  decode.success(JsonKnows(agent, operand))
}

fn believes_decoder() -> decode.Decoder(JsonProposition) {
  use agent <- decode.field("agent", decode.string)
  use operand <- decode.field("operand", decode.recursive(proposition_decoder))
  decode.success(JsonBelieves(agent, operand))
}

/// Decoder for ambiguity
fn ambiguity_decoder() -> decode.Decoder(JsonAmbiguity) {
  use amb_type <- decode.field("type", decode.string)
  use term <- decode.optional_field("term", "", decode.string)
  use description <- decode.field("description", decode.string)
  use alternatives <- decode.optional_field(
    "alternatives",
    [],
    decode.list(decode.string),
  )

  decode.success(JsonAmbiguity(
    amb_type: amb_type,
    term: case term {
      "" -> None
      s -> Some(s)
    },
    description: description,
    alternatives: alternatives,
  ))
}

/// Decoder that accepts both float and int as float
fn float_or_int_decoder() -> decode.Decoder(Float) {
  decode.one_of(decode.float, [
    decode.then(decode.int, fn(i) { decode.success(int_to_float(i)) }),
  ])
}

// =============================================================================
// Conversion Functions
// =============================================================================

/// Convert JSON proposition to internal Proposition type
fn json_proposition_to_proposition(jp: JsonProposition) -> Proposition {
  case jp {
    JsonAtom(name) -> proposition.Atom(name)
    JsonNot(operand) ->
      proposition.Not(json_proposition_to_proposition(operand))
    JsonAnd(left, right) ->
      proposition.And(
        json_proposition_to_proposition(left),
        json_proposition_to_proposition(right),
      )
    JsonOr(left, right) ->
      proposition.Or(
        json_proposition_to_proposition(left),
        json_proposition_to_proposition(right),
      )
    JsonImplies(left, right) ->
      proposition.Implies(
        json_proposition_to_proposition(left),
        json_proposition_to_proposition(right),
      )
    JsonNecessary(operand) ->
      proposition.Necessary(json_proposition_to_proposition(operand))
    JsonPossible(operand) ->
      proposition.Possible(json_proposition_to_proposition(operand))
    JsonObligatory(operand) ->
      proposition.Obligatory(json_proposition_to_proposition(operand))
    JsonPermitted(operand) ->
      proposition.Permitted(json_proposition_to_proposition(operand))
    JsonKnows(agent, operand) ->
      proposition.Knows(agent, json_proposition_to_proposition(operand))
    JsonBelieves(agent, operand) ->
      proposition.Believes(agent, json_proposition_to_proposition(operand))
  }
}

/// Convert JSON ambiguity to internal Ambiguity type
fn json_ambiguity_to_ambiguity(ja: JsonAmbiguity) -> CompileResult(Ambiguity) {
  let term = option.unwrap(ja.term, "")

  case ja.amb_type {
    "modal" -> {
      let interpretations =
        list.filter_map(ja.alternatives, fn(alt) {
          case string.lowercase(alt) {
            "epistemic" -> Ok(proposition.Epistemic)
            "deontic" -> Ok(proposition.Deontic)
            "alethic" -> Ok(proposition.Alethic)
            "temporal" -> Ok(proposition.Temporal)
            _ -> Error(Nil)
          }
        })
      Ok(argument.ModalAmbiguity(term, interpretations))
    }
    "scope" -> Ok(argument.ScopeAmbiguity(ja.description, ja.alternatives))
    "lexical" -> Ok(argument.LexicalAmbiguity(term, ja.alternatives))
    "structural" ->
      Ok(argument.StructuralAmbiguity(ja.description, ja.alternatives))
    other -> Error(InvalidAmbiguityType(other))
  }
}

/// Convert CompiledTranslation to a Formalization
pub fn to_formalization(
  translation: CompiledTranslation,
  id: String,
  argument_id: String,
) -> Formalization {
  let premises = list.map(translation.premises, fn(p) { p.formal })
  let conclusion = translation.conclusion.formal

  argument.Formalization(
    id: id,
    argument_id: argument_id,
    logic_system: translation.logic_system,
    premises: premises,
    conclusion: conclusion,
    assumptions: translation.assumptions,
    validation: None,
    created_at: None,
    updated_at: None,
  )
}

/// Extract just the premises as propositions
pub fn get_premises(translation: CompiledTranslation) -> List(Proposition) {
  list.map(translation.premises, fn(p) { p.formal })
}

/// Extract just the conclusion as a proposition
pub fn get_conclusion(translation: CompiledTranslation) -> Proposition {
  translation.conclusion.formal
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Parse logic system from string
pub fn parse_logic_system(s: String) -> CompileResult(LogicSystem) {
  case string.uppercase(s) {
    "K" -> Ok(proposition.K)
    "T" -> Ok(proposition.T)
    "K4" -> Ok(proposition.K4)
    "S4" -> Ok(proposition.S4)
    "S5" -> Ok(proposition.S5)
    "KD" -> Ok(proposition.KD)
    "KD45" -> Ok(proposition.KD45)
    other -> Error(UnknownLogicSystem(other))
  }
}

// =============================================================================
// Error Formatting
// =============================================================================

/// Format a compile error as a human-readable string
pub fn format_error(error: CompileError) -> String {
  case error {
    JsonParseError(msg) -> "JSON parse error: " <> msg
    MissingField(name) -> "Missing required field: " <> name
    TypeError(name, expected, got) ->
      "Type error in field '"
      <> name
      <> "': expected "
      <> expected
      <> ", got "
      <> got
    InvalidProposition(desc) -> "Invalid proposition: " <> desc
    UnknownLogicSystem(name) -> "Unknown logic system: " <> name
    InvalidAmbiguityType(value) -> "Invalid ambiguity type: " <> value
    ValidationError(msg) -> "Validation error: " <> msg
    MultipleErrors(errors) ->
      "Multiple errors:\n" <> string.join(list.map(errors, format_error), "\n")
  }
}

/// Check if an error is recoverable (can be retried with different input)
pub fn is_recoverable(error: CompileError) -> Bool {
  case error {
    JsonParseError(_) -> True
    MissingField(_) -> True
    TypeError(_, _, _) -> True
    InvalidProposition(_) -> True
    UnknownLogicSystem(_) -> False
    InvalidAmbiguityType(_) -> True
    ValidationError(_) -> True
    MultipleErrors(errors) -> list.any(errors, is_recoverable)
  }
}

// =============================================================================
// External Functions
// =============================================================================

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
