//// Explanation Generator
////
//// This module generates human-readable explanations of modal logic validation results.
//// It provides explanations at different levels of detail for different audiences:
//// - Brief: One-sentence summary
//// - Standard: Paragraph explanation
//// - Detailed: Full analysis with examples
//// - Technical: Formal logic details
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/explanation
////
//// let exp = explanation.explain_validation(formalization, result, StandardLevel)
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/argument.{type Formalization, type ValidationResult}
import modal_logic/countermodel
import modal_logic/proposition.{type LogicSystem, type Proposition}
import modal_logic/validator.{type Countermodel, type ValidationResponse}

// =============================================================================
// Types
// =============================================================================

/// Level of explanation detail
pub type ExplanationLevel {
  /// One-sentence summary
  BriefLevel
  /// Paragraph explanation for general audience
  StandardLevel
  /// Full analysis with examples
  DetailedLevel
  /// Formal logic details for experts
  TechnicalLevel
}

/// Target audience for explanation
pub type Audience {
  /// General public with no logic background
  GeneralAudience
  /// Philosophy students
  PhilosophyStudent
  /// Logic researchers
  LogicExpert
  /// Computer scientists
  ComputerScientist
}

/// A complete explanation
pub type Explanation {
  Explanation(
    /// Main explanation text
    main_text: String,
    /// Summary line
    summary: String,
    /// Key points
    key_points: List(String),
    /// Technical details (if requested)
    technical_details: Option(String),
    /// Suggested follow-up questions
    follow_up_questions: List(String),
    /// Related concepts
    related_concepts: List(String),
  )
}

/// Explanation configuration
pub type ExplanationConfig {
  ExplanationConfig(
    /// Detail level
    level: ExplanationLevel,
    /// Target audience
    audience: Audience,
    /// Include countermodel explanation
    include_countermodel: Bool,
    /// Include repair suggestions
    include_repairs: Bool,
    /// Include logic system explanation
    include_logic_explanation: Bool,
    /// Maximum length (0 = unlimited)
    max_length: Int,
  )
}

/// Context for generating explanations
pub type ExplanationContext {
  ExplanationContext(
    /// The formalization being explained
    formalization: Formalization,
    /// Validation result
    validation_result: ValidationResult,
    /// Optional countermodel
    countermodel: Option(Countermodel),
    /// Any ambiguities detected
    ambiguities: List(argument.Ambiguity),
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default explanation configuration
pub fn default_config() -> ExplanationConfig {
  ExplanationConfig(
    level: StandardLevel,
    audience: GeneralAudience,
    include_countermodel: True,
    include_repairs: True,
    include_logic_explanation: True,
    max_length: 0,
  )
}

/// Create brief explanation configuration
pub fn brief_config() -> ExplanationConfig {
  ExplanationConfig(
    level: BriefLevel,
    audience: GeneralAudience,
    include_countermodel: False,
    include_repairs: False,
    include_logic_explanation: False,
    max_length: 280,
  )
}

/// Create detailed explanation configuration
pub fn detailed_config() -> ExplanationConfig {
  ExplanationConfig(
    level: DetailedLevel,
    audience: PhilosophyStudent,
    include_countermodel: True,
    include_repairs: True,
    include_logic_explanation: True,
    max_length: 0,
  )
}

/// Create technical explanation configuration
pub fn technical_config() -> ExplanationConfig {
  ExplanationConfig(
    level: TechnicalLevel,
    audience: LogicExpert,
    include_countermodel: True,
    include_repairs: True,
    include_logic_explanation: True,
    max_length: 0,
  )
}

/// Set explanation level
pub fn with_level(
  config: ExplanationConfig,
  level: ExplanationLevel,
) -> ExplanationConfig {
  ExplanationConfig(..config, level: level)
}

/// Set target audience
pub fn with_audience(
  config: ExplanationConfig,
  audience: Audience,
) -> ExplanationConfig {
  ExplanationConfig(..config, audience: audience)
}

// =============================================================================
// Main Explanation Functions
// =============================================================================

/// Generate explanation for a validation result
pub fn explain_validation(
  formalization: Formalization,
  result: ValidationResult,
  level: ExplanationLevel,
) -> Explanation {
  let config = ExplanationConfig(..default_config(), level: level)
  let context =
    ExplanationContext(
      formalization: formalization,
      validation_result: result,
      countermodel: None,
      ambiguities: [],
    )

  explain_with_config(context, config)
}

/// Generate explanation with full configuration
pub fn explain_with_config(
  context: ExplanationContext,
  config: ExplanationConfig,
) -> Explanation {
  case config.level {
    BriefLevel -> generate_brief_explanation(context, config)
    StandardLevel -> generate_standard_explanation(context, config)
    DetailedLevel -> generate_detailed_explanation(context, config)
    TechnicalLevel -> generate_technical_explanation(context, config)
  }
}

/// Explain a validation response (from validator)
pub fn explain_response(
  response: ValidationResponse,
  formalization: Formalization,
  config: ExplanationConfig,
) -> Explanation {
  let countermodel = case response.result {
    argument.Invalid(reason) ->
      validator.parse_countermodel(reason, formalization.logic_system)
    _ -> None
  }

  let context =
    ExplanationContext(
      formalization: formalization,
      validation_result: response.result,
      countermodel: countermodel,
      ambiguities: [],
    )

  explain_with_config(context, config)
}

// =============================================================================
// Brief Explanation
// =============================================================================

fn generate_brief_explanation(
  context: ExplanationContext,
  _config: ExplanationConfig,
) -> Explanation {
  let summary = case context.validation_result {
    argument.Valid ->
      "The argument is valid: the conclusion necessarily follows from the premises."
    argument.Invalid(_) ->
      "The argument is invalid: there's a scenario where the premises are true but the conclusion is false."
    argument.Unknown(reason) -> "The argument's validity is unknown: " <> reason
    argument.Timeout ->
      "The argument's validity could not be determined within the given constraints."
    argument.Error(msg) -> "An error occurred during validation: " <> msg
  }

  Explanation(
    main_text: summary,
    summary: summary,
    key_points: [],
    technical_details: None,
    follow_up_questions: [],
    related_concepts: [],
  )
}

// =============================================================================
// Standard Explanation
// =============================================================================

fn generate_standard_explanation(
  context: ExplanationContext,
  config: ExplanationConfig,
) -> Explanation {
  let #(main_text, key_points) = case context.validation_result {
    argument.Valid -> {
      let text =
        "This argument is logically valid. Given the premises you've provided, "
        <> "the conclusion must be true. In other words, there's no possible "
        <> "situation where all the premises are true but the conclusion is false.\n\n"
        <> explain_logic_system_simple(context.formalization.logic_system)

      let points = [
        "The argument form is correct",
        "The conclusion follows necessarily from the premises",
        "No counterexample exists",
      ]

      #(text, points)
    }

    argument.Invalid(_reason) -> {
      let text =
        "This argument is logically invalid. While the premises might seem to support "
        <> "the conclusion, they don't actually guarantee it. There exists at least one "
        <> "scenario where all the premises are true, but the conclusion is false.\n\n"
        <> case config.include_countermodel {
          True -> explain_countermodel_simple(context.countermodel)
          False -> ""
        }

      let points = [
        "The premises don't guarantee the conclusion",
        "A counterexample exists",
        "Consider strengthening the premises or weakening the conclusion",
      ]

      #(text, points)
    }

    argument.Timeout -> {
      let text =
        "The validity of this argument could not be determined due to timeout. This might be because "
        <> "the argument is too complex, or because the logic system being used doesn't "
        <> "provide enough information to decide in the allowed time."

      let points = [
        "Try a different logic system",
        "Simplify the argument",
        "Check for ambiguous modal expressions",
      ]

      #(text, points)
    }

    argument.Error(msg) -> {
      let text =
        "An error occurred while analyzing this argument: "
        <> msg
        <> ". This might be due to a syntax error or an unsupported construction."

      let points = ["Check the argument syntax", "Verify all terms are defined"]

      #(text, points)
    }

    argument.Unknown(reason) -> {
      let text =
        "The validity of this argument could not be determined: "
        <> reason
        <> ". This typically occurs when the Z3 solver is unavailable or returned an inconclusive result."

      let points = [
        "Ensure Z3 solver is installed (pip3 install z3-solver)",
        "Try simplifying the argument",
        "Check that all modal operators are correctly specified",
      ]

      #(text, points)
    }
  }

  let summary = generate_one_line_summary(context.validation_result)

  let follow_ups = generate_follow_up_questions(context, config)

  let concepts = get_related_concepts(context.formalization.logic_system)

  Explanation(
    main_text: main_text,
    summary: summary,
    key_points: key_points,
    technical_details: None,
    follow_up_questions: follow_ups,
    related_concepts: concepts,
  )
}

// =============================================================================
// Detailed Explanation
// =============================================================================

fn generate_detailed_explanation(
  context: ExplanationContext,
  config: ExplanationConfig,
) -> Explanation {
  let logic_exp =
    explain_logic_system_detailed(context.formalization.logic_system)

  let #(validity_exp, key_points) = case context.validation_result {
    argument.Valid -> {
      let text =
        "## Validity Analysis\n\n"
        <> "This argument has been verified as **logically valid**. This means that "
        <> "the logical structure of the argument guarantees that if all premises are "
        <> "true, the conclusion must also be true.\n\n"
        <> "### What This Means\n\n"
        <> "A valid argument doesn't necessarily mean the conclusion is true in the "
        <> "real world—it means the reasoning is correct. If you accept the premises, "
        <> "you must accept the conclusion.\n\n"
        <> "### The Argument Structure\n\n"
        <> explain_argument_structure(context.formalization)

      let points = [
        "Valid argument: conclusion follows from premises",
        "No countermodel exists in the given logic system",
        "The truth of premises guarantees the truth of the conclusion",
        "Logic system: "
          <> logic_system_to_string(context.formalization.logic_system),
      ]

      #(text, points)
    }

    argument.Invalid(_reason) -> {
      let text =
        "## Validity Analysis\n\n"
        <> "This argument has been determined to be **logically invalid**. A "
        <> "countermodel has been found that demonstrates a scenario where all "
        <> "premises are true but the conclusion is false.\n\n"
        <> "### Understanding the Countermodel\n\n"
        <> case context.countermodel {
          Some(cm) -> countermodel.explain_countermodel(cm)
          None -> "No detailed countermodel available."
        }
        <> "\n\n"
        <> "### The Argument Structure\n\n"
        <> explain_argument_structure(context.formalization)

      let points = [
        "Invalid argument: conclusion doesn't follow from premises",
        "A countermodel exists demonstrating the invalidity",
        "The premises can be true while the conclusion is false",
        "Consider the repair suggestions below",
      ]

      #(text, points)
    }

    argument.Timeout -> {
      let text =
        "## Validity Analysis\n\n"
        <> "The validity of this argument **could not be determined** (timeout). This typically "
        <> "happens when:\n\n"
        <> "1. The argument involves constructions beyond the current logic system\n"
        <> "2. The solver timed out before finding a proof or countermodel\n"
        <> "3. The argument contains ambiguous modal expressions\n"

      #(text, ["Validity undetermined (timeout)", "Try a stronger logic system"])
    }

    argument.Error(msg) -> {
      let text = "## Error During Analysis\n\n" <> "An error occurred: " <> msg

      #(text, ["Error occurred: " <> msg])
    }

    argument.Unknown(reason) -> {
      let text =
        "## Validity Analysis\n\n"
        <> "The validity of this argument **could not be determined**: "
        <> reason
        <> "\n\n"
        <> "This typically occurs when:\n\n"
        <> "1. The Z3 solver is not installed or unavailable\n"
        <> "2. The solver returned an inconclusive result\n"
        <> "3. The problem is too complex for the solver\n\n"
        <> "### Installation\n\n"
        <> "To install Z3, run: `pip3 install z3-solver`"

      #(text, [
        "Validity unknown: " <> reason,
        "Install Z3: pip3 install z3-solver",
      ])
    }
  }

  let main_text =
    "# Explanation of Modal Logic Argument\n\n"
    <> logic_exp
    <> "\n\n"
    <> validity_exp

  let technical = case config.level {
    TechnicalLevel | DetailedLevel ->
      Some(generate_technical_details(context.formalization))
    _ -> None
  }

  Explanation(
    main_text: main_text,
    summary: generate_one_line_summary(context.validation_result),
    key_points: key_points,
    technical_details: technical,
    follow_up_questions: generate_follow_up_questions(context, config),
    related_concepts: get_related_concepts(context.formalization.logic_system),
  )
}

// =============================================================================
// Technical Explanation
// =============================================================================

fn generate_technical_explanation(
  context: ExplanationContext,
  config: ExplanationConfig,
) -> Explanation {
  let detailed = generate_detailed_explanation(context, config)

  let technical_text =
    generate_technical_details(context.formalization)
    <> "\n\n"
    <> generate_formal_semantics(context.formalization.logic_system)

  Explanation(..detailed, technical_details: Some(technical_text))
}

// =============================================================================
// Component Generators
// =============================================================================

fn explain_logic_system_simple(system: LogicSystem) -> String {
  case system {
    proposition.K ->
      "The argument was analyzed using basic modal logic (K), which makes no special assumptions about accessibility between possible worlds."
    proposition.T ->
      "The argument was analyzed using T logic, which assumes that what is necessarily true is actually true (reflexive accessibility)."
    proposition.S4 ->
      "The argument was analyzed using S4 logic, which assumes both reflexivity and transitivity of necessity."
    proposition.S5 ->
      "The argument was analyzed using S5 logic, where necessity and possibility are symmetric—if something is possibly necessary, it's necessary."
    proposition.KD ->
      "The argument was analyzed using deontic logic (KD), suitable for reasoning about obligations and permissions."
    proposition.K4 ->
      "The argument was analyzed using K4 logic, which assumes transitivity (if □p then □□p)."
    proposition.KD45 ->
      "The argument was analyzed using KD45 logic (deontic logic with positive and negative introspection)."
  }
}

fn explain_logic_system_detailed(system: LogicSystem) -> String {
  let name = logic_system_to_string(system)
  let base = "## Logic System: " <> name <> "\n\n"

  case system {
    proposition.K ->
      base
      <> "**K** is the most basic normal modal logic. It validates the distribution axiom:\n"
      <> "- K: □(p → q) → (□p → □q)\n\n"
      <> "This means necessity distributes over implication. No special properties "
      <> "are assumed about the accessibility relation between worlds."

    proposition.T ->
      base
      <> "**T** extends K with the reflexivity axiom:\n"
      <> "- T: □p → p (what is necessary is actual)\n\n"
      <> "This requires every world to be accessible from itself, meaning if something "
      <> "is necessary, it must be true in the actual world."

    proposition.S4 ->
      base
      <> "**S4** adds transitivity to T:\n"
      <> "- T: □p → p (reflexivity)\n"
      <> "- 4: □p → □□p (transitivity)\n\n"
      <> "If something is necessary, it's necessarily necessary."

    proposition.S5 ->
      base
      <> "**S5** is the strongest common modal logic:\n"
      <> "- T: □p → p (reflexivity)\n"
      <> "- 5: ◇p → □◇p (euclidean)\n\n"
      <> "All worlds are mutually accessible. What's possible is necessarily possible."

    proposition.KD ->
      base
      <> "**KD** is standard deontic logic:\n"
      <> "- D: □p → ◇p (seriality)\n\n"
      <> "Every world has at least one accessible world. Suitable for reasoning about "
      <> "obligations: if p is obligatory, then p is permitted."

    proposition.K4 ->
      base
      <> "**K4** adds transitivity to K:\n"
      <> "- 4: □p → □□p\n\n"
      <> "Accessibility is transitive but not necessarily reflexive."

    proposition.KD45 ->
      base
      <> "**KD45** combines seriality with transitivity and euclidean properties:\n"
      <> "- D: □p → ◇p\n"
      <> "- 4: □p → □□p\n"
      <> "- 5: ◇p → □◇p\n\n"
      <> "Often used for deontic logic with introspection properties."
  }
}

fn explain_countermodel_simple(countermodel: Option(Countermodel)) -> String {
  case countermodel {
    None ->
      "A counterexample exists, but detailed information is not available."
    Some(cm) -> {
      let world_count = list.length(cm.worlds)
      "The countermodel involves "
      <> int_to_string(world_count)
      <> " possible world(s). "
      <> "In the actual world ("
      <> cm.actual_world
      <> "), all premises hold but the conclusion fails."
    }
  }
}

fn explain_argument_structure(formalization: Formalization) -> String {
  let premise_count = list.length(formalization.premises)
  let premises_text =
    formalization.premises
    |> list.index_map(fn(p, i) {
      "P" <> int_to_string(i + 1) <> ": " <> proposition_to_english(p)
    })
    |> string.join("\n")

  let conclusion_text =
    "C: " <> proposition_to_english(formalization.conclusion)

  "**Premises** ("
  <> int_to_string(premise_count)
  <> "):\n"
  <> premises_text
  <> "\n\n"
  <> "**Conclusion**:\n"
  <> conclusion_text
}

fn generate_technical_details(formalization: Formalization) -> String {
  let premises_formal =
    formalization.premises
    |> list.index_map(fn(p, i) {
      "P" <> int_to_string(i + 1) <> ": " <> proposition_to_symbol(p)
    })
    |> string.join("\n")

  "### Formal Representation\n\n"
  <> "```\n"
  <> premises_formal
  <> "\n"
  <> "∴ "
  <> proposition_to_symbol(formalization.conclusion)
  <> "\n```\n\n"
  <> "Logic: "
  <> logic_system_to_string(formalization.logic_system)
}

fn generate_formal_semantics(system: LogicSystem) -> String {
  "### Kripke Semantics\n\n"
  <> "A Kripke model M = (W, R, V) consists of:\n"
  <> "- W: Set of possible worlds\n"
  <> "- R: Accessibility relation on W\n"
  <> "- V: Valuation function assigning truth values\n\n"
  <> "**Satisfaction conditions:**\n"
  <> "- M, w ⊨ □φ iff for all v with wRv: M, v ⊨ φ\n"
  <> "- M, w ⊨ ◇φ iff there exists v with wRv: M, v ⊨ φ\n\n"
  <> "**Frame conditions for "
  <> logic_system_to_string(system)
  <> ":**\n"
  <> get_frame_conditions(system)
}

fn get_frame_conditions(system: LogicSystem) -> String {
  case system {
    proposition.K -> "No special conditions (arbitrary frames)"
    proposition.T -> "Reflexive: ∀w: wRw"
    proposition.K4 -> "Transitive: ∀u,v,w: uRv ∧ vRw → uRw"
    proposition.S4 -> "Reflexive and transitive"
    proposition.S5 -> "Equivalence relation (reflexive, symmetric, transitive)"
    proposition.KD -> "Serial: ∀w ∃v: wRv"
    proposition.KD45 -> "Serial, transitive, euclidean"
  }
}

fn generate_one_line_summary(result: ValidationResult) -> String {
  case result {
    argument.Valid -> "Valid: conclusion follows from premises"
    argument.Invalid(_) -> "Invalid: countermodel exists"
    argument.Unknown(reason) -> "Unknown: " <> reason
    argument.Timeout -> "Validity undetermined (timeout)"
    argument.Error(msg) -> "Error: " <> msg
  }
}

fn generate_follow_up_questions(
  context: ExplanationContext,
  _config: ExplanationConfig,
) -> List(String) {
  case context.validation_result {
    argument.Valid -> [
      "Are the premises actually true?",
      "Could the argument be strengthened further?",
      "Does this validity hold in weaker logic systems?",
    ]
    argument.Invalid(_) -> [
      "What additional premises would make the argument valid?",
      "Is the conclusion too strong?",
      "Would a different logic system help?",
    ]
    argument.Unknown(_) -> [
      "Is Z3 solver installed and available?",
      "Should we try with a simpler formula?",
      "Would reducing the number of worlds help?",
    ]
    argument.Timeout -> [
      "Should we try a stronger logic system?",
      "Can the argument be simplified?",
      "Are there ambiguous expressions that need clarification?",
    ]
    argument.Error(_) -> [
      "Is the syntax correct?",
      "Are all terms properly defined?",
    ]
  }
}

fn get_related_concepts(system: LogicSystem) -> List(String) {
  let base = ["Possible worlds", "Accessibility relation", "Kripke semantics"]

  let specific = case system {
    proposition.K -> ["Normal modal logic", "Distribution axiom"]
    proposition.T -> ["Reflexivity", "Factivity", "T axiom"]
    proposition.S4 -> ["Transitivity", "Positive introspection"]
    proposition.S5 -> ["Universal accessibility", "Metaphysical necessity"]
    proposition.KD -> ["Deontic logic", "Seriality", "Obligation/Permission"]
    proposition.K4 -> ["Transitivity", "Provability logic"]
    proposition.KD45 -> ["Standard deontic logic", "Introspection axioms"]
  }

  list.append(base, specific)
}

// =============================================================================
// Proposition Formatting
// =============================================================================

fn proposition_to_english(prop: Proposition) -> String {
  case prop {
    proposition.Atom(name) -> name
    proposition.Not(inner) -> "not (" <> proposition_to_english(inner) <> ")"
    proposition.And(left, right) ->
      "("
      <> proposition_to_english(left)
      <> " and "
      <> proposition_to_english(right)
      <> ")"
    proposition.Or(left, right) ->
      "("
      <> proposition_to_english(left)
      <> " or "
      <> proposition_to_english(right)
      <> ")"
    proposition.Implies(ante, cons) ->
      "if "
      <> proposition_to_english(ante)
      <> " then "
      <> proposition_to_english(cons)
    proposition.Necessary(inner) ->
      "necessarily (" <> proposition_to_english(inner) <> ")"
    proposition.Possible(inner) ->
      "possibly (" <> proposition_to_english(inner) <> ")"
    proposition.Obligatory(inner) ->
      "it is obligatory that (" <> proposition_to_english(inner) <> ")"
    proposition.Permitted(inner) ->
      "it is permitted that (" <> proposition_to_english(inner) <> ")"
    proposition.Knows(agent, inner) ->
      agent <> " knows that (" <> proposition_to_english(inner) <> ")"
    proposition.Believes(agent, inner) ->
      agent <> " believes that (" <> proposition_to_english(inner) <> ")"
  }
}

fn proposition_to_symbol(prop: Proposition) -> String {
  case prop {
    proposition.Atom(name) -> name
    proposition.Not(inner) -> "¬" <> proposition_to_symbol(inner)
    proposition.And(left, right) ->
      "("
      <> proposition_to_symbol(left)
      <> " ∧ "
      <> proposition_to_symbol(right)
      <> ")"
    proposition.Or(left, right) ->
      "("
      <> proposition_to_symbol(left)
      <> " ∨ "
      <> proposition_to_symbol(right)
      <> ")"
    proposition.Implies(ante, cons) ->
      "("
      <> proposition_to_symbol(ante)
      <> " → "
      <> proposition_to_symbol(cons)
      <> ")"
    proposition.Necessary(inner) -> "□" <> proposition_to_symbol(inner)
    proposition.Possible(inner) -> "◇" <> proposition_to_symbol(inner)
    proposition.Obligatory(inner) -> "O" <> proposition_to_symbol(inner)
    proposition.Permitted(inner) -> "P" <> proposition_to_symbol(inner)
    proposition.Knows(agent, inner) ->
      "K_" <> agent <> proposition_to_symbol(inner)
    proposition.Believes(agent, inner) ->
      "B_" <> agent <> proposition_to_symbol(inner)
  }
}

fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    proposition.K -> "K"
    proposition.T -> "T"
    proposition.K4 -> "K4"
    proposition.S4 -> "S4"
    proposition.S5 -> "S5"
    proposition.KD -> "KD"
    proposition.KD45 -> "KD45"
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

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
