//// Argument and formalization types

import gleam/option.{type Option}
import modal_logic/proposition.{type LogicSystem, type Proposition}

/// A formalization of an argument in a specific logic system
pub type Formalization {
  Formalization(
    id: String,
    logic_system: LogicSystem,
    premises: List(Proposition),
    conclusion: Proposition,
    assumptions: List(String),
    validation: Option(ValidationResult),
  )
}

/// Result of validating a formalization
pub type ValidationResult {
  /// The argument is valid
  Valid

  /// The argument is invalid with a countermodel
  Invalid(countermodel: String)

  /// Validation timed out
  Timeout

  /// An error occurred during validation
  Error(String)
}

/// An ambiguity in interpreting the argument
pub type Ambiguity {
  ModalAmbiguity(
    term: String,
    interpretations: List(proposition.ModalInterpretation),
  )
  ScopeAmbiguity(description: String, readings: List(String))
}

/// A complete argument with its analysis
pub type Argument {
  Argument(
    id: String,
    natural_language: String,
    formalizations: List(Formalization),
    ambiguities: List(Ambiguity),
  )
}
