//// Argument and formalization types
////
//// This module defines the core domain types for argument analysis:
//// - Arguments with natural language text
//// - Formalizations in various modal logic systems
//// - Validation results with countermodels
//// - Ambiguities in interpretation
//// - Repair suggestions for invalid arguments

import gleam/option.{type Option}
import modal_logic/proposition.{type LogicSystem, type Proposition}

// =============================================================================
// Core Types
// =============================================================================

/// A formalization of an argument in a specific logic system
pub type Formalization {
  Formalization(
    /// Unique identifier
    id: String,
    /// The argument this formalizes
    argument_id: String,
    /// Logic system used (K, T, S4, S5, etc.)
    logic_system: LogicSystem,
    /// List of premise propositions
    premises: List(Proposition),
    /// Conclusion proposition
    conclusion: Proposition,
    /// Additional assumptions made during formalization
    assumptions: List(String),
    /// Cached validation result if available
    validation: Option(ValidationResult),
    /// Creation timestamp (ISO 8601)
    created_at: Option(String),
    /// Last update timestamp (ISO 8601)
    updated_at: Option(String),
  )
}

/// Result of validating a formalization
pub type ValidationResult {
  /// The argument is valid in the given logic system
  Valid
  /// The argument is invalid with a countermodel
  Invalid(countermodel: String)
  /// Validation result is unknown (e.g., Z3 unavailable, timeout, or incomplete)
  Unknown(reason: String)
  /// Validation timed out before completion
  Timeout
  /// An error occurred during validation
  Error(String)
}

/// Detailed validation record for persistence
pub type ValidationRecord {
  ValidationRecord(
    /// Unique identifier
    id: String,
    /// ID of the formalization being validated
    formalization_id: String,
    /// The validation result
    result: ValidationResult,
    /// Logic system used for validation
    logic_system: LogicSystem,
    /// Number of worlds in Kripke model (if applicable)
    world_count: Option(Int),
    /// Time taken to validate in milliseconds
    duration_ms: Int,
    /// Creation timestamp (ISO 8601)
    created_at: String,
  )
}

/// An ambiguity in interpreting the argument
pub type Ambiguity {
  /// Ambiguity in modal operator interpretation
  ModalAmbiguity(
    /// The ambiguous term or phrase
    term: String,
    /// Possible modal interpretations
    interpretations: List(proposition.ModalInterpretation),
  )
  /// Ambiguity in quantifier or operator scope
  ScopeAmbiguity(
    /// Description of the scope ambiguity
    description: String,
    /// Different possible readings
    readings: List(String),
  )
  /// Lexical ambiguity in word meaning
  LexicalAmbiguity(
    /// The ambiguous word or phrase
    term: String,
    /// Possible meanings
    meanings: List(String),
  )
  /// Structural ambiguity in sentence parsing
  StructuralAmbiguity(
    /// Description of the structural issue
    description: String,
    /// Different possible parse trees (as descriptions)
    parses: List(String),
  )
}

/// A complete argument with its analysis
pub type Argument {
  Argument(
    /// Unique identifier
    id: String,
    /// Original natural language text
    natural_language: String,
    /// Source or reference for the argument
    source: Option(String),
    /// List of formalizations in different logic systems
    formalizations: List(Formalization),
    /// Identified ambiguities
    ambiguities: List(Ambiguity),
    /// User-provided tags for categorization
    tags: List(String),
    /// Creation timestamp (ISO 8601)
    created_at: Option(String),
    /// Last update timestamp (ISO 8601)
    updated_at: Option(String),
  )
}

/// A suggestion for repairing an invalid argument
pub type RepairSuggestion {
  RepairSuggestion(
    /// Unique identifier
    id: String,
    /// ID of the formalization being repaired
    formalization_id: String,
    /// Type of repair suggested
    repair_type: RepairType,
    /// Description of the repair
    description: String,
    /// The repaired formalization (if applicable)
    repaired_formalization: Option(Formalization),
    /// Confidence score (0.0 to 1.0)
    confidence: Float,
    /// Creation timestamp (ISO 8601)
    created_at: String,
  )
}

/// Types of repairs that can be suggested
pub type RepairType {
  /// Add a missing premise
  AddPremise
  /// Strengthen an existing premise
  StrengthenPremise
  /// Weaken the conclusion
  WeakenConclusion
  /// Change the logic system
  ChangeLogicSystem
  /// Modify modal operators
  ModifyModality
  /// Resolve an ambiguity
  ResolveAmbiguity
}

// =============================================================================
// Constructor Functions
// =============================================================================

/// Create a new argument with defaults
pub fn new_argument(id: String, natural_language: String) -> Argument {
  Argument(
    id: id,
    natural_language: natural_language,
    source: option.None,
    formalizations: [],
    ambiguities: [],
    tags: [],
    created_at: option.None,
    updated_at: option.None,
  )
}

/// Create a new formalization with defaults
pub fn new_formalization(
  id: String,
  argument_id: String,
  logic_system: LogicSystem,
  premises: List(Proposition),
  conclusion: Proposition,
) -> Formalization {
  Formalization(
    id: id,
    argument_id: argument_id,
    logic_system: logic_system,
    premises: premises,
    conclusion: conclusion,
    assumptions: [],
    validation: option.None,
    created_at: option.None,
    updated_at: option.None,
  )
}

/// Create a validation record
pub fn new_validation_record(
  id: String,
  formalization_id: String,
  result: ValidationResult,
  logic_system: LogicSystem,
  duration_ms: Int,
  created_at: String,
) -> ValidationRecord {
  ValidationRecord(
    id: id,
    formalization_id: formalization_id,
    result: result,
    logic_system: logic_system,
    world_count: option.None,
    duration_ms: duration_ms,
    created_at: created_at,
  )
}

/// Create a repair suggestion
pub fn new_repair_suggestion(
  id: String,
  formalization_id: String,
  repair_type: RepairType,
  description: String,
  confidence: Float,
  created_at: String,
) -> RepairSuggestion {
  RepairSuggestion(
    id: id,
    formalization_id: formalization_id,
    repair_type: repair_type,
    description: description,
    repaired_formalization: option.None,
    confidence: confidence,
    created_at: created_at,
  )
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Check if a validation result indicates validity
pub fn is_valid(result: ValidationResult) -> Bool {
  case result {
    Valid -> True
    _ -> False
  }
}

/// Check if a validation result indicates invalidity
pub fn is_invalid(result: ValidationResult) -> Bool {
  case result {
    Invalid(_) -> True
    _ -> False
  }
}

/// Get countermodel from an invalid result
pub fn get_countermodel(result: ValidationResult) -> Option(String) {
  case result {
    Invalid(cm) -> option.Some(cm)
    _ -> option.None
  }
}

/// Get error message from an error result
pub fn get_error(result: ValidationResult) -> Option(String) {
  case result {
    Error(msg) -> option.Some(msg)
    _ -> option.None
  }
}

/// Convert repair type to string
pub fn repair_type_to_string(repair_type: RepairType) -> String {
  case repair_type {
    AddPremise -> "add_premise"
    StrengthenPremise -> "strengthen_premise"
    WeakenConclusion -> "weaken_conclusion"
    ChangeLogicSystem -> "change_logic_system"
    ModifyModality -> "modify_modality"
    ResolveAmbiguity -> "resolve_ambiguity"
  }
}

/// Parse repair type from string
pub fn repair_type_from_string(s: String) -> Option(RepairType) {
  case s {
    "add_premise" -> option.Some(AddPremise)
    "strengthen_premise" -> option.Some(StrengthenPremise)
    "weaken_conclusion" -> option.Some(WeakenConclusion)
    "change_logic_system" -> option.Some(ChangeLogicSystem)
    "modify_modality" -> option.Some(ModifyModality)
    "resolve_ambiguity" -> option.Some(ResolveAmbiguity)
    _ -> option.None
  }
}
