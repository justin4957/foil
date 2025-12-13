//// Modal Logic - Core domain types for modal logic analysis
////
//// This library provides shared types for:
//// - Modal propositions (alethic, deontic, epistemic)
//// - Arguments and formalizations
//// - Logic systems (K, T, S4, S5, KD, etc.)
//// - Persistence layer with PostgreSQL and caching
//// - LLM translation service for argument formalization
////
//// ## Quick Start
////
//// ```gleam
//// import modal_logic/proposition.{Atom, Necessary, Implies}
//// import modal_logic/argument.{new_argument, new_formalization}
////
//// let proposition = Implies(Atom("p"), Necessary(Atom("q")))
//// let arg = new_argument("arg-1", "All men are mortal")
//// ```
////
//// ## LLM Translation
////
//// ```gleam
//// import modal_logic/prompts
//// import modal_logic/compiler
//// import modal_logic/logic_detector
////
//// let prompt = prompts.build_translation_prompt(config, argument_text)
//// // Send to LLM, get JSON response
//// let result = compiler.compile_translation(json_response)
//// let detection = logic_detector.detect_from_text(argument_text)
//// ```

import modal_logic/argument
import modal_logic/compiler
import modal_logic/logic_detector
import modal_logic/prompts
import modal_logic/proposition
import modal_logic/translation_service

// Re-export proposition types
pub type Proposition =
  proposition.Proposition

pub type LogicSystem =
  proposition.LogicSystem

pub type ModalInterpretation =
  proposition.ModalInterpretation

// Re-export argument types
pub type Argument =
  argument.Argument

pub type Formalization =
  argument.Formalization

pub type ValidationResult =
  argument.ValidationResult

pub type ValidationRecord =
  argument.ValidationRecord

pub type Ambiguity =
  argument.Ambiguity

pub type RepairSuggestion =
  argument.RepairSuggestion

pub type RepairType =
  argument.RepairType

// Re-export translation types
pub type TranslationPrompt =
  prompts.TranslationPrompt

pub type PromptConfig =
  prompts.PromptConfig

pub type CompiledTranslation =
  compiler.CompiledTranslation

pub type CompileError =
  compiler.CompileError

pub type LogicDetection =
  logic_detector.LogicDetection

pub type TranslationService =
  translation_service.TranslationService

pub type TranslationResult =
  translation_service.TranslationResult

pub type TranslationError =
  translation_service.TranslationError
