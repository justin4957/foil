//// Modal Logic - Core domain types for modal logic analysis
////
//// This library provides shared types for:
//// - Modal propositions (alethic, deontic, epistemic)
//// - Arguments and formalizations
//// - Logic systems (K, T, S4, S5, KD, etc.)
////
//// ## Quick Start
////
//// ```gleam
//// import modal_logic/proposition.{Atom, Necessary, Implies}
////
//// let proposition = Implies(Atom("p"), Necessary(Atom("q")))
//// ```

import modal_logic/argument
import modal_logic/proposition

// Re-export proposition types
pub type Proposition = proposition.Proposition
pub type LogicSystem = proposition.LogicSystem
pub type ModalInterpretation = proposition.ModalInterpretation

// Re-export argument types
pub type Argument = argument.Argument
pub type Formalization = argument.Formalization
pub type ValidationResult = argument.ValidationResult
pub type Ambiguity = argument.Ambiguity
