//// Core proposition types for modal logic

/// A modal logic proposition
pub type Proposition {
  /// Atomic proposition with a name
  Atom(String)

  /// Negation
  Not(Proposition)

  /// Conjunction (AND)
  And(Proposition, Proposition)

  /// Disjunction (OR)
  Or(Proposition, Proposition)

  /// Material implication
  Implies(Proposition, Proposition)

  /// Alethic necessity (□)
  Necessary(Proposition)

  /// Alethic possibility (◇)
  Possible(Proposition)

  /// Deontic obligation
  Obligatory(Proposition)

  /// Deontic permission
  Permitted(Proposition)

  /// Epistemic knowledge
  Knows(agent: String, proposition: Proposition)

  /// Epistemic belief
  Believes(agent: String, proposition: Proposition)
}

/// Modal logic system
pub type LogicSystem {
  /// Base modal logic
  K

  /// Reflexive
  T

  /// Transitive
  K4

  /// Reflexive + Transitive
  S4

  /// Equivalence relation
  S5

  /// Deontic logic (serial)
  KD

  /// Deontic S5
  KD45
}

/// Modal interpretation type
pub type ModalInterpretation {
  /// Knowledge/belief modalities
  Epistemic

  /// Obligation/permission modalities
  Deontic

  /// Necessity/possibility modalities
  Alethic

  /// Temporal modalities
  Temporal
}
