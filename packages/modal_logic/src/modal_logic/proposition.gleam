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

  // =========================================================================
  // Probabilistic Modal Logic Operators (Phase E.1)
  // =========================================================================
  /// Probable - probability > 0.5
  /// Represents "φ is probably true"
  Probable(Proposition)

  /// Probability at least threshold: P(φ) ≥ threshold
  /// Represents "probability of φ is at least t"
  ProbAtLeast(proposition: Proposition, threshold: Float)

  /// Probability at most threshold: P(φ) ≤ threshold
  /// Represents "probability of φ is at most t"
  ProbAtMost(proposition: Proposition, threshold: Float)

  /// Exact probability: P(φ) = value
  /// Represents "probability of φ is exactly p"
  ProbExact(proposition: Proposition, probability: Float)

  /// Probability range: low ≤ P(φ) ≤ high
  /// Represents "probability of φ is between low and high"
  ProbRange(proposition: Proposition, low: Float, high: Float)

  /// Conditional probability: P(φ|ψ) = value
  /// Represents "probability of φ given ψ is p"
  CondProb(consequent: Proposition, antecedent: Proposition, probability: Float)
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
