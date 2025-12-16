//// Rule Builder DSL for Modal Logic
////
//// This module provides a fluent DSL for constructing modal logic
//// inference rules, axioms, and validation rules.

import gleam/list
import gleam/option.{type Option, None, Some}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, Atom, Implies, K, K4, KD, KD45, S4, S5, T,
}
import modal_logic/rules/axiom.{type Axiom, type FrameProperty, Axiom}
import modal_logic/rules/inference_rule.{
  type InferenceRule, type PropositionPattern, type RuleMetadata, AnyAtom,
  InferenceRule, PatternAnd, PatternBelieves, PatternImplies, PatternKnows,
  PatternNecessary, PatternNot, PatternObligatory, PatternOr, PatternPermitted,
  PatternPossible, RuleExample, RuleMetadata, SpecificAtom, Wildcard,
  default_metadata,
}

// ============ Pattern DSL Functions ============

/// Create an atom pattern variable
pub fn atom(variable: String) -> PropositionPattern {
  AnyAtom(variable)
}

/// Create a specific atom pattern
pub fn specific(name: String) -> PropositionPattern {
  SpecificAtom(name)
}

/// Create a wildcard pattern
pub fn any(variable: String) -> PropositionPattern {
  Wildcard(variable)
}

/// Create a negation pattern
pub fn not(inner: PropositionPattern) -> PropositionPattern {
  PatternNot(inner)
}

/// Create a conjunction pattern
pub fn and(
  left: PropositionPattern,
  right: PropositionPattern,
) -> PropositionPattern {
  PatternAnd(left, right)
}

/// Create a disjunction pattern
pub fn or(
  left: PropositionPattern,
  right: PropositionPattern,
) -> PropositionPattern {
  PatternOr(left, right)
}

/// Create an implication pattern
pub fn implies(
  antecedent: PropositionPattern,
  consequent: PropositionPattern,
) -> PropositionPattern {
  PatternImplies(antecedent, consequent)
}

/// Create a necessity pattern
pub fn necessary(inner: PropositionPattern) -> PropositionPattern {
  PatternNecessary(inner)
}

/// Create a possibility pattern
pub fn possible(inner: PropositionPattern) -> PropositionPattern {
  PatternPossible(inner)
}

/// Create an obligation pattern
pub fn obligatory(inner: PropositionPattern) -> PropositionPattern {
  PatternObligatory(inner)
}

/// Create a permission pattern
pub fn permitted(inner: PropositionPattern) -> PropositionPattern {
  PatternPermitted(inner)
}

/// Create a knowledge pattern
pub fn knows(agent_var: String, inner: PropositionPattern) -> PropositionPattern {
  PatternKnows(agent_var, inner)
}

/// Create a belief pattern
pub fn believes(
  agent_var: String,
  inner: PropositionPattern,
) -> PropositionPattern {
  PatternBelieves(agent_var, inner)
}

// ============ Inference Rule Builder ============

/// Builder state for inference rules
pub type InferenceRuleBuilder {
  InferenceRuleBuilder(
    id: String,
    name: String,
    description: String,
    premises: List(PropositionPattern),
    conclusion: Option(PropositionPattern),
    valid_in: List(LogicSystem),
    metadata: RuleMetadata,
  )
}

/// Start building an inference rule
pub fn inference_rule(id: String) -> InferenceRuleBuilder {
  InferenceRuleBuilder(
    id: id,
    name: id,
    description: "",
    premises: [],
    conclusion: None,
    valid_in: [K, T, K4, S4, S5, KD, KD45],
    metadata: default_metadata(),
  )
}

/// Set the rule name
pub fn named(
  builder: InferenceRuleBuilder,
  name: String,
) -> InferenceRuleBuilder {
  InferenceRuleBuilder(..builder, name: name)
}

/// Set the rule description
pub fn described(
  builder: InferenceRuleBuilder,
  description: String,
) -> InferenceRuleBuilder {
  InferenceRuleBuilder(..builder, description: description)
}

/// Add a premise pattern
pub fn with_premise(
  builder: InferenceRuleBuilder,
  pattern: PropositionPattern,
) -> InferenceRuleBuilder {
  InferenceRuleBuilder(
    ..builder,
    premises: list.append(builder.premises, [pattern]),
  )
}

/// Add multiple premise patterns
pub fn with_premises(
  builder: InferenceRuleBuilder,
  patterns: List(PropositionPattern),
) -> InferenceRuleBuilder {
  InferenceRuleBuilder(
    ..builder,
    premises: list.append(builder.premises, patterns),
  )
}

/// Set the conclusion pattern
pub fn derives(
  builder: InferenceRuleBuilder,
  pattern: PropositionPattern,
) -> InferenceRuleBuilder {
  InferenceRuleBuilder(..builder, conclusion: Some(pattern))
}

/// Set the logic systems where the rule is valid
pub fn valid_in(
  builder: InferenceRuleBuilder,
  systems: List(LogicSystem),
) -> InferenceRuleBuilder {
  InferenceRuleBuilder(..builder, valid_in: systems)
}

/// Set rule to be valid in all systems
pub fn valid_in_all(builder: InferenceRuleBuilder) -> InferenceRuleBuilder {
  InferenceRuleBuilder(..builder, valid_in: [K, T, K4, S4, S5, KD, KD45])
}

/// Set rule metadata
pub fn with_metadata(
  builder: InferenceRuleBuilder,
  metadata: RuleMetadata,
) -> InferenceRuleBuilder {
  InferenceRuleBuilder(..builder, metadata: metadata)
}

/// Add a tag to the rule
pub fn tagged(
  builder: InferenceRuleBuilder,
  tag: String,
) -> InferenceRuleBuilder {
  let new_metadata =
    RuleMetadata(..builder.metadata, tags: [tag, ..builder.metadata.tags])
  InferenceRuleBuilder(..builder, metadata: new_metadata)
}

/// Add an example to the rule
pub fn with_example(
  builder: InferenceRuleBuilder,
  description: String,
  premises: List(Proposition),
  conclusion: Proposition,
  should_succeed: Bool,
) -> InferenceRuleBuilder {
  let example =
    RuleExample(
      description: description,
      premises: premises,
      conclusion: conclusion,
      should_succeed: should_succeed,
    )
  let new_metadata =
    RuleMetadata(..builder.metadata, examples: [
      example,
      ..builder.metadata.examples
    ])
  InferenceRuleBuilder(..builder, metadata: new_metadata)
}

/// Build the inference rule
pub fn build(builder: InferenceRuleBuilder) -> Result(InferenceRule, String) {
  case builder.conclusion {
    None -> Error("Inference rule must have a conclusion pattern")
    Some(conclusion) ->
      Ok(InferenceRule(
        id: builder.id,
        name: builder.name,
        description: builder.description,
        premise_patterns: builder.premises,
        conclusion_pattern: conclusion,
        valid_in: builder.valid_in,
        metadata: builder.metadata,
      ))
  }
}

// ============ Axiom Builder ============

/// Builder state for axioms
pub type AxiomBuilder {
  AxiomBuilder(
    id: String,
    name: String,
    description: String,
    schema: Option(PropositionPattern),
    included_in: List(LogicSystem),
    frame_property: Option(FrameProperty),
    metadata: RuleMetadata,
  )
}

/// Start building an axiom
pub fn axiom(id: String) -> AxiomBuilder {
  AxiomBuilder(
    id: id,
    name: id,
    description: "",
    schema: None,
    included_in: [],
    frame_property: None,
    metadata: default_metadata(),
  )
}

/// Set the axiom name
pub fn axiom_named(builder: AxiomBuilder, name: String) -> AxiomBuilder {
  AxiomBuilder(..builder, name: name)
}

/// Set the axiom description
pub fn axiom_described(
  builder: AxiomBuilder,
  description: String,
) -> AxiomBuilder {
  AxiomBuilder(..builder, description: description)
}

/// Set the axiom schema
pub fn schema(
  builder: AxiomBuilder,
  pattern: PropositionPattern,
) -> AxiomBuilder {
  AxiomBuilder(..builder, schema: Some(pattern))
}

/// Set the logic systems that include this axiom
pub fn included_in(
  builder: AxiomBuilder,
  systems: List(LogicSystem),
) -> AxiomBuilder {
  AxiomBuilder(..builder, included_in: systems)
}

/// Set the frame property
pub fn frame_property(
  builder: AxiomBuilder,
  property: FrameProperty,
) -> AxiomBuilder {
  AxiomBuilder(..builder, frame_property: Some(property))
}

/// Build the axiom
pub fn build_axiom(builder: AxiomBuilder) -> Result(Axiom, String) {
  case builder.schema {
    None -> Error("Axiom must have a schema pattern")
    Some(schema_pattern) ->
      Ok(Axiom(
        id: builder.id,
        name: builder.name,
        description: builder.description,
        schema: schema_pattern,
        included_in: builder.included_in,
        frame_property: builder.frame_property,
        metadata: builder.metadata,
      ))
  }
}

// ============ Standard Inference Rules ============

/// Modus Ponens: p, p → q ⊢ q
pub fn modus_ponens() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("modus_ponens")
    |> named("Modus Ponens")
    |> described("From p and p implies q, derive q")
    |> with_premise(atom("p"))
    |> with_premise(implies(atom("p"), atom("q")))
    |> derives(atom("q"))
    |> valid_in_all()
    |> tagged("classical")
    |> tagged("fundamental")
    |> with_example(
      "Basic modus ponens",
      [Atom("rain"), Implies(Atom("rain"), Atom("wet"))],
      Atom("wet"),
      True,
    )
    |> build()
  rule
}

/// Modus Tollens: p → q, ¬q ⊢ ¬p
pub fn modus_tollens() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("modus_tollens")
    |> named("Modus Tollens")
    |> described("From p implies q and not q, derive not p")
    |> with_premise(implies(atom("p"), atom("q")))
    |> with_premise(not(atom("q")))
    |> derives(not(atom("p")))
    |> valid_in_all()
    |> tagged("classical")
    |> build()
  rule
}

/// Hypothetical Syllogism: p → q, q → r ⊢ p → r
pub fn hypothetical_syllogism() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("hypothetical_syllogism")
    |> named("Hypothetical Syllogism")
    |> described("From p implies q and q implies r, derive p implies r")
    |> with_premise(implies(atom("p"), atom("q")))
    |> with_premise(implies(atom("q"), atom("r")))
    |> derives(implies(atom("p"), atom("r")))
    |> valid_in_all()
    |> tagged("classical")
    |> build()
  rule
}

/// Disjunctive Syllogism: p ∨ q, ¬p ⊢ q
pub fn disjunctive_syllogism() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("disjunctive_syllogism")
    |> named("Disjunctive Syllogism")
    |> described("From p or q and not p, derive q")
    |> with_premise(or(atom("p"), atom("q")))
    |> with_premise(not(atom("p")))
    |> derives(atom("q"))
    |> valid_in_all()
    |> tagged("classical")
    |> build()
  rule
}

/// Conjunction Introduction: p, q ⊢ p ∧ q
pub fn conjunction_introduction() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("conjunction_introduction")
    |> named("Conjunction Introduction")
    |> described("From p and q, derive p and q")
    |> with_premise(atom("p"))
    |> with_premise(atom("q"))
    |> derives(and(atom("p"), atom("q")))
    |> valid_in_all()
    |> tagged("classical")
    |> build()
  rule
}

/// Conjunction Elimination (Left): p ∧ q ⊢ p
pub fn conjunction_elimination_left() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("conjunction_elimination_left")
    |> named("Conjunction Elimination (Left)")
    |> described("From p and q, derive p")
    |> with_premise(and(atom("p"), atom("q")))
    |> derives(atom("p"))
    |> valid_in_all()
    |> tagged("classical")
    |> build()
  rule
}

/// Conjunction Elimination (Right): p ∧ q ⊢ q
pub fn conjunction_elimination_right() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("conjunction_elimination_right")
    |> named("Conjunction Elimination (Right)")
    |> described("From p and q, derive q")
    |> with_premise(and(atom("p"), atom("q")))
    |> derives(atom("q"))
    |> valid_in_all()
    |> tagged("classical")
    |> build()
  rule
}

/// Double Negation Elimination: ¬¬p ⊢ p
pub fn double_negation_elimination() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("double_negation_elimination")
    |> named("Double Negation Elimination")
    |> described("From not not p, derive p")
    |> with_premise(not(not(atom("p"))))
    |> derives(atom("p"))
    |> valid_in_all()
    |> tagged("classical")
    |> build()
  rule
}

// ============ Modal Inference Rules ============

/// Modal Modus Ponens (K): □(p → q), □p ⊢ □q
pub fn modal_modus_ponens() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("modal_modus_ponens")
    |> named("Modal Modus Ponens")
    |> described(
      "From necessary(p implies q) and necessary p, derive necessary q",
    )
    |> with_premise(necessary(implies(atom("p"), atom("q"))))
    |> with_premise(necessary(atom("p")))
    |> derives(necessary(atom("q")))
    |> valid_in_all()
    |> tagged("modal")
    |> tagged("k_axiom")
    |> build()
  rule
}

/// Necessitation Rule: p ⊢ □p (for theorems only)
pub fn necessitation() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("necessitation")
    |> named("Necessitation")
    |> described("If p is a theorem, then necessary p is a theorem")
    |> with_premise(any("theorem"))
    |> derives(necessary(any("theorem")))
    |> valid_in_all()
    |> tagged("modal")
    |> tagged("meta_rule")
    |> build()
  rule
}

/// T Axiom Application: □p ⊢ p
pub fn t_axiom_application() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("t_axiom_application")
    |> named("T Axiom Application")
    |> described("From necessary p, derive p (reflexivity)")
    |> with_premise(necessary(atom("p")))
    |> derives(atom("p"))
    |> valid_in([T, S4, S5])
    |> tagged("modal")
    |> tagged("t_axiom")
    |> build()
  rule
}

/// 4 Axiom Application: □p ⊢ □□p
pub fn four_axiom_application() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("four_axiom_application")
    |> named("4 Axiom Application")
    |> described(
      "From necessary p, derive necessary necessary p (transitivity)",
    )
    |> with_premise(necessary(atom("p")))
    |> derives(necessary(necessary(atom("p"))))
    |> valid_in([K4, S4, S5, KD45])
    |> tagged("modal")
    |> tagged("4_axiom")
    |> build()
  rule
}

/// 5 Axiom Application: ◇p ⊢ □◇p
pub fn five_axiom_application() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("five_axiom_application")
    |> named("5 Axiom Application")
    |> described("From possible p, derive necessary possible p (Euclidean)")
    |> with_premise(possible(atom("p")))
    |> derives(necessary(possible(atom("p"))))
    |> valid_in([S5, KD45])
    |> tagged("modal")
    |> tagged("5_axiom")
    |> build()
  rule
}

/// D Axiom Application: □p ⊢ ◇p
pub fn d_axiom_application() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("d_axiom_application")
    |> named("D Axiom Application")
    |> described("From necessary p, derive possible p (seriality)")
    |> with_premise(necessary(atom("p")))
    |> derives(possible(atom("p")))
    |> valid_in([KD, KD45])
    |> tagged("modal")
    |> tagged("d_axiom")
    |> tagged("deontic")
    |> build()
  rule
}

/// Possibility from Non-Necessity: ¬□p ⊢ ◇¬p
pub fn possibility_from_non_necessity() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("possibility_from_non_necessity")
    |> named("Possibility from Non-Necessity")
    |> described("From not necessary p, derive possible not p")
    |> with_premise(not(necessary(atom("p"))))
    |> derives(possible(not(atom("p"))))
    |> valid_in_all()
    |> tagged("modal")
    |> tagged("dual")
    |> build()
  rule
}

// ============ Deontic Inference Rules ============

/// Deontic Modus Ponens: O(p → q), Op ⊢ Oq
pub fn deontic_modus_ponens() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("deontic_modus_ponens")
    |> named("Deontic Modus Ponens")
    |> described(
      "From obligatory(p implies q) and obligatory p, derive obligatory q",
    )
    |> with_premise(obligatory(implies(atom("p"), atom("q"))))
    |> with_premise(obligatory(atom("p")))
    |> derives(obligatory(atom("q")))
    |> valid_in([KD, KD45])
    |> tagged("deontic")
    |> build()
  rule
}

/// Permission from Obligation: Op ⊢ Pp
pub fn permission_from_obligation() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("permission_from_obligation")
    |> named("Permission from Obligation")
    |> described("From obligatory p, derive permitted p")
    |> with_premise(obligatory(atom("p")))
    |> derives(permitted(atom("p")))
    |> valid_in([KD, KD45])
    |> tagged("deontic")
    |> tagged("d_axiom")
    |> build()
  rule
}

// ============ Epistemic Inference Rules ============

/// Knowledge Distribution: K(p → q), Kp ⊢ Kq
pub fn knowledge_distribution() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("knowledge_distribution")
    |> named("Knowledge Distribution")
    |> described("From knows(p implies q) and knows p, derive knows q")
    |> with_premise(knows("agent", implies(atom("p"), atom("q"))))
    |> with_premise(knows("agent", atom("p")))
    |> derives(knows("agent", atom("q")))
    |> valid_in([T, S4, S5])
    |> tagged("epistemic")
    |> build()
  rule
}

/// Knowledge implies Truth: Kp ⊢ p
pub fn knowledge_implies_truth() -> InferenceRule {
  let assert Ok(rule) =
    inference_rule("knowledge_implies_truth")
    |> named("Knowledge implies Truth")
    |> described("From knows p, derive p")
    |> with_premise(knows("agent", atom("p")))
    |> derives(atom("p"))
    |> valid_in([T, S4, S5])
    |> tagged("epistemic")
    |> tagged("t_axiom")
    |> build()
  rule
}

// ============ Rule Collections ============

/// Get all classical inference rules
pub fn all_classical_rules() -> List(InferenceRule) {
  [
    modus_ponens(),
    modus_tollens(),
    hypothetical_syllogism(),
    disjunctive_syllogism(),
    conjunction_introduction(),
    conjunction_elimination_left(),
    conjunction_elimination_right(),
    double_negation_elimination(),
  ]
}

/// Get all modal inference rules
pub fn all_modal_rules() -> List(InferenceRule) {
  [
    modal_modus_ponens(),
    necessitation(),
    t_axiom_application(),
    four_axiom_application(),
    five_axiom_application(),
    d_axiom_application(),
    possibility_from_non_necessity(),
  ]
}

/// Get all deontic inference rules
pub fn all_deontic_rules() -> List(InferenceRule) {
  [deontic_modus_ponens(), permission_from_obligation()]
}

/// Get all epistemic inference rules
pub fn all_epistemic_rules() -> List(InferenceRule) {
  [knowledge_distribution(), knowledge_implies_truth()]
}

/// Get all inference rules
pub fn all_rules() -> List(InferenceRule) {
  list.flatten([
    all_classical_rules(),
    all_modal_rules(),
    all_deontic_rules(),
    all_epistemic_rules(),
  ])
}

/// Get rules valid in a specific logic system
pub fn rules_for_system(system: LogicSystem) -> List(InferenceRule) {
  all_rules()
  |> list.filter(fn(rule) { list.contains(rule.valid_in, system) })
}
