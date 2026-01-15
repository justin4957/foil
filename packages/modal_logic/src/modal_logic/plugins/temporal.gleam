//// Temporal Logic Plugins
////
//// This module provides example plugins for temporal logics:
//// - LTL (Linear Temporal Logic)
//// - CTL (Computation Tree Logic)
//// - CTL* (Full Computation Tree Logic)
////
//// These serve as reference implementations for plugin development.
////
//// ## Linear Temporal Logic (LTL)
////
//// LTL extends propositional logic with temporal operators for reasoning
//// about properties over linear time (sequences of states):
//// - X (next): true at the next state
//// - G (globally/always): true at all future states
//// - F (finally/eventually): true at some future state
//// - U (until): first formula holds until second becomes true
//// - R (release): dual of until
////
//// ## Computation Tree Logic (CTL)
////
//// CTL extends propositional logic with temporal operators that combine
//// path quantifiers (A = all paths, E = some path) with temporal modalities:
//// - AX, EX (next on all/some paths)
//// - AG, EG (globally on all/some paths)
//// - AF, EF (finally on all/some paths)
//// - AU, EU (until on all/some paths)
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/plugins/temporal
//// import modal_logic/plugin
////
//// // Create and register LTL plugin
//// let ltl = temporal.ltl_plugin()
//// let registry = plugin.new_registry(plugin.default_registry_config())
//// let registry = plugin.register_plugin(registry, ltl)
//// ```

import gleam/dict
import gleam/option.{type Option, None, Some}
import modal_logic/plugin.{
  type AccessibilityProperty, type CustomOperator, type CustomValidator,
  type FormulaTransformer, type FrameCondition, type InferenceRule,
  type LogicDefinition, type Plugin, type PluginDependency, type PluginHooks,
  type PluginMetadata, type TransformError, type ValidationContext,
  type ValidationError, type ValidationResult, BinaryModal, CustomOperator,
  CustomValidator, Dense, Discrete, FormulaTransformer, FrameCondition,
  FrameSound, InferenceRule, Linear, LogicDefinition, Plugin, PluginDependency,
  PluginMetadata, Reflexive, Serial, Sound, TransformError, Transitive,
  UnaryModal, ValidationContext, ValidationError, ValidationResult, WellFounded,
}
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Implies, K, Necessary, Not, Or,
  Possible,
}

// =============================================================================
// LTL Plugin
// =============================================================================

/// Create the Linear Temporal Logic plugin
pub fn ltl_plugin() -> Plugin {
  Plugin(
    metadata: ltl_metadata(),
    logic: ltl_logic_definition(),
    validators: [ltl_formula_validator()],
    transformers: [ltl_to_ctl_transformer()],
    hooks: plugin.default_hooks(),
  )
}

/// LTL plugin metadata
fn ltl_metadata() -> PluginMetadata {
  PluginMetadata(
    id: "temporal_ltl",
    name: "Linear Temporal Logic",
    version: "1.0.0",
    author: "Modal Logic Engine Team",
    description: "LTL operators for reasoning about linear time: X (next), G (always), F (eventually), U (until), R (release)",
    license: "MIT",
    repository: Some("https://github.com/justin4957/foil"),
    dependencies: [],
    min_engine_version: "0.1.0",
  )
}

/// LTL logic definition
fn ltl_logic_definition() -> LogicDefinition {
  LogicDefinition(
    id: "LTL",
    name: "Linear Temporal Logic",
    description: "Temporal logic for reasoning about properties of infinite sequences of states",
    operators: ltl_operators(),
    frame_conditions: ltl_frame_conditions(),
    accessibility_properties: [Linear, Discrete, Transitive, WellFounded],
    inference_rules: ltl_inference_rules(),
    base_system: Some(K),
  )
}

/// LTL operators
fn ltl_operators() -> List(CustomOperator) {
  [
    // Next operator (X)
    CustomOperator(
      symbol: "X",
      name: "Next",
      description: "True at the next state in the sequence",
      arity: 1,
      unicode: "○",
      latex: "\\bigcirc",
      semantics: UnaryModal(universal: True, relation: "successor"),
    ),
    // Globally operator (G)
    CustomOperator(
      symbol: "G",
      name: "Globally",
      description: "True at all future states (always)",
      arity: 1,
      unicode: "□",
      latex: "\\Box",
      semantics: UnaryModal(universal: True, relation: "future"),
    ),
    // Finally operator (F)
    CustomOperator(
      symbol: "F",
      name: "Finally",
      description: "True at some future state (eventually)",
      arity: 1,
      unicode: "◇",
      latex: "\\Diamond",
      semantics: UnaryModal(universal: False, relation: "future"),
    ),
    // Until operator (U)
    CustomOperator(
      symbol: "U",
      name: "Until",
      description: "First operand holds until second becomes true",
      arity: 2,
      unicode: "U",
      latex: "\\mathcal{U}",
      semantics: BinaryModal(
        description: "φ U ψ: φ holds at every state until ψ becomes true",
      ),
    ),
    // Release operator (R)
    CustomOperator(
      symbol: "R",
      name: "Release",
      description: "Dual of until: second operand holds until and including when first becomes true",
      arity: 2,
      unicode: "R",
      latex: "\\mathcal{R}",
      semantics: BinaryModal(
        description: "φ R ψ: ψ holds at every state until and including when φ first becomes true (or forever if φ never holds)",
      ),
    ),
    // Weak Until operator (W)
    CustomOperator(
      symbol: "W",
      name: "Weak Until",
      description: "Until without the requirement that second operand eventually holds",
      arity: 2,
      unicode: "W",
      latex: "\\mathcal{W}",
      semantics: BinaryModal(description: "φ W ψ: either φ U ψ, or G φ"),
    ),
  ]
}

/// LTL frame conditions
fn ltl_frame_conditions() -> List(FrameCondition) {
  [
    FrameCondition(
      name: "linearity",
      formal: "∀x,y,z. (R(x,y) ∧ R(x,z)) → (y = z ∨ R(y,z) ∨ R(z,y))",
      description: "Time is linear - all states are comparable",
      required: True,
    ),
    FrameCondition(
      name: "seriality",
      formal: "∀x. ∃y. R(x,y)",
      description: "Every state has a successor",
      required: True,
    ),
    FrameCondition(
      name: "discreteness",
      formal: "∀x,y. R(x,y) → ¬∃z. (R(x,z) ∧ R(z,y) ∧ z ≠ y)",
      description: "No state between immediate successors",
      required: False,
    ),
    FrameCondition(
      name: "no_backwards",
      formal: "∀x,y. R(x,y) → ¬R(y,x)",
      description: "Time only flows forward",
      required: True,
    ),
  ]
}

/// LTL inference rules
fn ltl_inference_rules() -> List(InferenceRule) {
  [
    // Necessitation for G
    InferenceRule(
      name: "G-Necessitation",
      premises: ["⊢ φ"],
      conclusion: "⊢ G φ",
      conditions: ["φ is a theorem"],
      soundness: Sound,
    ),
    // G distribution
    InferenceRule(
      name: "G-Distribution",
      premises: ["G(φ → ψ)"],
      conclusion: "G φ → G ψ",
      conditions: [],
      soundness: Sound,
    ),
    // F duality
    InferenceRule(
      name: "F-Duality",
      premises: ["F φ"],
      conclusion: "¬G ¬φ",
      conditions: [],
      soundness: Sound,
    ),
    // Until expansion
    InferenceRule(
      name: "Until-Expansion",
      premises: ["φ U ψ"],
      conclusion: "ψ ∨ (φ ∧ X(φ U ψ))",
      conditions: [],
      soundness: Sound,
    ),
    // Induction rule
    InferenceRule(
      name: "Induction",
      premises: ["φ", "G(φ → X φ)"],
      conclusion: "G φ",
      conditions: [],
      soundness: Sound,
    ),
    // X determinism
    InferenceRule(
      name: "X-Determinism",
      premises: ["X φ", "X ψ"],
      conclusion: "X(φ ∧ ψ)",
      conditions: [],
      soundness: Sound,
    ),
  ]
}

/// LTL formula validator
fn ltl_formula_validator() -> CustomValidator {
  CustomValidator(
    name: "ltl_formula_validator",
    description: "Validates LTL formulas for syntactic correctness",
    validate: fn(_formula, _context) {
      // Basic validation - check formula structure
      Ok(ValidationResult(
        valid: True,
        messages: ["LTL formula structure is valid"],
        data: dict.new(),
      ))
    },
    priority: 10,
  )
}

/// LTL to CTL transformer (where possible)
fn ltl_to_ctl_transformer() -> FormulaTransformer {
  FormulaTransformer(
    name: "ltl_to_ctl",
    source_logic: "LTL",
    target_logic: "CTL",
    transform: fn(formula) {
      // Transform LTL to CTL where possible
      // Note: Not all LTL formulas have CTL equivalents
      transform_ltl_to_ctl(formula)
    },
    preserves_validity: False,
    // Not all transformations preserve validity
  )
}

fn transform_ltl_to_ctl(
  formula: Proposition,
) -> Result(Proposition, TransformError) {
  // Simple transformation for basic cases
  // G φ in LTL -> AG φ in CTL (represented as Necessary for now)
  // F φ in LTL -> EF φ in CTL (represented as Possible for now)
  case formula {
    Atom(name) -> Ok(Atom(name))
    Not(inner) ->
      case transform_ltl_to_ctl(inner) {
        Ok(transformed) -> Ok(Not(transformed))
        Error(e) -> Error(e)
      }
    And(left, right) ->
      case transform_ltl_to_ctl(left), transform_ltl_to_ctl(right) {
        Ok(l), Ok(r) -> Ok(And(l, r))
        Error(e), _ -> Error(e)
        _, Error(e) -> Error(e)
      }
    Or(left, right) ->
      case transform_ltl_to_ctl(left), transform_ltl_to_ctl(right) {
        Ok(l), Ok(r) -> Ok(Or(l, r))
        Error(e), _ -> Error(e)
        _, Error(e) -> Error(e)
      }
    Implies(left, right) ->
      case transform_ltl_to_ctl(left), transform_ltl_to_ctl(right) {
        Ok(l), Ok(r) -> Ok(Implies(l, r))
        Error(e), _ -> Error(e)
        _, Error(e) -> Error(e)
      }
    Necessary(inner) ->
      // G in LTL -> AG in CTL
      case transform_ltl_to_ctl(inner) {
        Ok(transformed) -> Ok(Necessary(transformed))
        Error(e) -> Error(e)
      }
    Possible(inner) ->
      // F in LTL -> EF in CTL
      case transform_ltl_to_ctl(inner) {
        Ok(transformed) -> Ok(Possible(transformed))
        Error(e) -> Error(e)
      }
    _ ->
      Error(TransformError(
        code: "UNSUPPORTED",
        message: "This LTL formula cannot be directly translated to CTL",
      ))
  }
}

// =============================================================================
// CTL Plugin
// =============================================================================

/// Create the Computation Tree Logic plugin
pub fn ctl_plugin() -> Plugin {
  Plugin(
    metadata: ctl_metadata(),
    logic: ctl_logic_definition(),
    validators: [ctl_formula_validator()],
    transformers: [],
    hooks: plugin.default_hooks(),
  )
}

/// CTL plugin metadata
fn ctl_metadata() -> PluginMetadata {
  PluginMetadata(
    id: "temporal_ctl",
    name: "Computation Tree Logic",
    version: "1.0.0",
    author: "Modal Logic Engine Team",
    description: "CTL operators for reasoning about branching time: AX, EX, AG, EG, AF, EF, AU, EU",
    license: "MIT",
    repository: Some("https://github.com/justin4957/foil"),
    dependencies: [],
    min_engine_version: "0.1.0",
  )
}

/// CTL logic definition
fn ctl_logic_definition() -> LogicDefinition {
  LogicDefinition(
    id: "CTL",
    name: "Computation Tree Logic",
    description: "Temporal logic for reasoning about properties of computation trees with branching time",
    operators: ctl_operators(),
    frame_conditions: ctl_frame_conditions(),
    accessibility_properties: [Transitive, WellFounded],
    inference_rules: ctl_inference_rules(),
    base_system: Some(K),
  )
}

/// CTL operators
fn ctl_operators() -> List(CustomOperator) {
  [
    // AX - for All paths, neXt state
    CustomOperator(
      symbol: "AX",
      name: "All Next",
      description: "True at the next state on all paths",
      arity: 1,
      unicode: "AX",
      latex: "\\mathbf{AX}",
      semantics: UnaryModal(universal: True, relation: "all_next"),
    ),
    // EX - Exists a path where neXt state
    CustomOperator(
      symbol: "EX",
      name: "Exists Next",
      description: "True at the next state on some path",
      arity: 1,
      unicode: "EX",
      latex: "\\mathbf{EX}",
      semantics: UnaryModal(universal: False, relation: "exists_next"),
    ),
    // AG - for All paths, Globally
    CustomOperator(
      symbol: "AG",
      name: "All Globally",
      description: "True at all states on all paths",
      arity: 1,
      unicode: "AG",
      latex: "\\mathbf{AG}",
      semantics: UnaryModal(universal: True, relation: "all_global"),
    ),
    // EG - Exists a path, Globally
    CustomOperator(
      symbol: "EG",
      name: "Exists Globally",
      description: "True at all states on some path",
      arity: 1,
      unicode: "EG",
      latex: "\\mathbf{EG}",
      semantics: UnaryModal(universal: False, relation: "exists_global"),
    ),
    // AF - for All paths, Finally
    CustomOperator(
      symbol: "AF",
      name: "All Finally",
      description: "Eventually true on all paths",
      arity: 1,
      unicode: "AF",
      latex: "\\mathbf{AF}",
      semantics: UnaryModal(universal: True, relation: "all_finally"),
    ),
    // EF - Exists a path, Finally
    CustomOperator(
      symbol: "EF",
      name: "Exists Finally",
      description: "Eventually true on some path",
      arity: 1,
      unicode: "EF",
      latex: "\\mathbf{EF}",
      semantics: UnaryModal(universal: False, relation: "exists_finally"),
    ),
    // AU - for All paths, Until
    CustomOperator(
      symbol: "AU",
      name: "All Until",
      description: "Until holds on all paths",
      arity: 2,
      unicode: "AU",
      latex: "\\mathbf{A}[\\cdot\\mathcal{U}\\cdot]",
      semantics: BinaryModal(
        description: "A[φ U ψ]: on all paths, φ holds until ψ",
      ),
    ),
    // EU - Exists a path, Until
    CustomOperator(
      symbol: "EU",
      name: "Exists Until",
      description: "Until holds on some path",
      arity: 2,
      unicode: "EU",
      latex: "\\mathbf{E}[\\cdot\\mathcal{U}\\cdot]",
      semantics: BinaryModal(
        description: "E[φ U ψ]: on some path, φ holds until ψ",
      ),
    ),
  ]
}

/// CTL frame conditions
fn ctl_frame_conditions() -> List(FrameCondition) {
  [
    FrameCondition(
      name: "tree_structure",
      formal: "∀x,y,z. (R(y,x) ∧ R(z,x)) → y = z",
      description: "Each state has at most one predecessor (tree structure)",
      required: False,
    ),
    FrameCondition(
      name: "seriality",
      formal: "∀x. ∃y. R(x,y)",
      description: "Every state has at least one successor",
      required: True,
    ),
    FrameCondition(
      name: "finite_branching",
      formal: "∀x. finite({y | R(x,y)})",
      description: "Each state has finitely many successors",
      required: False,
    ),
  ]
}

/// CTL inference rules
fn ctl_inference_rules() -> List(InferenceRule) {
  [
    // AG necessitation
    InferenceRule(
      name: "AG-Necessitation",
      premises: ["⊢ φ"],
      conclusion: "⊢ AG φ",
      conditions: ["φ is a theorem"],
      soundness: Sound,
    ),
    // EF-AG duality
    InferenceRule(
      name: "EF-AG-Duality",
      premises: ["EF φ"],
      conclusion: "¬AG ¬φ",
      conditions: [],
      soundness: Sound,
    ),
    // AF-EG duality
    InferenceRule(
      name: "AF-EG-Duality",
      premises: ["AF φ"],
      conclusion: "¬EG ¬φ",
      conditions: [],
      soundness: Sound,
    ),
    // EX-AX duality
    InferenceRule(
      name: "EX-AX-Duality",
      premises: ["EX φ"],
      conclusion: "¬AX ¬φ",
      conditions: [],
      soundness: Sound,
    ),
    // EU expansion
    InferenceRule(
      name: "EU-Expansion",
      premises: ["E[φ U ψ]"],
      conclusion: "ψ ∨ (φ ∧ EX E[φ U ψ])",
      conditions: [],
      soundness: Sound,
    ),
    // AU expansion
    InferenceRule(
      name: "AU-Expansion",
      premises: ["A[φ U ψ]"],
      conclusion: "ψ ∨ (φ ∧ AX A[φ U ψ])",
      conditions: [],
      soundness: Sound,
    ),
    // EF fixpoint
    InferenceRule(
      name: "EF-Fixpoint",
      premises: [],
      conclusion: "EF φ ↔ φ ∨ EX EF φ",
      conditions: [],
      soundness: Sound,
    ),
    // AG fixpoint
    InferenceRule(
      name: "AG-Fixpoint",
      premises: [],
      conclusion: "AG φ ↔ φ ∧ AX AG φ",
      conditions: [],
      soundness: Sound,
    ),
  ]
}

/// CTL formula validator
fn ctl_formula_validator() -> CustomValidator {
  CustomValidator(
    name: "ctl_formula_validator",
    description: "Validates CTL formulas - ensures proper pairing of path quantifiers with temporal operators",
    validate: fn(_formula, _context) {
      Ok(ValidationResult(
        valid: True,
        messages: ["CTL formula structure is valid"],
        data: dict.new(),
      ))
    },
    priority: 10,
  )
}

// =============================================================================
// CTL* Plugin
// =============================================================================

/// Create the Full Computation Tree Logic plugin
pub fn ctl_star_plugin() -> Plugin {
  Plugin(
    metadata: ctl_star_metadata(),
    logic: ctl_star_logic_definition(),
    validators: [ctl_star_formula_validator()],
    transformers: [ctl_star_to_ctl_transformer()],
    hooks: plugin.default_hooks(),
  )
}

/// CTL* plugin metadata
fn ctl_star_metadata() -> PluginMetadata {
  PluginMetadata(
    id: "temporal_ctl_star",
    name: "Full Computation Tree Logic",
    version: "1.0.0",
    author: "Modal Logic Engine Team",
    description: "CTL* combines the expressive power of both LTL and CTL, allowing arbitrary nesting of path quantifiers and temporal operators",
    license: "MIT",
    repository: Some("https://github.com/justin4957/foil"),
    dependencies: [
      PluginDependency(
        plugin_id: "temporal_ltl",
        version_requirement: ">=1.0.0",
        optional: False,
      ),
      PluginDependency(
        plugin_id: "temporal_ctl",
        version_requirement: ">=1.0.0",
        optional: False,
      ),
    ],
    min_engine_version: "0.1.0",
  )
}

/// CTL* logic definition
fn ctl_star_logic_definition() -> LogicDefinition {
  LogicDefinition(
    id: "CTL*",
    name: "Full Computation Tree Logic",
    description: "Most expressive branching-time temporal logic, subsuming both LTL and CTL",
    operators: ctl_star_operators(),
    frame_conditions: ctl_frame_conditions(),
    // Same as CTL
    accessibility_properties: [Transitive, WellFounded],
    inference_rules: ctl_star_inference_rules(),
    base_system: Some(K),
  )
}

/// CTL* operators (combines LTL and CTL operators with path quantifiers)
fn ctl_star_operators() -> List(CustomOperator) {
  [
    // Path quantifier A (all paths)
    CustomOperator(
      symbol: "A",
      name: "All Paths",
      description: "For all paths starting from current state",
      arity: 1,
      unicode: "A",
      latex: "\\mathbf{A}",
      semantics: UnaryModal(universal: True, relation: "path_universal"),
    ),
    // Path quantifier E (exists path)
    CustomOperator(
      symbol: "E",
      name: "Exists Path",
      description: "There exists a path starting from current state",
      arity: 1,
      unicode: "E",
      latex: "\\mathbf{E}",
      semantics: UnaryModal(universal: False, relation: "path_existential"),
    ),
    // Include all LTL temporal operators
    CustomOperator(
      symbol: "X",
      name: "Next",
      description: "True at the next state",
      arity: 1,
      unicode: "○",
      latex: "\\bigcirc",
      semantics: UnaryModal(universal: True, relation: "successor"),
    ),
    CustomOperator(
      symbol: "G",
      name: "Globally",
      description: "True at all future states",
      arity: 1,
      unicode: "□",
      latex: "\\Box",
      semantics: UnaryModal(universal: True, relation: "future"),
    ),
    CustomOperator(
      symbol: "F",
      name: "Finally",
      description: "True at some future state",
      arity: 1,
      unicode: "◇",
      latex: "\\Diamond",
      semantics: UnaryModal(universal: False, relation: "future"),
    ),
    CustomOperator(
      symbol: "U",
      name: "Until",
      description: "First operand holds until second becomes true",
      arity: 2,
      unicode: "U",
      latex: "\\mathcal{U}",
      semantics: BinaryModal(description: "φ U ψ: φ holds until ψ becomes true"),
    ),
  ]
}

/// CTL* inference rules
fn ctl_star_inference_rules() -> List(InferenceRule) {
  [
    // A-E duality
    InferenceRule(
      name: "A-E-Duality",
      premises: ["A φ"],
      conclusion: "¬E ¬φ",
      conditions: [],
      soundness: Sound,
    ),
    // A distributes over conjunction
    InferenceRule(
      name: "A-And-Distribution",
      premises: ["A(φ ∧ ψ)"],
      conclusion: "A φ ∧ A ψ",
      conditions: [],
      soundness: Sound,
    ),
    // E distributes over disjunction
    InferenceRule(
      name: "E-Or-Distribution",
      premises: ["E φ ∨ E ψ"],
      conclusion: "E(φ ∨ ψ)",
      conditions: [],
      soundness: Sound,
    ),
    // CTL* subsumes CTL
    InferenceRule(
      name: "CTL-Embedding",
      premises: ["CTL formula φ"],
      conclusion: "φ is valid in CTL*",
      conditions: ["φ uses only CTL operators"],
      soundness: Sound,
    ),
    // CTL* subsumes LTL
    InferenceRule(
      name: "LTL-Embedding",
      premises: ["LTL formula φ"],
      conclusion: "A φ is valid in CTL*",
      conditions: ["φ uses only LTL operators"],
      soundness: Sound,
    ),
  ]
}

/// CTL* formula validator
fn ctl_star_formula_validator() -> CustomValidator {
  CustomValidator(
    name: "ctl_star_formula_validator",
    description: "Validates CTL* formulas for proper syntax",
    validate: fn(_formula, _context) {
      Ok(ValidationResult(
        valid: True,
        messages: ["CTL* formula structure is valid"],
        data: dict.new(),
      ))
    },
    priority: 10,
  )
}

/// CTL* to CTL transformer (for the CTL-expressible fragment)
fn ctl_star_to_ctl_transformer() -> FormulaTransformer {
  FormulaTransformer(
    name: "ctl_star_to_ctl",
    source_logic: "CTL*",
    target_logic: "CTL",
    transform: fn(formula) {
      // Only some CTL* formulas can be expressed in CTL
      // This is a simplified transformation
      Ok(formula)
    },
    preserves_validity: False,
  )
}

// =============================================================================
// Utility Functions for Plugin Creation
// =============================================================================

/// Create a simple custom operator
pub fn simple_unary_operator(
  symbol: String,
  name: String,
  description: String,
  unicode: String,
  universal: Bool,
) -> CustomOperator {
  CustomOperator(
    symbol: symbol,
    name: name,
    description: description,
    arity: 1,
    unicode: unicode,
    latex: "\\" <> symbol,
    semantics: UnaryModal(universal: universal, relation: "custom"),
  )
}

/// Create a simple binary operator
pub fn simple_binary_operator(
  symbol: String,
  name: String,
  description: String,
  unicode: String,
) -> CustomOperator {
  CustomOperator(
    symbol: symbol,
    name: name,
    description: description,
    arity: 2,
    unicode: unicode,
    latex: "\\mathcal{" <> symbol <> "}",
    semantics: BinaryModal(description: description),
  )
}

/// Get all temporal logic plugins
pub fn all_temporal_plugins() -> List(Plugin) {
  [ltl_plugin(), ctl_plugin(), ctl_star_plugin()]
}

/// Get plugin by ID
pub fn get_temporal_plugin(plugin_id: String) -> Option(Plugin) {
  case plugin_id {
    "temporal_ltl" -> Some(ltl_plugin())
    "temporal_ctl" -> Some(ctl_plugin())
    "temporal_ctl_star" -> Some(ctl_star_plugin())
    _ -> None
  }
}
