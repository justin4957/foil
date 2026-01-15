//// Dataset-Specific Modal Templates
////
//// This module provides pre-built modal logic templates for external
//// datasets (FOLIO, LogiQA, InPhO). Templates enable immediate application
//// of modal analysis to domain-specific problems without manual formalization.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, KD, Knows,
  Necessary, Not, Or, Possible, S4, S5, T,
}

// =============================================================================
// Types
// =============================================================================

/// Dataset type
pub type Dataset {
  FOLIO
  LogiQA
  InPhO
}

/// Template type for each dataset
pub type TemplateType {
  // FOLIO templates
  FOLIOBelief
  FOLIOUniversal
  FOLIOExistential
  FOLIOConditional

  // LogiQA templates
  LogiQAMultiHop
  LogiQAChainReasoning
  LogiQAConstraint
  LogiQAComparison

  // InPhO templates
  InPhOPhilosophicalArgument
  InPhOConceptualAnalysis
  InPhOMetaphysical
  InPhOEpistemic
}

/// Dataset template
pub type DatasetTemplate {
  DatasetTemplate(
    id: String,
    name: String,
    dataset: Dataset,
    template_type: TemplateType,
    description: String,
    logic_system: LogicSystem,
    pattern_description: String,
    example_premises: List(String),
    example_conclusion: String,
    use_cases: List(String),
  )
}

/// Template application result
pub type TemplateApplication {
  TemplateApplication(
    template: DatasetTemplate,
    applied_premises: List(Proposition),
    applied_conclusion: Proposition,
    confidence: Float,
    notes: List(String),
  )
}

// =============================================================================
// FOLIO Templates
// =============================================================================

/// Get all FOLIO templates
pub fn folio_templates() -> List(DatasetTemplate) {
  [
    DatasetTemplate(
      id: "folio_belief_01",
      name: "Belief Propagation",
      dataset: FOLIO,
      template_type: FOLIOBelief,
      description: "Model agent beliefs and belief propagation in FOLIO examples",
      logic_system: S5,
      pattern_description: "If agent believes all premises, they believe the conclusion",
      example_premises: [
        "All humans are mortal",
        "Socrates is a human",
      ],
      example_conclusion: "Socrates is mortal",
      use_cases: [
        "Logical reasoning with beliefs",
        "Agent knowledge modeling",
        "Belief consistency checking",
      ],
    ),
    DatasetTemplate(
      id: "folio_universal_01",
      name: "Universal Quantification as Necessity",
      dataset: FOLIO,
      template_type: FOLIOUniversal,
      description: "Translate universal quantifiers to modal necessity",
      logic_system: T,
      pattern_description: "∀x P(x) becomes □P(x) - universal truth as necessity",
      example_premises: [
        "All birds can fly",
        "Tweety is a bird",
      ],
      example_conclusion: "Tweety can fly",
      use_cases: [
        "First-order to modal translation",
        "Universal properties",
        "Domain constraints",
      ],
    ),
    DatasetTemplate(
      id: "folio_existential_01",
      name: "Existential Quantification as Possibility",
      dataset: FOLIO,
      template_type: FOLIOExistential,
      description: "Translate existential quantifiers to modal possibility",
      logic_system: T,
      pattern_description: "∃x P(x) becomes ◇P(x) - existence as possibility",
      example_premises: [
        "Some mammals lay eggs",
      ],
      example_conclusion: "Platypuses exist",
      use_cases: [
        "Existential reasoning",
        "Possibility modeling",
        "Domain exploration",
      ],
    ),
    DatasetTemplate(
      id: "folio_conditional_01",
      name: "Conditional Reasoning",
      dataset: FOLIO,
      template_type: FOLIOConditional,
      description: "Model if-then relationships in FOLIO",
      logic_system: K,
      pattern_description: "If P then Q, P is true, therefore Q",
      example_premises: [
        "If it rains, the ground is wet",
        "It is raining",
      ],
      example_conclusion: "The ground is wet",
      use_cases: [
        "Conditional inference",
        "Modus ponens",
        "Causal reasoning",
      ],
    ),
  ]
}

// =============================================================================
// LogiQA Templates
// =============================================================================

/// Get all LogiQA templates
pub fn logiqa_templates() -> List(DatasetTemplate) {
  [
    DatasetTemplate(
      id: "logiqa_multihop_01",
      name: "Multi-Hop Inference Chain",
      dataset: LogiQA,
      template_type: LogiQAMultiHop,
      description: "Model multi-step reasoning chains in LogiQA",
      logic_system: S4,
      pattern_description: "Chain implications through multiple steps with transitivity",
      example_premises: [
        "A implies B",
        "B implies C",
        "C implies D",
      ],
      example_conclusion: "A implies D",
      use_cases: [
        "Multi-hop reasoning",
        "Transitive inference",
        "Chain reasoning",
      ],
    ),
    DatasetTemplate(
      id: "logiqa_chain_01",
      name: "Reasoning Chain with Knowledge",
      dataset: LogiQA,
      template_type: LogiQAChainReasoning,
      description: "Model knowledge propagation through reasoning chains",
      logic_system: S4,
      pattern_description: "If agent knows P and knows P→Q, they know Q",
      example_premises: [
        "Agent knows P",
        "Agent knows P implies Q",
      ],
      example_conclusion: "Agent knows Q",
      use_cases: [
        "Knowledge closure",
        "Epistemic reasoning",
        "Logical omniscience",
      ],
    ),
    DatasetTemplate(
      id: "logiqa_constraint_01",
      name: "Constraint Satisfaction",
      dataset: LogiQA,
      template_type: LogiQAConstraint,
      description: "Model constraint satisfaction problems",
      logic_system: KD,
      pattern_description: "Model obligations and constraints as deontic operators",
      example_premises: [
        "It is obligatory that P",
        "It is obligatory that Q",
      ],
      example_conclusion: "It is obligatory that P and Q",
      use_cases: [
        "Constraint problems",
        "Obligation composition",
        "Normative reasoning",
      ],
    ),
    DatasetTemplate(
      id: "logiqa_comparison_01",
      name: "Comparative Reasoning",
      dataset: LogiQA,
      template_type: LogiQAComparison,
      description: "Model comparative statements and orderings",
      logic_system: S5,
      pattern_description: "Model comparisons with modal operators",
      example_premises: [
        "A is better than B",
        "B is better than C",
      ],
      example_conclusion: "A is better than C",
      use_cases: [
        "Comparative logic",
        "Ordering relations",
        "Preference reasoning",
      ],
    ),
  ]
}

// =============================================================================
// InPhO Templates
// =============================================================================

/// Get all InPhO templates
pub fn inpho_templates() -> List(DatasetTemplate) {
  [
    DatasetTemplate(
      id: "inpho_philosophical_01",
      name: "Philosophical Argument Structure",
      dataset: InPhO,
      template_type: InPhOPhilosophicalArgument,
      description: "Model standard philosophical argument patterns",
      logic_system: S5,
      pattern_description: "Premise 1, Premise 2, ... ⊢ Conclusion with modal operators",
      example_premises: [
        "Necessarily, if something exists, it has properties",
        "God exists",
      ],
      example_conclusion: "God has properties",
      use_cases: [
        "Philosophical arguments",
        "Metaphysical reasoning",
        "Ontological arguments",
      ],
    ),
    DatasetTemplate(
      id: "inpho_conceptual_01",
      name: "Conceptual Analysis",
      dataset: InPhO,
      template_type: InPhOConceptualAnalysis,
      description: "Model conceptual analysis and definitions",
      logic_system: S5,
      pattern_description: "Define concepts through necessary and sufficient conditions",
      example_premises: [
        "Necessarily, knowledge is justified true belief",
        "Smith has justified true belief that P",
      ],
      example_conclusion: "Smith knows P",
      use_cases: [
        "Conceptual analysis",
        "Definition examination",
        "Gettier problems",
      ],
    ),
    DatasetTemplate(
      id: "inpho_metaphysical_01",
      name: "Metaphysical Necessity",
      dataset: InPhO,
      template_type: InPhOMetaphysical,
      description: "Model metaphysical necessity and possibility",
      logic_system: S5,
      pattern_description: "Use S5 for metaphysical modality",
      example_premises: [
        "Necessarily, water is H2O",
        "This liquid is water",
      ],
      example_conclusion: "This liquid is H2O",
      use_cases: [
        "Metaphysical arguments",
        "Necessity a posteriori",
        "Essential properties",
      ],
    ),
    DatasetTemplate(
      id: "inpho_epistemic_01",
      name: "Epistemic Philosophical Reasoning",
      dataset: InPhO,
      template_type: InPhOEpistemic,
      description: "Model knowledge and justification in philosophy",
      logic_system: S5,
      pattern_description: "Use epistemic operators for knowledge claims",
      example_premises: [
        "If I know P, then P is true",
        "I know that I think",
      ],
      example_conclusion: "I think (cogito)",
      use_cases: [
        "Epistemic philosophy",
        "Cartesian arguments",
        "Foundationalism",
      ],
    ),
  ]
}

// =============================================================================
// Template Operations
// =============================================================================

/// Get all templates across all datasets
pub fn all_templates() -> List(DatasetTemplate) {
  list.flatten([
    folio_templates(),
    logiqa_templates(),
    inpho_templates(),
  ])
}

/// Get templates by dataset
pub fn by_dataset(dataset: Dataset) -> List(DatasetTemplate) {
  all_templates()
  |> list.filter(fn(t) { t.dataset == dataset })
}

/// Get template by ID
pub fn by_id(id: String) -> Option(DatasetTemplate) {
  all_templates()
  |> list.find(fn(t) { t.id == id })
  |> option.from_result
}

/// Get templates by logic system
pub fn by_logic_system(system: LogicSystem) -> List(DatasetTemplate) {
  all_templates()
  |> list.filter(fn(t) { t.logic_system == system })
}

/// Search templates by keyword
pub fn search(query: String) -> List(DatasetTemplate) {
  let lower_query = string.lowercase(query)

  all_templates()
  |> list.filter(fn(t) {
    string.contains(string.lowercase(t.name), lower_query)
    || string.contains(string.lowercase(t.description), lower_query)
    || string.contains(string.lowercase(t.pattern_description), lower_query)
  })
}

/// Get template count by dataset
pub fn count_by_dataset() -> List(#(Dataset, Int)) {
  [
    #(FOLIO, list.length(folio_templates())),
    #(LogiQA, list.length(logiqa_templates())),
    #(InPhO, list.length(inpho_templates())),
  ]
}

/// Total template count
pub fn count() -> Int {
  list.length(all_templates())
}

// =============================================================================
// Template Application
// =============================================================================

/// Apply a template to create propositions
/// This is a simplified version - full implementation would parse actual data
pub fn apply_template(
  template: DatasetTemplate,
  premises: List(String),
  conclusion: String,
) -> TemplateApplication {
  // Simplified: Convert strings to atomic propositions
  let prop_premises =
    premises
    |> list.index_map(fn(p, i) {
      Atom("premise_" <> string.inspect(i) <> "_" <> string.slice(p, 0, 20))
    })

  let prop_conclusion = Atom("conclusion_" <> string.slice(conclusion, 0, 20))

  TemplateApplication(
    template: template,
    applied_premises: prop_premises,
    applied_conclusion: prop_conclusion,
    confidence: 0.8,
    notes: ["Simplified template application", "Full parsing to be implemented"],
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Convert dataset to string
pub fn dataset_to_string(dataset: Dataset) -> String {
  case dataset {
    FOLIO -> "folio"
    LogiQA -> "logiqa"
    InPhO -> "inpho"
  }
}

/// Parse dataset from string
pub fn string_to_dataset(s: String) -> Option(Dataset) {
  case string.lowercase(s) {
    "folio" -> Some(FOLIO)
    "logiqa" -> Some(LogiQA)
    "inpho" -> Some(InPhO)
    _ -> None
  }
}

/// Convert template type to string
pub fn template_type_to_string(template_type: TemplateType) -> String {
  case template_type {
    FOLIOBelief -> "belief"
    FOLIOUniversal -> "universal"
    FOLIOExistential -> "existential"
    FOLIOConditional -> "conditional"
    LogiQAMultiHop -> "multihop"
    LogiQAChainReasoning -> "chain"
    LogiQAConstraint -> "constraint"
    LogiQAComparison -> "comparison"
    InPhOPhilosophicalArgument -> "philosophical"
    InPhOConceptualAnalysis -> "conceptual"
    InPhOMetaphysical -> "metaphysical"
    InPhOEpistemic -> "epistemic"
  }
}

/// Get template description with examples
pub fn format_template(template: DatasetTemplate) -> String {
  let header = "Template: " <> template.name <> " (" <> template.id <> ")\n"

  let dataset_info = "Dataset: " <> dataset_to_string(template.dataset) <> "\n"

  let system_info =
    "Logic System: " <> string.inspect(template.logic_system) <> "\n"

  let desc = "Description: " <> template.description <> "\n"

  let pattern = "Pattern: " <> template.pattern_description <> "\n"

  let examples =
    "\nExample:\n"
    <> "Premises:\n"
    <> {
      template.example_premises
      |> list.map(fn(p) { "  - " <> p })
      |> string.join("\n")
    }
    <> "\nConclusion: "
    <> template.example_conclusion

  header <> dataset_info <> system_info <> desc <> pattern <> examples
}
