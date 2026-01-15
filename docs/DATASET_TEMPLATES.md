# Dataset-Specific Modal Templates

## Overview

Dataset-specific templates provide pre-built modal logic schemas for external datasets (FOLIO, LogiQA, InPhO). Templates enable immediate application of modal analysis to domain-specific problems without manual formalization.

## Available Datasets

| Dataset | Templates | Description | Primary Use |
|---------|-----------|-------------|-------------|
| **FOLIO** | 4 | First-Order Logic Inference | Logical reasoning, FOL translation |
| **LogiQA** | 4 | Logical Question Answering | Multi-hop reasoning, chain inference |
| **InPhO** | 4 | Internet Philosophy Ontology | Philosophical arguments, conceptual analysis |

**Total**: 12 templates across 3 datasets

## Quick Start

### CLI

```bash
# List all dataset templates
foil datasets

# Templates for specific dataset
foil datasets folio
foil datasets logiqa
foil datasets inpho
```

### API

```bash
# List all templates
curl http://localhost:8080/api/datasets/templates

# Templates for specific dataset
curl http://localhost:8080/api/datasets/folio/templates
curl http://localhost:8080/api/datasets/logiqa/templates
curl http://localhost:8080/api/datasets/inpho/templates
```

### Programmatic

```gleam
import modal_logic/dataset_templates

// Get all templates
let all = dataset_templates.all_templates()

// Templates by dataset
let folio = dataset_templates.by_dataset(dataset_templates.FOLIO)

// Search templates
let belief_templates = dataset_templates.search("belief")
```

---

## FOLIO Templates (4)

### folio_belief_01: Belief Propagation

**Logic System**: S5
**Pattern**: If agent believes all premises, they believe the conclusion

**Example**:
- Premises:
  - "All humans are mortal"
  - "Socrates is a human"
- Conclusion: "Socrates is mortal"

**Use Cases**:
- Logical reasoning with beliefs
- Agent knowledge modeling
- Belief consistency checking

---

### folio_universal_01: Universal Quantification as Necessity

**Logic System**: T
**Pattern**: ∀x P(x) becomes □P(x) - universal truth as necessity

**Example**:
- Premises:
  - "All birds can fly"
  - "Tweety is a bird"
- Conclusion: "Tweety can fly"

**Use Cases**:
- First-order to modal translation
- Universal properties
- Domain constraints

---

### folio_existential_01: Existential Quantification as Possibility

**Logic System**: T
**Pattern**: ∃x P(x) becomes ◇P(x) - existence as possibility

**Example**:
- Premises:
  - "Some mammals lay eggs"
- Conclusion: "Platypuses exist"

**Use Cases**:
- Existential reasoning
- Possibility modeling
- Domain exploration

---

### folio_conditional_01: Conditional Reasoning

**Logic System**: K
**Pattern**: If P then Q, P is true, therefore Q

**Example**:
- Premises:
  - "If it rains, the ground is wet"
  - "It is raining"
- Conclusion: "The ground is wet"

**Use Cases**:
- Conditional inference
- Modus ponens
- Causal reasoning

---

## LogiQA Templates (4)

### logiqa_multihop_01: Multi-Hop Inference Chain

**Logic System**: S4
**Pattern**: Chain implications through multiple steps with transitivity

**Example**:
- Premises:
  - "A implies B"
  - "B implies C"
  - "C implies D"
- Conclusion: "A implies D"

**Use Cases**:
- Multi-hop reasoning
- Transitive inference
- Chain reasoning

---

### logiqa_chain_01: Reasoning Chain with Knowledge

**Logic System**: S4
**Pattern**: If agent knows P and knows P→Q, they know Q

**Example**:
- Premises:
  - "Agent knows P"
  - "Agent knows P implies Q"
- Conclusion: "Agent knows Q"

**Use Cases**:
- Knowledge closure
- Epistemic reasoning
- Logical omniscience

---

### logiqa_constraint_01: Constraint Satisfaction

**Logic System**: KD
**Pattern**: Model obligations and constraints as deontic operators

**Example**:
- Premises:
  - "It is obligatory that P"
  - "It is obligatory that Q"
- Conclusion: "It is obligatory that P and Q"

**Use Cases**:
- Constraint problems
- Obligation composition
- Normative reasoning

---

### logiqa_comparison_01: Comparative Reasoning

**Logic System**: S5
**Pattern**: Model comparisons with modal operators

**Example**:
- Premises:
  - "A is better than B"
  - "B is better than C"
- Conclusion: "A is better than C"

**Use Cases**:
- Comparative logic
- Ordering relations
- Preference reasoning

---

## InPhO Templates (4)

### inpho_philosophical_01: Philosophical Argument Structure

**Logic System**: S5
**Pattern**: Premise 1, Premise 2, ... ⊢ Conclusion with modal operators

**Example**:
- Premises:
  - "Necessarily, if something exists, it has properties"
  - "God exists"
- Conclusion: "God has properties"

**Use Cases**:
- Philosophical arguments
- Metaphysical reasoning
- Ontological arguments

---

### inpho_conceptual_01: Conceptual Analysis

**Logic System**: S5
**Pattern**: Define concepts through necessary and sufficient conditions

**Example**:
- Premises:
  - "Necessarily, knowledge is justified true belief"
  - "Smith has justified true belief that P"
- Conclusion: "Smith knows P"

**Use Cases**:
- Conceptual analysis
- Definition examination
- Gettier problems

---

### inpho_metaphysical_01: Metaphysical Necessity

**Logic System**: S5
**Pattern**: Use S5 for metaphysical modality

**Example**:
- Premises:
  - "Necessarily, water is H2O"
  - "This liquid is water"
- Conclusion: "This liquid is H2O"

**Use Cases**:
- Metaphysical arguments
- Necessity a posteriori
- Essential properties

---

### inpho_epistemic_01: Epistemic Philosophical Reasoning

**Logic System**: S5
**Pattern**: Use epistemic operators for knowledge claims

**Example**:
- Premises:
  - "If I know P, then P is true"
  - "I know that I think"
- Conclusion: "I think (cogito)"

**Use Cases**:
- Epistemic philosophy
- Cartesian arguments
- Foundationalism

---

## API Reference

### GET /api/datasets/templates

List all dataset-specific templates.

**Response**:
```json
{
  "total_templates": 12,
  "by_dataset": {
    "folio": 4,
    "logiqa": 4,
    "inpho": 4
  },
  "datasets": ["folio", "logiqa", "inpho"]
}
```

### GET /api/datasets/:dataset/templates

Get templates for a specific dataset.

**Example**: `GET /api/datasets/folio/templates`

**Response**:
```json
{
  "dataset": "folio",
  "count": 4,
  "templates": [
    {
      "id": "folio_belief_01",
      "name": "Belief Propagation",
      "dataset": "folio",
      "template_type": "belief",
      "description": "Model agent beliefs and belief propagation in FOLIO examples",
      "logic_system": "S5",
      "pattern_description": "If agent believes all premises, they believe the conclusion"
    }
    // ... more templates
  ]
}
```

## Usage Examples

### Find Templates for Your Dataset

```gleam
import modal_logic/dataset_templates

// Get FOLIO templates
let folio = dataset_templates.by_dataset(dataset_templates.FOLIO)
// Returns: 4 FOLIO-specific templates

// Get LogiQA templates
let logiqa = dataset_templates.by_dataset(dataset_templates.LogiQA)
// Returns: 4 LogiQA-specific templates

// Search across all datasets
let belief_templates = dataset_templates.search("belief")
// Returns: Templates matching "belief"
```

### Apply Template to Data

```gleam
case dataset_templates.by_id("folio_belief_01") {
  Some(template) -> {
    let application = dataset_templates.apply_template(
      template,
      ["All humans are mortal", "Socrates is human"],
      "Socrates is mortal"
    )

    // application.applied_premises: List(Proposition)
    // application.applied_conclusion: Proposition
    // application.confidence: 0.8
  }
  None -> Error("Template not found")
}
```

### Format Template for Display

```gleam
case dataset_templates.by_id("logiqa_multihop_01") {
  Some(template) -> {
    let formatted = dataset_templates.format_template(template)
    io.println(formatted)
    // Outputs:
    // Template: Multi-Hop Inference Chain (logiqa_multihop_01)
    // Dataset: logiqa
    // Logic System: S4
    // Description: Model multi-step reasoning chains in LogiQA
    // Pattern: Chain implications through multiple steps with transitivity
    // Example:
    // Premises:
    //   - A implies B
    //   - B implies C
    //   - C implies D
    // Conclusion: A implies D
  }
  None -> Error("Not found")
}
```

## Integration with Existing Adapters

Dataset templates build on top of existing adapters:

```
dataset_templates.gleam (NEW)
    ↓
    Uses existing dataset integrations:
    ├─ testing/external/folio_adapter.gleam
    ├─ testing/external/logiqa_adapter.gleam
    └─ testing/external/inpho_adapter.gleam
```

**Benefits**:
- Leverage existing dataset loading code
- Add modal logic templates on top
- Maintain backward compatibility
- Enable domain-specific analysis

## Template Selection Guide

### For FOLIO Users

| Your Task | Template | Logic System |
|-----------|----------|--------------|
| Model agent beliefs | folio_belief_01 | S5 |
| Translate universal statements | folio_universal_01 | T |
| Model existence claims | folio_existential_01 | T |
| Conditional reasoning | folio_conditional_01 | K |

### For LogiQA Users

| Your Task | Template | Logic System |
|-----------|----------|--------------|
| Multi-step inference | logiqa_multihop_01 | S4 |
| Knowledge reasoning | logiqa_chain_01 | S4 |
| Constraint problems | logiqa_constraint_01 | KD |
| Comparative statements | logiqa_comparison_01 | S5 |

### For InPhO Users

| Your Task | Template | Logic System |
|-----------|----------|--------------|
| General philosophical arguments | inpho_philosophical_01 | S5 |
| Conceptual analysis | inpho_conceptual_01 | S5 |
| Metaphysical necessity | inpho_metaphysical_01 | S5 |
| Epistemic arguments | inpho_epistemic_01 | S5 |

## Template Structure

Each template includes:

```gleam
DatasetTemplate(
  id: "folio_belief_01",
  name: "Belief Propagation",
  dataset: FOLIO,
  template_type: FOLIOBelief,
  description: "...",
  logic_system: S5,
  pattern_description: "...",
  example_premises: ["...", "..."],
  example_conclusion: "...",
  use_cases: ["...", "..."],
)
```

## Future Enhancements

### Planned Features

1. **Template Composition**: Combine multiple templates
2. **Custom Templates**: User-defined templates
3. **Template Validation**: Ensure soundness
4. **Template Export**: Save as reusable JSON
5. **Community Templates**: Shared template repository

### Integration Roadmap

1. **Phase 1** (Current): 12 core templates
2. **Phase 2**: Integration with actual dataset files
3. **Phase 3**: Automated template generation from datasets
4. **Phase 4**: Community contribution system

## See Also

- [FOLIO Adapter](../packages/modal_logic/src/modal_logic/testing/external/folio_adapter.gleam) - FOLIO dataset integration
- [LogiQA Adapter](../packages/modal_logic/src/modal_logic/testing/external/logiqa_adapter.gleam) - LogiQA dataset integration
- [InPhO Adapter](../packages/modal_logic/src/modal_logic/testing/external/inpho_adapter.gleam) - InPhO dataset integration
- [Patterns Library](PATTERNS.md) - General formula patterns
- [Profiles Documentation](PROFILES.md) - Modal system profiles
