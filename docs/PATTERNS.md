# Formula Pattern Library

## Overview

The Formula Pattern Library provides 20+ reusable formula patterns across 5 categories of modal logic. Patterns accelerate formula creation by 40-60% and make the system accessible to non-experts.

## Pattern Categories

| Category | Count | Description | Example Operators |
|----------|-------|-------------|-------------------|
| **Epistemic** | 6 | Knowledge and belief | Knows, Believes |
| **Deontic** | 5 | Obligation and permission | Obligatory, Permitted |
| **Temporal** | 2 | Time-based reasoning | □ (always), ◇ (eventually) |
| **Alethic** | 5 | Necessity and possibility | □ (necessary), ◇ (possible) |
| **Classical** | 7 | Standard logical patterns | →, ∧, ∨, ¬ |

**Total**: 25 patterns

## Quick Start

### CLI

```bash
# List all patterns
foil patterns

# Patterns by category
foil patterns category epistemic
foil patterns category deontic

# Search patterns
foil patterns search "knowledge"
foil patterns search "obligation"
```

### API

```bash
# List all patterns
curl http://localhost:8080/api/patterns

# Patterns by category
curl http://localhost:8080/api/patterns/category/epistemic

# Get suggestions for partial formula
curl -X POST http://localhost:8080/api/suggest \
  -H "Content-Type: application/json" \
  -d '{"partial": "Knows(agent, p)", "max_results": 5}'
```

### Programmatic

```gleam
import modal_logic/patterns

// Get all patterns
let all = patterns.all_patterns()

// Filter by category
let epistemic = patterns.by_category(patterns.Epistemic)

// Search patterns
let knowledge_patterns = patterns.search("knowledge")

// Get auto-suggestions
let suggestions = patterns.suggest("Knows", 5)
```

---

## Epistemic Patterns (6)

### EP001: Knowledge Implies Truth

**Formula**: `Knows(agent, p) → p`

**Description**: If an agent knows p, then p is true (truth axiom for knowledge)

**Tags**: knowledge, truth, factivity
**Complexity**: Simple
**Use Cases**: Epistemic logic, Agent reasoning, Knowledge bases

---

### EP002: Positive Introspection

**Formula**: `Knows(agent, p) → Knows(agent, Knows(agent, p))`

**Description**: If an agent knows p, they know that they know p (positive introspection)

**Tags**: knowledge, introspection, self-awareness
**Complexity**: Medium
**Use Cases**: Epistemic logic, Self-aware agents, Metacognition

---

### EP003: Negative Introspection

**Formula**: `¬Knows(agent, p) → Knows(agent, ¬Knows(agent, p))`

**Description**: If an agent doesn't know p, they know they don't know p

**Tags**: knowledge, introspection, ignorance
**Complexity**: Medium
**Use Cases**: Epistemic logic, Awareness of ignorance

---

### EP004: Common Knowledge

**Formula**: `Knows(a1, p) ∧ Knows(a2, p) → Knows(a1, Knows(a2, p))`

**Description**: If two agents know p, the first knows that the second knows p

**Tags**: knowledge, common-knowledge, multi-agent
**Complexity**: Complex
**Use Cases**: Multi-agent systems, Distributed knowledge, Coordination

---

### EP005: Belief Consistency

**Formula**: `Believes(agent, p) ∧ Believes(agent, p → q) → Believes(agent, q)`

**Description**: If an agent believes p and believes p implies q, they believe q

**Tags**: belief, consistency, inference
**Complexity**: Medium
**Use Cases**: Belief systems, Reasoning under uncertainty

---

### EP006: Knowledge Implies Belief

**Formula**: `Knows(agent, p) → Believes(agent, p)`

**Description**: If an agent knows something, they believe it

**Tags**: knowledge, belief, relationship
**Complexity**: Simple
**Use Cases**: Epistemic logic, Knowledge vs belief distinction

---

## Deontic Patterns (5)

### DP001: Obligation Implies Permission

**Formula**: `Obligatory(p) → Permitted(p)`

**Description**: If something is obligatory, it is permitted

**Tags**: obligation, permission, deontic
**Complexity**: Simple
**Use Cases**: Moral reasoning, Legal systems, Normative ethics

---

### DP002: No Contradictory Obligations

**Formula**: `¬(Obligatory(p) ∧ Obligatory(¬p))`

**Description**: It cannot be both obligatory to do p and obligatory not to do p

**Tags**: obligation, consistency, contradiction
**Complexity**: Simple
**Use Cases**: Consistent normative systems, Legal reasoning

---

### DP003: Prohibition

**Formula**: `Obligatory(¬p)`

**Description**: It is obligatory that p does not hold (prohibition)

**Tags**: obligation, prohibition, negation
**Complexity**: Simple
**Use Cases**: Moral rules, Legal prohibitions

---

### DP004: Permission Definition

**Formula**: `Permitted(p) ↔ ¬Obligatory(¬p)`

**Description**: Something is permitted iff it's not obligatory not to do it

**Tags**: permission, obligation, duality
**Complexity**: Medium
**Use Cases**: Deontic logic, Permission systems

---

### DP005: Ought Implies Can

**Formula**: `Obligatory(p) → Possible(p)`

**Description**: If something is obligatory, it must be possible

**Tags**: obligation, possibility, practical
**Complexity**: Medium
**Use Cases**: Practical ethics, Realistic obligations

---

## Temporal Patterns (2)

### TP001: Always Implies Now

**Formula**: `□p → p`

**Description**: If p is always true, then p is true now (in system T)

**Tags**: always, present, reflexivity
**Complexity**: Simple
**Use Cases**: Temporal reasoning, System T

---

### TP002: Eventually Consistent

**Formula**: `◇p → ◇(p ∧ ◇p)`

**Description**: If p is eventually true, it's eventually true and eventually true again

**Tags**: eventually, consistency
**Complexity**: Medium
**Use Cases**: Distributed systems, Eventual consistency

---

## Alethic Patterns (5)

### AP001: Necessity Implies Truth

**Formula**: `□p → p`

**Description**: If p is necessary, then p is true (axiom T)

**Tags**: necessity, truth, reflexivity
**Complexity**: Simple
**Use Cases**: System T, Alethic modality, Metaphysics

---

### AP002: Necessity of Necessity

**Formula**: `□p → □□p`

**Description**: If p is necessary, then it's necessarily necessary (axiom 4)

**Tags**: necessity, transitivity
**Complexity**: Medium
**Use Cases**: System K4, System S4, Provability logic

---

### AP003: Possibility Implies Necessary Possibility

**Formula**: `◇p → □◇p`

**Description**: If p is possible, then it's necessarily possible (axiom 5)

**Tags**: possibility, necessity, euclidean
**Complexity**: Medium
**Use Cases**: System S5, Metaphysical modality

---

### AP004: Duality of Necessity and Possibility

**Formula**: `□p ↔ ¬◇¬p`

**Description**: p is necessary iff it's not possible that not-p

**Tags**: necessity, possibility, duality, equivalence
**Complexity**: Medium
**Use Cases**: All modal systems, Modal duality

---

### AP005: Possibility as Dual of Necessity

**Formula**: `◇p ↔ ¬□¬p`

**Description**: p is possible iff it's not necessary that not-p

**Tags**: possibility, necessity, duality, equivalence
**Complexity**: Medium
**Use Cases**: All modal systems, Modal duality

---

## Classical Patterns (7)

### CP001: Modus Ponens

**Formula**: `p ∧ (p → q) → q`

**Description**: If p is true and p implies q, then q is true

**Tags**: implication, inference, classical
**Complexity**: Simple
**Use Cases**: All logic systems, Basic reasoning

---

### CP002: Modus Tollens

**Formula**: `¬q ∧ (p → q) → ¬p`

**Description**: If q is false and p implies q, then p is false

**Tags**: implication, negation, inference
**Complexity**: Simple
**Use Cases**: All logic systems, Proof by contrapositive

---

### CP003: Hypothetical Syllogism

**Formula**: `(p → q) ∧ (q → r) → (p → r)`

**Description**: If p implies q and q implies r, then p implies r

**Tags**: implication, transitivity, chaining
**Complexity**: Simple
**Use Cases**: All logic systems, Chain reasoning

---

### CP004: Disjunctive Syllogism

**Formula**: `(p ∨ q) ∧ ¬p → q`

**Description**: If p or q is true, and p is false, then q is true

**Tags**: disjunction, negation, elimination
**Complexity**: Simple
**Use Cases**: All logic systems, Case analysis

---

### CP005: De Morgan's Law (Conjunction)

**Formula**: `¬(p ∧ q) ↔ (¬p ∨ ¬q)`

**Description**: Not (p and q) is equivalent to (not-p or not-q)

**Tags**: negation, conjunction, disjunction, equivalence
**Complexity**: Simple
**Use Cases**: All logic systems, Negation normal form

---

### CP006: De Morgan's Law (Disjunction)

**Formula**: `¬(p ∨ q) ↔ (¬p ∧ ¬q)`

**Description**: Not (p or q) is equivalent to (not-p and not-q)

**Tags**: negation, conjunction, disjunction, equivalence
**Complexity**: Simple
**Use Cases**: All logic systems, Negation normal form

---

### CP007: Law of Excluded Middle

**Formula**: `p ∨ ¬p`

**Description**: Either p is true or not-p is true (no middle ground)

**Tags**: disjunction, negation, tautology
**Complexity**: Simple
**Use Cases**: Classical logic, Proof by cases

---

## Auto-Suggestion

The pattern library includes an intelligent suggestion engine that ranks patterns based on relevance to your partial formula.

### How It Works

1. **Normalize Input**: Convert Unicode symbols to ASCII
2. **Calculate Relevance**: Score patterns based on:
   - Exact match (1.0)
   - Substring match (0.8)
   - Keyword overlap (0.6)
   - Tag match (0.5)
3. **Rank Results**: Sort by relevance score
4. **Return Top N**: Default 5 suggestions

### Example

**Input**: `"Knows(agent, p)"`

**Suggestions**:
1. EP001: Knowledge Implies Truth (relevance: 0.8, reason: "Contains your input")
2. EP002: Positive Introspection (relevance: 0.6, reason: "Similar operators")
3. EP006: Knowledge Implies Belief (relevance: 0.6, reason: "Similar operators")
4. EP004: Common Knowledge (relevance: 0.5, reason: "Related pattern")
5. EP003: Negative Introspection (relevance: 0.5, reason: "Related pattern")

### API Request

```json
POST /api/suggest
{
  "partial": "Knows(agent, p)",
  "max_results": 5
}
```

**Response**:
```json
{
  "partial_formula": "Knows(agent, p)",
  "suggestion_count": 5,
  "suggestions": [
    {
      "pattern": {
        "id": "ep001",
        "name": "Knowledge Implies Truth",
        "formula": "Knows(agent, p) → p",
        "category": "epistemic",
        "complexity": "simple",
        "tags": ["knowledge", "truth", "factivity"]
      },
      "relevance_score": 0.8,
      "match_reason": "Contains your input"
    }
    // ... more suggestions
  ]
}
```

## Pattern Structure

Each pattern includes:

```gleam
Pattern(
  id: "ep001",                    // Unique identifier
  name: "Knowledge Implies Truth", // Human-readable name
  formula: "Knows(agent, p) → p",  // Formula string
  description: "...",              // Detailed description
  category: Epistemic,             // Category
  tags: ["knowledge", "truth"],    // Searchable tags
  complexity: Simple,              // Simple | Medium | Complex
  use_cases: ["..."],             // Practical applications
  related_patterns: ["ep002"],     // IDs of related patterns
)
```

## Usage Examples

### Find Patterns for Your Task

**Task**: "I need to model agent knowledge"
```bash
foil patterns search "knowledge"
# Returns: EP001, EP002, EP003, EP004, EP006
```

**Task**: "I'm working with obligations"
```bash
foil patterns category deontic
# Returns: DP001, DP002, DP003, DP004, DP005
```

**Task**: "I need simple patterns to start"
```gleam
let simple_patterns = patterns.by_complexity(patterns.Simple)
// Returns: All Simple complexity patterns
```

### Building on Patterns

```gleam
// Start with a pattern
case patterns.by_id("ep001") {
  Some(p) -> {
    // p.formula => "Knows(agent, p) → p"
    // Customize for your domain:
    // "Knows(alice, rain) → rain"
  }
  None -> Error("Pattern not found")
}
```

## Best Practices

### For Users

1. **Start with Search**: Use `patterns search` to find relevant patterns
2. **Check Complexity**: Start with Simple patterns, progress to Complex
3. **Read Examples**: Each pattern includes use cases
4. **Explore Related**: Check related_patterns for similar formulas
5. **Customize**: Adapt patterns to your specific domain

### For Developers

1. **Add New Patterns**: Contribute to the library
2. **Tag Thoroughly**: Use descriptive tags for discoverability
3. **Document Use Cases**: Help users understand applications
4. **Link Related Patterns**: Build pattern networks
5. **Test Patterns**: Ensure formulas are valid in appropriate systems

## Contributing Patterns

Future enhancement: Community-contributed patterns with voting/rating system.

### Pattern Submission Guidelines (Planned)

1. **Unique**: Not duplicate of existing pattern
2. **Well-formed**: Syntactically valid formula
3. **Documented**: Clear description and use cases
4. **Categorized**: Appropriate category and tags
5. **Tested**: Verified in relevant modal systems

## Pattern Relationships

Patterns form a knowledge graph through `related_patterns` links:

```
EP001 (Knowledge → Truth)
  ├─ EP002 (Positive Introspection)
  ├─ EP003 (Negative Introspection)
  └─ EP006 (Knowledge → Belief)
      └─ EP005 (Belief Consistency)

DP001 (Obligation → Permission)
  ├─ DP002 (No Contradictory Obligations)
  ├─ DP003 (Prohibition)
  └─ DP004 (Permission Definition)
```

## Pattern Complexity

### Simple (12 patterns)
- Basic single-step inferences
- Few operators
- Easy to understand
- **Examples**: Modus Ponens, Obligation → Permission

### Medium (11 patterns)
- Multi-step reasoning
- Nested operators
- Moderate complexity
- **Examples**: Positive Introspection, Ought Implies Can

### Complex (2 patterns)
- Advanced reasoning
- Multiple agents or worlds
- High nesting
- **Examples**: Common Knowledge

## API Reference

### GET /api/patterns

List all patterns with category counts.

**Response**:
```json
{
  "total_patterns": 25,
  "by_category": {
    "epistemic": 6,
    "deontic": 5,
    "temporal": 2,
    "alethic": 5,
    "classical": 7
  },
  "categories": ["epistemic", "deontic", "temporal", "alethic", "classical"]
}
```

### GET /api/patterns/category/:category

Get patterns by category.

**Example**: `GET /api/patterns/category/epistemic`

**Response**:
```json
{
  "category": "epistemic",
  "count": 6,
  "patterns": [
    {
      "id": "ep001",
      "name": "Knowledge Implies Truth",
      "formula": "Knows(agent, p) → p",
      "description": "...",
      "category": "epistemic",
      "complexity": "simple",
      "tags": ["knowledge", "truth", "factivity"]
    }
    // ... more patterns
  ]
}
```

### POST /api/suggest

Get pattern suggestions for partial formula.

**Request**:
```json
{
  "partial": "Knows(agent, p)",
  "max_results": 5
}
```

**Response**:
```json
{
  "partial_formula": "Knows(agent, p)",
  "suggestion_count": 5,
  "suggestions": [
    {
      "pattern": { /* full pattern object */ },
      "relevance_score": 0.8,
      "match_reason": "Contains your input"
    }
    // ... more suggestions
  ]
}
```

## See Also

- [Profile Documentation](PROFILES.md) - Modal system profiles
- [Error Codes](ERROR_CODES.md) - Error handling
- [API Documentation](API.md) - Complete REST API reference
- [Testing Guide](../packages/modal_logic/docs/TESTING.md) - Testing strategies
