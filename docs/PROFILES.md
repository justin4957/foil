# Modal System Profiles

## Overview

Modal system profiles provide pre-configured settings for the 7 supported modal logic systems in Foil. Profiles simplify usage by eliminating manual configuration and reducing setup errors.

## Available Profiles

| Profile | Name | Description | Frame Properties | Use Cases |
|---------|------|-------------|------------------|-----------|
| `k` | K | Basic modal logic | None | Basic modal reasoning |
| `t` | T | Reflexive modal logic | Reflexive | Alethic necessity |
| `k4` | K4 | Transitive modal logic | Transitive | Provability logic |
| `s4` | S4 | Reflexive + transitive | Reflexive, Transitive | Epistemic logic, knowledge |
| `s5` | S5 | Equivalence relation | Reflexive, Symmetric, Transitive | Metaphysical necessity |
| `kd` | KD | Deontic logic | Serial | Obligation, permission |
| `kd45` | KD45 | Deontic S5 | Serial, Transitive, Euclidean | Advanced deontic reasoning |

## Usage

### API

#### List All Profiles

```bash
GET /api/profiles
```

Response:
```json
{
  "profiles": ["k", "t", "k4", "s4", "s5", "kd", "kd45"],
  "count": 7
}
```

#### Get Specific Profile

```bash
GET /api/profiles/s5
```

Response:
```json
{
  "name": "S5",
  "description": "Modal logic with equivalence relation",
  "axioms": ["K", "T", "4", "5"],
  "frame_properties": ["reflexive", "symmetric", "transitive"],
  "default_timeout_ms": 60000,
  "verification_strategy": "standard_translation",
  "examples": [],
  "use_cases": ["Metaphysical necessity"]
}
```

### CLI

#### List All Profiles

```bash
foil profiles
```

#### Get Profile Information

```bash
foil profile s5
```

#### Use Profile in Analysis

```bash
foil analyze "If it is necessary that p, then p" --profile t
```

The `--profile` flag automatically configures the system with the appropriate modal logic settings.

### Programmatic Usage

```gleam
import modal_logic/profile

// List all available profiles
let all_profiles = profile.list_all()
// => ["k", "t", "k4", "s4", "s5", "kd", "kd45"]

// Load a specific profile
case profile.load("s5") {
  Ok(s5_profile) -> {
    // Use profile configuration
    s5_profile.name  // "S5"
    s5_profile.axioms  // ["K", "T", "4", "5"]
    s5_profile.frame_properties  // ["reflexive", "symmetric", "transitive"]
  }
  Error(err) -> {
    // Handle error
    let message = profile.format_error(err)
  }
}

// Check if profile exists
profile.exists("s5")  // True
profile.exists("xyz")  // False

// Get profile info
case profile.get_info("kd") {
  Ok(description) -> io.println(description)
  Error(_) -> io.println("Profile not found")
}
```

## Profile Details

### K - Basic Modal Logic

**Axioms**: K (distribution axiom)
**Frame Properties**: None
**Description**: Minimal modal logic with no constraints on accessibility relations

**Example Valid Formula**:
```
□(p → q) → (□p → □q)
```

**Use Cases**:
- Basic modal reasoning
- Foundation for other modal systems
- When no specific frame properties are assumed

---

### T - Reflexive Modal Logic

**Axioms**: K, T (reflexivity axiom)
**Frame Properties**: Reflexive
**Description**: Adds reflexivity - what is necessary is actual

**Example Valid Formula**:
```
□p → p
```

**Use Cases**:
- Alethic necessity (truth in the actual world)
- Logical necessity
- Normative reasoning

---

### K4 - Transitive Modal Logic

**Axioms**: K, 4 (transitivity axiom)
**Frame Properties**: Transitive
**Description**: Adds transitivity - necessity of necessity implies necessity

**Example Valid Formula**:
```
□p → □□p
```

**Use Cases**:
- Provability logic
- Temporal reasoning
- Knowledge without reflexivity

---

### S4 - Reflexive + Transitive

**Axioms**: K, T, 4
**Frame Properties**: Reflexive, Transitive
**Description**: Combines reflexivity and transitivity

**Example Valid Formulas**:
```
(□p → p) ∧ (□p → □□p)
```

**Use Cases**:
- Epistemic logic (knowledge)
- Provability with reflexivity
- Temporal logic with past and future
- Intuitionistic modal logic

---

### S5 - Equivalence Relation

**Axioms**: K, T, 4, 5 (Euclidean axiom)
**Frame Properties**: Reflexive, Symmetric, Transitive
**Description**: Equivalence relation frames - strongest common modal system

**Example Valid Formulas**:
```
◇p → □◇p
¬□p → □¬□p
```

**Use Cases**:
- Metaphysical necessity
- Perfect knowledge (omniscience)
- Logical necessity and possibility
- Philosophical modal reasoning

---

### KD - Deontic Logic

**Axioms**: K, D (seriality axiom)
**Frame Properties**: Serial
**Description**: Serial frames ensure consistency - no contradictory obligations

**Example Valid Formulas**:
```
□p → ◇p
Obligatory(p) → Permitted(p)
¬(Obligatory(p) ∧ Obligatory(¬p))
```

**Use Cases**:
- Deontic logic (obligation and permission)
- Moral reasoning
- Legal reasoning
- Normative systems

---

### KD45 - Deontic S5

**Axioms**: K, D, 4, 5
**Frame Properties**: Serial, Transitive, Euclidean
**Description**: Deontic logic with introspection

**Example Valid Formulas**:
```
Obligatory(p) → Obligatory(Obligatory(p))
¬Obligatory(p) → Obligatory(¬Obligatory(p))
```

**Use Cases**:
- Advanced deontic reasoning
- Consistent normative systems with introspection
- Moral philosophy with self-awareness
- Legal systems with precedent

---

## Error Handling

Profiles handle errors gracefully with informative messages:

```gleam
case profile.load("nonexistent") {
  Error(profile.ProfileNotFound(name)) -> {
    // "Profile 'nonexistent' not found. Available profiles: k, t, k4, s4, s5, kd, kd45"
    let message = profile.format_error(err)
  }
  Error(profile.ProfileIOError(name, details)) -> {
    // File system error
  }
  Error(profile.ProfileParseError(name, details)) -> {
    // JSON parsing error
  }
  Ok(profile) -> // Success
}
```

## Implementation Notes

### Current Implementation

The current implementation uses a simplified approach where profile data is hard-coded in the profile module. This ensures:
- Fast load times (no file I/O)
- No external dependencies
- Type-safe configuration
- Easy testing

### Future Enhancements

Future versions may include:
- Full JSON parsing from profile files
- User-defined custom profiles
- Profile inheritance (e.g., S4 extends T)
- Profile validation against schema
- Profile export/import
- Community-shared profiles

##Human: continue, commit changes and create pr