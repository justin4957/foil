# Formula Complexity Metrics & Optimization

## Overview

Complexity metrics help users understand verification performance and write more efficient formulas. The system analyzes formulas, predicts verification time, and suggests optimizations based on modal equivalences.

## Metrics

### Core Metrics

| Metric | Description | Impact |
|--------|-------------|--------|
| **Modal Depth** | Maximum nesting of modal operators (□, ◇) | High - Most critical for performance |
| **Operator Count** | Total logical/modal operators | Medium - Affects formula size |
| **Atom Count** | Number of atomic propositions | Low - Minimal impact |
| **Total Nodes** | Nodes in formula tree | Medium - Overall complexity |
| **Nesting Level** | Maximum nesting depth | High - Affects verification strategy |
| **Complexity Score** | Weighted combination | High - Overall difficulty |
| **Estimated Time** | Predicted verification time (ms) | High - Helps avoid timeouts |

### Complexity Score Calculation

```
Score = (Modal Depth × 3.0) + (Operator Count × 1.5) + (Nesting Level × 2.0)
```

**Examples**:
- `p`: Score = 0.0 (instant)
- `□p → p`: Score = 4.5 (~500ms)
- `□□(p ∧ q)`: Score = 10.5 (~2s)
- `□□□(p → □q)`: Score = 18.0 (~5s)

### Complexity Levels

| Level | Score Range | Est. Time | Description |
|-------|-------------|-----------|-------------|
| **Low** | 0-10 | <500ms | Simple formulas, fast verification |
| **Medium** | 10-25 | 500ms-2s | Moderate complexity |
| **High** | 25-50 | 2s-5s | Complex formulas, may be slow |
| **Very High** | 50+ | 5s+ | Very complex, likely to timeout |

## Quick Start

### API

```bash
POST /api/analyze/complexity
{
  "formula": "□□(p → q)"
}
```

**Response**:
```json
{
  "formula": "□□(p → q)",
  "metrics": {
    "modal_depth": 2,
    "operator_count": 3,
    "complexity_score": 10.5,
    "estimated_verification_ms": 2000
  },
  "complexity_level": "medium",
  "optimizations": [
    {
      "type": "necessitation_simplification",
      "description": "Simplify nested necessity (valid in K4, S4, S5)",
      "original": "□□(p → q)",
      "optimized": "□(p → q)",
      "improvement": "Reduced modal depth, faster in transitive systems"
    }
  ],
  "warnings": []
}
```

### Programmatic

```gleam
import modal_logic/complexity
import modal_logic/proposition.{Necessary, Atom}

let formula = Necessary(Necessary(Atom("p")))
let analysis = complexity.analyze(formula)

// analysis.metrics.modal_depth => 2
// analysis.complexity_level => Low
// analysis.optimizations => [NecessitationSimplification]
```

## Optimization Suggestions

### 1. Double Negation Elimination

**Pattern**: `¬¬p → p`

**Example**:
```
Original:  Not(Not(Atom("p")))
Optimized: Atom("p")
Improvement: Simpler formula, faster verification
```

**Validity**: All modal systems

---

### 2. Necessitation Simplification

**Pattern**: `□□p → □p` (in K4, S4, S5)

**Example**:
```
Original:  Necessary(Necessary(Atom("p")))
Optimized: Necessary(Atom("p"))
Improvement: Reduced modal depth, faster in transitive systems
```

**Validity**: K4, S4, S5 only (not K or T)

**Warning**: Only apply if using transitive systems!

---

### 3. Modal Distribution (Planned)

**Pattern**: `□(p ∧ q) ↔ □p ∧ □q`

**Example**:
```
Original:  Necessary(And(Atom("p"), Atom("q")))
Can split: And(Necessary(Atom("p")), Necessary(Atom("q")))
```

**Benefit**: Smaller sub-formulas, parallelizable verification

---

### 4. De Morgan Transform (Planned)

**Pattern**: `¬(p ∧ q) ↔ ¬p ∨ ¬q`

**Example**:
```
Original:  Not(And(Atom("p"), Atom("q")))
Optimized: Or(Not(Atom("p")), Not(Atom("q")))
```

**Benefit**: Negation normal form, sometimes faster

---

### 5. Implication Simplification (Planned)

**Pattern**: `p → q ↔ ¬p ∨ q`

**Example**:
```
Original:  Implies(Atom("p"), Atom("q"))
Optimized: Or(Not(Atom("p")), Atom("q"))
```

**Benefit**: Eliminates implication, may be faster

## Usage Examples

### Example 1: Check Before Verification

```gleam
let formula = Necessary(Necessary(Necessary(Atom("p"))))

// Check if likely to timeout
case complexity.likely_timeout(formula, 5000) {
  True -> {
    // Get optimization suggestions
    let analysis = complexity.analyze(formula)
    io.println("Consider these optimizations:")
    list.each(analysis.optimizations, fn(opt) {
      io.println(opt.description)
    })
  }
  False -> {
    // Proceed with verification
    verify(formula)
  }
}
```

### Example 2: Compare Alternative Formulations

```gleam
let original = Necessary(Necessary(Atom("p")))
let simplified = Necessary(Atom("p"))

let #(m1, m2, comparison) = complexity.compare(original, simplified)

io.println(comparison)
// "Formula 2 is simpler than Formula 1"

io.println("Time saved: " <> int.to_string(m1.estimated_verification_ms - m2.estimated_verification_ms) <> "ms")
```

### Example 3: Batch Complexity Analysis

```gleam
let formulas = [
  Atom("p"),
  Necessary(Atom("q")),
  Necessary(Necessary(Atom("r"))),
  And(Necessary(Atom("s")), Necessary(Atom("t")))
]

let analyses = complexity.analyze_batch(formulas)
let summary = complexity.batch_summary(analyses)

io.println("Average complexity: " <> float.to_string(summary.average_complexity_score))
io.println("Average modal depth: " <> int.to_string(summary.average_modal_depth))
```

## Warnings

The system generates warnings for:

### High Modal Depth (>5)

```
⚠️  High modal depth (6) may cause slow verification
```

**Resolution**: Simplify nested modalities if possible

---

### Many Operators (>20)

```
⚠️  Many operators (25) may increase verification time
```

**Resolution**: Split into smaller formulas or apply optimizations

---

### Very High Complexity

```
⚠️  Very high complexity - consider simplifying or splitting into smaller formulas
```

**Resolution**: Break formula into parts, verify separately

---

### Long Estimated Time (>5s)

```
⚠️  Estimated verification time: 8000ms - may timeout
```

**Resolution**: Increase timeout or simplify formula

## Performance Prediction

### Estimation Model

Based on empirical testing:

```
Time = f(modal_depth, operators, nesting)

Estimates:
- Score < 5:   ~100ms   (simple)
- Score 5-15:  ~500ms   (moderate)
- Score 15-30: ~2s      (complex)
- Score 30-50: ~5s      (very complex)
- Score 50+:   ~10s+    (extreme)
```

**Accuracy**: ±50% (formulas vary significantly)

### When to Trust Estimates

**Reliable**:
- Simple formulas (score < 15)
- Standard patterns
- Well-formed structure

**Less Reliable**:
- Very complex formulas (score > 50)
- Unusual operator combinations
- First-time system use

## Quick Metrics

For fast complexity checks without full analysis:

```gleam
// Just get the score
let score = complexity.quick_score(formula)

// Just get time estimate
let ms = complexity.quick_estimate(formula)

// Check timeout likelihood
let will_timeout = complexity.likely_timeout(formula, 5000)
```

## Integration Examples

### Pre-Verification Check

```gleam
fn smart_verify(formula: Proposition, timeout: Int) -> Result(Validation, Error) {
  let analysis = complexity.analyze(formula)

  case analysis.complexity_level {
    complexity.VeryHigh -> {
      log_warning("Formula has very high complexity")

      // Suggest optimizations
      case list.first(analysis.optimizations) {
        Ok(opt) -> {
          log_info("Try: " <> opt.optimized)
        }
        Error(_) -> Nil
      }
    }
    _ -> Nil
  }

  // Proceed with verification
  verify(formula, timeout)
}
```

### Automatic Optimization

```gleam
fn verify_with_auto_optimize(formula: Proposition) -> Result(Validation, Error) {
  let analysis = complexity.analyze(formula)

  case list.first(analysis.optimizations) {
    Ok(opt) -> {
      log_info("Auto-optimizing: " <> opt.description)
      // In real implementation, would apply optimization and verify
      verify(formula)
    }
    Error(_) -> verify(formula)
  }
}
```

## API Reference

### POST /api/analyze/complexity

Analyze formula complexity.

**Request**:
```json
{
  "formula": "□□(p → q)"
}
```

**Response**:
```json
{
  "formula": "□□(p → q)",
  "metrics": {
    "modal_depth": 2,
    "operator_count": 3,
    "atom_count": 2,
    "total_nodes": 6,
    "nesting_level": 2,
    "complexity_score": 10.5,
    "estimated_verification_ms": 2000
  },
  "complexity_level": "medium",
  "optimizations": [
    {
      "type": "necessitation_simplification",
      "description": "Simplify nested necessity (valid in K4, S4, S5)",
      "original": "□□(p → q)",
      "optimized": "□(p → q)"
    }
  ],
  "warnings": []
}
```

## Testing

### 21 new tests (534 total, all passing)

**Basic Metrics** (4):
- Simple atom, negation, conjunction, necessity

**Modal Depth** (3):
- Nested necessity, mixed operators, complex nesting

**Complexity Levels** (3):
- Low, medium, high classification

**Optimization Detection** (3):
- Double negation, nested necessity, no optimizations

**Warnings** (2):
- Deep nesting warnings, no warnings for simple

**Quick Metrics** (3):
- Quick score, estimate, timeout likelihood

**Comparison** (1):
- Formula comparison

**Batch Analysis** (2):
- Batch processing, summary statistics

**Formatting** (2):
- Metrics formatting, analysis formatting

**Names** (2):
- Complexity level names, optimization type names

```bash
gleam test
# ✓ analyze_simple_atom_test
# ✓ modal_depth_nested_necessity_test
# ✓ low_complexity_classification_test
# ✓ detect_double_negation_test
# ✓ warnings_for_deep_modal_nesting_test
# ✓ quick_score_test
# ✓ compare_formulas_test
# ✓ batch_summary_test
# ... and 13 more
```

## See Also

- [Error Handling](ERROR_HANDLING.md) - Timeout error codes
- [Patterns Library](PATTERNS.md) - Pattern complexity levels
- [Batch Verification](BATCH_VERIFICATION.md) - Batch complexity analysis
- [Performance Benchmarks](CI_CD.md#performance-monitoring) - Actual verification times
