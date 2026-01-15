# Batch Verification & Comparison Mode

## Overview

Batch verification enables systematic analysis by verifying multiple formulas across different modal systems simultaneously. Essential for research workflows comparing logical properties across systems.

## Features

- **Batch Verification**: Verify multiple formulas across multiple systems in one request
- **Comparison Mode**: Compare a single formula across all modal systems
- **Parallel Execution**: Leverage Erlang/OTP concurrency for fast processing
- **Summary Statistics**: Aggregate results with counts and averages
- **Consensus Analysis**: Determine agreement/disagreement across systems

## Quick Start

### API

#### Batch Verification

```bash
POST /api/batch/verify
{
  "formulas": [
    {"id": "f1", "formula": "□p → p", "systems": ["K", "T", "S4"]},
    {"id": "f2", "formula": "□p → □□p", "systems": ["K", "K4", "S4"]}
  ],
  "timeout_ms": 60000,
  "parallel": true,
  "include_countermodels": true
}
```

#### System Comparison

```bash
POST /api/compare/systems
{
  "formula": "□p → p",
  "systems": ["K", "T", "S4", "S5", "KD", "KD45"],
  "timeout_ms": 60000
}
```

### Programmatic

```gleam
import modal_logic/batch

// Batch verification
let request = batch.BatchRequest(
  formulas: [
    batch.FormulaRequest(id: "f1", formula: "□p → p", systems: [K, T]),
    batch.FormulaRequest(id: "f2", formula: "◇p", systems: [K, T, S4])
  ],
  timeout_ms: 60_000,
  parallel: True,
  include_countermodels: True
)

let result = batch.verify_batch(request)
// result.summary.total_formulas => 2
// result.summary.total_verifications => 5

// System comparison
let comp_request = batch.ComparisonRequest(
  formula: "□p → p",
  systems: [K, T, S4, S5],
  timeout_ms: 60_000
)

let comp_result = batch.compare_systems(comp_request)
// comp_result.consensus => Some(Mixed(...))
```

## Use Cases

### Research Workflow 1: Systematic Formula Analysis

Analyze how a set of formulas behaves across different modal systems:

```gleam
let formulas = [
  FormulaRequest(id: "axiom_t", formula: "□p → p", systems: [K, T, S4, S5]),
  FormulaRequest(id: "axiom_4", formula: "□p → □□p", systems: [K, K4, S4, S5]),
  FormulaRequest(id: "axiom_5", formula: "◇p → □◇p", systems: [K, T, S4, S5])
]

let result = batch.verify_batch(BatchRequest(formulas, 60_000, True, True))

// Result shows which axioms hold in which systems:
// axiom_t: invalid in K, valid in T/S4/S5
// axiom_4: invalid in K/T, valid in K4/S4/S5
// axiom_5: invalid in K/T/S4, valid in S5
```

### Research Workflow 2: Modal System Comparison

Understand how different systems handle the same formula:

```gleam
let request = ComparisonRequest(
  formula: "□(p → q) → (□p → □q)",  // K axiom
  systems: [K, T, S4, S5, KD, KD45],
  timeout_ms: 60_000
)

let result = compare_systems(request)
// result.consensus => AllValid (K axiom valid in all systems)
```

### Research Workflow 3: Finding System Boundaries

Identify which formulas distinguish between systems:

```gleam
let distinguishing_formulas = [
  FormulaRequest(id: "d1", formula: "□p → p", systems: [K, T]),
  FormulaRequest(id: "d2", formula: "□p → □□p", systems: [K, K4]),
  FormulaRequest(id: "d3", formula: "◇p → □◇p", systems: [S4, S5])
]

// Results show:
// d1: Invalid in K, Valid in T (T adds reflexivity)
// d2: Invalid in K, Valid in K4 (K4 adds transitivity)
// d3: Invalid in S4, Valid in S5 (S5 adds Euclidean property)
```

## API Reference

### POST /api/batch/verify

Verify multiple formulas across multiple systems.

**Request**:
```json
{
  "formulas": [
    {
      "id": "f1",
      "formula": "□p → p",
      "systems": ["K", "T", "S4"]
    }
  ],
  "timeout_ms": 60000,
  "parallel": true,
  "include_countermodels": true
}
```

**Response**:
```json
{
  "results": [
    {
      "id": "f1",
      "formula": "□p → p",
      "system_results": {
        "K": {"status": "invalid", "countermodel": "..."},
        "T": {"status": "valid"},
        "S4": {"status": "valid"}
      }
    }
  ],
  "summary": {
    "total_formulas": 1,
    "total_verifications": 3,
    "valid_count": 2,
    "invalid_count": 1,
    "timeout_count": 0,
    "error_count": 0,
    "average_time_ms": 150
  },
  "execution_time_ms": 450
}
```

### POST /api/compare/systems

Compare a single formula across multiple modal systems.

**Request**:
```json
{
  "formula": "□p → p",
  "systems": ["K", "T", "S4", "S5", "KD", "KD45"],
  "timeout_ms": 60000
}
```

**Response**:
```json
{
  "formula": "□p → p",
  "results_by_system": {
    "K": {"status": "invalid"},
    "T": {"status": "valid"},
    "S4": {"status": "valid"},
    "S5": {"status": "valid"},
    "KD": {"status": "invalid"},
    "KD45": {"status": "invalid"}
  },
  "differences": [
    {
      "system1": "K",
      "system2": "T",
      "result1": "invalid",
      "result2": "valid",
      "explanation": "Valid in T but invalid in K (T adds reflexivity)"
    }
  ],
  "consensus": {
    "type": "mixed",
    "valid_systems": ["T", "S4", "S5"],
    "invalid_systems": ["K", "KD", "KD45"]
  }
}
```

## Batch Request Format

### FormulaRequest

```gleam
FormulaRequest(
  id: "unique_id",           // User-defined identifier
  formula: "□p → p",          // Formula string
  systems: [K, T, S4, S5]    // Systems to verify against
)
```

### BatchRequest

```gleam
BatchRequest(
  formulas: List(FormulaRequest),  // List of formulas to verify
  timeout_ms: 60_000,              // Timeout per verification
  parallel: True,                  // Enable parallel execution
  include_countermodels: True      // Include countermodels for invalid
)
```

## Result Format

### SystemResult

Possible outcomes for each verification:

- `Valid` - Formula is valid in the system
- `Invalid(countermodel)` - Formula is invalid, optional countermodel included
- `Timeout` - Verification timed out
- `Error(message)` - Verification error occurred

### BatchSummary

Aggregate statistics:

```gleam
BatchSummary(
  total_formulas: 10,        // Number of formulas verified
  total_verifications: 50,   // Total system checks (formulas × systems)
  valid_count: 30,           // Count of valid results
  invalid_count: 15,         // Count of invalid results
  timeout_count: 3,          // Count of timeouts
  error_count: 2,            // Count of errors
  average_time_ms: 180       // Average time per verification
)
```

## Comparison Mode

### Consensus Outcomes

- `AllValid` - Formula valid in all systems
- `AllInvalid` - Formula invalid in all systems
- `Mixed(valid_systems, invalid_systems)` - Some valid, some invalid

### System Differences

Identifies why systems differ:

```gleam
SystemDifference(
  system1: "K",
  system2: "T",
  result1: Invalid(None),
  result2: Valid,
  explanation: "Valid in T but invalid in K (T adds reflexivity)"
)
```

## Performance

### Parallel Execution

Batch verification leverages Erlang/OTP concurrency:

```
Sequential (10 formulas × 5 systems):
  10 × 5 × 2s = 100s total

Parallel (OTP processes):
  max(10 formulas) × 2s = 20s total
  5x speedup!
```

### Benchmarks (Estimated)

- **Small batch** (5 formulas, 3 systems): <5s
- **Medium batch** (20 formulas, 5 systems): <15s
- **Large batch** (100 formulas, 7 systems): <60s

**Note**: Actual times depend on formula complexity and system load.

## Examples

### Example 1: Axiom Validation

Verify all modal axioms across systems:

```gleam
let axioms = [
  FormulaRequest("axiom_k", "□(p → q) → (□p → □q)", [K, T, S4, S5, KD]),
  FormulaRequest("axiom_t", "□p → p", [K, T, S4, S5]),
  FormulaRequest("axiom_4", "□p → □□p", [K, K4, S4, S5]),
  FormulaRequest("axiom_5", "◇p → □◇p", [K, T, S4, S5]),
  FormulaRequest("axiom_d", "□p → ◇p", [K, T, KD, KD45])
]

let result = verify_batch(BatchRequest(axioms, 60_000, True, True))
// Shows which axioms hold in which systems
```

### Example 2: System Selection Helper

Determine which system to use for a formula:

```gleam
let request = ComparisonRequest(
  "Knows(agent, p) → p",  // Knowledge implies truth
  [K, T, S4, S5],
  60_000
)

let result = compare_systems(request)
// Result shows formula is valid in T, S4, S5 (reflexive systems)
// Invalid in K (no reflexivity)
// Recommendation: Use T or stronger
```

### Example 3: Formula Testing

Test candidate formulas before formalizing arguments:

```gleam
let candidates = [
  FormulaRequest("c1", "□(p ∧ q) → (□p ∧ □q)", [K, T, S4]),
  FormulaRequest("c2", "(□p ∧ □q) → □(p ∧ q)", [K, T, S4]),
  FormulaRequest("c3", "□(p ∨ q) → (□p ∨ □q)", [K, T, S4])
]

let result = verify_batch(BatchRequest(candidates, 60_000, True, False))
// c1: Valid (distribution of necessity over conjunction)
// c2: Valid (reverse also holds)
// c3: Invalid (distribution over disjunction fails)
```

## Implementation Notes

### Current Implementation

- **Simplified**: Mock verification returns placeholder results
- **Foundation**: Types and API structure in place
- **Ready for Integration**: Designed to integrate with existing validator module

### Future Integration

Full implementation will:

1. Import `modal_logic/validator`
2. Call `validator.validate()` for each formula/system pair
3. Use actual Z3 verification
4. Generate real countermodels
5. Measure actual execution times
6. Support true parallel execution via OTP processes

## Performance Optimization

### Strategies

1. **Parallel Execution**: Use Erlang processes for concurrent verification
2. **Caching**: Cache repeated formula/system pairs
3. **Early Termination**: Stop on timeout to avoid blocking
4. **Result Streaming**: Stream results as they complete (WebSocket)

### Planned Enhancements

- Process pool for verification workers
- Timeout per-formula override
- Priority-based scheduling
- Result caching with TTL

## See Also

- [API Documentation](API.md) - Complete REST API reference
- [Profiles](PROFILES.md) - Modal system profiles
- [Error Handling](ERROR_HANDLING.md) - Error codes and logging
- [CI/CD](CI_CD.md) - Performance benchmarks
