# Foil

[![Build Status](https://github.com/justin4957/foil/actions/workflows/ci.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/ci.yml)
[![Coverage](https://github.com/justin4957/foil/actions/workflows/coverage.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/coverage.yml)
[![Performance](https://github.com/justin4957/foil/actions/workflows/performance.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/performance.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A modal logic verification engine that translates natural language arguments into formal logic and proves them with an SMT solver. LLMs translate; symbolic engines decide.

## What Foil Does

Give Foil an argument in plain English. It will:

1. **Translate** the argument into formal modal logic propositions (via Claude)
2. **Detect** which logic system fits (K, T, S4, S5, KD, KD45)
3. **Verify** validity using Z3 SMT solving with Kripke semantics
4. **Explain** why it's valid, or provide a countermodel showing why it fails
5. **Suggest repairs** for invalid arguments

```
Input:  "Necessarily, if it rains then the ground is wet.
         It is raining. Therefore the ground is wet."

Output: Valid (System T) — Tier 1 syntactic match: modus ponens under necessity
        □(rain → wet), rain ⊢ wet
```

### Design Principles

| Principle | What it means |
|-----------|---------------|
| LLMs never assert validity | They only translate natural language to candidate formal structures |
| Symbolic engines are authoritative | Z3 determines truth — not the language model |
| Persistence preserves plurality | Competing formalizations coexist; invalidity is informative |
| Self-correcting execution | Failures trigger repair suggestions and re-validation |

## Quick Start

### Prerequisites

- [Gleam](https://gleam.run/) >= 1.13.0
- [Erlang/OTP](https://www.erlang.org/) >= 27.0

### Build and Test

```bash
git clone https://github.com/justin4957/foil.git
cd foil/packages/modal_logic

gleam build
gleam test    # 736 tests, 0 failures
```

### Use As a Library

```gleam
import gleam/option.{None}
import modal_logic/proposition.{Atom, Implies, Necessary, K}
import modal_logic/argument.{Formalization}
import modal_logic/heuristics

// □(p → q), □p ⊢ □q  (K axiom — distribution of necessity over implication)
let formalization = Formalization(
  id: "k-axiom-1",
  argument_id: "arg-1",
  logic_system: K,
  premises: [
    Necessary(Implies(Atom("p"), Atom("q"))),
    Necessary(Atom("p")),
  ],
  conclusion: Necessary(Atom("q")),
  assumptions: [],
  validation: None,
  created_at: None,
  updated_at: None,
)

// Fast heuristic check — resolves in <1ms for syntactic patterns
case heuristics.try_heuristic_validation(formalization) {
  option.Some(result) -> result.result   // Valid
  option.None -> todo                    // Fall through to Z3
}
```

## Repository Structure

```
foil/
├── packages/
│   ├── anthropic_gleam/   # Claude API client (typed, streaming, tool use)
│   ├── z3_gleam/          # Z3 SMT solver bindings (Kripke semantics)
│   └── modal_logic/       # Core engine — types, validation, analysis
├── apps/
│   └── analyst/           # Application integrating all three packages
├── docs/                  # Architecture, testing, and reference docs
└── scripts/               # Development and CI scripts
```

### Packages

**anthropic_gleam** — A typed Gleam client for Anthropic's Claude API. Streaming responses, tool use, retry logic with exponential backoff, request validation.

**z3_gleam** — Gleam bindings to the Z3 theorem prover. Type-safe SMT expression construction, modal logic encoding via Kripke frames, standard translation to first-order logic, countermodel extraction.

**modal_logic** — The core engine. Everything below lives here.

## Validation Pipeline

Foil uses a three-tier validation strategy that avoids invoking the full SMT solver when a cheaper check suffices:

```
Argument
  │
  ▼
┌─────────────────────────────────┐
│ Tier 1: Syntactic Checks (<1ms) │  Identity, tautology, contradiction,
│                                 │  modus ponens, modus tollens, modal axioms
└────────────┬────────────────────┘
             │ unresolved
             ▼
┌──────────────────────────────────────┐
│ Tier 2: Truth Table Enumeration (<50ms) │  Complete check for formulas
│                                          │  with ≤ 5 propositional variables
└────────────┬─────────────────────────────┘
             │ unresolved
             ▼
┌──────────────────────────────────┐
│ Tier 3: Z3 SMT Solving          │  Full Kripke-frame satisfiability check,
│                                  │  countermodel extraction, repair suggestions
└──────────────────────────────────┘
```

**96% coverage** — Tier 1 resolves the vast majority of arguments before Tier 2 or Z3 is needed.

## Supported Logic

### Modal Systems

| System | Axioms | Frame Properties | Typical Use |
|--------|--------|------------------|-------------|
| **K** | K: `□(p→q) → (□p→□q)` | None | Base modal reasoning |
| **T** | K + T: `□p → p` | Reflexive | Alethic necessity |
| **K4** | K + 4: `□p → □□p` | Transitive | Introspection |
| **S4** | K + T + 4 | Reflexive + Transitive | Knowledge, provability |
| **S5** | K + T + 5: `◇p → □◇p` | Equivalence relation | Metaphysical necessity |
| **KD** | K + D: `□p → ◇p` | Serial | Deontic (obligation/permission) |
| **KD45** | K + D + 4 + 5 | Serial + Trans + Euclidean | Deontic S5 |

### Proposition Types

```
Boolean:        Atom("p")  Not(p)  And(p, q)  Or(p, q)  Implies(p, q)
Alethic modal:  Necessary(p)       Possible(p)
Deontic:        Obligatory(p)      Permitted(p)
Epistemic:      Knows("alice", p)  Believes("bob", p)
Probabilistic:  Probable(p)        ProbAtLeast(p, 0.8)   ProbAtMost(p, 0.3)
                ProbExact(p, 0.5)  ProbRange(p, 0.4, 0.7)
                CondProb(p, q, 0.9)
```

The probabilistic operators support constraint propagation, chain rule inference (`P(C) >= P(C|B) * P(B|A) * P(A)`), and interval arithmetic with configurable tolerance.

## Analysis Features

### Reason Chain Formalization

Extracts structured reasoning chains from natural language, classifying each step as premise introduction, inference application, assumption, or conclusion.

### Fallacy Detection

Identifies formal fallacies with explanations:
- Affirming the consequent
- Denying the antecedent
- Undistributed middle
- Modal scope errors

### Multi-System Comparison

Validates the same argument across all 7 logic systems in parallel and recommends the most appropriate system based on the argument's modal content.

### Confidence Scoring

Produces calibrated confidence scores using Brier scoring, expected calibration error (ECE), and per-bucket calibration curves.

### Visualization

Exports argument structures in Mermaid, Graphviz, LaTeX, and Markdown formats.

## Testing

736 tests across 12 dialogue test suites and unit tests. The test infrastructure includes:

- **Dialogue tests** — demonstrate real interaction flows (translation, validation, fallacy detection, etc.)
- **Epic validation framework** — 30 metrics organized across 5 phases with automated pass/fail thresholds
- **Curated ground-truth fixtures** — 52 hand-verified cases covering 10 categories
- **Golden master baselines** — regression detection with < 2% F1 tolerance
- **Adversarial analytical tests** — near-boundary probabilities, conflicting constraints, chain propagation, numerical stability

### Run Tests

```bash
cd packages/modal_logic
gleam test
```

### Validation Phases

| Phase | Focus | Metrics | Status |
|-------|-------|---------|--------|
| A | Fast Validation Pipeline | 7 (latency, coverage, accuracy, confidence, false positive rate) | 7/7 passing |
| B | Reason Chain Analysis | 7 (parsing, classification, detection, traces, fallacy) | 7/7 passing |
| C | Multi-System Comparison | 3 (parallel validation, comparison, recommendation) | 3/3 passing |
| D | Accuracy Benchmarking | 7 (FOLIO F1, LogiQA, InPhO, regression, baselines) | 7/7 passing |
| E | Extended Logic (Probabilistic) | 6 (detection, extraction, chain rule, bounds, conditionals, analytical) | 6/6 passing |

See [docs/TESTING.md](docs/TESTING.md) for detailed testing strategies and metric definitions.

## Documentation

| Document | Contents |
|----------|----------|
| [docs/TESTING.md](docs/TESTING.md) | Testing strategy, metrics, and validation procedures |
| [docs/foil_synopsis.md](docs/foil_synopsis.md) | Executive summary and capabilities overview |
| [docs/architecture/](docs/architecture/) | System architecture and design decisions |
| [docs/ERROR_HANDLING.md](docs/ERROR_HANDLING.md) | Error handling patterns and error codes |
| [docs/PATTERNS.md](docs/PATTERNS.md) | Common patterns and idioms |
| [docs/CI_CD.md](docs/CI_CD.md) | CI/CD pipeline documentation |

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/your-feature`)
3. Make changes and add tests
4. Format (`gleam format src test`) and run tests (`gleam test`)
5. Open a Pull Request with a linked issue

## License

MIT — see [LICENSE](LICENSE) for details.

---

Built with [Gleam](https://gleam.run/) | Verified by [Z3](https://github.com/Z3Prover/z3) | Translated by [Claude](https://www.anthropic.com/)
