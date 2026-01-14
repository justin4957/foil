# Modal Logic Engine

[![Build Status](https://github.com/justin4957/foil/actions/workflows/ci.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/ci.yml)
[![Coverage](https://github.com/justin4957/foil/actions/workflows/coverage.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/coverage.yml)
[![Performance](https://github.com/justin4957/foil/actions/workflows/performance.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/performance.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

![AI Thinking](https://media.giphy.com/media/KVVgWtScb37USleUB3/giphy.gif)

> A modal logic analysis system built in Gleam, combining LLM-based semantic translation with Z3-powered formal verification.

## Project Overview

This project analyzes natural language arguments using modal logic. It translates informal reasoning into formal logical structures, validates them using Z3, and provides countermodels and repair suggestions for invalid arguments.

### Core Principles

| Principle | Description |
|-----------|-------------|
| **LLMs never assert validity** | They translate natural language to candidate logical structures |
| **Symbolic engines are authoritative** | Z3 determines validity and generates countermodels |
| **Persistence preserves plurality** | Competing formalizations coexist; invalidity is informative |
| **Self-correcting execution** | Failures trigger repair suggestions and re-validation |

## Repository Structure

![Brain Think](https://media.giphy.com/media/Iup13qWlzAqsBItALB/giphy.gif)

This is a Gleam monorepo containing three development tracks:

```
foil/
├── packages/
│   ├── anthropic_gleam/    # Track A: LLM client library
│   ├── z3_gleam/           # Track B: Z3 bindings
│   └── modal_logic/        # Track C: Modal logic analysis
├── docs/                   # Documentation
└── scripts/                # Development scripts
```

## Packages

### Track A: anthropic_gleam

A well-typed, idiomatic Gleam client for Anthropic's Claude API.

| Feature | Status |
|---------|--------|
| Typed message interfaces | ✅ Complete |
| Streaming response support | ✅ Complete |
| Tool use capabilities | ✅ Complete |
| Retry logic & validation | ✅ Complete |

### Track B: z3_gleam

Gleam bindings to the Z3 theorem prover.

| Feature | Status |
|---------|--------|
| SMT solving | ✅ Complete |
| Modal logic encoding | ✅ Complete |
| Countermodel extraction | ✅ Complete |
| Kripke frame support | ✅ Complete |

### Track C: modal_logic

The main modal logic analysis package.

| Feature | Status |
|---------|--------|
| Domain types (propositions, arguments) | ✅ Complete |
| LLM translation pipeline | ✅ Complete |
| Validation & execution loop | ✅ Complete |
| HTTP API, WebSocket, CLI, Web UI | ✅ Complete |
| Visualization exports | ✅ Complete |

See [packages/modal_logic/README.md](packages/modal_logic/README.md) for detailed documentation.

## Getting Started

### Prerequisites

- [Gleam](https://gleam.run/) >= 1.13.0
- [Erlang/OTP](https://www.erlang.org/) >= 27.0
- Z3 (for z3_gleam package)

### Quick Start

```bash
# Clone the repository
git clone https://github.com/justin4957/foil.git
cd foil

# Build all packages
cd packages/modal_logic
gleam build

# Run tests
gleam test

# Run specific test modules
gleam run -m translation_test
gleam run -m validation_test
gleam run -m persistence_test
gleam run -m interface_test
```

### Example Usage

```gleam
import modal_logic/proposition.{Atom, Implies, Necessary, K}
import modal_logic/argument.{Formalization}
import modal_logic/validator

pub fn main() {
  // □(p → q), □p ⊢ □q (Modus Ponens under necessity)
  let formalization = Formalization(
    id: "modus-ponens-1",
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

  let config = validator.default_config()
  let result = validator.validate(formalization, config)
  // Result: Valid
}
```

## Modal Logic Systems

![Brain Visualization](https://media.giphy.com/media/l41lJ8ywG1ncm9FXW/giphy.gif)

| System | Axioms | Frame Properties | Use Cases |
|--------|--------|------------------|-----------|
| **K** | K: □(p→q) → (□p→□q) | None | Basic modal reasoning |
| **T** | K + T: □p → p | Reflexive | Alethic necessity |
| **K4** | K + 4: □p → □□p | Transitive | - |
| **S4** | K + T + 4 | Reflexive + Transitive | Knowledge, provability |
| **S5** | K + T + 5: ◇p → □◇p | Equivalence relation | Metaphysical necessity |
| **KD** | K + D: □p → ◇p | Serial | Deontic logic |
| **KD45** | K + D + 4 + 5 | Serial + Trans + Euclidean | Deontic S5 |

## Documentation

- [Modal Logic Package](packages/modal_logic/README.md) - Full API documentation
- [QA Testing Guide](packages/modal_logic/docs/TESTING.md) - Testing procedures
- [Development Guide](docs/DEVELOPMENT.md) - Setup and workflow

## License

MIT License - See LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests (`gleam test`)
5. Format code (`gleam format src test`)
6. Commit your changes
7. Push to the branch
8. Open a Pull Request

## Status

![Checkmark](https://media.giphy.com/media/3o7btNhMBytxAM6YBa/giphy.gif)

**Development Complete** - All three tracks (A, B, C) have been implemented:

- ✅ Track A: Anthropic Gleam Client (Phases A1-A4)
- ✅ Track B: Z3 Gleam Bindings (Phases B1-B4)
- ✅ Track C: Modal Logic Analysis (Phases C1-C4)

---

![Thank You](https://media.giphy.com/media/q2fVZhnpLc0G8wwkP4/giphy.gif)

Built with [Gleam](https://gleam.run/) | SMT solving by [Z3](https://github.com/Z3Prover/z3)
