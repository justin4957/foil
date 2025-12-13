# Modal Logic Engine

A modal logic analysis system built in Gleam, combining LLM-based semantic translation with Z3-powered formal verification.

## Project Overview

This project analyzes natural language arguments using modal logic. It translates informal reasoning into formal logical structures, validates them using Z3, and provides countermodels and repair suggestions for invalid arguments.

### Core Principles

1. **LLMs never assert validity** — they translate natural language to candidate logical structures
2. **Symbolic engines are epistemically authoritative** — Z3 determines validity and generates countermodels
3. **Persistence preserves modal plurality** — competing formalizations coexist; invalidity is informative
4. **The execution loop is self-correcting** — failures trigger repair suggestions and re-validation

## Repository Structure

This is a Gleam monorepo containing three development tracks:

```
modal-logic-engine/
├── packages/
│   ├── anthropic_gleam/    # Track A: LLM client library (publishable)
│   ├── z3_gleam/           # Track B: Z3 bindings (publishable)
│   └── modal_logic/        # Shared domain types
├── apps/
│   └── analyst/            # Track C: Main application
├── docs/                   # Documentation
└── scripts/                # Development scripts
```

### Packages

#### Track A: anthropic_gleam
A well-typed, idiomatic Gleam client for Anthropic's Claude API with:
- Typed message interfaces
- Streaming response support
- Tool use capabilities
- **Status:** v0.1.0 - Initial structure
- **Publishable to Hex:** Yes

#### Track B: z3_gleam
Gleam bindings to the Z3 theorem prover with:
- SMT solving
- Modal logic encoding via Kripke frames
- Countermodel extraction
- **Status:** v0.1.0 - Initial structure
- **Publishable to Hex:** Yes

#### Track C: analyst
The main application integrating LLM translation and Z3 validation:
- Natural language argument analysis
- Multiple formalization generation
- Validity checking with countermodels
- Repair suggestion system
- **Status:** v0.1.0 - Initial structure

## Getting Started

### Prerequisites

- [Gleam](https://gleam.run/) >= 0.34.0
- [Erlang/OTP](https://www.erlang.org/) >= 26.0
- Z3 (for z3_gleam package)
- PostgreSQL (for analyst persistence)
- Redis (for analyst caching)

### Installation

```bash
# Clone the repository
git clone https://github.com/coolbeans/foil.git
cd foil

# Build all packages
gleam build

# Run the analyst application
cd apps/analyst
gleam run
```

### Running Tests

```bash
# Test all packages
gleam test

# Test specific package
cd packages/anthropic_gleam
gleam test
```

## Development Tracks

### Track A: Anthropic Gleam Client
- **Phase A1** (2w): Core types, basic completion
- **Phase A2** (2w): Streaming support
- **Phase A3** (2w): Tool use
- **Phase A4** (2w): Production polish, publish to Hex

### Track B: Z3 Gleam Bindings
- **Phase B1** (2w): Design, NIF vs Port decision, prototypes
- **Phase B2** (3w): Core solver implementation
- **Phase B3** (2w): Modal logic encoding, countermodels
- **Phase B4** (2w): Performance, publish to Hex

### Track C: Main Application
- **Phase C1** (3w): Domain model, persistence (Postgres + Redis)
- **Phase C2** (3w): LLM translation pipeline
- **Phase C3** (3w): Validation orchestration, execution loop
- **Phase C4** (3w): HTTP API, WebSocket, CLI, Web UI

## Documentation

- [Architecture Overview](docs/architecture/README.md)
- [Testing Strategy](docs/TESTING.md)
- [Roadmap](modal-logic-engin-roadmap.md)
- [Quick Reference](modal-logic-engine-quickref.md)

## License

MIT License - See LICENSE file for details

## Contributing

This project follows standard Gleam conventions. See docs/contributing/ for guidelines.

## Status

🚧 **Early Development** - Initial project structure established. All tracks are in Phase 1.

### Current Focus
- Establishing core type systems in all three packages
- Setting up testing infrastructure
- Preparing for parallel development across tracks

## Contact

For questions or feedback, please open an issue on GitHub.
