# Architecture

## System Overview

The Modal Logic Engine is built as a monorepo with three parallel development tracks:

### Track A: anthropic_gleam
A publishable Gleam client library for Anthropic's Claude API with:
- Typed message interfaces
- Streaming support
- Tool use capabilities

### Track B: z3_gleam
A publishable Gleam binding to the Z3 theorem prover with:
- SMT solving
- Modal logic encoding via Kripke frames
- Countermodel extraction

### Track C: analyst
The main application integrating tracks A and B to provide:
- Natural language argument analysis
- LLM-based translation to formal logic
- Z3-based validation
- Countermodel generation and repair suggestions

## Core Principles

1. **LLMs never assert validity** — they translate natural language to candidate logical structures
2. **Symbolic engines are epistemically authoritative** — Z3 determines validity and generates countermodels
3. **Persistence preserves modal plurality** — competing formalizations coexist; invalidity is informative
4. **The execution loop is self-correcting** — failures trigger repair suggestions and re-validation

## Package Architecture

```
modal-logic-engine/
├── packages/
│   ├── anthropic_gleam/    # Independent, publishable to Hex
│   ├── z3_gleam/           # Independent, publishable to Hex
│   └── modal_logic/        # Shared domain types
└── apps/
    └── analyst/            # Main application (depends on all packages)
```

This structure enables:
- Independent development and publication of libraries
- Shared domain types across components
- Clean separation of concerns
