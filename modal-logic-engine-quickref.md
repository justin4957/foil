# Modal Logic Engine - Quick Reference

## Project Structure

```
modal-logic-engine/
├── packages/
│   ├── anthropic_gleam/    # LLM client (publishable)
│   ├── z3_gleam/           # Z3 bindings (publishable)  
│   └── modal_logic/        # Shared domain types
├── apps/
│   └── analyst/            # Main application
└── docs/
```

## Three Tracks

| Track | Package | Timeline | Hex Publishable |
|-------|---------|----------|-----------------|
| A | anthropic_gleam | 8 weeks | Yes |
| B | z3_gleam | 9 weeks | Yes |
| C | analyst | 12 weeks | No (application) |

## Key Dependencies

```
A2 (streaming) ──→ C2 (LLM translation)
B3 (modal logic) ──→ C3 (validation pipeline)
```

## Phase Overview

### Track A: Anthropic Client
- **A1** (2w): Core types, basic completion
- **A2** (2w): Streaming support  
- **A3** (2w): Tool use
- **A4** (2w): Production polish, publish

### Track B: Z3 Bindings
- **B1** (2w): Design, NIF vs Port decision, prototypes
- **B2** (3w): Core solver implementation
- **B3** (2w): Modal logic encoding, countermodels
- **B4** (2w): Performance, publish

### Track C: Main Application
- **C1** (3w): Domain model, persistence (Postgres + Redis)
- **C2** (3w): LLM translation pipeline
- **C3** (3w): Validation orchestration, execution loop
- **C4** (3w): HTTP API, WebSocket, CLI, Web UI

## First Week Actions

1. Set up monorepo structure
2. Create GitHub labels, milestones, projects
3. Create all issues from roadmap
4. Start A1.1 (Anthropic types) and B1.1 (Z3 research) in parallel
5. Start C1.1 (domain types) - no dependencies

## Technology Choices

| Component | Choice | Rationale |
|-----------|--------|-----------|
| Language | Gleam | BEAM runtime, type safety, learning goal |
| Database | PostgreSQL | JSONB for complex structures |
| Cache | Redis | Validation result caching |
| Logic Engine | Z3 | Best tooling, modal via translation |
| LLM | Claude | Structured outputs, strong reasoning |

## Core Architectural Principle

> LLMs identify structure; deterministic code generates syntax; symbolic engines validate.

The LLM never writes `◊P → □Q`. It outputs:
```json
{"type": "implies", 
 "antecedent": {"type": "possible", "proposition": "P"},
 "consequent": {"type": "necessary", "proposition": "Q"}}
```

The compiler generates Z3 input. This eliminates hallucinated syntax.

## Issue Naming Convention

```
{Track}{Phase}.{Number}: {Short description}

Examples:
A1.1: Define core message types
B2.3: Implement model inspection
C3.4: Implement the execution loop
```

## Labels to Create

```bash
# Tracks
track-a, track-b, track-c

# Phases  
phase-1, phase-2, phase-3, phase-4

# Types
types, api, testing, documentation, research, 
reliability, performance, streaming, modal, 
compiler, persistence, llm, prompts
```
