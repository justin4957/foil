# Foil: Modal Logic Engine - Synopsis

## Executive Summary

Foil is a sophisticated modal logic analysis system that bridges natural language reasoning with formal verification. Built in Gleam, it combines LLM-based semantic translation (via Claude API) with Z3-powered SMT solving to analyze philosophical arguments, validate logical structures, and provide actionable feedback through countermodels and repair suggestions.

## Core Value Proposition

**Problem**: Philosophical and modal logic arguments are difficult to validate. Natural language is imprecise, formal logic is inaccessible, and manual verification is error-prone.

**Solution**: Foil automates the complete pipeline:
1. Natural language → Formal logic translation (LLM)
2. Rigorous validity checking (Z3 SMT solver)
3. Countermodel generation for invalid arguments
4. Intelligent repair suggestions
5. Multi-format visualization

**Key Principle**: LLMs translate, symbolic engines verify. LLMs never assert validity—Z3 determines truth.

## Architecture Overview

### Three-Track Development

1. **anthropic_gleam** - Typed Claude API client with streaming, tools, and retry logic
2. **z3_gleam** - Z3 theorem prover bindings with modal logic support
3. **modal_logic** - Unified analysis engine combining tracks A & B

### Modal Logic Capabilities

**Supported Systems**: K, T, K4, S4, S5, KD, KD45 (7 systems total)

**Proposition Types**:
- Boolean operators (Not, And, Or, Implies)
- Modal operators (Necessary, Possible)
- Deontic operators (Obligatory, Permitted)
- Epistemic operators (Knows, Believes)

### Current Functionality

**Analysis Pipeline**:
1. **Input**: Natural language argument
2. **Translation**: LLM converts to formal propositions
3. **Logic Detection**: Auto-detect appropriate modal system (K/T/S4/S5/KD/KD45)
4. **Validation**: Z3 checks validity via standard translation to first-order logic
5. **Output**: Valid/Invalid + Countermodel/Repair suggestions

**Key Components**:
- Translation service with LLM prompts and JSON compilation
- Validator with Z3 integration and countermodel extraction
- Explanation and repair generation
- Self-correcting execution loop
- Multi-format visualization (Mermaid, Graphviz, LaTeX, Markdown)

**Persistence**:
- In-memory caching
- PostgreSQL repository
- Graph-based argument relationship storage

**Interfaces**:
- REST API (8+ endpoints)
- WebSocket for real-time updates
- CLI interface
- Web UI with HTML generation

## Testing & Quality

**Comprehensive Testing**:
- 20+ test modules covering all layers
- Integration tests for validation suite
- External dataset integration (FOLIO, LogiQA, InPhO)
- Philosophical argument corpus validation
- Rule soundness verification
- Auto-generated test documentation

**Recent Developments** (Last commit: Jan 2026):
- Integration testing for complete validation suite
- Auto-documentation from test results
- Dataset integration improvements

## Technical Stack

- **Language**: Gleam (functional, type-safe)
- **Runtime**: Erlang/OTP
- **AI**: Anthropic Claude API
- **Verification**: Z3 SMT Solver
- **Storage**: PostgreSQL, Redis

## Metrics

- **Codebase**: ~13,588 lines (modal_logic package)
- **Commits**: 80 across 3 development tracks
- **Test Coverage**: 20+ comprehensive test modules
- **Logic Systems**: 7 supported
- **Proposition Types**: 11
- **Export Formats**: 5+ visualization formats

## Current Status

**Development Complete**: All three tracks (A, B, C) implemented
**Branch**: feature/integration-testing-95 (clean)
**Latest Focus**: Integration testing and validation suite completeness

## Key Strengths

1. **Rigorous Validation**: Z3 provides provably correct verification
2. **Practical UX**: Multiple interfaces (API, WebSocket, CLI, Web)
3. **Actionable Feedback**: Countermodels show why arguments fail + repair suggestions
4. **Pluralistic**: Multiple competing formalizations can coexist
5. **Self-Correcting**: Failures trigger automatic repair and re-validation
6. **Production-Ready**: Comprehensive testing, documentation, and error handling

## Potential Growth Areas

1. **Scalability**: Handle larger argument corpora and real-time analysis
2. **User Adoption**: Broader interfaces, integrations, educational tools
3. **Theoretical Depth**: More modal systems, richer logic fragments, proof generation
4. **AI Capabilities**: Multi-LLM support, improved translation accuracy, context awareness
5. **Collaboration**: Multi-user workflows, argument sharing, peer review
6. **Domain Expansion**: Specialized domains (legal, medical, scientific reasoning)
