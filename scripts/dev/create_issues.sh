#!/bin/bash
# Script to create all GitHub issues from the roadmap

set -e

create_issue() {
    local title="$1"
    local body="$2"
    local labels="$3"
    local milestone="$4"

    echo "Creating: $title"
    gh api repos/justin4957/foil/issues --input - <<EOF
{
  "title": "$title",
  "body": "$body",
  "labels": $labels,
  "milestone": $milestone
}
EOF
    sleep 1  # Rate limiting
}

echo "Creating Track A Phase 3 issues..."

create_issue "A3.2: Implement tool use in requests" \
    "Add tool definitions to message requests. Add tools and tool_choice fields. Support Auto, Any, Tool(name), None choices." \
    '["track-a", "api", "tools", "phase-3"]' 3

create_issue "A3.3: Handle tool use responses" \
    "Process tool use in responses. Parse ToolUseBlock, extract tool name/id/input. Add get_tool_uses, has_tool_use helpers." \
    '["track-a", "api", "tools", "phase-3"]' 3

create_issue "A3.4: Implement tool result submission" \
    "Create utilities for submitting tool results. Handle tool_use -> execute -> tool_result flow. Add tool_result and tool_error helpers." \
    '["track-a", "api", "tools", "phase-3"]' 3

create_issue "A3.5: Create tool definition builder" \
    "Create ergonomic builder for tool definitions with fluent API. Type-safe parameters and clear error messages." \
    '["track-a", "tools", "ergonomics", "phase-3"]' 3

echo "Creating Track A Phase 4 issues..."

create_issue "A4.1: Implement retry logic" \
    "Add automatic retry for transient failures. Handle rate limits (429), overloaded (529), timeouts. Exponential backoff with jitter." \
    '["track-a", "reliability", "phase-4"]' 4

create_issue "A4.2: Add request validation" \
    "Validate requests before sending. Check non-empty messages, valid model, max_tokens bounds, tool definitions." \
    '["track-a", "validation", "phase-4"]' 4

create_issue "A4.3: Implement logging hooks" \
    "Add optional logging/telemetry hooks. Hook points: request start, response received, error occurred, stream events." \
    '["track-a", "observability", "phase-4"]' 4

create_issue "A4.4: Write comprehensive documentation" \
    "Create thorough documentation: README, API reference, examples directory, CHANGELOG, CONTRIBUTING guide." \
    '["track-a", "documentation", "phase-4"]' 4

create_issue "A4.5: Publish to Hex" \
    "Prepare and publish package to Hex.pm. Complete pre-publish checklist: version, license, metadata, tests, docs." \
    '["track-a", "release", "phase-4"]' 4

echo "Creating Track B Phase 1 issues..."

create_issue "B1.1: Research Z3 API surface" \
    "Document Z3 C API for our use cases. Key concepts: Context, Solver, AST, Model. Document required functions and memory management." \
    '["track-b", "research", "phase-1"]' 5

create_issue "B1.2: Design Gleam type mappings" \
    "Design how Z3 concepts map to Gleam types. Define Sort, Expr, CheckResult, Model, Value types. Document rationale." \
    '["track-b", "types", "phase-1"]' 5

create_issue "B1.3: Evaluate NIF vs Port" \
    "Compare NIF and Port implementation strategies. Prototype both, document tradeoffs, make recommendation." \
    '["track-b", "architecture", "research", "phase-1"]' 5

create_issue "B1.4: Create Port-based prototype" \
    "Build minimal Port-based Z3 integration. Python/C driver reading JSON commands, Gleam wrapper sending/parsing." \
    '["track-b", "prototype", "phase-1"]' 5

create_issue "B1.5: Create NIF-based prototype" \
    "Build minimal NIF-based Z3 integration. C NIF module linking Z3, Gleam wrapper with typed interface." \
    '["track-b", "prototype", "nif", "phase-1"]' 5

echo "Creating Track B Phase 2 issues..."

create_issue "B2.1: Implement expression builder" \
    "Create idiomatic Gleam API for building Z3 expressions. Pure Gleam expression building, compilation to Z3 separate." \
    '["track-b", "api", "phase-2"]' 6

create_issue "B2.2: Implement solver interface" \
    "Create solver management interface: new_solver, assert, check, get_model, push, pop, reset. Return CheckResult sum type." \
    '["track-b", "api", "phase-2"]' 6

create_issue "B2.3: Implement model inspection" \
    "Create interface for inspecting satisfying models. Add eval, get_const_interp, model_to_dict functions." \
    '["track-b", "api", "phase-2"]' 6

create_issue "B2.4: Implement expression compilation" \
    "Compile Gleam Expr to Z3 internal representation. Traverse Expr tree, create Z3 AST nodes, handle variable declarations." \
    '["track-b", "compiler", "phase-2"]' 6

create_issue "B2.5: Add unsat core extraction" \
    "Support extracting unsat cores for diagnostics. Add assert_named and get_unsat_core functions." \
    '["track-b", "api", "phase-2"]' 6

echo "Creating Track B Phase 3 issues..."

create_issue "B3.1: Implement Kripke frame encoding" \
    "Create utilities for encoding modal logic in Z3. Standard translation with World sort, accessibility relation, proposition predicates." \
    '["track-b", "modal", "phase-3"]' 7

create_issue "B3.2: Implement frame condition constraints" \
    "Add constraints for different modal logic systems: K, T, K4, S4, S5, KD, KD45. Define ModalSystem type." \
    '["track-b", "modal", "phase-3"]' 7

create_issue "B3.3: Implement countermodel extraction" \
    "Extract Kripke countermodels from Z3 models. Define KripkeModel type with worlds, accessibility, valuation." \
    '["track-b", "modal", "phase-3"]' 7

create_issue "B3.4: Create validity checker" \
    "High-level interface for modal validity checking. check_validity function returning Valid, Invalid(countermodel), or Unknown." \
    '["track-b", "modal", "api", "phase-3"]' 7

echo "Creating Track B Phase 4 issues..."

create_issue "B4.1: Implement timeout handling" \
    "Add configurable timeouts for solver operations. Use Z3 native timeout plus BEAM process timeout." \
    '["track-b", "reliability", "phase-4"]' 8

create_issue "B4.2: Add solver configuration" \
    "Expose Z3 configuration options: timeout, memory limit, random seed, tactic selection, model completion." \
    '["track-b", "api", "phase-4"]' 8

create_issue "B4.3: Benchmark and optimize" \
    "Profile and optimize critical paths. Create benchmarks for expression building, sat check, modal validity, incremental solving." \
    '["track-b", "performance", "phase-4"]' 8

create_issue "B4.4: Write documentation and examples" \
    "Create comprehensive documentation: README, API reference, SMT basics guide, modal logic guide, examples." \
    '["track-b", "documentation", "phase-4"]' 8

create_issue "B4.5: Publish to Hex" \
    "Prepare and publish package. Document Z3 installation requirements, consider pre-built binaries." \
    '["track-b", "release", "phase-4"]' 8

echo "Creating Track C Phase 1 issues..."

create_issue "C1.1: Define core domain types" \
    "Define core domain model: Proposition, Argument, Formalization, ValidationResult, Ambiguity types in modal_logic package." \
    '["track-c", "types", "phase-1"]' 9

create_issue "C1.2: Set up PostgreSQL schema" \
    "Design and implement database schema: arguments, formalizations, validations, repair_suggestions tables with JSONB columns." \
    '["track-c", "persistence", "phase-1"]' 9

create_issue "C1.3: Implement repository layer" \
    "Create data access layer using gleam_pgo. CRUD operations for arguments, formalizations, validations with transactions." \
    '["track-c", "persistence", "phase-1"]' 9

create_issue "C1.4: Implement caching layer" \
    "Add Redis caching for validation results. Key by hash of formalization, configurable TTL, normalization for consistent keys." \
    '["track-c", "caching", "phase-1"]' 9

create_issue "C1.5: Create argument graph queries" \
    "Implement graph-oriented queries: formalization comparison, cross-argument analysis, inference pattern tracking." \
    '["track-c", "persistence", "phase-1"]' 9

echo "Creating Track C Phase 2 issues..."

create_issue "C2.1: Design translation prompts" \
    "Create prompts for logical structure extraction. LLM identifies structure, outputs JSON, flags ambiguities." \
    '["track-c", "llm", "prompts", "phase-2"]' 10

create_issue "C2.2: Implement translation service" \
    "Create service for translating arguments via LLM. Format prompt, call Claude API with JSON mode, parse response." \
    '["track-c", "llm", "service", "phase-2"]' 10

create_issue "C2.3: Implement structure compiler" \
    "Compile LLM output to internal Proposition types. Parse JSON, validate structure, build Proposition AST." \
    '["track-c", "compiler", "phase-2"]' 10

create_issue "C2.4: Add logic system detection" \
    "Determine appropriate logic system for arguments. Heuristics for modal words, LLM suggestions, user override." \
    '["track-c", "llm", "phase-2"]' 10

create_issue "C2.5: Handle translation failures" \
    "Gracefully handle translation failures: retry with rephrased prompt, present multiple interpretations, explain limitations." \
    '["track-c", "llm", "error-handling", "phase-2"]' 10

echo "Creating Track C Phase 3 issues..."

create_issue "C3.1: Implement validation orchestrator" \
    "Create service for orchestrating validation. Check cache, compile to Z3, run validation, store results. Parallel validation." \
    '["track-c", "validation", "phase-3"]' 11

create_issue "C3.2: Implement countermodel formatting" \
    "Format countermodels for human understanding. Text and structured formats, link countermodel to argument." \
    '["track-c", "validation", "output", "phase-3"]' 11

create_issue "C3.3: Implement repair suggestion generator" \
    "Generate suggestions for strengthening invalid arguments: add premise, weaken conclusion, change logic, resolve ambiguity." \
    '["track-c", "llm", "validation", "phase-3"]' 11

create_issue "C3.4: Implement the execution loop" \
    "Create self-correcting execution loop: translate -> validate -> repair -> re-validate. Supervisor with worker processes." \
    '["track-c", "core", "phase-3"]' 11

create_issue "C3.5: Add explanation generator" \
    "Generate human-readable analysis explanations at technical, intermediate, and accessible levels." \
    '["track-c", "llm", "output", "phase-3"]' 11

echo "Creating Track C Phase 4 issues..."

create_issue "C4.1: Create HTTP API" \
    "Build REST API: POST/GET arguments, analysis endpoints, repair endpoints. OpenAPI spec, well-structured JSON responses." \
    '["track-c", "api", "phase-4"]' 12

create_issue "C4.2: Add WebSocket support for streaming" \
    "Enable real-time updates during analysis. Stream progress events: translation, formalization, validation, completion." \
    '["track-c", "api", "streaming", "phase-4"]' 12

create_issue "C4.3: Create CLI interface" \
    "Build command-line interface: analyze command, show formalization, re-validate, export. Colored output, progress indicators." \
    '["track-c", "cli", "phase-4"]' 12

create_issue "C4.4: Build web interface" \
    "Create web UI: text input, real-time progress, formalization display, countermodel visualization, repair interaction." \
    '["track-c", "web", "phase-4"]' 12

create_issue "C4.5: Add visualization exports" \
    "Generate exportable visualizations: Mermaid diagrams, Graphviz DOT, LaTeX proofs, Markdown reports." \
    '["track-c", "output", "phase-4"]' 12

echo "All issues created!"
