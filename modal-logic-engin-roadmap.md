# Modal Logic Engine Project Roadmap

## Project Overview

This project builds a modal logic analysis system in Gleam, targeting the BEAM runtime. The system uses LLMs as semantic translators (not reasoners) and symbolic logic engines for validation, with a persistence layer that preserves competing interpretations and countermodels.

The project has three parallel development tracks that produce independent, publishable Gleam packages alongside the main application.

### Core Principles

1. **LLMs never assert validity** — they translate natural language to candidate logical structures
2. **Symbolic engines are epistemically authoritative** — Z3 determines validity and generates countermodels
3. **Persistence preserves modal plurality** — competing formalizations coexist; invalidity is informative
4. **The execution loop is self-correcting** — failures trigger repair suggestions and re-validation

### Repository Structure

```
modal-logic-engine/
├── packages/
│   ├── anthropic_gleam/          # Track A: LLM client library
│   ├── z3_gleam/                 # Track B: Z3 bindings
│   └── modal_logic/              # Core domain types (shared)
├── apps/
│   └── analyst/                  # Track C: Main application
├── docs/
│   ├── architecture/
│   ├── api/
│   └── contributing/
├── scripts/
│   └── dev/
├── gleam.toml                    # Workspace configuration
└── README.md
```

---

## Track A: Anthropic Gleam Client Library

**Package name:** `anthropic_gleam`
**Goal:** A well-typed, idiomatic Gleam client for Claude's API with streaming support and tool use
**Publishable:** Yes — broadly useful to Gleam ecosystem

### Phase A1: Core Types and Basic Completion

**Duration:** 2 weeks
**Milestone:** `anthropic-gleam-v0.1.0`

#### Objectives
- Define comprehensive types for the Anthropic API
- Implement basic (non-streaming) message completion
- Establish error handling patterns
- Set up testing infrastructure

#### Deliverables
- Type definitions for all API structures
- Basic HTTP client wrapper
- Configuration management
- Unit tests with mocked responses

#### Issues

```yaml
- title: "A1.1: Define core message types"
  labels: [track-a, types, phase-1]
  body: |
    Define Gleam types for the Anthropic Messages API:
    
    ## Types to define
    - `Message` with role (user/assistant) and content
    - `ContentBlock` as sum type (text, image, tool_use, tool_result)
    - `TextBlock` with text content
    - `ImageSource` with base64 and media type
    - `ToolUseBlock` with id, name, and input
    - `ToolResultBlock` with tool_use_id and content
    
    ## Acceptance criteria
    - [ ] All types defined in `src/anthropic/types/message.gleam`
    - [ ] Types are fully documented with @doc comments
    - [ ] JSON encoding/decoding implemented via gleam_json
    - [ ] Round-trip encoding tests pass

- title: "A1.2: Define API request/response types"
  labels: [track-a, types, phase-1]
  body: |
    Define request and response structures for the Messages API.
    
    ## Types to define
    - `CreateMessageRequest` with model, messages, max_tokens, etc.
    - `CreateMessageResponse` with id, type, role, content, usage, etc.
    - `Usage` with input_tokens and output_tokens
    - `StopReason` enum (end_turn, max_tokens, stop_sequence, tool_use)
    
    ## Acceptance criteria
    - [ ] Request type supports all documented API parameters
    - [ ] Response type captures all API response fields
    - [ ] Optional fields properly typed with Option(a)
    - [ ] JSON encoding for requests, decoding for responses

- title: "A1.3: Define error types"
  labels: [track-a, types, error-handling, phase-1]
  body: |
    Define comprehensive error types for API interactions.
    
    ## Error categories
    - `ApiError` — errors returned by Anthropic API
      - authentication_error
      - invalid_request_error  
      - rate_limit_error
      - api_error
      - overloaded_error
    - `HttpError` — transport-level errors
    - `JsonError` — encoding/decoding failures
    - `ConfigError` — missing API key, invalid configuration
    
    ## Acceptance criteria
    - [ ] Sum type covering all error categories
    - [ ] Error messages preserve API error details
    - [ ] Errors are displayable (implement string conversion)

- title: "A1.4: Implement configuration management"
  labels: [track-a, config, phase-1]
  body: |
    Create configuration structure and loading.
    
    ## Configuration options
    - `api_key` — required, from parameter or environment
    - `base_url` — optional, defaults to Anthropic API
    - `default_model` — optional default model string
    - `timeout_ms` — request timeout
    - `max_retries` — retry count for transient errors
    
    ## Loading sources
    1. Explicit configuration passed to client
    2. Environment variables (ANTHROPIC_API_KEY)
    
    ## Acceptance criteria
    - [ ] Config type defined with sensible defaults
    - [ ] Environment variable loading works
    - [ ] Missing API key produces clear ConfigError
    - [ ] Base URL override works for testing/proxies

- title: "A1.5: Implement basic HTTP client wrapper"
  labels: [track-a, http, phase-1]
  body: |
    Create HTTP client wrapper for API calls.
    
    ## Requirements
    - Use hackney via gleam_httpc or similar
    - Set required headers (x-api-key, content-type, anthropic-version)
    - Handle response status codes appropriately
    - Parse error responses into ApiError type
    
    ## Acceptance criteria
    - [ ] POST requests work with JSON body
    - [ ] Headers correctly set including API version
    - [ ] 4xx/5xx responses parsed as appropriate error type
    - [ ] Timeout handling works

- title: "A1.6: Implement create_message function"
  labels: [track-a, api, phase-1]
  body: |
    Implement the core message creation function.
    
    ## Function signature
    ```gleam
    pub fn create_message(
      client: Client,
      request: CreateMessageRequest,
    ) -> Result(CreateMessageResponse, AnthropicError)
    ```
    
    ## Behavior
    - Validate request (non-empty messages, valid model)
    - Encode request to JSON
    - Make HTTP POST to /v1/messages
    - Decode response or error
    
    ## Acceptance criteria
    - [ ] Basic completion works with simple text message
    - [ ] Multi-turn conversations work
    - [ ] System prompts work
    - [ ] Error responses properly typed

- title: "A1.7: Set up test infrastructure"
  labels: [track-a, testing, phase-1]
  body: |
    Create testing utilities and initial test suite.
    
    ## Test infrastructure
    - Mock HTTP responses for unit tests
    - Test fixtures for common API responses
    - Integration test setup (requires API key, skipped in CI by default)
    
    ## Initial tests
    - JSON encoding/decoding round-trips
    - Error parsing
    - Basic completion (mocked)
    - Configuration loading
    
    ## Acceptance criteria
    - [ ] `gleam test` runs unit tests without API key
    - [ ] Integration tests runnable with ANTHROPIC_API_KEY set
    - [ ] Test coverage for all type conversions
```

### Phase A2: Streaming Support

**Duration:** 2 weeks
**Milestone:** `anthropic-gleam-v0.2.0`
**Depends on:** A1

#### Objectives
- Implement Server-Sent Events parsing
- Create streaming message interface
- Handle stream events properly
- Support partial message accumulation

#### Issues

```yaml
- title: "A2.1: Define streaming event types"
  labels: [track-a, types, streaming, phase-2]
  body: |
    Define types for streaming API events.
    
    ## Event types
    - `message_start` — contains Message with empty content
    - `content_block_start` — new content block beginning
    - `content_block_delta` — incremental content update
    - `content_block_stop` — content block complete
    - `message_delta` — message-level updates (stop_reason, usage)
    - `message_stop` — stream complete
    - `ping` — keepalive
    - `error` — stream error
    
    ## Delta types
    - `TextDelta` with text fragment
    - `InputJsonDelta` for tool input streaming
    
    ## Acceptance criteria
    - [ ] All event types defined as sum type
    - [ ] Delta types properly structured
    - [ ] JSON decoding works for all event types
    - [ ] Event type field correctly parsed

- title: "A2.2: Implement SSE parser"
  labels: [track-a, streaming, parser, phase-2]
  body: |
    Create Server-Sent Events parser for streaming responses.
    
    ## SSE format
    ```
    event: message_start
    data: {"type": "message_start", ...}
    
    event: content_block_delta
    data: {"type": "content_block_delta", ...}
    ```
    
    ## Parser requirements
    - Handle multi-line data fields
    - Parse event type and data separately
    - Handle connection keepalives
    - Gracefully handle malformed events
    
    ## Acceptance criteria
    - [ ] Parser extracts event type and data
    - [ ] Multi-line data concatenated correctly
    - [ ] Empty lines (event boundaries) handled
    - [ ] Unit tests with real API response samples

- title: "A2.3: Implement streaming HTTP handler"
  labels: [track-a, http, streaming, phase-2]
  body: |
    Create HTTP handler for streaming responses.
    
    ## Requirements
    - Initiate streaming request (stream: true)
    - Process response body as chunks arrive
    - Parse chunks through SSE parser
    - Yield parsed events
    
    ## BEAM considerations
    - Use erlang httpc or hackney streaming mode
    - Consider process-based streaming (each event as message)
    - Handle connection drops gracefully
    
    ## Acceptance criteria
    - [ ] Streaming request initiates correctly
    - [ ] Events emitted as they arrive
    - [ ] Connection cleanup on completion/error
    - [ ] Timeout handling for stalled streams

- title: "A2.4: Implement stream accumulator"
  labels: [track-a, streaming, phase-2]
  body: |
    Create utility for accumulating stream into complete message.
    
    ## Accumulator state
    - Current message (updated from message_start, message_delta)
    - Content blocks (accumulated from deltas)
    - Final usage statistics
    
    ## Interface options
    1. Callback-based: `stream_message(client, request, on_event)`
    2. Iterator-based: returns stream of events
    3. Accumulating: returns final Message after consuming stream
    
    ## Acceptance criteria
    - [ ] Text content correctly accumulated from deltas
    - [ ] Tool use input JSON correctly assembled
    - [ ] Final message matches non-streaming equivalent
    - [ ] All three interface styles available

- title: "A2.5: Add streaming integration tests"
  labels: [track-a, testing, streaming, phase-2]
  body: |
    Create integration tests for streaming functionality.
    
    ## Test cases
    - Simple text streaming
    - Long response (many chunks)
    - Multi-block response (text + tool_use)
    - Error mid-stream
    - Timeout handling
    
    ## Acceptance criteria
    - [ ] Streaming produces same final content as non-streaming
    - [ ] Events arrive incrementally (not batched at end)
    - [ ] Error handling works mid-stream
```

### Phase A3: Tool Use Support

**Duration:** 2 weeks  
**Milestone:** `anthropic-gleam-v0.3.0`
**Depends on:** A2

#### Objectives
- Define tool/function schema types
- Implement tool use in requests
- Handle tool results in responses
- Create ergonomic tool definition helpers

#### Issues

```yaml
- title: "A3.1: Define tool schema types"
  labels: [track-a, types, tools, phase-3]
  body: |
    Define types for tool definitions following Anthropic's schema.
    
    ## Types
    - `Tool` with name, description, input_schema
    - `InputSchema` representing JSON Schema subset
    - `PropertySchema` for individual properties
    - Schema types: string, number, integer, boolean, array, object
    
    ## JSON Schema support (minimum viable)
    - type field
    - description
    - properties (for objects)
    - required array
    - items (for arrays)
    - enum (for constrained strings)
    
    ## Acceptance criteria
    - [ ] Tool definition type matches API spec
    - [ ] JSON Schema subset sufficient for common tools
    - [ ] Encoding produces valid API requests
    - [ ] Examples in documentation

- title: "A3.2: Implement tool use in requests"
  labels: [track-a, api, tools, phase-3]
  body: |
    Add tool definitions to message requests.
    
    ## Request changes
    - Add `tools` field to CreateMessageRequest
    - Add `tool_choice` field (auto, any, tool, none)
    
    ## Tool choice types
    - `Auto` — model decides
    - `Any` — must use a tool
    - `Tool(name)` — must use specific tool
    - `None` — no tool use
    
    ## Acceptance criteria
    - [ ] Tools serialize correctly in request
    - [ ] Tool choice options all work
    - [ ] Request validation catches invalid tool definitions

- title: "A3.3: Handle tool use responses"
  labels: [track-a, api, tools, phase-3]
  body: |
    Process tool use in responses.
    
    ## Response handling
    - Parse ToolUseBlock in content array
    - Extract tool name, id, and input
    - Parse input as dynamic JSON (since schema varies)
    
    ## Convenience functions
    - `get_tool_uses(response) -> List(ToolUseBlock)`
    - `has_tool_use(response) -> Bool`
    - `get_tool_input(block, key) -> Result(Json, Error)`
    
    ## Acceptance criteria
    - [ ] Tool use blocks parsed correctly
    - [ ] Input JSON accessible
    - [ ] Works with streaming responses
    - [ ] Multiple tool uses in single response handled

- title: "A3.4: Implement tool result submission"
  labels: [track-a, api, tools, phase-3]
  body: |
    Create utilities for submitting tool results.
    
    ## Flow
    1. Receive response with tool_use blocks
    2. Execute tools (user code)
    3. Submit tool_result blocks in next request
    4. Receive final response
    
    ## Helper functions
    - `tool_result(tool_use_id, content)` — create result block
    - `tool_error(tool_use_id, error)` — create error result
    - Continue conversation with tool results
    
    ## Acceptance criteria
    - [ ] Tool result format matches API spec
    - [ ] Multi-tool result submission works
    - [ ] Error results handled appropriately
    - [ ] Integration test with real tool use cycle

- title: "A3.5: Create tool definition builder"
  labels: [track-a, tools, ergonomics, phase-3]
  body: |
    Create ergonomic builder for tool definitions.
    
    ## Builder pattern
    ```gleam
    tool("get_weather")
    |> description("Get current weather for a location")
    |> param("location", String, required: True, 
             description: "City name")
    |> param("unit", Enum(["celsius", "fahrenheit"]), 
             required: False)
    |> build()
    ```
    
    ## Goals
    - Type-safe parameter definitions
    - Clear error messages for invalid schemas
    - Documentation examples
    
    ## Acceptance criteria
    - [ ] Builder produces valid tool definitions
    - [ ] Common patterns are ergonomic
    - [ ] Invalid configurations caught at build time
    - [ ] Documentation with examples
```

### Phase A4: Production Readiness

**Duration:** 2 weeks
**Milestone:** `anthropic-gleam-v1.0.0`
**Depends on:** A3

#### Issues

```yaml
- title: "A4.1: Implement retry logic"
  labels: [track-a, reliability, phase-4]
  body: |
    Add automatic retry for transient failures.
    
    ## Retry conditions
    - Rate limit errors (429) — respect Retry-After header
    - Overloaded errors (529)
    - Network timeouts
    - Connection errors
    
    ## Configuration
    - max_retries (default 3)
    - base_delay_ms (default 1000)
    - max_delay_ms (default 60000)
    - exponential backoff with jitter
    
    ## Acceptance criteria
    - [ ] Rate limits trigger appropriate delay
    - [ ] Retry-After header respected
    - [ ] Exponential backoff works
    - [ ] Max retries enforced
    - [ ] Non-retryable errors fail immediately

- title: "A4.2: Add request validation"
  labels: [track-a, validation, phase-4]
  body: |
    Validate requests before sending to API.
    
    ## Validations
    - Non-empty messages list
    - Valid model string
    - max_tokens within bounds
    - Tool definitions have required fields
    - System prompt valid if present
    
    ## Behavior
    - Return ValidationError before HTTP call
    - Clear error messages indicating problem
    
    ## Acceptance criteria
    - [ ] Invalid requests caught locally
    - [ ] Error messages actionable
    - [ ] Valid requests pass through unchanged

- title: "A4.3: Implement logging hooks"
  labels: [track-a, observability, phase-4]
  body: |
    Add optional logging/telemetry hooks.
    
    ## Hook points
    - Request start (model, message count)
    - Response received (tokens used, duration)
    - Error occurred (error type, retry count)
    - Stream events (for debugging)
    
    ## Implementation
    - Optional callback in client config
    - Default: no logging
    - Example logger implementation in docs
    
    ## Acceptance criteria
    - [ ] Hooks fire at appropriate points
    - [ ] No performance impact when disabled
    - [ ] Example implementations documented

- title: "A4.4: Write comprehensive documentation"
  labels: [track-a, documentation, phase-4]
  body: |
    Create thorough documentation for the library.
    
    ## Documentation
    - README with quick start
    - API reference (generated from @doc)
    - Examples directory with common patterns
    - CHANGELOG
    - CONTRIBUTING guide
    
    ## Examples to include
    - Basic completion
    - Streaming with UI updates
    - Tool use cycle
    - Error handling patterns
    - Custom configuration
    
    ## Acceptance criteria
    - [ ] README sufficient for getting started
    - [ ] All public functions documented
    - [ ] Examples are runnable
    - [ ] Published to Hex with docs

- title: "A4.5: Publish to Hex"
  labels: [track-a, release, phase-4]
  body: |
    Prepare and publish package to Hex.pm.
    
    ## Pre-publish checklist
    - [ ] Version set to 1.0.0
    - [ ] License file present (MIT or Apache-2.0)
    - [ ] gleam.toml metadata complete
    - [ ] All tests passing
    - [ ] Documentation builds
    - [ ] CHANGELOG updated
    
    ## Hex metadata
    - name: anthropic_gleam
    - description: Typed Gleam client for Anthropic's Claude API
    - links: GitHub, documentation
    - licenses: [appropriate license]
    
    ## Acceptance criteria
    - [ ] Package published to hex.pm
    - [ ] Documentation available on hexdocs
    - [ ] Installation instructions work
```

---

## Track B: Z3 Gleam Bindings

**Package name:** `z3_gleam`
**Goal:** Gleam bindings to Z3 theorem prover via NIF or Port
**Publishable:** Yes — useful for constraint solving, SMT, verification

### Phase B1: Design and Prototype

**Duration:** 2 weeks
**Milestone:** `z3-gleam-design`

#### Objectives
- Evaluate NIF vs Port tradeoffs
- Design Gleam type mappings for Z3 concepts
- Create minimal proof-of-concept
- Document design decisions

#### Issues

```yaml
- title: "B1.1: Research Z3 API surface"
  labels: [track-b, research, phase-1]
  body: |
    Document Z3's C API relevant to our use cases.
    
    ## Key Z3 concepts
    - Context: configuration and memory management
    - Solver: assertion and checking
    - AST: expressions, sorts, declarations
    - Model: satisfying assignments
    
    ## API functions needed
    - Context creation/destruction
    - Sort creation (bool, int, real, uninterpreted)
    - Expression building (and, or, not, implies, forall, exists)
    - Solver operations (assert, check, get_model)
    - Model inspection
    
    ## Deliverable
    - Document listing required Z3 functions
    - Notes on memory management requirements
    - Complexity assessment for NIF wrapper

- title: "B1.2: Design Gleam type mappings"
  labels: [track-b, types, design, phase-1]
  body: |
    Design how Z3 concepts map to Gleam types.
    
    ## Type mappings
    
    ### Sorts
    ```gleam
    pub type Sort {
      BoolSort
      IntSort
      RealSort
      UninterpretedSort(name: String)
      ArraySort(domain: Sort, range: Sort)
    }
    ```
    
    ### Expressions
    ```gleam
    pub type Expr {
      // Literals
      BoolLit(Bool)
      IntLit(Int)
      
      // Variables
      Const(name: String, sort: Sort)
      
      // Boolean operations
      And(List(Expr))
      Or(List(Expr))
      Not(Expr)
      Implies(Expr, Expr)
      Iff(Expr, Expr)
      
      // Quantifiers
      ForAll(vars: List(#(String, Sort)), body: Expr)
      Exists(vars: List(#(String, Sort)), body: Expr)
      
      // Arithmetic
      Add(List(Expr))
      Mul(List(Expr))
      // etc.
    }
    ```
    
    ### Results
    ```gleam
    pub type CheckResult {
      Sat(Model)
      Unsat
      Unknown(reason: String)
    }
    ```
    
    ## Deliverable
    - Complete type definitions
    - Rationale for design choices
    - Comparison with other Z3 bindings (Python, Rust)

- title: "B1.3: Evaluate NIF vs Port"
  labels: [track-b, architecture, research, phase-1]
  body: |
    Compare implementation strategies.
    
    ## NIF approach
    **Pros:**
    - Direct function calls, lowest latency
    - Natural memory sharing
    - Can expose Z3's incremental API
    
    **Cons:**
    - Long-running NIFs block scheduler (Z3 can be slow)
    - Crashes take down VM
    - More complex build (need C toolchain)
    - Platform-specific binaries
    
    **Mitigations:**
    - Dirty NIF schedulers for long operations
    - Careful error handling
    
    ## Port approach
    **Pros:**
    - Isolation: crashes don't affect VM
    - Simpler build
    - Can be language-agnostic (Python/C driver)
    
    **Cons:**
    - Serialization overhead
    - Harder to maintain Z3 context across calls
    - Latency for many small operations
    
    ## Recommendation
    Document recommendation with rationale.
    
    ## Acceptance criteria
    - [ ] Both approaches prototyped (minimal)
    - [ ] Performance comparison documented
    - [ ] Recommendation made with rationale

- title: "B1.4: Create Port-based prototype"
  labels: [track-b, prototype, phase-1]
  body: |
    Build minimal Port-based Z3 integration.
    
    ## Components
    1. Small Python or C program that:
       - Reads JSON commands from stdin
       - Executes Z3 operations
       - Writes JSON results to stdout
    
    2. Gleam Port wrapper that:
       - Spawns the driver process
       - Sends encoded commands
       - Parses responses
    
    ## Commands to support
    - `declare_const(name, sort)`
    - `assert(expr)`
    - `check()`
    - `get_model()`
    - `reset()`
    
    ## Acceptance criteria
    - [ ] Can declare boolean constants
    - [ ] Can assert simple formulas
    - [ ] Can check satisfiability
    - [ ] Can retrieve model values

- title: "B1.5: Create NIF-based prototype"
  labels: [track-b, prototype, nif, phase-1]
  body: |
    Build minimal NIF-based Z3 integration.
    
    ## Components
    1. C NIF module that:
       - Links against Z3
       - Creates/destroys contexts
       - Builds expressions from Erlang terms
       - Runs solver and returns results
    
    2. Gleam wrapper that:
       - Loads NIF
       - Provides typed interface
       - Handles resource lifecycle
    
    ## NIF functions
    - `z3_context_new()` -> Resource
    - `z3_solver_new(Context)` -> Resource
    - `z3_mk_bool_const(Context, Name)` -> ExprRef
    - `z3_mk_and(Context, List)` -> ExprRef
    - `z3_solver_assert(Solver, Expr)` -> ok
    - `z3_solver_check(Solver)` -> sat | unsat | unknown
    - `z3_solver_get_model(Solver)` -> Model terms
    
    ## Acceptance criteria
    - [ ] NIF compiles and loads
    - [ ] Same functionality as Port prototype
    - [ ] No crashes on invalid input
    - [ ] Resource cleanup works
```

### Phase B2: Core Implementation

**Duration:** 3 weeks
**Milestone:** `z3-gleam-v0.1.0`
**Depends on:** B1

#### Issues

```yaml
- title: "B2.1: Implement expression builder"
  labels: [track-b, api, phase-2]
  body: |
    Create idiomatic Gleam API for building Z3 expressions.
    
    ## Builder interface
    ```gleam
    // Variables
    let x = bool("x")
    let y = bool("y")
    let n = int("n")
    
    // Expressions  
    let formula = and([x, implies(y, x), not(y)])
    let arithmetic = gt(n, int_lit(0))
    ```
    
    ## Implementation
    - Pure Gleam expression building (no Z3 calls yet)
    - Expressions are data structures
    - Compilation to Z3 is separate step
    
    ## Acceptance criteria
    - [ ] All boolean operations
    - [ ] All comparison operations
    - [ ] Arithmetic operations
    - [ ] Quantifiers
    - [ ] Pretty-printing for debugging

- title: "B2.2: Implement solver interface"
  labels: [track-b, api, phase-2]
  body: |
    Create solver management interface.
    
    ## Interface
    ```gleam
    pub fn new_solver() -> Result(Solver, Z3Error)
    pub fn assert(solver: Solver, expr: Expr) -> Result(Nil, Z3Error)
    pub fn check(solver: Solver) -> Result(CheckResult, Z3Error)
    pub fn get_model(solver: Solver) -> Result(Model, Z3Error)
    pub fn push(solver: Solver) -> Result(Nil, Z3Error)
    pub fn pop(solver: Solver) -> Result(Nil, Z3Error)
    pub fn reset(solver: Solver) -> Result(Nil, Z3Error)
    ```
    
    ## CheckResult
    - `Sat(Model)` — satisfiable with model
    - `Unsat` — unsatisfiable  
    - `Unknown(String)` — timeout or incomplete
    
    ## Acceptance criteria
    - [ ] Basic sat/unsat checking works
    - [ ] Model extraction works
    - [ ] Push/pop scoping works
    - [ ] Timeout handling works

- title: "B2.3: Implement model inspection"
  labels: [track-b, api, phase-2]
  body: |
    Create interface for inspecting satisfying models.
    
    ## Interface
    ```gleam
    pub fn eval(model: Model, expr: Expr) -> Result(Value, Z3Error)
    pub fn get_const_interp(model: Model, name: String) -> Result(Value, Z3Error)
    pub fn model_to_dict(model: Model) -> Dict(String, Value)
    ```
    
    ## Value type
    ```gleam
    pub type Value {
      BoolVal(Bool)
      IntVal(Int)
      RealVal(Float)
      UnknownVal(String)  // For uninterpreted sorts
    }
    ```
    
    ## Acceptance criteria
    - [ ] Boolean values extractable
    - [ ] Integer values extractable
    - [ ] All declared constants in model_to_dict
    - [ ] Works with both Port and NIF backends

- title: "B2.4: Implement expression compilation"
  labels: [track-b, compiler, phase-2]
  body: |
    Compile Gleam Expr to Z3 internal representation.
    
    ## Compilation process
    1. Traverse Gleam Expr tree
    2. Create corresponding Z3 AST nodes
    3. Handle variable declarations
    4. Return opaque reference for solver use
    
    ## Challenges
    - Z3 contexts must be threaded through
    - Sort inference for variables
    - Handling of let-bindings
    
    ## Acceptance criteria
    - [ ] All expression types compile
    - [ ] Error messages for type mismatches
    - [ ] Efficient handling of shared subexpressions

- title: "B2.5: Add unsat core extraction"
  labels: [track-b, api, phase-2]
  body: |
    Support extracting unsat cores for diagnostics.
    
    ## Interface
    ```gleam
    pub fn assert_named(solver: Solver, name: String, expr: Expr) 
      -> Result(Nil, Z3Error)
    pub fn get_unsat_core(solver: Solver) -> Result(List(String), Z3Error)
    ```
    
    ## Use case
    When a set of constraints is unsatisfiable, unsat cores
    identify a minimal conflicting subset. Essential for
    explaining why arguments are invalid.
    
    ## Acceptance criteria
    - [ ] Named assertions work
    - [ ] Unsat core returned after unsat check
    - [ ] Core is minimal (or close to minimal)
```

### Phase B3: Modal Logic Support

**Duration:** 2 weeks
**Milestone:** `z3-gleam-v0.2.0`
**Depends on:** B2

#### Issues

```yaml
- title: "B3.1: Implement Kripke frame encoding"
  labels: [track-b, modal, phase-3]
  body: |
    Create utilities for encoding modal logic in Z3.
    
    ## Standard translation
    Modal logic encoded in FOL with:
    - Sort `World` (uninterpreted)
    - Relation `R(w1, w2)` for accessibility
    - Predicates `P(w)` for propositional variables at worlds
    
    ## Translation rules
    - `□P` at w  →  `∀w'. R(w,w') → P(w')`
    - `◇P` at w  →  `∃w'. R(w,w') ∧ P(w')`
    - `¬P` at w  →  `¬P(w)`
    - `P ∧ Q` at w  →  `P(w) ∧ Q(w)`
    
    ## Interface
    ```gleam
    pub fn encode_modal(
      proposition: modal_logic.Proposition,
      world: Expr,
      accessibility: Expr,
    ) -> Expr
    ```
    
    ## Acceptance criteria
    - [ ] Box and diamond correctly encoded
    - [ ] Nested modals work
    - [ ] Boolean combinations work
    - [ ] Tests against known valid/invalid formulas

- title: "B3.2: Implement frame condition constraints"
  labels: [track-b, modal, phase-3]
  body: |
    Add constraints for different modal logic systems.
    
    ## Frame conditions
    - K: no constraints (base modal logic)
    - T: reflexive (R(w,w) for all w)
    - 4: transitive (R(w,v) ∧ R(v,u) → R(w,u))
    - 5: euclidean (R(w,v) ∧ R(w,u) → R(v,u))
    - D: serial (∀w. ∃v. R(w,v))
    - S4: reflexive + transitive
    - S5: equivalence relation
    
    ## Interface
    ```gleam
    pub type ModalSystem {
      K
      T
      K4
      S4
      S5
      KD     // Deontic
      KD45   // Deontic S5
    }
    
    pub fn frame_constraints(system: ModalSystem, r: Expr) -> List(Expr)
    ```
    
    ## Acceptance criteria
    - [ ] All listed systems supported
    - [ ] Constraints are correct (verified against literature)
    - [ ] Tests for characteristic formulas of each system

- title: "B3.3: Implement countermodel extraction"
  labels: [track-b, modal, phase-3]
  body: |
    Extract Kripke countermodels from Z3 models.
    
    ## Countermodel structure
    ```gleam
    pub type KripkeModel {
      KripkeModel(
        worlds: List(String),
        accessibility: List(#(String, String)),
        valuation: Dict(String, Set(String)),  // prop -> worlds where true
      )
    }
    ```
    
    ## Extraction process
    1. Get model from Z3
    2. Enumerate world sort values
    3. Extract accessibility relation
    4. Extract proposition valuations
    
    ## Interface
    ```gleam
    pub fn extract_kripke_model(model: Model) -> Result(KripkeModel, Z3Error)
    ```
    
    ## Acceptance criteria
    - [ ] Worlds correctly enumerated
    - [ ] Accessibility pairs extracted
    - [ ] Valuations match Z3 model
    - [ ] Works for small models (3-5 worlds)

- title: "B3.4: Create validity checker"
  labels: [track-b, modal, api, phase-3]
  body: |
    High-level interface for modal validity checking.
    
    ## Interface
    ```gleam
    pub fn check_validity(
      system: ModalSystem,
      premises: List(Proposition),
      conclusion: Proposition,
    ) -> Result(ValidityResult, Z3Error)
    
    pub type ValidityResult {
      Valid
      Invalid(countermodel: KripkeModel)
      Unknown(reason: String)
    }
    ```
    
    ## Implementation
    1. Create fresh world constant for evaluation world
    2. Add frame constraints for system
    3. Assert premises true at evaluation world
    4. Assert conclusion false at evaluation world
    5. Check satisfiability
    6. If sat: extract countermodel (argument invalid)
    7. If unsat: argument valid
    
    ## Acceptance criteria
    - [ ] Correctly identifies valid arguments
    - [ ] Correctly identifies invalid arguments
    - [ ] Countermodels are genuine counterexamples
    - [ ] Tests for standard modal argument forms
```

### Phase B4: Performance and Polish

**Duration:** 2 weeks
**Milestone:** `z3-gleam-v1.0.0`
**Depends on:** B3

#### Issues

```yaml
- title: "B4.1: Implement timeout handling"
  labels: [track-b, reliability, phase-4]
  body: |
    Add configurable timeouts for solver operations.
    
    ## Interface
    ```gleam
    pub fn check_with_timeout(
      solver: Solver, 
      timeout_ms: Int
    ) -> Result(CheckResult, Z3Error)
    ```
    
    ## Implementation options
    - Z3's native timeout parameter
    - BEAM process timeout with cancellation
    - Both (Z3 timeout + hard BEAM timeout)
    
    ## Acceptance criteria
    - [ ] Timeouts actually fire
    - [ ] Returns Unknown with reason on timeout
    - [ ] No resource leaks on timeout
    - [ ] Solver reusable after timeout

- title: "B4.2: Add solver configuration"
  labels: [track-b, api, phase-4]
  body: |
    Expose Z3 configuration options.
    
    ## Configuration options
    - timeout
    - memory limit  
    - random seed (for reproducibility)
    - tactic selection
    - model completion
    
    ## Interface
    ```gleam
    pub type SolverConfig {
      SolverConfig(
        timeout_ms: Option(Int),
        memory_mb: Option(Int),
        seed: Option(Int),
      )
    }
    
    pub fn new_solver_with_config(config: SolverConfig) -> Result(Solver, Z3Error)
    ```
    
    ## Acceptance criteria
    - [ ] Configurations apply correctly
    - [ ] Invalid configs rejected with clear errors
    - [ ] Documented which options matter for performance

- title: "B4.3: Benchmark and optimize"
  labels: [track-b, performance, phase-4]
  body: |
    Profile and optimize critical paths.
    
    ## Benchmarks to create
    - Expression building (1000 nodes)
    - Simple sat check (10 variables)
    - Modal validity (5 worlds, nested operators)
    - Repeated checks (incremental solving)
    
    ## Optimization targets
    - Expression compilation
    - Model extraction
    - NIF call overhead (if applicable)
    
    ## Acceptance criteria
    - [ ] Benchmark suite exists
    - [ ] Performance documented
    - [ ] No obvious bottlenecks remain

- title: "B4.4: Write documentation and examples"
  labels: [track-b, documentation, phase-4]
  body: |
    Create comprehensive documentation.
    
    ## Documentation
    - README with installation and quick start
    - API reference (from @doc)
    - Guide: SMT solving basics
    - Guide: Modal logic encoding
    - Examples directory
    
    ## Examples
    - Simple SAT solving
    - Sudoku solver
    - Modal validity checking
    - Propositional logic tautology checker
    
    ## Acceptance criteria
    - [ ] README sufficient for getting started
    - [ ] Modal logic guide understandable to non-experts
    - [ ] Examples run successfully
    - [ ] Published to Hex with docs

- title: "B4.5: Publish to Hex"
  labels: [track-b, release, phase-4]
  body: |
    Prepare and publish package.
    
    ## Pre-publish checklist
    - [ ] Version set appropriately
    - [ ] License file present
    - [ ] gleam.toml metadata complete
    - [ ] All tests passing
    - [ ] Documentation builds
    - [ ] Build instructions for NIF (if used)
    
    ## Platform considerations
    - Document Z3 installation requirements
    - Consider pre-built binaries for common platforms
    - Fallback instructions for building from source
    
    ## Acceptance criteria
    - [ ] Package published to hex.pm
    - [ ] Installation works on Linux/Mac
    - [ ] Windows support documented (even if manual)
```

---

## Track C: Main Application (Modal Logic Analyst)

**Application name:** `analyst`
**Goal:** The modal logic analysis system integrating LLM translation and Z3 validation
**Depends on:** Tracks A and B (phases can overlap)

### Phase C1: Domain Model and Persistence

**Duration:** 3 weeks
**Milestone:** `analyst-v0.1.0`

#### Issues

```yaml
- title: "C1.1: Define core domain types"
  labels: [track-c, types, phase-1]
  body: |
    Define the core domain model for modal logic analysis.
    
    ## Types (in packages/modal_logic)
    
    ### Propositions
    ```gleam
    pub type Proposition {
      Atom(String)
      Not(Proposition)
      And(Proposition, Proposition)
      Or(Proposition, Proposition)
      Implies(Proposition, Proposition)
      Necessary(Proposition)
      Possible(Proposition)
      Obligatory(Proposition)
      Permitted(Proposition)
      Knows(agent: String, Proposition)
      Believes(agent: String, Proposition)
    }
    ```
    
    ### Arguments
    ```gleam
    pub type Argument {
      Argument(
        id: String,
        natural_language: String,
        formalizations: List(Formalization),
        ambiguities: List(Ambiguity),
        created_at: Time,
        updated_at: Time,
      )
    }
    
    pub type Formalization {
      Formalization(
        id: String,
        logic_system: LogicSystem,
        premises: List(Proposition),
        conclusion: Proposition,
        validation: Option(ValidationResult),
        assumptions: List(String),
      )
    }
    
    pub type ValidationResult {
      Valid
      Invalid(countermodel: KripkeModel)
      Timeout
      Error(String)
    }
    ```
    
    ### Ambiguities
    ```gleam
    pub type Ambiguity {
      ModalAmbiguity(
        term: String,
        interpretations: List(ModalInterpretation),
      )
      ScopeAmbiguity(
        description: String,
        readings: List(String),
      )
    }
    
    pub type ModalInterpretation {
      Epistemic   // knowledge/belief
      Deontic     // obligation/permission
      Alethic     // necessity/possibility
      Temporal    // always/eventually
    }
    ```
    
    ## Acceptance criteria
    - [ ] All types defined and documented
    - [ ] JSON encoding/decoding for persistence
    - [ ] Pretty-printing for propositions
    - [ ] Equality and comparison defined

- title: "C1.2: Set up PostgreSQL schema"
  labels: [track-c, persistence, phase-1]
  body: |
    Design and implement database schema.
    
    ## Tables
    
    ### arguments
    ```sql
    CREATE TABLE arguments (
      id UUID PRIMARY KEY,
      natural_language TEXT NOT NULL,
      ambiguities JSONB DEFAULT '[]',
      created_at TIMESTAMPTZ DEFAULT NOW(),
      updated_at TIMESTAMPTZ DEFAULT NOW()
    );
    ```
    
    ### formalizations
    ```sql
    CREATE TABLE formalizations (
      id UUID PRIMARY KEY,
      argument_id UUID REFERENCES arguments(id),
      logic_system VARCHAR(50) NOT NULL,
      premises JSONB NOT NULL,
      conclusion JSONB NOT NULL,
      assumptions JSONB DEFAULT '[]',
      created_at TIMESTAMPTZ DEFAULT NOW()
    );
    ```
    
    ### validations
    ```sql
    CREATE TABLE validations (
      id UUID PRIMARY KEY,
      formalization_id UUID REFERENCES formalizations(id),
      status VARCHAR(20) NOT NULL,
      countermodel JSONB,
      duration_ms INT,
      engine VARCHAR(50),
      created_at TIMESTAMPTZ DEFAULT NOW()
    );
    ```
    
    ### repair_suggestions
    ```sql
    CREATE TABLE repair_suggestions (
      id UUID PRIMARY KEY,
      validation_id UUID REFERENCES validations(id),
      suggestion_type VARCHAR(50),
      description TEXT,
      proposed_change JSONB,
      applied BOOLEAN DEFAULT FALSE,
      created_at TIMESTAMPTZ DEFAULT NOW()
    );
    ```
    
    ## Acceptance criteria
    - [ ] Migrations created
    - [ ] Indexes on foreign keys
    - [ ] JSONB columns properly typed
    - [ ] Gleam types map cleanly to schema

- title: "C1.3: Implement repository layer"
  labels: [track-c, persistence, phase-1]
  body: |
    Create data access layer using gleam_pgo.
    
    ## Repository functions
    
    ### Arguments
    ```gleam
    pub fn create_argument(nl: String) -> Result(Argument, DbError)
    pub fn get_argument(id: String) -> Result(Argument, DbError)
    pub fn update_argument(arg: Argument) -> Result(Argument, DbError)
    pub fn list_arguments(limit: Int, offset: Int) -> Result(List(Argument), DbError)
    ```
    
    ### Formalizations
    ```gleam
    pub fn add_formalization(arg_id: String, f: Formalization) -> Result(Formalization, DbError)
    pub fn get_formalizations(arg_id: String) -> Result(List(Formalization), DbError)
    ```
    
    ### Validations
    ```gleam
    pub fn record_validation(form_id: String, result: ValidationResult) -> Result(Validation, DbError)
    pub fn get_validation_history(form_id: String) -> Result(List(Validation), DbError)
    ```
    
    ## Acceptance criteria
    - [ ] All CRUD operations work
    - [ ] Transactions for multi-table operations
    - [ ] Connection pooling configured
    - [ ] Error handling comprehensive

- title: "C1.4: Implement caching layer"
  labels: [track-c, caching, phase-1]
  body: |
    Add Redis caching for validation results.
    
    ## Cache strategy
    - Key: hash of (logic_system, premises, conclusion)
    - Value: serialized ValidationResult
    - TTL: configurable (default 24 hours)
    
    ## Interface
    ```gleam
    pub fn cache_validation(
      formalization: Formalization,
      result: ValidationResult,
    ) -> Result(Nil, CacheError)
    
    pub fn get_cached_validation(
      formalization: Formalization,
    ) -> Result(Option(ValidationResult), CacheError)
    
    pub fn invalidate_cache(
      formalization: Formalization,
    ) -> Result(Nil, CacheError)
    ```
    
    ## Normalization
    - Propositions normalized before hashing
    - Alpha-equivalence for bound variables
    - Commutative operations sorted
    
    ## Acceptance criteria
    - [ ] Cache hits return stored results
    - [ ] Cache misses return None
    - [ ] TTL expiration works
    - [ ] Normalization produces consistent keys

- title: "C1.5: Create argument graph queries"
  labels: [track-c, persistence, phase-1]
  body: |
    Implement graph-oriented queries for arguments.
    
    ## Queries needed
    
    ### Formalization comparison
    - Get all formalizations for argument
    - Compare validation results across formalizations
    - Find formalizations with same conclusion, different validity
    
    ### Cross-argument analysis
    - Find arguments with similar structure
    - Find arguments using same inference patterns
    - Track which logic systems tend to validate/invalidate
    
    ## Implementation
    Use PostgreSQL recursive CTEs or consider
    adding a graph layer later.
    
    ## Acceptance criteria
    - [ ] Basic graph queries work
    - [ ] Performance acceptable for 1000s of arguments
    - [ ] Results useful for analysis
```

### Phase C2: LLM Translation Pipeline

**Duration:** 3 weeks
**Milestone:** `analyst-v0.2.0`
**Depends on:** C1, A2

#### Issues

```yaml
- title: "C2.1: Design translation prompts"
  labels: [track-c, llm, prompts, phase-2]
  body: |
    Create prompts for logical structure extraction.
    
    ## Prompt design principles
    - LLM identifies structure, not syntax
    - Output is JSON matching our types
    - Multiple interpretations encouraged
    - Ambiguities explicitly flagged
    
    ## Prompt template
    ```
    Analyze the following argument and extract its logical structure.
    
    Argument: "{natural_language}"
    
    Identify:
    1. The premises (stated or implied)
    2. The conclusion
    3. Any modal operators (necessity, possibility, obligation, permission, knowledge, belief)
    4. Any ambiguities in interpretation
    
    For each distinct interpretation, provide a formalization.
    
    Output JSON matching this schema:
    {schema}
    ```
    
    ## Output schema
    ```json
    {
      "interpretations": [
        {
          "logic_system": "KD",
          "premises": [...],
          "conclusion": {...},
          "assumptions": ["..."],
          "confidence": 0.8
        }
      ],
      "ambiguities": [
        {
          "term": "should",
          "possible_readings": ["deontic_obligation", "epistemic_expectation"]
        }
      ]
    }
    ```
    
    ## Acceptance criteria
    - [ ] Prompt produces valid JSON
    - [ ] Multiple interpretations generated when appropriate
    - [ ] Ambiguities correctly identified
    - [ ] Test on 20 sample arguments

- title: "C2.2: Implement translation service"
  labels: [track-c, llm, service, phase-2]
  body: |
    Create service for translating arguments via LLM.
    
    ## Interface
    ```gleam
    pub fn translate_argument(
      client: anthropic.Client,
      natural_language: String,
    ) -> Result(TranslationResult, TranslationError)
    
    pub type TranslationResult {
      TranslationResult(
        formalizations: List(Formalization),
        ambiguities: List(Ambiguity),
        raw_response: String,
      )
    }
    ```
    
    ## Implementation
    1. Format prompt with argument text
    2. Call Claude API with JSON mode
    3. Parse response into domain types
    4. Handle parsing failures gracefully
    
    ## Error handling
    - API errors → retry with backoff
    - Invalid JSON → log and request retry
    - Missing fields → use defaults where sensible
    
    ## Acceptance criteria
    - [ ] Basic translation works
    - [ ] JSON parsing handles variations
    - [ ] Errors don't crash service
    - [ ] Raw response preserved for debugging

- title: "C2.3: Implement structure compiler"
  labels: [track-c, compiler, phase-2]
  body: |
    Compile LLM output to internal Proposition types.
    
    ## Compilation steps
    1. Parse JSON structure from LLM
    2. Validate structure (required fields, valid operators)
    3. Build Proposition AST
    4. Resolve references (shared atoms)
    
    ## JSON to Proposition mapping
    ```json
    {"type": "implies", 
     "antecedent": {"type": "possible", "body": {"type": "atom", "name": "rain"}},
     "consequent": {"type": "obligatory", "body": {"type": "atom", "name": "umbrella"}}}
    ```
    →
    ```gleam
    Implies(
      Possible(Atom("rain")),
      Obligatory(Atom("umbrella"))
    )
    ```
    
    ## Acceptance criteria
    - [ ] All operator types compile
    - [ ] Invalid structures produce clear errors
    - [ ] Nested structures handled correctly
    - [ ] Atom names normalized

- title: "C2.4: Add logic system detection"
  labels: [track-c, llm, phase-2]
  body: |
    Determine appropriate logic system for arguments.
    
    ## Detection heuristics
    - "must", "necessary" → alethic modal (T, S4, S5)
    - "should", "ought", "obligatory" → deontic (KD)
    - "knows", "believes" → epistemic (S5 for knowledge, KD45 for belief)
    - "always", "eventually" → temporal (LTL)
    - Multiple agents → multi-agent epistemic
    
    ## Implementation
    - LLM suggests logic system in output
    - Heuristics as fallback/validation
    - Allow user override
    
    ## Interface
    ```gleam
    pub fn suggest_logic_system(
      propositions: List(Proposition),
    ) -> List(#(LogicSystem, Float))  // System with confidence
    ```
    
    ## Acceptance criteria
    - [ ] Correct detection for common patterns
    - [ ] Multiple suggestions when ambiguous
    - [ ] User override mechanism works

- title: "C2.5: Handle translation failures"
  labels: [track-c, llm, error-handling, phase-2]
  body: |
    Gracefully handle cases where translation fails.
    
    ## Failure modes
    1. LLM returns unparseable response
    2. LLM misunderstands argument structure
    3. Argument genuinely ambiguous
    4. Argument not amenable to formal analysis
    
    ## Strategies
    - Retry with rephrased prompt (modes 1, 2)
    - Present multiple interpretations (mode 3)
    - Explain limitation to user (mode 4)
    
    ## Feedback loop
    - Store failed translations
    - Use failures to improve prompts
    - Allow user correction
    
    ## Acceptance criteria
    - [ ] Retries work for transient failures
    - [ ] User sees helpful error messages
    - [ ] Failed attempts logged for analysis
```

### Phase C3: Validation Pipeline

**Duration:** 3 weeks
**Milestone:** `analyst-v0.3.0`
**Depends on:** C2, B3

#### Issues

```yaml
- title: "C3.1: Implement validation orchestrator"
  labels: [track-c, validation, phase-3]
  body: |
    Create service for orchestrating validation.
    
    ## Interface
    ```gleam
    pub fn validate_formalization(
      formalization: Formalization,
    ) -> Result(ValidationResult, ValidationError)
    
    pub fn validate_argument(
      argument: Argument,
    ) -> Result(List(#(Formalization, ValidationResult)), ValidationError)
    ```
    
    ## Orchestration flow
    1. Check cache for existing result
    2. If miss, compile to Z3
    3. Run validation with timeout
    4. Store result in cache and database
    5. Return result
    
    ## Parallel validation
    - Validate multiple formalizations concurrently
    - Use BEAM processes for natural parallelism
    - Aggregate results
    
    ## Acceptance criteria
    - [ ] Single formalization validation works
    - [ ] Multiple formalizations validated in parallel
    - [ ] Cache integration works
    - [ ] Database recording works

- title: "C3.2: Implement countermodel formatting"
  labels: [track-c, validation, output, phase-3]
  body: |
    Format countermodels for human understanding.
    
    ## Countermodel representation
    Kripke model needs to be presented as:
    - List of worlds with names
    - Accessibility relation (as pairs or graph)
    - Which propositions hold at which worlds
    - Designated "actual world" where premises true, conclusion false
    
    ## Output formats
    
    ### Text format
    ```
    Countermodel found:
    Worlds: w0 (actual), w1, w2
    Accessibility: w0→w1, w0→w2, w1→w2
    
    At w0: rain=false, umbrella=false
    At w1: rain=true, umbrella=false
    At w2: rain=false, umbrella=true
    
    This shows: It's possible that it rains (true at w1),
    but you're not obligated to take an umbrella (false at w0).
    ```
    
    ### Structured format (for UI)
    ```gleam
    pub type FormattedCountermodel {
      FormattedCountermodel(
        worlds: List(FormattedWorld),
        edges: List(#(String, String)),
        explanation: String,
      )
    }
    ```
    
    ## Acceptance criteria
    - [ ] Text format is readable
    - [ ] Structured format supports visualization
    - [ ] Explanation links countermodel to argument

- title: "C3.3: Implement repair suggestion generator"
  labels: [track-c, llm, validation, phase-3]
  body: |
    Generate suggestions for strengthening invalid arguments.
    
    ## Repair types
    1. **Add premise**: "If you also assume X, the argument becomes valid"
    2. **Weaken conclusion**: "A weaker conclusion Y does follow"
    3. **Change logic**: "Under logic system Z, this is valid"
    4. **Resolve ambiguity**: "If 'should' means X rather than Y, valid"
    
    ## Implementation
    - Analyze countermodel to identify failure point
    - Use LLM to suggest natural language repairs
    - Validate suggested repairs automatically
    
    ## Interface
    ```gleam
    pub fn suggest_repairs(
      formalization: Formalization,
      countermodel: KripkeModel,
    ) -> Result(List(RepairSuggestion), Error)
    
    pub type RepairSuggestion {
      AddPremise(premise: Proposition, natural: String)
      WeakenConclusion(new_conclusion: Proposition, natural: String)
      ChangeLogic(new_system: LogicSystem, explanation: String)
      ResolveAmbiguity(resolution: String, resulting_formalization: Formalization)
    }
    ```
    
    ## Acceptance criteria
    - [ ] At least one repair type implemented
    - [ ] Suggestions are actionable
    - [ ] Repaired arguments can be re-validated

- title: "C3.4: Implement the execution loop"
  labels: [track-c, core, phase-3]
  body: |
    Create the self-correcting execution loop.
    
    ## Loop structure
    ```
    1. Receive argument (natural language)
    2. Translate → N formalizations
    3. For each formalization:
       a. Validate
       b. If invalid, generate repair suggestions
       c. Optionally apply repairs and re-validate
    4. Store all results
    5. Return analysis
    ```
    
    ## Process architecture
    - Supervisor for the analysis pipeline
    - Worker processes for each formalization
    - Aggregator for collecting results
    
    ## Configuration
    - max_formalizations: limit on interpretations to try
    - max_repair_iterations: limit on repair cycles
    - timeout_per_validation: per-formalization timeout
    - total_timeout: overall analysis timeout
    
    ## Acceptance criteria
    - [ ] Full loop executes end-to-end
    - [ ] Parallel validation works
    - [ ] Timeouts enforced at all levels
    - [ ] Results properly aggregated

- title: "C3.5: Add explanation generator"
  labels: [track-c, llm, output, phase-3]
  body: |
    Generate human-readable analysis explanations.
    
    ## Explanation levels
    
    ### Technical
    Full formal details: propositions, inference steps, countermodel
    
    ### Intermediate  
    Logical structure with inference rule names, but in 
    semi-natural language
    
    ### Accessible
    Plain language summary suitable for non-logicians
    
    ## Implementation
    Use LLM to synthesize explanation from:
    - Original argument
    - Chosen formalization
    - Validation result
    - Countermodel (if invalid)
    - Repair suggestions
    
    ## Interface
    ```gleam
    pub type ExplanationLevel {
      Technical
      Intermediate
      Accessible
    }
    
    pub fn generate_explanation(
      analysis: AnalysisResult,
      level: ExplanationLevel,
    ) -> Result(String, Error)
    ```
    
    ## Acceptance criteria
    - [ ] All three levels produce output
    - [ ] Accessible level understandable by non-experts
    - [ ] Technical level complete and accurate
```

### Phase C4: Interface and Integration

**Duration:** 3 weeks
**Milestone:** `analyst-v1.0.0`
**Depends on:** C3

#### Issues

```yaml
- title: "C4.1: Create HTTP API"
  labels: [track-c, api, phase-4]
  body: |
    Build REST API for the analysis service.
    
    ## Endpoints
    
    ### Arguments
    - `POST /arguments` — create and analyze argument
    - `GET /arguments/:id` — get argument with analysis
    - `GET /arguments` — list arguments
    
    ### Analysis
    - `POST /arguments/:id/analyze` — re-run analysis
    - `GET /arguments/:id/formalizations` — get all formalizations
    - `GET /formalizations/:id/validate` — re-validate single formalization
    
    ### Repairs
    - `POST /formalizations/:id/repair` — apply repair suggestion
    - `GET /formalizations/:id/repairs` — get repair suggestions
    
    ## Response format
    ```json
    {
      "argument": {...},
      "formalizations": [...],
      "analysis_summary": {
        "valid_count": 1,
        "invalid_count": 2,
        "ambiguities": [...]
      }
    }
    ```
    
    ## Acceptance criteria
    - [ ] All endpoints implemented
    - [ ] JSON responses well-structured
    - [ ] Error responses informative
    - [ ] OpenAPI spec generated

- title: "C4.2: Add WebSocket support for streaming"
  labels: [track-c, api, streaming, phase-4]
  body: |
    Enable real-time updates during analysis.
    
    ## Use case
    Analysis can take 10-30 seconds. Stream progress:
    - Translation started
    - Formalization N generated
    - Validation started for formalization N
    - Validation complete (result)
    - Repairs generated
    - Analysis complete
    
    ## Protocol
    ```json
    // Client sends
    {"type": "analyze", "argument": "..."}
    
    // Server streams
    {"type": "progress", "stage": "translating"}
    {"type": "formalization", "index": 0, "data": {...}}
    {"type": "validation_start", "formalization_index": 0}
    {"type": "validation_complete", "formalization_index": 0, "result": {...}}
    {"type": "complete", "summary": {...}}
    ```
    
    ## Acceptance criteria
    - [ ] WebSocket connection works
    - [ ] Progress events stream in real-time
    - [ ] Connection cleanup on client disconnect
    - [ ] Error events properly formatted

- title: "C4.3: Create CLI interface"
  labels: [track-c, cli, phase-4]
  body: |
    Build command-line interface for the analyzer.
    
    ## Commands
    ```bash
    # Analyze argument
    analyst analyze "Since it might rain, you should take an umbrella"
    
    # Analyze from file
    analyst analyze --file argument.txt
    
    # Show specific formalization
    analyst show <argument-id> --formalization 0
    
    # Re-validate with different logic
    analyst validate <formalization-id> --logic S5
    
    # Export analysis
    analyst export <argument-id> --format json|markdown
    ```
    
    ## Output formatting
    - Colored terminal output
    - Progress indicators for long operations
    - Countermodel visualization (ASCII)
    
    ## Acceptance criteria
    - [ ] Basic analyze command works
    - [ ] Output is readable and well-formatted
    - [ ] Exit codes meaningful
    - [ ] Help text complete

- title: "C4.4: Build web interface"
  labels: [track-c, web, phase-4]
  body: |
    Create web UI for argument analysis.
    
    ## Framework
    Use Wisp or similar Gleam web framework.
    Consider whether to server-render or build
    separate frontend (likely the latter).
    
    ## Features
    - Text input for arguments
    - Real-time analysis progress
    - Formalization display with syntax highlighting
    - Countermodel visualization (graph)
    - Repair suggestion interaction
    
    ## Visualization
    - Kripke models as directed graphs
    - Proposition trees
    - Comparison of formalizations
    
    ## Acceptance criteria
    - [ ] Argument submission works
    - [ ] Analysis results display clearly
    - [ ] Countermodel graph renders
    - [ ] Mobile-responsive

- title: "C4.5: Add visualization exports"
  labels: [track-c, output, phase-4]
  body: |
    Generate exportable visualizations.
    
    ## Export formats
    
    ### Mermaid diagrams
    ```mermaid
    graph LR
      w0((w0)) --> w1((w1))
      w0 --> w2((w2))
      w1 --> w2
    ```
    
    ### Graphviz DOT
    For more control over layout.
    
    ### LaTeX
    For academic use, export proofs in natural deduction style.
    
    ### Markdown report
    Complete analysis as markdown document.
    
    ## Acceptance criteria
    - [ ] Mermaid export works
    - [ ] Markdown report comprehensive
    - [ ] Exports embed correctly in documents
```

---

## Project Management Structure

### GitHub Labels

```yaml
# Track labels
- name: track-a
  color: "1f77b4"
  description: "Anthropic Gleam client library"
- name: track-b
  color: "ff7f0e"  
  description: "Z3 Gleam bindings"
- name: track-c
  color: "2ca02c"
  description: "Main application"

# Phase labels  
- name: phase-1
  color: "c5def5"
- name: phase-2
  color: "bfd4f2"
- name: phase-3
  color: "a2c4e0"
- name: phase-4
  color: "85b4ce"

# Type labels
- name: types
  color: "d4c5f9"
  description: "Type definitions"
- name: api
  color: "f9d4c5"
  description: "API implementation"
- name: testing
  color: "c5f9d4"
  description: "Tests and test infrastructure"
- name: documentation
  color: "f9f9c5"
  description: "Documentation"
- name: research
  color: "e0e0e0"
  description: "Research and design"
- name: reliability
  color: "ff9999"
  description: "Error handling, retries, resilience"
- name: performance
  color: "99ff99"
  description: "Performance optimization"
```

### GitHub Projects

Create three project boards:

#### Project: Anthropic Gleam Client
- **Columns**: Backlog, Design, In Progress, Review, Done
- **Linked to**: Issues with `track-a` label

#### Project: Z3 Gleam Bindings  
- **Columns**: Backlog, Research, In Progress, Review, Done
- **Linked to**: Issues with `track-b` label

#### Project: Modal Logic Analyst
- **Columns**: Backlog, Design, In Progress, Review, Done
- **Linked to**: Issues with `track-c` label

### Milestones

```yaml
- title: "anthropic-gleam-v0.1.0"
  description: "Core types and basic completion"
  due_date: "+2 weeks"

- title: "anthropic-gleam-v0.2.0"
  description: "Streaming support"
  due_date: "+4 weeks"

- title: "anthropic-gleam-v0.3.0"
  description: "Tool use support"
  due_date: "+6 weeks"

- title: "anthropic-gleam-v1.0.0"
  description: "Production ready, published to Hex"
  due_date: "+8 weeks"

- title: "z3-gleam-design"
  description: "Design complete, prototype working"
  due_date: "+2 weeks"

- title: "z3-gleam-v0.1.0"
  description: "Core solver functionality"
  due_date: "+5 weeks"

- title: "z3-gleam-v0.2.0"
  description: "Modal logic support"
  due_date: "+7 weeks"

- title: "z3-gleam-v1.0.0"
  description: "Production ready, published to Hex"
  due_date: "+9 weeks"

- title: "analyst-v0.1.0"
  description: "Domain model and persistence"
  due_date: "+3 weeks"

- title: "analyst-v0.2.0"
  description: "LLM translation pipeline"
  due_date: "+6 weeks"

- title: "analyst-v0.3.0"
  description: "Validation pipeline complete"
  due_date: "+9 weeks"

- title: "analyst-v1.0.0"
  description: "Full application with interfaces"
  due_date: "+12 weeks"
```

---

## Dependency Graph

```
Track A: Anthropic Client
  A1 → A2 → A3 → A4
  
Track B: Z3 Bindings
  B1 → B2 → B3 → B4

Track C: Main Application
  C1 (can start immediately)
    ↓
  C2 (needs A2 for streaming)
    ↓
  C3 (needs B3 for modal validation)
    ↓
  C4 (needs C3 complete)

Cross-track dependencies:
  A2 ----→ C2
  B3 ----→ C3
```

## Quick Start for Claude Code

### Initial Setup Commands

```bash
# Create repository structure
mkdir -p modal-logic-engine/{packages/{anthropic_gleam,z3_gleam,modal_logic},apps/analyst,docs/{architecture,api,contributing},scripts/dev}

# Initialize Gleam projects
cd modal-logic-engine/packages/anthropic_gleam && gleam new . --name anthropic_gleam
cd ../z3_gleam && gleam new . --name z3_gleam
cd ../modal_logic && gleam new . --name modal_logic
cd ../../apps/analyst && gleam new . --name analyst

# Set up workspace (create root gleam.toml manually or via script)
```

### GitHub Setup Commands

```bash
# Create labels
gh label create track-a --color 1f77b4 --description "Anthropic Gleam client library"
gh label create track-b --color ff7f0e --description "Z3 Gleam bindings"
gh label create track-c --color 2ca02c --description "Main application"
gh label create phase-1 --color c5def5
gh label create phase-2 --color bfd4f2
gh label create phase-3 --color a2c4e0
gh label create phase-4 --color 85b4ce
gh label create types --color d4c5f9 --description "Type definitions"
gh label create api --color f9d4c5 --description "API implementation"
gh label create testing --color c5f9d4 --description "Tests and test infrastructure"
gh label create documentation --color f9f9c5 --description "Documentation"

# Create milestones
gh milestone create "anthropic-gleam-v0.1.0" --due "2025-01-26"
# ... (continue for all milestones)

# Create projects
gh project create "Anthropic Gleam Client" --org <org>
gh project create "Z3 Gleam Bindings" --org <org>
gh project create "Modal Logic Analyst" --org <org>
```

### Issue Creation

Issues are defined in YAML format above. To create them:

```bash
# Example for first issue
gh issue create \
  --title "A1.1: Define core message types" \
  --label "track-a,types,phase-1" \
  --milestone "anthropic-gleam-v0.1.0" \
  --body "$(cat <<'EOF'
Define Gleam types for the Anthropic Messages API:

## Types to define
- `Message` with role (user/assistant) and content
- `ContentBlock` as sum type (text, image, tool_use, tool_result)
...
EOF
)"
```

---

## Notes for Implementation

### Gleam-Specific Patterns

1. **Result types everywhere**: All fallible operations return `Result(a, Error)`
2. **Use `use` for early returns**: Gleam's `use` keyword for monadic binding
3. **Opaque types for resources**: NIF resources should be opaque
4. **Builders for complex construction**: Use builder pattern for complex configs

### BEAM Patterns

1. **Supervisors for fault tolerance**: Every long-running process under supervision
2. **GenServer for stateful processes**: Arguments could be GenServers
3. **ETS for shared state**: Consider for validation cache if Redis overhead too high
4. **Dirty schedulers for NIFs**: Long Z3 operations on dirty schedulers

### Testing Strategy

1. **Unit tests**: All pure functions
2. **Integration tests**: API clients, database operations
3. **Property tests**: Logical properties (if P valid in S5, valid in S4)
4. **Golden tests**: Known valid/invalid arguments produce expected results
