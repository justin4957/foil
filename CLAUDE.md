# Foil Project Instructions

## Testing Requirements

### Interface Dialogue Tests

When implementing or modifying external integrations, create a "dialogue test" that demonstrates the back-and-forth interaction between the application and the external interface.

#### Purpose
- Validates that the integration layer works correctly
- Documents expected behavior in a readable format
- Provides clear evidence of functionality for PR reviews
- Identifies what's working vs placeholder functionality

#### Required for PRs involving:

1. **Z3/SMT Solver Integration** (`packages/z3_gleam/`)
   - Create `test/z3_dialogue_test.gleam` or similar
   - Show expression building, solver state, model evaluation, compilation
   - Clearly indicate placeholder vs functional components
   - Example: See `packages/z3_gleam/test/z3_dialogue_test.gleam`

2. **LLM Integration** (`lib/multi_agent_coder/`)
   - Create dialogue tests showing request/response flow
   - Demonstrate prompt construction, API calls, response parsing
   - Test error handling and retry logic
   - Use anthropic haiku for testing to save costs

#### Dialogue Test Structure
```
--- Test N: [Component] Dialogue ---
User: [Action or request]
[System]: [Response showing internal state or result]

User: [Next action]
[System]: [Response]
...
```

#### PR Comment Requirements
After running dialogue tests, add a PR comment containing:
1. Full dialogue test output
2. Analysis table showing what's working vs placeholder
3. Architecture validation (if applicable)
4. Any limitations or next steps identified

### General PR Requirements

- All example usages in PR documentation must be tested functional examples
- Clearly mark any placeholder, future enhancement, or unverified functionality
- Link issues in PRs for automatic closure
- Run linter before creating PR
- Ensure CI tests pass after PR creation
- Fetch main and create new branch for each PR task

## Project Structure

- `packages/z3_gleam/` - Z3 SMT solver Gleam bindings
- `lib/multi_agent_coder/` - LLM integration and multi-agent coding
- `apps/` - Application code
- `docs/` - Documentation (update docs/TESTING.md after implementations)
