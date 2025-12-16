# QA Testing Guide for Modal Logic Package

This document provides comprehensive testing procedures for validating the functionality of the `modal_logic` package. Follow these procedures to ensure all components work correctly.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Quick Validation](#quick-validation)
3. [Domain Layer Tests](#domain-layer-tests)
4. [Translation Layer Tests](#translation-layer-tests)
5. [Validation Layer Tests](#validation-layer-tests)
6. [Persistence Layer Tests](#persistence-layer-tests)
7. [Interface Layer Tests](#interface-layer-tests)
8. [Integration Tests](#integration-tests)
9. [Performance Tests](#performance-tests)
10. [Regression Testing](#regression-testing)

---

## Prerequisites

Before running tests, ensure you have:

- Gleam >= 1.13.0 installed
- Erlang/OTP >= 27.0 installed
- Project dependencies installed (`gleam deps download`)

```bash
# Verify installation
gleam --version
erl -version

# Install dependencies
cd packages/modal_logic
gleam deps download
```

---

## Quick Validation

Run these commands to quickly validate the package is working:

```bash
# Build the project (should complete without errors)
gleam build

# Run all tests (all should pass)
gleam test

# Run each test module individually
gleam run -m translation_test
gleam run -m validation_test
gleam run -m persistence_test
gleam run -m interface_test
```

### Expected Output

All tests should produce output ending with:
```
All [Module Name] Tests Passed!
```

---

## Domain Layer Tests

### Test 1: Proposition Type Construction

**Objective:** Verify all proposition types can be constructed correctly.

**Procedure:**
1. Run `gleam run -m persistence_test`
2. Verify "Core Domain Types" section passes

**Expected Results:**
- [OK] Atom proposition created
- [OK] Nested propositions created
- [OK] Logic systems enumerated

**Manual Verification:**
```gleam
import modal_logic/proposition.{Atom, Not, And, Or, Implies, Necessary, Possible}

// Create basic propositions
let p = Atom("p")
let q = Atom("q")
let not_p = Not(p)
let p_and_q = And(p, q)
let p_implies_q = Implies(p, q)
let necessary_p = Necessary(p)
let possible_q = Possible(q)
```

### Test 2: Argument Types

**Objective:** Verify argument and formalization types work correctly.

**Procedure:**
1. Review test output for argument creation
2. Verify ValidationResult types are handled

**Expected Results:**
- Arguments can be created with all required fields
- Formalizations link to arguments correctly
- ValidationResult variants are exhaustively handled

---

## Translation Layer Tests

### Test 3: Prompt Generation

**Objective:** Verify LLM prompts are generated correctly.

**Procedure:**
1. Run `gleam run -m translation_test`
2. Check "Prompt Generation" section

**Expected Results:**
- [OK] Default prompt config created
- [OK] Translation prompt built
- [OK] System message contains required elements
- [OK] Response schema is valid JSON

**Verification Checklist:**
- [ ] System message includes role definition
- [ ] User message contains the argument text
- [ ] Response schema specifies premises, conclusion, logic_system
- [ ] Few-shot examples are included when configured

### Test 4: Structure Compiler

**Objective:** Verify JSON compilation to internal types.

**Procedure:**
1. Run `gleam run -m translation_test`
2. Check "Structure Compiler" section

**Expected Results:**
- [OK] Valid JSON compiled successfully
- [OK] Invalid JSON produces appropriate errors
- [OK] Propositions are correctly typed

**Test Cases:**

| Input | Expected Output |
|-------|-----------------|
| `{"type": "atom", "name": "p"}` | `Atom("p")` |
| `{"type": "not", "prop": {"type": "atom", "name": "p"}}` | `Not(Atom("p"))` |
| `{"type": "necessary", "prop": {...}}` | `Necessary(...)` |
| Invalid JSON | `CompileError` |

### Test 5: Logic Detection

**Objective:** Verify appropriate logic systems are detected from text.

**Procedure:**
1. Run `gleam run -m translation_test`
2. Check "Logic Detection" section

**Expected Results:**
- [OK] Modal words extracted from text
- [OK] Logic system recommended based on modal patterns
- [OK] Detection confidence is reasonable (> 0.5)

**Test Scenarios:**

| Input Text | Expected System | Reason |
|------------|-----------------|--------|
| "necessarily true" | K or S4 | Alethic modality |
| "it is possible that" | K | Basic possibility |
| "everyone knows" | S4/S5 | Epistemic pattern |
| "you ought to" | KD | Deontic pattern |
| "in all possible worlds" | S5 | Strong necessity |

### Test 6: Translation Service

**Objective:** Verify end-to-end translation service.

**Procedure:**
1. Run `gleam run -m translation_test`
2. Check "Translation Service" section

**Expected Results:**
- [OK] Service configuration created
- [OK] Translation request formatted correctly
- [OK] Error handling works for various failure modes

---

## Validation Layer Tests

### Test 7: Validator Configuration

**Objective:** Verify validator configurations are created correctly.

**Procedure:**
1. Run `gleam run -m validation_test`
2. Check "Validator Configuration" section

**Expected Results:**
- [OK] Default validator config created
- [OK] Fast config has shorter timeout
- [OK] Thorough config has longer timeout
- [OK] Config modification works

**Configuration Variants:**

| Config | Timeout | Max Worlds | Use Case |
|--------|---------|------------|----------|
| Default | 5000ms | 10 | Standard validation |
| Fast | 2000ms | 5 | Quick checks |
| Thorough | 15000ms | 20 | Complex arguments |

### Test 8: SMT Generation

**Objective:** Verify SMT-LIB formulas are generated correctly.

**Procedure:**
1. Run `gleam run -m validation_test`
2. Check "SMT Generation" section

**Expected Results:**
- [OK] SMT formula generated
- [OK] Contains check-sat command
- [OK] Contains get-model command
- [OK] Modal formulas handled correctly

**SMT Structure Verification:**
```smt2
; Generated SMT should include:
(set-logic QF_UF)           ; Logic declaration
(declare-sort World 0)       ; World sort
(declare-fun R (World World) Bool)  ; Accessibility relation
; ... proposition definitions ...
(check-sat)
(get-model)
```

### Test 9: Countermodel Formatting

**Objective:** Verify countermodels are formatted for human consumption.

**Procedure:**
1. Run `gleam run -m validation_test`
2. Check "Countermodel Formatting" section

**Expected Results:**
- [OK] Default format created
- [OK] Brief format is shorter
- [OK] Detailed format includes more info
- [OK] Diagram format produces ASCII art

**Format Comparison:**

| Format | Includes Worlds | Includes Relations | ASCII Diagram |
|--------|-----------------|-------------------|---------------|
| Brief | Yes | Minimal | No |
| Standard | Yes | Yes | No |
| Detailed | Yes | Yes + properties | No |
| Diagram | Yes | Yes | Yes |

### Test 10: Repair Suggestions

**Objective:** Verify repair suggestions are generated for invalid arguments.

**Procedure:**
1. Run `gleam run -m validation_test`
2. Check "Repair Suggestions" section

**Expected Results:**
- [OK] Repair suggestions generated
- [OK] Multiple suggestion types available
- [OK] Confidence scores assigned
- [OK] Conservative config produces fewer suggestions

**Repair Types:**

| Type | Description | When Suggested |
|------|-------------|----------------|
| AddPremise | Add a new premise | Missing necessary assumption |
| StrengthenPremise | Make premise stronger | Premise too weak |
| WeakenConclusion | Make conclusion weaker | Conclusion too strong |
| ChangeLogicSystem | Use different logic | System mismatch |
| ModifyModality | Change □/◇ operators | Modal operator issue |

### Test 11: Execution Loop

**Objective:** Verify self-correcting execution loop.

**Procedure:**
1. Run `gleam run -m validation_test`
2. Check "Execution Loop" section

**Expected Results:**
- [OK] Default execution config created
- [OK] Valid validator produces success
- [OK] Invalid validator correctly identifies invalid argument
- [OK] Execution trace recorded

### Test 12: Explanation Generator

**Objective:** Verify human-readable explanations are generated.

**Procedure:**
1. Run `gleam run -m validation_test`
2. Check "Explanation Generator" section

**Expected Results:**
- [OK] Valid argument explanation generated
- [OK] Invalid argument explanation generated
- [OK] Brief/Detailed/Technical levels work
- [OK] Follow-up questions suggested

**Explanation Levels:**

| Level | Target Audience | Detail |
|-------|-----------------|--------|
| Brief | General | 1-2 sentences |
| Standard | Student | Paragraph with key points |
| Detailed | Philosophy student | Full explanation |
| Technical | Logic expert | Formal details |

---

## Persistence Layer Tests

### Test 13: Cache Operations

**Objective:** Verify caching system works correctly.

**Procedure:**
1. Run `gleam run -m persistence_test`
2. Check "Cache Operations" section

**Expected Results:**
- [OK] Cache config created
- [OK] Cache key generated from formalization
- [OK] Cache put/get works
- [OK] TTL expiration handled

**Cache Behavior:**

| Operation | Expected Result |
|-----------|-----------------|
| First lookup | Cache miss |
| After put | Cache hit |
| After TTL expiry | Cache miss |
| Clear | All entries removed |

### Test 14: Repository SQL

**Objective:** Verify SQL query building.

**Procedure:**
1. Run `gleam run -m persistence_test`
2. Check "Repository SQL Builders" section

**Expected Results:**
- [OK] Query options applied
- [OK] Filters work correctly
- [OK] Pagination works

### Test 15: Graph Queries

**Objective:** Verify graph-based argument analysis.

**Procedure:**
1. Run `gleam run -m persistence_test`
2. Check "Graph Queries" section

**Expected Results:**
- [OK] Similarity detection works
- [OK] Relationship types identified
- [OK] Pattern extraction works

---

## Interface Layer Tests

### Test 16: HTTP API

**Objective:** Verify REST API functionality.

**Procedure:**
1. Run `gleam run -m interface_test`
2. Check "HTTP API" section

**Expected Results:**
- [OK] Default API config created
- [OK] Router created with 8 routes
- [OK] Request handled successfully
- [OK] OpenAPI spec generated

**API Endpoints:**

| Method | Path | Description |
|--------|------|-------------|
| POST | /api/arguments | Submit argument |
| GET | /api/arguments/:id | Get argument by ID |
| POST | /api/analyze | Analyze text |
| POST | /api/validate | Validate formalization |
| GET | /api/repairs/:id | Get repair suggestions |
| GET | /api/systems | List logic systems |
| GET | /api/health | Health check |
| OPTIONS | /* | CORS preflight |

### Test 17: WebSocket

**Objective:** Verify WebSocket functionality.

**Procedure:**
1. Run `gleam run -m interface_test`
2. Check "WebSocket" section

**Expected Results:**
- [OK] WebSocket config created
- [OK] Handler created
- [OK] Connection add/remove works
- [OK] Progress events created

**Event Types:**

| Event | When Sent |
|-------|-----------|
| analysis_started | Analysis begins |
| translation_progress | During translation |
| formalization_complete | Translation done |
| validation_progress | During validation |
| validation_complete | Validation done |
| analysis_complete | All done |

### Test 18: CLI Interface

**Objective:** Verify command-line interface.

**Procedure:**
1. Run `gleam run -m interface_test`
2. Check "CLI Interface" section

**Expected Results:**
- [OK] Config variants created
- [OK] Commands parsed correctly
- [OK] Help text generated
- [OK] Interactive mode works

**Command Parsing:**

| Input | Expected Command |
|-------|------------------|
| `help` | HelpCommand |
| `version` | VersionCommand |
| `analyze "text"` | AnalyzeCommand |
| `validate ...` | ValidateCommand |
| `systems` | ListSystemsCommand |
| `interactive` | InteractiveCommand |

### Test 19: Web Interface

**Objective:** Verify HTML generation.

**Procedure:**
1. Run `gleam run -m interface_test`
2. Check "Web Interface" section

**Expected Results:**
- [OK] HTML elements rendered
- [OK] Nested elements work
- [OK] Form elements have attributes
- [OK] Full pages generated with DOCTYPE

**HTML Output Verification:**
- Contains `<!DOCTYPE html>`
- Contains proper `<html>`, `<head>`, `<body>` structure
- CSS styles included
- JavaScript functionality included

### Test 20: Visualization

**Objective:** Verify visualization exports.

**Procedure:**
1. Run `gleam run -m interface_test`
2. Check "Visualization" section

**Expected Results:**
- [OK] Mermaid export with `graph` directive
- [OK] Graphviz export with `digraph` directive
- [OK] LaTeX export with `tikzpicture` environment
- [OK] Markdown export generated
- [OK] Proposition formatting (LaTeX, Unicode, ASCII)

**Export Format Verification:**

| Format | Key Elements |
|--------|--------------|
| Mermaid | `graph TB`, node definitions, edges |
| Graphviz | `digraph`, `rankdir`, node/edge specs |
| LaTeX | `\begin{tikzpicture}`, `\node`, `\draw` |
| Markdown | Headers, tables, code blocks |

---

## Integration Tests

### Test 21: End-to-End Validation Flow

**Objective:** Test complete flow from formalization to result.

**Procedure:**
```gleam
// Create formalization
let form = create_test_formalization()

// Validate
let config = validator.default_config()
let result = validator.validate(form, config)

// Generate explanation
let explanation = explanation.explain_validation(form, result, StandardLevel)

// If invalid, generate repairs
case result {
  Invalid(cm) -> {
    let repairs = repair.generate_suggestions(form, cm)
    // Verify repairs exist and are appropriate
  }
  _ -> Nil
}
```

**Expected Flow:**
1. Formalization created successfully
2. Validation completes within timeout
3. Result is either Valid or Invalid with countermodel
4. Explanation is human-readable
5. Repairs (if applicable) are actionable

### Test 22: Visualization Pipeline

**Objective:** Test visualization from countermodel to all formats.

**Procedure:**
```gleam
// Create Kripke model
let model = create_test_model()

// Export to all formats
let mermaid = visualization.to_mermaid(model)
let graphviz = visualization.to_graphviz(model)
let latex = visualization.to_latex(model)
let bundle = visualization.export_all(model)

// Verify each format
assert string.contains(mermaid, "graph")
assert string.contains(graphviz, "digraph")
assert string.contains(latex, "tikzpicture")
```

---

## Performance Tests

### Test 23: Validation Performance

**Objective:** Ensure validation completes in reasonable time.

**Benchmarks:**

| Scenario | Target Time |
|----------|-------------|
| Simple argument (2 premises) | < 100ms |
| Medium argument (5 premises) | < 500ms |
| Complex argument (10 premises) | < 2000ms |

### Test 24: Cache Performance

**Objective:** Verify cache improves performance.

**Expected:**
- Cache hit should be < 1ms
- Cache miss + validation should be normal time
- Cache hit ratio > 80% for repeated queries

---

## Regression Testing

### Known Issues to Verify

1. **Logic System Handling:**
   - Verify K4, KD, KD45 systems work correctly
   - These were added later and should be fully supported

2. **Proposition Types:**
   - All 11 proposition types should be handled
   - Epistemic (Knows, Believes) and Deontic (Obligatory, Permitted) included

3. **Empty Collections:**
   - Empty premises list handled
   - Empty worlds in countermodel handled
   - Empty repair suggestions handled

### Test Checklist

Run before each release:

- [ ] `gleam build` completes without errors
- [ ] `gleam format --check src test` passes
- [ ] `gleam test` passes
- [ ] `gleam run -m translation_test` passes
- [ ] `gleam run -m validation_test` passes
- [ ] `gleam run -m persistence_test` passes
- [ ] `gleam run -m interface_test` passes
- [ ] All CI checks pass on GitHub

---

## Validation Testing Suite

The validation testing suite provides comprehensive testing for modal logic rules against philosophical arguments, external datasets, and automated soundness checking.

### Test Modules

| Test File | Description | Test Count |
|-----------|-------------|------------|
| `rule_builder_test.gleam` | Rule builder DSL tests | 25 |
| `rule_store_test.gleam` | Rule store with versioning | 26 |
| `validation_suite_test.gleam` | Philosophical argument validation | 24 |
| `doc_generator_test.gleam` | Documentation generator | 19 |
| `integration_test.gleam` | End-to-end integration | 21 |

### Running Validation Tests

```bash
# Run all tests (includes validation suite)
gleam test

# Build and verify
gleam build
```

### Rule Builder DSL

The rule builder provides a fluent API for constructing inference rules:

```gleam
import modal_logic/rules/rule_builder

// Use pre-built rules
let mp = rule_builder.modus_ponens()
let mt = rule_builder.modus_tollens()
let nec = rule_builder.necessitation()

// Build custom rules
let assert Ok(rule) =
  rule_builder.inference_rule("custom_rule")
  |> rule_builder.named("Custom Rule")
  |> rule_builder.with_premise(rule_builder.atom("p"))
  |> rule_builder.with_premise(rule_builder.implies(
    rule_builder.atom("p"),
    rule_builder.atom("q")
  ))
  |> rule_builder.derives(rule_builder.atom("q"))
  |> rule_builder.valid_in_all()
  |> rule_builder.build()
```

### Rule Store Operations

```gleam
import modal_logic/rules/rule_store

// Create and populate store
let store = rule_store.standard_store()

// Get rules and axioms
let rules = rule_store.list_rules(store)
let axioms = rule_store.list_axioms(store)

// Export store data
let export = rule_store.export(store)
let summary = rule_store.export_summary(export)
```

### Philosophical Argument Testing

```gleam
import modal_logic/testing/validation/philosophical_tester

// Configure and run tests
let store = rule_store.standard_store()
let config = philosophical_tester.default_config()
let result = philosophical_tester.run_tests(store, config)

// Analyze results
io.println("Tested: " <> int.to_string(result.total_tested))
io.println("Passed: " <> int.to_string(result.correctly_validated))
```

### Soundness Checking

```gleam
import modal_logic/testing/validation/soundness_checker

// Check rule soundness
let store = rule_store.standard_store()
let result = soundness_checker.check_store_soundness(store)

// Review results
io.println("Rules checked: " <> int.to_string(result.rules_checked))
io.println("Sound rules: " <> int.to_string(result.sound_rules))
```

### Documentation Generation

```gleam
import modal_logic/testing/docs/doc_generator

// Generate documentation
let store = rule_store.standard_store()
let config = doc_generator.default_config()
let doc = doc_generator.generate_store_doc(store, config)

// Render to string
let output = doc_generator.render(doc)

// Generate from test results
let test_doc = doc_generator.generate_test_doc(test_result, config)

// Generate from soundness analysis
let soundness_doc = doc_generator.generate_soundness_doc(soundness_result, config)
```

### External Dataset Integration

```gleam
// Stanford Encyclopedia of Philosophy
import modal_logic/testing/external/sep_adapter

let fixtures = sep_adapter.get_mock_fixtures()

// FOLIO dataset
import modal_logic/testing/external/folio_adapter

let config = folio_adapter.default_config()
let folio_fixtures = folio_adapter.get_all_fixtures(config)
```

### Integration Test Workflow

The integration tests verify the complete pipeline:

1. **Rule Building** - Construct rules using DSL
2. **Store Management** - Add, update, and version rules
3. **Argument Corpus** - Test against philosophical arguments
4. **Soundness Checking** - Verify rule soundness
5. **Documentation** - Generate reports from results

```gleam
// Complete workflow example
let store = rule_store.standard_store()
let test_config = philosophical_tester.default_config()
let test_result = philosophical_tester.run_tests(store, test_config)

let doc_config = doc_generator.comprehensive_config()
let doc = doc_generator.generate_test_doc(test_result, doc_config)
let output = doc_generator.render(doc)
```

### Test Categories

| Category | Arguments | Description |
|----------|-----------|-------------|
| Modal | 5+ | Necessity, possibility, S5 reasoning |
| Epistemic | 5+ | Knowledge, belief, common knowledge |
| Deontic | 3+ | Obligation, permission |
| Classical | 5+ | Propositional logic patterns |
| Fallacy | 5+ | Invalid argument patterns |

---

## Troubleshooting

### Common Issues

**Build Fails:**
```bash
# Clean and rebuild
rm -rf build
gleam deps download
gleam build
```

**Test Timeout:**
- Increase timeout in validator config
- Check for infinite loops in recursive functions

**Format Check Fails:**
```bash
gleam format src test
```

**Import Errors:**
- Ensure all module dependencies are correct
- Check for circular imports

---

## Test Coverage Summary

| Layer | Module | Tests | Status |
|-------|--------|-------|--------|
| Domain | proposition | Type construction | ✅ |
| Domain | argument | Argument/Formalization types | ✅ |
| Translation | prompts | Prompt generation | ✅ |
| Translation | compiler | JSON compilation | ✅ |
| Translation | logic_detector | Logic detection | ✅ |
| Translation | translation_service | Service layer | ✅ |
| Validation | validator | SMT generation, validation | ✅ |
| Validation | cache | Caching operations | ✅ |
| Validation | countermodel | Formatting | ✅ |
| Analysis | repair | Repair suggestions | ✅ |
| Analysis | execution | Execution loop | ✅ |
| Analysis | explanation | Explanations | ✅ |
| Persistence | repository | SQL builders | ✅ |
| Persistence | graph | Graph queries | ✅ |
| Interface | api | REST API | ✅ |
| Interface | websocket | WebSocket | ✅ |
| Interface | cli | CLI | ✅ |
| Interface | web | HTML generation | ✅ |
| Interface | visualization | Export formats | ✅ |

---

## Reporting Issues

When reporting test failures:

1. Include the full error message
2. Include the test command run
3. Include Gleam and Erlang versions
4. Include any relevant configuration

File issues at: https://github.com/justin4957/foil/issues
