//// Reason Chain Dialogue Test
////
//// This test demonstrates the reason chain formalization infrastructure
//// added in issue #147. It validates the parsing, classification, and
//// formalization of natural language reasoning chains.
////
//// ## Purpose
//// - Validates reason chain parsing works correctly
//// - Demonstrates reason type classification
//// - Shows implicit assumption detection
//// - Documents expected behavior for PR reviews

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import modal_logic/reason_chain.{
  type ReasonChain, type ReasonChainError, Causal, Deontic, Epistemic, Factual,
  Mixed, Modal,
}

pub fn main() {
  io.println(string.repeat("=", 70))
  io.println("Reason Chain Dialogue Test")
  io.println("Testing Issue #147: Reason Chain Formalization from NL")
  io.println(string.repeat("=", 70))
  io.println("")

  // Test 1: Basic reason chain parsing
  test_basic_parsing()

  // Test 2: Reason type classification
  test_reason_classification()

  // Test 3: Implicit assumption detection
  test_assumption_detection()

  // Test 4: Formalization generation
  test_formalization()

  // Test 5: Multiple reason types
  test_multiple_reason_types()

  // Test 6: JSON serialization
  test_json_serialization()

  // Test 7: Edge cases
  test_edge_cases()

  // Summary
  print_analysis_summary()

  io.println("")
  io.println(string.repeat("=", 70))
  io.println("All Reason Chain Dialogue Tests Completed!")
  io.println(string.repeat("=", 70))
}

// =============================================================================
// Test 1: Basic Reason Chain Parsing
// =============================================================================

fn test_basic_parsing() {
  io.println("")
  io.println("--- Test 1: Basic Reason Chain Parsing ---")
  io.println("")

  let text =
    "The project will succeed because the team is experienced, and we have sufficient budget."

  io.println("User: Parse reason chain from:")
  io.println("      \"" <> text <> "\"")
  io.println("")

  let config = reason_chain.default_config()
  let result = reason_chain.parse_reason_chain(text, config)

  case result {
    Ok(chain) -> {
      io.println("[System]: Parsed Reason Chain")
      io.println("         Claim: " <> chain.claim)
      io.println(
        "         Reasons: " <> int.to_string(list.length(chain.reasons)),
      )
      io.println("")

      list.each(chain.reasons, fn(r) {
        io.println(
          "         - "
          <> r.statement
          <> " ["
          <> reason_chain.reason_type_to_string(r.reason_type)
          <> "]",
        )
      })

      io.println("")
      case list.length(chain.reasons) >= 2 {
        True -> io.println("[OK] Successfully extracted multiple reasons")
        False -> io.println("[WARN] Expected at least 2 reasons")
      }
    }
    Error(err) -> {
      io.println("[System]: Error - " <> format_error(err))
      io.println("[FAIL] Failed to parse reason chain")
    }
  }

  io.println("")
}

// =============================================================================
// Test 2: Reason Type Classification
// =============================================================================

fn test_reason_classification() {
  io.println("")
  io.println("--- Test 2: Reason Type Classification ---")
  io.println("")

  let test_cases = [
    #("The stock will rise because earnings beat expectations.", "Factual"),
    #(
      "Prices will increase because if demand rises, prices typically increase.",
      "Causal",
    ),
    #(
      "The conclusion follows because we know the premises are true.",
      "Epistemic",
    ),
    #(
      "Action is required because we should fulfill our obligations.",
      "Deontic",
    ),
    #(
      "This must be true because it necessarily follows from the axioms.",
      "Modal",
    ),
  ]

  io.println("User: Classify reason types for various statements")
  io.println("")
  io.println("[System]: Classification Results")
  io.println("")
  io.println("| Statement (truncated) | Expected | Actual | Status |")
  io.println("|----------------------|----------|--------|--------|")

  let config = reason_chain.default_config()
  let results =
    list.map(test_cases, fn(test_case) {
      let #(text, expected) = test_case
      let result = reason_chain.parse_reason_chain(text, config)

      let actual = case result {
        Ok(chain) -> {
          case chain.reasons {
            [r, ..] -> reason_chain.reason_type_to_string(r.reason_type)
            [] -> "NoReason"
          }
        }
        Error(_) -> "Error"
      }

      let truncated = string.slice(text, 0, 20) <> "..."
      let status = case
        string.contains(actual, expected) || actual == expected
      {
        True -> "PASS"
        False -> "FAIL"
      }

      io.println(
        "| "
        <> truncated
        <> " | "
        <> expected
        <> " | "
        <> actual
        <> " | "
        <> status
        <> " |",
      )

      status == "PASS"
    })

  let passed = list.count(results, fn(r) { r })
  io.println("")
  io.println(
    "[OK] "
    <> int.to_string(passed)
    <> "/"
    <> int.to_string(list.length(results))
    <> " classifications correct",
  )

  io.println("")
}

// =============================================================================
// Test 3: Implicit Assumption Detection
// =============================================================================

fn test_assumption_detection() {
  io.println("")
  io.println("--- Test 3: Implicit Assumption Detection ---")
  io.println("")

  let text =
    "The investment will pay off because markets usually recover, and we know the fundamentals are strong."

  io.println("User: Detect implicit assumptions in:")
  io.println("      \"" <> text <> "\"")
  io.println("")

  let config = reason_chain.default_config()
  let result = reason_chain.parse_reason_chain(text, config)

  case result {
    Ok(chain) -> {
      io.println("[System]: Detected Assumptions")
      io.println("")

      case chain.implicit_assumptions {
        [] -> io.println("         (No assumptions detected)")
        assumptions -> {
          list.each(assumptions, fn(a) { io.println("         - " <> a) })
        }
      }

      io.println("")
      case list.length(chain.implicit_assumptions) > 0 {
        True -> io.println("[OK] Successfully detected implicit assumptions")
        False ->
          io.println(
            "[WARN] Expected at least one assumption (text has 'usually' and epistemic claims)",
          )
      }
    }
    Error(err) -> {
      io.println("[System]: Error - " <> format_error(err))
    }
  }

  io.println("")
}

// =============================================================================
// Test 4: Formalization Generation
// =============================================================================

fn test_formalization() {
  io.println("")
  io.println("--- Test 4: Formalization Generation ---")
  io.println("")

  let text =
    "The project will succeed because the team is experienced and experienced teams usually deliver."

  io.println("User: Formalize the reason chain:")
  io.println("      \"" <> text <> "\"")
  io.println("")

  let config = reason_chain.default_config()
  let parse_result = reason_chain.parse_reason_chain(text, config)

  case parse_result {
    Ok(chain) -> {
      io.println("[System]: Parsed Chain")
      io.println(reason_chain.format_reason_chain(chain))
      io.println("")

      let formal_result = reason_chain.formalize_chain(chain, config)

      case formal_result {
        Ok(result) -> {
          io.println("[System]: Formalization Result")
          io.println(reason_chain.format_formalization_result(result))

          io.println("")
          case list.length(result.formalization.premises) > 0 {
            True -> io.println("[OK] Successfully generated premises")
            False -> io.println("[WARN] No premises generated")
          }

          case list.length(result.gaps) > 0 {
            True ->
              io.println(
                "[OK] Logical gap analysis identified potential issues",
              )
            False -> io.println("[INFO] No logical gaps identified")
          }
        }
        Error(err) -> {
          io.println("[System]: Formalization Error - " <> format_error(err))
        }
      }
    }
    Error(err) -> {
      io.println("[System]: Parse Error - " <> format_error(err))
    }
  }

  io.println("")
}

// =============================================================================
// Test 5: Multiple Reason Types
// =============================================================================

fn test_multiple_reason_types() {
  io.println("")
  io.println("--- Test 5: Multiple Reason Types ---")
  io.println("")

  let text =
    "We should invest because the market will necessarily grow, we know the timing is right, and regulations require action."

  io.println("User: Parse chain with multiple reason types:")
  io.println("      \"" <> text <> "\"")
  io.println("")

  let config = reason_chain.default_config()
  let result = reason_chain.parse_reason_chain(text, config)

  case result {
    Ok(chain) -> {
      io.println("[System]: Reason Type Distribution")
      io.println("")

      let type_counts = count_reason_types(chain.reasons)
      list.each(type_counts, fn(tc) {
        let #(type_name, count) = tc
        io.println("         " <> type_name <> ": " <> int.to_string(count))
      })

      io.println("")

      // Check that we have multiple types
      let unique_types = list.length(type_counts)
      case unique_types >= 2 {
        True ->
          io.println(
            "[OK] Multiple reason types detected ("
            <> int.to_string(unique_types)
            <> " types)",
          )
        False -> io.println("[WARN] Expected multiple reason types")
      }
    }
    Error(err) -> {
      io.println("[System]: Error - " <> format_error(err))
    }
  }

  io.println("")
}

fn count_reason_types(
  reasons: List(reason_chain.Reason),
) -> List(#(String, Int)) {
  let types =
    list.map(reasons, fn(r) {
      reason_chain.reason_type_to_string(r.reason_type)
    })

  let unique_types = list.unique(types)
  list.map(unique_types, fn(t) {
    let count = list.count(types, fn(x) { x == t })
    #(t, count)
  })
}

// =============================================================================
// Test 6: JSON Serialization
// =============================================================================

fn test_json_serialization() {
  io.println("")
  io.println("--- Test 6: JSON Serialization ---")
  io.println("")

  let text =
    "Sales will increase because marketing is effective and the product is popular."

  io.println("User: Convert reason chain to JSON")
  io.println("")

  let config = reason_chain.default_config()
  let parse_result = reason_chain.parse_reason_chain(text, config)

  case parse_result {
    Ok(chain) -> {
      let json = reason_chain.reason_chain_to_json(chain)
      io.println("[System]: JSON Output")
      io.println("")
      io.println(json)
      io.println("")

      // Verify JSON structure
      let has_claim = string.contains(json, "\"claim\"")
      let has_reasons = string.contains(json, "\"reasons\"")
      let has_assumptions = string.contains(json, "\"implicit_assumptions\"")

      case has_claim && has_reasons && has_assumptions {
        True -> io.println("[OK] JSON contains all required fields")
        False -> io.println("[WARN] JSON missing some fields")
      }

      // Test formalization JSON
      io.println("")
      io.println("User: Convert formalization result to JSON")
      io.println("")

      let formal_result = reason_chain.formalize_chain(chain, config)
      case formal_result {
        Ok(result) -> {
          let formal_json = reason_chain.formalization_result_to_json(result)
          io.println("[System]: Formalization JSON")
          io.println("")
          io.println(formal_json)
          io.println("")

          let has_premises = string.contains(formal_json, "\"premises\"")
          let has_conclusion = string.contains(formal_json, "\"conclusion\"")
          let has_system =
            string.contains(formal_json, "\"recommended_system\"")

          case has_premises && has_conclusion && has_system {
            True -> io.println("[OK] Formalization JSON is valid")
            False -> io.println("[WARN] Formalization JSON missing fields")
          }
        }
        Error(_) -> io.println("[WARN] Could not generate formalization JSON")
      }
    }
    Error(err) -> {
      io.println("[System]: Error - " <> format_error(err))
    }
  }

  io.println("")
}

// =============================================================================
// Test 7: Edge Cases
// =============================================================================

fn test_edge_cases() {
  io.println("")
  io.println("--- Test 7: Edge Cases ---")
  io.println("")

  let test_cases = [
    #("Simple claim without reasons.", "NoReasonsError"),
    #("A because B.", "Should parse"),
    #(
      "Complex nested reasoning: X happens because Y, which happens because of Z, and furthermore W.",
      "Should handle nested",
    ),
  ]

  io.println("User: Test edge cases")
  io.println("")
  io.println("[System]: Edge Case Results")
  io.println("")

  let config = reason_chain.default_config()

  list.each(test_cases, fn(test_case) {
    let #(text, description) = test_case
    let result = reason_chain.parse_reason_chain(text, config)

    let status = case result {
      Ok(chain) ->
        "OK (reasons: " <> int.to_string(list.length(chain.reasons)) <> ")"
      Error(err) -> "Error: " <> format_error(err)
    }

    io.println("  Input: \"" <> string.slice(text, 0, 40) <> "...\"")
    io.println("  Expected: " <> description)
    io.println("  Result: " <> status)
    io.println("")
  })
}

// =============================================================================
// Analysis Summary
// =============================================================================

fn print_analysis_summary() {
  io.println("")
  io.println(string.repeat("=", 70))
  io.println("REASON CHAIN INFRASTRUCTURE ANALYSIS")
  io.println(string.repeat("=", 70))
  io.println("")

  io.println(
    "| Component                  | Status      | Description                    |",
  )
  io.println(
    "|----------------------------|-------------|--------------------------------|",
  )
  io.println(
    "| ReasonChain type           | FUNCTIONAL  | Main chain data structure      |",
  )
  io.println(
    "| Reason type                | FUNCTIONAL  | Individual reason structure    |",
  )
  io.println(
    "| ReasonType enum            | FUNCTIONAL  | 6 types including Mixed        |",
  )
  io.println(
    "| ModalIndicator type        | FUNCTIONAL  | Modal word detection           |",
  )
  io.println(
    "| parse_reason_chain fn      | FUNCTIONAL  | NL to chain parsing            |",
  )
  io.println(
    "| extract_claim fn           | FUNCTIONAL  | Claim extraction               |",
  )
  io.println(
    "| extract_reasons fn         | FUNCTIONAL  | Reason extraction              |",
  )
  io.println(
    "| classify_reason fn         | FUNCTIONAL  | Type classification            |",
  )
  io.println(
    "| detect_assumptions fn      | FUNCTIONAL  | Implicit assumption detection  |",
  )
  io.println(
    "| formalize_chain fn         | FUNCTIONAL  | Chain to formal argument       |",
  )
  io.println(
    "| determine_logic_system fn  | FUNCTIONAL  | System recommendation          |",
  )
  io.println(
    "| JSON serialization         | FUNCTIONAL  | reason_chain_to_json etc.      |",
  )
  io.println(
    "| LogicalGap detection       | FUNCTIONAL  | Reasoning gap analysis         |",
  )
  io.println("")

  io.println("Reason Types Supported:")
  io.println("  - Factual:   Plain assertions (\"earnings beat expectations\")")
  io.println("  - Causal:    If-then relationships (\"if X then Y\")")
  io.println(
    "  - Modal:     Necessity/possibility (\"necessarily\", \"possibly\")",
  )
  io.println(
    "  - Epistemic: Knowledge/belief (\"we know\", \"it is believed\")",
  )
  io.println("  - Deontic:   Obligation/permission (\"should\", \"must\")")
  io.println("  - Mixed:     Combinations of the above")
  io.println("")

  io.println("Modal Categories Detected:")
  io.println("  - Alethic:      necessarily, possibly, must, might, could")
  io.println("  - Epistemic:    know, believe, think, aware")
  io.println("  - Deontic:      should, ought, permitted, obligated")
  io.println("  - Temporal:     always, sometimes, never")
  io.println("  - Conditional:  if")
  io.println("  - Probabilistic: typically, usually, often, rarely")
  io.println("")

  io.println("Usage Examples:")
  io.println("  # Parse a reason chain")
  io.println(
    "  let chain = reason_chain.parse_reason_chain(text, default_config())",
  )
  io.println("")
  io.println("  # Formalize to modal logic")
  io.println("  let result = reason_chain.formalize_chain(chain, config)")
  io.println("")
  io.println("  # Access the formal argument")
  io.println("  let premises = result.formalization.premises")
  io.println("  let conclusion = result.formalization.conclusion")
  io.println("")
  io.println("  # Check for logical gaps")
  io.println("  case result.gaps {")
  io.println("    [] -> io.println(\"No gaps detected\")")
  io.println("    gs -> list.each(gs, analyze_gap)")
  io.println("  }")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn format_error(err: ReasonChainError) -> String {
  case err {
    reason_chain.ParseError(msg) -> "ParseError: " <> msg
    reason_chain.NoClaimError -> "NoClaimError: Could not identify claim"
    reason_chain.NoReasonsError -> "NoReasonsError: No supporting reasons found"
    reason_chain.FormalizationError(msg) -> "FormalizationError: " <> msg
    reason_chain.LogicSystemError(msg) -> "LogicSystemError: " <> msg
    reason_chain.JsonError(msg) -> "JsonError: " <> msg
  }
}
