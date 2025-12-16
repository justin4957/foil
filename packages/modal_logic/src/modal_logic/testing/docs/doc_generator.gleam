//// Documentation Auto-Generator
////
//// This module generates comprehensive documentation from inference rules,
//// axioms, and test results.

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/proposition.{type LogicSystem}
import modal_logic/rules/axiom.{type Axiom, type FrameProperty}
import modal_logic/rules/inference_rule.{type InferenceRule}
import modal_logic/rules/rule_store.{type RuleStore}
import modal_logic/testing/validation/philosophical_tester.{
  type PhilosophicalTestResult,
}
import modal_logic/testing/validation/soundness_checker.{
  type BatchSoundnessResult, type SoundnessCheckResult,
}

// ============ Core Types ============

/// Configuration for documentation generation
pub type DocConfig {
  DocConfig(
    /// Output format
    format: OutputFormat,
    /// Include rule details
    include_rules: Bool,
    /// Include axiom details
    include_axioms: Bool,
    /// Include test results
    include_tests: Bool,
    /// Include soundness analysis
    include_soundness: Bool,
    /// Include inference traces
    include_traces: Bool,
    /// Table of contents
    include_toc: Bool,
  )
}

/// Output formats
pub type OutputFormat {
  /// Markdown format
  Markdown
  /// Plain text format
  PlainText
  /// HTML format
  Html
  /// JSON format
  Json
}

/// Generated documentation
pub type GeneratedDoc {
  GeneratedDoc(
    /// Document title
    title: String,
    /// Document sections
    sections: List(DocSection),
    /// Format used
    format: OutputFormat,
    /// Generation metadata
    metadata: DocMetadata,
  )
}

/// A documentation section
pub type DocSection {
  DocSection(
    /// Section ID (for links)
    id: String,
    /// Section title
    title: String,
    /// Section content
    content: String,
    /// Subsections
    subsections: List(DocSection),
    /// Section level (1-6)
    level: Int,
  )
}

/// Document metadata
pub type DocMetadata {
  DocMetadata(
    /// Generation timestamp
    generated_at: String,
    /// Generator version
    version: String,
    /// Total sections
    total_sections: Int,
    /// Word count
    word_count: Int,
  )
}

// ============ Configuration ============

/// Default documentation config
pub fn default_config() -> DocConfig {
  DocConfig(
    format: Markdown,
    include_rules: True,
    include_axioms: True,
    include_tests: True,
    include_soundness: True,
    include_traces: False,
    include_toc: True,
  )
}

/// Markdown-only config
pub fn markdown_config() -> DocConfig {
  DocConfig(..default_config(), format: Markdown)
}

/// Comprehensive config with all details
pub fn comprehensive_config() -> DocConfig {
  DocConfig(
    format: Markdown,
    include_rules: True,
    include_axioms: True,
    include_tests: True,
    include_soundness: True,
    include_traces: True,
    include_toc: True,
  )
}

/// JSON export config
pub fn json_config() -> DocConfig {
  DocConfig(
    format: Json,
    include_rules: True,
    include_axioms: True,
    include_tests: True,
    include_soundness: True,
    include_traces: False,
    include_toc: False,
  )
}

/// HTML config with styling
pub fn html_config() -> DocConfig {
  DocConfig(
    format: Html,
    include_rules: True,
    include_axioms: True,
    include_tests: True,
    include_soundness: True,
    include_traces: True,
    include_toc: True,
  )
}

// ============ Document Generation ============

/// Generate documentation from a rule store
pub fn generate_store_doc(store: RuleStore, config: DocConfig) -> GeneratedDoc {
  let sections = []

  // Title section
  let title = "Modal Logic Rules Documentation"

  // Rules section
  let sections = case config.include_rules {
    True -> list.append(sections, [generate_rules_section(store, config)])
    False -> sections
  }

  // Axioms section
  let sections = case config.include_axioms {
    True -> list.append(sections, [generate_axioms_section(store, config)])
    False -> sections
  }

  // Table of contents
  let sections = case config.include_toc {
    True -> [generate_toc(sections, config), ..sections]
    False -> sections
  }

  let content = sections_to_content(sections, config.format)
  let word_count = count_words(content)

  GeneratedDoc(
    title: title,
    sections: sections,
    format: config.format,
    metadata: DocMetadata(
      generated_at: "now",
      version: "1.0.0",
      total_sections: count_sections(sections),
      word_count: word_count,
    ),
  )
}

/// Generate documentation from test results
pub fn generate_test_doc(
  result: PhilosophicalTestResult,
  config: DocConfig,
) -> GeneratedDoc {
  let title = "Modal Logic Test Results"

  let sections = [
    generate_test_summary_section(result, config),
    generate_test_details_section(result, config),
  ]

  // Add traces if configured
  let sections = case config.include_traces {
    True -> list.append(sections, [generate_traces_section(result, config)])
    False -> sections
  }

  // Table of contents
  let sections = case config.include_toc {
    True -> [generate_toc(sections, config), ..sections]
    False -> sections
  }

  let content = sections_to_content(sections, config.format)

  GeneratedDoc(
    title: title,
    sections: sections,
    format: config.format,
    metadata: DocMetadata(
      generated_at: "now",
      version: "1.0.0",
      total_sections: count_sections(sections),
      word_count: count_words(content),
    ),
  )
}

/// Generate documentation from soundness results
pub fn generate_soundness_doc(
  result: BatchSoundnessResult,
  config: DocConfig,
) -> GeneratedDoc {
  let title = "Soundness Analysis Report"

  let sections = [
    generate_soundness_summary_section(result, config),
    generate_soundness_details_section(result, config),
  ]

  let sections = case config.include_toc {
    True -> [generate_toc(sections, config), ..sections]
    False -> sections
  }

  let content = sections_to_content(sections, config.format)

  GeneratedDoc(
    title: title,
    sections: sections,
    format: config.format,
    metadata: DocMetadata(
      generated_at: "now",
      version: "1.0.0",
      total_sections: count_sections(sections),
      word_count: count_words(content),
    ),
  )
}

// ============ Section Generators ============

/// Generate table of contents
fn generate_toc(sections: List(DocSection), config: DocConfig) -> DocSection {
  let toc_content =
    sections
    |> list.map(fn(s) { format_toc_entry(s, config) })
    |> string.join("\n")

  DocSection(
    id: "toc",
    title: "Table of Contents",
    content: toc_content,
    subsections: [],
    level: 1,
  )
}

/// Format TOC entry
fn format_toc_entry(section: DocSection, config: DocConfig) -> String {
  let indent = string.repeat("  ", section.level - 1)
  case config.format {
    Markdown -> indent <> "- [" <> section.title <> "](#" <> section.id <> ")"
    Html ->
      indent
      <> "<li><a href=\"#"
      <> section.id
      <> "\">"
      <> section.title
      <> "</a></li>"
    PlainText -> indent <> "* " <> section.title
    Json -> indent <> "\"" <> section.id <> "\": \"" <> section.title <> "\""
  }
}

/// Generate rules section
fn generate_rules_section(store: RuleStore, config: DocConfig) -> DocSection {
  let rules = rule_store.list_rules(store)

  let subsections =
    rules
    |> list.map(fn(rule) { generate_rule_subsection(rule, config) })

  let summary =
    "This section documents "
    <> int.to_string(list.length(rules))
    <> " inference rules."

  DocSection(
    id: "rules",
    title: "Inference Rules",
    content: summary,
    subsections: subsections,
    level: 1,
  )
}

/// Generate subsection for a single rule
fn generate_rule_subsection(
  rule: InferenceRule,
  config: DocConfig,
) -> DocSection {
  let content = format_rule_content(rule, config)

  DocSection(
    id: "rule-" <> rule.id,
    title: rule.name,
    content: content,
    subsections: [],
    level: 2,
  )
}

/// Format rule content
fn format_rule_content(rule: InferenceRule, config: DocConfig) -> String {
  let premise_count = list.length(rule.premise_patterns)
  let systems =
    rule.valid_in
    |> list.map(logic_system_to_string)
    |> string.join(", ")

  let systems_json =
    rule.valid_in
    |> list.map(fn(s) { "\"" <> logic_system_to_string(s) <> "\"" })
    |> string.join(", ")

  let tags = string.join(rule.metadata.tags, ", ")
  let tags_json =
    rule.metadata.tags
    |> list.map(fn(t) { "\"" <> t <> "\"" })
    |> string.join(", ")

  case config.format {
    Markdown ->
      string.join(
        [
          "**ID:** `" <> rule.id <> "`",
          "",
          "**Description:** " <> rule.description,
          "",
          "**Premises:** " <> int.to_string(premise_count),
          "",
          "**Valid in:** " <> systems,
          "",
          case tags {
            "" -> ""
            _ -> "**Tags:** " <> tags
          },
        ],
        "\n",
      )
    PlainText ->
      string.join(
        [
          "ID: " <> rule.id,
          "Description: " <> rule.description,
          "Premises: " <> int.to_string(premise_count),
          "Valid in: " <> systems,
          "Tags: " <> tags,
        ],
        "\n",
      )
    Html ->
      string.join(
        [
          "<p><strong>ID:</strong> <code>" <> rule.id <> "</code></p>",
          "<p><strong>Description:</strong> " <> rule.description <> "</p>",
          "<p><strong>Premises:</strong> "
            <> int.to_string(premise_count)
            <> "</p>",
          "<p><strong>Valid in:</strong> " <> systems <> "</p>",
        ],
        "\n",
      )
    Json ->
      string.join(
        [
          "{",
          "  \"id\": \"" <> rule.id <> "\",",
          "  \"name\": \"" <> rule.name <> "\",",
          "  \"description\": \""
            <> escape_json_string(rule.description)
            <> "\",",
          "  \"premise_count\": " <> int.to_string(premise_count) <> ",",
          "  \"valid_in\": [" <> systems_json <> "],",
          "  \"tags\": [" <> tags_json <> "]",
          "}",
        ],
        "\n",
      )
  }
}

/// Generate axioms section
fn generate_axioms_section(store: RuleStore, config: DocConfig) -> DocSection {
  let axioms = rule_store.list_axioms(store)

  let subsections =
    axioms
    |> list.map(fn(ax) { generate_axiom_subsection(ax, config) })

  let summary =
    "This section documents "
    <> int.to_string(list.length(axioms))
    <> " axioms."

  DocSection(
    id: "axioms",
    title: "Axioms",
    content: summary,
    subsections: subsections,
    level: 1,
  )
}

/// Generate subsection for a single axiom
fn generate_axiom_subsection(ax: Axiom, config: DocConfig) -> DocSection {
  let content = format_axiom_content(ax, config)

  DocSection(
    id: "axiom-" <> ax.id,
    title: ax.name <> " (" <> ax.id <> ")",
    content: content,
    subsections: [],
    level: 2,
  )
}

/// Format axiom content
fn format_axiom_content(ax: Axiom, config: DocConfig) -> String {
  let systems =
    ax.included_in
    |> list.map(logic_system_to_string)
    |> string.join(", ")

  let systems_json =
    ax.included_in
    |> list.map(fn(s) { "\"" <> logic_system_to_string(s) <> "\"" })
    |> string.join(", ")

  let frame_prop = case ax.frame_property {
    Some(prop) -> format_frame_property(prop)
    None -> "None"
  }

  let frame_prop_json = case ax.frame_property {
    Some(prop) -> "\"" <> format_frame_property(prop) <> "\""
    None -> "null"
  }

  case config.format {
    Markdown ->
      string.join(
        [
          "**Description:** " <> ax.description,
          "",
          "**Included in:** " <> systems,
          "",
          "**Frame Property:** " <> frame_prop,
        ],
        "\n",
      )
    PlainText ->
      string.join(
        [
          "Description: " <> ax.description,
          "Included in: " <> systems,
          "Frame Property: " <> frame_prop,
        ],
        "\n",
      )
    Html ->
      string.join(
        [
          "<p><strong>Description:</strong> " <> ax.description <> "</p>",
          "<p><strong>Included in:</strong> " <> systems <> "</p>",
          "<p><strong>Frame Property:</strong> " <> frame_prop <> "</p>",
        ],
        "\n",
      )
    Json ->
      string.join(
        [
          "{",
          "  \"id\": \"" <> ax.id <> "\",",
          "  \"name\": \"" <> ax.name <> "\",",
          "  \"description\": \"" <> escape_json_string(ax.description) <> "\",",
          "  \"included_in\": [" <> systems_json <> "],",
          "  \"frame_property\": " <> frame_prop_json,
          "}",
        ],
        "\n",
      )
  }
}

/// Generate test summary section
fn generate_test_summary_section(
  result: PhilosophicalTestResult,
  config: DocConfig,
) -> DocSection {
  let pass_rate =
    float.round(result.soundness_assessment.score *. 100.0)
    |> int.to_string

  let content = case config.format {
    Markdown ->
      string.join(
        [
          "| Metric | Value |",
          "|--------|-------|",
          "| Total Tests | " <> int.to_string(result.total_tested) <> " |",
          "| Passed | " <> int.to_string(result.correctly_validated) <> " |",
          "| Failed | " <> int.to_string(result.incorrectly_validated) <> " |",
          "| Pass Rate | " <> pass_rate <> "% |",
        ],
        "\n",
      )
    PlainText ->
      string.join(
        [
          "Total Tests: " <> int.to_string(result.total_tested),
          "Passed: " <> int.to_string(result.correctly_validated),
          "Failed: " <> int.to_string(result.incorrectly_validated),
          "Pass Rate: " <> pass_rate <> "%",
        ],
        "\n",
      )
    Html ->
      string.join(
        [
          "<table>",
          "<tr><td>Total Tests</td><td>"
            <> int.to_string(result.total_tested)
            <> "</td></tr>",
          "<tr><td>Passed</td><td>"
            <> int.to_string(result.correctly_validated)
            <> "</td></tr>",
          "<tr><td>Failed</td><td>"
            <> int.to_string(result.incorrectly_validated)
            <> "</td></tr>",
          "<tr><td>Pass Rate</td><td>" <> pass_rate <> "%</td></tr>",
          "</table>",
        ],
        "\n",
      )
    Json ->
      string.join(
        [
          "{",
          "  \"total_tests\": " <> int.to_string(result.total_tested) <> ",",
          "  \"passed\": " <> int.to_string(result.correctly_validated) <> ",",
          "  \"failed\": " <> int.to_string(result.incorrectly_validated) <> ",",
          "  \"pass_rate\": " <> pass_rate,
          "}",
        ],
        "\n",
      )
  }

  DocSection(
    id: "test-summary",
    title: "Test Summary",
    content: content,
    subsections: [],
    level: 1,
  )
}

/// Generate test details section
fn generate_test_details_section(
  result: PhilosophicalTestResult,
  config: DocConfig,
) -> DocSection {
  let failed =
    result.argument_results
    |> list.filter(fn(r) { !r.passed })

  let content = case failed {
    [] ->
      case config.format {
        Json -> "{\"status\": \"all_passed\", \"failures\": []}"
        _ -> "All tests passed successfully."
      }
    _ -> {
      case config.format {
        Markdown -> {
          let failure_list =
            failed
            |> list.map(fn(f) {
              "- **"
              <> f.argument.name
              <> "**: "
              <> option.unwrap(f.error_message, "Unknown error")
            })
            |> string.join("\n")
          "### Failed Tests\n\n" <> failure_list
        }
        PlainText -> {
          let failure_list =
            failed
            |> list.map(fn(f) {
              "* "
              <> f.argument.name
              <> ": "
              <> option.unwrap(f.error_message, "Unknown error")
            })
            |> string.join("\n")
          "Failed Tests:\n" <> failure_list
        }
        Html -> {
          let failure_list =
            failed
            |> list.map(fn(f) {
              "<li><strong>"
              <> f.argument.name
              <> "</strong>: "
              <> option.unwrap(f.error_message, "Unknown error")
              <> "</li>"
            })
            |> string.join("\n")
          "<h3>Failed Tests</h3>\n<ul>\n" <> failure_list <> "\n</ul>"
        }
        Json -> {
          let failure_list =
            failed
            |> list.map(fn(f) {
              "    {\"name\": \""
              <> escape_json_string(f.argument.name)
              <> "\", \"error\": \""
              <> escape_json_string(option.unwrap(
                f.error_message,
                "Unknown error",
              ))
              <> "\"}"
            })
            |> string.join(",\n")
          "{\n  \"status\": \"has_failures\",\n  \"failures\": [\n"
          <> failure_list
          <> "\n  ]\n}"
        }
      }
    }
  }

  DocSection(
    id: "test-details",
    title: "Test Details",
    content: content,
    subsections: [],
    level: 1,
  )
}

/// Generate traces section
fn generate_traces_section(
  result: PhilosophicalTestResult,
  config: DocConfig,
) -> DocSection {
  let traces =
    result.argument_results
    |> list.filter_map(fn(r) {
      case r.trace {
        Some(t) -> Ok(#(r.argument.name, t))
        None -> Error(Nil)
      }
    })
    |> list.take(10)

  let content = case traces {
    [] ->
      case config.format {
        Json -> "{\"traces\": []}"
        _ -> "No inference traces available."
      }
    _ -> {
      case config.format {
        Markdown ->
          traces
          |> list.map(fn(pair) {
            let #(name, trace) = pair
            "#### " <> name <> "\n\nSteps: " <> int.to_string(trace.total_steps)
          })
          |> string.join("\n\n")
        PlainText ->
          traces
          |> list.map(fn(pair) {
            let #(name, trace) = pair
            name <> "\n  Steps: " <> int.to_string(trace.total_steps)
          })
          |> string.join("\n\n")
        Html ->
          traces
          |> list.map(fn(pair) {
            let #(name, trace) = pair
            "<h4>"
            <> name
            <> "</h4><p>Steps: "
            <> int.to_string(trace.total_steps)
            <> "</p>"
          })
          |> string.join("\n\n")
        Json -> {
          let trace_items =
            traces
            |> list.map(fn(pair) {
              let #(name, trace) = pair
              "    {\"name\": \""
              <> escape_json_string(name)
              <> "\", \"steps\": "
              <> int.to_string(trace.total_steps)
              <> "}"
            })
            |> string.join(",\n")
          "{\n  \"traces\": [\n" <> trace_items <> "\n  ]\n}"
        }
      }
    }
  }

  DocSection(
    id: "traces",
    title: "Inference Traces",
    content: content,
    subsections: [],
    level: 1,
  )
}

/// Generate soundness summary section
fn generate_soundness_summary_section(
  result: BatchSoundnessResult,
  config: DocConfig,
) -> DocSection {
  let content = case config.format {
    Markdown ->
      string.join(
        [
          "| Metric | Value |",
          "|--------|-------|",
          "| Rules Checked | " <> int.to_string(result.rules_checked) <> " |",
          "| Sound Rules | " <> int.to_string(result.sound_rules) <> " |",
          "| Problematic | " <> int.to_string(result.problematic_rules) <> " |",
        ],
        "\n",
      )
    PlainText ->
      string.join(
        [
          "Rules Checked: " <> int.to_string(result.rules_checked),
          "Sound Rules: " <> int.to_string(result.sound_rules),
          "Problematic: " <> int.to_string(result.problematic_rules),
        ],
        "\n",
      )
    Html ->
      "<table>"
      <> "<tr><td>Rules Checked</td><td>"
      <> int.to_string(result.rules_checked)
      <> "</td></tr>"
      <> "<tr><td>Sound Rules</td><td>"
      <> int.to_string(result.sound_rules)
      <> "</td></tr>"
      <> "<tr><td>Problematic</td><td>"
      <> int.to_string(result.problematic_rules)
      <> "</td></tr>"
      <> "</table>"
    Json ->
      string.join(
        [
          "{",
          "  \"rules_checked\": " <> int.to_string(result.rules_checked) <> ",",
          "  \"sound_rules\": " <> int.to_string(result.sound_rules) <> ",",
          "  \"problematic_rules\": " <> int.to_string(result.problematic_rules),
          "}",
        ],
        "\n",
      )
  }

  DocSection(
    id: "soundness-summary",
    title: "Soundness Summary",
    content: content,
    subsections: [],
    level: 1,
  )
}

/// Generate soundness details section
fn generate_soundness_details_section(
  result: BatchSoundnessResult,
  config: DocConfig,
) -> DocSection {
  let subsections =
    result.results
    |> list.map(fn(r) { generate_soundness_result_subsection(r, config) })

  DocSection(
    id: "soundness-details",
    title: "Soundness Details",
    content: "Individual rule soundness analysis.",
    subsections: subsections,
    level: 1,
  )
}

/// Generate subsection for soundness result
fn generate_soundness_result_subsection(
  result: SoundnessCheckResult,
  config: DocConfig,
) -> DocSection {
  let status = case result.is_sound {
    True -> "✓ Sound"
    False -> "✗ Unsound"
  }

  let systems =
    result.sound_in
    |> list.map(logic_system_to_string)
    |> string.join(", ")

  let systems_json =
    result.sound_in
    |> list.map(fn(s) { "\"" <> logic_system_to_string(s) <> "\"" })
    |> string.join(", ")

  let content = case config.format {
    Markdown ->
      string.join(
        [
          "**Status:** " <> status,
          "",
          "**Sound in:** "
            <> case systems {
            "" -> "(none)"
            s -> s
          },
          "",
          "**Counterexamples:** "
            <> int.to_string(list.length(result.counterexamples)),
        ],
        "\n",
      )
    PlainText ->
      string.join(
        [
          "Status: " <> status,
          "Sound in: " <> systems,
          "Counterexamples: "
            <> int.to_string(list.length(result.counterexamples)),
        ],
        "\n",
      )
    Html ->
      "<p><strong>Status:</strong> "
      <> status
      <> "</p>"
      <> "<p><strong>Sound in:</strong> "
      <> systems
      <> "</p>"
      <> "<p><strong>Counterexamples:</strong> "
      <> int.to_string(list.length(result.counterexamples))
      <> "</p>"
    Json ->
      string.join(
        [
          "{",
          "  \"rule_id\": \"" <> result.rule_id <> "\",",
          "  \"rule_name\": \"" <> escape_json_string(result.rule_name) <> "\",",
          "  \"is_sound\": " <> bool_to_json(result.is_sound) <> ",",
          "  \"sound_in\": [" <> systems_json <> "],",
          "  \"counterexamples\": "
            <> int.to_string(list.length(result.counterexamples)),
          "}",
        ],
        "\n",
      )
  }

  DocSection(
    id: "soundness-" <> result.rule_id,
    title: result.rule_name,
    content: content,
    subsections: [],
    level: 2,
  )
}

// ============ Output Functions ============

/// Render document to string
pub fn render(doc: GeneratedDoc) -> String {
  let title_str = format_title(doc.title, doc.format)
  let content = sections_to_content(doc.sections, doc.format)

  title_str <> "\n\n" <> content
}

/// Format title
fn format_title(title: String, format: OutputFormat) -> String {
  case format {
    Markdown -> "# " <> title
    PlainText -> title <> "\n" <> string.repeat("=", string.length(title))
    Html -> "<h1>" <> title <> "</h1>"
    Json -> "\"title\": \"" <> escape_json_string(title) <> "\""
  }
}

/// Convert sections to content string
fn sections_to_content(
  sections: List(DocSection),
  format: OutputFormat,
) -> String {
  sections
  |> list.map(fn(s) { section_to_string(s, format) })
  |> string.join("\n\n")
}

/// Convert section to string
fn section_to_string(section: DocSection, format: OutputFormat) -> String {
  let header = format_header(section.title, section.level, format, section.id)

  let subsection_content = case section.subsections {
    [] -> ""
    subs ->
      "\n\n"
      <> {
        subs
        |> list.map(fn(s) { section_to_string(s, format) })
        |> string.join("\n\n")
      }
  }

  header <> "\n\n" <> section.content <> subsection_content
}

/// Format header
fn format_header(
  title: String,
  level: Int,
  format: OutputFormat,
  id: String,
) -> String {
  case format {
    Markdown -> string.repeat("#", level) <> " " <> title
    PlainText -> {
      let underline = case level {
        1 -> "="
        2 -> "-"
        _ -> "~"
      }
      title <> "\n" <> string.repeat(underline, string.length(title))
    }
    Html ->
      "<h"
      <> int.to_string(level)
      <> " id=\""
      <> id
      <> "\">"
      <> title
      <> "</h"
      <> int.to_string(level)
      <> ">"
    Json ->
      "\"section_"
      <> id
      <> "\": {\"title\": \""
      <> escape_json_string(title)
      <> "\", \"level\": "
      <> int.to_string(level)
      <> "}"
  }
}

// ============ Utility Functions ============

/// Count total sections
fn count_sections(sections: List(DocSection)) -> Int {
  list.fold(sections, 0, fn(acc, s) { acc + 1 + count_sections(s.subsections) })
}

/// Count words in content
fn count_words(content: String) -> Int {
  content
  |> string.split(" ")
  |> list.filter(fn(w) { w != "" })
  |> list.length
}

/// Format frame property
fn format_frame_property(prop: FrameProperty) -> String {
  case prop {
    axiom.Reflexive -> "Reflexive"
    axiom.Transitive -> "Transitive"
    axiom.Symmetric -> "Symmetric"
    axiom.Euclidean -> "Euclidean"
    axiom.Serial -> "Serial"
  }
}

/// Convert logic system to string
fn logic_system_to_string(system: LogicSystem) -> String {
  case system {
    proposition.K -> "K"
    proposition.T -> "T"
    proposition.K4 -> "K4"
    proposition.S4 -> "S4"
    proposition.S5 -> "S5"
    proposition.KD -> "KD"
    proposition.KD45 -> "KD45"
  }
}

/// Get document summary
pub fn summary(doc: GeneratedDoc) -> String {
  string.join(
    [
      "Title: " <> doc.title,
      "Format: " <> format_to_string(doc.format),
      "Sections: " <> int.to_string(doc.metadata.total_sections),
      "Words: " <> int.to_string(doc.metadata.word_count),
    ],
    "\n",
  )
}

/// Format to string
fn format_to_string(format: OutputFormat) -> String {
  case format {
    Markdown -> "Markdown"
    PlainText -> "Plain Text"
    Html -> "HTML"
    Json -> "JSON"
  }
}

/// Escape string for JSON output
fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

/// Convert bool to JSON string
fn bool_to_json(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

/// Get CSS styles for HTML output
fn get_doc_css() -> String {
  "
:root {
  --primary-color: #3498db;
  --secondary-color: #2ecc71;
  --warning-color: #f39c12;
  --danger-color: #e74c3c;
  --text-color: #333;
  --bg-color: #f8f9fa;
  --card-bg: #ffffff;
  --border-color: #dee2e6;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  line-height: 1.6;
  color: var(--text-color);
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
  background: var(--bg-color);
}

.doc-container {
  background: var(--card-bg);
  padding: 40px;
  border-radius: 12px;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
}

h1 {
  color: var(--primary-color);
  border-bottom: 3px solid var(--primary-color);
  padding-bottom: 15px;
  margin-bottom: 30px;
  font-size: 2.5em;
}

h2 {
  color: #2c3e50;
  margin-top: 40px;
  padding-bottom: 10px;
  border-bottom: 2px solid var(--border-color);
}

h3 {
  color: #34495e;
  margin-top: 25px;
}

h4 {
  color: #5d6d7e;
  margin-top: 20px;
}

/* Table styles */
table {
  width: 100%;
  border-collapse: collapse;
  margin: 20px 0;
  background: var(--card-bg);
  border-radius: 8px;
  overflow: hidden;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
}

th, td {
  padding: 14px 16px;
  text-align: left;
  border-bottom: 1px solid var(--border-color);
}

th {
  background: var(--primary-color);
  color: white;
  font-weight: 600;
  text-transform: uppercase;
  font-size: 0.85em;
  letter-spacing: 0.5px;
}

tr:hover {
  background: #f5f6f7;
}

tr:last-child td {
  border-bottom: none;
}

/* Code styling */
code {
  background: #f0f3f5;
  padding: 3px 8px;
  border-radius: 4px;
  font-family: 'SF Mono', 'Monaco', 'Consolas', monospace;
  font-size: 0.9em;
  color: #e74c3c;
}

pre {
  background: #2c3e50;
  color: #ecf0f1;
  padding: 20px;
  border-radius: 8px;
  overflow-x: auto;
  font-family: 'SF Mono', 'Monaco', 'Consolas', monospace;
}

/* Status indicators */
.status-sound, .status-pass {
  color: var(--secondary-color);
  font-weight: bold;
}

.status-unsound, .status-fail {
  color: var(--danger-color);
  font-weight: bold;
}

/* Card components */
.info-card {
  background: linear-gradient(135deg, var(--primary-color), #2980b9);
  color: white;
  padding: 25px;
  border-radius: 10px;
  margin: 20px 0;
}

.info-card h3 {
  color: white;
  margin-top: 0;
}

/* Lists */
ul, ol {
  padding-left: 25px;
}

li {
  margin: 8px 0;
}

/* Links */
a {
  color: var(--primary-color);
  text-decoration: none;
  transition: color 0.2s;
}

a:hover {
  color: #2980b9;
  text-decoration: underline;
}

/* Table of contents */
.toc {
  background: #f8f9fa;
  padding: 20px 30px;
  border-radius: 8px;
  border-left: 4px solid var(--primary-color);
  margin-bottom: 30px;
}

.toc h2 {
  margin-top: 0;
  border-bottom: none;
  font-size: 1.3em;
}

.toc ul {
  list-style-type: none;
  padding-left: 0;
}

.toc li {
  padding: 5px 0;
}

.toc a {
  color: #555;
}

/* Responsive */
@media (max-width: 768px) {
  body {
    padding: 10px;
  }

  .doc-container {
    padding: 20px;
  }

  h1 {
    font-size: 1.8em;
  }

  table {
    font-size: 0.9em;
  }

  th, td {
    padding: 10px 12px;
  }
}
"
}

/// Render HTML document with full styling
pub fn render_html(doc: GeneratedDoc) -> String {
  let content = sections_to_content(doc.sections, Html)

  string.join(
    [
      "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "  <meta charset=\"UTF-8\">",
      "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
      "  <title>" <> escape_html(doc.title) <> "</title>",
      "  <style>",
      get_doc_css(),
      "  </style>",
      "</head>",
      "<body>",
      "  <div class=\"doc-container\">",
      "    <h1>" <> escape_html(doc.title) <> "</h1>",
      content,
      "  </div>",
      "</body>",
      "</html>",
    ],
    "\n",
  )
}

/// Escape HTML special characters
fn escape_html(s: String) -> String {
  s
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
}

/// Render JSON document
pub fn render_json(doc: GeneratedDoc) -> String {
  let sections_json =
    doc.sections
    |> list.map(fn(s) { section_to_json(s) })
    |> string.join(",\n")

  string.join(
    [
      "{",
      "  \"title\": \"" <> escape_json_string(doc.title) <> "\",",
      "  \"format\": \"" <> format_to_string(doc.format) <> "\",",
      "  \"metadata\": {",
      "    \"generated_at\": \"" <> doc.metadata.generated_at <> "\",",
      "    \"version\": \"" <> doc.metadata.version <> "\",",
      "    \"total_sections\": "
        <> int.to_string(doc.metadata.total_sections)
        <> ",",
      "    \"word_count\": " <> int.to_string(doc.metadata.word_count),
      "  },",
      "  \"sections\": [",
      sections_json,
      "  ]",
      "}",
    ],
    "\n",
  )
}

/// Convert section to JSON
fn section_to_json(section: DocSection) -> String {
  let subsections_json = case section.subsections {
    [] -> "[]"
    subs -> {
      let items =
        subs
        |> list.map(section_to_json)
        |> string.join(",\n")
      "[\n" <> items <> "\n    ]"
    }
  }

  string.join(
    [
      "    {",
      "      \"id\": \"" <> section.id <> "\",",
      "      \"title\": \"" <> escape_json_string(section.title) <> "\",",
      "      \"level\": " <> int.to_string(section.level) <> ",",
      "      \"content\": \"" <> escape_json_string(section.content) <> "\",",
      "      \"subsections\": " <> subsections_json,
      "    }",
    ],
    "\n",
  )
}
