//// Documentation Generator Tests
////
//// Tests for the documentation auto-generation module.

import gleeunit/should
import gleam/string
import modal_logic/rules/rule_store
import modal_logic/testing/docs/doc_generator.{
  type DocSection, DocConfig, DocSection, Html, Markdown, PlainText,
  comprehensive_config, default_config, generate_soundness_doc, generate_store_doc,
  markdown_config, render, summary,
}
import modal_logic/testing/validation/soundness_checker

// ============ Config Tests ============

pub fn default_config_test() {
  let config = default_config()

  config.format
  |> should.equal(Markdown)

  config.include_rules
  |> should.be_true

  config.include_axioms
  |> should.be_true

  config.include_tests
  |> should.be_true

  config.include_soundness
  |> should.be_true

  config.include_traces
  |> should.be_false

  config.include_toc
  |> should.be_true
}

pub fn markdown_config_test() {
  let config = markdown_config()

  config.format
  |> should.equal(Markdown)
}

pub fn comprehensive_config_test() {
  let config = comprehensive_config()

  config.include_traces
  |> should.be_true

  config.include_rules
  |> should.be_true
}

// ============ Store Documentation Tests ============

pub fn generate_store_doc_basic_test() {
  let store = rule_store.new()
  let config = default_config()

  let doc = generate_store_doc(store, config)

  doc.title
  |> should.equal("Modal Logic Rules Documentation")

  doc.format
  |> should.equal(Markdown)
}

pub fn generate_store_doc_with_rules_test() {
  let store = rule_store.standard_store()
  let config = default_config()

  let doc = generate_store_doc(store, config)

  // Should have multiple sections
  doc.sections
  |> should.not_equal([])

  // Should have metadata
  doc.metadata.total_sections
  |> should.not_equal(0)
}

pub fn generate_store_doc_no_toc_test() {
  let store = rule_store.new()
  let config = DocConfig(..default_config(), include_toc: False)

  let doc = generate_store_doc(store, config)

  // Should not have TOC section at beginning
  case doc.sections {
    [] -> should.be_true(True)
    [first, ..] ->
      first.id
      |> should.not_equal("toc")
  }
}

pub fn generate_store_doc_no_rules_test() {
  let store = rule_store.new()
  let config = DocConfig(..default_config(), include_rules: False, include_toc: False)

  let doc = generate_store_doc(store, config)

  // Without rules section, should have fewer sections
  let has_rules_section =
    doc.sections
    |> has_section_with_id("rules")

  has_rules_section
  |> should.be_false
}

fn has_section_with_id(sections: List(DocSection), id: String) -> Bool {
  case sections {
    [] -> False
    [first, ..rest] ->
      case first.id == id {
        True -> True
        False -> has_section_with_id(rest, id)
      }
  }
}

// ============ Render Tests ============

pub fn render_markdown_test() {
  let store = rule_store.new()
  let config = DocConfig(..default_config(), include_toc: False, include_rules: False, include_axioms: False)

  let doc = generate_store_doc(store, config)
  let rendered = render(doc)

  // Should start with markdown title
  rendered
  |> string.starts_with("# ")
  |> should.be_true
}

pub fn render_plaintext_test() {
  let store = rule_store.new()
  let config = DocConfig(..default_config(),
    format: PlainText,
    include_toc: False,
    include_rules: False,
    include_axioms: False)

  let doc = generate_store_doc(store, config)
  let rendered = render(doc)

  // Should not have markdown headers
  rendered
  |> string.starts_with("# ")
  |> should.be_false

  // Should have the title
  rendered
  |> string.contains("Modal Logic Rules Documentation")
  |> should.be_true
}

pub fn render_html_test() {
  let store = rule_store.new()
  let config = DocConfig(..default_config(),
    format: Html,
    include_toc: False,
    include_rules: False,
    include_axioms: False)

  let doc = generate_store_doc(store, config)
  let rendered = render(doc)

  // Should have HTML tags
  rendered
  |> string.contains("<h1>")
  |> should.be_true
}

// ============ Summary Tests ============

pub fn summary_test() {
  let store = rule_store.new()
  let config = default_config()

  let doc = generate_store_doc(store, config)
  let sum = summary(doc)

  sum
  |> string.contains("Title:")
  |> should.be_true

  sum
  |> string.contains("Format:")
  |> should.be_true

  sum
  |> string.contains("Sections:")
  |> should.be_true
}

// ============ Soundness Documentation Tests ============

pub fn generate_soundness_doc_test() {
  let store = rule_store.standard_store()
  let soundness_result = soundness_checker.check_store_soundness(store)
  let config = default_config()

  let doc = generate_soundness_doc(soundness_result, config)

  doc.title
  |> should.equal("Soundness Analysis Report")

  // Should have summary and details sections
  doc.sections
  |> should.not_equal([])
}

pub fn generate_soundness_doc_html_test() {
  let store = rule_store.standard_store()
  let soundness_result = soundness_checker.check_store_soundness(store)
  let config = DocConfig(..default_config(), format: Html)

  let doc = generate_soundness_doc(soundness_result, config)
  let rendered = render(doc)

  rendered
  |> string.contains("<table>")
  |> should.be_true
}

// ============ Metadata Tests ============

pub fn doc_metadata_test() {
  let store = rule_store.standard_store()
  let config = default_config()

  let doc = generate_store_doc(store, config)

  doc.metadata.version
  |> should.equal("1.0.0")

  // Word count should be non-zero for standard store
  doc.metadata.word_count
  |> should.not_equal(0)
}

// ============ Section Structure Tests ============

pub fn doc_section_structure_test() {
  let section = DocSection(
    id: "test-section",
    title: "Test Section",
    content: "Test content here",
    subsections: [],
    level: 1,
  )

  section.id
  |> should.equal("test-section")

  section.level
  |> should.equal(1)
}

pub fn doc_section_with_subsections_test() {
  let subsection = DocSection(
    id: "sub-1",
    title: "Subsection 1",
    content: "Subsection content",
    subsections: [],
    level: 2,
  )

  let section = DocSection(
    id: "parent",
    title: "Parent Section",
    content: "Parent content",
    subsections: [subsection],
    level: 1,
  )

  section.subsections
  |> should.not_equal([])
}

// ============ Format-Specific Tests ============

pub fn rules_section_markdown_format_test() {
  let store = rule_store.standard_store()
  let config = DocConfig(..default_config(), include_toc: False, include_axioms: False)

  let doc = generate_store_doc(store, config)
  let rendered = render(doc)

  // Should have markdown formatting
  rendered
  |> string.contains("**")
  |> should.be_true
}

pub fn rules_section_plaintext_format_test() {
  let store = rule_store.standard_store()
  let config = DocConfig(..default_config(), format: PlainText, include_toc: False, include_axioms: False)

  let doc = generate_store_doc(store, config)
  let rendered = render(doc)

  // Should not have markdown bold
  rendered
  |> string.contains("**")
  |> should.be_false
}

pub fn axioms_section_test() {
  let store = rule_store.standard_store()
  let config = DocConfig(..default_config(), include_toc: False, include_rules: False)

  let doc = generate_store_doc(store, config)
  let rendered = render(doc)

  // Should contain axiom-related content
  rendered
  |> string.contains("Axiom")
  |> should.be_true
}
