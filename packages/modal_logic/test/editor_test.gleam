import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import modal_logic/editor
import modal_logic/web

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Configuration Tests
// =============================================================================

pub fn default_config_test() {
  let config = editor.default_config()

  editor.theme_name(config.theme) |> should.equal("vs")
  config.font_size |> should.equal(14)
  config.tab_size |> should.equal(2)
  config.enable_autocompletion |> should.be_true()
  config.enable_syntax_highlighting |> should.be_true()
}

pub fn dark_config_test() {
  let config = editor.dark_config()

  editor.theme_name(config.theme) |> should.equal("vs-dark")
  config.enable_autocompletion |> should.be_true()
}

pub fn high_contrast_config_test() {
  let config = editor.high_contrast_config()

  editor.theme_name(config.theme) |> should.equal("hc-black")
}

// =============================================================================
// Theme Tests
// =============================================================================

pub fn theme_name_light_test() {
  editor.theme_name(editor.Light) |> should.equal("vs")
}

pub fn theme_name_dark_test() {
  editor.theme_name(editor.Dark) |> should.equal("vs-dark")
}

pub fn theme_name_high_contrast_test() {
  editor.theme_name(editor.HighContrast) |> should.equal("hc-black")
}

// =============================================================================
// Completion Tests
// =============================================================================

pub fn modal_logic_completions_test() {
  let completions = editor.modal_logic_completions()

  { list.length(completions) >= 8 } |> should.be_true()

  // Check for key operators
  let labels = list.map(completions, fn(c) { c.label })

  list.contains(labels, "Necessary") |> should.be_true()
  list.contains(labels, "Possible") |> should.be_true()
  list.contains(labels, "And") |> should.be_true()
  list.contains(labels, "Implies") |> should.be_true()
}

pub fn completion_has_required_fields_test() {
  let completions = editor.modal_logic_completions()

  case list.first(completions) {
    Ok(item) -> {
      item.label |> should.not_equal("")
      item.detail |> should.not_equal("")
      item.documentation |> should.not_equal("")
      item.insert_text |> should.not_equal("")
    }
    Error(_) -> should.fail()
  }
}

pub fn pattern_completions_test() {
  let patterns = editor.pattern_completions()

  { list.length(patterns) >= 3 } |> should.be_true()

  // Check for knowledge implies truth
  let labels = list.map(patterns, fn(p) { p.label })

  list.contains(labels, "Knowledge Implies Truth") |> should.be_true()
  list.contains(labels, "Modus Ponens") |> should.be_true()
}

pub fn completion_kind_name_test() {
  editor.completion_kind_name(editor.Keyword) |> should.equal("keyword")
  editor.completion_kind_name(editor.Operator) |> should.equal("operator")
  editor.completion_kind_name(editor.Pattern) |> should.equal("pattern")
}

// =============================================================================
// Syntax Token Tests
// =============================================================================

pub fn syntax_tokens_test() {
  let tokens = editor.syntax_tokens()

  { list.length(tokens) >= 6 } |> should.be_true()

  // Check token types exist
  let types =
    list.map(tokens, fn(t) {
      let #(type_name, _) = t
      type_name
    })

  list.contains(types, "keyword") |> should.be_true()
  list.contains(types, "operator") |> should.be_true()
  list.contains(types, "type") |> should.be_true()
}

// =============================================================================
// Error Marker Tests
// =============================================================================

pub fn format_error_markers_empty_test() {
  let markers = []
  let formatted = editor.format_error_markers(markers)

  formatted |> should.equal("[]")
}

pub fn format_error_markers_single_test() {
  let marker =
    editor.ErrorMarker(
      line: 5,
      column: 12,
      end_column: 20,
      severity: editor.ErrorLevel,
      message: "Syntax error",
    )

  let formatted = editor.format_error_markers([marker])

  formatted |> should_contain("startLineNumber: 5")
  formatted |> should_contain("startColumn: 12")
  formatted |> should_contain("Syntax error")
}

pub fn format_error_markers_multiple_test() {
  let markers = [
    editor.ErrorMarker(1, 1, 5, editor.ErrorLevel, "Error 1"),
    editor.ErrorMarker(2, 3, 8, editor.WarningLevel, "Warning 1"),
  ]

  let formatted = editor.format_error_markers(markers)

  formatted |> should_contain("Error 1")
  formatted |> should_contain("Warning 1")
}

// =============================================================================
// HTML Generation Tests
// =============================================================================

pub fn editor_page_generates_html_test() {
  let config = editor.default_config()
  let page = editor.editor_page(config)

  let rendered = web.render(page)

  rendered |> should_contain("<html")
  rendered |> should_contain("</html>")
  rendered |> should_contain("monaco-editor")
}

pub fn editor_page_includes_toolbar_test() {
  let config = editor.default_config()
  let page = editor.editor_page(config)

  let rendered = web.render(page)

  rendered |> should_contain("toolbar")
  rendered |> should_contain("Analyze Formula")
  rendered |> should_contain("Check Complexity")
}

pub fn editor_page_includes_monaco_cdn_test() {
  let config = editor.default_config()
  let page = editor.editor_page(config)

  let rendered = web.render(page)

  rendered |> should_contain("monaco-editor")
  rendered |> should_contain("cdnjs.cloudflare.com")
}

pub fn editor_page_includes_theme_selector_test() {
  let config = editor.default_config()
  let page = editor.editor_page(config)

  let rendered = web.render(page)

  rendered |> should_contain("Light Theme")
  rendered |> should_contain("Dark Theme")
  rendered |> should_contain("High Contrast")
}

pub fn editor_page_respects_theme_config_test() {
  let dark_config = editor.dark_config()
  let page = editor.editor_page(dark_config)

  let rendered = web.render(page)

  rendered |> should_contain("vs-dark")
}

pub fn editor_page_includes_autocompletion_test() {
  let config = editor.default_config()
  let page = editor.editor_page(config)

  let rendered = web.render(page)

  rendered |> should_contain("registerCompletionItemProvider")
  rendered |> should_contain("Necessary")
  rendered |> should_contain("Possible")
}

pub fn editor_page_includes_syntax_highlighting_test() {
  let config = editor.default_config()
  let page = editor.editor_page(config)

  let rendered = web.render(page)

  rendered |> should_contain("setMonarchTokensProvider")
  rendered |> should_contain("modal-logic")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False -> should.fail()
  }
}
