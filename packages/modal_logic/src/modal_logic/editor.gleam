//// Interactive Formula Editor Module
////
//// Provides configuration and utilities for the Monaco Editor-based
//// interactive formula editor with syntax highlighting, autocompletion,
//// and real-time error checking.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/web.{type Html, Element, Fragment, Raw, Text}

// =============================================================================
// Editor Configuration
// =============================================================================

/// Editor theme
pub type EditorTheme {
  Light
  Dark
  HighContrast
}

/// Editor configuration
pub type EditorConfig {
  EditorConfig(
    theme: EditorTheme,
    font_size: Int,
    tab_size: Int,
    enable_autocompletion: Bool,
    enable_syntax_highlighting: Bool,
    enable_error_checking: Bool,
    enable_snippets: Bool,
    minimap_enabled: Bool,
  )
}

/// Autocompletion item
pub type CompletionItem {
  CompletionItem(
    label: String,
    kind: CompletionKind,
    detail: String,
    documentation: String,
    insert_text: String,
  )
}

/// Completion item kind
pub type CompletionKind {
  Keyword
  Operator
  Function
  Snippet
  Pattern
}

// =============================================================================
// Default Configuration
// =============================================================================

/// Default editor configuration
pub fn default_config() -> EditorConfig {
  EditorConfig(
    theme: Light,
    font_size: 14,
    tab_size: 2,
    enable_autocompletion: True,
    enable_syntax_highlighting: True,
    enable_error_checking: True,
    enable_snippets: True,
    minimap_enabled: False,
  )
}

/// Dark theme configuration
pub fn dark_config() -> EditorConfig {
  EditorConfig(..default_config(), theme: Dark)
}

/// High contrast configuration
pub fn high_contrast_config() -> EditorConfig {
  EditorConfig(..default_config(), theme: HighContrast)
}

// =============================================================================
// Editor HTML Generation
// =============================================================================

/// Generate complete editor HTML page
pub fn editor_page(config: EditorConfig) -> Html {
  Element(tag: "html", attrs: [], children: [
    Element(tag: "head", attrs: [], children: [
      Element(tag: "title", attrs: [], children: [
        Text("Foil Modal Logic Editor"),
      ]),
      Element(
        tag: "meta",
        attrs: [
          web.Attr("charset", "UTF-8"),
          web.Attr("name", "viewport"),
          web.Attr("content", "width=device-width, initial-scale=1.0"),
        ],
        children: [],
      ),
      monaco_editor_styles(),
      editor_custom_styles(config),
    ]),
    Element(tag: "body", attrs: [], children: [
      editor_toolbar(),
      editor_container(),
      monaco_editor_scripts(),
      editor_initialization_script(config),
    ]),
  ])
}

/// Monaco Editor CDN styles
fn monaco_editor_styles() -> Html {
  Element(
    tag: "link",
    attrs: [
      web.Attr("rel", "stylesheet"),
      web.Attr(
        "href",
        "https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs/editor/editor.main.min.css",
      ),
    ],
    children: [],
  )
}

/// Custom editor styles
fn editor_custom_styles(config: EditorConfig) -> Html {
  let theme_bg = case config.theme {
    Light -> "#ffffff"
    Dark -> "#1e1e1e"
    HighContrast -> "#000000"
  }

  Element(tag: "style", attrs: [], children: [
    Raw("
        body {
          margin: 0;
          padding: 0;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          background: " <> theme_bg <> ";
        }
        #toolbar {
          height: 50px;
          background: #f0f0f0;
          border-bottom: 1px solid #ccc;
          padding: 10px 20px;
          display: flex;
          align-items: center;
          gap: 15px;
        }
        #editor-container {
          width: 100%;
          height: calc(100vh - 50px);
        }
        .btn {
          padding: 8px 16px;
          background: #007acc;
          color: white;
          border: none;
          border-radius: 4px;
          cursor: pointer;
          font-size: 14px;
        }
        .btn:hover {
          background: #005a9e;
        }
        .theme-selector {
          padding: 6px 12px;
          border: 1px solid #ccc;
          border-radius: 4px;
          font-size: 14px;
        }
        "),
  ])
}

/// Editor toolbar
fn editor_toolbar() -> Html {
  Element(tag: "div", attrs: [web.Attr("id", "toolbar")], children: [
    Element(
      tag: "button",
      attrs: [
        web.Attr("class", "btn"),
        web.Attr("onclick", "analyzeFormula()"),
      ],
      children: [Text("Analyze Formula")],
    ),
    Element(
      tag: "button",
      attrs: [
        web.Attr("class", "btn"),
        web.Attr("onclick", "checkComplexity()"),
      ],
      children: [Text("Check Complexity")],
    ),
    Element(
      tag: "select",
      attrs: [
        web.Attr("class", "theme-selector"),
        web.Attr("onchange", "changeTheme(this.value)"),
      ],
      children: [
        Element(tag: "option", attrs: [web.Attr("value", "vs")], children: [
          Text("Light Theme"),
        ]),
        Element(tag: "option", attrs: [web.Attr("value", "vs-dark")], children: [
          Text("Dark Theme"),
        ]),
        Element(
          tag: "option",
          attrs: [web.Attr("value", "hc-black")],
          children: [Text("High Contrast")],
        ),
      ],
    ),
  ])
}

/// Editor container
fn editor_container() -> Html {
  Element(tag: "div", attrs: [web.Attr("id", "editor-container")], children: [])
}

/// Monaco Editor CDN scripts
fn monaco_editor_scripts() -> Html {
  Fragment(children: [
    Element(
      tag: "script",
      attrs: [
        web.Attr(
          "src",
          "https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs/loader.min.js",
        ),
      ],
      children: [],
    ),
  ])
}

/// Editor initialization script
fn editor_initialization_script(config: EditorConfig) -> Html {
  let theme_name = case config.theme {
    Light -> "vs"
    Dark -> "vs-dark"
    HighContrast -> "hc-black"
  }

  Element(tag: "script", attrs: [], children: [
    Raw("
        require.config({ paths: { vs: 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs' }});

        require(['vs/editor/editor.main'], function() {
          // Register modal logic language
          monaco.languages.register({ id: 'modal-logic' });

          // Define syntax highlighting
          monaco.languages.setMonarchTokensProvider('modal-logic', {
            tokenizer: {
              root: [
                [/\\b(Necessary|Possible|Obligatory|Permitted|Knows|Believes)\\b/, 'keyword'],
                [/\\b(And|Or|Not|Implies|Atom)\\b/, 'keyword'],
                [/\\b(K|T|K4|S4|S5|KD|KD45)\\b/, 'type'],
                [/□|◇/, 'operator.modal'],
                [/→|∧|∨|¬|↔/, 'operator.logic'],
                [/\\(|\\)/, 'delimiter.parenthesis'],
                [/,/, 'delimiter.comma'],
                [/\"[^\"]*\"/, 'string'],
                [/[a-z][a-z0-9_]*/, 'identifier'],
              ]
            }
          });

          // Define theme colors
          monaco.editor.defineTheme('modal-logic-light', {
            base: 'vs',
            inherit: true,
            rules: [
              { token: 'keyword', foreground: '0000FF', fontStyle: 'bold' },
              { token: 'operator.modal', foreground: 'FF00FF', fontStyle: 'bold' },
              { token: 'operator.logic', foreground: 'A31515' },
              { token: 'type', foreground: '008000' },
            ],
            colors: {}
          });

          // Create editor instance
          window.editor = monaco.editor.create(document.getElementById('editor-container'), {
            value: '-- Enter your modal logic formula here\\n-- Example: Necessary(Implies(Atom(\"p\"), Atom(\"q\")))',
            language: 'modal-logic',
            theme: '" <> theme_name <> "',
            fontSize: " <> string.inspect(config.font_size) <> ",
            tabSize: " <> string.inspect(config.tab_size) <> ",
            minimap: { enabled: " <> string.inspect(config.minimap_enabled) <> " },
            automaticLayout: true,
            suggestOnTriggerCharacters: true,
          });

          // Register autocompletion
          monaco.languages.registerCompletionItemProvider('modal-logic', {
            provideCompletionItems: function(model, position) {
              const word = model.getWordUntilPosition(position);
              const range = {
                startLineNumber: position.lineNumber,
                endLineNumber: position.lineNumber,
                startColumn: word.startColumn,
                endColumn: word.endColumn
              };

              return {
                suggestions: [
                  {
                    label: 'Necessary',
                    kind: monaco.languages.CompletionItemKind.Keyword,
                    insertText: 'Necessary(${1:p})',
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    documentation: 'Necessity operator (□)',
                    range: range
                  },
                  {
                    label: 'Possible',
                    kind: monaco.languages.CompletionItemKind.Keyword,
                    insertText: 'Possible(${1:p})',
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    documentation: 'Possibility operator (◇)',
                    range: range
                  },
                  {
                    label: 'Knows',
                    kind: monaco.languages.CompletionItemKind.Keyword,
                    insertText: 'Knows(${1:agent}, ${2:p})',
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    documentation: 'Epistemic knowledge operator',
                    range: range
                  },
                  {
                    label: 'And',
                    kind: monaco.languages.CompletionItemKind.Keyword,
                    insertText: 'And(${1:p}, ${2:q})',
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    documentation: 'Conjunction (∧)',
                    range: range
                  },
                  {
                    label: 'Implies',
                    kind: monaco.languages.CompletionItemKind.Keyword,
                    insertText: 'Implies(${1:p}, ${2:q})',
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    documentation: 'Implication (→)',
                    range: range
                  },
                ]
              };
            }
          });
        });

        // Helper functions
        function analyzeFormula() {
          const formula = window.editor.getValue();
          console.log('Analyzing:', formula);
          alert('Analysis functionality to be connected to API');
        }

        function checkComplexity() {
          const formula = window.editor.getValue();
          console.log('Checking complexity:', formula);
          alert('Complexity check to be connected to API');
        }

        function changeTheme(theme) {
          monaco.editor.setTheme(theme);
        }
        "),
  ])
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get theme name as string
pub fn theme_name(theme: EditorTheme) -> String {
  case theme {
    Light -> "vs"
    Dark -> "vs-dark"
    HighContrast -> "hc-black"
  }
}

/// Get completion kind name
pub fn completion_kind_name(kind: CompletionKind) -> String {
  case kind {
    Keyword -> "keyword"
    Operator -> "operator"
    Function -> "function"
    Snippet -> "snippet"
    Pattern -> "pattern"
  }
}

/// Generate completion items for modal logic operators
pub fn modal_logic_completions() -> List(CompletionItem) {
  [
    CompletionItem(
      label: "Necessary",
      kind: Keyword,
      detail: "□p",
      documentation: "Necessity operator - true in all accessible worlds",
      insert_text: "Necessary(${1:p})",
    ),
    CompletionItem(
      label: "Possible",
      kind: Keyword,
      detail: "◇p",
      documentation: "Possibility operator - true in at least one accessible world",
      insert_text: "Possible(${1:p})",
    ),
    CompletionItem(
      label: "Knows",
      kind: Keyword,
      detail: "Knows(agent, p)",
      documentation: "Epistemic knowledge operator",
      insert_text: "Knows(${1:agent}, ${2:p})",
    ),
    CompletionItem(
      label: "Believes",
      kind: Keyword,
      detail: "Believes(agent, p)",
      documentation: "Epistemic belief operator",
      insert_text: "Believes(${1:agent}, ${2:p})",
    ),
    CompletionItem(
      label: "And",
      kind: Operator,
      detail: "p ∧ q",
      documentation: "Logical conjunction - both must be true",
      insert_text: "And(${1:p}, ${2:q})",
    ),
    CompletionItem(
      label: "Or",
      kind: Operator,
      detail: "p ∨ q",
      documentation: "Logical disjunction - at least one must be true",
      insert_text: "Or(${1:p}, ${2:q})",
    ),
    CompletionItem(
      label: "Implies",
      kind: Operator,
      detail: "p → q",
      documentation: "Logical implication - if p then q",
      insert_text: "Implies(${1:p}, ${2:q})",
    ),
    CompletionItem(
      label: "Not",
      kind: Operator,
      detail: "¬p",
      documentation: "Logical negation",
      insert_text: "Not(${1:p})",
    ),
  ]
}

/// Generate completion items from pattern library
pub fn pattern_completions() -> List(CompletionItem) {
  [
    CompletionItem(
      label: "Knowledge Implies Truth",
      kind: Pattern,
      detail: "Knows(agent, p) → p",
      documentation: "If an agent knows p, then p is true (epistemic axiom)",
      insert_text: "Implies(Knows(${1:agent}, Atom(\"${2:p}\")), Atom(\"${2:p}\"))",
    ),
    CompletionItem(
      label: "Modus Ponens",
      kind: Pattern,
      detail: "p ∧ (p → q) → q",
      documentation: "Classic modus ponens inference rule",
      insert_text: "Implies(And(Atom(\"${1:p}\"), Implies(Atom(\"${1:p}\"), Atom(\"${2:q}\"))), Atom(\"${2:q}\"))",
    ),
    CompletionItem(
      label: "Necessity Implies Truth",
      kind: Pattern,
      detail: "□p → p",
      documentation: "If p is necessary, then p is true (axiom T)",
      insert_text: "Implies(Necessary(Atom(\"${1:p}\")), Atom(\"${1:p}\"))",
    ),
  ]
}

// =============================================================================
// Syntax Highlighting Tokens
// =============================================================================

/// Get syntax token types
pub fn syntax_tokens() -> List(#(String, String)) {
  [
    #(
      "keyword",
      "Necessary|Possible|Obligatory|Permitted|Knows|Believes|And|Or|Not|Implies|Atom",
    ),
    #("type", "K|T|K4|S4|S5|KD|KD45"),
    #("operator", "□|◇|→|∧|∨|¬|↔"),
    #("delimiter", "\\(|\\)|,"),
    #("string", "\"[^\"]*\""),
    #("identifier", "[a-z][a-z0-9_]*"),
  ]
}

// =============================================================================
// Error Markers
// =============================================================================

/// Error marker for editor
pub type ErrorMarker {
  ErrorMarker(
    line: Int,
    column: Int,
    end_column: Int,
    severity: ErrorSeverity,
    message: String,
  )
}

/// Error severity
pub type ErrorSeverity {
  ErrorLevel
  WarningLevel
  InfoLevel
}

/// Format error markers for Monaco
pub fn format_error_markers(markers: List(ErrorMarker)) -> String {
  "["
  <> {
    markers
    |> list.map(format_error_marker)
    |> string.join(", ")
  }
  <> "]"
}

fn format_error_marker(marker: ErrorMarker) -> String {
  let severity_num = case marker.severity {
    ErrorLevel -> "8"
    WarningLevel -> "4"
    InfoLevel -> "1"
  }

  "{"
  <> "startLineNumber: "
  <> string.inspect(marker.line)
  <> ", startColumn: "
  <> string.inspect(marker.column)
  <> ", endLineNumber: "
  <> string.inspect(marker.line)
  <> ", endColumn: "
  <> string.inspect(marker.end_column)
  <> ", severity: monaco.MarkerSeverity.Error"
  <> ", message: \""
  <> marker.message
  <> "\"}"
}
