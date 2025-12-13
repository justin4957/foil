//// Web Interface Module (C4.4)
////
//// Provides HTML generation and web UI components for modal logic analysis.
//// Includes template system, form builders, and result renderers.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{type LogicSystem, K, K4, KD, KD45, S4, S5, T}

// ============================================================================
// HTML Types
// ============================================================================

/// HTML element representation
pub type Html {
  Element(tag: String, attrs: List(Attribute), children: List(Html))
  Text(content: String)
  Raw(html: String)
  Fragment(children: List(Html))
}

/// HTML attribute
pub type Attribute {
  Attr(name: String, value: String)
  BoolAttr(name: String)
  DataAttr(name: String, value: String)
}

/// Render HTML to string
pub fn render(html: Html) -> String {
  case html {
    Element(tag, attrs, children) -> {
      let attrs_str = render_attrs(attrs)
      let children_str = list.map(children, render) |> string.join("")
      case is_void_element(tag) {
        True -> "<" <> tag <> attrs_str <> " />"
        False ->
          "<" <> tag <> attrs_str <> ">" <> children_str <> "</" <> tag <> ">"
      }
    }
    Text(content) -> escape_html(content)
    Raw(html_str) -> html_str
    Fragment(children) -> list.map(children, render) |> string.join("")
  }
}

fn render_attrs(attrs: List(Attribute)) -> String {
  case attrs {
    [] -> ""
    _ -> {
      let rendered =
        list.map(attrs, fn(attr) {
          case attr {
            Attr(name, value) ->
              " " <> name <> "=\"" <> escape_attr(value) <> "\""
            BoolAttr(name) -> " " <> name
            DataAttr(name, value) ->
              " data-" <> name <> "=\"" <> escape_attr(value) <> "\""
          }
        })
      string.join(rendered, "")
    }
  }
}

fn is_void_element(tag: String) -> Bool {
  list.contains(
    [
      "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta",
      "param", "source", "track", "wbr",
    ],
    tag,
  )
}

fn escape_html(text: String) -> String {
  text
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&#39;")
}

fn escape_attr(value: String) -> String {
  value
  |> string.replace("&", "&amp;")
  |> string.replace("\"", "&quot;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
}

// ============================================================================
// HTML Builders
// ============================================================================

/// Create an element
pub fn element(
  tag: String,
  attrs: List(Attribute),
  children: List(Html),
) -> Html {
  Element(tag, attrs, children)
}

/// Create a text node
pub fn text(content: String) -> Html {
  Text(content)
}

/// Create raw HTML (unescaped)
pub fn raw(html: String) -> Html {
  Raw(html)
}

/// Create a fragment
pub fn fragment(children: List(Html)) -> Html {
  Fragment(children)
}

// Common elements
pub fn div(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("div", attrs, children)
}

pub fn span(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("span", attrs, children)
}

pub fn p(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("p", attrs, children)
}

pub fn h1(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("h1", attrs, children)
}

pub fn h2(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("h2", attrs, children)
}

pub fn h3(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("h3", attrs, children)
}

pub fn h4(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("h4", attrs, children)
}

pub fn ul(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("ul", attrs, children)
}

pub fn li(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("li", attrs, children)
}

pub fn a(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("a", attrs, children)
}

pub fn button(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("button", attrs, children)
}

pub fn form(attrs: List(Attribute), children: List(Html)) -> Html {
  Element("form", attrs, children)
}

pub fn input(attrs: List(Attribute)) -> Html {
  Element("input", attrs, [])
}

pub fn textarea(attrs: List(Attribute), content: String) -> Html {
  Element("textarea", attrs, [Text(content)])
}

pub fn select(attrs: List(Attribute), options: List(Html)) -> Html {
  Element("select", attrs, options)
}

pub fn option(value: String, label: String, selected: Bool) -> Html {
  let attrs = case selected {
    True -> [Attr("value", value), BoolAttr("selected")]
    False -> [Attr("value", value)]
  }
  Element("option", attrs, [Text(label)])
}

pub fn label(for_id: String, children: List(Html)) -> Html {
  Element("label", [Attr("for", for_id)], children)
}

pub fn br() -> Html {
  Element("br", [], [])
}

pub fn hr() -> Html {
  Element("hr", [], [])
}

// Common attributes
pub fn id(value: String) -> Attribute {
  Attr("id", value)
}

pub fn class(value: String) -> Attribute {
  Attr("class", value)
}

pub fn style(value: String) -> Attribute {
  Attr("style", value)
}

pub fn href(value: String) -> Attribute {
  Attr("href", value)
}

pub fn src(value: String) -> Attribute {
  Attr("src", value)
}

pub fn type_(value: String) -> Attribute {
  Attr("type", value)
}

pub fn name(value: String) -> Attribute {
  Attr("name", value)
}

pub fn value(v: String) -> Attribute {
  Attr("value", v)
}

pub fn placeholder(value: String) -> Attribute {
  Attr("placeholder", value)
}

pub fn disabled() -> Attribute {
  BoolAttr("disabled")
}

pub fn required() -> Attribute {
  BoolAttr("required")
}

// ============================================================================
// Page Layout
// ============================================================================

/// Page configuration
pub type PageConfig {
  PageConfig(
    title: String,
    description: String,
    theme: Theme,
    include_analytics: Bool,
  )
}

/// Theme options
pub type Theme {
  LightTheme
  DarkTheme
  SystemTheme
}

/// Default page configuration
pub fn default_page_config() -> PageConfig {
  PageConfig(
    title: "Modal Logic Analyzer",
    description: "Analyze and validate modal logic arguments",
    theme: SystemTheme,
    include_analytics: False,
  )
}

/// Generate full HTML page
pub fn page(config: PageConfig, body_content: Html) -> String {
  let theme_class = case config.theme {
    LightTheme -> "theme-light"
    DarkTheme -> "theme-dark"
    SystemTheme -> "theme-system"
  }

  "<!DOCTYPE html>\n"
  <> "<html lang=\"en\" class=\""
  <> theme_class
  <> "\">\n"
  <> "<head>\n"
  <> "  <meta charset=\"UTF-8\">\n"
  <> "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
  <> "  <meta name=\"description\" content=\""
  <> escape_attr(config.description)
  <> "\">\n"
  <> "  <title>"
  <> escape_html(config.title)
  <> "</title>\n"
  <> "  <style>\n"
  <> css_styles()
  <> "  </style>\n"
  <> "</head>\n"
  <> "<body>\n"
  <> render(body_content)
  <> "\n"
  <> "<script>\n"
  <> javascript_code()
  <> "</script>\n"
  <> "</body>\n"
  <> "</html>"
}

fn css_styles() -> String {
  "
    :root {
      --bg-color: #ffffff;
      --text-color: #1a1a2e;
      --primary-color: #4361ee;
      --secondary-color: #3f37c9;
      --success-color: #06d6a0;
      --error-color: #ef476f;
      --border-color: #e0e0e0;
      --shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    }

    .theme-dark {
      --bg-color: #1a1a2e;
      --text-color: #edf2f4;
      --primary-color: #4cc9f0;
      --secondary-color: #4361ee;
      --success-color: #06d6a0;
      --error-color: #ef476f;
      --border-color: #2d2d44;
      --shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
    }

    @media (prefers-color-scheme: dark) {
      .theme-system {
        --bg-color: #1a1a2e;
        --text-color: #edf2f4;
        --primary-color: #4cc9f0;
        --secondary-color: #4361ee;
        --border-color: #2d2d44;
        --shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
      }
    }

    * { box-sizing: border-box; margin: 0; padding: 0; }

    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: var(--bg-color);
      color: var(--text-color);
      line-height: 1.6;
      min-height: 100vh;
    }

    .container { max-width: 1200px; margin: 0 auto; padding: 2rem; }
    .card { background: var(--bg-color); border: 1px solid var(--border-color); border-radius: 8px; padding: 1.5rem; margin-bottom: 1rem; box-shadow: var(--shadow); }
    .header { text-align: center; margin-bottom: 2rem; }
    .header h1 { font-size: 2.5rem; color: var(--primary-color); margin-bottom: 0.5rem; }

    .form-group { margin-bottom: 1rem; }
    .form-group label { display: block; margin-bottom: 0.5rem; font-weight: 600; }
    .form-control { width: 100%; padding: 0.75rem; border: 1px solid var(--border-color); border-radius: 4px; font-size: 1rem; background: var(--bg-color); color: var(--text-color); }
    .form-control:focus { outline: none; border-color: var(--primary-color); }
    textarea.form-control { min-height: 120px; resize: vertical; }

    .btn { display: inline-block; padding: 0.75rem 1.5rem; border: none; border-radius: 4px; font-size: 1rem; cursor: pointer; transition: all 0.2s; }
    .btn-primary { background: var(--primary-color); color: white; }
    .btn-primary:hover { background: var(--secondary-color); }
    .btn-secondary { background: var(--border-color); color: var(--text-color); }

    .result { margin-top: 2rem; }
    .result-valid { border-left: 4px solid var(--success-color); }
    .result-invalid { border-left: 4px solid var(--error-color); }
    .result-header { display: flex; align-items: center; gap: 0.5rem; margin-bottom: 1rem; }
    .result-icon { font-size: 1.5rem; }

    .countermodel { background: rgba(0, 0, 0, 0.05); border-radius: 4px; padding: 1rem; margin-top: 1rem; }
    .world { display: inline-block; padding: 0.5rem 1rem; margin: 0.25rem; border: 1px solid var(--border-color); border-radius: 20px; }
    .world.actual { border-color: var(--primary-color); background: rgba(67, 97, 238, 0.1); }
    .relation { color: var(--secondary-color); }

    .loading { display: none; align-items: center; justify-content: center; gap: 0.5rem; }
    .loading.active { display: flex; }
    .spinner { width: 20px; height: 20px; border: 2px solid var(--border-color); border-top-color: var(--primary-color); border-radius: 50%; animation: spin 1s linear infinite; }
    @keyframes spin { to { transform: rotate(360deg); } }

    .progress-bar { height: 4px; background: var(--border-color); border-radius: 2px; overflow: hidden; }
    .progress-fill { height: 100%; background: var(--primary-color); transition: width 0.3s; }

    .tabs { display: flex; border-bottom: 1px solid var(--border-color); margin-bottom: 1rem; }
    .tab { padding: 0.75rem 1.5rem; border: none; background: none; cursor: pointer; border-bottom: 2px solid transparent; }
    .tab.active { border-bottom-color: var(--primary-color); color: var(--primary-color); }
    .tab-content { display: none; }
    .tab-content.active { display: block; }

    .premise { display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.5rem; }
    .premise-number { min-width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; background: var(--primary-color); color: white; border-radius: 50%; font-size: 0.875rem; }
  "
}

fn javascript_code() -> String {
  "
    // Tab switching
    document.querySelectorAll('.tab').forEach(tab => {
      tab.addEventListener('click', () => {
        const tabGroup = tab.closest('.tabs').parentElement;
        tabGroup.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
        tabGroup.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
        tab.classList.add('active');
        document.getElementById(tab.dataset.tab).classList.add('active');
      });
    });

    // Form submission
    const analyzeForm = document.getElementById('analyze-form');
    if (analyzeForm) {
      analyzeForm.addEventListener('submit', async (e) => {
        e.preventDefault();
        const loading = document.querySelector('.loading');
        const result = document.querySelector('.result');
        loading.classList.add('active');
        result.innerHTML = '';

        const formData = new FormData(analyzeForm);
        const data = Object.fromEntries(formData);

        try {
          const response = await fetch('/api/analyze', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(data)
          });
          const json = await response.json();
          displayResult(json, result);
        } catch (error) {
          result.innerHTML = '<div class=\"card result-invalid\"><p>Error: ' + error.message + '</p></div>';
        } finally {
          loading.classList.remove('active');
        }
      });
    }

    function displayResult(data, container) {
      const isValid = data.valid;
      const cssClass = isValid ? 'result-valid' : 'result-invalid';
      const icon = isValid ? '✓' : '✗';
      const status = isValid ? 'Valid' : 'Invalid';

      let html = '<div class=\"card ' + cssClass + '\">';
      html += '<div class=\"result-header\"><span class=\"result-icon\">' + icon + '</span><h3>' + status + ' in ' + data.system + '</h3></div>';

      if (data.premises && data.premises.length > 0) {
        html += '<h4>Premises:</h4><ul>';
        data.premises.forEach((p, i) => {
          html += '<li class=\"premise\"><span class=\"premise-number\">' + (i + 1) + '</span>' + p + '</li>';
        });
        html += '</ul>';
      }

      if (data.conclusion) {
        html += '<h4>Conclusion:</h4><p>∴ ' + data.conclusion + '</p>';
      }

      if (!isValid && data.countermodel) {
        html += '<div class=\"countermodel\"><h4>Countermodel</h4>';
        html += '<p><strong>Actual world:</strong> ' + data.countermodel.actual + '</p>';
        html += '<div class=\"worlds\">';
        data.countermodel.worlds.forEach(w => {
          const actualClass = w.name === data.countermodel.actual ? ' actual' : '';
          html += '<span class=\"world' + actualClass + '\">' + w.name + ': {' + w.true.join(', ') + '}</span>';
        });
        html += '</div></div>';
      }

      html += '</div>';
      container.innerHTML = html;
    }

    // WebSocket for streaming updates
    function connectWebSocket(requestId) {
      const ws = new WebSocket('ws://' + window.location.host + '/ws');
      ws.onmessage = (event) => {
        const data = JSON.parse(event.data);
        if (data.request_id === requestId) {
          updateProgress(data);
        }
      };
      return ws;
    }

    function updateProgress(event) {
      const progressBar = document.querySelector('.progress-fill');
      if (progressBar) {
        progressBar.style.width = event.progress + '%';
      }
      const statusText = document.querySelector('.status-text');
      if (statusText) {
        statusText.textContent = event.message;
      }
    }
  "
}

// ============================================================================
// Component Builders
// ============================================================================

/// Build the main analysis form
pub fn analysis_form(current_system: Option(LogicSystem)) -> Html {
  let system = option.unwrap(current_system, K)

  div([class("card")], [
    form([id("analyze-form"), Attr("method", "POST")], [
      div([class("tabs")], [
        button([class("tab active"), DataAttr("tab", "natural-tab"), type_("button")], [
          text("Natural Language"),
        ]),
        button([class("tab"), DataAttr("tab", "formal-tab"), type_("button")], [
          text("Formal Logic"),
        ]),
      ]),
      div([id("natural-tab"), class("tab-content active")], [
        div([class("form-group")], [
          label("argument-text", [text("Enter your argument:")]),
          textarea(
            [
              id("argument-text"),
              name("text"),
              class("form-control"),
              placeholder(
                "Enter a natural language argument...\n\nExample: All men are mortal. Socrates is a man. Therefore, Socrates is mortal.",
              ),
              required(),
            ],
            "",
          ),
        ]),
      ]),
      div([id("formal-tab"), class("tab-content")], [
        div([class("form-group")], [
          label("premises", [text("Premises (one per line):")]),
          textarea(
            [
              id("premises"),
              name("premises"),
              class("form-control"),
              placeholder("□(p → q)\n□p"),
            ],
            "",
          ),
        ]),
        div([class("form-group")], [
          label("conclusion", [text("Conclusion:")]),
          input([
            id("conclusion"),
            name("conclusion"),
            class("form-control"),
            type_("text"),
            placeholder("□q"),
          ]),
        ]),
      ]),
      div([class("form-group")], [
        label("logic-system", [text("Logic System:")]),
        select([id("logic-system"), name("system"), class("form-control")], [
          option("K", "K - Basic Modal Logic", system == K),
          option("T", "T - Reflexive", system == T),
          option("S4", "S4 - Reflexive + Transitive", system == S4),
          option("S5", "S5 - Equivalence Relation", system == S5),
        ]),
      ]),
      div([class("form-group")], [
        button([type_("submit"), class("btn btn-primary")], [text("Analyze")]),
        button([type_("button"), class("btn btn-secondary"), id("clear-btn")], [
          text("Clear"),
        ]),
      ]),
      div([class("loading")], [
        div([class("spinner")], []),
        span([class("status-text")], [text("Analyzing...")]),
      ]),
      div([class("progress-bar")], [div([class("progress-fill"), style("width: 0%")], [])]),
    ]),
  ])
}

/// Build result display component
pub fn result_display(result: AnalysisDisplayResult) -> Html {
  case result {
    ValidDisplay(system, premises, conclusion) ->
      valid_result_card(system, premises, conclusion)
    InvalidDisplay(system, premises, conclusion, countermodel, repairs) ->
      invalid_result_card(system, premises, conclusion, countermodel, repairs)
    ErrorDisplay(message) -> error_card(message)
    EmptyDisplay -> div([class("result")], [])
  }
}

/// Analysis result for display
pub type AnalysisDisplayResult {
  ValidDisplay(
    system: LogicSystem,
    premises: List(String),
    conclusion: String,
  )
  InvalidDisplay(
    system: LogicSystem,
    premises: List(String),
    conclusion: String,
    countermodel: Option(CountermodelDisplay),
    repairs: List(RepairDisplay),
  )
  ErrorDisplay(message: String)
  EmptyDisplay
}

/// Countermodel for display
pub type CountermodelDisplay {
  CountermodelDisplay(
    worlds: List(WorldDisplay),
    relations: List(#(String, String)),
    actual_world: String,
  )
}

/// World for display
pub type WorldDisplay {
  WorldDisplay(name: String, true_atoms: List(String), false_atoms: List(String))
}

/// Repair suggestion for display
pub type RepairDisplay {
  RepairDisplay(suggestion_type: String, description: String)
}

fn valid_result_card(
  system: LogicSystem,
  premises: List(String),
  conclusion: String,
) -> Html {
  div([class("card result result-valid")], [
    div([class("result-header")], [
      span([class("result-icon")], [text("✓")]),
      h3([], [text("Valid Argument in " <> system_to_string(system))]),
    ]),
    h4([], [text("Premises:")]),
    ul(
      [],
      list.index_map(premises, fn(p, i) {
        li([class("premise")], [
          span([class("premise-number")], [text(int_to_string(i + 1))]),
          text(p),
        ])
      }),
    ),
    h4([], [text("Conclusion:")]),
    p([], [text("∴ " <> conclusion)]),
    p([class("explanation")], [
      text(
        "The conclusion follows necessarily from the premises in all possible worlds.",
      ),
    ]),
  ])
}

fn invalid_result_card(
  system: LogicSystem,
  premises: List(String),
  conclusion: String,
  countermodel: Option(CountermodelDisplay),
  repairs: List(RepairDisplay),
) -> Html {
  let base_elements = [
    div([class("result-header")], [
      span([class("result-icon")], [text("✗")]),
      h3([], [text("Invalid Argument in " <> system_to_string(system))]),
    ]),
    h4([], [text("Premises:")]),
    ul(
      [],
      list.index_map(premises, fn(p, i) {
        li([class("premise")], [
          span([class("premise-number")], [text(int_to_string(i + 1))]),
          text(p),
        ])
      }),
    ),
    h4([], [text("Conclusion:")]),
    p([], [text("∴ " <> conclusion)]),
  ]

  let with_countermodel = case countermodel {
    Some(cm) -> list.append(base_elements, [countermodel_component(cm)])
    None -> base_elements
  }

  let with_repairs = case repairs {
    [] -> with_countermodel
    _ -> list.append(with_countermodel, [repairs_component(repairs)])
  }

  div([class("card result result-invalid")], with_repairs)
}

fn countermodel_component(cm: CountermodelDisplay) -> Html {
  div([class("countermodel")], [
    h4([], [text("Countermodel")]),
    p([], [
      span([class("label")], [text("Actual world: ")]),
      span([class("value")], [text(cm.actual_world)]),
    ]),
    div([class("worlds")], list.map(cm.worlds, world_badge)),
    div(
      [class("relations")],
      list.map(cm.relations, fn(r) {
        span([class("relation")], [text(r.0 <> " → " <> r.1 <> "  ")])
      }),
    ),
  ])
}

fn world_badge(world: WorldDisplay) -> Html {
  let true_str = string.join(world.true_atoms, ", ")
  let content = world.name <> ": {" <> true_str <> "}"
  span([class("world")], [text(content)])
}

fn repairs_component(repairs: List(RepairDisplay)) -> Html {
  div([class("repairs")], [
    h4([], [text("Repair Suggestions")]),
    ul(
      [],
      list.map(repairs, fn(r) {
        li([], [
          span([class("repair-type")], [text("[" <> r.suggestion_type <> "] ")]),
          text(r.description),
        ])
      }),
    ),
  ])
}

fn error_card(message: String) -> Html {
  div([class("card result result-invalid")], [
    div([class("result-header")], [
      span([class("result-icon")], [text("⚠")]),
      h3([], [text("Error")]),
    ]),
    p([], [text(message)]),
  ])
}

// ============================================================================
// Full Page Builders
// ============================================================================

/// Build the main analyzer page
pub fn analyzer_page() -> String {
  let config = default_page_config()
  let content =
    div([class("container")], [
      div([class("header")], [
        h1([], [text("Modal Logic Analyzer")]),
        p([], [text("Analyze and validate modal logic arguments")]),
      ]),
      analysis_form(None),
      div([class("result")], []),
    ])
  page(config, content)
}

/// Build the documentation page
pub fn docs_page() -> String {
  let config = PageConfig(..default_page_config(), title: "Documentation - Modal Logic Analyzer")
  let content =
    div([class("container")], [
      div([class("header")], [
        h1([], [text("Documentation")]),
      ]),
      div([class("card")], [
        h2([], [text("Logic Systems")]),
        h3([], [text("System K")]),
        p([], [text("The basic modal logic with no additional axioms beyond the K axiom: □(p→q) → (□p→□q)")]),
        h3([], [text("System T")]),
        p([], [text("Adds reflexivity: □p → p (what is necessary is true)")]),
        h3([], [text("System S4")]),
        p([], [text("Adds transitivity: □p → □□p (necessary truths are necessarily necessary)")]),
        h3([], [text("System S5")]),
        p([], [text("Full equivalence relation: ◇p → □◇p (what is possible is necessarily possible)")]),
      ]),
      div([class("card")], [
        h2([], [text("Formula Syntax")]),
        ul([], [
          li([], [text("p, q, r, ... - Atomic propositions")]),
          li([], [text("~p, ¬p, !p - Negation")]),
          li([], [text("p & q, p ∧ q - Conjunction")]),
          li([], [text("p | q, p ∨ q - Disjunction")]),
          li([], [text("p -> q, p → q - Implication")]),
          li([], [text("[]p, □p - Necessity")]),
          li([], [text("<>p, ◇p - Possibility")]),
        ]),
      ]),
    ])
  page(config, content)
}

/// Build API documentation page
pub fn api_docs_page() -> String {
  let config = PageConfig(..default_page_config(), title: "API Documentation")
  let content =
    div([class("container")], [
      div([class("header")], [
        h1([], [text("API Documentation")]),
      ]),
      div([class("card")], [
        h2([], [text("REST Endpoints")]),
        h3([], [text("POST /api/analyze")]),
        p([], [text("Analyze a natural language argument")]),
        raw("<pre><code>{\"text\": \"All men are mortal...\", \"system\": \"K\"}</code></pre>"),
        h3([], [text("POST /api/validate")]),
        p([], [text("Validate a formal argument")]),
        raw("<pre><code>{\"premises\": [\"□(p→q)\", \"□p\"], \"conclusion\": \"□q\", \"system\": \"K\"}</code></pre>"),
        h3([], [text("GET /api/systems")]),
        p([], [text("List available logic systems")]),
      ]),
      div([class("card")], [
        h2([], [text("WebSocket")]),
        p([], [text("Connect to /ws for real-time analysis updates")]),
        h3([], [text("Event Types")]),
        ul([], [
          li([], [text("analysis_started - Analysis has begun")]),
          li([], [text("translation_progress - Translation progress update")]),
          li([], [text("validation_progress - Validation progress update")]),
          li([], [text("analysis_complete - Final result")]),
        ]),
      ]),
    ])
  page(config, content)
}

// ============================================================================
// Helper Functions
// ============================================================================

fn system_to_string(system: LogicSystem) -> String {
  case system {
    K -> "K"
    T -> "T"
    K4 -> "K4"
    S4 -> "S4"
    S5 -> "S5"
    KD -> "KD"
    KD45 -> "KD45"
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_string(-n, "")
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
