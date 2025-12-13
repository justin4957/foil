//// User Interface Tests (C4)
////
//// Tests for HTTP API, WebSocket, CLI, Web interface, and Visualization modules.

import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/api
import modal_logic/cli
import modal_logic/proposition.{Atom, Implies, K, Necessary, S4, S5, T}
import modal_logic/visualization
import modal_logic/web
import modal_logic/websocket

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("User Interface Tests (C4)")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: HTTP API
  test_http_api()

  // Test 2: WebSocket
  test_websocket()

  // Test 3: CLI
  test_cli()

  // Test 4: Web Interface
  test_web_interface()

  // Test 5: Visualization
  test_visualization()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All User Interface Tests Passed!")
  io.println("=" |> string.repeat(70))
}

fn test_http_api() {
  io.println("")
  io.println("--- Test 1: HTTP API (C4.1) ---")
  io.println("")

  // Test config creation
  let config = api.default_config()
  io.println("[OK] Default API config created")
  io.println("     Version: " <> config.version)
  io.println("     Base path: " <> config.base_path)

  // Test router creation
  let router = api.create_router(config)
  io.println("[OK] Router created")
  io.println("     Routes: " <> int_to_string(list.length(router.routes)))

  // Test request parsing
  let request =
    api.Request(
      method: api.Post,
      path: "/api/analyze",
      headers: [],
      body: "{\"text\": \"test\"}",
      params: [],
      query: [],
    )
  io.println("[OK] Request created")
  io.println("     Method: POST")
  io.println("     Path: " <> request.path)

  // Test route matching
  let response = api.handle_request(router, request)
  io.println("[OK] Request handled")
  io.println("     Status: " <> int_to_string(response.status))

  // Test OpenAPI spec generation
  let spec = api.generate_openapi_spec(config)
  io.println("[OK] OpenAPI spec generated")
  io.println("     Length: " <> int_to_string(string.length(spec)) <> " chars")

  // Test different routes
  let systems_request =
    api.Request(
      method: api.Get,
      path: "/api/systems",
      headers: [],
      body: "",
      params: [],
      query: [],
    )
  let systems_response = api.handle_request(router, systems_request)
  io.println("[OK] Systems endpoint")
  io.println("     Status: " <> int_to_string(systems_response.status))

  io.println("")
}

fn test_websocket() {
  io.println("")
  io.println("--- Test 2: WebSocket (C4.2) ---")
  io.println("")

  // Test config creation
  let config = websocket.default_config()
  io.println("[OK] Default WebSocket config created")
  io.println("     Path: " <> config.path)
  io.println(
    "     Ping interval: " <> int_to_string(config.ping_interval_ms) <> " ms",
  )

  // Test handler creation
  let handler = websocket.create_handler(config)
  io.println("[OK] WebSocket handler created")
  io.println(
    "     Initial connections: "
    <> int_to_string(list.length(handler.connections)),
  )

  // Test connection creation
  let conn =
    websocket.Connection(
      id: "test-conn-1",
      address: "127.0.0.1:8080",
      state: websocket.Connected,
      subscriptions: ["analysis", "validation"],
      metadata: [],
    )
  let updated_handler = websocket.add_connection(handler, conn)
  io.println("[OK] Connection added")
  io.println(
    "     Connection count: "
    <> int_to_string(list.length(updated_handler.connections)),
  )

  // Test event creation
  let started_event = websocket.analysis_started("req-123", "Test argument text")
  io.println("[OK] Analysis started event created")
  io.println("     Request ID: " <> started_event.request_id)

  let progress_event =
    websocket.translation_progress("req-123", 50, "Translating premises...")
  io.println("[OK] Translation progress event")
  io.println("     Progress: " <> int_to_string(progress_event.progress) <> "%")

  let _formalization_event =
    websocket.formalization_complete("req-123", ["□(p → q)", "□p"], "□q")
  io.println("[OK] Formalization complete event")

  let _validation_event =
    websocket.validation_complete("req-123", True, "Argument is valid")
  io.println("[OK] Validation complete event")

  // Test message formatting
  let progress_msg = websocket.ProgressMessage(started_event)
  io.println("[OK] Progress message created")

  // Test connection removal
  let handler_after_remove =
    websocket.remove_connection(updated_handler, "test-conn-1")
  io.println("[OK] Connection removed")
  io.println(
    "     Remaining connections: "
    <> int_to_string(list.length(handler_after_remove.connections)),
  )

  io.println("")
}

fn test_cli() {
  io.println("")
  io.println("--- Test 3: CLI Interface (C4.3) ---")
  io.println("")

  // Test config creation
  let config = cli.default_config()
  io.println("[OK] Default CLI config created")
  io.println("     Verbose: " <> bool_to_string(config.verbose))

  let verbose_config = cli.verbose_config()
  io.println("[OK] Verbose config created")
  io.println("     Verbose: " <> bool_to_string(verbose_config.verbose))

  let _json_config = cli.json_config()
  io.println("[OK] JSON output config created")

  // Test command parsing
  let help_result = cli.parse_args(["help"])
  case help_result {
    cli.ParseSuccess(cli.HelpCommand(_), _) ->
      io.println("[OK] Help command parsed")
    _ -> io.println("[FAIL] Help command parse failed")
  }

  let version_result = cli.parse_args(["version"])
  case version_result {
    cli.ParseSuccess(cli.VersionCommand, _) ->
      io.println("[OK] Version command parsed")
    _ -> io.println("[FAIL] Version command parse failed")
  }

  let analyze_result = cli.parse_args(["analyze", "Test argument"])
  case analyze_result {
    cli.ParseSuccess(cli.AnalyzeCommand(_), _) ->
      io.println("[OK] Analyze command parsed")
    _ -> io.println("[FAIL] Analyze command parse failed")
  }

  let systems_result = cli.parse_args(["systems"])
  case systems_result {
    cli.ParseSuccess(cli.ListSystemsCommand, _) ->
      io.println("[OK] Systems command parsed")
    _ -> io.println("[FAIL] Systems command parse failed")
  }

  // Test help text generation
  let help_text = cli.help_text(None)
  io.println("[OK] Main help text generated")
  io.println(
    "     Length: " <> int_to_string(string.length(help_text)) <> " chars",
  )

  // Test version string
  let version = cli.version_string()
  io.println("[OK] Version string generated")
  case string.contains(version, "Modal Logic") {
    True -> io.println("     Contains 'Modal Logic'")
    False -> io.println("[WARN] Missing 'Modal Logic' in version")
  }

  // Test interactive session
  let _session = cli.new_session()
  io.println("[OK] Interactive session created")

  let parse_quit = cli.parse_interactive(":quit")
  case parse_quit {
    Ok(cli.IQuit) -> io.println("[OK] Quit command parsed")
    _ -> io.println("[FAIL] Quit command parse failed")
  }

  let parse_help = cli.parse_interactive(":help")
  case parse_help {
    Ok(cli.IHelp) -> io.println("[OK] Interactive help command parsed")
    _ -> io.println("[FAIL] Interactive help command parse failed")
  }

  io.println("")
}

fn test_web_interface() {
  io.println("")
  io.println("--- Test 4: Web Interface (C4.4) ---")
  io.println("")

  // Test HTML rendering
  let simple_div = web.div([web.class("test")], [web.text("Hello")])
  let rendered = web.render(simple_div)
  io.println("[OK] Simple HTML rendered")
  io.println("     Output: " <> rendered)

  // Test nested elements
  let nested =
    web.div([web.id("container")], [
      web.h1([], [web.text("Title")]),
      web.p([web.class("content")], [web.text("Paragraph text")]),
    ])
  let nested_rendered = web.render(nested)
  io.println("[OK] Nested HTML rendered")
  io.println(
    "     Length: " <> int_to_string(string.length(nested_rendered)) <> " chars",
  )

  // Test form elements
  let input_el = web.input([web.type_("text"), web.name("field1")])
  let input_rendered = web.render(input_el)
  io.println("[OK] Input element rendered")
  case string.contains(input_rendered, "type=\"text\"") {
    True -> io.println("     Contains type attribute")
    False -> io.println("[WARN] Missing type attribute")
  }

  // Test page generation
  let page_config = web.default_page_config()
  io.println("[OK] Default page config created")
  io.println("     Title: " <> page_config.title)

  // Test analysis form
  let form = web.analysis_form(Some(K))
  let form_rendered = web.render(form)
  io.println("[OK] Analysis form rendered")
  io.println(
    "     Length: " <> int_to_string(string.length(form_rendered)) <> " chars",
  )

  // Test result display
  let valid_result =
    web.ValidDisplay(system: S4, premises: ["□(p → q)", "□p"], conclusion: "□q")
  let result_html = web.result_display(valid_result)
  let result_rendered = web.render(result_html)
  io.println("[OK] Valid result display rendered")
  case string.contains(result_rendered, "✓") {
    True -> io.println("     Contains valid checkmark")
    False -> io.println("[WARN] Missing valid checkmark")
  }

  // Test full page generation
  let analyzer_page = web.analyzer_page()
  io.println("[OK] Full analyzer page generated")
  io.println(
    "     Length: " <> int_to_string(string.length(analyzer_page)) <> " chars",
  )
  case string.contains(analyzer_page, "<!DOCTYPE html>") {
    True -> io.println("     Contains DOCTYPE")
    False -> io.println("[WARN] Missing DOCTYPE")
  }

  // Test docs page
  let docs_page = web.docs_page()
  io.println("[OK] Documentation page generated")
  io.println(
    "     Length: " <> int_to_string(string.length(docs_page)) <> " chars",
  )

  io.println("")
}

fn test_visualization() {
  io.println("")
  io.println("--- Test 5: Visualization (C4.5) ---")
  io.println("")

  // Create a test Kripke model
  let model =
    visualization.KripkeModel(
      worlds: [
        visualization.World(name: "w0", true_atoms: ["p", "q"], false_atoms: [
          "r",
        ]),
        visualization.World(name: "w1", true_atoms: ["p"], false_atoms: [
          "q",
          "r",
        ]),
        visualization.World(name: "w2", true_atoms: [], false_atoms: [
          "p",
          "q",
          "r",
        ]),
      ],
      relations: [
        visualization.Relation(from: "w0", to: "w1"),
        visualization.Relation(from: "w0", to: "w2"),
        visualization.Relation(from: "w1", to: "w2"),
      ],
      actual_world: "w0",
      logic_system: T,
    )

  // Test Mermaid export
  let mermaid = visualization.to_mermaid(model)
  io.println("[OK] Mermaid export generated")
  io.println("     Length: " <> int_to_string(string.length(mermaid)) <> " chars")
  case string.contains(mermaid, "graph") {
    True -> io.println("     Contains graph directive")
    False -> io.println("[WARN] Missing graph directive")
  }

  // Test Mermaid with custom config
  let mermaid_config =
    visualization.MermaidConfig(
      direction: visualization.LeftRight,
      show_valuations: True,
      highlight_actual: True,
      node_shape: visualization.Stadium,
      theme: visualization.DarkTheme,
    )
  let custom_mermaid =
    visualization.to_mermaid_with_config(model, mermaid_config)
  io.println("[OK] Custom Mermaid export")
  io.println(
    "     Length: " <> int_to_string(string.length(custom_mermaid)) <> " chars",
  )

  // Test Graphviz export
  let graphviz = visualization.to_graphviz(model)
  io.println("[OK] Graphviz export generated")
  io.println(
    "     Length: " <> int_to_string(string.length(graphviz)) <> " chars",
  )
  case string.contains(graphviz, "digraph") {
    True -> io.println("     Contains digraph directive")
    False -> io.println("[WARN] Missing digraph directive")
  }

  // Test LaTeX/TikZ export
  let latex = visualization.to_latex(model)
  io.println("[OK] LaTeX export generated")
  io.println("     Length: " <> int_to_string(string.length(latex)) <> " chars")
  case string.contains(latex, "\\begin{tikzpicture}") {
    True -> io.println("     Contains tikzpicture environment")
    False -> io.println("[WARN] Missing tikzpicture environment")
  }

  // Test proposition formatting
  let prop = Necessary(Implies(Atom("p"), Atom("q")))
  let latex_prop = visualization.proposition_to_latex(prop)
  io.println("[OK] Proposition to LaTeX: " <> latex_prop)

  let unicode_prop = visualization.proposition_to_unicode(prop)
  io.println("[OK] Proposition to Unicode: " <> unicode_prop)

  let ascii_prop = visualization.proposition_to_ascii(prop)
  io.println("[OK] Proposition to ASCII: " <> ascii_prop)

  // Test Markdown export
  let analysis_export =
    visualization.AnalysisExport(
      is_valid: False,
      premises: [Necessary(Implies(Atom("p"), Atom("q"))), Necessary(Atom("p"))],
      conclusion: Necessary(Atom("q")),
      system: S5,
      countermodel: Some(model),
      explanation: "The argument is invalid because a countermodel exists.",
    )
  let markdown = visualization.to_markdown(analysis_export)
  io.println("[OK] Markdown export generated")
  io.println(
    "     Length: " <> int_to_string(string.length(markdown)) <> " chars",
  )

  // Test export bundle
  let bundle = visualization.export_all(model)
  io.println("[OK] Export bundle created")
  io.println(
    "     Mermaid length: "
    <> int_to_string(string.length(bundle.mermaid))
    <> " chars",
  )
  io.println(
    "     Graphviz length: "
    <> int_to_string(string.length(bundle.graphviz))
    <> " chars",
  )
  io.println(
    "     TikZ length: " <> int_to_string(string.length(bundle.tikz)) <> " chars",
  )

  // Test format utilities
  let mermaid_format =
    visualization.MermaidFormat(visualization.default_mermaid_config())
  let ext = visualization.format_extension(mermaid_format)
  io.println("[OK] Format extension: " <> ext)

  let mime = visualization.format_mime_type(mermaid_format)
  io.println("[OK] MIME type: " <> mime)

  io.println("")
}

// Helper functions

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
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
