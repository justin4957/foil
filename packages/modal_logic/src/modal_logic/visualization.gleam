//// Visualization Exports Module (C4.5)
////
//// Provides export functionality for Kripke models and analysis results
//// in various formats: Mermaid, Graphviz, LaTeX, and Markdown.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, K4, KD,
  KD45, Knows, Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}

// ============================================================================
// Core Types
// ============================================================================

/// Kripke model for visualization
pub type KripkeModel {
  KripkeModel(
    worlds: List(World),
    relations: List(Relation),
    actual_world: String,
    logic_system: LogicSystem,
  )
}

/// World in a Kripke model
pub type World {
  World(name: String, true_atoms: List(String), false_atoms: List(String))
}

/// Accessibility relation
pub type Relation {
  Relation(from: String, to: String)
}

/// Export format options
pub type ExportFormat {
  MermaidFormat(config: MermaidConfig)
  GraphvizFormat(config: GraphvizConfig)
  LatexFormat(config: LatexConfig)
  MarkdownFormat(config: MarkdownConfig)
}

/// Analysis result for export
pub type AnalysisExport {
  AnalysisExport(
    is_valid: Bool,
    premises: List(Proposition),
    conclusion: Proposition,
    system: LogicSystem,
    countermodel: Option(KripkeModel),
    explanation: String,
  )
}

// ============================================================================
// Mermaid Export
// ============================================================================

/// Mermaid diagram configuration
pub type MermaidConfig {
  MermaidConfig(
    direction: MermaidDirection,
    show_valuations: Bool,
    highlight_actual: Bool,
    node_shape: MermaidNodeShape,
    theme: MermaidTheme,
  )
}

/// Diagram direction
pub type MermaidDirection {
  TopBottom
  BottomTop
  LeftRight
  RightLeft
}

/// Node shape options
pub type MermaidNodeShape {
  Circle
  RoundedRect
  Stadium
  Hexagon
}

/// Theme options
pub type MermaidTheme {
  DefaultTheme
  ForestTheme
  DarkTheme
  NeutralTheme
}

/// Default Mermaid configuration
pub fn default_mermaid_config() -> MermaidConfig {
  MermaidConfig(
    direction: TopBottom,
    show_valuations: True,
    highlight_actual: True,
    node_shape: Circle,
    theme: DefaultTheme,
  )
}

/// Export Kripke model to Mermaid
pub fn to_mermaid(model: KripkeModel) -> String {
  to_mermaid_with_config(model, default_mermaid_config())
}

/// Export with custom configuration
pub fn to_mermaid_with_config(
  model: KripkeModel,
  config: MermaidConfig,
) -> String {
  let direction = case config.direction {
    TopBottom -> "TB"
    BottomTop -> "BT"
    LeftRight -> "LR"
    RightLeft -> "RL"
  }

  let theme_directive = case config.theme {
    DefaultTheme -> ""
    ForestTheme -> "%%{init: {'theme': 'forest'}}%%\n"
    DarkTheme -> "%%{init: {'theme': 'dark'}}%%\n"
    NeutralTheme -> "%%{init: {'theme': 'neutral'}}%%\n"
  }

  let header = theme_directive <> "graph " <> direction <> "\n"

  let world_definitions =
    list.map(model.worlds, fn(w) {
      let label = format_world_label(w, config.show_valuations)
      let shape = format_node_shape(w.name, label, config.node_shape)
      "    " <> shape
    })
    |> string.join("\n")

  let relation_definitions =
    list.map(model.relations, fn(r) { "    " <> r.from <> " --> " <> r.to })
    |> string.join("\n")

  let styling = case config.highlight_actual {
    True ->
      "\n    style "
      <> model.actual_world
      <> " fill:#90EE90,stroke:#228B22,stroke-width:3px"
    False -> ""
  }

  let system_note =
    "\n    subgraph Legend\n        system[\"System: "
    <> system_to_string(model.logic_system)
    <> "\"]\n    end"

  header
  <> world_definitions
  <> "\n"
  <> relation_definitions
  <> styling
  <> system_note
}

fn format_world_label(world: World, show_valuations: Bool) -> String {
  case show_valuations {
    True -> {
      let true_str = case world.true_atoms {
        [] -> "∅"
        atoms -> string.join(atoms, ", ")
      }
      world.name <> "<br/>{" <> true_str <> "}"
    }
    False -> world.name
  }
}

fn format_node_shape(
  node_id: String,
  label: String,
  shape: MermaidNodeShape,
) -> String {
  case shape {
    Circle -> node_id <> "((" <> label <> "))"
    RoundedRect -> node_id <> "(" <> label <> ")"
    Stadium -> node_id <> "([" <> label <> "])"
    Hexagon -> node_id <> "{{" <> label <> "}}"
  }
}

// ============================================================================
// Graphviz Export
// ============================================================================

/// Graphviz configuration
pub type GraphvizConfig {
  GraphvizConfig(
    rankdir: GraphvizRankDir,
    node_style: GraphvizNodeStyle,
    edge_style: GraphvizEdgeStyle,
    show_valuations: Bool,
    highlight_actual: Bool,
    font_name: String,
    font_size: Int,
  )
}

/// Rank direction
pub type GraphvizRankDir {
  GvTopBottom
  GvBottomTop
  GvLeftRight
  GvRightLeft
}

/// Node style
pub type GraphvizNodeStyle {
  GvCircle
  GvEllipse
  GvBox
  GvDoublecircle
}

/// Edge style
pub type GraphvizEdgeStyle {
  GvSolid
  GvDashed
  GvBold
}

/// Default Graphviz configuration
pub fn default_graphviz_config() -> GraphvizConfig {
  GraphvizConfig(
    rankdir: GvTopBottom,
    node_style: GvCircle,
    edge_style: GvSolid,
    show_valuations: True,
    highlight_actual: True,
    font_name: "Helvetica",
    font_size: 12,
  )
}

/// Export Kripke model to Graphviz DOT format
pub fn to_graphviz(model: KripkeModel) -> String {
  to_graphviz_with_config(model, default_graphviz_config())
}

/// Export with custom configuration
pub fn to_graphviz_with_config(
  model: KripkeModel,
  config: GraphvizConfig,
) -> String {
  let rankdir = case config.rankdir {
    GvTopBottom -> "TB"
    GvBottomTop -> "BT"
    GvLeftRight -> "LR"
    GvRightLeft -> "RL"
  }

  let node_shape = case config.node_style {
    GvCircle -> "circle"
    GvEllipse -> "ellipse"
    GvBox -> "box"
    GvDoublecircle -> "doublecircle"
  }

  let edge_style = case config.edge_style {
    GvSolid -> ""
    GvDashed -> "style=dashed"
    GvBold -> "style=bold"
  }

  let header =
    "digraph KripkeModel {\n"
    <> "    rankdir="
    <> rankdir
    <> ";\n"
    <> "    node [shape="
    <> node_shape
    <> ", fontname=\""
    <> config.font_name
    <> "\", fontsize="
    <> int_to_string(config.font_size)
    <> "];\n"
    <> "    edge [fontname=\""
    <> config.font_name
    <> "\", fontsize="
    <> int_to_string(config.font_size - 2)
    <> case edge_style {
      "" -> ""
      s -> ", " <> s
    }
    <> "];\n\n"

  let world_nodes =
    list.map(model.worlds, fn(w) {
      let label = format_graphviz_label(w, config.show_valuations)
      let style = case w.name == model.actual_world && config.highlight_actual {
        True -> ", style=filled, fillcolor=\"#90EE90\", penwidth=3"
        False -> ""
      }
      "    " <> w.name <> " [label=\"" <> label <> "\"" <> style <> "];"
    })
    |> string.join("\n")

  let edges =
    list.map(model.relations, fn(r) {
      "    " <> r.from <> " -> " <> r.to <> ";"
    })
    |> string.join("\n")

  let legend =
    "\n\n    // Legend\n"
    <> "    subgraph cluster_legend {\n"
    <> "        label=\"System: "
    <> system_to_string(model.logic_system)
    <> "\";\n"
    <> "        style=dashed;\n"
    <> "        legend_node [label=\"\", shape=point, width=0];\n"
    <> "    }\n"

  header <> world_nodes <> "\n\n" <> edges <> legend <> "}"
}

fn format_graphviz_label(world: World, show_valuations: Bool) -> String {
  case show_valuations {
    True -> {
      let true_str = case world.true_atoms {
        [] -> "∅"
        atoms -> string.join(atoms, ", ")
      }
      world.name <> "\\n{" <> true_str <> "}"
    }
    False -> world.name
  }
}

// ============================================================================
// LaTeX Export
// ============================================================================

/// LaTeX configuration
pub type LatexConfig {
  LatexConfig(
    package: LatexPackage,
    scale: Float,
    show_valuations: Bool,
    use_symbols: Bool,
    include_preamble: Bool,
  )
}

/// LaTeX package to use
pub type LatexPackage {
  TikzPackage
  ForestPackage
  XypicPackage
}

/// Default LaTeX configuration
pub fn default_latex_config() -> LatexConfig {
  LatexConfig(
    package: TikzPackage,
    scale: 1.0,
    show_valuations: True,
    use_symbols: True,
    include_preamble: True,
  )
}

/// Export Kripke model to LaTeX
pub fn to_latex(model: KripkeModel) -> String {
  to_latex_with_config(model, default_latex_config())
}

/// Export with custom configuration
pub fn to_latex_with_config(model: KripkeModel, config: LatexConfig) -> String {
  case config.package {
    TikzPackage -> to_tikz(model, config)
    ForestPackage -> to_forest(model, config)
    XypicPackage -> to_xypic(model, config)
  }
}

fn to_tikz(model: KripkeModel, config: LatexConfig) -> String {
  let preamble = case config.include_preamble {
    True ->
      "\\documentclass{standalone}\n"
      <> "\\usepackage{tikz}\n"
      <> "\\usetikzlibrary{positioning,arrows.meta}\n\n"
      <> "\\begin{document}\n"
    False -> ""
  }

  let postamble = case config.include_preamble {
    True -> "\\end{document}\n"
    False -> ""
  }

  let scale_str = float_to_string(config.scale)

  let header =
    "\\begin{tikzpicture}[scale="
    <> scale_str
    <> ",\n"
    <> "    every node/.style={circle, draw, minimum size=1.5cm},\n"
    <> "    actual/.style={fill=green!30, very thick},\n"
    <> "    ->{Stealth}]\n\n"

  // Calculate positions (simple grid layout)
  let nodes =
    list.index_map(model.worlds, fn(w, i) {
      let x = i % 3 * 4
      let y = i / 3 * -3
      let label =
        format_tikz_label(w, config.show_valuations, config.use_symbols)
      let style = case w.name == model.actual_world {
        True -> "actual"
        False -> ""
      }
      "    \\node["
      <> style
      <> "] ("
      <> w.name
      <> ") at ("
      <> int_to_string(x)
      <> ","
      <> int_to_string(y)
      <> ") {"
      <> label
      <> "};"
    })
    |> string.join("\n")

  let edges =
    "\n\n"
    <> list.map(model.relations, fn(r) {
      let bend = case r.from == r.to {
        True -> "[loop above]"
        False -> ""
      }
      "    \\draw" <> bend <> " (" <> r.from <> ") -- (" <> r.to <> ");"
    })
    |> string.join("\n")

  let legend =
    "\n\n    % Legend\n"
    <> "    \\node[draw=none, rectangle, below=1cm of "
    <> result.unwrap(list.first(model.worlds), World("w0", [], []))
    |> fn(w: World) { w.name }
    <> "] {System: $\\mathbf{"
    <> system_to_string(model.logic_system)
    <> "}$};"

  preamble
  <> header
  <> nodes
  <> edges
  <> legend
  <> "\n\\end{tikzpicture}\n"
  <> postamble
}

fn to_forest(model: KripkeModel, config: LatexConfig) -> String {
  let preamble = case config.include_preamble {
    True ->
      "\\documentclass{standalone}\n"
      <> "\\usepackage{forest}\n\n"
      <> "\\begin{document}\n"
    False -> ""
  }

  let postamble = case config.include_preamble {
    True -> "\\end{document}\n"
    False -> ""
  }

  // Forest is tree-based, so we do a simple representation
  let content =
    "\\begin{forest}\n"
    <> "  for tree={circle, draw, minimum size=1.2cm}\n"
    <> "  ["
    <> model.actual_world
    <> ", fill=green!30\n"
    <> list.filter_map(model.relations, fn(r) {
      case r.from == model.actual_world {
        True -> Ok("    [" <> r.to <> "]\n")
        False -> Error(Nil)
      }
    })
    |> string.join("")
    <> "  ]\n"
    <> "\\end{forest}\n"

  preamble <> content <> postamble
}

fn to_xypic(model: KripkeModel, config: LatexConfig) -> String {
  let preamble = case config.include_preamble {
    True ->
      "\\documentclass{standalone}\n"
      <> "\\usepackage[all]{xy}\n\n"
      <> "\\begin{document}\n"
    False -> ""
  }

  let postamble = case config.include_preamble {
    True -> "\\end{document}\n"
    False -> ""
  }

  let world_count = list.length(model.worlds)

  let content =
    "\\[\n\\xymatrix{\n"
    <> list.index_map(model.worlds, fn(w, i) {
      let label = format_xypic_label(w, config.show_valuations)
      let style = case w.name == model.actual_world {
        True -> "*+[F-,]{" <> label <> "}"
        False -> label
      }
      let arrows =
        list.filter_map(model.relations, fn(r) {
          case r.from == w.name {
            True -> Ok("\\ar[r]")
            False -> Error(Nil)
          }
        })
        |> string.join("")
      style
      <> arrows
      <> case i + 1 < world_count {
        True ->
          case { i + 1 } % 3 == 0 {
            True -> " \\\\\n"
            False -> " & "
          }
        False -> ""
      }
    })
    |> string.join("")
    <> "\n}\n\\]\n"

  preamble <> content <> postamble
}

fn format_tikz_label(
  world: World,
  show_valuations: Bool,
  use_symbols: Bool,
) -> String {
  case show_valuations {
    True -> {
      let true_str = case world.true_atoms {
        [] ->
          case use_symbols {
            True -> "$\\emptyset$"
            False -> "empty"
          }
        atoms -> string.join(atoms, ", ")
      }
      world.name <> "\\\\{" <> true_str <> "}"
    }
    False -> world.name
  }
}

fn format_xypic_label(world: World, show_valuations: Bool) -> String {
  case show_valuations {
    True -> {
      let true_str = case world.true_atoms {
        [] -> "\\emptyset"
        atoms -> string.join(atoms, ", ")
      }
      world.name <> ":\\{" <> true_str <> "\\}"
    }
    False -> world.name
  }
}

// ============================================================================
// Markdown Export
// ============================================================================

/// Markdown configuration
pub type MarkdownConfig {
  MarkdownConfig(
    include_mermaid: Bool,
    include_explanation: Bool,
    heading_level: Int,
    table_format: TableFormat,
  )
}

/// Table format options
pub type TableFormat {
  StandardTable
  AlignedTable
  CompactTable
}

/// Default Markdown configuration
pub fn default_markdown_config() -> MarkdownConfig {
  MarkdownConfig(
    include_mermaid: True,
    include_explanation: True,
    heading_level: 2,
    table_format: StandardTable,
  )
}

/// Export analysis result to Markdown
pub fn to_markdown(export: AnalysisExport) -> String {
  to_markdown_with_config(export, default_markdown_config())
}

/// Export with custom configuration
pub fn to_markdown_with_config(
  export: AnalysisExport,
  config: MarkdownConfig,
) -> String {
  let heading = string.repeat("#", config.heading_level) <> " "

  let status_section = case export.is_valid {
    True -> heading <> "✓ Valid Argument\n\n"
    False -> heading <> "✗ Invalid Argument\n\n"
  }

  let system_section =
    "**Logic System:** " <> system_to_string(export.system) <> "\n\n"

  let premises_section =
    heading
    <> "# Premises\n\n"
    <> list.index_map(export.premises, fn(p, i) {
      int_to_string(i + 1) <> ". " <> proposition_to_latex(p) <> "\n"
    })
    |> string.join("")
    <> "\n"

  let conclusion_section =
    heading
    <> "# Conclusion\n\n"
    <> "∴ "
    <> proposition_to_latex(export.conclusion)
    <> "\n\n"

  let countermodel_section = case export.countermodel {
    Some(cm) -> format_countermodel_markdown(cm, config)
    None -> ""
  }

  let explanation_section = case config.include_explanation {
    True -> heading <> "# Explanation\n\n" <> export.explanation <> "\n\n"
    False -> ""
  }

  status_section
  <> system_section
  <> premises_section
  <> conclusion_section
  <> countermodel_section
  <> explanation_section
}

fn format_countermodel_markdown(
  model: KripkeModel,
  config: MarkdownConfig,
) -> String {
  let heading = string.repeat("#", config.heading_level) <> "# "

  let header = heading <> "Countermodel\n\n"

  let actual = "**Actual World:** " <> model.actual_world <> "\n\n"

  let worlds_table = format_worlds_table(model.worlds, config.table_format)

  let relations_section =
    "**Accessibility Relations:**\n\n"
    <> list.map(model.relations, fn(r) { "- " <> r.from <> " → " <> r.to })
    |> string.join("\n")
    <> "\n\n"

  let mermaid_section = case config.include_mermaid {
    True -> "**Diagram:**\n\n```mermaid\n" <> to_mermaid(model) <> "\n```\n\n"
    False -> ""
  }

  header <> actual <> worlds_table <> relations_section <> mermaid_section
}

fn format_worlds_table(worlds: List(World), format: TableFormat) -> String {
  let header = case format {
    StandardTable | AlignedTable ->
      "| World | True Atoms | False Atoms |\n|-------|------------|-------------|\n"
    CompactTable -> "| World | Valuation |\n|-------|-----------|\n"
  }

  let rows =
    list.map(worlds, fn(w) {
      case format {
        StandardTable | AlignedTable -> {
          let true_str = case w.true_atoms {
            [] -> "∅"
            atoms -> string.join(atoms, ", ")
          }
          let false_str = case w.false_atoms {
            [] -> "∅"
            atoms -> string.join(atoms, ", ")
          }
          "| " <> w.name <> " | " <> true_str <> " | " <> false_str <> " |"
        }
        CompactTable -> {
          let val_str = case w.true_atoms {
            [] -> "∅"
            atoms -> "{" <> string.join(atoms, ", ") <> "}"
          }
          "| " <> w.name <> " | " <> val_str <> " |"
        }
      }
    })
    |> string.join("\n")

  header <> rows <> "\n\n"
}

// ============================================================================
// Proposition to String Conversions
// ============================================================================

/// Convert proposition to LaTeX string
pub fn proposition_to_latex(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "\\neg " <> proposition_to_latex(inner)
    And(left, right) ->
      "("
      <> proposition_to_latex(left)
      <> " \\land "
      <> proposition_to_latex(right)
      <> ")"
    Or(left, right) ->
      "("
      <> proposition_to_latex(left)
      <> " \\lor "
      <> proposition_to_latex(right)
      <> ")"
    Implies(left, right) ->
      "("
      <> proposition_to_latex(left)
      <> " \\to "
      <> proposition_to_latex(right)
      <> ")"
    Necessary(inner) -> "\\Box " <> proposition_to_latex(inner)
    Possible(inner) -> "\\Diamond " <> proposition_to_latex(inner)
    Obligatory(inner) -> "\\mathcal{O} " <> proposition_to_latex(inner)
    Permitted(inner) -> "\\mathcal{P} " <> proposition_to_latex(inner)
    Knows(agent, inner) -> "K_{" <> agent <> "} " <> proposition_to_latex(inner)
    Believes(agent, inner) ->
      "B_{" <> agent <> "} " <> proposition_to_latex(inner)
  }
}

/// Convert proposition to Unicode string
pub fn proposition_to_unicode(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "¬" <> proposition_to_unicode(inner)
    And(left, right) ->
      "("
      <> proposition_to_unicode(left)
      <> " ∧ "
      <> proposition_to_unicode(right)
      <> ")"
    Or(left, right) ->
      "("
      <> proposition_to_unicode(left)
      <> " ∨ "
      <> proposition_to_unicode(right)
      <> ")"
    Implies(left, right) ->
      "("
      <> proposition_to_unicode(left)
      <> " → "
      <> proposition_to_unicode(right)
      <> ")"
    Necessary(inner) -> "□" <> proposition_to_unicode(inner)
    Possible(inner) -> "◇" <> proposition_to_unicode(inner)
    Obligatory(inner) -> "O" <> proposition_to_unicode(inner)
    Permitted(inner) -> "P" <> proposition_to_unicode(inner)
    Knows(agent, inner) ->
      "K_" <> agent <> "(" <> proposition_to_unicode(inner) <> ")"
    Believes(agent, inner) ->
      "B_" <> agent <> "(" <> proposition_to_unicode(inner) <> ")"
  }
}

/// Convert proposition to ASCII string
pub fn proposition_to_ascii(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "~" <> proposition_to_ascii(inner)
    And(left, right) ->
      "("
      <> proposition_to_ascii(left)
      <> " & "
      <> proposition_to_ascii(right)
      <> ")"
    Or(left, right) ->
      "("
      <> proposition_to_ascii(left)
      <> " | "
      <> proposition_to_ascii(right)
      <> ")"
    Implies(left, right) ->
      "("
      <> proposition_to_ascii(left)
      <> " -> "
      <> proposition_to_ascii(right)
      <> ")"
    Necessary(inner) -> "[]" <> proposition_to_ascii(inner)
    Possible(inner) -> "<>" <> proposition_to_ascii(inner)
    Obligatory(inner) -> "O" <> proposition_to_ascii(inner)
    Permitted(inner) -> "P" <> proposition_to_ascii(inner)
    Knows(agent, inner) ->
      "K_" <> agent <> "(" <> proposition_to_ascii(inner) <> ")"
    Believes(agent, inner) ->
      "B_" <> agent <> "(" <> proposition_to_ascii(inner) <> ")"
  }
}

// ============================================================================
// Export Utilities
// ============================================================================

/// Export all formats at once
pub fn export_all(model: KripkeModel) -> ExportBundle {
  ExportBundle(
    mermaid: to_mermaid(model),
    graphviz: to_graphviz(model),
    tikz: to_latex_with_config(
      model,
      LatexConfig(..default_latex_config(), include_preamble: False),
    ),
    system: model.logic_system,
  )
}

/// Bundle of all export formats
pub type ExportBundle {
  ExportBundle(
    mermaid: String,
    graphviz: String,
    tikz: String,
    system: LogicSystem,
  )
}

/// Generate file extension for format
pub fn format_extension(format: ExportFormat) -> String {
  case format {
    MermaidFormat(_) -> ".md"
    GraphvizFormat(_) -> ".dot"
    LatexFormat(_) -> ".tex"
    MarkdownFormat(_) -> ".md"
  }
}

/// Generate MIME type for format
pub fn format_mime_type(format: ExportFormat) -> String {
  case format {
    MermaidFormat(_) -> "text/markdown"
    GraphvizFormat(_) -> "text/vnd.graphviz"
    LatexFormat(_) -> "application/x-latex"
    MarkdownFormat(_) -> "text/markdown"
  }
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

fn float_to_string(f: Float) -> String {
  // Simple float formatting
  let whole = float_truncate(f)
  let frac = float_truncate({ f -. int_to_float(whole) } *. 10.0)
  int_to_string(whole) <> "." <> int_to_string(frac)
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
