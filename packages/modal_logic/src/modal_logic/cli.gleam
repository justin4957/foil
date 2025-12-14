//// CLI Interface Module (C4.3)
////
//// Provides a command-line interface for modal logic argument analysis.
//// Supports interactive and batch modes, argument input, and result display.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, K, K4, KD,
  KD45, Knows, Necessary, Not, Obligatory, Or, Permitted, Possible, S4, S5, T,
}

// ============================================================================
// CLI Configuration
// ============================================================================

/// Configuration for CLI behavior
pub type CliConfig {
  CliConfig(
    verbose: Bool,
    output_format: OutputFormat,
    color_enabled: Bool,
    show_countermodels: Bool,
    show_repairs: Bool,
    max_width: Int,
  )
}

/// Output format options
pub type OutputFormat {
  TextFormat
  JsonFormat
  MarkdownFormat
  CompactFormat
}

/// Create default CLI configuration
pub fn default_config() -> CliConfig {
  CliConfig(
    verbose: False,
    output_format: TextFormat,
    color_enabled: True,
    show_countermodels: True,
    show_repairs: True,
    max_width: 80,
  )
}

/// Create verbose configuration
pub fn verbose_config() -> CliConfig {
  CliConfig(..default_config(), verbose: True)
}

/// Create JSON output configuration
pub fn json_config() -> CliConfig {
  CliConfig(..default_config(), output_format: JsonFormat, color_enabled: False)
}

/// Create markdown output configuration
pub fn markdown_config() -> CliConfig {
  CliConfig(..default_config(), output_format: MarkdownFormat)
}

/// Create compact configuration
pub fn compact_config() -> CliConfig {
  CliConfig(
    ..default_config(),
    output_format: CompactFormat,
    show_countermodels: False,
    show_repairs: False,
  )
}

// Config modifiers

pub fn with_verbose(config: CliConfig, verbose: Bool) -> CliConfig {
  CliConfig(..config, verbose: verbose)
}

pub fn with_output_format(config: CliConfig, format: OutputFormat) -> CliConfig {
  CliConfig(..config, output_format: format)
}

pub fn with_color(config: CliConfig, enabled: Bool) -> CliConfig {
  CliConfig(..config, color_enabled: enabled)
}

pub fn with_max_width(config: CliConfig, width: Int) -> CliConfig {
  CliConfig(..config, max_width: width)
}

// ============================================================================
// Command Parsing
// ============================================================================

/// CLI command types
pub type Command {
  AnalyzeCommand(input: ArgumentInput)
  ValidateCommand(formalization: FormalizationInput)
  ParseCommand(text: String)
  HelpCommand(topic: Option(String))
  VersionCommand
  InteractiveCommand
  BatchCommand(file_path: String)
  ExportCommand(format: ExportFormat, output_path: String)
  ListSystemsCommand
  ExampleCommand(system: Option(LogicSystem))
}

/// Argument input for analysis
pub type ArgumentInput {
  TextInput(text: String, system: Option(LogicSystem))
  FileInput(path: String, system: Option(LogicSystem))
  StdinInput(system: Option(LogicSystem))
}

/// Direct formalization input
pub type FormalizationInput {
  FormalizationInput(
    premises: List(String),
    conclusion: String,
    system: LogicSystem,
  )
}

/// Export format options
pub type ExportFormat {
  MermaidExport
  GraphvizExport
  LatexExport
  MarkdownExport
  JsonExport
}

/// Parse result
pub type ParseResult {
  ParseSuccess(command: Command, config: CliConfig)
  ParseError(message: String, suggestion: Option(String))
}

/// Parse command-line arguments
pub fn parse_args(args: List(String)) -> ParseResult {
  let config = default_config()
  parse_args_with_config(args, config)
}

/// Parse arguments with custom config
pub fn parse_args_with_config(
  args: List(String),
  config: CliConfig,
) -> ParseResult {
  case args {
    [] -> ParseSuccess(HelpCommand(None), config)
    ["help"] -> ParseSuccess(HelpCommand(None), config)
    ["help", topic] -> ParseSuccess(HelpCommand(Some(topic)), config)
    ["--help"] -> ParseSuccess(HelpCommand(None), config)
    ["-h"] -> ParseSuccess(HelpCommand(None), config)
    ["version"] -> ParseSuccess(VersionCommand, config)
    ["--version"] -> ParseSuccess(VersionCommand, config)
    ["-v"] -> ParseSuccess(VersionCommand, config)
    ["interactive"] -> ParseSuccess(InteractiveCommand, config)
    ["-i"] -> ParseSuccess(InteractiveCommand, config)
    ["systems"] -> ParseSuccess(ListSystemsCommand, config)
    ["example"] -> ParseSuccess(ExampleCommand(None), config)
    ["example", system] ->
      case parse_logic_system(system) {
        Ok(s) -> ParseSuccess(ExampleCommand(Some(s)), config)
        Error(msg) ->
          ParseError(
            msg,
            Some("Try: example K, example T, example S4, example S5"),
          )
      }
    ["analyze", ..rest] -> parse_analyze_command(rest, config)
    ["validate", ..rest] -> parse_validate_command(rest, config)
    ["parse", text] -> ParseSuccess(ParseCommand(text), config)
    ["batch", file] -> ParseSuccess(BatchCommand(file), config)
    ["export", format, output] ->
      case parse_export_format(format) {
        Ok(fmt) -> ParseSuccess(ExportCommand(fmt, output), config)
        Error(msg) ->
          ParseError(
            msg,
            Some("Formats: mermaid, graphviz, latex, markdown, json"),
          )
      }
    [unknown, ..] ->
      ParseError(
        "Unknown command: " <> unknown,
        Some("Try 'help' for available commands"),
      )
  }
}

fn parse_analyze_command(args: List(String), config: CliConfig) -> ParseResult {
  case args {
    [] ->
      ParseError(
        "Missing argument text",
        Some("Usage: analyze <text> [--system K|T|S4|S5]"),
      )
    ["-"] -> ParseSuccess(AnalyzeCommand(StdinInput(None)), config)
    ["--file", path] ->
      ParseSuccess(AnalyzeCommand(FileInput(path, None)), config)
    ["--file", path, "--system", system] ->
      case parse_logic_system(system) {
        Ok(s) -> ParseSuccess(AnalyzeCommand(FileInput(path, Some(s))), config)
        Error(msg) -> ParseError(msg, None)
      }
    [text] -> ParseSuccess(AnalyzeCommand(TextInput(text, None)), config)
    [text, "--system", system] ->
      case parse_logic_system(system) {
        Ok(s) -> ParseSuccess(AnalyzeCommand(TextInput(text, Some(s))), config)
        Error(msg) -> ParseError(msg, None)
      }
    _ ->
      ParseError(
        "Invalid analyze arguments",
        Some("Usage: analyze <text> [--system K|T|S4|S5]"),
      )
  }
}

fn parse_validate_command(args: List(String), config: CliConfig) -> ParseResult {
  case args {
    [] ->
      ParseError(
        "Missing formalization",
        Some("Usage: validate --premises P1,P2 --conclusion C --system K"),
      )
    _ -> {
      let parsed = parse_validate_args(args, [], None, None)
      case parsed {
        Ok(input) -> ParseSuccess(ValidateCommand(input), config)
        Error(msg) -> ParseError(msg, None)
      }
    }
  }
}

fn parse_validate_args(
  args: List(String),
  premises: List(String),
  conclusion: Option(String),
  system: Option(LogicSystem),
) -> Result(FormalizationInput, String) {
  case args {
    [] ->
      case conclusion {
        Some(c) ->
          Ok(FormalizationInput(
            premises: premises,
            conclusion: c,
            system: option.unwrap(system, K),
          ))
        None -> Error("Missing conclusion (--conclusion)")
      }
    ["--premises", p, ..rest] -> {
      let ps = string.split(p, ",")
      parse_validate_args(rest, list.append(premises, ps), conclusion, system)
    }
    ["--conclusion", c, ..rest] ->
      parse_validate_args(rest, premises, Some(c), system)
    ["--system", s, ..rest] ->
      case parse_logic_system(s) {
        Ok(sys) -> parse_validate_args(rest, premises, conclusion, Some(sys))
        Error(msg) -> Error(msg)
      }
    [unknown, ..] -> Error("Unknown option: " <> unknown)
  }
}

fn parse_logic_system(s: String) -> Result(LogicSystem, String) {
  case string.uppercase(s) {
    "K" -> Ok(K)
    "T" -> Ok(T)
    "S4" -> Ok(S4)
    "S5" -> Ok(S5)
    _ -> Error("Unknown logic system: " <> s <> ". Valid: K, T, S4, S5")
  }
}

fn parse_export_format(s: String) -> Result(ExportFormat, String) {
  case string.lowercase(s) {
    "mermaid" -> Ok(MermaidExport)
    "graphviz" | "dot" -> Ok(GraphvizExport)
    "latex" | "tex" -> Ok(LatexExport)
    "markdown" | "md" -> Ok(MarkdownExport)
    "json" -> Ok(JsonExport)
    _ -> Error("Unknown export format: " <> s)
  }
}

// ============================================================================
// Output Formatting
// ============================================================================

/// Analysis result for CLI display
pub type CliResult {
  CliSuccess(output: String, details: Option(String))
  CliFailure(error: String, hint: Option(String))
}

/// Format analysis result for display
pub fn format_result(result: AnalysisResult, config: CliConfig) -> String {
  case config.output_format {
    TextFormat -> format_text_result(result, config)
    JsonFormat -> format_json_result(result)
    MarkdownFormat -> format_markdown_result(result, config)
    CompactFormat -> format_compact_result(result)
  }
}

/// Analysis result structure
pub type AnalysisResult {
  ValidResult(
    is_valid: Bool,
    premises: List(Proposition),
    conclusion: Proposition,
    system: LogicSystem,
    countermodel: Option(CountermodelInfo),
    repairs: List(RepairInfo),
  )
  ErrorResult(message: String, details: Option(String))
}

/// Countermodel information
pub type CountermodelInfo {
  CountermodelInfo(
    worlds: List(WorldInfo),
    relations: List(RelationInfo),
    actual_world: String,
  )
}

/// World information
pub type WorldInfo {
  WorldInfo(name: String, true_props: List(String), false_props: List(String))
}

/// Relation information
pub type RelationInfo {
  RelationInfo(from: String, to: String)
}

/// Repair suggestion information
pub type RepairInfo {
  RepairInfo(suggestion_type: String, description: String, confidence: Float)
}

fn format_text_result(result: AnalysisResult, config: CliConfig) -> String {
  case result {
    ValidResult(is_valid, premises, conclusion, system, countermodel, repairs) -> {
      let header = case is_valid {
        True -> "✓ VALID"
        False -> "✗ INVALID"
      }
      let system_str = logic_system_to_string(system)
      let premise_strs =
        list.map(premises, proposition_to_string)
        |> list.index_map(fn(p, i) { "  " <> int_to_string(i + 1) <> ". " <> p })
        |> string.join("\n")
      let conclusion_str = "  ∴ " <> proposition_to_string(conclusion)

      let base =
        header
        <> " in "
        <> system_str
        <> "\n\nPremises:\n"
        <> premise_strs
        <> "\n\nConclusion:\n"
        <> conclusion_str

      let with_countermodel = case countermodel, config.show_countermodels {
        Some(cm), True -> base <> "\n\n" <> format_countermodel_text(cm)
        _, _ -> base
      }

      case repairs, config.show_repairs {
        [_, ..], True ->
          with_countermodel <> "\n\n" <> format_repairs_text(repairs)
        _, _ -> with_countermodel
      }
    }
    ErrorResult(message, details) -> {
      let base = "Error: " <> message
      case details {
        Some(d) -> base <> "\n\nDetails: " <> d
        None -> base
      }
    }
  }
}

fn format_countermodel_text(cm: CountermodelInfo) -> String {
  let header = "Countermodel (actual world: " <> cm.actual_world <> "):"
  let worlds =
    list.map(cm.worlds, fn(w) {
      let true_str = case w.true_props {
        [] -> "∅"
        props -> string.join(props, ", ")
      }
      let false_str = case w.false_props {
        [] -> "∅"
        props -> string.join(props, ", ")
      }
      "  " <> w.name <> ": {" <> true_str <> "} / ¬{" <> false_str <> "}"
    })
    |> string.join("\n")
  let relations =
    list.map(cm.relations, fn(r) { "  " <> r.from <> " → " <> r.to })
    |> string.join("\n")
  header <> "\n\nWorlds:\n" <> worlds <> "\n\nRelations:\n" <> relations
}

fn format_repairs_text(repairs: List(RepairInfo)) -> String {
  let header = "Repair Suggestions:"
  let items =
    list.map(repairs, fn(r) {
      "  • ["
      <> r.suggestion_type
      <> "] "
      <> r.description
      <> " (confidence: "
      <> float_to_string(r.confidence)
      <> ")"
    })
    |> string.join("\n")
  header <> "\n" <> items
}

fn format_json_result(result: AnalysisResult) -> String {
  case result {
    ValidResult(is_valid, premises, conclusion, system, countermodel, repairs) -> {
      let valid_str = case is_valid {
        True -> "true"
        False -> "false"
      }
      let premises_json =
        list.map(premises, fn(p) { "\"" <> proposition_to_string(p) <> "\"" })
        |> string.join(", ")
      let repairs_json =
        list.map(repairs, fn(r) {
          "{\"type\": \""
          <> r.suggestion_type
          <> "\", \"description\": \""
          <> r.description
          <> "\", \"confidence\": "
          <> float_to_string(r.confidence)
          <> "}"
        })
        |> string.join(", ")
      let cm_json = case countermodel {
        Some(cm) -> format_countermodel_json(cm)
        None -> "null"
      }
      "{"
      <> "\"valid\": "
      <> valid_str
      <> ", "
      <> "\"system\": \""
      <> logic_system_to_string(system)
      <> "\", "
      <> "\"premises\": ["
      <> premises_json
      <> "], "
      <> "\"conclusion\": \""
      <> proposition_to_string(conclusion)
      <> "\", "
      <> "\"countermodel\": "
      <> cm_json
      <> ", "
      <> "\"repairs\": ["
      <> repairs_json
      <> "]"
      <> "}"
    }
    ErrorResult(message, details) -> {
      let details_json = case details {
        Some(d) -> "\"" <> d <> "\""
        None -> "null"
      }
      "{\"error\": \"" <> message <> "\", \"details\": " <> details_json <> "}"
    }
  }
}

fn format_countermodel_json(cm: CountermodelInfo) -> String {
  let worlds_json =
    list.map(cm.worlds, fn(w) {
      let true_json =
        list.map(w.true_props, fn(p) { "\"" <> p <> "\"" })
        |> string.join(", ")
      let false_json =
        list.map(w.false_props, fn(p) { "\"" <> p <> "\"" })
        |> string.join(", ")
      "{\"name\": \""
      <> w.name
      <> "\", \"true\": ["
      <> true_json
      <> "], \"false\": ["
      <> false_json
      <> "]}"
    })
    |> string.join(", ")
  let relations_json =
    list.map(cm.relations, fn(r) {
      "{\"from\": \"" <> r.from <> "\", \"to\": \"" <> r.to <> "\"}"
    })
    |> string.join(", ")
  "{\"worlds\": ["
  <> worlds_json
  <> "], \"relations\": ["
  <> relations_json
  <> "], \"actual\": \""
  <> cm.actual_world
  <> "\"}"
}

fn format_markdown_result(result: AnalysisResult, config: CliConfig) -> String {
  case result {
    ValidResult(is_valid, premises, conclusion, system, countermodel, repairs) -> {
      let header = case is_valid {
        True -> "## ✓ Valid Argument"
        False -> "## ✗ Invalid Argument"
      }
      let system_str = "**Logic System:** " <> logic_system_to_string(system)
      let premises_md =
        list.map(premises, fn(p) { "- " <> proposition_to_string(p) })
        |> string.join("\n")
      let conclusion_md = "**∴** " <> proposition_to_string(conclusion)

      let base =
        header
        <> "\n\n"
        <> system_str
        <> "\n\n### Premises\n\n"
        <> premises_md
        <> "\n\n### Conclusion\n\n"
        <> conclusion_md

      let with_cm = case countermodel, config.show_countermodels {
        Some(cm), True ->
          base <> "\n\n### Countermodel\n\n" <> format_countermodel_markdown(cm)
        _, _ -> base
      }

      case repairs, config.show_repairs {
        [_, ..], True ->
          with_cm
          <> "\n\n### Repair Suggestions\n\n"
          <> format_repairs_markdown(repairs)
        _, _ -> with_cm
      }
    }
    ErrorResult(message, details) -> {
      let base = "## Error\n\n" <> message
      case details {
        Some(d) -> base <> "\n\n```\n" <> d <> "\n```"
        None -> base
      }
    }
  }
}

fn format_countermodel_markdown(cm: CountermodelInfo) -> String {
  let header = "**Actual World:** " <> cm.actual_world
  let worlds =
    list.map(cm.worlds, fn(w) {
      "- **"
      <> w.name
      <> "**: "
      <> string.join(w.true_props, ", ")
      <> " | ¬"
      <> string.join(w.false_props, ", ")
    })
    |> string.join("\n")
  let relations =
    list.map(cm.relations, fn(r) { "- " <> r.from <> " → " <> r.to })
    |> string.join("\n")
  header <> "\n\n**Worlds:**\n" <> worlds <> "\n\n**Relations:**\n" <> relations
}

fn format_repairs_markdown(repairs: List(RepairInfo)) -> String {
  list.map(repairs, fn(r) {
    "- **"
    <> r.suggestion_type
    <> "**: "
    <> r.description
    <> " (_confidence: "
    <> float_to_string(r.confidence)
    <> "_)"
  })
  |> string.join("\n")
}

fn format_compact_result(result: AnalysisResult) -> String {
  case result {
    ValidResult(is_valid, _, _, system, _, _) -> {
      let status = case is_valid {
        True -> "VALID"
        False -> "INVALID"
      }
      status <> " [" <> logic_system_to_string(system) <> "]"
    }
    ErrorResult(message, _) -> "ERROR: " <> message
  }
}

// ============================================================================
// Help Text Generation
// ============================================================================

/// Generate help text
pub fn help_text(topic: Option(String)) -> String {
  case topic {
    None -> main_help()
    Some("analyze") -> analyze_help()
    Some("validate") -> validate_help()
    Some("systems") -> systems_help()
    Some("export") -> export_help()
    Some("examples") -> examples_help()
    Some(t) -> "Unknown help topic: " <> t <> "\n\n" <> main_help()
  }
}

fn main_help() -> String {
  "Modal Logic Analyzer - Command Line Interface

USAGE:
    modal_logic <command> [options]

COMMANDS:
    analyze <text>        Analyze natural language argument
    validate              Validate formal argument directly
    parse <text>          Parse text without validation
    interactive, -i       Enter interactive mode
    batch <file>          Process batch file
    export <fmt> <out>    Export results to file
    systems               List available logic systems
    example [system]      Show example arguments
    help [topic]          Show help (topics: analyze, validate, systems, export)
    version, -v           Show version information

OPTIONS:
    --verbose             Enable verbose output
    --format <fmt>        Output format: text, json, markdown, compact
    --no-color            Disable colored output
    --system <sys>        Logic system: K, T, S4, S5

EXAMPLES:
    modal_logic analyze \"All men are mortal. Socrates is a man. Therefore Socrates is mortal.\"
    modal_logic validate --premises \"□(p→q),□p\" --conclusion \"□q\" --system K
    modal_logic interactive
    modal_logic export mermaid output.md

For more information on a command, use: modal_logic help <command>"
}

fn analyze_help() -> String {
  "ANALYZE - Analyze natural language arguments

USAGE:
    modal_logic analyze <text> [options]
    modal_logic analyze --file <path> [options]
    modal_logic analyze - [options]           (read from stdin)

OPTIONS:
    --system <sys>    Suggest logic system (K, T, S4, S5)
                      If not specified, system is auto-detected

DESCRIPTION:
    Analyzes a natural language argument for validity. The analyzer will:
    1. Parse the argument to identify premises and conclusion
    2. Translate to formal modal logic
    3. Validate using tableau/SMT methods
    4. Report validity and provide countermodel if invalid

EXAMPLES:
    modal_logic analyze \"If it's raining, the ground is wet. It's raining. So the ground is wet.\"
    modal_logic analyze --file argument.txt --system S5
    echo \"...\" | modal_logic analyze -"
}

fn validate_help() -> String {
  "VALIDATE - Validate formal modal logic arguments

USAGE:
    modal_logic validate --premises <P1,P2,...> --conclusion <C> --system <S>

OPTIONS:
    --premises <list>     Comma-separated list of premise formulas
    --conclusion <formula>  The conclusion formula
    --system <sys>        Logic system: K, T, S4, S5 (default: K)

FORMULA SYNTAX:
    p, q, r, ...          Atomic propositions
    ~p, ¬p, !p            Negation
    p & q, p ∧ q          Conjunction
    p | q, p ∨ q          Disjunction
    p -> q, p → q         Implication
    p <-> q, p ↔ q        Biconditional
    []p, □p               Necessity (box)
    <>p, ◇p               Possibility (diamond)

EXAMPLES:
    modal_logic validate --premises \"□(p→q),□p\" --conclusion \"□q\" --system K
    modal_logic validate --premises \"◇p\" --conclusion \"□p\" --system S5"
}

fn systems_help() -> String {
  "LOGIC SYSTEMS - Available modal logic systems

SYSTEM K (Basic Modal Logic):
    - No additional axioms beyond K: □(p→q) → (□p→□q)
    - The weakest normal modal logic
    - Use for: minimal assumptions about necessity/possibility

SYSTEM T (Reflexive):
    - Axiom T: □p → p (what is necessary is true)
    - Accessibility relation is reflexive
    - Use for: when necessary truths must be actual truths

SYSTEM S4 (Reflexive + Transitive):
    - Axiom 4: □p → □□p (necessary truths are necessarily necessary)
    - Accessibility is reflexive and transitive
    - Use for: knowledge, provability

SYSTEM S5 (Equivalence Relation):
    - Axiom 5: ◇p → □◇p (what is possible is necessarily possible)
    - Accessibility is an equivalence relation
    - Use for: metaphysical necessity, logical necessity

CHOOSING A SYSTEM:
    - Start with K if unsure
    - Use T for basic alethic modality
    - Use S4 for epistemic logic (knowledge)
    - Use S5 for logical/metaphysical necessity"
}

fn export_help() -> String {
  "EXPORT - Export analysis results

USAGE:
    modal_logic export <format> <output-path>

FORMATS:
    mermaid     Mermaid diagram (for Markdown rendering)
    graphviz    DOT format (for Graphviz tools)
    latex       LaTeX document
    markdown    Markdown report
    json        JSON data format

DESCRIPTION:
    Exports the last analysis result to the specified format.
    Use after an analyze or validate command.

EXAMPLES:
    modal_logic analyze \"...\" && modal_logic export mermaid diagram.md
    modal_logic export latex report.tex
    modal_logic export json data.json"
}

fn examples_help() -> String {
  "EXAMPLES - Sample modal logic arguments

PROPOSITIONAL (System K):
    Modus Ponens: □(p→q), □p ⊢ □q  [VALID]
    Affirming Consequent: □(p→q), □q ⊢ □p  [INVALID]

SYSTEM T:
    Necessitation Elimination: □p ⊢ p  [VALID]
    Possibility to Necessity: ◇p ⊢ □p  [INVALID]

SYSTEM S4:
    Positive Introspection: □p ⊢ □□p  [VALID]
    Negative Introspection: ¬□p ⊢ □¬□p  [INVALID - valid in S5]

SYSTEM S5:
    S5 Characteristic: ◇p ⊢ □◇p  [VALID]
    Barcan Formula: ∀x□Fx ⊢ □∀xFx  [VALID in S5]

NATURAL LANGUAGE:
    \"It's necessarily true that if it rains, the ground gets wet.
     It's raining. Therefore, the ground is wet.\"
     -> Formalizes to: □(r→w), r ⊢ w (VALID in T)

TRY:
    modal_logic example K
    modal_logic example S5"
}

/// Get version string
pub fn version_string() -> String {
  "Modal Logic Analyzer v0.1.0
Part of the Foil project
Built with Gleam"
}

// ============================================================================
// Interactive Mode
// ============================================================================

/// Interactive session state
pub type InteractiveSession {
  InteractiveSession(
    config: CliConfig,
    history: List(String),
    current_system: LogicSystem,
    last_result: Option(AnalysisResult),
  )
}

/// Create new interactive session
pub fn new_session() -> InteractiveSession {
  InteractiveSession(
    config: default_config(),
    history: [],
    current_system: K,
    last_result: None,
  )
}

/// Interactive command
pub type InteractiveCommand {
  IAnalyze(text: String)
  IValidate(premises: List(String), conclusion: String)
  ISetSystem(system: LogicSystem)
  ISetFormat(format: OutputFormat)
  IShowLast
  IHistory
  IClear
  IHelp
  IQuit
}

/// Parse interactive input
pub fn parse_interactive(input: String) -> Result(InteractiveCommand, String) {
  let trimmed = string.trim(input)
  case string.split(trimmed, " ") {
    [":q"] | [":quit"] | [":exit"] -> Ok(IQuit)
    [":h"] | [":help"] -> Ok(IHelp)
    [":clear"] -> Ok(IClear)
    [":history"] -> Ok(IHistory)
    [":last"] -> Ok(IShowLast)
    [":system", s] ->
      case parse_logic_system(s) {
        Ok(sys) -> Ok(ISetSystem(sys))
        Error(msg) -> Error(msg)
      }
    [":format", f] ->
      case string.lowercase(f) {
        "text" -> Ok(ISetFormat(TextFormat))
        "json" -> Ok(ISetFormat(JsonFormat))
        "markdown" | "md" -> Ok(ISetFormat(MarkdownFormat))
        "compact" -> Ok(ISetFormat(CompactFormat))
        _ -> Error("Unknown format: " <> f)
      }
    _ ->
      case string.starts_with(trimmed, ":v ") {
        True -> {
          let rest = string.drop_start(trimmed, 3)
          parse_validate_inline(rest)
        }
        False -> Ok(IAnalyze(trimmed))
      }
  }
}

fn parse_validate_inline(input: String) -> Result(InteractiveCommand, String) {
  // Format: :v P1, P2 |- C
  case string.split(input, "|-") {
    [premises_str, conclusion] -> {
      let premises = string.split(premises_str, ",") |> list.map(string.trim)
      Ok(IValidate(premises, string.trim(conclusion)))
    }
    _ -> Error("Invalid format. Use: :v P1, P2 |- C")
  }
}

/// Process interactive command
pub fn process_interactive(
  session: InteractiveSession,
  cmd: InteractiveCommand,
) -> #(InteractiveSession, String) {
  case cmd {
    IQuit -> #(session, "Goodbye!")
    IHelp -> #(session, interactive_help())
    IClear -> #(InteractiveSession(..session, history: []), "History cleared.")
    IHistory -> #(session, format_history(session.history))
    IShowLast ->
      case session.last_result {
        Some(r) -> #(session, format_result(r, session.config))
        None -> #(session, "No previous result.")
      }
    ISetSystem(sys) -> {
      let new_session = InteractiveSession(..session, current_system: sys)
      #(new_session, "Logic system set to " <> logic_system_to_string(sys))
    }
    ISetFormat(fmt) -> {
      let new_config = CliConfig(..session.config, output_format: fmt)
      let new_session = InteractiveSession(..session, config: new_config)
      #(new_session, "Output format changed.")
    }
    IAnalyze(text) -> {
      let new_history = list.append(session.history, [text])
      let new_session = InteractiveSession(..session, history: new_history)
      #(new_session, "Analyzing: " <> text <> "\n(Analysis would run here)")
    }
    IValidate(premises, conclusion) -> {
      let input = string.join(premises, ", ") <> " |- " <> conclusion
      let new_history = list.append(session.history, [input])
      let new_session = InteractiveSession(..session, history: new_history)
      #(new_session, "Validating: " <> input <> "\n(Validation would run here)")
    }
  }
}

fn interactive_help() -> String {
  "Interactive Mode Commands:

  <text>           Analyze natural language argument
  :v P1, P2 |- C   Validate formal argument
  :system <K|T|S4|S5>  Change logic system
  :format <fmt>    Change output format
  :last            Show last result
  :history         Show command history
  :clear           Clear history
  :help            Show this help
  :quit            Exit interactive mode"
}

fn format_history(history: List(String)) -> String {
  case history {
    [] -> "No history."
    _ ->
      list.index_map(history, fn(h, i) { int_to_string(i + 1) <> ". " <> h })
      |> string.join("\n")
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn logic_system_to_string(system: LogicSystem) -> String {
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

fn proposition_to_string(prop: Proposition) -> String {
  case prop {
    Atom(name) -> name
    Not(inner) -> "¬" <> proposition_to_string(inner)
    And(left, right) ->
      "("
      <> proposition_to_string(left)
      <> " ∧ "
      <> proposition_to_string(right)
      <> ")"
    Or(left, right) ->
      "("
      <> proposition_to_string(left)
      <> " ∨ "
      <> proposition_to_string(right)
      <> ")"
    Implies(left, right) ->
      "("
      <> proposition_to_string(left)
      <> " → "
      <> proposition_to_string(right)
      <> ")"
    Necessary(inner) -> "□" <> proposition_to_string(inner)
    Possible(inner) -> "◇" <> proposition_to_string(inner)
    Obligatory(inner) -> "O" <> proposition_to_string(inner)
    Permitted(inner) -> "P" <> proposition_to_string(inner)
    Knows(agent, inner) ->
      "K_" <> agent <> "(" <> proposition_to_string(inner) <> ")"
    Believes(agent, inner) ->
      "B_" <> agent <> "(" <> proposition_to_string(inner) <> ")"
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
  // Simple float formatting - whole part only for now
  let whole = float_truncate(f)
  int_to_string(whole)
  <> "."
  <> int_to_string(float_truncate({ f -. int_to_float(whole) } *. 100.0))
}

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
