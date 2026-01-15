//// Step-by-Step Debugger Module
////
//// This module provides an interactive debugger for proof visualization.
//// It allows stepping through proofs forward and backward, inspecting
//// state at each step, and understanding the verification process.
////
//// ## Features
//// - Step-by-step navigation (forward/backward)
//// - State inspection at each step
//// - Breakpoint support
//// - Watch expressions
//// - Playback controls

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/proof_tree.{
  type ProofState, type ProofStep, type ProofTree, ProofState, ProofStep,
  ProofTree,
}
import modal_logic/proposition.{type LogicSystem, type Proposition}

// ============================================================================
// Debugger Types
// ============================================================================

/// A debugging session for a proof
pub type DebugSession {
  DebugSession(
    id: String,
    proof_tree: ProofTree,
    current_step: Int,
    total_steps: Int,
    breakpoints: List(Breakpoint),
    watches: List(WatchExpression),
    history: List(DebugAction),
    playback_state: PlaybackState,
    config: DebugConfig,
  )
}

/// Configuration for the debugger
pub type DebugConfig {
  DebugConfig(
    auto_step_delay_ms: Int,
    show_intermediate_states: Bool,
    highlight_changes: Bool,
    max_history_size: Int,
    enable_undo: Bool,
  )
}

/// A breakpoint in the proof
pub type Breakpoint {
  Breakpoint(
    id: String,
    breakpoint_type: BreakpointType,
    enabled: Bool,
    hit_count: Int,
    condition: Option(BreakpointCondition),
  )
}

/// Types of breakpoints
pub type BreakpointType {
  /// Break at a specific step number
  StepBreakpoint(step: Int)
  /// Break when entering a specific world
  WorldBreakpoint(world: String)
  /// Break when a specific formula is encountered
  FormulaBreakpoint(formula: Proposition)
  /// Break when a specific rule is applied
  RuleBreakpoint(rule: proof_tree.InferenceRule)
  /// Break on contradiction
  ContradictionBreakpoint
  /// Break when an atom becomes true/false
  AtomBreakpoint(atom: String, value: Bool)
}

/// Condition for conditional breakpoints
pub type BreakpointCondition {
  /// Only break if expression evaluates to true
  WhenTrue(expr: Proposition)
  /// Only break after N hits
  AfterHits(count: Int)
  /// Only break in specific world
  InWorld(world: String)
}

/// Watch expression to track values
pub type WatchExpression {
  WatchExpression(
    id: String,
    name: String,
    expression: WatchExprType,
    current_value: Option(WatchValue),
    history: List(WatchValue),
  )
}

/// Types of watch expressions
pub type WatchExprType {
  /// Watch a formula's truth value at current world
  FormulaWatch(formula: Proposition)
  /// Watch accessibility from a world
  AccessibilityWatch(world: String)
  /// Watch all valuations at a world
  WorldValuationWatch(world: String)
  /// Watch the number of open branches
  OpenBranchesWatch
  /// Watch the current world
  CurrentWorldWatch
}

/// Value of a watch expression
pub type WatchValue {
  BoolValue(Bool)
  StringValue(String)
  IntValue(Int)
  ListValue(List(String))
  WorldValue(world: String, atoms: List(String))
}

/// Actions that can be performed in the debugger
pub type DebugAction {
  StepForward
  StepBackward
  StepToEnd
  StepToStart
  RunToBreakpoint
  ToggleBreakpoint(id: String)
  AddWatch(expr: WatchExpression)
  RemoveWatch(id: String)
  SetPlaybackSpeed(speed: PlaybackSpeed)
  Pause
  Resume
}

/// Playback state for auto-stepping
pub type PlaybackState {
  Playing(speed: PlaybackSpeed)
  Paused
  Stopped
}

/// Playback speed options
pub type PlaybackSpeed {
  Slow
  Normal
  Fast
  Custom(delay_ms: Int)
}

/// Result of a debug step
pub type StepResult {
  StepResult(
    session: DebugSession,
    step: Option(ProofStep),
    state: ProofState,
    breakpoint_hit: Option(Breakpoint),
    watch_updates: List(#(String, WatchValue)),
    events: List(DebugEvent),
  )
}

/// Events emitted during debugging
pub type DebugEvent {
  StepExecuted(step_number: Int)
  BreakpointHit(breakpoint_id: String, reason: String)
  WatchUpdated(
    watch_id: String,
    old_value: Option(WatchValue),
    new_value: WatchValue,
  )
  ProofCompleted(is_valid: Bool)
  ContradictionFound(world: String, formula: Proposition)
  NewWorldExplored(world: String)
  StateChanged(changes: List(StateChange))
}

/// A change in proof state
pub type StateChange {
  FormulaAdded(formula: Proposition, world: String)
  FormulaRemoved(formula: Proposition, world: String)
  WorldAdded(world: String)
  AccessibilityAdded(from: String, to: String)
  AtomValueChanged(atom: String, world: String, new_value: Bool)
}

// ============================================================================
// Session Management
// ============================================================================

/// Create a new debugging session
pub fn new_session(proof_tree: ProofTree) -> DebugSession {
  DebugSession(
    id: generate_session_id(),
    proof_tree: proof_tree,
    current_step: 0,
    total_steps: list.length(proof_tree.steps),
    breakpoints: [],
    watches: [],
    history: [],
    playback_state: Stopped,
    config: default_config(),
  )
}

/// Create a new session with custom configuration
pub fn new_session_with_config(
  proof_tree: ProofTree,
  config: DebugConfig,
) -> DebugSession {
  DebugSession(..new_session(proof_tree), config: config)
}

/// Default debugger configuration
pub fn default_config() -> DebugConfig {
  DebugConfig(
    auto_step_delay_ms: 500,
    show_intermediate_states: True,
    highlight_changes: True,
    max_history_size: 100,
    enable_undo: True,
  )
}

/// Generate a unique session ID
fn generate_session_id() -> String {
  "debug_" <> int.to_string(erlang_system_time())
}

@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

// ============================================================================
// Navigation
// ============================================================================

/// Step forward one step
pub fn step_forward(session: DebugSession) -> StepResult {
  case session.current_step < session.total_steps {
    False ->
      // Already at end
      StepResult(
        session: session,
        step: None,
        state: get_current_state(session),
        breakpoint_hit: None,
        watch_updates: [],
        events: [],
      )
    True -> {
      let next_step_index = session.current_step
      let step = get_step_at(session, next_step_index)

      let new_session =
        DebugSession(
          ..session,
          current_step: session.current_step + 1,
          history: add_to_history(
            session.history,
            StepForward,
            session.config.max_history_size,
          ),
        )

      let state = case step {
        Some(s) -> s.state_after
        None -> get_current_state(session)
      }

      // Check for breakpoints
      let breakpoint_hit = check_breakpoints(new_session, step)

      // Update watches
      let watch_updates = update_watches(new_session, state)

      // Generate events
      let events = generate_step_events(session, new_session, step)

      StepResult(
        session: new_session,
        step: step,
        state: state,
        breakpoint_hit: breakpoint_hit,
        watch_updates: watch_updates,
        events: events,
      )
    }
  }
}

/// Step backward one step
pub fn step_backward(session: DebugSession) -> StepResult {
  case session.current_step > 0 {
    False ->
      // Already at start
      StepResult(
        session: session,
        step: None,
        state: get_initial_state(),
        breakpoint_hit: None,
        watch_updates: [],
        events: [],
      )
    True -> {
      let prev_step_index = session.current_step - 1
      let step = get_step_at(session, prev_step_index)

      let new_session =
        DebugSession(
          ..session,
          current_step: prev_step_index,
          history: add_to_history(
            session.history,
            StepBackward,
            session.config.max_history_size,
          ),
        )

      let state = case step {
        Some(s) -> s.state_before
        None -> get_initial_state()
      }

      let watch_updates = update_watches(new_session, state)

      StepResult(
        session: new_session,
        step: step,
        state: state,
        breakpoint_hit: None,
        watch_updates: watch_updates,
        events: [StepExecuted(prev_step_index)],
      )
    }
  }
}

/// Jump to start
pub fn go_to_start(session: DebugSession) -> StepResult {
  let new_session =
    DebugSession(
      ..session,
      current_step: 0,
      history: add_to_history(
        session.history,
        StepToStart,
        session.config.max_history_size,
      ),
    )

  StepResult(
    session: new_session,
    step: None,
    state: get_initial_state(),
    breakpoint_hit: None,
    watch_updates: update_watches(new_session, get_initial_state()),
    events: [StepExecuted(0)],
  )
}

/// Jump to end
pub fn go_to_end(session: DebugSession) -> StepResult {
  let new_session =
    DebugSession(
      ..session,
      current_step: session.total_steps,
      history: add_to_history(
        session.history,
        StepToEnd,
        session.config.max_history_size,
      ),
    )

  let final_state = get_final_state(session)

  StepResult(
    session: new_session,
    step: get_step_at(session, session.total_steps - 1),
    state: final_state,
    breakpoint_hit: None,
    watch_updates: update_watches(new_session, final_state),
    events: [
      StepExecuted(session.total_steps),
      ProofCompleted(session.proof_tree.metadata.is_valid),
    ],
  )
}

/// Go to a specific step
pub fn go_to_step(session: DebugSession, step_number: Int) -> StepResult {
  let clamped_step = clamp(step_number, 0, session.total_steps)

  let new_session = DebugSession(..session, current_step: clamped_step)

  let state = case clamped_step {
    0 -> get_initial_state()
    _ -> {
      case get_step_at(session, clamped_step - 1) {
        Some(s) -> s.state_after
        None -> get_current_state(session)
      }
    }
  }

  StepResult(
    session: new_session,
    step: get_step_at(session, clamped_step - 1),
    state: state,
    breakpoint_hit: None,
    watch_updates: update_watches(new_session, state),
    events: [StepExecuted(clamped_step)],
  )
}

/// Run until a breakpoint is hit or end is reached
pub fn run_to_breakpoint(session: DebugSession) -> StepResult {
  run_to_breakpoint_loop(session)
}

fn run_to_breakpoint_loop(session: DebugSession) -> StepResult {
  case session.current_step >= session.total_steps {
    True -> go_to_end(session)
    False -> {
      let result = step_forward(session)
      case result.breakpoint_hit {
        Some(_) -> result
        None -> run_to_breakpoint_loop(result.session)
      }
    }
  }
}

// ============================================================================
// Breakpoints
// ============================================================================

/// Add a breakpoint
pub fn add_breakpoint(
  session: DebugSession,
  breakpoint: Breakpoint,
) -> DebugSession {
  DebugSession(
    ..session,
    breakpoints: list.append(session.breakpoints, [breakpoint]),
  )
}

/// Remove a breakpoint by ID
pub fn remove_breakpoint(
  session: DebugSession,
  breakpoint_id: String,
) -> DebugSession {
  DebugSession(
    ..session,
    breakpoints: list.filter(session.breakpoints, fn(bp) {
      bp.id != breakpoint_id
    }),
  )
}

/// Toggle a breakpoint
pub fn toggle_breakpoint(
  session: DebugSession,
  breakpoint_id: String,
) -> DebugSession {
  DebugSession(
    ..session,
    breakpoints: list.map(session.breakpoints, fn(bp) {
      case bp.id == breakpoint_id {
        True -> Breakpoint(..bp, enabled: !bp.enabled)
        False -> bp
      }
    }),
  )
}

/// Create a step breakpoint
pub fn step_breakpoint(step: Int) -> Breakpoint {
  Breakpoint(
    id: "bp_step_" <> int.to_string(step),
    breakpoint_type: StepBreakpoint(step),
    enabled: True,
    hit_count: 0,
    condition: None,
  )
}

/// Create a world breakpoint
pub fn world_breakpoint(world: String) -> Breakpoint {
  Breakpoint(
    id: "bp_world_" <> world,
    breakpoint_type: WorldBreakpoint(world),
    enabled: True,
    hit_count: 0,
    condition: None,
  )
}

/// Create a contradiction breakpoint
pub fn contradiction_breakpoint() -> Breakpoint {
  Breakpoint(
    id: "bp_contradiction",
    breakpoint_type: ContradictionBreakpoint,
    enabled: True,
    hit_count: 0,
    condition: None,
  )
}

/// Check if any breakpoint is hit
fn check_breakpoints(
  session: DebugSession,
  step: Option(ProofStep),
) -> Option(Breakpoint) {
  let enabled_breakpoints =
    list.filter(session.breakpoints, fn(bp) { bp.enabled })

  list.find(enabled_breakpoints, fn(bp) { is_breakpoint_hit(bp, session, step) })
  |> option.from_result
}

fn is_breakpoint_hit(
  breakpoint: Breakpoint,
  session: DebugSession,
  step: Option(ProofStep),
) -> Bool {
  case breakpoint.breakpoint_type {
    StepBreakpoint(target_step) -> session.current_step == target_step
    WorldBreakpoint(world) ->
      case step {
        Some(s) -> s.world == Some(world)
        None -> False
      }
    FormulaBreakpoint(formula) ->
      case step {
        Some(s) -> s.formula == formula
        None -> False
      }
    RuleBreakpoint(rule) ->
      case step {
        Some(s) ->
          case s.action {
            proof_tree.ApplyRule(r) -> r == rule
            _ -> False
          }
        None -> False
      }
    ContradictionBreakpoint ->
      case step {
        Some(s) ->
          case s.action {
            proof_tree.FindContradiction -> True
            _ -> False
          }
        None -> False
      }
    AtomBreakpoint(_, _) -> False
  }
}

// ============================================================================
// Watch Expressions
// ============================================================================

/// Add a watch expression
pub fn add_watch(session: DebugSession, watch: WatchExpression) -> DebugSession {
  DebugSession(..session, watches: list.append(session.watches, [watch]))
}

/// Remove a watch expression
pub fn remove_watch(session: DebugSession, watch_id: String) -> DebugSession {
  DebugSession(
    ..session,
    watches: list.filter(session.watches, fn(w) { w.id != watch_id }),
  )
}

/// Create a formula watch
pub fn formula_watch(name: String, formula: Proposition) -> WatchExpression {
  WatchExpression(
    id: "watch_formula_" <> name,
    name: name,
    expression: FormulaWatch(formula),
    current_value: None,
    history: [],
  )
}

/// Create a world valuation watch
pub fn world_valuation_watch(world: String) -> WatchExpression {
  WatchExpression(
    id: "watch_world_" <> world,
    name: "Valuation at " <> world,
    expression: WorldValuationWatch(world),
    current_value: None,
    history: [],
  )
}

/// Update all watches with current state
fn update_watches(
  session: DebugSession,
  state: ProofState,
) -> List(#(String, WatchValue)) {
  list.filter_map(session.watches, fn(watch) {
    let new_value = evaluate_watch(watch.expression, state)
    case new_value != watch.current_value {
      True ->
        case new_value {
          Some(v) -> Ok(#(watch.id, v))
          None -> Error(Nil)
        }
      False -> Error(Nil)
    }
  })
}

fn evaluate_watch(expr: WatchExprType, state: ProofState) -> Option(WatchValue) {
  case expr {
    FormulaWatch(_) -> None
    AccessibilityWatch(world) -> {
      let accessible =
        list.filter_map(state.accessibility, fn(pair) {
          let #(from, to) = pair
          case from == world {
            True -> Ok(to)
            False -> Error(Nil)
          }
        })
      Some(ListValue(accessible))
    }
    WorldValuationWatch(world) -> {
      let world_state = list.find(state.worlds, fn(w) { w.name == world })
      case world_state {
        Ok(ws) -> Some(WorldValue(ws.name, ws.true_atoms))
        Error(_) -> None
      }
    }
    OpenBranchesWatch -> Some(IntValue(state.open_branches))
    CurrentWorldWatch -> None
  }
}

// ============================================================================
// Playback Controls
// ============================================================================

/// Start auto-play
pub fn play(session: DebugSession, speed: PlaybackSpeed) -> DebugSession {
  DebugSession(..session, playback_state: Playing(speed))
}

/// Pause auto-play
pub fn pause(session: DebugSession) -> DebugSession {
  DebugSession(..session, playback_state: Paused)
}

/// Stop playback
pub fn stop(session: DebugSession) -> DebugSession {
  DebugSession(..session, playback_state: Stopped)
}

/// Get delay for playback speed
pub fn get_playback_delay(speed: PlaybackSpeed) -> Int {
  case speed {
    Slow -> 1000
    Normal -> 500
    Fast -> 100
    Custom(delay) -> delay
  }
}

/// Check if playing
pub fn is_playing(session: DebugSession) -> Bool {
  case session.playback_state {
    Playing(_) -> True
    _ -> False
  }
}

// ============================================================================
// State Inspection
// ============================================================================

/// Get the current proof state
pub fn get_current_state(session: DebugSession) -> ProofState {
  case session.current_step {
    0 -> get_initial_state()
    n -> {
      case get_step_at(session, n - 1) {
        Some(step) -> step.state_after
        None -> get_initial_state()
      }
    }
  }
}

/// Get the initial state
fn get_initial_state() -> ProofState {
  ProofState(
    active_formulas: [],
    worlds: [proof_tree.WorldState("w0", [], [], [])],
    accessibility: [],
    contradictions: [],
    open_branches: 1,
  )
}

/// Get the final state
fn get_final_state(session: DebugSession) -> ProofState {
  case list.last(session.proof_tree.steps) {
    Ok(step) -> step.state_after
    Error(_) -> get_initial_state()
  }
}

/// Get step at index
fn get_step_at(session: DebugSession, index: Int) -> Option(ProofStep) {
  session.proof_tree.steps
  |> list.drop(index)
  |> list.first
  |> option.from_result
}

/// Get current step
pub fn get_current_step(session: DebugSession) -> Option(ProofStep) {
  case session.current_step > 0 {
    True -> get_step_at(session, session.current_step - 1)
    False -> None
  }
}

// ============================================================================
// Export for UI
// ============================================================================

/// Export session state to JSON for UI
pub fn session_to_json(session: DebugSession) -> String {
  "{"
  <> "\"id\": \""
  <> session.id
  <> "\", "
  <> "\"currentStep\": "
  <> int.to_string(session.current_step)
  <> ", "
  <> "\"totalSteps\": "
  <> int.to_string(session.total_steps)
  <> ", "
  <> "\"isPlaying\": "
  <> bool_to_string(is_playing(session))
  <> ", "
  <> "\"isValid\": "
  <> bool_to_string(session.proof_tree.metadata.is_valid)
  <> ", "
  <> "\"breakpoints\": "
  <> breakpoints_to_json(session.breakpoints)
  <> ", "
  <> "\"watches\": "
  <> watches_to_json(session.watches)
  <> ", "
  <> "\"state\": "
  <> state_to_json(get_current_state(session))
  <> "}"
}

fn breakpoints_to_json(breakpoints: List(Breakpoint)) -> String {
  let items =
    list.map(breakpoints, fn(bp) {
      "{"
      <> "\"id\": \""
      <> bp.id
      <> "\", "
      <> "\"enabled\": "
      <> bool_to_string(bp.enabled)
      <> ", "
      <> "\"hitCount\": "
      <> int.to_string(bp.hit_count)
      <> "}"
    })
    |> string.join(", ")
  "[" <> items <> "]"
}

fn watches_to_json(watches: List(WatchExpression)) -> String {
  let items =
    list.map(watches, fn(w) {
      let value_str = case w.current_value {
        Some(BoolValue(b)) -> bool_to_string(b)
        Some(StringValue(s)) -> "\"" <> s <> "\""
        Some(IntValue(i)) -> int.to_string(i)
        Some(ListValue(l)) ->
          "["
          <> string.join(list.map(l, fn(s) { "\"" <> s <> "\"" }), ", ")
          <> "]"
        Some(WorldValue(world, atoms)) ->
          "{\"world\": \""
          <> world
          <> "\", \"atoms\": ["
          <> string.join(list.map(atoms, fn(a) { "\"" <> a <> "\"" }), ", ")
          <> "]}"
        None -> "null"
      }
      "{"
      <> "\"id\": \""
      <> w.id
      <> "\", "
      <> "\"name\": \""
      <> w.name
      <> "\", "
      <> "\"value\": "
      <> value_str
      <> "}"
    })
    |> string.join(", ")
  "[" <> items <> "]"
}

fn state_to_json(state: ProofState) -> String {
  let worlds_json =
    list.map(state.worlds, fn(w) {
      "{"
      <> "\"name\": \""
      <> w.name
      <> "\", "
      <> "\"trueAtoms\": ["
      <> string.join(list.map(w.true_atoms, fn(a) { "\"" <> a <> "\"" }), ", ")
      <> "], "
      <> "\"falseAtoms\": ["
      <> string.join(list.map(w.false_atoms, fn(a) { "\"" <> a <> "\"" }), ", ")
      <> "]"
      <> "}"
    })
    |> string.join(", ")

  let accessibility_json =
    list.map(state.accessibility, fn(pair) {
      let #(from, to) = pair
      "{\"from\": \"" <> from <> "\", \"to\": \"" <> to <> "\"}"
    })
    |> string.join(", ")

  "{"
  <> "\"worlds\": ["
  <> worlds_json
  <> "], "
  <> "\"accessibility\": ["
  <> accessibility_json
  <> "], "
  <> "\"openBranches\": "
  <> int.to_string(state.open_branches)
  <> "}"
}

// ============================================================================
// Helper Functions
// ============================================================================

fn add_to_history(
  history: List(DebugAction),
  action: DebugAction,
  max_size: Int,
) -> List(DebugAction) {
  let new_history = list.append(history, [action])
  case list.length(new_history) > max_size {
    True -> list.drop(new_history, 1)
    False -> new_history
  }
}

fn generate_step_events(
  _old_session: DebugSession,
  new_session: DebugSession,
  step: Option(ProofStep),
) -> List(DebugEvent) {
  let step_event = StepExecuted(new_session.current_step)

  let other_events = case step {
    Some(s) ->
      case s.action {
        proof_tree.ExploreWorld(world) -> [NewWorldExplored(world)]
        proof_tree.FindContradiction -> [
          case list.first(s.state_after.contradictions) {
            Ok(c) -> ContradictionFound(c.world, c.formula)
            Error(_) -> StepExecuted(new_session.current_step)
          },
        ]
        proof_tree.CompleteProof -> [
          ProofCompleted(new_session.proof_tree.metadata.is_valid),
        ]
        _ -> []
      }
    None -> []
  }

  [step_event, ..other_events]
}

fn clamp(value: Int, min_val: Int, max_val: Int) -> Int {
  case value < min_val {
    True -> min_val
    False ->
      case value > max_val {
        True -> max_val
        False -> value
      }
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
