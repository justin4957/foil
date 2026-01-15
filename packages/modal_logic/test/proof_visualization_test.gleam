//// Proof Visualization Tests
////
//// Tests for proof tree, debugger, and visualization modules.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleeunit/should
import modal_logic/debugger.{
  type DebugSession, ContradictionBreakpoint, Paused, Playing, StepBreakpoint,
  Stopped, WorldBreakpoint,
}
import modal_logic/proof_tree.{
  type ProofTree, ConclusionNode, ContradictionNode, CounterexampleNode,
  InferenceNode, ModalNode, PremiseNode, ProofNode, WorldTransitionNode,
}
import modal_logic/proof_visualization.{
  type ColorScheme, type KripkeConfig, type ProofTreeConfig,
}
import modal_logic/proposition.{And, Atom, Implies, K, Necessary, Not, S4}
import modal_logic/visualization.{KripkeModel, Relation, World}

// ============================================================================
// Proof Tree Tests
// ============================================================================

pub fn proof_tree_empty_test() {
  let tree = proof_tree.empty(K)

  tree.metadata.system
  |> should.equal(K)

  tree.metadata.is_valid
  |> should.equal(False)

  tree.metadata.total_steps
  |> should.equal(0)
}

pub fn proof_tree_valid_construction_test() {
  let premises = [Atom("p"), Implies(Atom("p"), Atom("q"))]
  let conclusion = Atom("q")
  let steps = []

  let tree = proof_tree.valid_proof(K, premises, conclusion, steps)

  tree.metadata.is_valid
  |> should.equal(True)

  tree.metadata.system
  |> should.equal(K)

  tree.root.node_type
  |> should.equal(ConclusionNode)

  tree.root.formula
  |> should.equal(conclusion)

  list.length(tree.root.children)
  |> should.equal(2)
}

pub fn proof_tree_invalid_construction_test() {
  let premises = [Atom("p")]
  let conclusion = Atom("q")
  let countermodel_worlds = [
    proof_tree.WorldState("w0", ["p"], ["q"], []),
  ]
  let accessibility = []
  let steps = []

  let tree =
    proof_tree.invalid_proof(
      K,
      premises,
      conclusion,
      countermodel_worlds,
      accessibility,
      steps,
    )

  tree.metadata.is_valid
  |> should.equal(False)

  tree.metadata.proof_method
  |> should.equal(proof_tree.CountermodelConstruction)

  tree.metadata.worlds_explored
  |> should.equal(1)
}

pub fn proof_tree_step_generation_test() {
  let premises = [Necessary(Atom("p")), Implies(Atom("p"), Atom("q"))]
  let conclusion = Necessary(Atom("q"))

  let steps = proof_tree.generate_steps(premises, conclusion, S4)

  // Should have at least premise introduction steps + negation step
  { list.length(steps) >= 3 }
  |> should.be_true

  // First steps should be premise introductions
  case list.first(steps) {
    Ok(step) -> {
      step.action
      |> should.equal(proof_tree.IntroducePremise)
    }
    Error(_) -> should.fail()
  }
}

pub fn proof_tree_get_all_nodes_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])

  let nodes = proof_tree.get_all_nodes(tree)

  // Should have root + at least one child
  { list.length(nodes) >= 2 }
  |> should.be_true
}

pub fn proof_tree_find_node_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])

  // Should find conclusion node
  let found = proof_tree.find_node(tree, "conclusion")
  found
  |> option.is_some
  |> should.be_true

  // Should not find non-existent node
  let not_found = proof_tree.find_node(tree, "nonexistent")
  not_found
  |> should.equal(None)
}

pub fn proof_tree_to_d3_json_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])

  let json = proof_tree.to_d3_json(tree)

  // Should be valid JSON structure
  string.contains(json, "\"tree\":")
  |> should.equal(True)

  string.contains(json, "\"metadata\":")
  |> should.equal(True)

  string.contains(json, "\"isValid\": true")
  |> should.equal(True)
}

pub fn proof_tree_steps_to_json_test() {
  let steps = proof_tree.generate_steps([Atom("p")], Atom("q"), K)

  let json = proof_tree.steps_to_json(steps)

  // Should be a JSON array
  string.starts_with(json, "[")
  |> should.equal(True)

  string.ends_with(json, "]")
  |> should.equal(True)
}

// ============================================================================
// Debugger Tests
// ============================================================================

pub fn debugger_new_session_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  session.current_step
  |> should.equal(0)

  session.total_steps
  |> should.equal(0)

  session.playback_state
  |> should.equal(Stopped)

  list.length(session.breakpoints)
  |> should.equal(0)

  list.length(session.watches)
  |> should.equal(0)
}

pub fn debugger_step_forward_test() {
  let steps = proof_tree.generate_steps([Atom("p")], Atom("q"), K)
  let tree =
    proof_tree.ProofTree(
      root: proof_tree.ProofNode(
        id: "root",
        node_type: ConclusionNode,
        formula: Atom("q"),
        children: [],
        justification: proof_tree.Assumption,
        world: Some("w0"),
        evaluation: None,
      ),
      metadata: proof_tree.ProofMetadata(
        system: K,
        is_valid: True,
        total_steps: list.length(steps),
        max_depth: 1,
        worlds_explored: 1,
        proof_method: proof_tree.DirectProof,
        duration_ms: 0,
      ),
      steps: steps,
    )

  let session = debugger.new_session(tree)
  let result = debugger.step_forward(session)

  result.session.current_step
  |> should.equal(1)

  { list.length(result.events) >= 1 }
  |> should.be_true
}

pub fn debugger_step_backward_test() {
  let steps = proof_tree.generate_steps([Atom("p")], Atom("q"), K)
  let tree =
    proof_tree.ProofTree(
      root: proof_tree.ProofNode(
        id: "root",
        node_type: ConclusionNode,
        formula: Atom("q"),
        children: [],
        justification: proof_tree.Assumption,
        world: Some("w0"),
        evaluation: None,
      ),
      metadata: proof_tree.ProofMetadata(
        system: K,
        is_valid: True,
        total_steps: list.length(steps),
        max_depth: 1,
        worlds_explored: 1,
        proof_method: proof_tree.DirectProof,
        duration_ms: 0,
      ),
      steps: steps,
    )

  let session = debugger.new_session(tree)
  // Step forward first
  let result1 = debugger.step_forward(session)
  // Then step backward
  let result2 = debugger.step_backward(result1.session)

  result2.session.current_step
  |> should.equal(0)
}

pub fn debugger_go_to_start_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  // Move forward
  let result1 = debugger.step_forward(session)
  // Go to start
  let result2 = debugger.go_to_start(result1.session)

  result2.session.current_step
  |> should.equal(0)
}

pub fn debugger_go_to_end_test() {
  let steps = proof_tree.generate_steps([Atom("p")], Atom("q"), K)
  let tree =
    proof_tree.ProofTree(
      root: proof_tree.ProofNode(
        id: "root",
        node_type: ConclusionNode,
        formula: Atom("q"),
        children: [],
        justification: proof_tree.Assumption,
        world: Some("w0"),
        evaluation: None,
      ),
      metadata: proof_tree.ProofMetadata(
        system: K,
        is_valid: True,
        total_steps: list.length(steps),
        max_depth: 1,
        worlds_explored: 1,
        proof_method: proof_tree.DirectProof,
        duration_ms: 0,
      ),
      steps: steps,
    )

  let session = debugger.new_session(tree)
  let result = debugger.go_to_end(session)

  result.session.current_step
  |> should.equal(session.total_steps)
}

pub fn debugger_add_breakpoint_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  let bp = debugger.step_breakpoint(5)
  let updated = debugger.add_breakpoint(session, bp)

  list.length(updated.breakpoints)
  |> should.equal(1)
}

pub fn debugger_remove_breakpoint_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  let bp = debugger.step_breakpoint(5)
  let with_bp = debugger.add_breakpoint(session, bp)
  let without_bp = debugger.remove_breakpoint(with_bp, bp.id)

  list.length(without_bp.breakpoints)
  |> should.equal(0)
}

pub fn debugger_toggle_breakpoint_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  let bp = debugger.step_breakpoint(5)
  let with_bp = debugger.add_breakpoint(session, bp)

  // Initially enabled
  case list.first(with_bp.breakpoints) {
    Ok(b) -> b.enabled |> should.equal(True)
    Error(_) -> should.fail()
  }

  // Toggle off
  let toggled = debugger.toggle_breakpoint(with_bp, bp.id)
  case list.first(toggled.breakpoints) {
    Ok(b) -> b.enabled |> should.equal(False)
    Error(_) -> should.fail()
  }
}

pub fn debugger_add_watch_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  let watch = debugger.formula_watch("p_value", Atom("p"))
  let updated = debugger.add_watch(session, watch)

  list.length(updated.watches)
  |> should.equal(1)
}

pub fn debugger_playback_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  // Start playing
  let playing = debugger.play(session, debugger.Normal)
  debugger.is_playing(playing)
  |> should.equal(True)

  // Pause
  let paused = debugger.pause(playing)
  debugger.is_playing(paused)
  |> should.equal(False)

  paused.playback_state
  |> should.equal(Paused)
}

pub fn debugger_session_to_json_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let session = debugger.new_session(tree)

  let json = debugger.session_to_json(session)

  string.contains(json, "\"id\":")
  |> should.equal(True)

  string.contains(json, "\"currentStep\":")
  |> should.equal(True)

  string.contains(json, "\"isPlaying\":")
  |> should.equal(True)
}

// ============================================================================
// Proof Visualization Tests
// ============================================================================

pub fn proof_visualization_default_config_test() {
  let config = proof_visualization.default_proof_config()

  config.width
  |> should.equal(800)

  config.height
  |> should.equal(600)

  config.show_formulas
  |> should.equal(True)
}

pub fn proof_visualization_default_kripke_config_test() {
  let config = proof_visualization.default_kripke_config()

  config.width
  |> should.equal(600)

  config.height
  |> should.equal(400)

  config.show_valuations
  |> should.equal(True)
}

pub fn proof_visualization_color_scheme_test() {
  let scheme = proof_visualization.default_color_scheme()

  string.starts_with(scheme.premise, "#")
  |> should.equal(True)

  string.starts_with(scheme.valid, "#")
  |> should.equal(True)

  let dark = proof_visualization.dark_color_scheme()
  dark.background
  |> should.equal("#303030")
}

pub fn proof_visualization_proof_tree_to_d3_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let config = proof_visualization.default_proof_config()

  let js = proof_visualization.proof_tree_to_d3(tree, config)

  // Should be JavaScript code
  string.contains(js, "d3.select")
  |> should.equal(True)

  string.contains(js, "proofTreeControls")
  |> should.equal(True)

  string.contains(js, "stepForward")
  |> should.equal(True)

  string.contains(js, "stepBackward")
  |> should.equal(True)
}

pub fn proof_visualization_kripke_to_d3_test() {
  let model =
    KripkeModel(
      worlds: [
        World("w0", ["p"], ["q"]),
        World("w1", ["q"], ["p"]),
      ],
      relations: [Relation("w0", "w1")],
      actual_world: "w0",
      logic_system: K,
    )
  let config = proof_visualization.default_kripke_config()

  let js = proof_visualization.kripke_to_d3(model, config)

  // Should be JavaScript code
  string.contains(js, "d3.select")
  |> should.equal(True)

  string.contains(js, "kripkeControls")
  |> should.equal(True)

  string.contains(js, "forceSimulation")
  |> should.equal(True)
}

pub fn proof_visualization_to_html_page_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let model =
    KripkeModel(
      worlds: [World("w0", ["p"], [])],
      relations: [],
      actual_world: "w0",
      logic_system: K,
    )
  let config = proof_visualization.default_proof_config()

  let html = proof_visualization.to_html_page(tree, Some(model), config)

  // Should be valid HTML
  string.contains(html, "<!DOCTYPE html>")
  |> should.equal(True)

  string.contains(html, "<title>Proof Visualization")
  |> should.equal(True)

  string.contains(html, "d3.v7.min.js")
  |> should.equal(True)

  // Should include controls
  string.contains(html, "proofTreeControls")
  |> should.equal(True)

  // Should include Kripke container when model provided
  string.contains(html, "kripke-container")
  |> should.equal(True)
}

pub fn proof_visualization_to_html_page_without_kripke_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let config = proof_visualization.default_proof_config()

  let html = proof_visualization.to_html_page(tree, None, config)

  // Should be valid HTML
  string.contains(html, "<!DOCTYPE html>")
  |> should.equal(True)

  // Should NOT include Kripke container div when no model
  // (CSS may still reference it for styling consistency)
  string.contains(html, "<div id='kripke-container'>")
  |> should.equal(False)
}

pub fn proof_visualization_to_svg_test() {
  let tree = proof_tree.valid_proof(K, [Atom("p")], Atom("q"), [])
  let config = proof_visualization.default_proof_config()

  let svg = proof_visualization.to_svg(tree, config)

  // Should be valid SVG
  string.contains(svg, "<svg")
  |> should.equal(True)

  string.contains(svg, "</svg>")
  |> should.equal(True)

  string.contains(svg, "xmlns")
  |> should.equal(True)
}

// ============================================================================
// Integration Tests
// ============================================================================

pub fn debugger_with_visualization_integration_test() {
  // Create a proof with steps
  let steps =
    proof_tree.generate_steps(
      [Atom("p"), Implies(Atom("p"), Atom("q"))],
      Atom("q"),
      K,
    )

  let tree =
    proof_tree.ProofTree(
      root: proof_tree.ProofNode(
        id: "root",
        node_type: ConclusionNode,
        formula: Atom("q"),
        children: [
          proof_tree.ProofNode(
            id: "p0",
            node_type: PremiseNode,
            formula: Atom("p"),
            children: [],
            justification: proof_tree.Premise(0),
            world: Some("w0"),
            evaluation: Some(True),
          ),
        ],
        justification: proof_tree.Inference(proof_tree.ModusPonens, ["p0", "p1"]),
        world: Some("w0"),
        evaluation: Some(True),
      ),
      metadata: proof_tree.ProofMetadata(
        system: K,
        is_valid: True,
        total_steps: list.length(steps),
        max_depth: 2,
        worlds_explored: 1,
        proof_method: proof_tree.DirectProof,
        duration_ms: 100,
      ),
      steps: steps,
    )

  // Create debug session
  let session = debugger.new_session(tree)

  // Add breakpoints and watches
  let bp = debugger.step_breakpoint(2)
  let watch = debugger.formula_watch("conclusion", Atom("q"))

  let session_with_bp = debugger.add_breakpoint(session, bp)
  let session_with_watch = debugger.add_watch(session_with_bp, watch)

  // Generate visualization
  let config = proof_visualization.default_proof_config()
  let js = proof_visualization.proof_tree_to_d3(tree, config)

  // Export session state
  let session_json = debugger.session_to_json(session_with_watch)

  // All should work together
  list.length(session_with_watch.breakpoints)
  |> should.equal(1)

  list.length(session_with_watch.watches)
  |> should.equal(1)

  string.contains(js, "d3")
  |> should.equal(True)

  string.contains(session_json, "breakpoints")
  |> should.equal(True)
}

pub fn full_workflow_test() {
  // Simulate a complete proof visualization workflow

  // 1. Create premises and conclusion
  let premises = [
    Necessary(Atom("p")),
    Implies(Atom("p"), Atom("q")),
  ]
  let conclusion = Necessary(Atom("q"))

  // 2. Generate proof steps
  let steps = proof_tree.generate_steps(premises, conclusion, S4)

  // 3. Create proof tree
  let tree =
    proof_tree.ProofTree(
      root: proof_tree.ProofNode(
        id: "root",
        node_type: ConclusionNode,
        formula: conclusion,
        children: [],
        justification: proof_tree.Assumption,
        world: Some("w0"),
        evaluation: None,
      ),
      metadata: proof_tree.ProofMetadata(
        system: S4,
        is_valid: False,
        total_steps: list.length(steps),
        max_depth: 1,
        worlds_explored: 2,
        proof_method: proof_tree.CountermodelConstruction,
        duration_ms: 50,
      ),
      steps: steps,
    )

  // 4. Create debug session
  let session = debugger.new_session(tree)

  // 5. Add breakpoints
  let session =
    session
    |> debugger.add_breakpoint(debugger.step_breakpoint(1))
    |> debugger.add_breakpoint(debugger.world_breakpoint("w1"))
    |> debugger.add_breakpoint(debugger.contradiction_breakpoint())

  // 6. Add watches
  let session =
    session
    |> debugger.add_watch(debugger.formula_watch("p", Atom("p")))
    |> debugger.add_watch(debugger.world_valuation_watch("w0"))

  // 7. Step through proof
  let result1 = debugger.step_forward(session)
  let result2 = debugger.step_forward(result1.session)
  let _result3 = debugger.step_backward(result2.session)

  // 8. Generate HTML visualization
  let config = proof_visualization.default_proof_config()
  let html = proof_visualization.to_html_page(tree, None, config)

  // 9. Verify everything works
  list.length(session.breakpoints)
  |> should.equal(3)

  list.length(session.watches)
  |> should.equal(2)

  string.contains(html, "S4")
  |> should.equal(True)

  string.contains(html, "Invalid")
  |> should.equal(True)
}
