//// Proof Tree Module
////
//// This module provides data structures and functions for representing
//// and manipulating proof trees extracted from Z3 solver results.
//// Proof trees visualize the logical derivation steps that lead to
//// validity or invalidity of modal logic arguments.
////
//// ## Features
//// - Proof tree data structures
//// - Proof step extraction
//// - Tree traversal and manipulation
//// - Export to various formats

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/proposition.{
  type LogicSystem, type Proposition, And, Atom, Believes, Implies, Knows,
  Necessary, Not, Or, Permitted, Possible,
}

// ============================================================================
// Proof Tree Types
// ============================================================================

/// A proof tree representing a logical derivation
pub type ProofTree {
  ProofTree(root: ProofNode, metadata: ProofMetadata, steps: List(ProofStep))
}

/// A node in the proof tree
pub type ProofNode {
  ProofNode(
    id: String,
    node_type: ProofNodeType,
    formula: Proposition,
    children: List(ProofNode),
    justification: Justification,
    world: Option(String),
    evaluation: Option(Bool),
  )
}

/// Type of proof node
pub type ProofNodeType {
  /// A premise or assumption
  PremiseNode
  /// An inference step
  InferenceNode
  /// The conclusion being proved/disproved
  ConclusionNode
  /// A modal operator application
  ModalNode
  /// A world transition in Kripke semantics
  WorldTransitionNode
  /// A contradiction found
  ContradictionNode
  /// A counterexample world
  CounterexampleNode
}

/// Justification for a proof step
pub type Justification {
  /// Direct premise
  Premise(premise_index: Int)
  /// Inference rule application
  Inference(rule: InferenceRule, from: List(String))
  /// Modal operator elimination
  ModalElim(operator: ModalOperator, world_from: String, world_to: String)
  /// Modal operator introduction
  ModalIntro(operator: ModalOperator, universal: Bool)
  /// Assumption for proof by contradiction
  Assumption
  /// Negation of conclusion for refutation
  NegatedConclusion
  /// Frame property constraint
  FrameProperty(property: FramePropertyType)
  /// Countermodel witness
  Witness(world: String, valuation: Dict(String, Bool))
}

/// Inference rules used in modal logic proofs
pub type InferenceRule {
  /// Modus Ponens: From A and A→B, derive B
  ModusPonens
  /// Modus Tollens: From ¬B and A→B, derive ¬A
  ModusTollens
  /// Conjunction Introduction: From A and B, derive A∧B
  AndIntro
  /// Conjunction Elimination: From A∧B, derive A (or B)
  AndElim
  /// Disjunction Introduction: From A, derive A∨B
  OrIntro
  /// Disjunction Elimination: From A∨B and A→C and B→C, derive C
  OrElim
  /// Double Negation Elimination: From ¬¬A, derive A
  DoubleNegElim
  /// Necessitation: From A (proved), derive □A
  Necessitation
  /// Distribution Axiom (K): From □(A→B), derive □A→□B
  DistributionK
  /// T Axiom: From □A, derive A
  ReflexivityT
  /// 4 Axiom: From □A, derive □□A
  Transitivity4
  /// 5 Axiom: From ◇A, derive □◇A
  Euclidean5
  /// B Axiom: From A, derive □◇A
  SymmetryB
  /// D Axiom: From □A, derive ◇A
  SerialityD
}

/// Modal operators
pub type ModalOperator {
  BoxOp
  DiamondOp
  KnowsOp(agent: String)
  BelievesOp(agent: String)
  ObligatoryOp
  PermittedOp
}

/// Frame property types
pub type FramePropertyType {
  Reflexivity
  Symmetry
  Transitivity
  Seriality
  Euclidean
}

/// Metadata about the proof
pub type ProofMetadata {
  ProofMetadata(
    system: LogicSystem,
    is_valid: Bool,
    total_steps: Int,
    max_depth: Int,
    worlds_explored: Int,
    proof_method: ProofMethod,
    duration_ms: Int,
  )
}

/// Method used for proof
pub type ProofMethod {
  /// Direct proof of validity
  DirectProof
  /// Proof by refutation (showing negation is unsatisfiable)
  RefutationProof
  /// Countermodel construction (for invalidity)
  CountermodelConstruction
  /// Semantic tableau method
  TableauMethod
}

/// A single step in the proof
pub type ProofStep {
  ProofStep(
    step_number: Int,
    node_id: String,
    action: StepAction,
    formula: Proposition,
    world: Option(String),
    justification: Justification,
    state_before: ProofState,
    state_after: ProofState,
  )
}

/// Action taken in a proof step
pub type StepAction {
  /// Introduce a premise
  IntroducePremise
  /// Apply an inference rule
  ApplyRule(rule: InferenceRule)
  /// Negate the conclusion for refutation
  NegateConclusion
  /// Explore a new world
  ExploreWorld(world: String)
  /// Evaluate formula at world
  EvaluateAtWorld
  /// Check accessibility relation
  CheckAccessibility(from: String, to: String)
  /// Find contradiction
  FindContradiction
  /// Construct counterexample
  ConstructCounterexample
  /// Complete proof
  CompleteProof
}

/// State of the proof at a given step
pub type ProofState {
  ProofState(
    active_formulas: List(#(Proposition, String)),
    worlds: List(WorldState),
    accessibility: List(#(String, String)),
    contradictions: List(Contradiction),
    open_branches: Int,
  )
}

/// State of a world
pub type WorldState {
  WorldState(
    name: String,
    true_atoms: List(String),
    false_atoms: List(String),
    forced_formulas: List(Proposition),
  )
}

/// A contradiction in the proof
pub type Contradiction {
  Contradiction(
    formula: Proposition,
    world: String,
    positive_ref: String,
    negative_ref: String,
  )
}

// ============================================================================
// Proof Tree Construction
// ============================================================================

/// Create an empty proof tree
pub fn empty(system: LogicSystem) -> ProofTree {
  ProofTree(
    root: ProofNode(
      id: "root",
      node_type: ConclusionNode,
      formula: Atom("empty"),
      children: [],
      justification: Assumption,
      world: Some("w0"),
      evaluation: None,
    ),
    metadata: ProofMetadata(
      system: system,
      is_valid: False,
      total_steps: 0,
      max_depth: 0,
      worlds_explored: 1,
      proof_method: DirectProof,
      duration_ms: 0,
    ),
    steps: [],
  )
}

/// Create a proof tree for a valid argument
pub fn valid_proof(
  system: LogicSystem,
  premises: List(Proposition),
  conclusion: Proposition,
  steps: List(ProofStep),
) -> ProofTree {
  let premise_nodes =
    list.index_map(premises, fn(p, i) {
      ProofNode(
        id: "p" <> int.to_string(i),
        node_type: PremiseNode,
        formula: p,
        children: [],
        justification: Premise(i),
        world: Some("w0"),
        evaluation: Some(True),
      )
    })

  let conclusion_node =
    ProofNode(
      id: "conclusion",
      node_type: ConclusionNode,
      formula: conclusion,
      children: premise_nodes,
      justification: Inference(
        ModusPonens,
        list.map(premise_nodes, fn(n) { n.id }),
      ),
      world: Some("w0"),
      evaluation: Some(True),
    )

  ProofTree(
    root: conclusion_node,
    metadata: ProofMetadata(
      system: system,
      is_valid: True,
      total_steps: list.length(steps),
      max_depth: calculate_depth(conclusion_node),
      worlds_explored: 1,
      proof_method: DirectProof,
      duration_ms: 0,
    ),
    steps: steps,
  )
}

/// Create a proof tree for an invalid argument with countermodel
pub fn invalid_proof(
  system: LogicSystem,
  premises: List(Proposition),
  conclusion: Proposition,
  countermodel_worlds: List(WorldState),
  accessibility: List(#(String, String)),
  steps: List(ProofStep),
) -> ProofTree {
  let premise_nodes =
    list.index_map(premises, fn(p, i) {
      ProofNode(
        id: "p" <> int.to_string(i),
        node_type: PremiseNode,
        formula: p,
        children: [],
        justification: Premise(i),
        world: Some("w0"),
        evaluation: Some(True),
      )
    })

  let counterexample_nodes =
    list.map(countermodel_worlds, fn(w) {
      ProofNode(
        id: "cm_" <> w.name,
        node_type: CounterexampleNode,
        formula: Atom(w.name),
        children: [],
        justification: Witness(
          w.name,
          dict.from_list(
            list.flatten([
              list.map(w.true_atoms, fn(a) { #(a, True) }),
              list.map(w.false_atoms, fn(a) { #(a, False) }),
            ]),
          ),
        ),
        world: Some(w.name),
        evaluation: None,
      )
    })

  let conclusion_node =
    ProofNode(
      id: "conclusion",
      node_type: ConclusionNode,
      formula: conclusion,
      children: list.flatten([premise_nodes, counterexample_nodes]),
      justification: NegatedConclusion,
      world: Some("w0"),
      evaluation: Some(False),
    )

  ProofTree(
    root: conclusion_node,
    metadata: ProofMetadata(
      system: system,
      is_valid: False,
      total_steps: list.length(steps),
      max_depth: calculate_depth(conclusion_node),
      worlds_explored: list.length(countermodel_worlds),
      proof_method: CountermodelConstruction,
      duration_ms: 0,
    ),
    steps: steps,
  )
}

/// Calculate the depth of a proof node
fn calculate_depth(node: ProofNode) -> Int {
  case node.children {
    [] -> 1
    children -> {
      let max_child_depth =
        list.fold(children, 0, fn(max, child) {
          let child_depth = calculate_depth(child)
          case child_depth > max {
            True -> child_depth
            False -> max
          }
        })
      1 + max_child_depth
    }
  }
}

// ============================================================================
// Proof Step Generation
// ============================================================================

/// Generate proof steps from premises and conclusion
pub fn generate_steps(
  premises: List(Proposition),
  conclusion: Proposition,
  system: LogicSystem,
) -> List(ProofStep) {
  let initial_state =
    ProofState(
      active_formulas: [],
      worlds: [WorldState("w0", [], [], [])],
      accessibility: [],
      contradictions: [],
      open_branches: 1,
    )

  // Step 1: Introduce premises
  let #(premise_steps, state_after_premises) =
    list.index_fold(premises, #([], initial_state), fn(acc, p, i) {
      let #(steps, current_state) = acc
      let new_state =
        ProofState(
          ..current_state,
          active_formulas: list.append(current_state.active_formulas, [
            #(p, "w0"),
          ]),
        )
      let step =
        ProofStep(
          step_number: i + 1,
          node_id: "p" <> int.to_string(i),
          action: IntroducePremise,
          formula: p,
          world: Some("w0"),
          justification: Premise(i),
          state_before: current_state,
          state_after: new_state,
        )
      #(list.append(steps, [step]), new_state)
    })

  // Step 2: Negate conclusion for refutation
  let negated_conclusion = Not(conclusion)
  let state_with_neg =
    ProofState(
      ..state_after_premises,
      active_formulas: list.append(state_after_premises.active_formulas, [
        #(negated_conclusion, "w0"),
      ]),
    )
  let negation_step =
    ProofStep(
      step_number: list.length(premise_steps) + 1,
      node_id: "neg_conc",
      action: NegateConclusion,
      formula: negated_conclusion,
      world: Some("w0"),
      justification: NegatedConclusion,
      state_before: state_after_premises,
      state_after: state_with_neg,
    )

  // Step 3: Expand modal formulas (simplified - in practice would be more complex)
  let modal_steps =
    expand_modal_formulas(
      state_with_neg,
      system,
      list.length(premise_steps) + 2,
    )

  list.flatten([premise_steps, [negation_step], modal_steps])
}

/// Expand modal formulas according to the logic system
fn expand_modal_formulas(
  state: ProofState,
  system: LogicSystem,
  start_step: Int,
) -> List(ProofStep) {
  // Find modal formulas to expand
  let modal_formulas =
    list.filter(state.active_formulas, fn(pair) {
      let #(formula, _world) = pair
      is_modal_formula(formula)
    })

  list.index_map(modal_formulas, fn(pair, i) {
    let #(formula, world) = pair
    let new_world = "w" <> int.to_string(list.length(state.worlds) + i)

    ProofStep(
      step_number: start_step + i,
      node_id: "modal_" <> int.to_string(i),
      action: ExploreWorld(new_world),
      formula: get_inner_formula(formula),
      world: Some(new_world),
      justification: ModalElim(get_modal_operator(formula), world, new_world),
      state_before: state,
      state_after: ProofState(
        ..state,
        worlds: list.append(state.worlds, [
          WorldState(new_world, [], [], [get_inner_formula(formula)]),
        ]),
        accessibility: list.append(state.accessibility, [#(world, new_world)]),
      ),
    )
  })
}

/// Check if a formula is a modal formula
fn is_modal_formula(formula: Proposition) -> Bool {
  case formula {
    Necessary(_) | Possible(_) -> True
    Knows(_, _) | Believes(_, _) -> True
    _ -> False
  }
}

/// Get the inner formula from a modal formula
fn get_inner_formula(formula: Proposition) -> Proposition {
  case formula {
    Necessary(inner) | Possible(inner) -> inner
    Knows(_, inner) | Believes(_, inner) -> inner
    Not(inner) -> get_inner_formula(inner)
    _ -> formula
  }
}

/// Get the modal operator from a formula
fn get_modal_operator(formula: Proposition) -> ModalOperator {
  case formula {
    Necessary(_) -> BoxOp
    Possible(_) -> DiamondOp
    Knows(agent, _) -> KnowsOp(agent)
    Believes(agent, _) -> BelievesOp(agent)
    _ -> BoxOp
  }
}

// ============================================================================
// Proof Tree Traversal
// ============================================================================

/// Get all nodes in the proof tree (depth-first)
pub fn get_all_nodes(tree: ProofTree) -> List(ProofNode) {
  collect_nodes(tree.root, [])
}

fn collect_nodes(node: ProofNode, acc: List(ProofNode)) -> List(ProofNode) {
  let with_current = list.append(acc, [node])
  list.fold(node.children, with_current, fn(nodes, child) {
    collect_nodes(child, nodes)
  })
}

/// Find a node by ID
pub fn find_node(tree: ProofTree, node_id: String) -> Option(ProofNode) {
  find_node_in(tree.root, node_id)
}

fn find_node_in(node: ProofNode, target_id: String) -> Option(ProofNode) {
  case node.id == target_id {
    True -> Some(node)
    False ->
      list.find_map(node.children, fn(child) {
        find_node_in(child, target_id)
        |> option.to_result(Nil)
      })
      |> option.from_result
  }
}

/// Get the path from root to a specific node
pub fn get_path_to(tree: ProofTree, node_id: String) -> List(ProofNode) {
  find_path(tree.root, node_id, [])
  |> option.unwrap([])
}

fn find_path(
  node: ProofNode,
  target_id: String,
  current_path: List(ProofNode),
) -> Option(List(ProofNode)) {
  let new_path = list.append(current_path, [node])
  case node.id == target_id {
    True -> Some(new_path)
    False ->
      list.find_map(node.children, fn(child) {
        find_path(child, target_id, new_path)
        |> option.to_result(Nil)
      })
      |> option.from_result
  }
}

// ============================================================================
// Proof Tree Export
// ============================================================================

/// Export proof tree to D3.js compatible JSON format
pub fn to_d3_json(tree: ProofTree) -> String {
  let node_json = node_to_d3_json(tree.root)
  "{\"tree\": "
  <> node_json
  <> ", \"metadata\": "
  <> metadata_to_json(tree.metadata)
  <> "}"
}

fn node_to_d3_json(node: ProofNode) -> String {
  let children_json = case node.children {
    [] -> "[]"
    children ->
      "[" <> string.join(list.map(children, node_to_d3_json), ", ") <> "]"
  }

  let eval_str = case node.evaluation {
    Some(True) -> "true"
    Some(False) -> "false"
    None -> "null"
  }

  let world_str = case node.world {
    Some(w) -> "\"" <> w <> "\""
    None -> "null"
  }

  "{"
  <> "\"id\": \""
  <> node.id
  <> "\", "
  <> "\"type\": \""
  <> node_type_to_string(node.node_type)
  <> "\", "
  <> "\"formula\": \""
  <> proposition_to_string(node.formula)
  <> "\", "
  <> "\"world\": "
  <> world_str
  <> ", "
  <> "\"evaluation\": "
  <> eval_str
  <> ", "
  <> "\"justification\": \""
  <> justification_to_string(node.justification)
  <> "\", "
  <> "\"children\": "
  <> children_json
  <> "}"
}

fn metadata_to_json(metadata: ProofMetadata) -> String {
  "{"
  <> "\"system\": \""
  <> system_to_string(metadata.system)
  <> "\", "
  <> "\"isValid\": "
  <> bool_to_string(metadata.is_valid)
  <> ", "
  <> "\"totalSteps\": "
  <> int.to_string(metadata.total_steps)
  <> ", "
  <> "\"maxDepth\": "
  <> int.to_string(metadata.max_depth)
  <> ", "
  <> "\"worldsExplored\": "
  <> int.to_string(metadata.worlds_explored)
  <> ", "
  <> "\"proofMethod\": \""
  <> proof_method_to_string(metadata.proof_method)
  <> "\", "
  <> "\"durationMs\": "
  <> int.to_string(metadata.duration_ms)
  <> "}"
}

/// Export proof steps to JSON for debugger
pub fn steps_to_json(steps: List(ProofStep)) -> String {
  let steps_json =
    list.map(steps, step_to_json)
    |> string.join(", ")
  "[" <> steps_json <> "]"
}

fn step_to_json(step: ProofStep) -> String {
  let world_str = case step.world {
    Some(w) -> "\"" <> w <> "\""
    None -> "null"
  }

  "{"
  <> "\"stepNumber\": "
  <> int.to_string(step.step_number)
  <> ", "
  <> "\"nodeId\": \""
  <> step.node_id
  <> "\", "
  <> "\"action\": \""
  <> action_to_string(step.action)
  <> "\", "
  <> "\"formula\": \""
  <> proposition_to_string(step.formula)
  <> "\", "
  <> "\"world\": "
  <> world_str
  <> ", "
  <> "\"justification\": \""
  <> justification_to_string(step.justification)
  <> "\""
  <> "}"
}

// ============================================================================
// String Conversions
// ============================================================================

fn node_type_to_string(node_type: ProofNodeType) -> String {
  case node_type {
    PremiseNode -> "premise"
    InferenceNode -> "inference"
    ConclusionNode -> "conclusion"
    ModalNode -> "modal"
    WorldTransitionNode -> "world_transition"
    ContradictionNode -> "contradiction"
    CounterexampleNode -> "counterexample"
  }
}

fn justification_to_string(justification: Justification) -> String {
  case justification {
    Premise(i) -> "Premise " <> int.to_string(i + 1)
    Inference(rule, from) ->
      rule_to_string(rule) <> " from " <> string.join(from, ", ")
    ModalElim(op, from, to) ->
      modal_op_to_string(op) <> " elimination: " <> from <> " → " <> to
    ModalIntro(op, universal) ->
      modal_op_to_string(op)
      <> " introduction"
      <> case universal {
        True -> " (universal)"
        False -> ""
      }
    Assumption -> "Assumption"
    NegatedConclusion -> "Negation of conclusion"
    FrameProperty(prop) -> "Frame property: " <> frame_property_to_string(prop)
    Witness(world, _) -> "Countermodel witness at " <> world
  }
}

fn rule_to_string(rule: InferenceRule) -> String {
  case rule {
    ModusPonens -> "Modus Ponens"
    ModusTollens -> "Modus Tollens"
    AndIntro -> "∧-Intro"
    AndElim -> "∧-Elim"
    OrIntro -> "∨-Intro"
    OrElim -> "∨-Elim"
    DoubleNegElim -> "¬¬-Elim"
    Necessitation -> "Necessitation"
    DistributionK -> "K Axiom"
    ReflexivityT -> "T Axiom"
    Transitivity4 -> "4 Axiom"
    Euclidean5 -> "5 Axiom"
    SymmetryB -> "B Axiom"
    SerialityD -> "D Axiom"
  }
}

fn modal_op_to_string(op: ModalOperator) -> String {
  case op {
    BoxOp -> "□"
    DiamondOp -> "◇"
    KnowsOp(agent) -> "K_" <> agent
    BelievesOp(agent) -> "B_" <> agent
    ObligatoryOp -> "O"
    PermittedOp -> "P"
  }
}

fn frame_property_to_string(prop: FramePropertyType) -> String {
  case prop {
    Reflexivity -> "Reflexivity"
    Symmetry -> "Symmetry"
    Transitivity -> "Transitivity"
    Seriality -> "Seriality"
    Euclidean -> "Euclidean"
  }
}

fn action_to_string(action: StepAction) -> String {
  case action {
    IntroducePremise -> "Introduce premise"
    ApplyRule(rule) -> "Apply " <> rule_to_string(rule)
    NegateConclusion -> "Negate conclusion"
    ExploreWorld(w) -> "Explore world " <> w
    EvaluateAtWorld -> "Evaluate at world"
    CheckAccessibility(from, to) -> "Check R(" <> from <> ", " <> to <> ")"
    FindContradiction -> "Find contradiction"
    ConstructCounterexample -> "Construct counterexample"
    CompleteProof -> "Complete proof"
  }
}

fn proof_method_to_string(method: ProofMethod) -> String {
  case method {
    DirectProof -> "direct"
    RefutationProof -> "refutation"
    CountermodelConstruction -> "countermodel"
    TableauMethod -> "tableau"
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
    Knows(agent, inner) -> "K_" <> agent <> proposition_to_string(inner)
    Believes(agent, inner) -> "B_" <> agent <> proposition_to_string(inner)
    _ -> "?"
  }
}

fn system_to_string(system: LogicSystem) -> String {
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

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
