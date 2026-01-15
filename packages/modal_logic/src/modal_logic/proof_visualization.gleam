//// Proof Visualization Module
////
//// This module provides D3.js-based interactive visualizations for
//// proof trees and Kripke frames. It generates JavaScript code that
//// can be embedded in web pages for interactive exploration.
////
//// ## Features
//// - D3.js proof tree visualization
//// - Interactive Kripke frame diagrams
//// - Animated step-by-step playback
//// - Export to SVG/PNG
//// - Responsive layouts

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/debugger.{type DebugSession}
import modal_logic/proof_tree.{type ProofNode, type ProofTree}
import modal_logic/proposition.{type LogicSystem}
import modal_logic/visualization.{type KripkeModel, type Relation, type World}

// ============================================================================
// Visualization Types
// ============================================================================

/// Configuration for proof tree visualization
pub type ProofTreeConfig {
  ProofTreeConfig(
    width: Int,
    height: Int,
    node_radius: Int,
    level_height: Int,
    animation_duration: Int,
    show_formulas: Bool,
    show_justifications: Bool,
    highlight_path: Bool,
    color_scheme: ColorScheme,
    layout: TreeLayout,
  )
}

/// Configuration for Kripke frame visualization
pub type KripkeConfig {
  KripkeConfig(
    width: Int,
    height: Int,
    node_radius: Int,
    force_strength: Float,
    link_distance: Int,
    animation_duration: Int,
    show_valuations: Bool,
    show_labels: Bool,
    highlight_actual: Bool,
    color_scheme: ColorScheme,
    layout: GraphLayout,
  )
}

/// Color scheme for visualizations
pub type ColorScheme {
  ColorScheme(
    premise: String,
    inference: String,
    conclusion: String,
    modal: String,
    contradiction: String,
    counterexample: String,
    valid: String,
    invalid: String,
    highlight: String,
    background: String,
    text: String,
    link: String,
  )
}

/// Tree layout algorithms
pub type TreeLayout {
  /// Standard top-down tree
  TopDownTree
  /// Radial tree layout
  RadialTree
  /// Cluster dendrogram
  ClusterLayout
  /// Tidy tree (Reingold-Tilford)
  TidyTree
}

/// Graph layout algorithms
pub type GraphLayout {
  /// Force-directed layout
  ForceDirected
  /// Circular layout
  Circular
  /// Grid layout
  Grid
  /// Hierarchical layout
  Hierarchical
}

/// Animation state for playback
pub type AnimationState {
  AnimationState(
    current_step: Int,
    total_steps: Int,
    is_playing: Bool,
    speed: Float,
    highlighted_nodes: List(String),
    visible_nodes: List(String),
  )
}

// ============================================================================
// Default Configurations
// ============================================================================

/// Default proof tree configuration
pub fn default_proof_config() -> ProofTreeConfig {
  ProofTreeConfig(
    width: 800,
    height: 600,
    node_radius: 25,
    level_height: 100,
    animation_duration: 500,
    show_formulas: True,
    show_justifications: True,
    highlight_path: True,
    color_scheme: default_color_scheme(),
    layout: TidyTree,
  )
}

/// Default Kripke configuration
pub fn default_kripke_config() -> KripkeConfig {
  KripkeConfig(
    width: 600,
    height: 400,
    node_radius: 30,
    force_strength: -300.0,
    link_distance: 150,
    animation_duration: 500,
    show_valuations: True,
    show_labels: True,
    highlight_actual: True,
    color_scheme: default_color_scheme(),
    layout: ForceDirected,
  )
}

/// Default color scheme
pub fn default_color_scheme() -> ColorScheme {
  ColorScheme(
    premise: "#4CAF50",
    inference: "#2196F3",
    conclusion: "#9C27B0",
    modal: "#FF9800",
    contradiction: "#F44336",
    counterexample: "#E91E63",
    valid: "#4CAF50",
    invalid: "#F44336",
    highlight: "#FFC107",
    background: "#FAFAFA",
    text: "#212121",
    link: "#757575",
  )
}

/// Dark color scheme
pub fn dark_color_scheme() -> ColorScheme {
  ColorScheme(
    premise: "#81C784",
    inference: "#64B5F6",
    conclusion: "#BA68C8",
    modal: "#FFB74D",
    contradiction: "#E57373",
    counterexample: "#F48FB1",
    valid: "#81C784",
    invalid: "#E57373",
    highlight: "#FFD54F",
    background: "#303030",
    text: "#FFFFFF",
    link: "#BDBDBD",
  )
}

// ============================================================================
// Proof Tree Visualization
// ============================================================================

/// Generate D3.js code for proof tree visualization
pub fn proof_tree_to_d3(tree: ProofTree, config: ProofTreeConfig) -> String {
  let tree_json = proof_tree.to_d3_json(tree)
  let layout_fn = get_tree_layout_function(config.layout)

  "
(function() {
  const config = " <> config_to_json(config) <> ";
  const data = " <> tree_json <> ";

  // Create SVG container
  const svg = d3.select('#proof-tree-container')
    .append('svg')
    .attr('width', config.width)
    .attr('height', config.height)
    .attr('viewBox', [0, 0, config.width, config.height])
    .style('background', config.colorScheme.background);

  // Create main group with margins
  const margin = {top: 40, right: 40, bottom: 40, left: 40};
  const g = svg.append('g')
    .attr('transform', `translate(${margin.left},${margin.top})`);

  // Create hierarchy from data
  const root = d3.hierarchy(data.tree);

  // Apply layout
  " <> layout_fn <> "

  // Create links
  const links = g.selectAll('.link')
    .data(root.links())
    .join('path')
    .attr('class', 'link')
    .attr('fill', 'none')
    .attr('stroke', config.colorScheme.link)
    .attr('stroke-width', 2)
    .attr('d', d3.linkVertical()
      .x(d => d.x)
      .y(d => d.y));

  // Create nodes
  const nodes = g.selectAll('.node')
    .data(root.descendants())
    .join('g')
    .attr('class', 'node')
    .attr('transform', d => `translate(${d.x},${d.y})`)
    .style('cursor', 'pointer')
    .on('click', handleNodeClick)
    .on('mouseover', handleNodeHover)
    .on('mouseout', handleNodeUnhover);

  // Node circles
  nodes.append('circle')
    .attr('r', config.nodeRadius)
    .attr('fill', d => getNodeColor(d.data, config))
    .attr('stroke', d => d.data.evaluation === true ? config.colorScheme.valid :
                         d.data.evaluation === false ? config.colorScheme.invalid :
                         config.colorScheme.text)
    .attr('stroke-width', d => d.data.evaluation !== null ? 3 : 1);

  // Node labels (formula)
  if (config.showFormulas) {
    nodes.append('text')
      .attr('dy', 5)
      .attr('text-anchor', 'middle')
      .attr('fill', config.colorScheme.text)
      .attr('font-size', '12px')
      .attr('font-family', 'monospace')
      .text(d => truncateFormula(d.data.formula, 15));
  }

  // Node type indicators
  nodes.append('text')
    .attr('dy', config.nodeRadius + 15)
    .attr('text-anchor', 'middle')
    .attr('fill', config.colorScheme.text)
    .attr('font-size', '10px')
    .text(d => d.data.type);

  // Justification labels on hover
  if (config.showJustifications) {
    const tooltip = d3.select('#proof-tree-container')
      .append('div')
      .attr('class', 'tooltip')
      .style('position', 'absolute')
      .style('visibility', 'hidden')
      .style('background', 'white')
      .style('border', '1px solid #ccc')
      .style('padding', '8px')
      .style('border-radius', '4px')
      .style('font-size', '12px');
  }

  // Animation state
  let animationState = {
    currentStep: 0,
    isPlaying: false,
    highlightedPath: []
  };

  // Event handlers
  function handleNodeClick(event, d) {
    if (config.highlightPath) {
      highlightPathToNode(d);
    }
    dispatchEvent(new CustomEvent('nodeClick', { detail: d.data }));
  }

  function handleNodeHover(event, d) {
    d3.select(this).select('circle')
      .transition()
      .duration(200)
      .attr('r', config.nodeRadius * 1.2);

    if (config.showJustifications) {
      d3.select('.tooltip')
        .style('visibility', 'visible')
        .html(`<strong>${d.data.type}</strong><br/>${d.data.justification}`)
        .style('left', (event.pageX + 10) + 'px')
        .style('top', (event.pageY - 10) + 'px');
    }
  }

  function handleNodeUnhover(event, d) {
    d3.select(this).select('circle')
      .transition()
      .duration(200)
      .attr('r', config.nodeRadius);

    d3.select('.tooltip').style('visibility', 'hidden');
  }

  function highlightPathToNode(node) {
    // Reset all nodes
    nodes.selectAll('circle')
      .attr('stroke-width', d => d.data.evaluation !== null ? 3 : 1);

    // Highlight path
    let current = node;
    while (current) {
      d3.select(nodes.nodes()[current.index]).select('circle')
        .attr('stroke', config.colorScheme.highlight)
        .attr('stroke-width', 4);
      current = current.parent;
    }
  }

  function getNodeColor(data, config) {
    switch (data.type) {
      case 'premise': return config.colorScheme.premise;
      case 'inference': return config.colorScheme.inference;
      case 'conclusion': return config.colorScheme.conclusion;
      case 'modal': return config.colorScheme.modal;
      case 'contradiction': return config.colorScheme.contradiction;
      case 'counterexample': return config.colorScheme.counterexample;
      default: return config.colorScheme.inference;
    }
  }

  function truncateFormula(formula, maxLength) {
    if (formula.length <= maxLength) return formula;
    return formula.substring(0, maxLength - 3) + '...';
  }

  // Playback controls
  window.proofTreeControls = {
    play: function() {
      animationState.isPlaying = true;
      stepAnimation();
    },
    pause: function() {
      animationState.isPlaying = false;
    },
    reset: function() {
      animationState.currentStep = 0;
      animationState.isPlaying = false;
      updateVisualization();
    },
    stepForward: function() {
      if (animationState.currentStep < data.metadata.totalSteps) {
        animationState.currentStep++;
        updateVisualization();
      }
    },
    stepBackward: function() {
      if (animationState.currentStep > 0) {
        animationState.currentStep--;
        updateVisualization();
      }
    },
    goToStep: function(step) {
      animationState.currentStep = Math.max(0, Math.min(step, data.metadata.totalSteps));
      updateVisualization();
    }
  };

  function stepAnimation() {
    if (!animationState.isPlaying) return;
    if (animationState.currentStep < data.metadata.totalSteps) {
      animationState.currentStep++;
      updateVisualization();
      setTimeout(stepAnimation, config.animationDuration);
    } else {
      animationState.isPlaying = false;
    }
  }

  function updateVisualization() {
    // Emit step change event
    dispatchEvent(new CustomEvent('stepChange', {
      detail: { step: animationState.currentStep, total: data.metadata.totalSteps }
    }));
  }
})();
"
}

fn get_tree_layout_function(layout: TreeLayout) -> String {
  case layout {
    TopDownTree ->
      "
  const treeLayout = d3.tree()
    .size([config.width - margin.left - margin.right, config.height - margin.top - margin.bottom]);
  treeLayout(root);
"
    RadialTree ->
      "
  const treeLayout = d3.tree()
    .size([2 * Math.PI, Math.min(config.width, config.height) / 2 - 100])
    .separation((a, b) => (a.parent == b.parent ? 1 : 2) / a.depth);
  treeLayout(root);
  root.each(d => {
    const angle = d.x;
    const radius = d.y;
    d.x = radius * Math.cos(angle - Math.PI / 2) + config.width / 2;
    d.y = radius * Math.sin(angle - Math.PI / 2) + config.height / 2;
  });
"
    ClusterLayout ->
      "
  const clusterLayout = d3.cluster()
    .size([config.width - margin.left - margin.right, config.height - margin.top - margin.bottom]);
  clusterLayout(root);
"
    TidyTree ->
      "
  const treeLayout = d3.tree()
    .nodeSize([config.nodeRadius * 3, config.levelHeight]);
  treeLayout(root);
  // Center the tree
  let minX = Infinity, maxX = -Infinity;
  root.each(d => {
    if (d.x < minX) minX = d.x;
    if (d.x > maxX) maxX = d.x;
  });
  const offsetX = (config.width - margin.left - margin.right) / 2 - (minX + maxX) / 2;
  root.each(d => d.x += offsetX);
"
  }
}

fn config_to_json(config: ProofTreeConfig) -> String {
  "{"
  <> "\"width\": "
  <> int.to_string(config.width)
  <> ", "
  <> "\"height\": "
  <> int.to_string(config.height)
  <> ", "
  <> "\"nodeRadius\": "
  <> int.to_string(config.node_radius)
  <> ", "
  <> "\"levelHeight\": "
  <> int.to_string(config.level_height)
  <> ", "
  <> "\"animationDuration\": "
  <> int.to_string(config.animation_duration)
  <> ", "
  <> "\"showFormulas\": "
  <> bool_to_string(config.show_formulas)
  <> ", "
  <> "\"showJustifications\": "
  <> bool_to_string(config.show_justifications)
  <> ", "
  <> "\"highlightPath\": "
  <> bool_to_string(config.highlight_path)
  <> ", "
  <> "\"colorScheme\": "
  <> color_scheme_to_json(config.color_scheme)
  <> "}"
}

fn color_scheme_to_json(scheme: ColorScheme) -> String {
  "{"
  <> "\"premise\": \""
  <> scheme.premise
  <> "\", "
  <> "\"inference\": \""
  <> scheme.inference
  <> "\", "
  <> "\"conclusion\": \""
  <> scheme.conclusion
  <> "\", "
  <> "\"modal\": \""
  <> scheme.modal
  <> "\", "
  <> "\"contradiction\": \""
  <> scheme.contradiction
  <> "\", "
  <> "\"counterexample\": \""
  <> scheme.counterexample
  <> "\", "
  <> "\"valid\": \""
  <> scheme.valid
  <> "\", "
  <> "\"invalid\": \""
  <> scheme.invalid
  <> "\", "
  <> "\"highlight\": \""
  <> scheme.highlight
  <> "\", "
  <> "\"background\": \""
  <> scheme.background
  <> "\", "
  <> "\"text\": \""
  <> scheme.text
  <> "\", "
  <> "\"link\": \""
  <> scheme.link
  <> "\""
  <> "}"
}

// ============================================================================
// Kripke Frame Visualization
// ============================================================================

/// Generate D3.js code for interactive Kripke frame visualization
pub fn kripke_to_d3(model: KripkeModel, config: KripkeConfig) -> String {
  let nodes_json = worlds_to_json(model.worlds, model.actual_world)
  let links_json = relations_to_json(model.relations)

  "
(function() {
  const config = " <> kripke_config_to_json(config) <> ";
  const nodes = " <> nodes_json <> ";
  const links = " <> links_json <> ";
  const actualWorld = '" <> model.actual_world <> "';

  // Create SVG container
  const svg = d3.select('#kripke-container')
    .append('svg')
    .attr('width', config.width)
    .attr('height', config.height)
    .style('background', config.colorScheme.background);

  // Arrow marker for directed edges
  svg.append('defs').append('marker')
    .attr('id', 'arrowhead')
    .attr('viewBox', '-0 -5 10 10')
    .attr('refX', config.nodeRadius + 10)
    .attr('refY', 0)
    .attr('orient', 'auto')
    .attr('markerWidth', 8)
    .attr('markerHeight', 8)
    .append('path')
    .attr('d', 'M 0,-5 L 10 ,0 L 0,5')
    .attr('fill', config.colorScheme.link);

  // Create force simulation
  const simulation = d3.forceSimulation(nodes)
    .force('link', d3.forceLink(links)
      .id(d => d.id)
      .distance(config.linkDistance))
    .force('charge', d3.forceManyBody().strength(config.forceStrength))
    .force('center', d3.forceCenter(config.width / 2, config.height / 2))
    .force('collision', d3.forceCollide().radius(config.nodeRadius + 10));

  // Create links
  const link = svg.append('g')
    .selectAll('line')
    .data(links)
    .join('line')
    .attr('stroke', config.colorScheme.link)
    .attr('stroke-width', 2)
    .attr('marker-end', 'url(#arrowhead)');

  // Create self-loop paths for reflexive relations
  const selfLoops = svg.append('g')
    .selectAll('path')
    .data(links.filter(l => l.source === l.target || l.source.id === l.target.id))
    .join('path')
    .attr('fill', 'none')
    .attr('stroke', config.colorScheme.link)
    .attr('stroke-width', 2)
    .attr('marker-end', 'url(#arrowhead)');

  // Create node groups
  const node = svg.append('g')
    .selectAll('g')
    .data(nodes)
    .join('g')
    .call(d3.drag()
      .on('start', dragstarted)
      .on('drag', dragged)
      .on('end', dragended))
    .on('click', handleNodeClick)
    .on('mouseover', handleNodeHover)
    .on('mouseout', handleNodeUnhover);

  // Node circles
  node.append('circle')
    .attr('r', config.nodeRadius)
    .attr('fill', d => d.isActual ? config.colorScheme.valid : config.colorScheme.inference)
    .attr('stroke', d => d.isActual ? config.colorScheme.highlight : config.colorScheme.text)
    .attr('stroke-width', d => d.isActual ? 4 : 2);

  // Node labels
  if (config.showLabels) {
    node.append('text')
      .attr('dy', 5)
      .attr('text-anchor', 'middle')
      .attr('fill', config.colorScheme.text)
      .attr('font-weight', 'bold')
      .attr('font-size', '14px')
      .text(d => d.id);
  }

  // Valuation labels
  if (config.showValuations) {
    node.append('text')
      .attr('dy', config.nodeRadius + 15)
      .attr('text-anchor', 'middle')
      .attr('fill', config.colorScheme.text)
      .attr('font-size', '10px')
      .text(d => d.trueAtoms.length > 0 ? '{' + d.trueAtoms.join(', ') + '}' : '∅');
  }

  // Tooltip
  const tooltip = d3.select('#kripke-container')
    .append('div')
    .attr('class', 'kripke-tooltip')
    .style('position', 'absolute')
    .style('visibility', 'hidden')
    .style('background', 'white')
    .style('border', '1px solid #ccc')
    .style('padding', '10px')
    .style('border-radius', '4px')
    .style('box-shadow', '0 2px 4px rgba(0,0,0,0.1)');

  // Update positions on simulation tick
  simulation.on('tick', () => {
    link
      .attr('x1', d => d.source.x)
      .attr('y1', d => d.source.y)
      .attr('x2', d => d.target.x)
      .attr('y2', d => d.target.y);

    selfLoops.attr('d', d => {
      const x = typeof d.source === 'object' ? d.source.x : nodes.find(n => n.id === d.source).x;
      const y = typeof d.source === 'object' ? d.source.y : nodes.find(n => n.id === d.source).y;
      return `M ${x} ${y - config.nodeRadius}
              C ${x - 40} ${y - 60}, ${x + 40} ${y - 60}, ${x} ${y - config.nodeRadius}`;
    });

    node.attr('transform', d => `translate(${d.x},${d.y})`);
  });

  // Drag handlers
  function dragstarted(event) {
    if (!event.active) simulation.alphaTarget(0.3).restart();
    event.subject.fx = event.subject.x;
    event.subject.fy = event.subject.y;
  }

  function dragged(event) {
    event.subject.fx = event.x;
    event.subject.fy = event.y;
  }

  function dragended(event) {
    if (!event.active) simulation.alphaTarget(0);
    event.subject.fx = null;
    event.subject.fy = null;
  }

  // Event handlers
  function handleNodeClick(event, d) {
    dispatchEvent(new CustomEvent('worldClick', { detail: d }));
  }

  function handleNodeHover(event, d) {
    d3.select(this).select('circle')
      .transition()
      .duration(200)
      .attr('r', config.nodeRadius * 1.2);

    tooltip
      .style('visibility', 'visible')
      .html(`
        <strong>World: ${d.id}</strong><br/>
        <span style='color: green'>True: {${d.trueAtoms.join(', ') || '∅'}}</span><br/>
        <span style='color: red'>False: {${d.falseAtoms.join(', ') || '∅'}}</span>
        ${d.isActual ? '<br/><em>Actual World</em>' : ''}
      `)
      .style('left', (event.pageX + 15) + 'px')
      .style('top', (event.pageY - 15) + 'px');
  }

  function handleNodeUnhover(event, d) {
    d3.select(this).select('circle')
      .transition()
      .duration(200)
      .attr('r', config.nodeRadius);

    tooltip.style('visibility', 'hidden');
  }

  // Expose controls
  window.kripkeControls = {
    highlightWorld: function(worldId) {
      node.selectAll('circle')
        .attr('stroke', d => d.id === worldId ? config.colorScheme.highlight :
                            (d.isActual ? config.colorScheme.highlight : config.colorScheme.text))
        .attr('stroke-width', d => d.id === worldId ? 5 : (d.isActual ? 4 : 2));
    },
    highlightPath: function(worldIds) {
      node.selectAll('circle')
        .attr('stroke', d => worldIds.includes(d.id) ? config.colorScheme.highlight : config.colorScheme.text)
        .attr('stroke-width', d => worldIds.includes(d.id) ? 4 : 2);
      link
        .attr('stroke', d => {
          const sourceId = typeof d.source === 'object' ? d.source.id : d.source;
          const targetId = typeof d.target === 'object' ? d.target.id : d.target;
          return worldIds.includes(sourceId) && worldIds.includes(targetId) ?
                 config.colorScheme.highlight : config.colorScheme.link;
        })
        .attr('stroke-width', d => {
          const sourceId = typeof d.source === 'object' ? d.source.id : d.source;
          const targetId = typeof d.target === 'object' ? d.target.id : d.target;
          return worldIds.includes(sourceId) && worldIds.includes(targetId) ? 4 : 2;
        });
    },
    resetHighlights: function() {
      node.selectAll('circle')
        .attr('stroke', d => d.isActual ? config.colorScheme.highlight : config.colorScheme.text)
        .attr('stroke-width', d => d.isActual ? 4 : 2);
      link
        .attr('stroke', config.colorScheme.link)
        .attr('stroke-width', 2);
    },
    zoomIn: function() {
      svg.transition().call(zoom.scaleBy, 1.3);
    },
    zoomOut: function() {
      svg.transition().call(zoom.scaleBy, 0.7);
    },
    resetView: function() {
      svg.transition().call(zoom.transform, d3.zoomIdentity);
    }
  };

  // Zoom behavior
  const zoom = d3.zoom()
    .scaleExtent([0.5, 3])
    .on('zoom', (event) => {
      svg.selectAll('g').attr('transform', event.transform);
    });

  svg.call(zoom);
})();
"
}

fn worlds_to_json(worlds: List(World), actual_world: String) -> String {
  let items =
    list.map(worlds, fn(w) {
      "{"
      <> "\"id\": \""
      <> w.name
      <> "\", "
      <> "\"trueAtoms\": ["
      <> string.join(list.map(w.true_atoms, fn(a) { "\"" <> a <> "\"" }), ", ")
      <> "], "
      <> "\"falseAtoms\": ["
      <> string.join(list.map(w.false_atoms, fn(a) { "\"" <> a <> "\"" }), ", ")
      <> "], "
      <> "\"isActual\": "
      <> bool_to_string(w.name == actual_world)
      <> "}"
    })
    |> string.join(", ")
  "[" <> items <> "]"
}

fn relations_to_json(relations: List(Relation)) -> String {
  let items =
    list.map(relations, fn(r) {
      "{\"source\": \"" <> r.from <> "\", \"target\": \"" <> r.to <> "\"}"
    })
    |> string.join(", ")
  "[" <> items <> "]"
}

fn kripke_config_to_json(config: KripkeConfig) -> String {
  "{"
  <> "\"width\": "
  <> int.to_string(config.width)
  <> ", "
  <> "\"height\": "
  <> int.to_string(config.height)
  <> ", "
  <> "\"nodeRadius\": "
  <> int.to_string(config.node_radius)
  <> ", "
  <> "\"forceStrength\": "
  <> float_to_string(config.force_strength)
  <> ", "
  <> "\"linkDistance\": "
  <> int.to_string(config.link_distance)
  <> ", "
  <> "\"animationDuration\": "
  <> int.to_string(config.animation_duration)
  <> ", "
  <> "\"showValuations\": "
  <> bool_to_string(config.show_valuations)
  <> ", "
  <> "\"showLabels\": "
  <> bool_to_string(config.show_labels)
  <> ", "
  <> "\"highlightActual\": "
  <> bool_to_string(config.highlight_actual)
  <> ", "
  <> "\"colorScheme\": "
  <> color_scheme_to_json(config.color_scheme)
  <> "}"
}

// ============================================================================
// HTML Page Generation
// ============================================================================

/// Generate a complete HTML page with visualization
pub fn to_html_page(
  tree: ProofTree,
  model: Option(KripkeModel),
  config: ProofTreeConfig,
) -> String {
  let proof_js = proof_tree_to_d3(tree, config)

  let kripke_js = case model {
    Some(m) -> kripke_to_d3(m, default_kripke_config())
    None -> ""
  }

  let kripke_container = case model {
    Some(_) ->
      "
    <div class='section'>
      <h2>Kripke Frame</h2>
      <div id='kripke-container'></div>
    </div>
"
    None -> ""
  }

  "<!DOCTYPE html>
<html lang='en'>
<head>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>
  <title>Proof Visualization - " <> system_to_string(tree.metadata.system) <> "</title>
  <script src='https://d3js.org/d3.v7.min.js'></script>
  <style>
    * { box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      margin: 0;
      padding: 20px;
      background: #f5f5f5;
    }
    .container {
      max-width: 1200px;
      margin: 0 auto;
    }
    h1 {
      color: #333;
      margin-bottom: 10px;
    }
    .metadata {
      color: #666;
      margin-bottom: 20px;
    }
    .section {
      background: white;
      border-radius: 8px;
      padding: 20px;
      margin-bottom: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    h2 {
      margin-top: 0;
      color: #444;
    }
    .controls {
      display: flex;
      gap: 10px;
      margin-bottom: 15px;
      flex-wrap: wrap;
    }
    button {
      padding: 8px 16px;
      border: none;
      border-radius: 4px;
      background: #2196F3;
      color: white;
      cursor: pointer;
      font-size: 14px;
    }
    button:hover {
      background: #1976D2;
    }
    button:disabled {
      background: #ccc;
      cursor: not-allowed;
    }
    .step-indicator {
      padding: 8px 16px;
      background: #f0f0f0;
      border-radius: 4px;
      font-size: 14px;
    }
    #proof-tree-container, #kripke-container {
      border: 1px solid #e0e0e0;
      border-radius: 4px;
      overflow: hidden;
    }
    .valid { color: #4CAF50; }
    .invalid { color: #F44336; }
    .tooltip, .kripke-tooltip {
      z-index: 1000;
      max-width: 300px;
    }
  </style>
</head>
<body>
  <div class='container'>
    <h1>Proof Visualization</h1>
    <div class='metadata'>
      <span class='" <> case tree.metadata.is_valid {
    True -> "valid"
    False -> "invalid"
  } <> "'>
        " <> case tree.metadata.is_valid {
    True -> "✓ Valid"
    False -> "✗ Invalid"
  } <> "
      </span>
      | System: <strong>" <> system_to_string(tree.metadata.system) <> "</strong>
      | Steps: " <> int.to_string(tree.metadata.total_steps) <> "
      | Worlds: " <> int.to_string(tree.metadata.worlds_explored) <> "
    </div>

    <div class='section'>
      <h2>Proof Tree</h2>
      <div class='controls'>
        <button onclick='proofTreeControls.stepBackward()'>⏮ Back</button>
        <button onclick='proofTreeControls.play()'>▶ Play</button>
        <button onclick='proofTreeControls.pause()'>⏸ Pause</button>
        <button onclick='proofTreeControls.stepForward()'>Forward ⏭</button>
        <button onclick='proofTreeControls.reset()'>↺ Reset</button>
        <span class='step-indicator' id='step-indicator'>Step: 0 / " <> int.to_string(
    tree.metadata.total_steps,
  ) <> "</span>
      </div>
      <div id='proof-tree-container'></div>
    </div>
" <> kripke_container <> "
  </div>

  <script>
    " <> proof_js <> "

    " <> kripke_js <> "

    // Update step indicator
    window.addEventListener('stepChange', (e) => {
      document.getElementById('step-indicator').textContent =
        `Step: ${e.detail.step} / ${e.detail.total}`;
    });
  </script>
</body>
</html>"
}

// ============================================================================
// Export Functions
// ============================================================================

/// Export visualization to SVG string
pub fn to_svg(tree: ProofTree, config: ProofTreeConfig) -> String {
  // Generate a static SVG representation
  let nodes = proof_tree.get_all_nodes(tree)
  let node_elements =
    list.index_map(nodes, fn(node, i) {
      let x = { i % 4 } * 150 + 100
      let y = { i / 4 } * 100 + 100
      let color = node_type_color(node.node_type, config.color_scheme)
      "<g transform='translate("
      <> int.to_string(x)
      <> ","
      <> int.to_string(y)
      <> ")'>"
      <> "<circle r='"
      <> int.to_string(config.node_radius)
      <> "' fill='"
      <> color
      <> "' stroke='#333' stroke-width='2'/>"
      <> "<text dy='5' text-anchor='middle' font-size='10'>"
      <> node.id
      <> "</text>"
      <> "</g>"
    })
    |> string.join("\n")

  "<svg xmlns='http://www.w3.org/2000/svg' width='"
  <> int.to_string(config.width)
  <> "' height='"
  <> int.to_string(config.height)
  <> "'>\n"
  <> "<rect width='100%' height='100%' fill='"
  <> config.color_scheme.background
  <> "'/>\n"
  <> node_elements
  <> "\n"
  <> "</svg>"
}

fn node_type_color(
  node_type: proof_tree.ProofNodeType,
  scheme: ColorScheme,
) -> String {
  case node_type {
    proof_tree.PremiseNode -> scheme.premise
    proof_tree.InferenceNode -> scheme.inference
    proof_tree.ConclusionNode -> scheme.conclusion
    proof_tree.ModalNode -> scheme.modal
    proof_tree.ContradictionNode -> scheme.contradiction
    proof_tree.CounterexampleNode -> scheme.counterexample
    proof_tree.WorldTransitionNode -> scheme.modal
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn float_to_string(f: Float) -> String {
  let whole = float_truncate(f)
  let frac = float_truncate({ f -. int_to_float(whole) } *. 100.0)
  int.to_string(whole) <> "." <> int.to_string(abs(frac))
}

fn abs(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
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

@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float
