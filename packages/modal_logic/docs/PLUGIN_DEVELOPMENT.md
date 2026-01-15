# Plugin Development Guide

This guide explains how to create plugins for the Modal Logic Engine to extend it with custom logic systems.

## Overview

The plugin system allows researchers and developers to add custom logic systems without modifying the core codebase. Plugins can define:

- **Custom Operators**: New modal operators (temporal, deontic, epistemic, etc.)
- **Frame Conditions**: Axioms and constraints on accessibility relations
- **Inference Rules**: Proof rules for the logic
- **Validators**: Custom validation logic
- **Transformers**: Convert formulas between different logics

## Quick Start

### 1. Create Plugin Metadata

```gleam
import modal_logic/plugin.{Plugin, PluginMetadata, PluginDependency}
import gleam/option.{None, Some}

fn my_plugin_metadata() -> PluginMetadata {
  PluginMetadata(
    id: "my_custom_logic",
    name: "My Custom Logic",
    version: "1.0.0",
    author: "Your Name",
    description: "Description of what this logic does",
    license: "MIT",
    repository: Some("https://github.com/you/my-plugin"),
    dependencies: [],
    min_engine_version: "0.1.0",
  )
}
```

### 2. Define Logic System

```gleam
import modal_logic/plugin.{
  LogicDefinition, CustomOperator, FrameCondition,
  InferenceRule, AccessibilityProperty, UnaryModal, Sound
}
import modal_logic/proposition.{K}

fn my_logic_definition() -> LogicDefinition {
  LogicDefinition(
    id: "MY_LOGIC",
    name: "My Custom Logic",
    description: "A custom modal logic for specific domain",
    operators: my_operators(),
    frame_conditions: my_frame_conditions(),
    accessibility_properties: [plugin.Reflexive, plugin.Transitive],
    inference_rules: my_inference_rules(),
    base_system: Some(K),
  )
}

fn my_operators() -> List(CustomOperator) {
  [
    CustomOperator(
      symbol: "O",
      name: "Obligation",
      description: "It is obligatory that",
      arity: 1,
      unicode: "○",
      latex: "\\bigcirc",
      semantics: UnaryModal(universal: True, relation: "deontic"),
    ),
  ]
}
```

### 3. Create the Plugin

```gleam
pub fn my_plugin() -> Plugin {
  Plugin(
    metadata: my_plugin_metadata(),
    logic: my_logic_definition(),
    validators: [],
    transformers: [],
    hooks: plugin.default_hooks(),
  )
}
```

### 4. Register the Plugin

```gleam
import modal_logic/plugin

pub fn main() {
  let config = plugin.default_registry_config()
  let registry = plugin.new_registry(config)

  case plugin.register_plugin(registry, my_plugin()) {
    Ok(registry) -> {
      // Plugin registered successfully
      let loader = plugin.new_loader(registry, plugin.default_loader_config())
      case plugin.load_plugin(loader, "my_custom_logic") {
        Ok(loader) -> io.println("Plugin loaded!")
        Error(e) -> io.println("Load error: " <> plugin.load_error_to_string(e))
      }
    }
    Error(e) -> io.println("Registration error: " <> plugin.registry_error_to_string(e))
  }
}
```

## Plugin Components

### Custom Operators

Operators define the modal connectives of your logic:

```gleam
CustomOperator(
  symbol: "X",           // Short symbol (used in formulas)
  name: "Next",          // Human-readable name
  description: "...",    // What the operator means
  arity: 1,              // 1 = unary, 2 = binary
  unicode: "○",          // Unicode display symbol
  latex: "\\bigcirc",    // LaTeX representation
  semantics: UnaryModal(
    universal: True,     // True = □-like, False = ◇-like
    relation: "temporal" // Relation type
  ),
)
```

For binary operators (like Until):

```gleam
CustomOperator(
  symbol: "U",
  name: "Until",
  arity: 2,
  semantics: BinaryModal(
    description: "φ U ψ means φ holds until ψ becomes true"
  ),
  // ...
)
```

### Frame Conditions

Frame conditions define constraints on Kripke frames:

```gleam
FrameCondition(
  name: "seriality",
  formal: "∀x. ∃y. R(x,y)",  // First-order logic condition
  description: "Every world has at least one successor",
  required: True,             // Must be satisfied
)
```

### Accessibility Properties

Built-in accessibility relation properties:

| Property | Description |
|----------|-------------|
| `Reflexive` | Every world accesses itself |
| `Symmetric` | If w₁ → w₂, then w₂ → w₁ |
| `Transitive` | If w₁ → w₂ → w₃, then w₁ → w₃ |
| `Serial` | Every world accesses at least one world |
| `Euclidean` | If w₁ → w₂ and w₁ → w₃, then w₂ → w₃ |
| `WellFounded` | No infinite descending chains |
| `Linear` | Worlds form a total order |
| `Dense` | Between any two worlds is another |
| `Discrete` | Each world has immediate successor |
| `Custom(name, desc)` | Custom property with description |

### Inference Rules

Define proof rules for your logic:

```gleam
InferenceRule(
  name: "Necessitation",
  premises: ["⊢ φ"],           // Required premises
  conclusion: "⊢ □φ",           // What can be derived
  conditions: ["φ is a theorem"], // Side conditions
  soundness: Sound,             // Sound, FrameSound, or Derived
)
```

### Custom Validators

Add custom validation logic:

```gleam
CustomValidator(
  name: "my_validator",
  description: "Checks specific properties",
  validate: fn(formula, context) {
    // Return Ok(ValidationResult) or Error(ValidationError)
    Ok(ValidationResult(
      valid: True,
      messages: ["Validation passed"],
      data: dict.new(),
    ))
  },
  priority: 10,  // Lower = runs earlier
)
```

### Formula Transformers

Convert formulas between logics:

```gleam
FormulaTransformer(
  name: "ltl_to_ctl",
  source_logic: "LTL",
  target_logic: "CTL",
  transform: fn(formula) {
    // Transform and return Ok(new_formula) or Error(TransformError)
    Ok(transform_formula(formula))
  },
  preserves_validity: False,  // Does transformation preserve validity?
)
```

### Lifecycle Hooks

React to plugin lifecycle events:

```gleam
PluginHooks(
  on_load: Some(fn() {
    // Called when plugin is loaded
    io.println("Plugin loaded!")
    Ok(Nil)
  }),
  on_unload: Some(fn() {
    // Called when plugin is unloaded
    io.println("Plugin unloaded!")
    Ok(Nil)
  }),
  before_validate: Some(fn(formula) {
    // Transform formula before validation
    formula
  }),
  after_validate: Some(fn(formula, is_valid) {
    // Called after validation
    Nil
  }),
  on_error: Some(fn(error) {
    // Called on error
    io.println("Error: " <> error)
  }),
)
```

## Dependencies

Plugins can depend on other plugins:

```gleam
dependencies: [
  PluginDependency(
    plugin_id: "temporal_ltl",
    version_requirement: ">=1.0.0",  // Semver requirement
    optional: False,                  // Required dependency
  ),
],
```

### Version Requirements

| Syntax | Meaning |
|--------|---------|
| `1.0.0` | Exact version |
| `>=1.0.0` | Greater than or equal |
| `>1.0.0` | Greater than |
| `<=1.0.0` | Less than or equal |
| `<1.0.0` | Less than |
| `^1.2.3` | Compatible with 1.x.x (≥1.2.3, <2.0.0) |
| `~1.2.3` | Approximately 1.2.x (≥1.2.3, <1.3.0) |

## Security Sandboxing

Plugins run in a security sandbox with configurable limits:

```gleam
SandboxConfig(
  max_memory_bytes: 100_000_000,  // 100MB memory limit
  max_execution_ms: 30_000,        // 30s execution limit
  max_formula_depth: 100,          // Maximum nesting depth
  max_worlds: 1000,                // Maximum Kripke frame size
  allowed_operations: [OpEvaluate, OpBuildModel, OpProofSearch],
  network_allowed: False,          // No network access
  filesystem_allowed: False,       // No filesystem access
)
```

## Example: Temporal Logic Plugins

See the built-in temporal logic plugins for reference implementations:

- **LTL (Linear Temporal Logic)**: `modal_logic/plugins/temporal.gleam`
  - Operators: X (next), G (always), F (eventually), U (until), R (release)

- **CTL (Computation Tree Logic)**: `modal_logic/plugins/temporal.gleam`
  - Operators: AX, EX, AG, EG, AF, EF, AU, EU

- **CTL\***: Combines LTL and CTL

```gleam
import modal_logic/plugins/temporal

// Get all temporal plugins
let plugins = temporal.all_temporal_plugins()

// Get specific plugin
let ltl = temporal.ltl_plugin()
let ctl = temporal.ctl_plugin()
```

## Best Practices

1. **Unique IDs**: Use descriptive, namespaced plugin IDs (e.g., `temporal_ltl`, `deontic_sdl`)

2. **Semantic Versioning**: Follow semver for version numbers

3. **Documentation**: Provide clear descriptions for operators and rules

4. **Testing**: Write tests for your validators and transformers

5. **Minimal Dependencies**: Only depend on plugins you actually need

6. **Graceful Errors**: Return meaningful error messages

7. **Resource Efficiency**: Be mindful of memory and execution time

## Testing Your Plugin

```gleam
import gleeunit/should

pub fn my_plugin_loads_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let result = plugin.register_plugin(registry, my_plugin())

  result
  |> should.be_ok
}

pub fn my_plugin_validates_test() {
  // Test your custom validators
  let context = plugin.ValidationContext(
    logic: "MY_LOGIC",
    active_plugins: ["my_custom_logic"],
    context_data: dict.new(),
  )

  let formula = Atom("p")
  let validator = my_validator()

  validator.validate(formula, context)
  |> should.be_ok
}
```

## Publishing Your Plugin

1. Create a Git repository for your plugin
2. Add proper licensing and documentation
3. Submit to the community plugin registry (coming soon)
4. Share with the modal logic community!

## API Reference

For complete API documentation, see the source code:
- `modal_logic/plugin.gleam` - Core plugin types and functions
- `modal_logic/plugins/temporal.gleam` - Example temporal logic plugins
