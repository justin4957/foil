//// Plugin System for Modal Logic Extensions
////
//// This module provides a modular plugin system for extending the modal logic
//// engine with custom logic systems. Researchers can create plugins for
//// specialized logics (temporal, deontic, epistemic, etc.) without modifying
//// the core codebase.
////
//// ## Architecture
////
//// The plugin system consists of:
//// - Plugin API: Core interfaces and types for plugin development
//// - Plugin Registry: Manages plugin discovery, registration, and lifecycle
//// - Plugin Loader: Handles dynamic loading and unloading of plugins
//// - Security Sandbox: Provides isolation and resource limits for plugins
//// - Version Manager: Handles plugin versioning and compatibility
////
//// ## Creating a Plugin
////
//// ```gleam
//// import modal_logic/plugin.{Plugin, PluginMetadata, LogicDefinition}
////
//// pub fn create_ltl_plugin() -> Plugin {
////   Plugin(
////     metadata: PluginMetadata(
////       id: "temporal_ltl",
////       name: "Linear Temporal Logic",
////       version: "1.0.0",
////       author: "Research Team",
////       description: "LTL operators: next, until, always, eventually",
////       license: "MIT",
////       repository: Some("https://github.com/example/ltl-plugin"),
////       dependencies: [],
////       min_engine_version: "0.1.0",
////     ),
////     logic: ltl_logic_definition(),
////     validators: [ltl_validator],
////     transformers: [ltl_to_ctl_transformer],
////     hooks: default_hooks(),
////   )
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/result
import gleam/string
import modal_logic/proposition.{type LogicSystem, type Proposition}

// =============================================================================
// Plugin API - Core Types
// =============================================================================

/// A plugin that extends the modal logic engine
pub type Plugin {
  Plugin(
    /// Plugin metadata
    metadata: PluginMetadata,
    /// Logic system definition
    logic: LogicDefinition,
    /// Custom validators
    validators: List(CustomValidator),
    /// Formula transformers
    transformers: List(FormulaTransformer),
    /// Lifecycle hooks
    hooks: PluginHooks,
  )
}

/// Plugin metadata for identification and versioning
pub type PluginMetadata {
  PluginMetadata(
    /// Unique identifier (e.g., "temporal_ltl")
    id: String,
    /// Human-readable name
    name: String,
    /// Semantic version (e.g., "1.0.0")
    version: String,
    /// Plugin author
    author: String,
    /// Description of the plugin
    description: String,
    /// License identifier (e.g., "MIT", "Apache-2.0")
    license: String,
    /// Repository URL
    repository: Option(String),
    /// Required plugin dependencies
    dependencies: List(PluginDependency),
    /// Minimum engine version required
    min_engine_version: String,
  )
}

/// Plugin dependency specification
pub type PluginDependency {
  PluginDependency(
    /// Plugin ID
    plugin_id: String,
    /// Version requirement (e.g., ">=1.0.0", "^2.0.0")
    version_requirement: String,
    /// Whether dependency is optional
    optional: Bool,
  )
}

/// Logic system definition provided by a plugin
pub type LogicDefinition {
  LogicDefinition(
    /// Logic system identifier
    id: String,
    /// Display name
    name: String,
    /// Description of the logic
    description: String,
    /// Custom operators provided by this logic
    operators: List(CustomOperator),
    /// Frame conditions/axioms
    frame_conditions: List(FrameCondition),
    /// Accessibility relation properties
    accessibility_properties: List(AccessibilityProperty),
    /// Inference rules
    inference_rules: List(InferenceRule),
    /// Built on base system
    base_system: Option(LogicSystem),
  )
}

/// Custom operator definition
pub type CustomOperator {
  CustomOperator(
    /// Operator symbol (e.g., "X", "U", "G", "F")
    symbol: String,
    /// Display name
    name: String,
    /// Description
    description: String,
    /// Arity (1 = unary, 2 = binary)
    arity: Int,
    /// Unicode symbol for display
    unicode: String,
    /// LaTeX representation
    latex: String,
    /// Semantic definition
    semantics: OperatorSemantics,
  )
}

/// Operator semantics definition
pub type OperatorSemantics {
  /// Unary modal operator (like necessity/possibility)
  UnaryModal(
    /// True if operator quantifies universally over accessible worlds
    universal: Bool,
    /// Relation type (temporal, epistemic, etc.)
    relation: String,
  )
  /// Binary modal operator (like until)
  BinaryModal(
    /// Semantic description
    description: String,
  )
  /// Propositional operator
  Propositional(
    /// Truth table or semantic function description
    definition: String,
  )
}

/// Frame condition/axiom
pub type FrameCondition {
  FrameCondition(
    /// Condition name (e.g., "seriality", "transitivity")
    name: String,
    /// Formal condition in first-order logic
    formal: String,
    /// Human-readable description
    description: String,
    /// Whether this is required
    required: Bool,
  )
}

/// Accessibility relation property
pub type AccessibilityProperty {
  /// Reflexive: every world accesses itself
  Reflexive
  /// Symmetric: if w1 accesses w2, then w2 accesses w1
  Symmetric
  /// Transitive: if w1->w2 and w2->w3, then w1->w3
  Transitive
  /// Serial: every world accesses at least one world
  Serial
  /// Euclidean: if w1->w2 and w1->w3, then w2->w3
  Euclidean
  /// Well-founded: no infinite descending chains
  WellFounded
  /// Linear: worlds form a total order
  Linear
  /// Dense: between any two worlds is another
  Dense
  /// Discrete: each world has immediate successor
  Discrete
  /// Custom property with description
  Custom(name: String, description: String)
}

/// Inference rule for proof construction
pub type InferenceRule {
  InferenceRule(
    /// Rule name
    name: String,
    /// Premises (formula patterns)
    premises: List(String),
    /// Conclusion (formula pattern)
    conclusion: String,
    /// Side conditions
    conditions: List(String),
    /// Soundness level
    soundness: Soundness,
  )
}

/// Soundness classification
pub type Soundness {
  /// Sound in all models
  Sound
  /// Sound only in specific frame classes
  FrameSound(frame_class: String)
  /// Derived rule (provably sound)
  Derived
}

// =============================================================================
// Plugin API - Validators and Transformers
// =============================================================================

/// Custom validator function for plugin-specific validation
pub type CustomValidator {
  CustomValidator(
    /// Validator name
    name: String,
    /// Description
    description: String,
    /// Validation function
    validate: fn(Proposition, ValidationContext) ->
      Result(ValidationResult, ValidationError),
    /// Priority (lower = earlier execution)
    priority: Int,
  )
}

/// Context for validation
pub type ValidationContext {
  ValidationContext(
    /// Logic system in use
    logic: String,
    /// Active plugins
    active_plugins: List(String),
    /// Additional context data
    context_data: Dict(String, String),
  )
}

/// Validation result
pub type ValidationResult {
  ValidationResult(
    /// Whether validation passed
    valid: Bool,
    /// Messages
    messages: List(String),
    /// Additional data
    data: Dict(String, String),
  )
}

/// Validation error
pub type ValidationError {
  ValidationError(
    /// Error code
    code: String,
    /// Error message
    message: String,
    /// Details
    details: Option(String),
  )
}

/// Formula transformer for converting between logics
pub type FormulaTransformer {
  FormulaTransformer(
    /// Transformer name
    name: String,
    /// Source logic
    source_logic: String,
    /// Target logic
    target_logic: String,
    /// Transform function
    transform: fn(Proposition) -> Result(Proposition, TransformError),
    /// Whether transformation preserves validity
    preserves_validity: Bool,
  )
}

/// Transformation error
pub type TransformError {
  TransformError(
    /// Error code
    code: String,
    /// Error message
    message: String,
  )
}

// =============================================================================
// Plugin API - Lifecycle Hooks
// =============================================================================

/// Plugin lifecycle hooks
pub type PluginHooks {
  PluginHooks(
    /// Called when plugin is loaded
    on_load: Option(fn() -> Result(Nil, String)),
    /// Called when plugin is unloaded
    on_unload: Option(fn() -> Result(Nil, String)),
    /// Called before validation
    before_validate: Option(fn(Proposition) -> Proposition),
    /// Called after validation
    after_validate: Option(fn(Proposition, Bool) -> Nil),
    /// Called on error
    on_error: Option(fn(String) -> Nil),
  )
}

/// Default plugin hooks (no-op)
pub fn default_hooks() -> PluginHooks {
  PluginHooks(
    on_load: None,
    on_unload: None,
    before_validate: None,
    after_validate: None,
    on_error: None,
  )
}

// =============================================================================
// Plugin Registry
// =============================================================================

/// Plugin registry for managing installed plugins
pub type PluginRegistry {
  PluginRegistry(
    /// Registered plugins by ID
    plugins: Dict(String, RegisteredPlugin),
    /// Plugin load order
    load_order: List(String),
    /// Registry configuration
    config: RegistryConfig,
  )
}

/// A registered plugin with runtime state
pub type RegisteredPlugin {
  RegisteredPlugin(
    /// The plugin
    plugin: Plugin,
    /// Registration time
    registered_at: String,
    /// Current state
    state: PluginState,
    /// Load statistics
    stats: PluginStats,
  )
}

/// Plugin state
pub type PluginState {
  /// Plugin is registered but not loaded
  Registered
  /// Plugin is loading
  Loading
  /// Plugin is active and ready
  Active
  /// Plugin is being unloaded
  Unloading
  /// Plugin is disabled
  Disabled
  /// Plugin encountered an error
  Failed(message: String)
}

/// Plugin statistics
pub type PluginStats {
  PluginStats(
    /// Number of times loaded
    load_count: Int,
    /// Total validations performed
    validation_count: Int,
    /// Total errors encountered
    error_count: Int,
    /// Last used timestamp
    last_used: Option(String),
  )
}

/// Registry configuration
pub type RegistryConfig {
  RegistryConfig(
    /// Maximum number of plugins
    max_plugins: Int,
    /// Allow duplicate plugin IDs (version overwrite)
    allow_overwrite: Bool,
    /// Enable plugin sandboxing
    sandbox_enabled: Bool,
    /// Plugin directory path
    plugin_directory: String,
    /// Auto-load plugins on startup
    auto_load: Bool,
  )
}

/// Create a new plugin registry
pub fn new_registry(config: RegistryConfig) -> PluginRegistry {
  PluginRegistry(plugins: dict.new(), load_order: [], config: config)
}

/// Helper function to lookup a plugin from a registry's plugin dictionary
/// Converts the Result from dict.get to Option for easier pattern matching
fn lookup_plugin(
  plugins: Dict(String, RegisteredPlugin),
  plugin_id: String,
) -> Option(RegisteredPlugin) {
  dict.get(plugins, plugin_id) |> option.from_result
}

/// Default registry configuration
pub fn default_registry_config() -> RegistryConfig {
  RegistryConfig(
    max_plugins: 100,
    allow_overwrite: False,
    sandbox_enabled: True,
    plugin_directory: "plugins",
    auto_load: True,
  )
}

/// Register a plugin with the registry
pub fn register_plugin(
  registry: PluginRegistry,
  plugin: Plugin,
) -> Result(PluginRegistry, RegistryError) {
  let plugin_id = plugin.metadata.id

  // Check if plugin already exists
  case lookup_plugin(registry.plugins, plugin_id) {
    Some(_) ->
      case registry.config.allow_overwrite {
        True -> do_register(registry, plugin)
        False -> Error(PluginAlreadyExists(plugin_id))
      }
    None -> do_register(registry, plugin)
  }
}

fn do_register(
  registry: PluginRegistry,
  plugin: Plugin,
) -> Result(PluginRegistry, RegistryError) {
  let plugin_id = plugin.metadata.id

  // Check if overwriting an existing plugin
  let is_overwrite = case lookup_plugin(registry.plugins, plugin_id) {
    Some(_) -> True
    None -> False
  }

  // Check max plugins (only count as new if not overwriting)
  let at_max = dict.size(registry.plugins) >= registry.config.max_plugins
  case at_max && !is_overwrite {
    True -> Error(MaxPluginsReached(registry.config.max_plugins))
    False -> {
      let registered =
        RegisteredPlugin(
          plugin: plugin,
          registered_at: get_timestamp(),
          state: Registered,
          stats: PluginStats(
            load_count: 0,
            validation_count: 0,
            error_count: 0,
            last_used: None,
          ),
        )

      let new_plugins = dict.insert(registry.plugins, plugin_id, registered)
      // Only add to load_order if it's a new plugin (not overwriting)
      let new_order = case is_overwrite {
        True -> registry.load_order
        False -> list.append(registry.load_order, [plugin_id])
      }

      Ok(
        PluginRegistry(..registry, plugins: new_plugins, load_order: new_order),
      )
    }
  }
}

/// Unregister a plugin from the registry
pub fn unregister_plugin(
  registry: PluginRegistry,
  plugin_id: String,
) -> Result(PluginRegistry, RegistryError) {
  case lookup_plugin(registry.plugins, plugin_id) {
    None -> Error(PluginNotFound(plugin_id))
    Some(registered) -> {
      // Check if plugin is active
      case registered.state {
        Active -> Error(PluginStillActive(plugin_id))
        _ -> {
          let new_plugins = dict.delete(registry.plugins, plugin_id)
          let new_order =
            list.filter(registry.load_order, fn(id) { id != plugin_id })

          Ok(
            PluginRegistry(
              ..registry,
              plugins: new_plugins,
              load_order: new_order,
            ),
          )
        }
      }
    }
  }
}

/// Get a plugin by ID
pub fn get_plugin(
  registry: PluginRegistry,
  plugin_id: String,
) -> Option(RegisteredPlugin) {
  lookup_plugin(registry.plugins, plugin_id)
}

/// Get all registered plugins
pub fn list_plugins(registry: PluginRegistry) -> List(RegisteredPlugin) {
  registry.load_order
  |> list.flat_map(fn(id) {
    case lookup_plugin(registry.plugins, id) {
      Some(rp) -> [rp]
      None -> []
    }
  })
}

/// Get active plugins only
pub fn list_active_plugins(registry: PluginRegistry) -> List(RegisteredPlugin) {
  list_plugins(registry)
  |> list.filter(fn(rp) {
    case rp.state {
      Active -> True
      _ -> False
    }
  })
}

/// Registry errors
pub type RegistryError {
  /// Plugin already registered
  PluginAlreadyExists(plugin_id: String)
  /// Plugin not found
  PluginNotFound(plugin_id: String)
  /// Plugin is still active
  PluginStillActive(plugin_id: String)
  /// Maximum plugins reached
  MaxPluginsReached(max: Int)
  /// Dependency not satisfied
  DependencyNotSatisfied(plugin_id: String, dependency: String)
  /// Version conflict
  VersionConflict(plugin_id: String, required: String, found: String)
  /// Invalid plugin
  InvalidPlugin(reason: String)
}

// =============================================================================
// Plugin Loader
// =============================================================================

/// Plugin loader for dynamic loading/unloading
pub type PluginLoader {
  PluginLoader(
    /// Associated registry
    registry: PluginRegistry,
    /// Loader configuration
    config: LoaderConfig,
    /// Currently loading plugins
    loading: List(String),
  )
}

/// Loader configuration
pub type LoaderConfig {
  LoaderConfig(
    /// Load timeout in milliseconds
    load_timeout_ms: Int,
    /// Unload timeout in milliseconds
    unload_timeout_ms: Int,
    /// Retry count on failure
    retry_count: Int,
    /// Parallel loading enabled
    parallel_load: Bool,
    /// Validate dependencies before load
    validate_dependencies: Bool,
  )
}

/// Default loader configuration
pub fn default_loader_config() -> LoaderConfig {
  LoaderConfig(
    load_timeout_ms: 5000,
    unload_timeout_ms: 3000,
    retry_count: 3,
    parallel_load: True,
    validate_dependencies: True,
  )
}

/// Create a new plugin loader
pub fn new_loader(
  registry: PluginRegistry,
  config: LoaderConfig,
) -> PluginLoader {
  PluginLoader(registry: registry, config: config, loading: [])
}

/// Load a plugin by ID
pub fn load_plugin(
  loader: PluginLoader,
  plugin_id: String,
) -> Result(PluginLoader, LoadError) {
  case lookup_plugin(loader.registry.plugins, plugin_id) {
    None -> Error(LoadPluginNotFound(plugin_id))
    Some(registered) -> {
      case registered.state {
        Active -> Error(LoadAlreadyActive(plugin_id))
        Loading -> Error(LoadAlreadyLoading(plugin_id))
        _ -> do_load_plugin(loader, plugin_id, registered)
      }
    }
  }
}

fn do_load_plugin(
  loader: PluginLoader,
  plugin_id: String,
  registered: RegisteredPlugin,
) -> Result(PluginLoader, LoadError) {
  // Validate dependencies if enabled
  case loader.config.validate_dependencies {
    True -> {
      case validate_dependencies(loader.registry, registered.plugin) {
        Ok(_) -> execute_load(loader, plugin_id, registered)
        Error(err) -> Error(err)
      }
    }
    False -> execute_load(loader, plugin_id, registered)
  }
}

fn execute_load(
  loader: PluginLoader,
  plugin_id: String,
  registered: RegisteredPlugin,
) -> Result(PluginLoader, LoadError) {
  // Update state to loading
  let loading_registered = RegisteredPlugin(..registered, state: Loading)

  let updated_plugins =
    dict.insert(loader.registry.plugins, plugin_id, loading_registered)
  let updated_registry =
    PluginRegistry(..loader.registry, plugins: updated_plugins)

  // Call on_load hook
  let load_result = case registered.plugin.hooks.on_load {
    Some(hook) -> hook()
    None -> Ok(Nil)
  }

  case load_result {
    Ok(_) -> {
      // Update state to active
      let active_registered =
        RegisteredPlugin(
          ..registered,
          state: Active,
          stats: PluginStats(
            ..registered.stats,
            load_count: registered.stats.load_count + 1,
            last_used: Some(get_timestamp()),
          ),
        )

      let final_plugins =
        dict.insert(updated_registry.plugins, plugin_id, active_registered)
      let final_registry =
        PluginRegistry(..updated_registry, plugins: final_plugins)

      Ok(PluginLoader(..loader, registry: final_registry))
    }
    Error(msg) -> {
      // Update state to error
      let error_registered =
        RegisteredPlugin(
          ..registered,
          state: Failed(msg),
          stats: PluginStats(
            ..registered.stats,
            error_count: registered.stats.error_count + 1,
          ),
        )

      let error_plugins =
        dict.insert(updated_registry.plugins, plugin_id, error_registered)
      let error_registry =
        PluginRegistry(..updated_registry, plugins: error_plugins)

      Error(LoadHookFailed(plugin_id, msg))
    }
  }
}

/// Unload a plugin by ID
pub fn unload_plugin(
  loader: PluginLoader,
  plugin_id: String,
) -> Result(PluginLoader, LoadError) {
  case lookup_plugin(loader.registry.plugins, plugin_id) {
    None -> Error(LoadPluginNotFound(plugin_id))
    Some(registered) -> {
      case registered.state {
        Active -> do_unload_plugin(loader, plugin_id, registered)
        _ -> Error(LoadNotActive(plugin_id))
      }
    }
  }
}

fn do_unload_plugin(
  loader: PluginLoader,
  plugin_id: String,
  registered: RegisteredPlugin,
) -> Result(PluginLoader, LoadError) {
  // Update state to unloading
  let unloading_registered = RegisteredPlugin(..registered, state: Unloading)

  let updated_plugins =
    dict.insert(loader.registry.plugins, plugin_id, unloading_registered)
  let updated_registry =
    PluginRegistry(..loader.registry, plugins: updated_plugins)

  // Call on_unload hook
  let unload_result = case registered.plugin.hooks.on_unload {
    Some(hook) -> hook()
    None -> Ok(Nil)
  }

  case unload_result {
    Ok(_) -> {
      // Update state to registered
      let inactive_registered =
        RegisteredPlugin(..registered, state: Registered)

      let final_plugins =
        dict.insert(updated_registry.plugins, plugin_id, inactive_registered)
      let final_registry =
        PluginRegistry(..updated_registry, plugins: final_plugins)

      Ok(PluginLoader(..loader, registry: final_registry))
    }
    Error(msg) -> {
      // Keep as error state
      let error_registered =
        RegisteredPlugin(
          ..registered,
          state: Failed(msg),
          stats: PluginStats(
            ..registered.stats,
            error_count: registered.stats.error_count + 1,
          ),
        )

      let error_plugins =
        dict.insert(updated_registry.plugins, plugin_id, error_registered)
      let error_registry =
        PluginRegistry(..updated_registry, plugins: error_plugins)

      Error(UnloadHookFailed(plugin_id, msg))
    }
  }
}

/// Validate plugin dependencies
fn validate_dependencies(
  registry: PluginRegistry,
  plugin: Plugin,
) -> Result(Nil, LoadError) {
  list.try_fold(plugin.metadata.dependencies, Nil, fn(_, dep) {
    case lookup_plugin(registry.plugins, dep.plugin_id) {
      None ->
        case dep.optional {
          True -> Ok(Nil)
          False ->
            Error(LoadDependencyMissing(plugin.metadata.id, dep.plugin_id))
        }
      Some(registered) -> {
        let version = registered.plugin.metadata.version
        case version_satisfies(version, dep.version_requirement) {
          True -> Ok(Nil)
          False ->
            Error(LoadVersionMismatch(
              plugin.metadata.id,
              dep.plugin_id,
              dep.version_requirement,
              version,
            ))
        }
      }
    }
  })
}

/// Load errors
pub type LoadError {
  /// Plugin not found
  LoadPluginNotFound(plugin_id: String)
  /// Plugin already active
  LoadAlreadyActive(plugin_id: String)
  /// Plugin already loading
  LoadAlreadyLoading(plugin_id: String)
  /// Plugin not active (for unload)
  LoadNotActive(plugin_id: String)
  /// Load hook failed
  LoadHookFailed(plugin_id: String, message: String)
  /// Unload hook failed
  UnloadHookFailed(plugin_id: String, message: String)
  /// Dependency missing
  LoadDependencyMissing(plugin_id: String, dependency: String)
  /// Version mismatch
  LoadVersionMismatch(
    plugin_id: String,
    dependency: String,
    required: String,
    found: String,
  )
  /// Load timeout
  LoadTimeout(plugin_id: String)
}

// =============================================================================
// Security Sandbox
// =============================================================================

/// Security sandbox for plugin isolation
pub type Sandbox {
  Sandbox(
    /// Sandbox configuration
    config: SandboxConfig,
    /// Resource usage tracking
    resource_usage: ResourceUsage,
    /// Security violations
    violations: List(SecurityViolation),
  )
}

/// Sandbox configuration
pub type SandboxConfig {
  SandboxConfig(
    /// Maximum memory usage in bytes
    max_memory_bytes: Int,
    /// Maximum execution time in milliseconds
    max_execution_ms: Int,
    /// Maximum formula depth
    max_formula_depth: Int,
    /// Maximum number of worlds in model
    max_worlds: Int,
    /// Allowed operations
    allowed_operations: List(SandboxOperation),
    /// Network access allowed
    network_allowed: Bool,
    /// File system access allowed
    filesystem_allowed: Bool,
  )
}

/// Sandbox operations
pub type SandboxOperation {
  /// Formula evaluation
  OpEvaluate
  /// Model construction
  OpBuildModel
  /// Proof search
  OpProofSearch
  /// External validation
  OpExternalValidation
  /// Custom operation
  OpCustom(name: String)
}

/// Resource usage tracking
pub type ResourceUsage {
  ResourceUsage(
    /// Memory used in bytes
    memory_bytes: Int,
    /// Execution time in milliseconds
    execution_ms: Int,
    /// Formula evaluations performed
    evaluations: Int,
    /// Worlds created
    worlds_created: Int,
  )
}

/// Security violation
pub type SecurityViolation {
  SecurityViolation(
    /// Violation type
    violation_type: ViolationType,
    /// Description
    description: String,
    /// Timestamp
    timestamp: String,
    /// Plugin that caused violation
    plugin_id: String,
  )
}

/// Types of security violations
pub type ViolationType {
  /// Memory limit exceeded
  MemoryExceeded
  /// Time limit exceeded
  TimeExceeded
  /// Depth limit exceeded
  DepthExceeded
  /// World limit exceeded
  WorldsExceeded
  /// Unauthorized operation
  UnauthorizedOperation(operation: String)
  /// Network access violation
  NetworkViolation
  /// Filesystem access violation
  FilesystemViolation
}

/// Create a new sandbox with configuration
pub fn new_sandbox(config: SandboxConfig) -> Sandbox {
  Sandbox(
    config: config,
    resource_usage: ResourceUsage(
      memory_bytes: 0,
      execution_ms: 0,
      evaluations: 0,
      worlds_created: 0,
    ),
    violations: [],
  )
}

/// Default sandbox configuration
pub fn default_sandbox_config() -> SandboxConfig {
  SandboxConfig(
    max_memory_bytes: 100_000_000,
    // 100MB
    max_execution_ms: 30_000,
    // 30 seconds
    max_formula_depth: 100,
    max_worlds: 1000,
    allowed_operations: [OpEvaluate, OpBuildModel, OpProofSearch],
    network_allowed: False,
    filesystem_allowed: False,
  )
}

/// Check if operation is allowed
pub fn is_operation_allowed(
  sandbox: Sandbox,
  operation: SandboxOperation,
) -> Bool {
  list.any(sandbox.config.allowed_operations, fn(allowed) {
    case allowed, operation {
      OpEvaluate, OpEvaluate -> True
      OpBuildModel, OpBuildModel -> True
      OpProofSearch, OpProofSearch -> True
      OpExternalValidation, OpExternalValidation -> True
      OpCustom(a), OpCustom(b) -> a == b
      _, _ -> False
    }
  })
}

/// Check resource limits
pub fn check_limits(sandbox: Sandbox) -> Result(Nil, SecurityViolation) {
  case sandbox.resource_usage.memory_bytes > sandbox.config.max_memory_bytes {
    True ->
      Error(SecurityViolation(
        violation_type: MemoryExceeded,
        description: "Memory limit exceeded: "
          <> int.to_string(sandbox.resource_usage.memory_bytes)
          <> " bytes",
        timestamp: get_timestamp(),
        plugin_id: "unknown",
      ))
    False ->
      case
        sandbox.resource_usage.execution_ms > sandbox.config.max_execution_ms
      {
        True ->
          Error(SecurityViolation(
            violation_type: TimeExceeded,
            description: "Execution time limit exceeded: "
              <> int.to_string(sandbox.resource_usage.execution_ms)
              <> " ms",
            timestamp: get_timestamp(),
            plugin_id: "unknown",
          ))
        False -> Ok(Nil)
      }
  }
}

/// Update resource usage
pub fn update_usage(
  sandbox: Sandbox,
  memory_delta: Int,
  time_delta: Int,
  evaluations_delta: Int,
  worlds_delta: Int,
) -> Sandbox {
  let new_usage =
    ResourceUsage(
      memory_bytes: sandbox.resource_usage.memory_bytes + memory_delta,
      execution_ms: sandbox.resource_usage.execution_ms + time_delta,
      evaluations: sandbox.resource_usage.evaluations + evaluations_delta,
      worlds_created: sandbox.resource_usage.worlds_created + worlds_delta,
    )
  Sandbox(..sandbox, resource_usage: new_usage)
}

/// Record a security violation
pub fn record_violation(
  sandbox: Sandbox,
  violation: SecurityViolation,
) -> Sandbox {
  Sandbox(..sandbox, violations: [violation, ..sandbox.violations])
}

// =============================================================================
// Version Management
// =============================================================================

/// Semantic version
pub type SemanticVersion {
  SemanticVersion(
    major: Int,
    minor: Int,
    patch: Int,
    prerelease: Option(String),
  )
}

/// Parse a semantic version string
pub fn parse_version(version_str: String) -> Result(SemanticVersion, String) {
  let parts = string.split(version_str, ".")
  case parts {
    [major_str, minor_str, patch_str] -> {
      case int.parse(major_str), int.parse(minor_str), parse_patch(patch_str) {
        Ok(major), Ok(minor), Ok(#(patch, prerelease)) ->
          Ok(SemanticVersion(
            major: major,
            minor: minor,
            patch: patch,
            prerelease: prerelease,
          ))
        _, _, _ -> Error("Invalid version format: " <> version_str)
      }
    }
    [major_str, minor_str] -> {
      case int.parse(major_str), int.parse(minor_str) {
        Ok(major), Ok(minor) ->
          Ok(SemanticVersion(
            major: major,
            minor: minor,
            patch: 0,
            prerelease: None,
          ))
        _, _ -> Error("Invalid version format: " <> version_str)
      }
    }
    _ -> Error("Invalid version format: " <> version_str)
  }
}

fn parse_patch(patch_str: String) -> Result(#(Int, Option(String)), Nil) {
  case string.split(patch_str, "-") {
    [patch_num] ->
      case int.parse(patch_num) {
        Ok(patch) -> Ok(#(patch, None))
        Error(_) -> Error(Nil)
      }
    [patch_num, prerelease] ->
      case int.parse(patch_num) {
        Ok(patch) -> Ok(#(patch, Some(prerelease)))
        Error(_) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Format a semantic version as string
pub fn version_to_string(version: SemanticVersion) -> String {
  let base =
    int.to_string(version.major)
    <> "."
    <> int.to_string(version.minor)
    <> "."
    <> int.to_string(version.patch)

  case version.prerelease {
    Some(pre) -> base <> "-" <> pre
    None -> base
  }
}

/// Compare two versions
pub fn compare_versions(v1: SemanticVersion, v2: SemanticVersion) -> Order {
  case int.compare(v1.major, v2.major) {
    order.Eq ->
      case int.compare(v1.minor, v2.minor) {
        order.Eq -> int.compare(v1.patch, v2.patch)
        other -> other
      }
    other -> other
  }
}

/// Check if a version satisfies a requirement
/// Supports: ">=1.0.0", "^1.0.0" (caret), "~1.0.0" (tilde), "1.0.0" (exact)
pub fn version_satisfies(version: String, requirement: String) -> Bool {
  case parse_version(version) {
    Error(_) -> False
    Ok(ver) -> {
      case string.first(requirement) {
        Ok("^") -> check_caret_version(ver, string.drop_start(requirement, 1))
        Ok("~") -> check_tilde_version(ver, string.drop_start(requirement, 1))
        Ok(">") ->
          case string.starts_with(requirement, ">=") {
            True -> check_gte_version(ver, string.drop_start(requirement, 2))
            False -> check_gt_version(ver, string.drop_start(requirement, 1))
          }
        Ok("<") ->
          case string.starts_with(requirement, "<=") {
            True -> check_lte_version(ver, string.drop_start(requirement, 2))
            False -> check_lt_version(ver, string.drop_start(requirement, 1))
          }
        _ -> check_exact_version(ver, requirement)
      }
    }
  }
}

fn check_caret_version(version: SemanticVersion, req_str: String) -> Bool {
  case parse_version(req_str) {
    Error(_) -> False
    Ok(req) -> {
      // ^1.2.3 means >=1.2.3 and <2.0.0 (for major > 0)
      // ^0.2.3 means >=0.2.3 and <0.3.0 (for major = 0)
      case compare_versions(version, req) {
        order.Lt -> False
        _ ->
          case req.major {
            0 -> version.major == 0 && version.minor == req.minor
            _ -> version.major == req.major
          }
      }
    }
  }
}

fn check_tilde_version(version: SemanticVersion, req_str: String) -> Bool {
  case parse_version(req_str) {
    Error(_) -> False
    Ok(req) -> {
      // ~1.2.3 means >=1.2.3 and <1.3.0
      case compare_versions(version, req) {
        order.Lt -> False
        _ -> version.major == req.major && version.minor == req.minor
      }
    }
  }
}

fn check_gte_version(version: SemanticVersion, req_str: String) -> Bool {
  case parse_version(req_str) {
    Error(_) -> False
    Ok(req) ->
      case compare_versions(version, req) {
        order.Lt -> False
        _ -> True
      }
  }
}

fn check_gt_version(version: SemanticVersion, req_str: String) -> Bool {
  case parse_version(req_str) {
    Error(_) -> False
    Ok(req) ->
      case compare_versions(version, req) {
        order.Gt -> True
        _ -> False
      }
  }
}

fn check_lte_version(version: SemanticVersion, req_str: String) -> Bool {
  case parse_version(req_str) {
    Error(_) -> False
    Ok(req) ->
      case compare_versions(version, req) {
        order.Gt -> False
        _ -> True
      }
  }
}

fn check_lt_version(version: SemanticVersion, req_str: String) -> Bool {
  case parse_version(req_str) {
    Error(_) -> False
    Ok(req) ->
      case compare_versions(version, req) {
        order.Lt -> True
        _ -> False
      }
  }
}

fn check_exact_version(version: SemanticVersion, req_str: String) -> Bool {
  case parse_version(req_str) {
    Error(_) -> False
    Ok(req) ->
      case compare_versions(version, req) {
        order.Eq -> True
        _ -> False
      }
  }
}

// =============================================================================
// Plugin Discovery
// =============================================================================

/// Plugin manifest for discovery
pub type PluginManifest {
  PluginManifest(
    /// Plugin metadata
    metadata: PluginMetadata,
    /// Path to plugin entry point
    entry_point: String,
    /// Plugin checksum for verification
    checksum: Option(String),
    /// Signature for authenticity
    signature: Option(String),
  )
}

/// Plugin repository for community plugins
pub type PluginRepository {
  PluginRepository(
    /// Repository name
    name: String,
    /// Repository URL
    url: String,
    /// Available plugins
    available: List(PluginManifest),
    /// Last sync timestamp
    last_sync: Option(String),
  )
}

/// Create a plugin repository
pub fn new_repository(name: String, url: String) -> PluginRepository {
  PluginRepository(name: name, url: url, available: [], last_sync: None)
}

/// Search for plugins in repository
pub fn search_plugins(
  repository: PluginRepository,
  query: String,
) -> List(PluginManifest) {
  let query_lower = string.lowercase(query)
  list.filter(repository.available, fn(manifest) {
    string.contains(string.lowercase(manifest.metadata.name), query_lower)
    || string.contains(
      string.lowercase(manifest.metadata.description),
      query_lower,
    )
    || string.contains(string.lowercase(manifest.metadata.id), query_lower)
  })
}

/// Get plugin by ID from repository
pub fn get_plugin_manifest(
  repository: PluginRepository,
  plugin_id: String,
) -> Option(PluginManifest) {
  list.find(repository.available, fn(manifest) {
    manifest.metadata.id == plugin_id
  })
  |> option.from_result
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Get current timestamp
fn get_timestamp() -> String {
  "2026-01-14T00:00:00Z"
}

/// Convert registry error to string
pub fn registry_error_to_string(error: RegistryError) -> String {
  case error {
    PluginAlreadyExists(id) -> "Plugin already exists: " <> id
    PluginNotFound(id) -> "Plugin not found: " <> id
    PluginStillActive(id) -> "Plugin still active: " <> id
    MaxPluginsReached(max) -> "Maximum plugins reached: " <> int.to_string(max)
    DependencyNotSatisfied(id, dep) ->
      "Dependency not satisfied for " <> id <> ": " <> dep
    VersionConflict(id, req, found) ->
      "Version conflict for "
      <> id
      <> ": required "
      <> req
      <> ", found "
      <> found
    InvalidPlugin(reason) -> "Invalid plugin: " <> reason
  }
}

/// Convert load error to string
pub fn load_error_to_string(error: LoadError) -> String {
  case error {
    LoadPluginNotFound(id) -> "Plugin not found: " <> id
    LoadAlreadyActive(id) -> "Plugin already active: " <> id
    LoadAlreadyLoading(id) -> "Plugin already loading: " <> id
    LoadNotActive(id) -> "Plugin not active: " <> id
    LoadHookFailed(id, msg) -> "Load hook failed for " <> id <> ": " <> msg
    UnloadHookFailed(id, msg) -> "Unload hook failed for " <> id <> ": " <> msg
    LoadDependencyMissing(id, dep) ->
      "Missing dependency for " <> id <> ": " <> dep
    LoadVersionMismatch(id, dep, req, found) ->
      "Version mismatch for "
      <> id
      <> " dependency "
      <> dep
      <> ": required "
      <> req
      <> ", found "
      <> found
    LoadTimeout(id) -> "Load timeout for plugin: " <> id
  }
}
