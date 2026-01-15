//// Plugin System Tests
////
//// Tests for the modular plugin system including:
//// - Plugin registration and lifecycle
//// - Version management and dependency resolution
//// - Security sandboxing
//// - Example temporal logic plugins

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleeunit/should
import modal_logic/plugin.{
  type LoadError, type Plugin, type PluginRegistry, type RegistryError,
  type Sandbox, type SandboxConfig, type SemanticVersion, Active, CustomOperator,
  CustomValidator, Disabled, Failed, FormulaTransformer, FrameCondition,
  InferenceRule, LoadAlreadyActive, LoadHookFailed, LoadPluginNotFound,
  LogicDefinition, MaxPluginsReached, OpBuildModel, OpCustom, OpEvaluate,
  OpProofSearch, Plugin, PluginAlreadyExists, PluginDependency, PluginMetadata,
  PluginNotFound, PluginStillActive, Reflexive, Registered, SandboxConfig, Sound,
  Transitive, UnaryModal, ValidationResult,
}
import modal_logic/plugins/temporal
import modal_logic/proposition.{Atom, K, Necessary, Not, S4}

// =============================================================================
// Plugin Metadata Tests
// =============================================================================

pub fn plugin_metadata_creation_test() {
  let metadata =
    PluginMetadata(
      id: "test_plugin",
      name: "Test Plugin",
      version: "1.0.0",
      author: "Test Author",
      description: "A test plugin",
      license: "MIT",
      repository: Some("https://github.com/test/plugin"),
      dependencies: [],
      min_engine_version: "0.1.0",
    )

  metadata.id
  |> should.equal("test_plugin")

  metadata.version
  |> should.equal("1.0.0")

  metadata.dependencies
  |> list.length
  |> should.equal(0)
}

pub fn plugin_with_dependencies_test() {
  let metadata =
    PluginMetadata(
      id: "dependent_plugin",
      name: "Dependent Plugin",
      version: "2.0.0",
      author: "Test Author",
      description: "A plugin with dependencies",
      license: "MIT",
      repository: None,
      dependencies: [
        PluginDependency(
          plugin_id: "base_plugin",
          version_requirement: ">=1.0.0",
          optional: False,
        ),
        PluginDependency(
          plugin_id: "optional_plugin",
          version_requirement: "^1.0.0",
          optional: True,
        ),
      ],
      min_engine_version: "0.1.0",
    )

  metadata.dependencies
  |> list.length
  |> should.equal(2)

  case list.first(metadata.dependencies) {
    Ok(dep) -> {
      dep.plugin_id
      |> should.equal("base_plugin")
      dep.optional
      |> should.equal(False)
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Logic Definition Tests
// =============================================================================

pub fn logic_definition_creation_test() {
  let logic =
    LogicDefinition(
      id: "TEST_LOGIC",
      name: "Test Logic",
      description: "A test logic system",
      operators: [
        CustomOperator(
          symbol: "T",
          name: "Test Operator",
          description: "A test operator",
          arity: 1,
          unicode: "T",
          latex: "\\mathbf{T}",
          semantics: UnaryModal(universal: True, relation: "test"),
        ),
      ],
      frame_conditions: [
        FrameCondition(
          name: "test_condition",
          formal: "∀x. R(x,x)",
          description: "Test condition",
          required: True,
        ),
      ],
      accessibility_properties: [Reflexive, Transitive],
      inference_rules: [
        InferenceRule(
          name: "Test-Rule",
          premises: ["φ"],
          conclusion: "T φ",
          conditions: [],
          soundness: Sound,
        ),
      ],
      base_system: Some(K),
    )

  logic.id
  |> should.equal("TEST_LOGIC")

  logic.operators
  |> list.length
  |> should.equal(1)

  logic.accessibility_properties
  |> list.length
  |> should.equal(2)
}

// =============================================================================
// Plugin Registry Tests
// =============================================================================

fn create_test_plugin(id: String) -> Plugin {
  Plugin(
    metadata: PluginMetadata(
      id: id,
      name: "Test Plugin " <> id,
      version: "1.0.0",
      author: "Test",
      description: "Test plugin",
      license: "MIT",
      repository: None,
      dependencies: [],
      min_engine_version: "0.1.0",
    ),
    logic: LogicDefinition(
      id: id <> "_LOGIC",
      name: "Test Logic",
      description: "Test",
      operators: [],
      frame_conditions: [],
      accessibility_properties: [],
      inference_rules: [],
      base_system: None,
    ),
    validators: [],
    transformers: [],
    hooks: plugin.default_hooks(),
  )
}

pub fn registry_creation_test() {
  let config = plugin.default_registry_config()
  let registry = plugin.new_registry(config)

  plugin.list_plugins(registry)
  |> list.length
  |> should.equal(0)
}

pub fn registry_register_plugin_test() {
  let config = plugin.default_registry_config()
  let registry = plugin.new_registry(config)
  let test_plugin = create_test_plugin("test_1")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      plugin.list_plugins(updated_registry)
      |> list.length
      |> should.equal(1)

      case plugin.get_plugin(updated_registry, "test_1") {
        Some(registered) -> {
          registered.plugin.metadata.id
          |> should.equal("test_1")

          case registered.state {
            Registered -> should.be_true(True)
            _ -> should.fail()
          }
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn registry_duplicate_plugin_error_test() {
  let config = plugin.default_registry_config()
  let registry = plugin.new_registry(config)
  let test_plugin = create_test_plugin("duplicate")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      // Try to register again
      case plugin.register_plugin(updated_registry, test_plugin) {
        Ok(_) -> should.fail()
        Error(err) ->
          case err {
            PluginAlreadyExists(id) ->
              id
              |> should.equal("duplicate")
            _ -> should.fail()
          }
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn registry_allow_overwrite_test() {
  let config =
    plugin.RegistryConfig(
      ..plugin.default_registry_config(),
      allow_overwrite: True,
    )
  let registry = plugin.new_registry(config)
  let test_plugin = create_test_plugin("overwritable")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      // Try to register again - should succeed with overwrite enabled
      case plugin.register_plugin(updated_registry, test_plugin) {
        Ok(final_registry) -> {
          plugin.list_plugins(final_registry)
          |> list.length
          |> should.equal(1)
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn registry_max_plugins_test() {
  let config =
    plugin.RegistryConfig(..plugin.default_registry_config(), max_plugins: 2)
  let registry = plugin.new_registry(config)

  case plugin.register_plugin(registry, create_test_plugin("p1")) {
    Ok(r1) ->
      case plugin.register_plugin(r1, create_test_plugin("p2")) {
        Ok(r2) -> {
          // Third plugin should fail
          case plugin.register_plugin(r2, create_test_plugin("p3")) {
            Ok(_) -> should.fail()
            Error(err) ->
              case err {
                MaxPluginsReached(max) ->
                  max
                  |> should.equal(2)
                _ -> should.fail()
              }
          }
        }
        Error(_) -> should.fail()
      }
    Error(_) -> should.fail()
  }
}

pub fn registry_unregister_plugin_test() {
  let config = plugin.default_registry_config()
  let registry = plugin.new_registry(config)
  let test_plugin = create_test_plugin("to_unregister")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      case plugin.unregister_plugin(updated_registry, "to_unregister") {
        Ok(final_registry) -> {
          plugin.list_plugins(final_registry)
          |> list.length
          |> should.equal(0)
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn registry_unregister_nonexistent_test() {
  let config = plugin.default_registry_config()
  let registry = plugin.new_registry(config)

  case plugin.unregister_plugin(registry, "nonexistent") {
    Ok(_) -> should.fail()
    Error(err) ->
      case err {
        PluginNotFound(id) ->
          id
          |> should.equal("nonexistent")
        _ -> should.fail()
      }
  }
}

// =============================================================================
// Plugin Loader Tests
// =============================================================================

pub fn loader_creation_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let loader = plugin.new_loader(registry, plugin.default_loader_config())

  loader.config.load_timeout_ms
  |> should.equal(5000)
}

pub fn loader_load_plugin_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let test_plugin = create_test_plugin("loadable")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      let loader =
        plugin.new_loader(updated_registry, plugin.default_loader_config())

      case plugin.load_plugin(loader, "loadable") {
        Ok(updated_loader) -> {
          case plugin.get_plugin(updated_loader.registry, "loadable") {
            Some(registered) ->
              case registered.state {
                Active -> should.be_true(True)
                _ -> should.fail()
              }
            None -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn loader_load_nonexistent_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let loader = plugin.new_loader(registry, plugin.default_loader_config())

  case plugin.load_plugin(loader, "nonexistent") {
    Ok(_) -> should.fail()
    Error(err) ->
      case err {
        LoadPluginNotFound(id) ->
          id
          |> should.equal("nonexistent")
        _ -> should.fail()
      }
  }
}

pub fn loader_load_already_active_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let test_plugin = create_test_plugin("active_plugin")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      let loader =
        plugin.new_loader(updated_registry, plugin.default_loader_config())

      case plugin.load_plugin(loader, "active_plugin") {
        Ok(updated_loader) -> {
          // Try to load again
          case plugin.load_plugin(updated_loader, "active_plugin") {
            Ok(_) -> should.fail()
            Error(err) ->
              case err {
                LoadAlreadyActive(id) ->
                  id
                  |> should.equal("active_plugin")
                _ -> should.fail()
              }
          }
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn loader_unload_plugin_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let test_plugin = create_test_plugin("unloadable")

  case plugin.register_plugin(registry, test_plugin) {
    Ok(updated_registry) -> {
      let loader =
        plugin.new_loader(updated_registry, plugin.default_loader_config())

      case plugin.load_plugin(loader, "unloadable") {
        Ok(loaded_loader) -> {
          case plugin.unload_plugin(loaded_loader, "unloadable") {
            Ok(unloaded_loader) -> {
              case plugin.get_plugin(unloaded_loader.registry, "unloadable") {
                Some(registered) ->
                  case registered.state {
                    Registered -> should.be_true(True)
                    _ -> should.fail()
                  }
                None -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Version Management Tests
// =============================================================================

pub fn version_parse_test() {
  case plugin.parse_version("1.2.3") {
    Ok(version) -> {
      version.major
      |> should.equal(1)
      version.minor
      |> should.equal(2)
      version.patch
      |> should.equal(3)
      version.prerelease
      |> should.equal(None)
    }
    Error(_) -> should.fail()
  }
}

pub fn version_parse_with_prerelease_test() {
  case plugin.parse_version("2.0.0-beta") {
    Ok(version) -> {
      version.major
      |> should.equal(2)
      version.patch
      |> should.equal(0)
      version.prerelease
      |> should.equal(Some("beta"))
    }
    Error(_) -> should.fail()
  }
}

pub fn version_parse_short_test() {
  case plugin.parse_version("1.0") {
    Ok(version) -> {
      version.major
      |> should.equal(1)
      version.minor
      |> should.equal(0)
      version.patch
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

pub fn version_compare_test() {
  case plugin.parse_version("1.0.0"), plugin.parse_version("2.0.0") {
    Ok(v1), Ok(v2) -> {
      plugin.compare_versions(v1, v2)
      |> should.equal(order.Lt)

      plugin.compare_versions(v2, v1)
      |> should.equal(order.Gt)

      plugin.compare_versions(v1, v1)
      |> should.equal(order.Eq)
    }
    _, _ -> should.fail()
  }
}

pub fn version_compare_minor_test() {
  case plugin.parse_version("1.2.0"), plugin.parse_version("1.3.0") {
    Ok(v1), Ok(v2) -> {
      plugin.compare_versions(v1, v2)
      |> should.equal(order.Lt)
    }
    _, _ -> should.fail()
  }
}

pub fn version_to_string_test() {
  case plugin.parse_version("1.2.3") {
    Ok(version) -> {
      plugin.version_to_string(version)
      |> should.equal("1.2.3")
    }
    Error(_) -> should.fail()
  }
}

pub fn version_satisfies_exact_test() {
  plugin.version_satisfies("1.0.0", "1.0.0")
  |> should.be_true

  plugin.version_satisfies("1.0.1", "1.0.0")
  |> should.be_false
}

pub fn version_satisfies_gte_test() {
  plugin.version_satisfies("1.0.0", ">=1.0.0")
  |> should.be_true

  plugin.version_satisfies("2.0.0", ">=1.0.0")
  |> should.be_true

  plugin.version_satisfies("0.9.0", ">=1.0.0")
  |> should.be_false
}

pub fn version_satisfies_gt_test() {
  plugin.version_satisfies("1.0.1", ">1.0.0")
  |> should.be_true

  plugin.version_satisfies("1.0.0", ">1.0.0")
  |> should.be_false
}

pub fn version_satisfies_caret_test() {
  // ^1.2.3 means >=1.2.3 and <2.0.0
  plugin.version_satisfies("1.2.3", "^1.2.3")
  |> should.be_true

  plugin.version_satisfies("1.9.0", "^1.2.3")
  |> should.be_true

  plugin.version_satisfies("2.0.0", "^1.2.3")
  |> should.be_false

  plugin.version_satisfies("1.2.2", "^1.2.3")
  |> should.be_false
}

pub fn version_satisfies_tilde_test() {
  // ~1.2.3 means >=1.2.3 and <1.3.0
  plugin.version_satisfies("1.2.3", "~1.2.3")
  |> should.be_true

  plugin.version_satisfies("1.2.9", "~1.2.3")
  |> should.be_true

  plugin.version_satisfies("1.3.0", "~1.2.3")
  |> should.be_false
}

// =============================================================================
// Security Sandbox Tests
// =============================================================================

pub fn sandbox_creation_test() {
  let config = plugin.default_sandbox_config()
  let sandbox = plugin.new_sandbox(config)

  sandbox.resource_usage.memory_bytes
  |> should.equal(0)

  sandbox.violations
  |> list.length
  |> should.equal(0)
}

pub fn sandbox_operation_allowed_test() {
  let config = plugin.default_sandbox_config()
  let sandbox = plugin.new_sandbox(config)

  plugin.is_operation_allowed(sandbox, OpEvaluate)
  |> should.be_true

  plugin.is_operation_allowed(sandbox, OpBuildModel)
  |> should.be_true

  plugin.is_operation_allowed(sandbox, OpProofSearch)
  |> should.be_true
}

pub fn sandbox_operation_not_allowed_test() {
  let config =
    SandboxConfig(..plugin.default_sandbox_config(), allowed_operations: [
      OpEvaluate,
    ])
  let sandbox = plugin.new_sandbox(config)

  plugin.is_operation_allowed(sandbox, OpEvaluate)
  |> should.be_true

  plugin.is_operation_allowed(sandbox, OpBuildModel)
  |> should.be_false
}

pub fn sandbox_custom_operation_test() {
  let config =
    SandboxConfig(..plugin.default_sandbox_config(), allowed_operations: [
      OpCustom("my_operation"),
    ])
  let sandbox = plugin.new_sandbox(config)

  plugin.is_operation_allowed(sandbox, OpCustom("my_operation"))
  |> should.be_true

  plugin.is_operation_allowed(sandbox, OpCustom("other_operation"))
  |> should.be_false
}

pub fn sandbox_check_limits_ok_test() {
  let config = plugin.default_sandbox_config()
  let sandbox = plugin.new_sandbox(config)

  case plugin.check_limits(sandbox) {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.fail()
  }
}

pub fn sandbox_update_usage_test() {
  let config = plugin.default_sandbox_config()
  let sandbox = plugin.new_sandbox(config)

  let updated = plugin.update_usage(sandbox, 1000, 100, 5, 2)

  updated.resource_usage.memory_bytes
  |> should.equal(1000)

  updated.resource_usage.execution_ms
  |> should.equal(100)

  updated.resource_usage.evaluations
  |> should.equal(5)

  updated.resource_usage.worlds_created
  |> should.equal(2)
}

// =============================================================================
// Temporal Logic Plugin Tests
// =============================================================================

pub fn ltl_plugin_creation_test() {
  let ltl = temporal.ltl_plugin()

  ltl.metadata.id
  |> should.equal("temporal_ltl")

  ltl.metadata.name
  |> should.equal("Linear Temporal Logic")

  ltl.logic.id
  |> should.equal("LTL")
}

pub fn ltl_operators_test() {
  let ltl = temporal.ltl_plugin()

  ltl.logic.operators
  |> list.length
  |> should.equal(6)

  // Check for specific operators
  let operator_symbols = list.map(ltl.logic.operators, fn(op) { op.symbol })

  list.contains(operator_symbols, "X")
  |> should.be_true

  list.contains(operator_symbols, "G")
  |> should.be_true

  list.contains(operator_symbols, "F")
  |> should.be_true

  list.contains(operator_symbols, "U")
  |> should.be_true
}

pub fn ctl_plugin_creation_test() {
  let ctl = temporal.ctl_plugin()

  ctl.metadata.id
  |> should.equal("temporal_ctl")

  ctl.logic.id
  |> should.equal("CTL")
}

pub fn ctl_operators_test() {
  let ctl = temporal.ctl_plugin()

  ctl.logic.operators
  |> list.length
  |> should.equal(8)

  let operator_symbols = list.map(ctl.logic.operators, fn(op) { op.symbol })

  // Check for CTL-specific operators
  list.contains(operator_symbols, "AX")
  |> should.be_true

  list.contains(operator_symbols, "EX")
  |> should.be_true

  list.contains(operator_symbols, "AG")
  |> should.be_true

  list.contains(operator_symbols, "EF")
  |> should.be_true
}

pub fn ctl_star_plugin_creation_test() {
  let ctl_star = temporal.ctl_star_plugin()

  ctl_star.metadata.id
  |> should.equal("temporal_ctl_star")

  ctl_star.logic.id
  |> should.equal("CTL*")

  // CTL* depends on LTL and CTL
  ctl_star.metadata.dependencies
  |> list.length
  |> should.equal(2)
}

pub fn all_temporal_plugins_test() {
  let plugins = temporal.all_temporal_plugins()

  plugins
  |> list.length
  |> should.equal(3)
}

pub fn get_temporal_plugin_test() {
  case temporal.get_temporal_plugin("temporal_ltl") {
    Some(ltl) ->
      ltl.metadata.id
      |> should.equal("temporal_ltl")
    None -> should.fail()
  }

  temporal.get_temporal_plugin("nonexistent")
  |> should.equal(None)
}

pub fn register_temporal_plugin_test() {
  let registry = plugin.new_registry(plugin.default_registry_config())
  let ltl = temporal.ltl_plugin()

  case plugin.register_plugin(registry, ltl) {
    Ok(updated_registry) -> {
      plugin.list_plugins(updated_registry)
      |> list.length
      |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Custom Validator Tests
// =============================================================================

pub fn custom_validator_test() {
  let validator =
    CustomValidator(
      name: "test_validator",
      description: "Test validator",
      validate: fn(_formula, _context) {
        Ok(ValidationResult(
          valid: True,
          messages: ["Test passed"],
          data: dict.new(),
        ))
      },
      priority: 10,
    )

  let context =
    plugin.ValidationContext(
      logic: "TEST",
      active_plugins: ["test"],
      context_data: dict.new(),
    )

  case validator.validate(Atom("p"), context) {
    Ok(result) -> {
      result.valid
      |> should.be_true

      result.messages
      |> list.length
      |> should.equal(1)
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Error Message Tests
// =============================================================================

pub fn registry_error_to_string_test() {
  plugin.registry_error_to_string(PluginAlreadyExists("test"))
  |> should.equal("Plugin already exists: test")

  plugin.registry_error_to_string(PluginNotFound("test"))
  |> should.equal("Plugin not found: test")

  plugin.registry_error_to_string(MaxPluginsReached(10))
  |> should.equal("Maximum plugins reached: 10")
}

pub fn load_error_to_string_test() {
  plugin.load_error_to_string(LoadPluginNotFound("test"))
  |> should.equal("Plugin not found: test")

  plugin.load_error_to_string(LoadAlreadyActive("test"))
  |> should.equal("Plugin already active: test")

  plugin.load_error_to_string(LoadHookFailed("test", "error message"))
  |> should.equal("Load hook failed for test: error message")
}

// =============================================================================
// Plugin Discovery Tests
// =============================================================================

pub fn plugin_repository_creation_test() {
  let repo =
    plugin.new_repository("Community Plugins", "https://plugins.example.com")

  repo.name
  |> should.equal("Community Plugins")

  repo.available
  |> list.length
  |> should.equal(0)
}

pub fn plugin_repository_search_test() {
  let manifest =
    plugin.PluginManifest(
      metadata: PluginMetadata(
        id: "temporal_ltl",
        name: "Linear Temporal Logic",
        version: "1.0.0",
        author: "Test",
        description: "LTL plugin for temporal reasoning",
        license: "MIT",
        repository: None,
        dependencies: [],
        min_engine_version: "0.1.0",
      ),
      entry_point: "temporal_ltl.gleam",
      checksum: None,
      signature: None,
    )

  let repo =
    plugin.PluginRepository(
      name: "Test Repo",
      url: "https://test.com",
      available: [manifest],
      last_sync: None,
    )

  // Search by name
  plugin.search_plugins(repo, "temporal")
  |> list.length
  |> should.equal(1)

  // Search by description
  plugin.search_plugins(repo, "LTL")
  |> list.length
  |> should.equal(1)

  // Search with no results
  plugin.search_plugins(repo, "nonexistent")
  |> list.length
  |> should.equal(0)
}

// =============================================================================
// Integration Tests
// =============================================================================

pub fn full_plugin_lifecycle_test() {
  // Create registry
  let registry = plugin.new_registry(plugin.default_registry_config())

  // Register LTL and CTL plugins
  case plugin.register_plugin(registry, temporal.ltl_plugin()) {
    Ok(r1) ->
      case plugin.register_plugin(r1, temporal.ctl_plugin()) {
        Ok(r2) -> {
          // Verify both registered
          plugin.list_plugins(r2)
          |> list.length
          |> should.equal(2)

          // Create loader and load LTL
          let loader = plugin.new_loader(r2, plugin.default_loader_config())
          case plugin.load_plugin(loader, "temporal_ltl") {
            Ok(l1) -> {
              // Verify LTL is active
              plugin.list_active_plugins(l1.registry)
              |> list.length
              |> should.equal(1)

              // Load CTL
              case plugin.load_plugin(l1, "temporal_ctl") {
                Ok(l2) -> {
                  plugin.list_active_plugins(l2.registry)
                  |> list.length
                  |> should.equal(2)

                  // Unload LTL
                  case plugin.unload_plugin(l2, "temporal_ltl") {
                    Ok(l3) -> {
                      plugin.list_active_plugins(l3.registry)
                      |> list.length
                      |> should.equal(1)
                    }
                    Error(_) -> should.fail()
                  }
                }
                Error(_) -> should.fail()
              }
            }
            Error(_) -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }
    Error(_) -> should.fail()
  }
}
