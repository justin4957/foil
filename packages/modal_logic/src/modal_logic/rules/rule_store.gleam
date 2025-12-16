//// Rule Store with Versioning
////
//// This module provides persistent storage for inference rules and axioms
//// with support for versioning, history tracking, and multiple backends.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None}
import gleam/string
import modal_logic/proposition.{type LogicSystem}
import modal_logic/rules/axiom.{type Axiom}
import modal_logic/rules/inference_rule.{type InferenceRule}
import modal_logic/rules/rule_builder

// ============ Core Types ============

/// A versioned rule entry
pub type VersionedRule {
  VersionedRule(
    /// The current rule
    rule: InferenceRule,
    /// Version number (starts at 1)
    version: Int,
    /// Previous versions (most recent first)
    history: List(RuleVersion),
    /// When the rule was created
    created_at: String,
    /// When the rule was last updated
    updated_at: String,
    /// Whether the rule is active (soft delete)
    is_active: Bool,
  )
}

/// A historical version of a rule
pub type RuleVersion {
  RuleVersion(
    /// The rule at this version
    rule: InferenceRule,
    /// Version number
    version: Int,
    /// When this version was created
    timestamp: String,
    /// Description of changes
    change_description: Option(String),
  )
}

/// A versioned axiom entry
pub type VersionedAxiom {
  VersionedAxiom(
    /// The current axiom
    axiom: Axiom,
    /// Version number
    version: Int,
    /// Previous versions
    history: List(AxiomVersion),
    /// When created
    created_at: String,
    /// When last updated
    updated_at: String,
    /// Whether active
    is_active: Bool,
  )
}

/// A historical version of an axiom
pub type AxiomVersion {
  AxiomVersion(
    /// The axiom at this version
    axiom: Axiom,
    /// Version number
    version: Int,
    /// Timestamp
    timestamp: String,
    /// Change description
    change_description: Option(String),
  )
}

/// A named collection of related rules
pub type RuleSet {
  RuleSet(
    /// Set identifier
    id: String,
    /// Human-readable name
    name: String,
    /// Description
    description: String,
    /// Rule IDs in this set
    rule_ids: List(String),
    /// Axiom IDs in this set
    axiom_ids: List(String),
    /// Logic system this set targets
    target_system: Option(LogicSystem),
    /// Tags
    tags: List(String),
  )
}

/// The rule store
pub type RuleStore {
  RuleStore(
    /// Versioned rules by ID
    rules: Dict(String, VersionedRule),
    /// Versioned axioms by ID
    axioms: Dict(String, VersionedAxiom),
    /// Rule sets by ID
    rule_sets: Dict(String, RuleSet),
    /// Store metadata
    metadata: StoreMetadata,
  )
}

/// Store metadata
pub type StoreMetadata {
  StoreMetadata(
    /// Store name
    name: String,
    /// Store version
    version: Int,
    /// When created
    created_at: String,
    /// When last modified
    last_modified: String,
    /// Total rule count (including inactive)
    total_rules: Int,
    /// Active rule count
    active_rules: Int,
    /// Total axiom count
    total_axioms: Int,
    /// Active axiom count
    active_axioms: Int,
  )
}

/// Result type for store operations
pub type StoreResult(a) {
  StoreOk(value: a)
  StoreError(error: StoreError)
}

/// Store error types
pub type StoreError {
  /// Rule not found
  RuleNotFound(id: String)
  /// Axiom not found
  AxiomNotFound(id: String)
  /// Rule set not found
  RuleSetNotFound(id: String)
  /// Duplicate ID
  DuplicateId(id: String, entity_type: String)
  /// Version conflict
  VersionConflict(id: String, expected: Int, actual: Int)
  /// Validation error
  ValidationError(message: String)
  /// Storage backend error
  StorageError(message: String)
}

// ============ Store Operations ============

/// Create a new empty rule store
pub fn new() -> RuleStore {
  RuleStore(
    rules: dict.new(),
    axioms: dict.new(),
    rule_sets: dict.new(),
    metadata: StoreMetadata(
      name: "default",
      version: 1,
      created_at: "now",
      last_modified: "now",
      total_rules: 0,
      active_rules: 0,
      total_axioms: 0,
      active_axioms: 0,
    ),
  )
}

/// Create a named rule store
pub fn new_named(name: String) -> RuleStore {
  let store = new()
  RuleStore(..store, metadata: StoreMetadata(..store.metadata, name: name))
}

// ============ Rule Operations ============

/// Add a new rule to the store
pub fn add_rule(
  store: RuleStore,
  rule: InferenceRule,
) -> Result(RuleStore, StoreError) {
  case dict.get(store.rules, rule.id) {
    Ok(_) -> Error(DuplicateId(id: rule.id, entity_type: "rule"))
    Error(_) -> {
      let versioned =
        VersionedRule(
          rule: rule,
          version: 1,
          history: [],
          created_at: "now",
          updated_at: "now",
          is_active: True,
        )

      let new_rules = dict.insert(store.rules, rule.id, versioned)
      let new_metadata =
        StoreMetadata(
          ..store.metadata,
          total_rules: store.metadata.total_rules + 1,
          active_rules: store.metadata.active_rules + 1,
          last_modified: "now",
        )

      Ok(RuleStore(..store, rules: new_rules, metadata: new_metadata))
    }
  }
}

/// Add multiple rules at once
pub fn add_rules(
  store: RuleStore,
  rules: List(InferenceRule),
) -> Result(RuleStore, StoreError) {
  list.fold(rules, Ok(store), fn(acc, rule) {
    case acc {
      Ok(s) -> add_rule(s, rule)
      Error(e) -> Error(e)
    }
  })
}

/// Get a rule by ID
pub fn get_rule(
  store: RuleStore,
  id: String,
) -> Result(InferenceRule, StoreError) {
  case dict.get(store.rules, id) {
    Ok(versioned) ->
      case versioned.is_active {
        True -> Ok(versioned.rule)
        False -> Error(RuleNotFound(id: id))
      }
    Error(_) -> Error(RuleNotFound(id: id))
  }
}

/// Get a versioned rule entry
pub fn get_versioned_rule(
  store: RuleStore,
  id: String,
) -> Result(VersionedRule, StoreError) {
  case dict.get(store.rules, id) {
    Ok(versioned) -> Ok(versioned)
    Error(_) -> Error(RuleNotFound(id: id))
  }
}

/// Update an existing rule (creates new version)
pub fn update_rule(
  store: RuleStore,
  rule: InferenceRule,
  change_description: Option(String),
) -> Result(RuleStore, StoreError) {
  case dict.get(store.rules, rule.id) {
    Error(_) -> Error(RuleNotFound(id: rule.id))
    Ok(versioned) -> {
      // Create history entry for current version
      let history_entry =
        RuleVersion(
          rule: versioned.rule,
          version: versioned.version,
          timestamp: versioned.updated_at,
          change_description: change_description,
        )

      // Create new versioned entry
      let new_versioned =
        VersionedRule(
          rule: rule,
          version: versioned.version + 1,
          history: [history_entry, ..versioned.history],
          created_at: versioned.created_at,
          updated_at: "now",
          is_active: True,
        )

      let new_rules = dict.insert(store.rules, rule.id, new_versioned)
      let new_metadata = StoreMetadata(..store.metadata, last_modified: "now")

      Ok(RuleStore(..store, rules: new_rules, metadata: new_metadata))
    }
  }
}

/// Soft delete a rule
pub fn delete_rule(
  store: RuleStore,
  id: String,
) -> Result(RuleStore, StoreError) {
  case dict.get(store.rules, id) {
    Error(_) -> Error(RuleNotFound(id: id))
    Ok(versioned) ->
      case versioned.is_active {
        False -> Error(RuleNotFound(id: id))
        True -> {
          let new_versioned = VersionedRule(..versioned, is_active: False)
          let new_rules = dict.insert(store.rules, id, new_versioned)
          let new_metadata =
            StoreMetadata(
              ..store.metadata,
              active_rules: store.metadata.active_rules - 1,
              last_modified: "now",
            )

          Ok(RuleStore(..store, rules: new_rules, metadata: new_metadata))
        }
      }
  }
}

/// Restore a soft-deleted rule
pub fn restore_rule(
  store: RuleStore,
  id: String,
) -> Result(RuleStore, StoreError) {
  case dict.get(store.rules, id) {
    Error(_) -> Error(RuleNotFound(id: id))
    Ok(versioned) ->
      case versioned.is_active {
        True -> Ok(store)
        // Already active
        False -> {
          let new_versioned = VersionedRule(..versioned, is_active: True)
          let new_rules = dict.insert(store.rules, id, new_versioned)
          let new_metadata =
            StoreMetadata(
              ..store.metadata,
              active_rules: store.metadata.active_rules + 1,
              last_modified: "now",
            )

          Ok(RuleStore(..store, rules: new_rules, metadata: new_metadata))
        }
      }
  }
}

/// Get a specific version of a rule
pub fn get_rule_version(
  store: RuleStore,
  id: String,
  version: Int,
) -> Result(InferenceRule, StoreError) {
  case dict.get(store.rules, id) {
    Error(_) -> Error(RuleNotFound(id: id))
    Ok(versioned) -> {
      case version == versioned.version {
        True -> Ok(versioned.rule)
        False -> {
          // Search history
          let found =
            list.find(versioned.history, fn(rv) { rv.version == version })
          case found {
            Ok(rv) -> Ok(rv.rule)
            Error(_) ->
              Error(RuleNotFound(id: id <> "@v" <> int.to_string(version)))
          }
        }
      }
    }
  }
}

/// List all active rules
pub fn list_rules(store: RuleStore) -> List(InferenceRule) {
  store.rules
  |> dict.values
  |> list.filter(fn(v) { v.is_active })
  |> list.map(fn(v) { v.rule })
}

/// List all rules (including inactive)
pub fn list_all_rules(store: RuleStore) -> List(VersionedRule) {
  store.rules
  |> dict.values
}

/// List rules by tag
pub fn list_rules_by_tag(store: RuleStore, tag: String) -> List(InferenceRule) {
  list_rules(store)
  |> list.filter(fn(rule) { list.contains(rule.metadata.tags, tag) })
}

/// List rules valid in a specific logic system
pub fn list_rules_for_system(
  store: RuleStore,
  system: LogicSystem,
) -> List(InferenceRule) {
  list_rules(store)
  |> list.filter(fn(rule) { list.contains(rule.valid_in, system) })
}

// ============ Axiom Operations ============

/// Add an axiom to the store
pub fn add_axiom(store: RuleStore, ax: Axiom) -> Result(RuleStore, StoreError) {
  case dict.get(store.axioms, ax.id) {
    Ok(_) -> Error(DuplicateId(id: ax.id, entity_type: "axiom"))
    Error(_) -> {
      let versioned =
        VersionedAxiom(
          axiom: ax,
          version: 1,
          history: [],
          created_at: "now",
          updated_at: "now",
          is_active: True,
        )

      let new_axioms = dict.insert(store.axioms, ax.id, versioned)
      let new_metadata =
        StoreMetadata(
          ..store.metadata,
          total_axioms: store.metadata.total_axioms + 1,
          active_axioms: store.metadata.active_axioms + 1,
          last_modified: "now",
        )

      Ok(RuleStore(..store, axioms: new_axioms, metadata: new_metadata))
    }
  }
}

/// Add multiple axioms
pub fn add_axioms(
  store: RuleStore,
  axioms: List(Axiom),
) -> Result(RuleStore, StoreError) {
  list.fold(axioms, Ok(store), fn(acc, ax) {
    case acc {
      Ok(s) -> add_axiom(s, ax)
      Error(e) -> Error(e)
    }
  })
}

/// Get an axiom by ID
pub fn get_axiom(store: RuleStore, id: String) -> Result(Axiom, StoreError) {
  case dict.get(store.axioms, id) {
    Ok(versioned) ->
      case versioned.is_active {
        True -> Ok(versioned.axiom)
        False -> Error(AxiomNotFound(id: id))
      }
    Error(_) -> Error(AxiomNotFound(id: id))
  }
}

/// List all active axioms
pub fn list_axioms(store: RuleStore) -> List(Axiom) {
  store.axioms
  |> dict.values
  |> list.filter(fn(v) { v.is_active })
  |> list.map(fn(v) { v.axiom })
}

/// List axioms for a logic system
pub fn list_axioms_for_system(
  store: RuleStore,
  system: LogicSystem,
) -> List(Axiom) {
  list_axioms(store)
  |> list.filter(fn(ax) { list.contains(ax.included_in, system) })
}

// ============ Rule Set Operations ============

/// Create a new rule set
pub fn create_rule_set(
  store: RuleStore,
  id: String,
  name: String,
  description: String,
) -> Result(RuleStore, StoreError) {
  case dict.get(store.rule_sets, id) {
    Ok(_) -> Error(DuplicateId(id: id, entity_type: "rule_set"))
    Error(_) -> {
      let rule_set =
        RuleSet(
          id: id,
          name: name,
          description: description,
          rule_ids: [],
          axiom_ids: [],
          target_system: None,
          tags: [],
        )

      let new_sets = dict.insert(store.rule_sets, id, rule_set)
      Ok(RuleStore(..store, rule_sets: new_sets))
    }
  }
}

/// Add a rule to a rule set
pub fn add_rule_to_set(
  store: RuleStore,
  set_id: String,
  rule_id: String,
) -> Result(RuleStore, StoreError) {
  case dict.get(store.rule_sets, set_id) {
    Error(_) -> Error(RuleSetNotFound(id: set_id))
    Ok(rule_set) -> {
      // Verify rule exists
      case dict.get(store.rules, rule_id) {
        Error(_) -> Error(RuleNotFound(id: rule_id))
        Ok(_) -> {
          let new_set =
            RuleSet(
              ..rule_set,
              rule_ids: list.unique([rule_id, ..rule_set.rule_ids]),
            )
          let new_sets = dict.insert(store.rule_sets, set_id, new_set)
          Ok(RuleStore(..store, rule_sets: new_sets))
        }
      }
    }
  }
}

/// Add an axiom to a rule set
pub fn add_axiom_to_set(
  store: RuleStore,
  set_id: String,
  axiom_id: String,
) -> Result(RuleStore, StoreError) {
  case dict.get(store.rule_sets, set_id) {
    Error(_) -> Error(RuleSetNotFound(id: set_id))
    Ok(rule_set) -> {
      case dict.get(store.axioms, axiom_id) {
        Error(_) -> Error(AxiomNotFound(id: axiom_id))
        Ok(_) -> {
          let new_set =
            RuleSet(
              ..rule_set,
              axiom_ids: list.unique([axiom_id, ..rule_set.axiom_ids]),
            )
          let new_sets = dict.insert(store.rule_sets, set_id, new_set)
          Ok(RuleStore(..store, rule_sets: new_sets))
        }
      }
    }
  }
}

/// Get a rule set
pub fn get_rule_set(store: RuleStore, id: String) -> Result(RuleSet, StoreError) {
  case dict.get(store.rule_sets, id) {
    Ok(set) -> Ok(set)
    Error(_) -> Error(RuleSetNotFound(id: id))
  }
}

/// Get all rules in a rule set
pub fn get_rules_in_set(
  store: RuleStore,
  set_id: String,
) -> Result(List(InferenceRule), StoreError) {
  case dict.get(store.rule_sets, set_id) {
    Error(_) -> Error(RuleSetNotFound(id: set_id))
    Ok(rule_set) -> {
      let rules =
        rule_set.rule_ids
        |> list.filter_map(fn(id) { get_rule(store, id) })
      Ok(rules)
    }
  }
}

/// List all rule sets
pub fn list_rule_sets(store: RuleStore) -> List(RuleSet) {
  store.rule_sets
  |> dict.values
}

// ============ Query Operations ============

/// Search rules by name or description
pub fn search_rules(store: RuleStore, query: String) -> List(InferenceRule) {
  let lower_query = string.lowercase(query)

  list_rules(store)
  |> list.filter(fn(rule) {
    string.contains(string.lowercase(rule.name), lower_query)
    || string.contains(string.lowercase(rule.description), lower_query)
  })
}

/// Count rules by tag
pub fn count_rules_by_tag(store: RuleStore) -> Dict(String, Int) {
  list_rules(store)
  |> list.flat_map(fn(rule) { rule.metadata.tags })
  |> list.group(fn(tag) { tag })
  |> dict.map_values(fn(_tag, tags) { list.length(tags) })
}

// ============ Store Statistics ============

/// Get store statistics
pub fn statistics(store: RuleStore) -> StoreStatistics {
  let rules = list_all_rules(store)
  let active_rules = list.filter(rules, fn(v) { v.is_active })

  let total_versions =
    list.fold(rules, 0, fn(acc, v) { acc + 1 + list.length(v.history) })

  StoreStatistics(
    total_rules: list.length(rules),
    active_rules: list.length(active_rules),
    total_axioms: list.length(dict.values(store.axioms)),
    active_axioms: list.length(list_axioms(store)),
    total_rule_sets: list.length(dict.values(store.rule_sets)),
    total_versions: total_versions,
    rules_by_tag: count_rules_by_tag(store),
  )
}

/// Store statistics summary
pub type StoreStatistics {
  StoreStatistics(
    total_rules: Int,
    active_rules: Int,
    total_axioms: Int,
    active_axioms: Int,
    total_rule_sets: Int,
    total_versions: Int,
    rules_by_tag: Dict(String, Int),
  )
}

// ============ Export/Import ============

/// Export store to a serializable format
pub type StoreExport {
  StoreExport(
    /// Store name
    name: String,
    /// Export version
    version: Int,
    /// Exported rules with metadata
    rules: List(RuleExport),
    /// Exported axioms
    axioms: List(AxiomExport),
    /// Exported rule sets
    rule_sets: List(RuleSet),
  )
}

/// Exported rule format
pub type RuleExport {
  RuleExport(
    id: String,
    name: String,
    description: String,
    version: Int,
    is_active: Bool,
    tags: List(String),
    valid_in: List(String),
  )
}

/// Exported axiom format
pub type AxiomExport {
  AxiomExport(
    id: String,
    name: String,
    description: String,
    version: Int,
    is_active: Bool,
    included_in: List(String),
  )
}

/// Export the store
pub fn export(store: RuleStore) -> StoreExport {
  let rules =
    list_all_rules(store)
    |> list.map(fn(v) {
      RuleExport(
        id: v.rule.id,
        name: v.rule.name,
        description: v.rule.description,
        version: v.version,
        is_active: v.is_active,
        tags: v.rule.metadata.tags,
        valid_in: list.map(v.rule.valid_in, logic_system_to_string),
      )
    })

  let axioms =
    store.axioms
    |> dict.values
    |> list.map(fn(v) {
      AxiomExport(
        id: v.axiom.id,
        name: v.axiom.name,
        description: v.axiom.description,
        version: v.version,
        is_active: v.is_active,
        included_in: list.map(v.axiom.included_in, logic_system_to_string),
      )
    })

  StoreExport(
    name: store.metadata.name,
    version: store.metadata.version,
    rules: rules,
    axioms: axioms,
    rule_sets: list_rule_sets(store),
  )
}

/// Format store export as summary string
pub fn export_summary(export: StoreExport) -> String {
  string.concat([
    "Store: ",
    export.name,
    " (v",
    int.to_string(export.version),
    ")\n",
    "Rules: ",
    int.to_string(list.length(export.rules)),
    "\n",
    "Axioms: ",
    int.to_string(list.length(export.axioms)),
    "\n",
    "Rule Sets: ",
    int.to_string(list.length(export.rule_sets)),
    "\n",
  ])
}

// ============ Utility Functions ============

/// Convert logic system to string
fn logic_system_to_string(system: LogicSystem) -> String {
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

/// Format store error as string
pub fn format_error(error: StoreError) -> String {
  case error {
    RuleNotFound(id) -> "Rule not found: " <> id
    AxiomNotFound(id) -> "Axiom not found: " <> id
    RuleSetNotFound(id) -> "Rule set not found: " <> id
    DuplicateId(id, entity_type) -> "Duplicate " <> entity_type <> " ID: " <> id
    VersionConflict(id, expected, actual) ->
      "Version conflict for "
      <> id
      <> ": expected v"
      <> int.to_string(expected)
      <> ", got v"
      <> int.to_string(actual)
    ValidationError(msg) -> "Validation error: " <> msg
    StorageError(msg) -> "Storage error: " <> msg
  }
}

// ============ Pre-populated Stores ============

/// Create a store with all standard inference rules
pub fn with_standard_rules(store: RuleStore) -> RuleStore {
  // Import the pre-built rules
  let all = rule_builder.all_rules()

  case add_rules(store, all) {
    Ok(s) -> s
    Error(_) -> store
  }
}

/// Create a store with all standard axioms
pub fn with_standard_axioms(store: RuleStore) -> RuleStore {
  let all = axiom.all_standard_axioms()

  case add_axioms(store, all) {
    Ok(s) -> s
    Error(_) -> store
  }
}

/// Create a fully populated standard store
pub fn standard_store() -> RuleStore {
  new_named("standard")
  |> with_standard_rules
  |> with_standard_axioms
}
