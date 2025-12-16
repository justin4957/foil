//// Rule Serialization
////
//// This module provides JSON serialization for inference rules,
//// axioms, and rule stores.

import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/proposition.{type LogicSystem, K, K4, KD, KD45, S4, S5, T}
import modal_logic/rules/axiom.{type FrameProperty}
import modal_logic/rules/rule_store.{
  type AxiomExport, type RuleExport, type RuleSet, type StoreExport, StoreExport,
}

// ============ JSON Encoding ============

/// Encode a StoreExport to JSON
pub fn encode_store_export(export: StoreExport) -> Json {
  json.object([
    #("name", json.string(export.name)),
    #("version", json.int(export.version)),
    #("schema_version", encode_schema_version(current_schema_version())),
    #("rules", json.array(export.rules, encode_rule_export)),
    #("axioms", json.array(export.axioms, encode_axiom_export)),
    #("rule_sets", json.array(export.rule_sets, encode_rule_set)),
  ])
}

/// Encode a RuleExport to JSON
pub fn encode_rule_export(rule: RuleExport) -> Json {
  json.object([
    #("id", json.string(rule.id)),
    #("name", json.string(rule.name)),
    #("description", json.string(rule.description)),
    #("version", json.int(rule.version)),
    #("is_active", json.bool(rule.is_active)),
    #("tags", json.array(rule.tags, json.string)),
    #("valid_in", json.array(rule.valid_in, json.string)),
  ])
}

/// Encode an AxiomExport to JSON
pub fn encode_axiom_export(axiom: AxiomExport) -> Json {
  json.object([
    #("id", json.string(axiom.id)),
    #("name", json.string(axiom.name)),
    #("description", json.string(axiom.description)),
    #("version", json.int(axiom.version)),
    #("is_active", json.bool(axiom.is_active)),
    #("included_in", json.array(axiom.included_in, json.string)),
  ])
}

/// Encode a RuleSet to JSON
pub fn encode_rule_set(rule_set: RuleSet) -> Json {
  json.object([
    #("id", json.string(rule_set.id)),
    #("name", json.string(rule_set.name)),
    #("description", json.string(rule_set.description)),
    #("rule_ids", json.array(rule_set.rule_ids, json.string)),
    #("axiom_ids", json.array(rule_set.axiom_ids, json.string)),
    #("tags", json.array(rule_set.tags, json.string)),
    #("target_system", case rule_set.target_system {
      Some(sys) -> json.string(logic_system_to_string(sys))
      None -> json.null()
    }),
  ])
}

/// Convert StoreExport to JSON string
pub fn store_export_to_json(export: StoreExport) -> String {
  encode_store_export(export)
  |> json.to_string
}

// ============ Serialization Error ============

/// Serialization error type
pub type SerializationError {
  /// JSON parsing failed
  JsonParseError(message: String)
  /// Field decoding failed
  DecodeError(field: String, message: String)
  /// Missing required field
  MissingField(field: String)
  /// Invalid enum value
  InvalidEnumValue(field: String, value: String)
}

/// Format serialization error
pub fn format_error(error: SerializationError) -> String {
  case error {
    JsonParseError(message) -> "JSON parse error: " <> message
    DecodeError(field, message) ->
      "Decode error in field '" <> field <> "': " <> message
    MissingField(field) -> "Missing required field: " <> field
    InvalidEnumValue(field, value) ->
      "Invalid value '" <> value <> "' for field '" <> field <> "'"
  }
}

// ============ Logic System Conversion ============

/// Parse logic system from string
pub fn parse_logic_system(s: String) -> Result(LogicSystem, SerializationError) {
  case s {
    "K" -> Ok(K)
    "T" -> Ok(T)
    "K4" -> Ok(K4)
    "S4" -> Ok(S4)
    "S5" -> Ok(S5)
    "KD" -> Ok(KD)
    "KD45" -> Ok(KD45)
    _ -> Error(InvalidEnumValue(field: "logic_system", value: s))
  }
}

/// Convert logic system to string
pub fn logic_system_to_string(system: LogicSystem) -> String {
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

/// Parse multiple logic systems
pub fn parse_logic_systems(
  strings: List(String),
) -> Result(List(LogicSystem), SerializationError) {
  list.try_map(strings, parse_logic_system)
}

// ============ Frame Property Conversion ============

/// Parse frame property from string
pub fn parse_frame_property(
  s: String,
) -> Result(FrameProperty, SerializationError) {
  case s {
    "Reflexive" -> Ok(axiom.Reflexive)
    "Transitive" -> Ok(axiom.Transitive)
    "Symmetric" -> Ok(axiom.Symmetric)
    "Euclidean" -> Ok(axiom.Euclidean)
    "Serial" -> Ok(axiom.Serial)
    _ -> Error(InvalidEnumValue(field: "frame_property", value: s))
  }
}

/// Convert frame property to string
pub fn frame_property_to_string(prop: FrameProperty) -> String {
  case prop {
    axiom.Reflexive -> "Reflexive"
    axiom.Transitive -> "Transitive"
    axiom.Symmetric -> "Symmetric"
    axiom.Euclidean -> "Euclidean"
    axiom.Serial -> "Serial"
  }
}

// ============ Migration Support ============

/// Schema version for migration
pub type SchemaVersion {
  SchemaVersion(major: Int, minor: Int, patch: Int)
}

/// Current schema version
pub fn current_schema_version() -> SchemaVersion {
  SchemaVersion(major: 1, minor: 0, patch: 0)
}

/// Migration result
pub type MigrationResult {
  /// No migration needed
  NoMigrationNeeded
  /// Migration successful
  MigrationSuccessful(from_version: SchemaVersion, to_version: SchemaVersion)
  /// Migration failed
  MigrationFailed(reason: String)
}

/// Check if migration is needed
pub fn needs_migration(data_version: Int) -> Bool {
  let current = current_schema_version()
  data_version < current.major
}

/// Migrate export data to current version
pub fn migrate_export(
  export: StoreExport,
  from_version: Int,
) -> Result(StoreExport, MigrationResult) {
  let current = current_schema_version()

  case from_version {
    v if v == current.major -> Ok(export)
    v if v < current.major -> {
      // Apply migrations sequentially
      migrate_from_version(export, v, current.major)
    }
    _ -> Error(MigrationFailed(reason: "Cannot downgrade from future version"))
  }
}

/// Apply migrations from one version to another
fn migrate_from_version(
  export: StoreExport,
  from: Int,
  to: Int,
) -> Result(StoreExport, MigrationResult) {
  case from {
    // Version 0 to 1: No structural changes, just update version
    0 -> {
      let updated = StoreExport(..export, version: 1)
      case to {
        1 -> Ok(updated)
        _ -> migrate_from_version(updated, 1, to)
      }
    }
    // Add future migration handlers here
    _ -> Ok(export)
  }
}

/// Encode schema version to JSON
pub fn encode_schema_version(version: SchemaVersion) -> Json {
  json.object([
    #("major", json.int(version.major)),
    #("minor", json.int(version.minor)),
    #("patch", json.int(version.patch)),
  ])
}

/// Format schema version as string
pub fn format_schema_version(version: SchemaVersion) -> String {
  int.to_string(version.major)
  <> "."
  <> int.to_string(version.minor)
  <> "."
  <> int.to_string(version.patch)
}

// ============ Validation ============

/// Validate a StoreExport
pub fn validate_export(
  export: StoreExport,
) -> Result(StoreExport, SerializationError) {
  // Check for duplicate rule IDs
  let rule_ids = list.map(export.rules, fn(r) { r.id })
  case has_duplicates(rule_ids) {
    True ->
      Error(DecodeError(field: "rules", message: "Duplicate rule IDs found"))
    False -> {
      // Check for duplicate axiom IDs
      let axiom_ids = list.map(export.axioms, fn(a) { a.id })
      case has_duplicates(axiom_ids) {
        True ->
          Error(DecodeError(
            field: "axioms",
            message: "Duplicate axiom IDs found",
          ))
        False -> Ok(export)
      }
    }
  }
}

/// Check if a list has duplicates
fn has_duplicates(items: List(String)) -> Bool {
  let unique_count =
    items
    |> list.unique
    |> list.length

  unique_count != list.length(items)
}

// ============ Export Utilities ============

/// Get summary statistics from export
pub fn export_stats(export: StoreExport) -> ExportStats {
  let active_rules =
    export.rules
    |> list.filter(fn(r) { r.is_active })
    |> list.length

  let active_axioms =
    export.axioms
    |> list.filter(fn(a) { a.is_active })
    |> list.length

  let all_tags =
    export.rules
    |> list.flat_map(fn(r) { r.tags })
    |> list.unique

  ExportStats(
    total_rules: list.length(export.rules),
    active_rules: active_rules,
    total_axioms: list.length(export.axioms),
    active_axioms: active_axioms,
    total_rule_sets: list.length(export.rule_sets),
    unique_tags: all_tags,
  )
}

/// Export statistics
pub type ExportStats {
  ExportStats(
    total_rules: Int,
    active_rules: Int,
    total_axioms: Int,
    active_axioms: Int,
    total_rule_sets: Int,
    unique_tags: List(String),
  )
}

/// Format export stats as string
pub fn format_export_stats(stats: ExportStats) -> String {
  string.concat([
    "Rules: ",
    int.to_string(stats.active_rules),
    "/",
    int.to_string(stats.total_rules),
    " active\n",
    "Axioms: ",
    int.to_string(stats.active_axioms),
    "/",
    int.to_string(stats.total_axioms),
    " active\n",
    "Rule Sets: ",
    int.to_string(stats.total_rule_sets),
    "\n",
    "Tags: ",
    string.join(stats.unique_tags, ", "),
    "\n",
  ])
}
