//// Tests for Rule Serialization
////
//// This module tests JSON serialization for rule stores.

import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import modal_logic/proposition.{K, K4, KD, KD45, S4, S5, T}
import modal_logic/rules/axiom
import modal_logic/rules/rule_builder
import modal_logic/rules/rule_serialization.{
  DecodeError, ExportStats, InvalidEnumValue, JsonParseError, MigrationFailed,
  MissingField, SchemaVersion,
}
import modal_logic/rules/rule_store

// ============ Logic System Conversion Tests ============

pub fn logic_system_to_string_test() {
  rule_serialization.logic_system_to_string(K) |> should.equal("K")
  rule_serialization.logic_system_to_string(T) |> should.equal("T")
  rule_serialization.logic_system_to_string(K4) |> should.equal("K4")
  rule_serialization.logic_system_to_string(S4) |> should.equal("S4")
  rule_serialization.logic_system_to_string(S5) |> should.equal("S5")
  rule_serialization.logic_system_to_string(KD) |> should.equal("KD")
  rule_serialization.logic_system_to_string(KD45) |> should.equal("KD45")
}

pub fn parse_logic_system_test() {
  rule_serialization.parse_logic_system("K") |> should.equal(Ok(K))
  rule_serialization.parse_logic_system("T") |> should.equal(Ok(T))
  rule_serialization.parse_logic_system("K4") |> should.equal(Ok(K4))
  rule_serialization.parse_logic_system("S4") |> should.equal(Ok(S4))
  rule_serialization.parse_logic_system("S5") |> should.equal(Ok(S5))
  rule_serialization.parse_logic_system("KD") |> should.equal(Ok(KD))
  rule_serialization.parse_logic_system("KD45") |> should.equal(Ok(KD45))
}

pub fn parse_logic_system_invalid_test() {
  case rule_serialization.parse_logic_system("invalid") {
    Ok(_) -> panic as "Expected InvalidEnumValue error"
    Error(InvalidEnumValue(field, value)) -> {
      field |> should.equal("logic_system")
      value |> should.equal("invalid")
    }
    Error(_) -> panic as "Expected InvalidEnumValue error"
  }
}

pub fn parse_logic_systems_test() {
  rule_serialization.parse_logic_systems(["K", "T", "S5"])
  |> should.equal(Ok([K, T, S5]))
}

pub fn parse_logic_systems_with_invalid_test() {
  case rule_serialization.parse_logic_systems(["K", "invalid", "S5"]) {
    Ok(_) -> panic as "Expected error"
    Error(_) -> should.be_true(True)
  }
}

// ============ Frame Property Conversion Tests ============

pub fn frame_property_to_string_test() {
  rule_serialization.frame_property_to_string(axiom.Reflexive)
  |> should.equal("Reflexive")
  rule_serialization.frame_property_to_string(axiom.Transitive)
  |> should.equal("Transitive")
  rule_serialization.frame_property_to_string(axiom.Symmetric)
  |> should.equal("Symmetric")
  rule_serialization.frame_property_to_string(axiom.Euclidean)
  |> should.equal("Euclidean")
  rule_serialization.frame_property_to_string(axiom.Serial)
  |> should.equal("Serial")
}

pub fn parse_frame_property_test() {
  rule_serialization.parse_frame_property("Reflexive")
  |> should.equal(Ok(axiom.Reflexive))
  rule_serialization.parse_frame_property("Transitive")
  |> should.equal(Ok(axiom.Transitive))
  rule_serialization.parse_frame_property("Symmetric")
  |> should.equal(Ok(axiom.Symmetric))
  rule_serialization.parse_frame_property("Euclidean")
  |> should.equal(Ok(axiom.Euclidean))
  rule_serialization.parse_frame_property("Serial")
  |> should.equal(Ok(axiom.Serial))
}

pub fn parse_frame_property_invalid_test() {
  case rule_serialization.parse_frame_property("Unknown") {
    Ok(_) -> panic as "Expected InvalidEnumValue error"
    Error(InvalidEnumValue(field, value)) -> {
      field |> should.equal("frame_property")
      value |> should.equal("Unknown")
    }
    Error(_) -> panic as "Expected InvalidEnumValue error"
  }
}

// ============ Schema Version Tests ============

pub fn current_schema_version_test() {
  let version = rule_serialization.current_schema_version()
  version.major |> should.equal(1)
  version.minor |> should.equal(0)
  version.patch |> should.equal(0)
}

pub fn format_schema_version_test() {
  let version = SchemaVersion(major: 1, minor: 2, patch: 3)
  rule_serialization.format_schema_version(version)
  |> should.equal("1.2.3")
}

pub fn needs_migration_test() {
  // Version 0 needs migration to version 1
  rule_serialization.needs_migration(0) |> should.be_true

  // Current version doesn't need migration
  rule_serialization.needs_migration(1) |> should.be_false

  // Future version doesn't need migration
  rule_serialization.needs_migration(2) |> should.be_false
}

// ============ Encoding Tests ============

pub fn encode_store_export_test() {
  let store =
    rule_store.new_named("test_store")
    |> rule_store.with_standard_rules

  let export = rule_store.export(store)
  let json_value = rule_serialization.encode_store_export(export)
  let json_str = json.to_string(json_value)

  // Should contain key fields
  json_str |> string.contains("test_store") |> should.be_true
  json_str |> string.contains("rules") |> should.be_true
  json_str |> string.contains("axioms") |> should.be_true
  json_str |> string.contains("schema_version") |> should.be_true
}

pub fn store_export_to_json_test() {
  let store =
    rule_store.new_named("json_test")
    |> rule_store.with_standard_rules

  let export = rule_store.export(store)
  let json_str = rule_serialization.store_export_to_json(export)

  json_str |> string.contains("json_test") |> should.be_true
  json_str |> string.contains("modus_ponens") |> should.be_true
}

pub fn encode_rule_export_test() {
  let rule = rule_builder.modus_ponens()
  let store = rule_store.new()
  let assert Ok(store2) = rule_store.add_rule(store, rule)
  let export = rule_store.export(store2)

  // Should have exactly one rule
  list.length(export.rules) |> should.equal(1)

  let assert [rule_export] = export.rules
  let json_value = rule_serialization.encode_rule_export(rule_export)
  let json_str = json.to_string(json_value)

  json_str |> string.contains("modus_ponens") |> should.be_true
  json_str |> string.contains("Modus Ponens") |> should.be_true
}

pub fn encode_axiom_export_test() {
  let ax = axiom.k_axiom()
  let store = rule_store.new()
  let assert Ok(store2) = rule_store.add_axiom(store, ax)
  let export = rule_store.export(store2)

  list.length(export.axioms) |> should.equal(1)

  let assert [axiom_export] = export.axioms
  let json_value = rule_serialization.encode_axiom_export(axiom_export)
  let json_str = json.to_string(json_value)

  // Check for axiom id and name - JSON escapes quotes
  json_str |> string.contains("\"id\":\"K\"") |> should.be_true
  json_str |> string.contains("Distribution Axiom") |> should.be_true
}

pub fn encode_rule_set_test() {
  let store = rule_store.new()
  let rule = rule_builder.modus_ponens()

  let assert Ok(store2) = rule_store.add_rule(store, rule)
  let assert Ok(store3) =
    rule_store.create_rule_set(
      store2,
      "test_set",
      "Test Set",
      "A test rule set",
    )
  let assert Ok(store4) =
    rule_store.add_rule_to_set(store3, "test_set", "modus_ponens")

  let export = rule_store.export(store4)
  list.length(export.rule_sets) |> should.equal(1)

  let assert [rule_set] = export.rule_sets
  let json_value = rule_serialization.encode_rule_set(rule_set)
  let json_str = json.to_string(json_value)

  json_str |> string.contains("test_set") |> should.be_true
  json_str |> string.contains("Test Set") |> should.be_true
  json_str |> string.contains("modus_ponens") |> should.be_true
}

// ============ Validation Tests ============

pub fn validate_export_test() {
  let store =
    rule_store.new()
    |> rule_store.with_standard_rules
    |> rule_store.with_standard_axioms

  let export = rule_store.export(store)
  let result = rule_serialization.validate_export(export)

  case result {
    Ok(_) -> should.be_true(True)
    Error(err) -> panic as rule_serialization.format_error(err)
  }
}

// ============ Export Stats Tests ============

pub fn export_stats_test() {
  let store =
    rule_store.new()
    |> rule_store.with_standard_rules
    |> rule_store.with_standard_axioms

  let export = rule_store.export(store)
  let stats = rule_serialization.export_stats(export)

  stats.total_rules |> should.not_equal(0)
  stats.active_rules |> should.equal(stats.total_rules)
  stats.total_axioms |> should.not_equal(0)
  stats.active_axioms |> should.equal(stats.total_axioms)
}

pub fn format_export_stats_test() {
  let stats =
    ExportStats(
      total_rules: 5,
      active_rules: 4,
      total_axioms: 3,
      active_axioms: 3,
      total_rule_sets: 1,
      unique_tags: ["modal", "classical"],
    )

  let formatted = rule_serialization.format_export_stats(stats)

  formatted |> string.contains("Rules: 4/5 active") |> should.be_true
  formatted |> string.contains("Axioms: 3/3 active") |> should.be_true
  formatted |> string.contains("Rule Sets: 1") |> should.be_true
  formatted |> string.contains("modal") |> should.be_true
  formatted |> string.contains("classical") |> should.be_true
}

// ============ Error Formatting Tests ============

pub fn format_error_json_parse_test() {
  let err = JsonParseError("Unexpected token")
  rule_serialization.format_error(err)
  |> should.equal("JSON parse error: Unexpected token")
}

pub fn format_error_decode_test() {
  let err = DecodeError(field: "name", message: "Expected string")
  rule_serialization.format_error(err)
  |> should.equal("Decode error in field 'name': Expected string")
}

pub fn format_error_missing_field_test() {
  let err = MissingField("id")
  rule_serialization.format_error(err)
  |> should.equal("Missing required field: id")
}

pub fn format_error_invalid_enum_test() {
  let err = InvalidEnumValue(field: "system", value: "X")
  rule_serialization.format_error(err)
  |> should.equal("Invalid value 'X' for field 'system'")
}

// ============ Migration Tests ============

pub fn migrate_export_current_version_test() {
  let store = rule_store.new()
  let export = rule_store.export(store)

  // Current version should not need migration
  case rule_serialization.migrate_export(export, 1) {
    Ok(migrated) -> migrated.name |> should.equal(export.name)
    Error(_) -> panic as "Expected successful no-op migration"
  }
}

pub fn migrate_export_from_v0_test() {
  let store = rule_store.new_named("v0_store")
  let export = rule_store.export(store)

  // Migrate from version 0 to current
  case rule_serialization.migrate_export(export, 0) {
    Ok(migrated) -> {
      migrated.name |> should.equal("v0_store")
      migrated.version |> should.equal(1)
    }
    Error(_) -> panic as "Expected successful migration"
  }
}

pub fn migrate_export_future_version_test() {
  let store = rule_store.new()
  let export = rule_store.export(store)

  // Future version should fail
  case rule_serialization.migrate_export(export, 99) {
    Ok(_) -> panic as "Expected migration failure for future version"
    Error(MigrationFailed(reason)) ->
      reason |> string.contains("downgrade") |> should.be_true
    Error(_) -> panic as "Expected MigrationFailed error"
  }
}
