//// Repository Layer
////
//// This module provides data access operations for arguments, formalizations,
//// validations, and repair suggestions using PostgreSQL via gleam_pgo.
////
//// ## Features
////
//// - CRUD operations for all domain types
//// - Transaction support for complex operations
//// - Query builders for flexible searching
//// - Efficient batch operations
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/repository
////
//// let conn = repository.connect(config)
//// let arg = repository.get_argument(conn, "arg-1")
//// ```

import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import modal_logic/argument.{
  type Ambiguity, type Argument, type Formalization, type RepairSuggestion,
  type RepairType, type ValidationRecord, type ValidationResult,
}
import modal_logic/proposition.{type LogicSystem, type Proposition}

// =============================================================================
// Types
// =============================================================================

/// Database connection configuration
pub type DbConfig {
  DbConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: String,
    pool_size: Int,
  )
}

/// Repository error types
pub type RepositoryError {
  /// Database connection error
  ConnectionError(message: String)
  /// Query execution error
  QueryError(message: String)
  /// Entity not found
  NotFound(entity_type: String, id: String)
  /// Constraint violation
  ConstraintViolation(message: String)
  /// Serialization error
  SerializationError(message: String)
  /// Transaction error
  TransactionError(message: String)
}

/// Query options for filtering and pagination
pub type QueryOptions {
  QueryOptions(
    /// Maximum number of results
    limit: Option(Int),
    /// Number of results to skip
    offset: Option(Int),
    /// Order by field
    order_by: Option(String),
    /// Ascending or descending
    ascending: Bool,
  )
}

/// Filter criteria for arguments
pub type ArgumentFilter {
  ArgumentFilter(
    /// Filter by tags (any match)
    tags: Option(List(String)),
    /// Full-text search in natural language
    search_text: Option(String),
    /// Filter by source
    source: Option(String),
    /// Created after this timestamp
    created_after: Option(String),
    /// Created before this timestamp
    created_before: Option(String),
  )
}

/// Filter criteria for formalizations
pub type FormalizationFilter {
  FormalizationFilter(
    /// Filter by argument ID
    argument_id: Option(String),
    /// Filter by logic system
    logic_system: Option(LogicSystem),
    /// Only include validated formalizations
    has_validation: Option(Bool),
    /// Filter by validation result
    validation_result: Option(String),
  )
}

// =============================================================================
// Default Constructors
// =============================================================================

/// Create default database configuration
pub fn default_config() -> DbConfig {
  DbConfig(
    host: "localhost",
    port: 5432,
    database: "modal_logic",
    user: "postgres",
    password: "",
    pool_size: 10,
  )
}

/// Create default query options
pub fn default_query_options() -> QueryOptions {
  QueryOptions(limit: Some(100), offset: None, order_by: None, ascending: False)
}

/// Create empty argument filter
pub fn empty_argument_filter() -> ArgumentFilter {
  ArgumentFilter(
    tags: None,
    search_text: None,
    source: None,
    created_after: None,
    created_before: None,
  )
}

/// Create empty formalization filter
pub fn empty_formalization_filter() -> FormalizationFilter {
  FormalizationFilter(
    argument_id: None,
    logic_system: None,
    has_validation: None,
    validation_result: None,
  )
}

// =============================================================================
// SQL Query Builders
// =============================================================================

/// Build INSERT SQL for arguments
pub fn build_insert_argument_sql() -> String {
  "INSERT INTO arguments (id, natural_language, source, tags, ambiguities, created_at, updated_at)
   VALUES ($1, $2, $3, $4::jsonb, $5::jsonb, NOW(), NOW())
   RETURNING id, created_at, updated_at"
}

/// Build SELECT SQL for arguments
pub fn build_select_argument_sql() -> String {
  "SELECT id, natural_language, source, tags, ambiguities, created_at, updated_at
   FROM arguments
   WHERE id = $1"
}

/// Build UPDATE SQL for arguments
pub fn build_update_argument_sql() -> String {
  "UPDATE arguments
   SET natural_language = $2, source = $3, tags = $4::jsonb, ambiguities = $5::jsonb, updated_at = NOW()
   WHERE id = $1
   RETURNING id, created_at, updated_at"
}

/// Build DELETE SQL for arguments
pub fn build_delete_argument_sql() -> String {
  "DELETE FROM arguments WHERE id = $1"
}

/// Build INSERT SQL for formalizations
pub fn build_insert_formalization_sql() -> String {
  "INSERT INTO formalizations (id, argument_id, logic_system, premises, conclusion, assumptions, created_at, updated_at)
   VALUES ($1, $2, $3, $4::jsonb, $5::jsonb, $6::jsonb, NOW(), NOW())
   RETURNING id, created_at, updated_at"
}

/// Build SELECT SQL for formalizations
pub fn build_select_formalization_sql() -> String {
  "SELECT id, argument_id, logic_system, premises, conclusion, assumptions, created_at, updated_at
   FROM formalizations
   WHERE id = $1"
}

/// Build SELECT SQL for formalizations by argument
pub fn build_select_formalizations_by_argument_sql() -> String {
  "SELECT id, argument_id, logic_system, premises, conclusion, assumptions, created_at, updated_at
   FROM formalizations
   WHERE argument_id = $1
   ORDER BY created_at DESC"
}

/// Build INSERT SQL for validations
pub fn build_insert_validation_sql() -> String {
  "INSERT INTO validations (id, formalization_id, result_type, countermodel, error_message, logic_system, world_count, duration_ms, created_at)
   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, NOW())
   RETURNING id, created_at"
}

/// Build SELECT SQL for validations
pub fn build_select_validation_sql() -> String {
  "SELECT id, formalization_id, result_type, countermodel, error_message, logic_system, world_count, duration_ms, created_at
   FROM validations
   WHERE id = $1"
}

/// Build SELECT SQL for latest validation by formalization
pub fn build_select_latest_validation_sql() -> String {
  "SELECT id, formalization_id, result_type, countermodel, error_message, logic_system, world_count, duration_ms, created_at
   FROM validations
   WHERE formalization_id = $1
   ORDER BY created_at DESC
   LIMIT 1"
}

/// Build INSERT SQL for repair suggestions
pub fn build_insert_repair_suggestion_sql() -> String {
  "INSERT INTO repair_suggestions (id, formalization_id, repair_type, description, repaired_formalization, confidence, created_at)
   VALUES ($1, $2, $3, $4, $5::jsonb, $6, NOW())
   RETURNING id, created_at"
}

/// Build SELECT SQL for repair suggestions by formalization
pub fn build_select_repair_suggestions_sql() -> String {
  "SELECT id, formalization_id, repair_type, description, repaired_formalization, confidence, created_at
   FROM repair_suggestions
   WHERE formalization_id = $1
   ORDER BY confidence DESC"
}

// =============================================================================
// Query Building with Filters
// =============================================================================

/// Build filtered argument query
pub fn build_filtered_arguments_sql(
  filter: ArgumentFilter,
  options: QueryOptions,
) -> #(String, List(String)) {
  let base_query =
    "SELECT id, natural_language, source, tags, ambiguities, created_at, updated_at FROM arguments"
  let #(where_clauses, params) = build_argument_where_clauses(filter)

  let query = case where_clauses {
    [] -> base_query
    clauses -> base_query <> " WHERE " <> string.join(clauses, " AND ")
  }

  let query = add_order_by(query, options)
  let query = add_pagination(query, options)

  #(query, params)
}

fn build_argument_where_clauses(
  filter: ArgumentFilter,
) -> #(List(String), List(String)) {
  let clauses = []
  let params = []

  let #(clauses, params) = case filter.search_text {
    Some(text) -> #(
      [
        "to_tsvector('english', natural_language) @@ plainto_tsquery($1)",
        ..clauses
      ],
      [text, ..params],
    )
    None -> #(clauses, params)
  }

  let #(clauses, params) = case filter.source {
    Some(source) -> {
      let idx = list.length(params) + 1
      #(
        ["source = $" <> int_to_string(idx), ..clauses],
        list.append(params, [source]),
      )
    }
    None -> #(clauses, params)
  }

  let #(clauses, params) = case filter.created_after {
    Some(ts) -> {
      let idx = list.length(params) + 1
      #(
        ["created_at >= $" <> int_to_string(idx), ..clauses],
        list.append(params, [ts]),
      )
    }
    None -> #(clauses, params)
  }

  let #(clauses, params) = case filter.created_before {
    Some(ts) -> {
      let idx = list.length(params) + 1
      #(
        ["created_at <= $" <> int_to_string(idx), ..clauses],
        list.append(params, [ts]),
      )
    }
    None -> #(clauses, params)
  }

  #(list.reverse(clauses), params)
}

fn add_order_by(query: String, options: QueryOptions) -> String {
  case options.order_by {
    Some(field) -> {
      let direction = case options.ascending {
        True -> "ASC"
        False -> "DESC"
      }
      query <> " ORDER BY " <> field <> " " <> direction
    }
    None -> query <> " ORDER BY created_at DESC"
  }
}

fn add_pagination(query: String, options: QueryOptions) -> String {
  let query = case options.limit {
    Some(limit) -> query <> " LIMIT " <> int_to_string(limit)
    None -> query
  }

  case options.offset {
    Some(offset) -> query <> " OFFSET " <> int_to_string(offset)
    None -> query
  }
}

// =============================================================================
// Serialization Functions
// =============================================================================

/// Serialize a proposition to JSON string
pub fn serialize_proposition(prop: Proposition) -> String {
  proposition_to_json(prop)
  |> json.to_string
}

/// Serialize a list of propositions to JSON string
pub fn serialize_propositions(props: List(Proposition)) -> String {
  props
  |> list.map(proposition_to_json)
  |> json.preprocessed_array
  |> json.to_string
}

fn proposition_to_json(prop: Proposition) -> json.Json {
  case prop {
    proposition.Atom(name) ->
      json.object([#("type", json.string("atom")), #("name", json.string(name))])
    proposition.Not(inner) ->
      json.object([
        #("type", json.string("not")),
        #("inner", proposition_to_json(inner)),
      ])
    proposition.And(left, right) ->
      json.object([
        #("type", json.string("and")),
        #("left", proposition_to_json(left)),
        #("right", proposition_to_json(right)),
      ])
    proposition.Or(left, right) ->
      json.object([
        #("type", json.string("or")),
        #("left", proposition_to_json(left)),
        #("right", proposition_to_json(right)),
      ])
    proposition.Implies(antecedent, consequent) ->
      json.object([
        #("type", json.string("implies")),
        #("antecedent", proposition_to_json(antecedent)),
        #("consequent", proposition_to_json(consequent)),
      ])
    proposition.Necessary(inner) ->
      json.object([
        #("type", json.string("necessary")),
        #("inner", proposition_to_json(inner)),
      ])
    proposition.Possible(inner) ->
      json.object([
        #("type", json.string("possible")),
        #("inner", proposition_to_json(inner)),
      ])
    proposition.Obligatory(inner) ->
      json.object([
        #("type", json.string("obligatory")),
        #("inner", proposition_to_json(inner)),
      ])
    proposition.Permitted(inner) ->
      json.object([
        #("type", json.string("permitted")),
        #("inner", proposition_to_json(inner)),
      ])
    proposition.Knows(agent, inner) ->
      json.object([
        #("type", json.string("knows")),
        #("agent", json.string(agent)),
        #("inner", proposition_to_json(inner)),
      ])
    proposition.Believes(agent, inner) ->
      json.object([
        #("type", json.string("believes")),
        #("agent", json.string(agent)),
        #("inner", proposition_to_json(inner)),
      ])
    // Probabilistic operators
    proposition.Probable(inner) ->
      json.object([
        #("type", json.string("probable")),
        #("inner", proposition_to_json(inner)),
      ])
    proposition.ProbAtLeast(inner, threshold) ->
      json.object([
        #("type", json.string("prob_at_least")),
        #("inner", proposition_to_json(inner)),
        #("threshold", json.float(threshold)),
      ])
    proposition.ProbAtMost(inner, threshold) ->
      json.object([
        #("type", json.string("prob_at_most")),
        #("inner", proposition_to_json(inner)),
        #("threshold", json.float(threshold)),
      ])
    proposition.ProbExact(inner, probability) ->
      json.object([
        #("type", json.string("prob_exact")),
        #("inner", proposition_to_json(inner)),
        #("probability", json.float(probability)),
      ])
    proposition.ProbRange(inner, low, high) ->
      json.object([
        #("type", json.string("prob_range")),
        #("inner", proposition_to_json(inner)),
        #("low", json.float(low)),
        #("high", json.float(high)),
      ])
    proposition.CondProb(consequent, antecedent, probability) ->
      json.object([
        #("type", json.string("cond_prob")),
        #("consequent", proposition_to_json(consequent)),
        #("antecedent", proposition_to_json(antecedent)),
        #("probability", json.float(probability)),
      ])
  }
}

/// Serialize a logic system to string
pub fn serialize_logic_system(system: LogicSystem) -> String {
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

/// Parse a logic system from string
pub fn parse_logic_system(s: String) -> Option(LogicSystem) {
  case s {
    "K" -> Some(proposition.K)
    "T" -> Some(proposition.T)
    "K4" -> Some(proposition.K4)
    "S4" -> Some(proposition.S4)
    "S5" -> Some(proposition.S5)
    "KD" -> Some(proposition.KD)
    "KD45" -> Some(proposition.KD45)
    _ -> None
  }
}

/// Serialize validation result type to string
pub fn serialize_validation_result(result: ValidationResult) -> String {
  case result {
    argument.Valid -> "valid"
    argument.Invalid(_) -> "invalid"
    argument.Unknown(_) -> "unknown"
    argument.Timeout -> "timeout"
    argument.Error(_) -> "error"
  }
}

/// Serialize ambiguities to JSON string
pub fn serialize_ambiguities(ambiguities: List(Ambiguity)) -> String {
  ambiguities
  |> list.map(ambiguity_to_json)
  |> json.preprocessed_array
  |> json.to_string
}

fn ambiguity_to_json(ambiguity: Ambiguity) -> json.Json {
  case ambiguity {
    argument.ModalAmbiguity(term, interpretations) ->
      json.object([
        #("type", json.string("modal")),
        #("term", json.string(term)),
        #(
          "interpretations",
          json.array(interpretations, fn(i) {
            json.string(modal_interpretation_to_string(i))
          }),
        ),
      ])
    argument.ScopeAmbiguity(description, readings) ->
      json.object([
        #("type", json.string("scope")),
        #("description", json.string(description)),
        #("readings", json.array(readings, json.string)),
      ])
    argument.LexicalAmbiguity(term, meanings) ->
      json.object([
        #("type", json.string("lexical")),
        #("term", json.string(term)),
        #("meanings", json.array(meanings, json.string)),
      ])
    argument.StructuralAmbiguity(description, parses) ->
      json.object([
        #("type", json.string("structural")),
        #("description", json.string(description)),
        #("parses", json.array(parses, json.string)),
      ])
  }
}

fn modal_interpretation_to_string(
  interpretation: proposition.ModalInterpretation,
) -> String {
  case interpretation {
    proposition.Epistemic -> "epistemic"
    proposition.Deontic -> "deontic"
    proposition.Alethic -> "alethic"
    proposition.Temporal -> "temporal"
  }
}

/// Serialize tags to JSON string
pub fn serialize_tags(tags: List(String)) -> String {
  tags
  |> json.array(json.string)
  |> json.to_string
}

/// Serialize repair type to string
pub fn serialize_repair_type(repair_type: RepairType) -> String {
  argument.repair_type_to_string(repair_type)
}

// =============================================================================
// Transaction Support
// =============================================================================

/// Transaction operations for complex multi-step operations
pub type TransactionOp {
  InsertArgument(Argument)
  UpdateArgument(Argument)
  DeleteArgument(String)
  InsertFormalization(Formalization)
  UpdateFormalization(Formalization)
  DeleteFormalization(String)
  InsertValidation(ValidationRecord)
  InsertRepairSuggestion(RepairSuggestion)
}

/// Build a transaction from a list of operations
pub fn build_transaction(ops: List(TransactionOp)) -> List(String) {
  ["BEGIN"]
  |> list.append(list.map(ops, transaction_op_to_sql))
  |> list.append(["COMMIT"])
}

fn transaction_op_to_sql(op: TransactionOp) -> String {
  case op {
    InsertArgument(_) -> build_insert_argument_sql()
    UpdateArgument(_) -> build_update_argument_sql()
    DeleteArgument(_) -> build_delete_argument_sql()
    InsertFormalization(_) -> build_insert_formalization_sql()
    UpdateFormalization(_) ->
      "UPDATE formalizations SET logic_system = $3, premises = $4::jsonb, conclusion = $5::jsonb, assumptions = $6::jsonb, updated_at = NOW() WHERE id = $1"
    DeleteFormalization(_) -> "DELETE FROM formalizations WHERE id = $1"
    InsertValidation(_) -> build_insert_validation_sql()
    InsertRepairSuggestion(_) -> build_insert_repair_suggestion_sql()
  }
}

// =============================================================================
// Batch Operations
// =============================================================================

/// Build batch insert SQL for arguments
pub fn build_batch_insert_arguments_sql(count: Int) -> String {
  let values =
    list.range(0, count - 1)
    |> list.map(fn(i) {
      let base = i * 5
      "($"
      <> int_to_string(base + 1)
      <> ", $"
      <> int_to_string(base + 2)
      <> ", $"
      <> int_to_string(base + 3)
      <> ", $"
      <> int_to_string(base + 4)
      <> "::jsonb, $"
      <> int_to_string(base + 5)
      <> "::jsonb, NOW(), NOW())"
    })
    |> string.join(", ")

  "INSERT INTO arguments (id, natural_language, source, tags, ambiguities, created_at, updated_at) VALUES "
  <> values
}

/// Build batch insert SQL for validations
pub fn build_batch_insert_validations_sql(count: Int) -> String {
  let values =
    list.range(0, count - 1)
    |> list.map(fn(i) {
      let base = i * 8
      "($"
      <> int_to_string(base + 1)
      <> ", $"
      <> int_to_string(base + 2)
      <> ", $"
      <> int_to_string(base + 3)
      <> ", $"
      <> int_to_string(base + 4)
      <> ", $"
      <> int_to_string(base + 5)
      <> ", $"
      <> int_to_string(base + 6)
      <> ", $"
      <> int_to_string(base + 7)
      <> ", $"
      <> int_to_string(base + 8)
      <> ", NOW())"
    })
    |> string.join(", ")

  "INSERT INTO validations (id, formalization_id, result_type, countermodel, error_message, logic_system, world_count, duration_ms, created_at) VALUES "
  <> values
}

// =============================================================================
// Statistics Queries
// =============================================================================

/// Build SQL to get argument statistics
pub fn build_argument_stats_sql() -> String {
  "SELECT
     COUNT(*) as total_arguments,
     COUNT(DISTINCT f.argument_id) as arguments_with_formalizations,
     AVG(jsonb_array_length(a.tags)) as avg_tags_per_argument
   FROM arguments a
   LEFT JOIN formalizations f ON f.argument_id = a.id"
}

/// Build SQL to get validation statistics
pub fn build_validation_stats_sql() -> String {
  "SELECT
     result_type,
     COUNT(*) as count,
     AVG(duration_ms) as avg_duration_ms,
     MIN(duration_ms) as min_duration_ms,
     MAX(duration_ms) as max_duration_ms
   FROM validations
   GROUP BY result_type"
}

/// Build SQL to get logic system usage
pub fn build_logic_system_usage_sql() -> String {
  "SELECT
     logic_system,
     COUNT(*) as formalization_count,
     COUNT(DISTINCT argument_id) as argument_count
   FROM formalizations
   GROUP BY logic_system
   ORDER BY formalization_count DESC"
}

// =============================================================================
// Internal Helpers
// =============================================================================

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ if n < 0 -> "-" <> do_int_to_string(-n, "")
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
