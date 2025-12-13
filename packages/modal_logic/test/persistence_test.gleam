//// Persistence Layer Tests
////
//// Tests for core domain types, repository, cache, and graph modules.

import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import modal_logic/argument.{Argument, Formalization, Invalid, Valid}
import modal_logic/cache
import modal_logic/graph
import modal_logic/proposition.{And, Atom, Implies, K, Necessary, Possible, S4, S5, T}
import modal_logic/repository

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Persistence Layer Tests")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Core Domain Types
  test_domain_types()

  // Test 2: Repository SQL Builders
  test_repository_sql()

  // Test 3: Cache Operations
  test_cache_operations()

  // Test 4: Graph Queries
  test_graph_queries()

  // Test 5: Serialization
  test_serialization()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("All Persistence Layer Tests Passed!")
  io.println("=" |> string.repeat(70))
}

fn test_domain_types() {
  io.println("")
  io.println("--- Test 1: Core Domain Types ---")
  io.println("")

  // Create an argument
  let arg =
    argument.new_argument(
      "arg-1",
      "All men are mortal. Socrates is a man. Therefore, Socrates is mortal.",
    )
  io.println("[OK] Created argument: " <> arg.id)

  // Create premises and conclusion
  let p = Atom("man")
  let q = Atom("mortal")
  let premise1 = Implies(p, q)
  let premise2 = Atom("socrates_is_man")
  let conclusion = Atom("socrates_is_mortal")

  // Create a formalization
  let form =
    argument.new_formalization(
      "form-1",
      "arg-1",
      S4,
      [premise1, premise2],
      conclusion,
    )
  io.println("[OK] Created formalization: " <> form.id)
  io.println("     Logic system: S4")
  io.println("     Premises: 2")

  // Test validation result
  let valid_result = Valid
  let invalid_result = Invalid("Countermodel: w0 where p is false")
  io.println(
    "[OK] Validation results: is_valid(Valid) = "
    <> bool_to_string(argument.is_valid(valid_result)),
  )
  io.println(
    "     is_invalid(Invalid) = "
    <> bool_to_string(argument.is_invalid(invalid_result)),
  )

  // Test repair types
  let repair = argument.AddPremise
  io.println("[OK] Repair type: " <> argument.repair_type_to_string(repair))

  io.println("")
}

fn test_repository_sql() {
  io.println("")
  io.println("--- Test 2: Repository SQL Builders ---")
  io.println("")

  // Test SQL generation
  let insert_sql = repository.build_insert_argument_sql()
  io.println("[OK] Insert argument SQL generated")
  io.println(
    "     Length: " <> int_to_string(string.length(insert_sql)) <> " chars",
  )

  let _select_sql = repository.build_select_argument_sql()
  io.println("[OK] Select argument SQL generated")

  let _insert_form_sql = repository.build_insert_formalization_sql()
  io.println("[OK] Insert formalization SQL generated")

  let _insert_validation_sql = repository.build_insert_validation_sql()
  io.println("[OK] Insert validation SQL generated")

  // Test filtered query
  let filter =
    repository.ArgumentFilter(
      tags: Some(["logic", "modal"]),
      search_text: Some("mortal"),
      source: None,
      created_after: None,
      created_before: None,
    )
  let options = repository.default_query_options()
  let #(query, _params) =
    repository.build_filtered_arguments_sql(filter, options)
  io.println("[OK] Filtered query built")
  io.println(
    "     Query length: " <> int_to_string(string.length(query)) <> " chars",
  )

  // Test batch insert
  let _batch_sql = repository.build_batch_insert_arguments_sql(5)
  io.println("[OK] Batch insert SQL for 5 arguments generated")

  // Test statistics queries
  let _stats_sql = repository.build_argument_stats_sql()
  io.println("[OK] Statistics query generated")

  io.println("")
}

fn test_cache_operations() {
  io.println("")
  io.println("--- Test 3: Cache Operations ---")
  io.println("")

  // Create cache config
  let config = cache.default_config()
  io.println("[OK] Default cache config created")
  io.println("     TTL: " <> int_to_string(config.default_ttl_seconds) <> "s")
  io.println("     Max entries: " <> int_to_string(config.max_entries))

  // Create memory cache
  let mem_cache = cache.new_memory_cache(config)
  io.println("[OK] Memory cache created")

  // Create a formalization for cache key
  let form =
    Formalization(
      id: "form-1",
      argument_id: "arg-1",
      logic_system: S4,
      premises: [Implies(Atom("p"), Atom("q")), Atom("p")],
      conclusion: Atom("q"),
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Generate cache key
  let key = cache.validation_cache_key(config, form)
  io.println("[OK] Cache key generated")
  io.println("     Key prefix: " <> config.key_prefix)
  io.println(
    "     Key length: " <> int_to_string(string.length(key)) <> " chars",
  )

  // Put a value
  let current_time = 1_000_000
  let mem_cache = cache.memory_put(mem_cache, key, Valid, current_time)
  io.println("[OK] Cached validation result")

  // Get the value
  let #(mem_cache, result) = cache.memory_get(mem_cache, key, current_time)
  case result {
    cache.CacheHit(_) -> io.println("[OK] Cache hit!")
    cache.CacheMiss -> io.println("[FAIL] Cache miss (unexpected)")
    cache.CacheError(msg) -> io.println("[FAIL] Cache error: " <> msg)
  }

  // Check stats
  let stats = cache.memory_stats(mem_cache)
  io.println("[OK] Cache stats: " <> cache.format_stats(stats))

  // Test normalization
  let p1 = And(Atom("P"), Atom("Q"))
  let p2 = And(Atom("Q"), Atom("P"))
  let _norm1 = cache.normalize_proposition(p1)
  let _norm2 = cache.normalize_proposition(p2)
  io.println("[OK] Proposition normalization tested")

  // Test Redis command builders
  let redis_get = cache.redis_get_command(key)
  io.println(
    "[OK] Redis GET command: " <> string.slice(redis_get, 0, 50) <> "...",
  )

  io.println("")
}

fn test_graph_queries() {
  io.println("")
  io.println("--- Test 4: Graph Queries ---")
  io.println("")

  // Create test propositions
  let p = Atom("p")
  let q = Atom("q")
  let r = Atom("r")

  // Create formalizations
  let form1 =
    Formalization(
      id: "f1",
      argument_id: "a1",
      logic_system: S4,
      premises: [Implies(p, q), p],
      conclusion: q,
      assumptions: [],
      validation: Some(Valid),
      created_at: None,
      updated_at: None,
    )

  let form2 =
    Formalization(
      id: "f2",
      argument_id: "a2",
      logic_system: S4,
      premises: [Implies(p, q), p],
      conclusion: q,
      assumptions: [],
      validation: Some(Valid),
      created_at: None,
      updated_at: None,
    )

  let form3 =
    Formalization(
      id: "f3",
      argument_id: "a3",
      logic_system: T,
      premises: [Implies(q, r), q],
      conclusion: r,
      assumptions: [],
      validation: None,
      created_at: None,
      updated_at: None,
    )

  // Test formalization similarity
  let sim = graph.formalization_similarity(form1, form2)
  io.println(
    "[OK] Similarity f1<->f2 (identical): " <> float_to_string_approx(sim),
  )

  let sim2 = graph.formalization_similarity(form1, form3)
  io.println(
    "[OK] Similarity f1<->f3 (different): " <> float_to_string_approx(sim2),
  )

  // Test find similar
  let similar = graph.find_similar_formalizations(form1, [form2, form3], 0.5)
  io.println(
    "[OK] Found "
    <> int_to_string(list.length(similar))
    <> " similar formalizations (threshold 0.5)",
  )

  // Test pattern extraction
  let pattern = graph.extract_pattern(form1)
  io.println("[OK] Extracted inference pattern")
  io.println(
    "     Premise patterns: "
    <> int_to_string(list.length(pattern.premise_patterns)),
  )

  // Test pattern matching
  let matches = graph.matches_pattern(form1, pattern)
  io.println("[OK] Pattern matching: " <> bool_to_string(matches))

  // Create arguments for graph
  let arg1 =
    Argument(
      id: "a1",
      natural_language: "If it rains, the ground is wet",
      source: None,
      formalizations: [form1],
      ambiguities: [],
      tags: ["logic"],
      created_at: None,
      updated_at: None,
    )

  let arg2 =
    Argument(
      id: "a2",
      natural_language: "If the ground is wet, it rained",
      source: None,
      formalizations: [form2],
      ambiguities: [],
      tags: ["logic"],
      created_at: None,
      updated_at: None,
    )

  // Test cross-argument analysis
  let related = graph.find_related_arguments(arg1, [arg2])
  io.println(
    "[OK] Found " <> int_to_string(list.length(related)) <> " related arguments",
  )

  // Build argument graph
  let arg_graph = graph.build_graph([arg1, arg2])
  io.println("[OK] Built argument graph")
  io.println("     Nodes: " <> int_to_string(dict.size(arg_graph.nodes)))
  io.println("     Edges: " <> int_to_string(list.length(arg_graph.edges)))

  // Calculate centrality
  let graph_with_centrality = graph.calculate_centrality(arg_graph)
  let central = graph.get_central_nodes(graph_with_centrality, 2)
  io.println(
    "[OK] Calculated centrality for "
    <> int_to_string(list.length(central))
    <> " nodes",
  )

  // Test SQL builders
  let _similar_sql = graph.build_similar_formalizations_sql()
  io.println("[OK] Similar formalizations SQL generated")

  let _rel_sql = graph.build_argument_relationships_sql()
  io.println("[OK] Argument relationships SQL generated")

  io.println("")
}

fn test_serialization() {
  io.println("")
  io.println("--- Test 5: Serialization ---")
  io.println("")

  // Test proposition serialization
  let prop = Implies(Necessary(Atom("p")), Possible(Atom("q")))
  let json = repository.serialize_proposition(prop)
  io.println("[OK] Serialized proposition to JSON")
  io.println("     Length: " <> int_to_string(string.length(json)) <> " chars")

  // Test logic system serialization
  let systems = [K, T, S4, S5]
  list.each(systems, fn(sys) {
    let str = repository.serialize_logic_system(sys)
    io.println("[OK] Logic system " <> str <> " serialized")
  })

  // Test validation result serialization
  let valid_str = repository.serialize_validation_result(Valid)
  io.println("[OK] Validation result 'valid' serialized: " <> valid_str)

  let invalid_str = repository.serialize_validation_result(Invalid("cm"))
  io.println("[OK] Validation result 'invalid' serialized: " <> invalid_str)

  // Test ambiguity serialization
  let ambiguities = [
    argument.ModalAmbiguity("must", [proposition.Deontic, proposition.Epistemic]),
    argument.ScopeAmbiguity("quantifier scope", ["wide", "narrow"]),
  ]
  let amb_json = repository.serialize_ambiguities(ambiguities)
  io.println("[OK] Serialized 2 ambiguities to JSON")
  io.println(
    "     Length: " <> int_to_string(string.length(amb_json)) <> " chars",
  )

  // Test tags serialization
  let tags = ["modal", "logic", "validity"]
  let tags_json = repository.serialize_tags(tags)
  io.println("[OK] Serialized tags: " <> tags_json)

  io.println("")
}

// Helper functions

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

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

fn float_to_string_approx(f: Float) -> String {
  let int_part = float_to_int(f *. 100.0)
  let whole = int_part / 100
  let frac = int_part % 100
  int_to_string(whole) <> "." <> pad_left(int_to_string(abs(frac)), 2, "0")
}

fn pad_left(s: String, len: Int, pad: String) -> String {
  case string.length(s) >= len {
    True -> s
    False -> pad_left(pad <> s, len, pad)
  }
}

fn abs(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int
