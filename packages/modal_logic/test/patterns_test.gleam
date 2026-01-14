import gleam/list
import gleam/option
import gleam/string
import gleeunit
import gleeunit/should
import modal_logic/patterns

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Pattern Database Tests
// =============================================================================

pub fn all_patterns_returns_at_least_20_test() {
  let all = patterns.all_patterns()
  let count = list.length(all)

  { count >= 20 } |> should.be_true()
}

pub fn total_count_matches_all_patterns_test() {
  let count = patterns.count()
  let expected = list.length(patterns.all_patterns())

  count |> should.equal(expected)
}

pub fn epistemic_patterns_test() {
  let eps = patterns.epistemic_patterns()

  { list.length(eps) >= 5 } |> should.be_true()

  // Check first pattern exists
  case list.first(eps) {
    Ok(pattern) -> {
      pattern.category |> should.equal(patterns.Epistemic)
      pattern.id |> should_contain("ep")
    }
    Error(_) -> should.fail()
  }
}

pub fn deontic_patterns_test() {
  let dps = patterns.deontic_patterns()

  { list.length(dps) >= 5 } |> should.be_true()

  // All should be deontic category
  dps
  |> list.all(fn(p) { p.category == patterns.Deontic })
  |> should.be_true()
}

pub fn alethic_patterns_test() {
  let aps = patterns.alethic_patterns()

  { list.length(aps) >= 5 } |> should.be_true()

  aps
  |> list.all(fn(p) { p.category == patterns.Alethic })
  |> should.be_true()
}

pub fn classical_patterns_test() {
  let cps = patterns.classical_patterns()

  { list.length(cps) >= 5 } |> should.be_true()

  cps
  |> list.all(fn(p) { p.category == patterns.Classical })
  |> should.be_true()
}

pub fn temporal_patterns_test() {
  let tps = patterns.temporal_patterns()

  { list.length(tps) >= 1 } |> should.be_true()

  tps
  |> list.all(fn(p) { p.category == patterns.Temporal })
  |> should.be_true()
}

// =============================================================================
// Pattern Filtering Tests
// =============================================================================

pub fn by_category_epistemic_test() {
  let eps = patterns.by_category(patterns.Epistemic)

  eps
  |> list.all(fn(p) { p.category == patterns.Epistemic })
  |> should.be_true()

  { list.length(eps) > 0 } |> should.be_true()
}

pub fn by_category_deontic_test() {
  let dps = patterns.by_category(patterns.Deontic)

  dps
  |> list.all(fn(p) { p.category == patterns.Deontic })
  |> should.be_true()
}

pub fn by_tag_knowledge_test() {
  let knowledge_patterns = patterns.by_tag("knowledge")

  { list.length(knowledge_patterns) > 0 } |> should.be_true()

  knowledge_patterns
  |> list.all(fn(p) { list.contains(p.tags, "knowledge") })
  |> should.be_true()
}

pub fn by_complexity_simple_test() {
  let simple = patterns.by_complexity(patterns.Simple)

  { list.length(simple) > 0 } |> should.be_true()

  simple
  |> list.all(fn(p) { p.complexity == patterns.Simple })
  |> should.be_true()
}

pub fn by_id_test() {
  case patterns.by_id("ep001") {
    option.Some(pattern) -> {
      pattern.id |> should.equal("ep001")
      pattern.category |> should.equal(patterns.Epistemic)
    }
    option.None -> should.fail()
  }
}

pub fn by_id_nonexistent_test() {
  case patterns.by_id("nonexistent") {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

pub fn search_knowledge_test() {
  let results = patterns.search("knowledge")

  { list.length(results) > 0 } |> should.be_true()

  // Should find epistemic patterns
  results
  |> list.any(fn(p) { p.category == patterns.Epistemic })
  |> should.be_true()
}

pub fn search_obligation_test() {
  let results = patterns.search("obligation")

  { list.length(results) > 0 } |> should.be_true()

  // Should find deontic patterns
  results
  |> list.any(fn(p) { p.category == patterns.Deontic })
  |> should.be_true()
}

pub fn search_case_insensitive_test() {
  let lower = patterns.search("modus ponens")
  let upper = patterns.search("MODUS PONENS")
  let mixed = patterns.search("Modus Ponens")

  list.length(lower) |> should.equal(list.length(upper))
  list.length(lower) |> should.equal(list.length(mixed))
}

// =============================================================================
// Auto-Suggestion Tests
// =============================================================================

pub fn suggest_with_empty_input_test() {
  let suggestions = patterns.suggest("", 5)

  // Empty input should return no suggestions or low-scoring ones
  { list.length(suggestions) <= 5 } |> should.be_true()
}

pub fn suggest_with_knows_keyword_test() {
  let suggestions = patterns.suggest("Knows", 5)

  { list.length(suggestions) > 0 } |> should.be_true()

  // Should suggest epistemic patterns
  case list.first(suggestions) {
    Ok(sugg) -> {
      sugg.pattern.category |> should.equal(patterns.Epistemic)
      { sugg.relevance_score >. 0.0 } |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn suggest_with_obligatory_keyword_test() {
  let suggestions = patterns.suggest("Obligatory", 5)

  { list.length(suggestions) > 0 } |> should.be_true()

  // Should suggest deontic patterns
  suggestions
  |> list.any(fn(s) { s.pattern.category == patterns.Deontic })
  |> should.be_true()
}

pub fn suggest_respects_max_limit_test() {
  let suggestions = patterns.suggest("p", 3)

  { list.length(suggestions) <= 3 } |> should.be_true()
}

pub fn suggest_sorts_by_relevance_test() {
  let suggestions = patterns.suggest("Knows(agent, p)", 5)

  case suggestions {
    [first, second, ..] -> {
      // First should have higher or equal score than second
      { first.relevance_score >=. second.relevance_score }
      |> should.be_true()
    }
    _ -> should.be_true(True)
    // Less than 2 results, can't compare
  }
}

pub fn suggest_by_category_filters_correctly_test() {
  let suggestions =
    patterns.suggest_by_category(patterns.Deontic, "Obligatory", 5)

  // All suggestions should be deontic
  suggestions
  |> list.all(fn(s) { s.pattern.category == patterns.Deontic })
  |> should.be_true()
}

// =============================================================================
// Pattern Structure Tests
// =============================================================================

pub fn pattern_has_required_fields_test() {
  case patterns.by_id("ep001") {
    option.Some(pattern) -> {
      pattern.id |> should.not_equal("")
      pattern.name |> should.not_equal("")
      pattern.formula |> should.not_equal("")
      pattern.description |> should.not_equal("")
    }
    option.None -> should.fail()
  }
}

pub fn pattern_has_tags_test() {
  case patterns.by_id("ep001") {
    option.Some(pattern) -> {
      { list.length(pattern.tags) > 0 } |> should.be_true()
    }
    option.None -> should.fail()
  }
}

pub fn pattern_has_use_cases_test() {
  case patterns.by_id("dp001") {
    option.Some(pattern) -> {
      { list.length(pattern.use_cases) > 0 } |> should.be_true()
    }
    option.None -> should.fail()
  }
}

// =============================================================================
// Category Conversion Tests
// =============================================================================

pub fn category_to_string_test() {
  patterns.category_to_string(patterns.Epistemic) |> should.equal("epistemic")
  patterns.category_to_string(patterns.Deontic) |> should.equal("deontic")
  patterns.category_to_string(patterns.Temporal) |> should.equal("temporal")
  patterns.category_to_string(patterns.Alethic) |> should.equal("alethic")
  patterns.category_to_string(patterns.Classical) |> should.equal("classical")
}

pub fn string_to_category_test() {
  case patterns.string_to_category("epistemic") {
    option.Some(cat) -> cat |> should.equal(patterns.Epistemic)
    option.None -> should.fail()
  }

  case patterns.string_to_category("deontic") {
    option.Some(cat) -> cat |> should.equal(patterns.Deontic)
    option.None -> should.fail()
  }
}

pub fn string_to_category_invalid_test() {
  case patterns.string_to_category("invalid") {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

pub fn complexity_to_string_test() {
  patterns.complexity_to_string(patterns.Simple) |> should.equal("simple")
  patterns.complexity_to_string(patterns.Medium) |> should.equal("medium")
  patterns.complexity_to_string(patterns.Complex) |> should.equal("complex")
}

// =============================================================================
// Count Tests
// =============================================================================

pub fn count_by_category_sums_correctly_test() {
  let by_cat = patterns.count_by_category()
  let total =
    by_cat
    |> list.map(fn(pair) {
      let #(_, count) = pair
      count
    })
    |> list.fold(0, fn(acc, n) { acc + n })

  total |> should.equal(patterns.count())
}

// =============================================================================
// Helper Functions
// =============================================================================

fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False -> should.fail()
  }
}
