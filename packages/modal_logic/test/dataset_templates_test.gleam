import gleam/list
import gleam/option
import gleam/string
import gleeunit
import gleeunit/should
import modal_logic/dataset_templates
import modal_logic/proposition.{S5}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Template Database Tests
// =============================================================================

pub fn all_templates_returns_12_templates_test() {
  let all = dataset_templates.all_templates()
  let count = list.length(all)

  count |> should.equal(12)
}

pub fn total_count_matches_all_templates_test() {
  let count = dataset_templates.count()
  let expected = list.length(dataset_templates.all_templates())

  count |> should.equal(expected)
}

pub fn folio_templates_test() {
  let templates = dataset_templates.folio_templates()

  list.length(templates) |> should.equal(4)

  // All should be FOLIO dataset
  templates
  |> list.all(fn(t) { t.dataset == dataset_templates.FOLIO })
  |> should.be_true()
}

pub fn logiqa_templates_test() {
  let templates = dataset_templates.logiqa_templates()

  list.length(templates) |> should.equal(4)

  // All should be LogiQA dataset
  templates
  |> list.all(fn(t) { t.dataset == dataset_templates.LogiQA })
  |> should.be_true()
}

pub fn inpho_templates_test() {
  let templates = dataset_templates.inpho_templates()

  list.length(templates) |> should.equal(4)

  // All should be InPhO dataset
  templates
  |> list.all(fn(t) { t.dataset == dataset_templates.InPhO })
  |> should.be_true()
}

// =============================================================================
// Filtering Tests
// =============================================================================

pub fn by_dataset_folio_test() {
  let folio_templates = dataset_templates.by_dataset(dataset_templates.FOLIO)

  list.length(folio_templates) |> should.equal(4)

  folio_templates
  |> list.all(fn(t) { t.dataset == dataset_templates.FOLIO })
  |> should.be_true()
}

pub fn by_dataset_logiqa_test() {
  let templates = dataset_templates.by_dataset(dataset_templates.LogiQA)

  list.length(templates) |> should.equal(4)

  templates
  |> list.all(fn(t) { t.dataset == dataset_templates.LogiQA })
  |> should.be_true()
}

pub fn by_dataset_inpho_test() {
  let templates = dataset_templates.by_dataset(dataset_templates.InPhO)

  list.length(templates) |> should.equal(4)

  templates
  |> list.all(fn(t) { t.dataset == dataset_templates.InPhO })
  |> should.be_true()
}

pub fn by_id_folio_belief_test() {
  case dataset_templates.by_id("folio_belief_01") {
    option.Some(template) -> {
      template.id |> should.equal("folio_belief_01")
      template.dataset |> should.equal(dataset_templates.FOLIO)
      template.template_type |> should.equal(dataset_templates.FOLIOBelief)
    }
    option.None -> should.fail()
  }
}

pub fn by_id_logiqa_multihop_test() {
  case dataset_templates.by_id("logiqa_multihop_01") {
    option.Some(template) -> {
      template.id |> should.equal("logiqa_multihop_01")
      template.dataset |> should.equal(dataset_templates.LogiQA)
    }
    option.None -> should.fail()
  }
}

pub fn by_id_nonexistent_test() {
  case dataset_templates.by_id("nonexistent") {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

pub fn by_logic_system_s5_test() {
  let s5_templates = dataset_templates.by_logic_system(S5)

  // Should have InPhO philosophical templates which use S5
  { list.length(s5_templates) > 0 } |> should.be_true()

  s5_templates
  |> list.all(fn(t) { t.logic_system == S5 })
  |> should.be_true()
}

pub fn search_belief_test() {
  let results = dataset_templates.search("belief")

  { list.length(results) > 0 } |> should.be_true()

  // Should find FOLIO belief template
  results
  |> list.any(fn(t) { t.dataset == dataset_templates.FOLIO })
  |> should.be_true()
}

pub fn search_philosophical_test() {
  let results = dataset_templates.search("philosophical")

  { list.length(results) > 0 } |> should.be_true()

  // Should find InPhO templates
  results
  |> list.any(fn(t) { t.dataset == dataset_templates.InPhO })
  |> should.be_true()
}

pub fn search_case_insensitive_test() {
  let lower = dataset_templates.search("belief")
  let upper = dataset_templates.search("BELIEF")
  let mixed = dataset_templates.search("Belief")

  list.length(lower) |> should.equal(list.length(upper))
  list.length(lower) |> should.equal(list.length(mixed))
}

// =============================================================================
// Template Structure Tests
// =============================================================================

pub fn template_has_required_fields_test() {
  case dataset_templates.by_id("folio_belief_01") {
    option.Some(template) -> {
      template.id |> should.not_equal("")
      template.name |> should.not_equal("")
      template.description |> should.not_equal("")
      template.pattern_description |> should.not_equal("")
    }
    option.None -> should.fail()
  }
}

pub fn template_has_examples_test() {
  case dataset_templates.by_id("folio_universal_01") {
    option.Some(template) -> {
      { list.length(template.example_premises) > 0 } |> should.be_true()
      template.example_conclusion |> should.not_equal("")
    }
    option.None -> should.fail()
  }
}

pub fn template_has_use_cases_test() {
  case dataset_templates.by_id("logiqa_multihop_01") {
    option.Some(template) -> {
      { list.length(template.use_cases) > 0 } |> should.be_true()
    }
    option.None -> should.fail()
  }
}

// =============================================================================
// Dataset Conversion Tests
// =============================================================================

pub fn dataset_to_string_test() {
  dataset_templates.dataset_to_string(dataset_templates.FOLIO)
  |> should.equal("folio")
  dataset_templates.dataset_to_string(dataset_templates.LogiQA)
  |> should.equal("logiqa")
  dataset_templates.dataset_to_string(dataset_templates.InPhO)
  |> should.equal("inpho")
}

pub fn string_to_dataset_test() {
  case dataset_templates.string_to_dataset("folio") {
    option.Some(ds) -> ds |> should.equal(dataset_templates.FOLIO)
    option.None -> should.fail()
  }

  case dataset_templates.string_to_dataset("logiqa") {
    option.Some(ds) -> ds |> should.equal(dataset_templates.LogiQA)
    option.None -> should.fail()
  }

  case dataset_templates.string_to_dataset("inpho") {
    option.Some(ds) -> ds |> should.equal(dataset_templates.InPhO)
    option.None -> should.fail()
  }
}

pub fn string_to_dataset_invalid_test() {
  case dataset_templates.string_to_dataset("invalid") {
    option.Some(_) -> should.fail()
    option.None -> should.be_true(True)
  }
}

pub fn template_type_to_string_test() {
  dataset_templates.template_type_to_string(dataset_templates.FOLIOBelief)
  |> should.equal("belief")
  dataset_templates.template_type_to_string(dataset_templates.LogiQAMultiHop)
  |> should.equal("multihop")
  dataset_templates.template_type_to_string(
    dataset_templates.InPhOPhilosophicalArgument,
  )
  |> should.equal("philosophical")
}

// =============================================================================
// Count Tests
// =============================================================================

pub fn count_by_dataset_test() {
  let by_dataset = dataset_templates.count_by_dataset()

  list.length(by_dataset) |> should.equal(3)

  // Check totals sum correctly
  let total =
    by_dataset
    |> list.map(fn(pair) {
      let #(_, count) = pair
      count
    })
    |> list.fold(0, fn(acc, n) { acc + n })

  total |> should.equal(dataset_templates.count())
}

// =============================================================================
// Template Application Tests
// =============================================================================

pub fn apply_template_test() {
  case dataset_templates.by_id("folio_belief_01") {
    option.Some(template) -> {
      let application =
        dataset_templates.apply_template(
          template,
          ["All humans are mortal", "Socrates is human"],
          "Socrates is mortal",
        )

      list.length(application.applied_premises) |> should.equal(2)
      { application.confidence >. 0.0 } |> should.be_true()
    }
    option.None -> should.fail()
  }
}

// =============================================================================
// Template Formatting Tests
// =============================================================================

pub fn format_template_test() {
  case dataset_templates.by_id("logiqa_chain_01") {
    option.Some(template) -> {
      let formatted = dataset_templates.format_template(template)

      formatted |> should_contain("Template:")
      formatted |> should_contain("Dataset:")
      formatted |> should_contain("Logic System:")
      formatted |> should_contain("Example:")
    }
    option.None -> should.fail()
  }
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
