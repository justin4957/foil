//// HuggingFace Dataset Loader
////
//// This module provides functionality to fetch and parse datasets
//// from HuggingFace for expanded test coverage.
////
//// Supported datasets:
//// - tasksource/folio - FOL reasoning (~1,435 examples)
//// - AtlasUnified/Atlas-Reasoning - Syllogistic arguments

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// A dataset from HuggingFace
pub type HuggingFaceDataset {
  HuggingFaceDataset(
    /// Dataset name (e.g., "tasksource/folio")
    name: String,
    /// Dataset description
    description: String,
    /// Total number of examples
    total_examples: Int,
    /// Loaded examples
    examples: List(DatasetExample),
    /// Cache metadata
    cache_info: CacheInfo,
  )
}

/// A single example from a dataset
pub type DatasetExample {
  DatasetExample(
    /// Unique identifier within dataset
    id: String,
    /// Natural language premises
    premises: List(String),
    /// Natural language conclusion
    conclusion: String,
    /// Expected label (entailment, contradiction, neutral)
    label: ExampleLabel,
    /// Original source format
    source_format: String,
    /// Any additional metadata
    metadata: Dict(String, String),
  )
}

/// Label for dataset examples
pub type ExampleLabel {
  Entailment
  Contradiction
  Neutral
  Unknown
}

/// Cache information for a dataset
pub type CacheInfo {
  CacheInfo(
    /// When the dataset was cached
    cached_at: Option(String),
    /// Cache expiration (ISO 8601)
    expires_at: Option(String),
    /// Whether cache is still valid
    is_valid: Bool,
  )
}

/// Error types for dataset loading
pub type LoadError {
  /// Network error during fetch
  NetworkError(message: String)
  /// Failed to parse dataset
  ParseError(message: String)
  /// Dataset not found
  NotFound(dataset: String)
  /// Cache expired
  CacheExpired(dataset: String)
  /// Rate limited
  RateLimited(retry_after: Int)
}

/// Dataset configuration
pub type DatasetConfig {
  DatasetConfig(
    /// Maximum examples to load
    max_examples: Option(Int),
    /// Split to use (train, test, validation)
    split: String,
    /// Whether to use cached data
    use_cache: Bool,
    /// Cache duration in seconds (default: 7 days)
    cache_duration_seconds: Int,
  )
}

/// Create default dataset configuration
pub fn default_config() -> DatasetConfig {
  DatasetConfig(
    max_examples: Some(100),
    split: "test",
    use_cache: True,
    cache_duration_seconds: 604_800,
  )
}

/// Create configuration for full dataset loading
pub fn full_dataset_config() -> DatasetConfig {
  DatasetConfig(
    max_examples: None,
    split: "test",
    use_cache: True,
    cache_duration_seconds: 604_800,
  )
}

/// Simulated dataset loader (actual HTTP would require additional dependencies)
/// Returns mock data representing the structure of real HuggingFace datasets
pub fn load_dataset(
  name: String,
  config: DatasetConfig,
) -> Result(HuggingFaceDataset, LoadError) {
  case name {
    "tasksource/folio" -> Ok(load_folio_mock(config))
    "AtlasUnified/Atlas-Reasoning" -> Ok(load_atlas_mock(config))
    _ -> Error(NotFound(dataset: name))
  }
}

/// Load mock FOLIO dataset (First Order Logic Inference Open-domain)
fn load_folio_mock(config: DatasetConfig) -> HuggingFaceDataset {
  let examples = [
    DatasetExample(
      id: "folio_1",
      premises: [
        "All students who study hard pass their exams.",
        "John is a student.",
        "John studies hard.",
      ],
      conclusion: "John passes his exams.",
      label: Entailment,
      source_format: "folio",
      metadata: dict.new(),
    ),
    DatasetExample(
      id: "folio_2",
      premises: [
        "No reptiles have fur.",
        "All snakes are reptiles.",
      ],
      conclusion: "Snakes do not have fur.",
      label: Entailment,
      source_format: "folio",
      metadata: dict.new(),
    ),
    DatasetExample(
      id: "folio_3",
      premises: [
        "Some philosophers are logicians.",
        "All logicians study formal systems.",
      ],
      conclusion: "Some philosophers study formal systems.",
      label: Entailment,
      source_format: "folio",
      metadata: dict.new(),
    ),
    DatasetExample(
      id: "folio_4",
      premises: [
        "If it rains, the ground gets wet.",
        "The ground is wet.",
      ],
      conclusion: "It rained.",
      label: Neutral,
      source_format: "folio",
      metadata: dict.new(),
    ),
    DatasetExample(
      id: "folio_5",
      premises: [
        "All mammals are warm-blooded.",
        "Whales are mammals.",
      ],
      conclusion: "Whales are cold-blooded.",
      label: Contradiction,
      source_format: "folio",
      metadata: dict.new(),
    ),
    DatasetExample(
      id: "folio_6",
      premises: [
        "Either it is sunny or it is raining.",
        "It is not sunny.",
      ],
      conclusion: "It is raining.",
      label: Entailment,
      source_format: "folio",
      metadata: dict.new(),
    ),
    DatasetExample(
      id: "folio_7",
      premises: [
        "If someone is a doctor, they have a medical degree.",
        "Alice is a doctor.",
      ],
      conclusion: "Alice has a medical degree.",
      label: Entailment,
      source_format: "folio",
      metadata: dict.new(),
    ),
    DatasetExample(
      id: "folio_8",
      premises: [
        "All birds can fly.",
        "Penguins are birds.",
      ],
      conclusion: "Penguins can fly.",
      label: Entailment,
      // Note: This is actually a tricky case!
      source_format: "folio",
      metadata: dict.new(),
    ),
  ]

  let limited_examples = case config.max_examples {
    Some(max) -> list.take(examples, max)
    None -> examples
  }

  HuggingFaceDataset(
    name: "tasksource/folio",
    description: "First Order Logic Inference Open-domain dataset",
    total_examples: list.length(limited_examples),
    examples: limited_examples,
    cache_info: CacheInfo(
      cached_at: Some("2024-01-15T00:00:00Z"),
      expires_at: Some("2024-01-22T00:00:00Z"),
      is_valid: True,
    ),
  )
}

/// Load mock Atlas-Reasoning dataset
fn load_atlas_mock(config: DatasetConfig) -> HuggingFaceDataset {
  let examples = [
    DatasetExample(
      id: "atlas_1",
      premises: [
        "All men are mortal.",
        "Socrates is a man.",
      ],
      conclusion: "Socrates is mortal.",
      label: Entailment,
      source_format: "atlas",
      metadata: dict.from_list([#("type", "syllogism")]),
    ),
    DatasetExample(
      id: "atlas_2",
      premises: [
        "Necessarily, if God exists then God is perfect.",
        "A perfect being must exist.",
      ],
      conclusion: "God necessarily exists.",
      label: Entailment,
      source_format: "atlas",
      metadata: dict.from_list([#("type", "modal"), #("topic", "ontological")]),
    ),
    DatasetExample(
      id: "atlas_3",
      premises: [
        "It is possible that there is intelligent life on other planets.",
        "What is possible is not necessarily false.",
      ],
      conclusion: "It is not necessarily false that there is intelligent life on other planets.",
      label: Entailment,
      source_format: "atlas",
      metadata: dict.from_list([#("type", "modal")]),
    ),
    DatasetExample(
      id: "atlas_4",
      premises: [
        "One ought to keep one's promises.",
        "John promised to help Mary.",
      ],
      conclusion: "John ought to help Mary.",
      label: Entailment,
      source_format: "atlas",
      metadata: dict.from_list([#("type", "deontic")]),
    ),
    DatasetExample(
      id: "atlas_5",
      premises: [
        "Alice knows that it is raining.",
        "If someone knows something, they believe it.",
      ],
      conclusion: "Alice believes that it is raining.",
      label: Entailment,
      source_format: "atlas",
      metadata: dict.from_list([#("type", "epistemic")]),
    ),
  ]

  let limited_examples = case config.max_examples {
    Some(max) -> list.take(examples, max)
    None -> examples
  }

  HuggingFaceDataset(
    name: "AtlasUnified/Atlas-Reasoning",
    description: "Atlas Reasoning dataset with syllogistic and philosophical arguments",
    total_examples: list.length(limited_examples),
    examples: limited_examples,
    cache_info: CacheInfo(
      cached_at: Some("2024-01-15T00:00:00Z"),
      expires_at: Some("2024-01-22T00:00:00Z"),
      is_valid: True,
    ),
  )
}

/// Get available datasets
pub fn available_datasets() -> List(#(String, String)) {
  [
    #(
      "tasksource/folio",
      "First Order Logic Inference Open-domain (~1,435 examples)",
    ),
    #(
      "AtlasUnified/Atlas-Reasoning",
      "Syllogistic arguments and philosophical debates",
    ),
  ]
}

/// Format dataset as summary string
pub fn dataset_summary(dataset: HuggingFaceDataset) -> String {
  string.concat([
    "Dataset: ",
    dataset.name,
    "\n",
    "Description: ",
    dataset.description,
    "\n",
    "Examples: ",
    int.to_string(dataset.total_examples),
    "\n",
    "Cache Valid: ",
    case dataset.cache_info.is_valid {
      True -> "Yes"
      False -> "No"
    },
    "\n",
  ])
}

/// Convert example label to string
pub fn label_to_string(label: ExampleLabel) -> String {
  case label {
    Entailment -> "entailment"
    Contradiction -> "contradiction"
    Neutral -> "neutral"
    Unknown -> "unknown"
  }
}

/// Convert string to example label
pub fn string_to_label(s: String) -> ExampleLabel {
  case string.lowercase(s) {
    "entailment" -> Entailment
    "contradiction" -> Contradiction
    "neutral" -> Neutral
    _ -> Unknown
  }
}

/// Check if dataset cache is valid
pub fn is_cache_valid(dataset: HuggingFaceDataset) -> Bool {
  dataset.cache_info.is_valid
}

/// Refresh dataset (would fetch from HuggingFace API in real implementation)
pub fn refresh_dataset(
  dataset: HuggingFaceDataset,
  config: DatasetConfig,
) -> Result(HuggingFaceDataset, LoadError) {
  // In a real implementation, this would fetch fresh data
  load_dataset(dataset.name, config)
}
