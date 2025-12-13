//// Translation Prompts for LLM-based Argument Analysis
////
//// This module provides prompt templates for extracting logical structure
//// from natural language arguments using an LLM. The prompts are designed
//// to produce structured JSON output suitable for parsing.
////
//// ## Design Principles
////
//// 1. **Explicit JSON Schema**: Each prompt includes a precise schema
//// 2. **Ambiguity Flagging**: LLM identifies uncertain interpretations
//// 3. **Modal Operator Detection**: Special attention to necessity/possibility
//// 4. **Compositional Structure**: Breaking down complex sentences
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/prompts
////
//// let prompt = prompts.build_translation_prompt(argument_text)
//// // Send to LLM with JSON mode enabled
//// ```

import gleam/list
import gleam/string
import modal_logic/proposition.{type LogicSystem}

// =============================================================================
// Types
// =============================================================================

/// Configuration for prompt generation
pub type PromptConfig {
  PromptConfig(
    /// Include examples in the prompt
    include_examples: Bool,
    /// Maximum number of premises to extract
    max_premises: Int,
    /// Preferred logic system (if known)
    preferred_logic: PromptLogicPreference,
    /// Level of detail in ambiguity reporting
    ambiguity_detail: AmbiguityDetail,
    /// Language for prompts (for future i18n)
    language: String,
  )
}

/// Logic system preference for prompts
pub type PromptLogicPreference {
  /// No preference, let LLM decide
  AutoDetect
  /// Suggest a specific system
  Suggest(LogicSystem)
  /// Require a specific system
  Require(LogicSystem)
}

/// Level of ambiguity detail in responses
pub type AmbiguityDetail {
  /// Only flag critical ambiguities
  Minimal
  /// Standard level of detail
  Standard
  /// Report all potential ambiguities
  Verbose
}

/// A complete prompt ready to send to LLM
pub type TranslationPrompt {
  TranslationPrompt(
    /// System message for context
    system_message: String,
    /// User message with the argument
    user_message: String,
    /// Expected JSON schema (for validation)
    response_schema: String,
  )
}

/// Structure for few-shot examples in prompts
pub type PromptExample {
  PromptExample(
    /// Natural language input
    input: String,
    /// Expected JSON output
    output: String,
    /// Explanation of the formalization
    explanation: String,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Create default prompt configuration
pub fn default_config() -> PromptConfig {
  PromptConfig(
    include_examples: True,
    max_premises: 10,
    preferred_logic: AutoDetect,
    ambiguity_detail: Standard,
    language: "en",
  )
}

/// Create minimal configuration (shorter prompts)
pub fn minimal_config() -> PromptConfig {
  PromptConfig(
    include_examples: False,
    max_premises: 5,
    preferred_logic: AutoDetect,
    ambiguity_detail: Minimal,
    language: "en",
  )
}

/// Create verbose configuration (more detailed analysis)
pub fn verbose_config() -> PromptConfig {
  PromptConfig(
    include_examples: True,
    max_premises: 20,
    preferred_logic: AutoDetect,
    ambiguity_detail: Verbose,
    language: "en",
  )
}

/// Set preferred logic system
pub fn with_logic_preference(
  config: PromptConfig,
  preference: PromptLogicPreference,
) -> PromptConfig {
  PromptConfig(..config, preferred_logic: preference)
}

/// Set ambiguity detail level
pub fn with_ambiguity_detail(
  config: PromptConfig,
  detail: AmbiguityDetail,
) -> PromptConfig {
  PromptConfig(..config, ambiguity_detail: detail)
}

// =============================================================================
// Prompt Building
// =============================================================================

/// Build a complete translation prompt for an argument
pub fn build_translation_prompt(
  config: PromptConfig,
  argument_text: String,
) -> TranslationPrompt {
  let system_message = build_system_message(config)
  let user_message = build_user_message(config, argument_text)
  let response_schema = get_response_schema()

  TranslationPrompt(
    system_message: system_message,
    user_message: user_message,
    response_schema: response_schema,
  )
}

/// Build the system message for LLM context
pub fn build_system_message(config: PromptConfig) -> String {
  let base_message =
    "You are an expert in formal logic, specializing in modal logic and "
    <> "argument formalization. Your task is to analyze natural language "
    <> "arguments and extract their logical structure.\n\n"
    <> "## Your Responsibilities\n\n"
    <> "1. Identify premises and conclusions in the argument\n"
    <> "2. Translate each statement into modal logic propositions\n"
    <> "3. Detect modal operators (necessity, possibility, obligation, etc.)\n"
    <> "4. Flag any ambiguities in interpretation\n"
    <> "5. Suggest the appropriate logic system\n\n"
    <> "## Output Format\n\n"
    <> "Always respond with valid JSON matching the specified schema. "
    <> "Do not include any text outside the JSON object.\n\n"

  let logic_guidance = case config.preferred_logic {
    AutoDetect ->
      "## Logic System Selection\n\n"
      <> "Choose the most appropriate logic system based on:\n"
      <> "- K: Base modal logic, minimal assumptions\n"
      <> "- T: When 'what is necessary is actual' applies\n"
      <> "- S4: When necessity is transitive\n"
      <> "- S5: When possibility/necessity are symmetric\n"
      <> "- KD: For deontic (obligation/permission) reasoning\n\n"
    Suggest(system) ->
      "## Logic System\n\n"
      <> "Consider using "
      <> logic_system_to_string(system)
      <> " logic, but adjust if the argument requires different properties.\n\n"
    Require(system) ->
      "## Logic System\n\n"
      <> "Use "
      <> logic_system_to_string(system)
      <> " logic for this formalization.\n\n"
  }

  let ambiguity_guidance = case config.ambiguity_detail {
    Minimal ->
      "## Ambiguity Handling\n\n"
      <> "Only report ambiguities that significantly affect validity.\n\n"
    Standard ->
      "## Ambiguity Handling\n\n"
      <> "Report ambiguities in modal operators, scope, and key terms.\n\n"
    Verbose ->
      "## Ambiguity Handling\n\n"
      <> "Report all potential ambiguities including:\n"
      <> "- Modal operator interpretation (epistemic vs alethic vs deontic)\n"
      <> "- Quantifier scope\n"
      <> "- Lexical ambiguity in key terms\n"
      <> "- Structural parsing alternatives\n\n"
  }

  let examples = case config.include_examples {
    True -> build_examples_section()
    False -> ""
  }

  base_message <> logic_guidance <> ambiguity_guidance <> examples
}

/// Build the user message with the argument
pub fn build_user_message(config: PromptConfig, argument_text: String) -> String {
  let max_premises_note = case config.max_premises > 0 {
    True ->
      "Extract up to " <> int_to_string(config.max_premises) <> " premises.\n\n"
    False -> ""
  }

  "Analyze the following argument and provide a formal logical structure:\n\n"
  <> "---\n"
  <> argument_text
  <> "\n---\n\n"
  <> max_premises_note
  <> "Respond with JSON matching the schema."
}

/// Build the examples section for few-shot learning
fn build_examples_section() -> String {
  let examples = get_standard_examples()

  "## Examples\n\n"
  <> string.join(
    list.map(examples, fn(ex) {
      "**Input:**\n"
      <> ex.input
      <> "\n\n**Output:**\n```json\n"
      <> ex.output
      <> "\n```\n\n**Explanation:** "
      <> ex.explanation
      <> "\n\n---\n\n"
    }),
    "",
  )
}

// =============================================================================
// Response Schema
// =============================================================================

/// Get the JSON schema for LLM responses
pub fn get_response_schema() -> String {
  "{
  \"type\": \"object\",
  \"required\": [\"premises\", \"conclusion\", \"logic_system\", \"confidence\"],
  \"properties\": {
    \"premises\": {
      \"type\": \"array\",
      \"items\": {
        \"type\": \"object\",
        \"required\": [\"natural_text\", \"formal\"],
        \"properties\": {
          \"natural_text\": {\"type\": \"string\"},
          \"formal\": {\"$ref\": \"#/definitions/proposition\"},
          \"source_sentence\": {\"type\": \"string\"}
        }
      }
    },
    \"conclusion\": {
      \"type\": \"object\",
      \"required\": [\"natural_text\", \"formal\"],
      \"properties\": {
        \"natural_text\": {\"type\": \"string\"},
        \"formal\": {\"$ref\": \"#/definitions/proposition\"},
        \"source_sentence\": {\"type\": \"string\"}
      }
    },
    \"logic_system\": {
      \"type\": \"string\",
      \"enum\": [\"K\", \"T\", \"K4\", \"S4\", \"S5\", \"KD\", \"KD45\"]
    },
    \"confidence\": {
      \"type\": \"number\",
      \"minimum\": 0,
      \"maximum\": 1
    },
    \"ambiguities\": {
      \"type\": \"array\",
      \"items\": {
        \"type\": \"object\",
        \"required\": [\"type\", \"description\"],
        \"properties\": {
          \"type\": {
            \"type\": \"string\",
            \"enum\": [\"modal\", \"scope\", \"lexical\", \"structural\"]
          },
          \"term\": {\"type\": \"string\"},
          \"description\": {\"type\": \"string\"},
          \"alternatives\": {
            \"type\": \"array\",
            \"items\": {\"type\": \"string\"}
          }
        }
      }
    },
    \"assumptions\": {
      \"type\": \"array\",
      \"items\": {\"type\": \"string\"}
    },
    \"notes\": {\"type\": \"string\"}
  },
  \"definitions\": {
    \"proposition\": {
      \"oneOf\": [
        {
          \"type\": \"object\",
          \"required\": [\"type\", \"name\"],
          \"properties\": {
            \"type\": {\"const\": \"atom\"},
            \"name\": {\"type\": \"string\"}
          }
        },
        {
          \"type\": \"object\",
          \"required\": [\"type\", \"operand\"],
          \"properties\": {
            \"type\": {\"enum\": [\"not\", \"necessary\", \"possible\", \"obligatory\", \"permitted\"]},
            \"operand\": {\"$ref\": \"#/definitions/proposition\"}
          }
        },
        {
          \"type\": \"object\",
          \"required\": [\"type\", \"left\", \"right\"],
          \"properties\": {
            \"type\": {\"enum\": [\"and\", \"or\", \"implies\"]},
            \"left\": {\"$ref\": \"#/definitions/proposition\"},
            \"right\": {\"$ref\": \"#/definitions/proposition\"}
          }
        },
        {
          \"type\": \"object\",
          \"required\": [\"type\", \"agent\", \"operand\"],
          \"properties\": {
            \"type\": {\"enum\": [\"knows\", \"believes\"]},
            \"agent\": {\"type\": \"string\"},
            \"operand\": {\"$ref\": \"#/definitions/proposition\"}
          }
        }
      ]
    }
  }
}"
}

// =============================================================================
// Standard Examples
// =============================================================================

/// Get standard few-shot examples for the prompt
pub fn get_standard_examples() -> List(PromptExample) {
  [
    // Simple propositional argument
    PromptExample(
      input: "All men are mortal. Socrates is a man. Therefore, Socrates is mortal.",
      output: "{
  \"premises\": [
    {
      \"natural_text\": \"All men are mortal\",
      \"formal\": {\"type\": \"implies\", \"left\": {\"type\": \"atom\", \"name\": \"man\"}, \"right\": {\"type\": \"atom\", \"name\": \"mortal\"}},
      \"source_sentence\": \"All men are mortal.\"
    },
    {
      \"natural_text\": \"Socrates is a man\",
      \"formal\": {\"type\": \"atom\", \"name\": \"socrates_is_man\"},
      \"source_sentence\": \"Socrates is a man.\"
    }
  ],
  \"conclusion\": {
    \"natural_text\": \"Socrates is mortal\",
    \"formal\": {\"type\": \"atom\", \"name\": \"socrates_is_mortal\"},
    \"source_sentence\": \"Therefore, Socrates is mortal.\"
  },
  \"logic_system\": \"K\",
  \"confidence\": 0.95,
  \"ambiguities\": [],
  \"assumptions\": [\"Treating 'all men' as a conditional rather than universal quantification\"],
  \"notes\": \"Classic syllogism formalized propositionally\"
}",
      explanation: "A classic syllogism. In propositional modal logic, we represent 'All X are Y' as X → Y.",
    ),
    // Modal argument with necessity
    PromptExample(
      input: "If it's necessary that all bachelors are unmarried, and John is a bachelor, then it's necessary that John is unmarried.",
      output: "{
  \"premises\": [
    {
      \"natural_text\": \"It is necessary that all bachelors are unmarried\",
      \"formal\": {\"type\": \"necessary\", \"operand\": {\"type\": \"implies\", \"left\": {\"type\": \"atom\", \"name\": \"bachelor\"}, \"right\": {\"type\": \"atom\", \"name\": \"unmarried\"}}},
      \"source_sentence\": \"If it's necessary that all bachelors are unmarried\"
    },
    {
      \"natural_text\": \"John is a bachelor\",
      \"formal\": {\"type\": \"atom\", \"name\": \"john_is_bachelor\"},
      \"source_sentence\": \"and John is a bachelor\"
    }
  ],
  \"conclusion\": {
    \"natural_text\": \"It is necessary that John is unmarried\",
    \"formal\": {\"type\": \"necessary\", \"operand\": {\"type\": \"atom\", \"name\": \"john_is_unmarried\"}},
    \"source_sentence\": \"then it's necessary that John is unmarried\"
  },
  \"logic_system\": \"S4\",
  \"confidence\": 0.85,
  \"ambiguities\": [
    {
      \"type\": \"modal\",
      \"term\": \"necessary\",
      \"description\": \"Could be logical necessity (analytic truth) or metaphysical necessity\",
      \"alternatives\": [\"logical\", \"metaphysical\"]
    }
  ],
  \"assumptions\": [\"Interpreting necessity as alethic modality\"],
  \"notes\": \"Requires K axiom for distribution of necessity\"
}",
      explanation: "Modal argument using the necessity operator □. Uses S4 for transitive accessibility.",
    ),
    // Deontic argument
    PromptExample(
      input: "You ought to keep your promises. You promised to help. So you ought to help.",
      output: "{
  \"premises\": [
    {
      \"natural_text\": \"You ought to keep your promises\",
      \"formal\": {\"type\": \"obligatory\", \"operand\": {\"type\": \"implies\", \"left\": {\"type\": \"atom\", \"name\": \"promised\"}, \"right\": {\"type\": \"atom\", \"name\": \"keep_promise\"}}},
      \"source_sentence\": \"You ought to keep your promises.\"
    },
    {
      \"natural_text\": \"You promised to help\",
      \"formal\": {\"type\": \"atom\", \"name\": \"promised_to_help\"},
      \"source_sentence\": \"You promised to help.\"
    }
  ],
  \"conclusion\": {
    \"natural_text\": \"You ought to help\",
    \"formal\": {\"type\": \"obligatory\", \"operand\": {\"type\": \"atom\", \"name\": \"help\"}},
    \"source_sentence\": \"So you ought to help.\"
  },
  \"logic_system\": \"KD\",
  \"confidence\": 0.80,
  \"ambiguities\": [
    {
      \"type\": \"scope\",
      \"description\": \"Scope of obligation: is 'ought' over the whole conditional or just consequent?\",
      \"alternatives\": [\"O(p → q)\", \"p → O(q)\"]
    }
  ],
  \"assumptions\": [\"Treating 'ought' as deontic obligation\"],
  \"notes\": \"Deontic reasoning requiring KD for serial accessibility\"
}",
      explanation: "Deontic argument using obligation. KD ensures no conflicting obligations.",
    ),
  ]
}

// =============================================================================
// Specialized Prompts
// =============================================================================

/// Build a prompt for ambiguity analysis only
pub fn build_ambiguity_prompt(argument_text: String) -> TranslationPrompt {
  let system_message =
    "You are an expert linguist and logician. Analyze the following argument "
    <> "for potential ambiguities that could affect its logical interpretation.\n\n"
    <> "Focus on:\n"
    <> "1. Modal words (must, should, can, might, etc.)\n"
    <> "2. Quantifier scope\n"
    <> "3. Word meanings that could differ\n"
    <> "4. Sentence structure alternatives\n\n"
    <> "Respond with JSON listing all ambiguities found."

  let user_message =
    "Identify all ambiguities in this argument:\n\n---\n"
    <> argument_text
    <> "\n---"

  let schema =
    "{
  \"type\": \"object\",
  \"required\": [\"ambiguities\"],
  \"properties\": {
    \"ambiguities\": {
      \"type\": \"array\",
      \"items\": {
        \"type\": \"object\",
        \"required\": [\"type\", \"term\", \"description\", \"impact\"],
        \"properties\": {
          \"type\": {\"enum\": [\"modal\", \"scope\", \"lexical\", \"structural\"]},
          \"term\": {\"type\": \"string\"},
          \"description\": {\"type\": \"string\"},
          \"impact\": {\"enum\": [\"high\", \"medium\", \"low\"]},
          \"alternatives\": {\"type\": \"array\", \"items\": {\"type\": \"string\"}}
        }
      }
    }
  }
}"

  TranslationPrompt(
    system_message: system_message,
    user_message: user_message,
    response_schema: schema,
  )
}

/// Build a prompt for logic system recommendation
pub fn build_logic_system_prompt(argument_text: String) -> TranslationPrompt {
  let system_message =
    "You are an expert in modal logic. Recommend the most appropriate "
    <> "modal logic system for formalizing the given argument.\n\n"
    <> "Consider:\n"
    <> "- K: Minimal modal logic\n"
    <> "- T: Reflexive (what's necessary is actual)\n"
    <> "- K4: Transitive (necessity iterates)\n"
    <> "- S4: Reflexive + Transitive\n"
    <> "- S5: Equivalence relation (symmetric necessity)\n"
    <> "- KD: Deontic (serial, no conflicts)\n"
    <> "- KD45: Full deontic logic\n\n"
    <> "Respond with JSON explaining your recommendation."

  let user_message =
    "What logic system best fits this argument?\n\n---\n"
    <> argument_text
    <> "\n---"

  let schema =
    "{
  \"type\": \"object\",
  \"required\": [\"recommended_system\", \"confidence\", \"reasoning\"],
  \"properties\": {
    \"recommended_system\": {\"enum\": [\"K\", \"T\", \"K4\", \"S4\", \"S5\", \"KD\", \"KD45\"]},
    \"confidence\": {\"type\": \"number\", \"minimum\": 0, \"maximum\": 1},
    \"reasoning\": {\"type\": \"string\"},
    \"modal_words_found\": {\"type\": \"array\", \"items\": {\"type\": \"string\"}},
    \"alternatives\": {
      \"type\": \"array\",
      \"items\": {
        \"type\": \"object\",
        \"properties\": {
          \"system\": {\"type\": \"string\"},
          \"reason\": {\"type\": \"string\"}
        }
      }
    }
  }
}"

  TranslationPrompt(
    system_message: system_message,
    user_message: user_message,
    response_schema: schema,
  )
}

/// Build a prompt for rephrasing an ambiguous argument
pub fn build_rephrase_prompt(
  argument_text: String,
  ambiguity_description: String,
) -> TranslationPrompt {
  let system_message =
    "You are helping to clarify an ambiguous logical argument. "
    <> "Given the original argument and a description of an ambiguity, "
    <> "provide alternative phrasings that resolve the ambiguity.\n\n"
    <> "For each alternative, explain which interpretation it represents."

  let user_message =
    "Original argument:\n---\n"
    <> argument_text
    <> "\n---\n\n"
    <> "Ambiguity identified:\n"
    <> ambiguity_description
    <> "\n\nProvide alternative phrasings that resolve this ambiguity."

  let schema =
    "{
  \"type\": \"object\",
  \"required\": [\"alternatives\"],
  \"properties\": {
    \"alternatives\": {
      \"type\": \"array\",
      \"items\": {
        \"type\": \"object\",
        \"required\": [\"rephrased_argument\", \"interpretation\"],
        \"properties\": {
          \"rephrased_argument\": {\"type\": \"string\"},
          \"interpretation\": {\"type\": \"string\"},
          \"changes_made\": {\"type\": \"array\", \"items\": {\"type\": \"string\"}}
        }
      }
    }
  }
}"

  TranslationPrompt(
    system_message: system_message,
    user_message: user_message,
    response_schema: schema,
  )
}

// =============================================================================
// Helpers
// =============================================================================

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
