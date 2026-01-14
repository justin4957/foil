//// Modal System Profile Management
////
//// This module handles loading and managing pre-configured profiles
//// for different modal logic systems (K, T, S4, S5, KD, KD45, K4).
////
//// ## Usage
////
//// ```gleam
//// import modal_logic/profile
////
//// // Load a profile by name
//// case profile.load("s5") {
////   Ok(profile) -> {
////     // Use profile configuration
////     profile.name  // "S5"
////     profile.axioms  // ["K", "T", "4", "5"]
////   }
////   Error(err) -> // Handle error
//// }
////
//// // List all available profiles
//// let profiles = profile.list_all()
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import simplifile

/// Modal system profile configuration
pub type Profile {
  Profile(
    name: String,
    description: String,
    axioms: List(String),
    frame_properties: List(String),
    default_timeout_ms: Int,
    verification_strategy: String,
    examples: List(ProfileExample),
    use_cases: List(String),
  )
}

/// Example formula for a profile
pub type ProfileExample {
  ProfileExample(
    description: String,
    formula: String,
    expected: String,
  )
}

/// Profile loading errors
pub type ProfileError {
  ProfileNotFound(name: String)
  ProfileParseError(name: String, details: String)
  ProfileIOError(name: String, details: String)
}

/// Path to profile configurations
const profiles_dir = "packages/modal_logic/config/profiles"

/// Available modal system profiles
const available_profiles = ["k", "t", "k4", "s4", "s5", "kd", "kd45"]

/// Load a profile by name
pub fn load(name: String) -> Result(Profile, ProfileError) {
  let normalized_name = string.lowercase(name)

  case list.contains(available_profiles, normalized_name) {
    False -> Error(ProfileNotFound(name))
    True -> {
      let file_path = profiles_dir <> "/" <> normalized_name <> ".json"

      case simplifile.read(file_path) {
        Error(_) ->
          Error(ProfileIOError(name, "Failed to read profile file"))
        Ok(content) -> parse_profile(name, content)
      }
    }
  }
}

/// Parse profile JSON content
/// Note: Simplified implementation - returns profile with name only
/// Full JSON parsing to be implemented with proper decoder library
fn parse_profile(name: String, _content: String) -> Result(Profile, ProfileError) {
  // Simplified implementation: create profile based on name
  case string.lowercase(name) {
    "k" ->
      Ok(Profile(
        name: "K",
        description: "Basic modal logic system",
        axioms: ["K"],
        frame_properties: [],
        default_timeout_ms: 60000,
        verification_strategy: "standard_translation",
        examples: [],
        use_cases: ["Basic modal reasoning"],
      ))
    "t" ->
      Ok(Profile(
        name: "T",
        description: "Modal logic with reflexive frames",
        axioms: ["K", "T"],
        frame_properties: ["reflexive"],
        default_timeout_ms: 60000,
        verification_strategy: "standard_translation",
        examples: [],
        use_cases: ["Alethic necessity"],
      ))
    "k4" ->
      Ok(Profile(
        name: "K4",
        description: "Modal logic with transitive frames",
        axioms: ["K", "4"],
        frame_properties: ["transitive"],
        default_timeout_ms: 60000,
        verification_strategy: "standard_translation",
        examples: [],
        use_cases: ["Provability logic"],
      ))
    "s4" ->
      Ok(Profile(
        name: "S4",
        description: "Modal logic with reflexive and transitive frames",
        axioms: ["K", "T", "4"],
        frame_properties: ["reflexive", "transitive"],
        default_timeout_ms: 60000,
        verification_strategy: "standard_translation",
        examples: [],
        use_cases: ["Epistemic logic"],
      ))
    "s5" ->
      Ok(Profile(
        name: "S5",
        description: "Modal logic with equivalence relation",
        axioms: ["K", "T", "4", "5"],
        frame_properties: ["reflexive", "symmetric", "transitive"],
        default_timeout_ms: 60000,
        verification_strategy: "standard_translation",
        examples: [],
        use_cases: ["Metaphysical necessity"],
      ))
    "kd" ->
      Ok(Profile(
        name: "KD",
        description: "Deontic logic with serial frames",
        axioms: ["K", "D"],
        frame_properties: ["serial"],
        default_timeout_ms: 60000,
        verification_strategy: "standard_translation",
        examples: [],
        use_cases: ["Deontic logic"],
      ))
    "kd45" ->
      Ok(Profile(
        name: "KD45",
        description: "Deontic S5",
        axioms: ["K", "D", "4", "5"],
        frame_properties: ["serial", "transitive", "euclidean"],
        default_timeout_ms: 60000,
        verification_strategy: "standard_translation",
        examples: [],
        use_cases: ["Advanced deontic reasoning"],
      ))
    _ -> Error(ProfileNotFound(name))
  }
}

/// List all available profile names
pub fn list_all() -> List(String) {
  available_profiles
}

/// Load all available profiles
pub fn load_all() -> Dict(String, Profile) {
  available_profiles
  |> list.filter_map(fn(name) {
    case load(name) {
      Ok(profile) -> Ok(#(name, profile))
      Error(_) -> Error(Nil)
    }
  })
  |> dict.from_list
}

/// Get profile information without loading full configuration
pub fn get_info(name: String) -> Result(String, ProfileError) {
  case load(name) {
    Ok(profile) -> Ok(profile.description)
    Error(err) -> Error(err)
  }
}

/// Check if a profile exists
pub fn exists(name: String) -> Bool {
  let normalized_name = string.lowercase(name)
  list.contains(available_profiles, normalized_name)
}

/// Format profile error for display
pub fn format_error(error: ProfileError) -> String {
  case error {
    ProfileNotFound(name) ->
      "Profile '" <> name <> "' not found. Available profiles: " <> string.join(available_profiles, ", ")

    ProfileParseError(name, details) ->
      "Failed to parse profile '" <> name <> "': " <> details

    ProfileIOError(name, details) ->
      "Failed to read profile '" <> name <> "': " <> details
  }
}
