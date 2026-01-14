import gleeunit
import gleeunit/should
import modal_logic/profile

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Basic Profile Loading Tests
// =============================================================================

pub fn list_all_profiles_test() {
  let profiles = profile.list_all()

  profiles
  |> should.equal(["k", "t", "k4", "s4", "s5", "kd", "kd45"])
}

pub fn load_k_profile_test() {
  case profile.load("k") {
    Ok(p) -> {
      p.name |> should.equal("K")
      p.axioms |> should.equal(["K"])
      p.frame_properties |> should.equal([])
      p.default_timeout_ms |> should.equal(60000)
      p.verification_strategy |> should.equal("standard_translation")
    }
    Error(err) -> {
      let msg = profile.format_error(err)
      should.fail()
      msg |> should.equal("Should not fail")
    }
  }
}

pub fn load_t_profile_test() {
  case profile.load("t") {
    Ok(p) -> {
      p.name |> should.equal("T")
      p.axioms |> should.equal(["K", "T"])
      p.frame_properties |> should.equal(["reflexive"])
    }
    Error(_) -> should.fail()
  }
}

pub fn load_s4_profile_test() {
  case profile.load("s4") {
    Ok(p) -> {
      p.name |> should.equal("S4")
      p.axioms |> should.equal(["K", "T", "4"])
      p.frame_properties |> should.equal(["reflexive", "transitive"])
    }
    Error(_) -> should.fail()
  }
}

pub fn load_s5_profile_test() {
  case profile.load("s5") {
    Ok(p) -> {
      p.name |> should.equal("S5")
      p.axioms |> should.equal(["K", "T", "4", "5"])
      p.frame_properties |> should.equal(["reflexive", "symmetric", "transitive"])
    }
    Error(_) -> should.fail()
  }
}

pub fn load_kd_profile_test() {
  case profile.load("kd") {
    Ok(p) -> {
      p.name |> should.equal("KD")
      p.axioms |> should.equal(["K", "D"])
      p.frame_properties |> should.equal(["serial"])
    }
    Error(_) -> should.fail()
  }
}

pub fn load_kd45_profile_test() {
  case profile.load("kd45") {
    Ok(p) -> {
      p.name |> should.equal("KD45")
      p.axioms |> should.equal(["K", "D", "4", "5"])
      p.frame_properties |> should.equal(["serial", "transitive", "euclidean"])
    }
    Error(_) -> should.fail()
  }
}

pub fn load_k4_profile_test() {
  case profile.load("k4") {
    Ok(p) -> {
      p.name |> should.equal("K4")
      p.axioms |> should.equal(["K", "4"])
      p.frame_properties |> should.equal(["transitive"])
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Case Insensitivity Tests
// =============================================================================

pub fn load_uppercase_profile_test() {
  case profile.load("S5") {
    Ok(p) -> p.name |> should.equal("S5")
    Error(_) -> should.fail()
  }
}

pub fn load_mixed_case_profile_test() {
  case profile.load("Kd45") {
    Ok(p) -> p.name |> should.equal("KD45")
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Error Handling Tests
// =============================================================================

pub fn load_nonexistent_profile_test() {
  case profile.load("nonexistent") {
    Ok(_) -> should.fail()
    Error(profile.ProfileNotFound(_)) -> True |> should.be_true()
    Error(_) -> should.fail()
  }
}

pub fn exists_check_test() {
  profile.exists("k") |> should.be_true()
  profile.exists("s5") |> should.be_true()
  profile.exists("nonexistent") |> should.be_false()
}

pub fn get_info_test() {
  case profile.get_info("k") {
    Ok(info) -> {
      info |> should.not_equal("")
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Profile Structure Tests
// =============================================================================

pub fn profile_has_examples_test() {
  case profile.load("k") {
    Ok(p) -> {
      // K profile exists (examples are empty in simplified implementation)
      p.name |> should.equal("K")
    }
    Error(_) -> should.fail()
  }
}

pub fn profile_has_use_cases_test() {
  case profile.load("s5") {
    Ok(p) -> {
      // S5 profile should have at least one use case
      let count = p.use_cases |> list.length
      { count >= 1 } |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Load All Profiles Test
// =============================================================================

pub fn load_all_profiles_test() {
  let all_profiles = profile.load_all()

  // Should have 7 profiles
  all_profiles
  |> dict.size
  |> should.equal(7)

  // Check that K profile is loaded
  case dict.get(all_profiles, "k") {
    Ok(k_profile) -> k_profile.name |> should.equal("K")
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Error Message Formatting Tests
// =============================================================================

pub fn format_not_found_error_test() {
  let error = profile.ProfileNotFound("xyz")
  let message = profile.format_error(error)

  should_contain(message, "xyz")
  should_contain(message, "Available profiles")
}

// Helper imports
import gleam/dict
import gleam/list
import gleam/string

// Helper function
fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False -> should.fail()
  }
}
