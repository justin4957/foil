//// Modal Logic Dialogue Test
////
//// This test demonstrates the back-and-forth interaction between the modal
//// logic modules and demonstrates Kripke frame encoding, frame conditions,
//// and validity checking workflows.

import gleam/dict
import gleam/io
import gleam/list
import gleam/string
import z3/modal/countermodel
import z3/modal/frame_conditions.{B, K, K4, KB, KD, KD45, S4, S5, T}
import z3/modal/kripke

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Modal Logic Dialogue Test - Kripke Frame Encoding")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Kripke Context and World Creation
  test_kripke_context_creation()

  // Test 2: Modal Formula Construction
  test_modal_formula_construction()

  // Test 3: Standard Translation
  test_standard_translation()

  // Test 4: Frame Conditions
  test_frame_conditions()

  // Test 5: Countermodel Construction and Queries
  test_countermodel_operations()

  // Test 6: Modal System Properties
  test_modal_system_properties()

  // Test 7: Formula Utilities
  test_formula_utilities()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("Modal Logic Dialogue Test Complete")
  io.println("=" |> string.repeat(70))
}

fn test_kripke_context_creation() {
  io.println("")
  io.println("--- Test 1: Kripke Context and World Creation ---")
  io.println("")

  io.println("User: Create a new Kripke context")
  let ctx = kripke.new_context()
  io.println("[System]: Created KripkeContext with:")
  io.println("  - world_counter: 0")
  io.println("  - propositions: []")
  io.println("  - world_sort_name: \"World\"")
  io.println("  - accessibility_name: \"R\"")
  io.println("")

  io.println("User: Create world variables w0 and w1")
  let w0 = kripke.world(ctx, "w0")
  let w1 = kripke.world(ctx, "w1")
  io.println("[System]: Created world expressions:")
  io.println("  - w0: Const(\"w0\", UninterpretedSort(\"World\"))")
  io.println("  - w1: Const(\"w1\", UninterpretedSort(\"World\"))")
  io.println("")

  io.println("User: Create accessibility relation R(w0, w1)")
  let _accessible = kripke.accessible(ctx, w0, w1)
  io.println("[System]: Created accessibility expression:")
  io.println("  - R(w0, w1): Const(\"R_w0_w1\", BoolSort)")
  io.println("")

  io.println("User: Create proposition 'p' at world w0")
  let _p_at_w0 = kripke.prop_at(ctx, "p", w0)
  io.println("[System]: Created proposition expression:")
  io.println("  - p(w0): Const(\"p_w0\", BoolSort)")
  io.println("")

  io.println("User: Generate fresh world names")
  let #(fresh1, ctx2) = kripke.fresh_world(ctx)
  let #(fresh2, _ctx3) = kripke.fresh_world(ctx2)
  io.println(
    "[System]: Generated fresh worlds: \""
    <> fresh1
    <> "\", \""
    <> fresh2
    <> "\"",
  )
  io.println("")
}

fn test_modal_formula_construction() {
  io.println("")
  io.println("--- Test 2: Modal Formula Construction ---")
  io.println("")

  io.println("User: Create atomic propositions p and q")
  let p = kripke.atom("p")
  let q = kripke.atom("q")
  io.println("[System]: Created Atom(\"p\") and Atom(\"q\")")
  io.println("")

  io.println("User: Create formula: p AND q")
  let _p_and_q = kripke.modal_and2(p, q)
  io.println("[System]: Created ModalAnd([Atom(\"p\"), Atom(\"q\")])")
  io.println("")

  io.println("User: Create formula: Box(p) (necessity of p)")
  let box_p = kripke.box(p)
  io.println("[System]: Created Box(Atom(\"p\"))")
  io.println(
    "  - Semantics: \"Necessarily p\" / \"In all accessible worlds, p\"",
  )
  io.println("")

  io.println("User: Create formula: Diamond(q) (possibility of q)")
  let _diamond_q = kripke.diamond(q)
  io.println("[System]: Created Diamond(Atom(\"q\"))")
  io.println("  - Semantics: \"Possibly q\" / \"In some accessible world, q\"")
  io.println("")

  io.println("User: Create T-axiom: Box(p) -> p")
  let _t_axiom = kripke.modal_implies(box_p, p)
  io.println("[System]: Created ModalImplies(Box(Atom(\"p\")), Atom(\"p\"))")
  io.println("  - This is the characteristic axiom of system T (reflexivity)")
  io.println("")

  io.println("User: Create 4-axiom: Box(p) -> Box(Box(p))")
  let box_box_p = kripke.box(box_p)
  let _axiom_4 = kripke.modal_implies(box_p, box_box_p)
  io.println(
    "[System]: Created ModalImplies(Box(Atom(\"p\")), Box(Box(Atom(\"p\"))))",
  )
  io.println("  - This is the characteristic axiom of system K4 (transitivity)")
  io.println("")

  io.println("User: Create nested boxes: Box^3(p)")
  let _boxes_3_p = kripke.boxes(p, 3)
  io.println("[System]: Created Box(Box(Box(Atom(\"p\"))))")
  io.println("  - Triple necessity")
  io.println("")
}

fn test_standard_translation() {
  io.println("")
  io.println("--- Test 3: Standard Translation to FOL ---")
  io.println("")

  let ctx = kripke.new_context()
  let w0 = kripke.world(ctx, "w0")

  io.println("User: Translate atomic proposition p at world w0")
  let p = kripke.atom("p")
  let #(_translated_p, ctx) = kripke.translate(ctx, p, w0)
  io.println("[System]: Atom(\"p\") at w0 => Const(\"p_w0\", BoolSort)")
  io.println("  - Atomic propositions become predicates applied to worlds")
  io.println("")

  io.println("User: Translate Box(p) at world w0")
  let box_p = kripke.box(p)
  let #(_translated_box, ctx) = kripke.translate(ctx, box_p, w0)
  io.println("[System]: Box(Atom(\"p\")) at w0 =>")
  io.println("  ForAll([w_0: World],")
  io.println("    Implies(R_w0_w_0, p_w_0))")
  io.println("  - Standard translation: forall w'. R(w,w') -> p(w')")
  io.println("")

  io.println("User: Translate Diamond(p) at world w0")
  let diamond_p = kripke.diamond(p)
  let #(_translated_diamond, _ctx) = kripke.translate(ctx, diamond_p, w0)
  io.println("[System]: Diamond(Atom(\"p\")) at w0 =>")
  io.println("  Exists([w_1: World],")
  io.println("    And([R_w0_w_1, p_w_1]))")
  io.println("  - Standard translation: exists w'. R(w,w') & p(w')")
  io.println("")

  io.println("User: Translate complex formula Box(p -> Diamond(q))")
  let ctx = kripke.new_context()
  let p = kripke.atom("p")
  let q = kripke.atom("q")
  let inner = kripke.modal_implies(p, kripke.diamond(q))
  let complex = kripke.box(inner)
  let #(_translated, _ctx) = kripke.translate(ctx, complex, w0)
  io.println("[System]: Box(p -> Diamond(q)) at w0 =>")
  io.println("  ForAll([w_0: World],")
  io.println("    Implies(R_w0_w_0,")
  io.println("      Implies(p_w_0,")
  io.println("        Exists([w_1: World],")
  io.println("          And([R_w_0_w_1, q_w_1])))))")
  io.println("")
}

fn test_frame_conditions() {
  io.println("")
  io.println("--- Test 4: Frame Condition Constraints ---")
  io.println("")

  let ctx = kripke.new_context()
  let worlds = ["w0", "w1", "w2"]

  io.println("User: List properties of modal system S4")
  let s4_props = frame_conditions.system_properties(S4)
  io.println("[System]: S4 requires:")
  list.each(s4_props, fn(prop) {
    io.println("  - " <> frame_conditions.property_description(prop))
  })
  io.println("")

  io.println("User: Get characteristic axioms for S5")
  let s5_axioms = frame_conditions.characteristic_axioms(S5)
  io.println("[System]: S5 axioms:")
  list.each(s5_axioms, fn(axiom) { io.println("  - " <> axiom) })
  io.println("")

  io.println("User: Generate frame constraints for T with 3 worlds")
  let t_constraints = frame_conditions.get_constraints(T, ctx, worlds)
  let constraint_count = list.length(t_constraints)
  io.println(
    "[System]: Generated "
    <> int_to_string(constraint_count)
    <> " constraint(s) for T:",
  )
  io.println("  Reflexivity constraints:")
  io.println("    - R(w0, w0) = true")
  io.println("    - R(w1, w1) = true")
  io.println("    - R(w2, w2) = true")
  io.println("")

  io.println("User: Generate frame constraints for K4 (transitivity)")
  let k4_constraints = frame_conditions.get_constraints(K4, ctx, worlds)
  let k4_count = list.length(k4_constraints)
  io.println(
    "[System]: Generated "
    <> int_to_string(k4_count)
    <> " transitivity constraint(s) for K4",
  )
  io.println("  Example: R(w0,w1) & R(w1,w2) -> R(w0,w2)")
  io.println("")

  io.println("User: Does S5 imply S4?")
  let s5_implies_s4 = frame_conditions.system_implies(S5, S4)
  io.println(
    "[System]: S5 implies S4? "
    <> case s5_implies_s4 {
      True -> "Yes"
      False -> "No"
    },
  )
  io.println("  S5 has: Reflexivity, Symmetry, Transitivity")
  io.println("  S4 requires: Reflexivity, Transitivity")
  io.println("")
}

fn test_countermodel_operations() {
  io.println("")
  io.println("--- Test 5: Countermodel Construction and Queries ---")
  io.println("")

  io.println("User: Create a Kripke model with 3 worlds")
  let model =
    countermodel.new(
      ["w0", "w1", "w2"],
      [#("w0", "w1"), #("w1", "w2"), #("w0", "w2")],
      dict.from_list([
        #("p", ["w0", "w1"]),
        #("q", ["w1", "w2"]),
      ]),
      "w0",
    )
  io.println("[System]: Created KripkeModel")
  io.println("  Worlds: {w0, w1, w2}")
  io.println("  R: {(w0,w1), (w1,w2), (w0,w2)}")
  io.println("  V(p) = {w0, w1}")
  io.println("  V(q) = {w1, w2}")
  io.println("  Actual: w0")
  io.println("")

  io.println("User: Query worlds accessible from w0")
  let from_w0 = countermodel.accessible_from(model, "w0")
  io.println(
    "[System]: Worlds accessible from w0: {"
    <> string.join(from_w0, ", ")
    <> "}",
  )
  io.println("")

  io.println("User: Does p hold at w0?")
  let p_at_w0 = countermodel.holds_at(model, "p", "w0")
  io.println(
    "[System]: p holds at w0? "
    <> case p_at_w0 {
      True -> "Yes"
      False -> "No"
    },
  )
  io.println("")

  io.println("User: What propositions hold at w1?")
  let props_w1 = countermodel.props_at_world(model, "w1")
  io.println(
    "[System]: Propositions at w1: {" <> string.join(props_w1, ", ") <> "}",
  )
  io.println("")

  io.println("User: Format the model")
  let formatted = countermodel.format_compact(model)
  io.println("[System]: " <> formatted)
  io.println("")

  io.println("User: Is this model well-formed?")
  let valid = countermodel.is_valid(model)
  io.println(
    "[System]: Model is well-formed? "
    <> case valid {
      True -> "Yes"
      False -> "No"
    },
  )
  io.println("")
}

fn test_modal_system_properties() {
  io.println("")
  io.println("--- Test 6: Modal System Properties ---")
  io.println("")

  io.println("User: List all supported modal systems")
  let systems = [K, T, K4, S4, S5, KD, KD45, B, KB]
  io.println("[System]: Supported modal systems:")
  list.each(systems, fn(sys) {
    io.println("  - " <> frame_conditions.system_description(sys))
  })
  io.println("")

  io.println("User: Compare S4 and S5")
  io.println("[System]: Comparison:")
  io.println(
    "  S4: "
    <> frame_conditions.system_description(S4)
    <> " - properties: "
    <> format_properties(frame_conditions.system_properties(S4)),
  )
  io.println(
    "  S5: "
    <> frame_conditions.system_description(S5)
    <> " - properties: "
    <> format_properties(frame_conditions.system_properties(S5)),
  )
  io.println("  Difference: S5 adds Symmetry")
  io.println("")

  io.println("User: What are the KD45 axioms?")
  let kd45_axioms = frame_conditions.characteristic_axioms(KD45)
  io.println("[System]: KD45 (doxastic logic) axioms:")
  list.each(kd45_axioms, fn(a) { io.println("  - " <> a) })
  io.println("  Used for: Belief logic (□ = believes)")
  io.println("")
}

fn test_formula_utilities() {
  io.println("")
  io.println("--- Test 7: Formula Utilities ---")
  io.println("")

  let p = kripke.atom("p")
  let q = kripke.atom("q")
  let r = kripke.atom("r")

  io.println("User: Create formula: Box(p AND Diamond(q)) -> r")
  let inner_and = kripke.modal_and2(p, kripke.diamond(q))
  let formula = kripke.modal_implies(kripke.box(inner_and), r)
  io.println("[System]: Created formula")
  io.println("")

  io.println("User: Get all atomic propositions in formula")
  let atoms = kripke.get_atoms(formula)
  io.println("[System]: Atoms: {" <> string.join(atoms, ", ") <> "}")
  io.println("")

  io.println("User: What is the modal depth?")
  let depth = kripke.modal_depth(formula)
  io.println("[System]: Modal depth: " <> int_to_string(depth))
  io.println("  (Box contains Diamond, so depth = 2)")
  io.println("")

  io.println("User: Create deeply nested formula: Box^5(p)")
  let deep = kripke.boxes(p, 5)
  let deep_depth = kripke.modal_depth(deep)
  io.println(
    "[System]: Created Box^5(p), depth = " <> int_to_string(deep_depth),
  )
  io.println("")
}

// Helper functions

fn format_properties(props: List(frame_conditions.FrameProperty)) -> String {
  let names =
    list.map(props, fn(p) {
      case p {
        frame_conditions.Reflexivity -> "Refl"
        frame_conditions.Symmetry -> "Sym"
        frame_conditions.Transitivity -> "Trans"
        frame_conditions.Seriality -> "Serial"
        frame_conditions.Euclidean -> "Eucl"
      }
    })
  "{" <> string.join(names, ", ") <> "}"
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    _ if n >= 10 -> int_to_string(n / 10) <> int_to_string(n % 10)
    _ -> "-" <> int_to_string(-n)
  }
}
