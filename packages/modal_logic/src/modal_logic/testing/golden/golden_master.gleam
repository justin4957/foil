//// Golden Master Testing for Modal Logic Translation
////
//// This module manages golden master snapshots that capture approved
//// translation outputs. Tests compare current outputs against these
//// approved baselines to detect regressions.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import modal_logic/proposition.{type Proposition}

/// A golden master snapshot
pub type GoldenSnapshot {
  GoldenSnapshot(
    /// Unique identifier for this snapshot
    id: String,
    /// The fixture ID this snapshot belongs to
    fixture_id: String,
    /// Approved translated premises
    approved_premises: List(Proposition),
    /// Approved translated conclusion
    approved_conclusion: Proposition,
    /// When this snapshot was approved
    approved_at: String,
    /// Who/what approved this snapshot
    approved_by: String,
    /// Version number for tracking updates
    version: Int,
    /// Notes about this approval
    notes: Option(String),
  )
}

/// Result of comparing against golden master
pub type GoldenComparison {
  /// Output matches the golden master exactly
  ExactMatch
  /// Output differs from golden master
  Mismatch(differences: List(GoldenDifference))
  /// No golden master exists for this fixture
  NoBaseline
  /// Multiple golden masters exist (valid alternatives)
  MultipleValid(matched_index: Int)
}

/// A specific difference from the golden master
pub type GoldenDifference {
  /// Premise count differs
  PremiseCountDiff(expected: Int, actual: Int)
  /// Premise at index differs
  PremiseDiff(index: Int, expected: Proposition, actual: Proposition)
  /// Conclusion differs
  ConclusionDiff(expected: Proposition, actual: Proposition)
}

/// Golden master store
pub type GoldenStore {
  GoldenStore(
    /// Snapshots keyed by fixture ID
    snapshots: Dict(String, List(GoldenSnapshot)),
    /// Pending approvals (outputs awaiting review)
    pending: List(PendingApproval),
    /// History of changes
    history: List(HistoryEntry),
  )
}

/// An output pending approval as a new golden master
pub type PendingApproval {
  PendingApproval(
    fixture_id: String,
    premises: List(Proposition),
    conclusion: Proposition,
    generated_at: String,
    reason: String,
  )
}

/// History of golden master changes
pub type HistoryEntry {
  HistoryEntry(
    timestamp: String,
    action: HistoryAction,
    fixture_id: String,
    notes: Option(String),
  )
}

/// Types of history actions
pub type HistoryAction {
  Created
  Updated
  Deleted
  ApprovedPending
  RejectedPending
}

/// Create a new empty golden store
pub fn new_store() -> GoldenStore {
  GoldenStore(snapshots: dict.new(), pending: [], history: [])
}

/// Create a store with pre-loaded golden masters
pub fn with_defaults() -> GoldenStore {
  let snapshots = load_default_snapshots()
  GoldenStore(snapshots: snapshots, pending: [], history: [])
}

/// Compare an output against the golden master
pub fn compare(
  store: GoldenStore,
  fixture_id: String,
  premises: List(Proposition),
  conclusion: Proposition,
) -> GoldenComparison {
  case dict.get(store.snapshots, fixture_id) {
    Error(_) -> NoBaseline
    Ok([]) -> NoBaseline
    Ok(snapshots) ->
      compare_against_snapshots(snapshots, premises, conclusion, 0)
  }
}

/// Compare against a list of valid snapshots
fn compare_against_snapshots(
  snapshots: List(GoldenSnapshot),
  premises: List(Proposition),
  conclusion: Proposition,
  index: Int,
) -> GoldenComparison {
  case snapshots {
    [] -> {
      // No match found, generate differences from first snapshot
      Mismatch(differences: [])
    }
    [snapshot, ..rest] -> {
      let differences = compute_differences(snapshot, premises, conclusion)
      case differences {
        [] -> {
          case index {
            0 -> ExactMatch
            _ -> MultipleValid(matched_index: index)
          }
        }
        _ -> compare_against_snapshots(rest, premises, conclusion, index + 1)
      }
    }
  }
}

/// Compute differences between output and golden snapshot
fn compute_differences(
  snapshot: GoldenSnapshot,
  premises: List(Proposition),
  conclusion: Proposition,
) -> List(GoldenDifference) {
  let premise_count_diff = case
    list.length(snapshot.approved_premises) == list.length(premises)
  {
    True -> []
    False -> [
      PremiseCountDiff(
        expected: list.length(snapshot.approved_premises),
        actual: list.length(premises),
      ),
    ]
  }

  let premise_diffs =
    list.zip(snapshot.approved_premises, premises)
    |> list.index_map(fn(pair, idx) {
      let #(expected, actual) = pair
      case propositions_equal(expected, actual) {
        True -> None
        False ->
          Some(PremiseDiff(index: idx, expected: expected, actual: actual))
      }
    })
    |> list.filter_map(fn(x) { option.to_result(x, Nil) })

  let conclusion_diff = case
    propositions_equal(snapshot.approved_conclusion, conclusion)
  {
    True -> []
    False -> [
      ConclusionDiff(expected: snapshot.approved_conclusion, actual: conclusion),
    ]
  }

  list.flatten([premise_count_diff, premise_diffs, conclusion_diff])
}

/// Check if two propositions are structurally equal
fn propositions_equal(a: Proposition, b: Proposition) -> Bool {
  case a, b {
    proposition.Atom(n1), proposition.Atom(n2) -> n1 == n2
    proposition.Not(p1), proposition.Not(p2) -> propositions_equal(p1, p2)
    proposition.And(l1, r1), proposition.And(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    proposition.Or(l1, r1), proposition.Or(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    proposition.Implies(l1, r1), proposition.Implies(l2, r2) ->
      propositions_equal(l1, l2) && propositions_equal(r1, r2)
    proposition.Necessary(p1), proposition.Necessary(p2) ->
      propositions_equal(p1, p2)
    proposition.Possible(p1), proposition.Possible(p2) ->
      propositions_equal(p1, p2)
    proposition.Obligatory(p1), proposition.Obligatory(p2) ->
      propositions_equal(p1, p2)
    proposition.Permitted(p1), proposition.Permitted(p2) ->
      propositions_equal(p1, p2)
    proposition.Knows(a1, p1), proposition.Knows(a2, p2) ->
      a1 == a2 && propositions_equal(p1, p2)
    proposition.Believes(a1, p1), proposition.Believes(a2, p2) ->
      a1 == a2 && propositions_equal(p1, p2)
    _, _ -> False
  }
}

/// Add a new golden snapshot
pub fn add_snapshot(store: GoldenStore, snapshot: GoldenSnapshot) -> GoldenStore {
  let existing =
    result.unwrap(dict.get(store.snapshots, snapshot.fixture_id), [])
  let updated =
    dict.insert(store.snapshots, snapshot.fixture_id, [snapshot, ..existing])
  let history_entry =
    HistoryEntry(
      timestamp: snapshot.approved_at,
      action: Created,
      fixture_id: snapshot.fixture_id,
      notes: snapshot.notes,
    )
  GoldenStore(..store, snapshots: updated, history: [
    history_entry,
    ..store.history
  ])
}

/// Update an existing golden snapshot
pub fn update_snapshot(
  store: GoldenStore,
  fixture_id: String,
  new_snapshot: GoldenSnapshot,
) -> GoldenStore {
  let updated = dict.insert(store.snapshots, fixture_id, [new_snapshot])
  let history_entry =
    HistoryEntry(
      timestamp: new_snapshot.approved_at,
      action: Updated,
      fixture_id: fixture_id,
      notes: Some("Updated to version " <> int.to_string(new_snapshot.version)),
    )
  GoldenStore(..store, snapshots: updated, history: [
    history_entry,
    ..store.history
  ])
}

/// Add a pending approval
pub fn add_pending(
  store: GoldenStore,
  fixture_id: String,
  premises: List(Proposition),
  conclusion: Proposition,
  reason: String,
) -> GoldenStore {
  let pending =
    PendingApproval(
      fixture_id: fixture_id,
      premises: premises,
      conclusion: conclusion,
      generated_at: "now",
      reason: reason,
    )
  GoldenStore(..store, pending: [pending, ..store.pending])
}

/// Approve a pending item as a new golden master
pub fn approve_pending(
  store: GoldenStore,
  fixture_id: String,
  approved_by: String,
) -> Result(GoldenStore, String) {
  let matching =
    list.filter(store.pending, fn(p) { p.fixture_id == fixture_id })

  case matching {
    [] -> Error("No pending approval for fixture: " <> fixture_id)
    [pending, ..] -> {
      let snapshot =
        GoldenSnapshot(
          id: fixture_id <> "_golden",
          fixture_id: fixture_id,
          approved_premises: pending.premises,
          approved_conclusion: pending.conclusion,
          approved_at: "now",
          approved_by: approved_by,
          version: 1,
          notes: Some("Approved from pending: " <> pending.reason),
        )

      let remaining_pending =
        list.filter(store.pending, fn(p) { p.fixture_id != fixture_id })

      let updated_store =
        GoldenStore(..store, pending: remaining_pending)
        |> add_snapshot(snapshot)

      Ok(updated_store)
    }
  }
}

/// Reject a pending item
pub fn reject_pending(store: GoldenStore, fixture_id: String) -> GoldenStore {
  let remaining =
    list.filter(store.pending, fn(p) { p.fixture_id != fixture_id })
  let history_entry =
    HistoryEntry(
      timestamp: "now",
      action: RejectedPending,
      fixture_id: fixture_id,
      notes: None,
    )
  GoldenStore(..store, pending: remaining, history: [
    history_entry,
    ..store.history
  ])
}

/// Get all pending approvals
pub fn get_pending(store: GoldenStore) -> List(PendingApproval) {
  store.pending
}

/// Get history for a fixture
pub fn get_history(store: GoldenStore, fixture_id: String) -> List(HistoryEntry) {
  list.filter(store.history, fn(h) { h.fixture_id == fixture_id })
}

/// Get snapshot count
pub fn snapshot_count(store: GoldenStore) -> Int {
  dict.fold(store.snapshots, 0, fn(acc, _key, snapshots) {
    acc + list.length(snapshots)
  })
}

/// Load default golden master snapshots
fn load_default_snapshots() -> Dict(String, List(GoldenSnapshot)) {
  // Pre-approved golden masters for classic arguments
  let snapshots = [
    // Modus Ponens
    GoldenSnapshot(
      id: "modus_ponens_golden",
      fixture_id: "modus_ponens",
      approved_premises: [
        proposition.Implies(proposition.Atom("p"), proposition.Atom("q")),
        proposition.Atom("p"),
      ],
      approved_conclusion: proposition.Atom("q"),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Classic modus ponens"),
    ),
    // Modal Modus Ponens
    GoldenSnapshot(
      id: "modal_modus_ponens_golden",
      fixture_id: "modal_modus_ponens",
      approved_premises: [
        proposition.Necessary(proposition.Implies(
          proposition.Atom("p"),
          proposition.Atom("q"),
        )),
        proposition.Necessary(proposition.Atom("p")),
      ],
      approved_conclusion: proposition.Necessary(proposition.Atom("q")),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("K axiom application"),
    ),
    // Disjunctive Syllogism
    GoldenSnapshot(
      id: "disjunctive_syllogism_golden",
      fixture_id: "disjunctive_syllogism",
      approved_premises: [
        proposition.Or(proposition.Atom("p"), proposition.Atom("q")),
        proposition.Not(proposition.Atom("p")),
      ],
      approved_conclusion: proposition.Atom("q"),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Classic disjunctive syllogism"),
    ),
    // Hypothetical Syllogism
    GoldenSnapshot(
      id: "hypothetical_syllogism_golden",
      fixture_id: "hypothetical_syllogism",
      approved_premises: [
        proposition.Implies(proposition.Atom("p"), proposition.Atom("q")),
        proposition.Implies(proposition.Atom("q"), proposition.Atom("r")),
      ],
      approved_conclusion: proposition.Implies(
        proposition.Atom("p"),
        proposition.Atom("r"),
      ),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Transitivity of implication"),
    ),
    // T Axiom
    GoldenSnapshot(
      id: "t_axiom_golden",
      fixture_id: "t_axiom",
      approved_premises: [proposition.Necessary(proposition.Atom("p"))],
      approved_conclusion: proposition.Atom("p"),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Reflexivity: □p → p"),
    ),
    // D Axiom
    GoldenSnapshot(
      id: "d_axiom_golden",
      fixture_id: "d_axiom",
      approved_premises: [proposition.Obligatory(proposition.Atom("p"))],
      approved_conclusion: proposition.Permitted(proposition.Atom("p")),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Deontic: Op → Pp"),
    ),
    // Knowledge entails belief
    GoldenSnapshot(
      id: "knowledge_belief_golden",
      fixture_id: "knowledge_implies_belief",
      approved_premises: [proposition.Knows("a", proposition.Atom("p"))],
      approved_conclusion: proposition.Believes("a", proposition.Atom("p")),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Epistemic: Kp → Bp"),
    ),
    // Double negation
    GoldenSnapshot(
      id: "double_negation_golden",
      fixture_id: "double_negation",
      approved_premises: [
        proposition.Not(proposition.Not(proposition.Atom("p"))),
      ],
      approved_conclusion: proposition.Atom("p"),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Double negation elimination"),
    ),
    // S5: ◇p → □◇p
    GoldenSnapshot(
      id: "s5_characteristic_golden",
      fixture_id: "s5_characteristic",
      approved_premises: [proposition.Possible(proposition.Atom("p"))],
      approved_conclusion: proposition.Necessary(
        proposition.Possible(proposition.Atom("p")),
      ),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("S5 characteristic axiom"),
    ),
    // Affirming the consequent (invalid)
    GoldenSnapshot(
      id: "affirming_consequent_golden",
      fixture_id: "affirming_consequent",
      approved_premises: [
        proposition.Implies(proposition.Atom("p"), proposition.Atom("q")),
        proposition.Atom("q"),
      ],
      approved_conclusion: proposition.Atom("p"),
      approved_at: "2024-01-15T00:00:00Z",
      approved_by: "system",
      version: 1,
      notes: Some("Invalid: affirming the consequent"),
    ),
  ]

  snapshots
  |> list.fold(dict.new(), fn(d, snapshot) {
    let existing = result.unwrap(dict.get(d, snapshot.fixture_id), [])
    dict.insert(d, snapshot.fixture_id, [snapshot, ..existing])
  })
}

/// Format comparison result as string
pub fn comparison_to_string(comparison: GoldenComparison) -> String {
  case comparison {
    ExactMatch -> "Exact match with golden master"
    NoBaseline -> "No golden master baseline exists"
    MultipleValid(index) -> "Matched alternative #" <> int.to_string(index + 1)
    Mismatch(diffs) -> {
      let diff_strs =
        diffs
        |> list.map(difference_to_string)
        |> string.join("; ")
      "Mismatch: " <> diff_strs
    }
  }
}

/// Format a difference as string
fn difference_to_string(diff: GoldenDifference) -> String {
  case diff {
    PremiseCountDiff(expected, actual) ->
      "premise count ("
      <> int.to_string(expected)
      <> " vs "
      <> int.to_string(actual)
      <> ")"
    PremiseDiff(index, _, _) ->
      "premise #" <> int.to_string(index + 1) <> " differs"
    ConclusionDiff(_, _) -> "conclusion differs"
  }
}
