# z3_gleam

[![Package Version](https://img.shields.io/hexpm/v/z3_gleam)](https://hex.pm/packages/z3_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/z3_gleam/)

A Gleam library for SMT solving and modal logic using Z3.

## Overview

z3_gleam provides a type-safe, idiomatic Gleam interface to the Z3 SMT solver. It supports:

- **SMT Expressions**: Boolean, integer, real, and array expressions
- **Modal Logic**: Kripke semantics with standard translation to FOL
- **Solver Configuration**: Timeout, memory limits, tactics, and more
- **Benchmarking**: Performance measurement and profiling utilities

## Installation

```sh
gleam add z3_gleam@1
```

## Quick Start

### Basic SMT Solving

```gleam
import z3/solver
import z3/expr
import z3/types

pub fn main() {
  // Create a solver
  let assert Ok(s) = solver.new()

  // Create integer variables
  let x = expr.int_const("x")
  let y = expr.int_const("y")

  // Add constraints: x > 0, y < 10, x + y = 15
  let assert Ok(s) = solver.assert_(s, expr.gt(x, expr.int(0)))
  let assert Ok(s) = solver.assert_(s, expr.lt(y, expr.int(10)))
  let assert Ok(s) = solver.assert_(s, expr.eq(expr.add([x, y]), expr.int(15)))

  // Check satisfiability
  case solver.check(s) {
    Ok(#(_, solver.SolverSat(model))) -> {
      io.println("SAT! Found a solution.")
    }
    Ok(#(_, solver.SolverUnsat)) -> {
      io.println("UNSAT - no solution exists")
    }
    _ -> io.println("Unknown or error")
  }
}
```

### Modal Logic

```gleam
import z3/modal/kripke
import z3/modal/frame_conditions.{S4}
import z3/modal/validity

pub fn main() {
  // Create a modal formula: □p → p (T-axiom)
  let p = kripke.atom("p")
  let box_p = kripke.box(p)
  let t_axiom = kripke.modal_implies(box_p, p)

  // Check validity in S4 (reflexive + transitive frames)
  let result = validity.check_validity(t_axiom, S4, 3)

  case result {
    validity.Valid -> io.println("Formula is valid in S4")
    validity.Invalid(model) -> {
      io.println("Countermodel found:")
      io.println(countermodel.format(model))
    }
    validity.Unknown(reason) -> io.println("Unknown: " <> reason)
  }
}
```

## Modules

### Core Modules

| Module | Description |
|--------|-------------|
| `z3/types` | Core types: Sort, Expr, Value, CheckResult |
| `z3/expr` | Expression construction utilities |
| `z3/solver` | Solver interface with push/pop, assertions |
| `z3/compile` | Expression compilation to SMT-LIB format |
| `z3/model` | Model evaluation and extraction |

### Modal Logic Modules

| Module | Description |
|--------|-------------|
| `z3/modal/kripke` | Kripke frame encoding, standard translation |
| `z3/modal/frame_conditions` | Modal systems: K, T, K4, S4, S5, KD, KD45, B, KB |
| `z3/modal/countermodel` | Kripke model extraction and queries |
| `z3/modal/validity` | High-level validity checking |

### Configuration Modules

| Module | Description |
|--------|-------------|
| `z3/config` | Comprehensive solver configuration |
| `z3/timeout` | Timeout handling (Z3 + BEAM) |
| `z3/benchmark` | Performance measurement utilities |

## SMT Basics Guide

### Sorts (Types)

```gleam
import z3/types.{BoolSort, IntSort, RealSort, ArraySort}

// Boolean sort
let bool_sort = BoolSort

// Integer sort
let int_sort = IntSort

// Real number sort
let real_sort = RealSort

// Array from Int to Bool
let array_sort = ArraySort(IntSort, BoolSort)
```

### Expressions

#### Literals

```gleam
import z3/types.{BoolLit, IntLit, RealLit}

let true_val = BoolLit(True)
let number = IntLit(42)
let fraction = RealLit(1, 2)  // 1/2
```

#### Variables (Constants)

```gleam
import z3/expr

let x = expr.int_const("x")
let y = expr.real_const("y")
let p = expr.bool_const("p")
```

#### Boolean Operations

```gleam
import z3/expr

let and_expr = expr.and_([p, q])
let or_expr = expr.or_([p, q])
let not_expr = expr.not_(p)
let implies = expr.implies(p, q)
let iff = expr.iff(p, q)
```

#### Arithmetic Operations

```gleam
import z3/expr

let sum = expr.add([x, y, z])
let product = expr.mul([x, y])
let difference = expr.sub(x, y)
let quotient = expr.div(x, y)
let negation = expr.neg(x)
```

#### Comparisons

```gleam
import z3/expr

let eq = expr.eq(x, y)
let lt = expr.lt(x, y)
let le = expr.le(x, y)
let gt = expr.gt(x, y)
let ge = expr.ge(x, y)
```

#### Quantifiers

```gleam
import z3/types.{ForAll, Exists, IntSort}

// ∀x. x > 0 → x + 1 > 0
let forall_expr = ForAll(
  [#("x", IntSort)],
  expr.implies(
    expr.gt(expr.int_const("x"), expr.int(0)),
    expr.gt(expr.add([expr.int_const("x"), expr.int(1)]), expr.int(0))
  )
)

// ∃x. x * x = 4
let exists_expr = Exists(
  [#("x", IntSort)],
  expr.eq(expr.mul([expr.int_const("x"), expr.int_const("x")]), expr.int(4))
)
```

## Modal Logic Guide

### Kripke Semantics

Modal logic is interpreted using Kripke semantics:

- **Worlds**: Possible states/situations
- **Accessibility Relation**: R(w, w') means w' is accessible from w
- **Valuation**: Which propositions hold at each world

### Modal Operators

| Operator | Meaning | Standard Translation |
|----------|---------|---------------------|
| □φ (Box) | "Necessarily φ" | ∀w'. R(w,w') → φ[w'] |
| ◇φ (Diamond) | "Possibly φ" | ∃w'. R(w,w') ∧ φ[w'] |

### Modal Systems

| System | Frame Conditions | Characteristic Axioms |
|--------|------------------|----------------------|
| K | None | K: □(p→q) → (□p→□q) |
| T | Reflexive | T: □p → p |
| K4 | Transitive | 4: □p → □□p |
| S4 | Reflexive + Transitive | T + 4 |
| S5 | Equivalence | T + 5: ◇p → □◇p |
| KD | Serial | D: □p → ◇p |
| B | Reflexive + Symmetric | T + B: p → □◇p |

### Example: Checking Modal Validity

```gleam
import z3/modal/kripke
import z3/modal/frame_conditions.{S4, S5, K}
import z3/modal/validity

// The 4-axiom: □p → □□p
let p = kripke.atom("p")
let axiom_4 = kripke.modal_implies(
  kripke.box(p),
  kripke.box(kripke.box(p))
)

// This is valid in S4 (which has transitivity)
let s4_result = validity.is_valid(axiom_4, S4, 3)
// Returns: True

// This is NOT valid in K (no frame conditions)
let k_result = validity.is_valid(axiom_4, K, 3)
// Returns: False (countermodel exists)
```

## Solver Configuration

### Timeout Configuration

```gleam
import z3/config
import z3/timeout

// Create configuration with 5 second timeout
let cfg = config.new()
  |> config.set_timeout(5000)

// Or use timeout presets
let fast = timeout.preset_fast()     // 1s Z3, 2s port
let normal = timeout.preset_normal() // 5s Z3, 6s port
let slow = timeout.preset_slow()     // 30s Z3, 35s port
```

### Memory Configuration

```gleam
import z3/config

let cfg = config.new()
  |> config.set_memory_limit(512)  // 512 MB limit
```

### Tactic Selection

```gleam
import z3/config.{TacticSAT, TacticSMT, TacticQFLIA}

// Use SAT tactic for propositional logic
let cfg = config.new()
  |> config.set_tactic(TacticSAT)

// Use linear integer arithmetic tactic
let cfg = config.new()
  |> config.set_tactic(TacticQFLIA)
```

### Model Options

```gleam
import z3/config

let cfg = config.new()
  |> config.enable_models()
  |> config.enable_model_completion()
  |> config.enable_model_validation()
```

### Configuration Presets

```gleam
import z3/config

let fast = config.preset_fast()           // Quick solving
let thorough = config.preset_thorough()   // Comprehensive
let debug = config.preset_debug()         // Full diagnostics
let repro = config.preset_reproducible(42) // Fixed seed
```

## Benchmarking

### Simple Benchmarks

```gleam
import z3/benchmark

// Run a single benchmark
let result = benchmark.run("my_operation", fn() {
  // expensive operation here
})

io.println(benchmark.format_result(result))
// Output: my_operation: 123µs (1 iterations)
```

### Statistical Benchmarks

```gleam
import z3/benchmark

// Collect statistics over multiple runs
let stats = benchmark.collect_stats("my_operation", 100, fn() {
  // operation to benchmark
})

io.println(benchmark.format_stats(stats))
// Output includes min, max, mean, median, p95, std_dev, ops/sec
```

### Benchmark Suites

```gleam
import z3/benchmark

let suite = benchmark.new_suite("SMT Operations")
  |> benchmark.collect_in_suite("expr_building", 100, build_expr)
  |> benchmark.collect_in_suite("sat_check", 50, check_sat)
  |> benchmark.collect_in_suite("model_eval", 100, eval_model)

io.println(benchmark.format_suite(suite))
```

## API Reference

### z3/types

#### Sort

```gleam
pub type Sort {
  BoolSort
  IntSort
  RealSort
  UninterpretedSort(name: String)
  ArraySort(domain: Sort, range: Sort)
}
```

#### Expr

```gleam
pub type Expr {
  BoolLit(Bool)
  IntLit(Int)
  RealLit(numerator: Int, denominator: Int)
  Const(name: String, sort: Sort)
  And(List(Expr))
  Or(List(Expr))
  Not(Expr)
  Implies(Expr, Expr)
  Iff(Expr, Expr)
  Ite(condition: Expr, then_branch: Expr, else_branch: Expr)
  ForAll(vars: List(#(String, Sort)), body: Expr)
  Exists(vars: List(#(String, Sort)), body: Expr)
  Add(List(Expr))
  Mul(List(Expr))
  Sub(Expr, Expr)
  Div(Expr, Expr)
  Neg(Expr)
  Eq(Expr, Expr)
  Lt(Expr, Expr)
  Le(Expr, Expr)
  Gt(Expr, Expr)
  Ge(Expr, Expr)
}
```

#### CheckResult

```gleam
pub type CheckResult {
  Sat(Model)
  Unsat
  Unknown(reason: String)
}
```

### z3/solver

#### Key Functions

| Function | Description |
|----------|-------------|
| `new()` | Create a new solver |
| `assert_(solver, expr)` | Add an assertion |
| `assert_all(solver, exprs)` | Add multiple assertions |
| `check(solver)` | Check satisfiability |
| `push(solver)` | Push a scope |
| `pop(solver)` | Pop a scope |
| `reset(solver)` | Reset all assertions |

### z3/modal/kripke

#### Key Functions

| Function | Description |
|----------|-------------|
| `new_context()` | Create Kripke context |
| `atom(name)` | Create atomic proposition |
| `box(formula)` | Necessity (□) |
| `diamond(formula)` | Possibility (◇) |
| `translate(ctx, formula, world)` | Standard translation |

### z3/modal/validity

#### Key Functions

| Function | Description |
|----------|-------------|
| `check_validity(formula, system, max_worlds)` | Check validity |
| `is_valid(formula, system, max_worlds)` | Boolean validity check |
| `is_satisfiable(formula, system, max_worlds)` | Check satisfiability |
| `are_equivalent(f1, f2, system, max_worlds)` | Check equivalence |

## Examples

See the `test/` directory for comprehensive examples:

- `z3_gleam_test.gleam` - Core API tests
- `z3_dialogue_test.gleam` - Z3 API dialogue demonstration
- `modal_dialogue_test.gleam` - Modal logic demonstration

## Development

```sh
gleam build  # Build the project
gleam test   # Run the tests
gleam run -m modal_dialogue_test  # Run modal logic demo
gleam run -m z3_dialogue_test     # Run Z3 API demo
```

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions welcome! Please open an issue or pull request on GitHub.

Further documentation can be found at <https://hexdocs.pm/z3_gleam>.
