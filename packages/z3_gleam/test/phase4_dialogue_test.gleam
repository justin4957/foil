//// Phase 4 Dialogue Test
////
//// This test demonstrates the back-and-forth interaction between the Phase 4
//// modules: timeout handling, solver configuration, and benchmarking.

import gleam/io
import gleam/string
import z3/benchmark
import z3/config
import z3/timeout

pub fn main() {
  io.println("=" |> string.repeat(70))
  io.println("Phase 4 Dialogue Test - Configuration & Optimization")
  io.println("=" |> string.repeat(70))
  io.println("")

  // Test 1: Timeout Configuration
  test_timeout_configuration()

  // Test 2: Solver Configuration
  test_solver_configuration()

  // Test 3: Benchmarking
  test_benchmarking()

  // Test 4: Configuration Presets
  test_configuration_presets()

  // Test 5: Timeout Statistics
  test_timeout_statistics()

  io.println("")
  io.println("=" |> string.repeat(70))
  io.println("Phase 4 Dialogue Test Complete")
  io.println("=" |> string.repeat(70))
}

fn test_timeout_configuration() {
  io.println("")
  io.println("--- Test 1: Timeout Configuration ---")
  io.println("")

  io.println("User: Create a default timeout configuration")
  let _default = timeout.default_config()
  io.println("[System]: Created TimeoutConfig")
  io.println("  z3_timeout_ms: 0 (unlimited)")
  io.println("  port_timeout_ms: 0 (unlimited)")
  io.println("  soft_timeout: false")
  io.println("")

  io.println("User: Create a timeout with 5 second Z3 timeout")
  let cfg = timeout.with_z3_timeout(5000)
  io.println("[System]: Created TimeoutConfig")
  io.println("  z3_timeout_ms: 5000")
  io.println("  port_timeout_ms: 6000 (auto-adjusted)")
  io.println("  soft_timeout: false")
  io.println("")

  io.println("User: Check if configuration has timeout")
  let has_timeout = timeout.has_timeout(cfg)
  io.println(
    "[System]: has_timeout = "
    <> case has_timeout {
      True -> "true"
      False -> "false"
    },
  )
  io.println("")

  io.println("User: Get effective timeout")
  case timeout.effective_timeout(cfg) {
    option.Some(ms) ->
      io.println("[System]: effective_timeout = " <> int_to_string(ms) <> "ms")
    option.None -> io.println("[System]: effective_timeout = None (unlimited)")
  }
  io.println("")

  io.println("User: Check remaining time after 3000ms elapsed")
  case timeout.remaining_time(cfg, 3000) {
    option.Some(ms) ->
      io.println("[System]: remaining_time = " <> int_to_string(ms) <> "ms")
    option.None -> io.println("[System]: remaining_time = None (unlimited)")
  }
  io.println("")

  io.println("User: Use preset_fast()")
  let _fast = timeout.preset_fast()
  io.println("[System]: Created fast preset")
  io.println("  z3_timeout_ms: 1000")
  io.println("  port_timeout_ms: 2000")
  io.println("")

  io.println("User: Generate Z3 parameters")
  let params = timeout.to_z3_params(cfg)
  io.println("[System]: Z3 parameters:")
  case params {
    [] -> io.println("  (none)")
    _ -> {
      io.println("  " <> format_params(params))
    }
  }
  io.println("")
}

fn test_solver_configuration() {
  io.println("")
  io.println("--- Test 2: Solver Configuration ---")
  io.println("")

  io.println("User: Create a new solver configuration")
  let cfg = config.new()
  io.println("[System]: Created Z3Config with defaults")
  io.println("  timeout: unlimited")
  io.println("  memory_limit: unlimited")
  io.println("  tactic: default")
  io.println("  models: enabled")
  io.println("  proofs: disabled")
  io.println("  unsat_core: disabled")
  io.println("")

  io.println("User: Set timeout to 5000ms")
  let cfg = config.set_timeout(cfg, 5000)
  io.println("[System]: timeout set to 5000ms")
  io.println("")

  io.println("User: Set memory limit to 512MB")
  let cfg = config.set_memory_limit(cfg, 512)
  io.println("[System]: memory_limit set to 512MB")
  io.println("")

  io.println("User: Set random seed for reproducibility")
  let cfg = config.set_random_seed(cfg, 42)
  io.println("[System]: random_seed set to 42")
  io.println("")

  io.println("User: Set tactic to SAT")
  let cfg = config.set_tactic(cfg, config.TacticSAT)
  io.println("[System]: tactic set to SAT")
  io.println("")

  io.println("User: Enable model completion")
  let cfg = config.enable_model_completion(cfg)
  io.println("[System]: model_completion enabled")
  io.println("")

  io.println("User: Enable unsat core extraction")
  let cfg = config.enable_unsat_core(cfg)
  io.println("[System]: unsat_core enabled")
  io.println("")

  io.println("User: Format configuration")
  let formatted = config.format(cfg)
  io.println("[System]: " <> formatted)
  io.println("")

  io.println("User: Validate configuration")
  case config.validate(cfg) {
    Ok(_) -> io.println("[System]: Configuration is valid")
    Error(errors) -> {
      io.println("[System]: Configuration errors:")
      print_errors(errors)
    }
  }
  io.println("")

  io.println("User: Generate Z3 parameters")
  let params = config.to_z3_params(cfg)
  io.println(
    "[System]: Generated " <> int_to_string(length(params)) <> " parameters",
  )
  io.println("")
}

fn test_benchmarking() {
  io.println("")
  io.println("--- Test 3: Benchmarking ---")
  io.println("")

  io.println("User: Run a simple benchmark")
  let result =
    benchmark.run("simple_add", fn() {
      // Simple operation
      1 + 1
    })
  io.println("[System]: " <> benchmark.format_result(result))
  io.println("")

  io.println("User: Run benchmark with 10 iterations")
  let result =
    benchmark.run_n("multiple_adds", 10, fn() {
      // Repeated operation
      let _ = 1 + 2 + 3 + 4 + 5
      Nil
    })
  io.println("[System]: " <> benchmark.format_result(result))
  io.println("")

  io.println("User: Collect statistics over 20 samples")
  let stats =
    benchmark.collect_stats("list_creation", 20, fn() {
      // Create a list
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    })
  io.println("[System]:")
  io.println(benchmark.format_stats(stats))
  io.println("")

  io.println("User: Create a benchmark suite")
  let _suite =
    benchmark.new_suite("Sample Operations")
    |> benchmark.run_in_suite("addition", fn() { 1 + 2 })
    |> benchmark.run_in_suite("multiplication", fn() { 3 * 4 })
  io.println("[System]: Created suite with 2 benchmarks")
  io.println("")

  io.println("User: Format duration examples")
  io.println("[System]: Duration formatting:")
  io.println("  100ns -> " <> benchmark.format_duration(100))
  io.println("  1500ns -> " <> benchmark.format_duration(1500))
  io.println("  1500000ns -> " <> benchmark.format_duration(1_500_000))
  io.println("  1500000000ns -> " <> benchmark.format_duration(1_500_000_000))
  io.println("")
}

fn test_configuration_presets() {
  io.println("")
  io.println("--- Test 4: Configuration Presets ---")
  io.println("")

  io.println("User: List available configuration presets")
  io.println("[System]: Available presets:")
  io.println("")

  io.println("  preset_fast():")
  let fast = config.preset_fast()
  io.println("    " <> config.format(fast))
  io.println("")

  io.println("  preset_thorough():")
  let thorough = config.preset_thorough()
  io.println("    " <> config.format(thorough))
  io.println("")

  io.println("  preset_debug():")
  let debug = config.preset_debug()
  io.println("    " <> config.format(debug))
  io.println("")

  io.println("  preset_reproducible(42):")
  let repro = config.preset_reproducible(42)
  io.println("    " <> config.format(repro))
  io.println("")

  io.println("  preset_low_memory():")
  let low_mem = config.preset_low_memory()
  io.println("    " <> config.format(low_mem))
  io.println("")
}

fn test_timeout_statistics() {
  io.println("")
  io.println("--- Test 5: Timeout Statistics ---")
  io.println("")

  io.println("User: Create empty timeout statistics")
  let stats = timeout.empty_stats()
  io.println("[System]: " <> timeout.format_stats(stats))
  io.println("")

  io.println("User: Record 3 completions (100ms, 150ms, 200ms)")
  let stats =
    stats
    |> timeout.record_completion(100)
    |> timeout.record_completion(150)
    |> timeout.record_completion(200)
  io.println("[System]: " <> timeout.format_stats(stats))
  io.println("")

  io.println("User: Record 1 timeout")
  let stats = timeout.record_timeout(stats)
  io.println("[System]: " <> timeout.format_stats(stats))
  io.println("")

  io.println("User: Calculate timeout rate")
  let rate = timeout.timeout_rate(stats)
  io.println("[System]: timeout_rate = " <> float_to_string_approx(rate) <> "%")
  io.println("")
}

// Helper functions

fn format_params(params: List(#(String, String))) -> String {
  case params {
    [] -> ""
    [#(k, v)] -> k <> "=" <> v
    [#(k, v), ..rest] -> k <> "=" <> v <> ", " <> format_params(rest)
  }
}

fn print_errors(errors: List(String)) -> Nil {
  case errors {
    [] -> Nil
    [e, ..rest] -> {
      io.println("    - " <> e)
      print_errors(rest)
    }
  }
}

fn length(list: List(a)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> 1 + length(rest)
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
  // Simple approximation for display
  let int_part = float_to_int(f)
  int_to_string(int_part)
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

import gleam/option
