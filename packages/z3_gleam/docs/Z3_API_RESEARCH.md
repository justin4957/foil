# Z3 API Research (B1.1)

This document provides a comprehensive overview of the Z3 C API for use in creating Gleam bindings.

## Overview

Z3 is a high-performance theorem prover from Microsoft Research. The C API provides low-level access to Z3's SMT solving capabilities.

## Key Concepts

### 1. Context (`Z3_context`)

The context is the central object that manages:
- Memory allocation for Z3 objects
- Global configuration settings
- Symbol management

```c
// Create a new context with default configuration
Z3_config cfg = Z3_mk_config();
Z3_context ctx = Z3_mk_context(cfg);
Z3_del_config(cfg);

// Clean up when done
Z3_del_context(ctx);
```

**Key Functions:**
- `Z3_mk_config()` - Create configuration
- `Z3_set_param_value(cfg, name, value)` - Set configuration parameter
- `Z3_mk_context(cfg)` - Create context
- `Z3_del_context(ctx)` - Delete context

### 2. Sorts (`Z3_sort`)

Sorts are Z3's type system. Every expression has a sort.

```c
// Built-in sorts
Z3_sort bool_sort = Z3_mk_bool_sort(ctx);
Z3_sort int_sort = Z3_mk_int_sort(ctx);
Z3_sort real_sort = Z3_mk_real_sort(ctx);

// Uninterpreted sort (user-defined type)
Z3_symbol sym = Z3_mk_string_symbol(ctx, "MyType");
Z3_sort my_sort = Z3_mk_uninterpreted_sort(ctx, sym);

// Array sort
Z3_sort arr_sort = Z3_mk_array_sort(ctx, int_sort, bool_sort);
```

**Key Functions:**
- `Z3_mk_bool_sort(ctx)` - Boolean sort
- `Z3_mk_int_sort(ctx)` - Integer sort
- `Z3_mk_real_sort(ctx)` - Real number sort
- `Z3_mk_bv_sort(ctx, sz)` - Bit-vector sort
- `Z3_mk_array_sort(ctx, domain, range)` - Array sort
- `Z3_mk_uninterpreted_sort(ctx, name)` - Uninterpreted sort

### 3. AST / Expressions (`Z3_ast`)

AST (Abstract Syntax Tree) nodes represent expressions, sorts, and other Z3 objects.

#### Constants and Variables

```c
// Create a boolean constant
Z3_symbol name = Z3_mk_string_symbol(ctx, "x");
Z3_ast x = Z3_mk_const(ctx, name, bool_sort);

// Create integer constants
Z3_ast five = Z3_mk_int(ctx, 5, int_sort);
Z3_ast y = Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, "y"), int_sort);
```

#### Boolean Operations

```c
// AND, OR, NOT
Z3_ast args[2] = {x, y};
Z3_ast and_expr = Z3_mk_and(ctx, 2, args);
Z3_ast or_expr = Z3_mk_or(ctx, 2, args);
Z3_ast not_x = Z3_mk_not(ctx, x);

// Implication and Equivalence
Z3_ast implies = Z3_mk_implies(ctx, x, y);
Z3_ast iff = Z3_mk_iff(ctx, x, y);
```

#### Arithmetic Operations

```c
// Addition, Multiplication
Z3_ast add_args[2] = {a, b};
Z3_ast sum = Z3_mk_add(ctx, 2, add_args);
Z3_ast product = Z3_mk_mul(ctx, 2, add_args);

// Subtraction, Division
Z3_ast sub_args[2] = {a, b};
Z3_ast diff = Z3_mk_sub(ctx, 2, sub_args);
Z3_ast quotient = Z3_mk_div(ctx, a, b);

// Unary minus
Z3_ast neg = Z3_mk_unary_minus(ctx, a);
```

#### Comparisons

```c
Z3_ast eq = Z3_mk_eq(ctx, a, b);      // a == b
Z3_ast lt = Z3_mk_lt(ctx, a, b);      // a < b
Z3_ast le = Z3_mk_le(ctx, a, b);      // a <= b
Z3_ast gt = Z3_mk_gt(ctx, a, b);      // a > b
Z3_ast ge = Z3_mk_ge(ctx, a, b);      // a >= b
```

#### Quantifiers

```c
// ForAll: ∀x. P(x)
Z3_sort sorts[1] = {int_sort};
Z3_symbol names[1] = {Z3_mk_string_symbol(ctx, "x")};
Z3_ast body = /* ... */;
Z3_ast forall = Z3_mk_forall(ctx, 0, 0, NULL, 1, sorts, names, body);

// Exists: ∃x. P(x)
Z3_ast exists = Z3_mk_exists(ctx, 0, 0, NULL, 1, sorts, names, body);
```

**Key Expression Functions:**
- `Z3_mk_const(ctx, name, sort)` - Named constant
- `Z3_mk_int(ctx, v, sort)` - Integer literal
- `Z3_mk_real(ctx, num, den)` - Rational literal
- `Z3_mk_true(ctx)` / `Z3_mk_false(ctx)` - Boolean literals
- `Z3_mk_and(ctx, n, args)` / `Z3_mk_or(ctx, n, args)` - N-ary boolean
- `Z3_mk_not(ctx, a)` - Negation
- `Z3_mk_implies(ctx, a, b)` / `Z3_mk_iff(ctx, a, b)` - Implication/equivalence
- `Z3_mk_add(ctx, n, args)` / `Z3_mk_mul(ctx, n, args)` - N-ary arithmetic
- `Z3_mk_sub(ctx, n, args)` / `Z3_mk_div(ctx, a, b)` - Subtraction/division
- `Z3_mk_eq(ctx, a, b)` - Equality
- `Z3_mk_lt/le/gt/ge(ctx, a, b)` - Comparisons
- `Z3_mk_forall/exists(ctx, ...)` - Quantifiers

### 4. Solver (`Z3_solver`)

The solver manages assertions and checks satisfiability.

```c
// Create solver
Z3_solver solver = Z3_mk_solver(ctx);
Z3_solver_inc_ref(ctx, solver);  // Increment reference count

// Add assertions
Z3_solver_assert(ctx, solver, constraint1);
Z3_solver_assert(ctx, solver, constraint2);

// Check satisfiability
Z3_lbool result = Z3_solver_check(ctx, solver);
// Z3_L_TRUE = satisfiable
// Z3_L_FALSE = unsatisfiable
// Z3_L_UNDEF = unknown

// Push/pop for incremental solving
Z3_solver_push(ctx, solver);
// ... add temporary assertions ...
Z3_solver_pop(ctx, solver, 1);

// Clean up
Z3_solver_dec_ref(ctx, solver);
```

**Key Solver Functions:**
- `Z3_mk_solver(ctx)` - Create solver
- `Z3_mk_simple_solver(ctx)` - Simple solver (no incremental)
- `Z3_solver_assert(ctx, s, a)` - Add assertion
- `Z3_solver_check(ctx, s)` - Check satisfiability
- `Z3_solver_check_assumptions(ctx, s, n, assumptions)` - Check with assumptions
- `Z3_solver_push(ctx, s)` / `Z3_solver_pop(ctx, s, n)` - Incremental solving
- `Z3_solver_reset(ctx, s)` - Clear all assertions
- `Z3_solver_get_model(ctx, s)` - Get satisfying model
- `Z3_solver_get_unsat_core(ctx, s)` - Get unsat core

### 5. Model (`Z3_model`)

When a formula is satisfiable, the model provides concrete values.

```c
if (Z3_solver_check(ctx, solver) == Z3_L_TRUE) {
    Z3_model model = Z3_solver_get_model(ctx, solver);
    Z3_model_inc_ref(ctx, model);

    // Evaluate an expression in the model
    Z3_ast result;
    if (Z3_model_eval(ctx, model, expr, true, &result)) {
        // result contains the value
    }

    // Get value of a constant
    Z3_func_decl decl = Z3_mk_func_decl(ctx, name, 0, NULL, sort);
    Z3_ast interp;
    if (Z3_model_get_const_interp(ctx, model, decl)) {
        // interp contains the value
    }

    Z3_model_dec_ref(ctx, model);
}
```

**Key Model Functions:**
- `Z3_solver_get_model(ctx, s)` - Get model from solver
- `Z3_model_eval(ctx, m, t, completion, result)` - Evaluate expression
- `Z3_model_get_const_interp(ctx, m, d)` - Get constant interpretation
- `Z3_model_get_num_consts(ctx, m)` - Number of constants
- `Z3_model_get_const_decl(ctx, m, i)` - Get i-th constant declaration
- `Z3_model_to_string(ctx, m)` - String representation

## Memory Management

Z3 uses reference counting for memory management:

### Reference Counting Rules

1. **AST nodes** are reference counted by the context
2. **Solvers, models, params** need explicit reference counting
3. Objects returned by Z3 functions are owned by Z3 until `inc_ref`

```c
// Increment reference (take ownership)
Z3_solver_inc_ref(ctx, solver);
Z3_model_inc_ref(ctx, model);
Z3_ast_vector_inc_ref(ctx, vec);

// Decrement reference (release ownership)
Z3_solver_dec_ref(ctx, solver);
Z3_model_dec_ref(ctx, model);
Z3_ast_vector_dec_ref(ctx, vec);
```

### Memory Safety Considerations for Bindings

1. **Context lifetime**: All Z3 objects are tied to a context. The context must outlive all objects created from it.

2. **AST sharing**: AST nodes can be safely shared within the same context.

3. **Thread safety**: Z3 contexts are NOT thread-safe. Use separate contexts for parallel solving.

4. **Error handling**: Use `Z3_get_error_code(ctx)` and `Z3_get_error_msg(ctx, code)` for error information.

## Required Functions Summary

### Initialization
- `Z3_mk_config`, `Z3_del_config`
- `Z3_set_param_value`
- `Z3_mk_context`, `Z3_del_context`

### Sorts
- `Z3_mk_bool_sort`, `Z3_mk_int_sort`, `Z3_mk_real_sort`
- `Z3_mk_bv_sort`, `Z3_mk_array_sort`
- `Z3_mk_uninterpreted_sort`

### Symbols
- `Z3_mk_string_symbol`, `Z3_mk_int_symbol`

### Expressions
- `Z3_mk_const`, `Z3_mk_true`, `Z3_mk_false`
- `Z3_mk_int`, `Z3_mk_real`
- `Z3_mk_and`, `Z3_mk_or`, `Z3_mk_not`
- `Z3_mk_implies`, `Z3_mk_iff`
- `Z3_mk_add`, `Z3_mk_sub`, `Z3_mk_mul`, `Z3_mk_div`
- `Z3_mk_eq`, `Z3_mk_lt`, `Z3_mk_le`, `Z3_mk_gt`, `Z3_mk_ge`
- `Z3_mk_forall`, `Z3_mk_exists`

### Solver
- `Z3_mk_solver`, `Z3_solver_inc_ref`, `Z3_solver_dec_ref`
- `Z3_solver_assert`, `Z3_solver_check`
- `Z3_solver_push`, `Z3_solver_pop`
- `Z3_solver_get_model`

### Model
- `Z3_model_inc_ref`, `Z3_model_dec_ref`
- `Z3_model_eval`
- `Z3_model_get_const_interp`
- `Z3_model_to_string`

### String Conversion
- `Z3_ast_to_string`
- `Z3_sort_to_string`
- `Z3_model_to_string`

### Error Handling
- `Z3_get_error_code`
- `Z3_get_error_msg`

## Use Cases for Foil

### 1. Constraint Solving
- Define variables with sorts
- Build constraint expressions
- Check satisfiability
- Extract solutions from models

### 2. Modal Logic (Kripke Frames)
- Encode possible worlds as uninterpreted sort
- Encode accessibility relation as function/array
- Encode propositions as predicates over worlds
- Translate modal operators to quantifiers

### 3. Theorem Proving
- Assert axioms
- Assert negation of theorem
- Check for unsatisfiability (proves theorem)

## References

- [Z3 C API Documentation](https://z3prover.github.io/api/html/group__capi.html)
- [Z3 GitHub Repository](https://github.com/Z3Prover/z3)
- [Z3 Guide](https://microsoft.github.io/z3guide/)
