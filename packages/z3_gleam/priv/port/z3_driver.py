#!/usr/bin/env python3
"""
Z3 Port Driver for Gleam

This script provides a JSON-based interface to Z3 for the Gleam port implementation.
It reads JSON commands from stdin and writes JSON responses to stdout.

Protocol:
- Each message is a JSON object on a single line
- Request format: {"id": int, "cmd": string, ...params}
- Response format: {"id": int, "ok": true, ...result} or {"id": int, "error": string}

Commands:
- new_context: Create a new Z3 context
- del_context: Delete a context
- new_solver: Create a solver for a context
- assert: Add an assertion to a solver
- check: Check satisfiability
- get_model: Get the model (after sat result)
- push: Push a scope
- pop: Pop n scopes

Usage:
    python3 z3_driver.py
"""

import json
import sys
import traceback
from typing import Any, Dict, Optional

try:
    from z3 import (
        Context, Solver, sat, unsat, unknown,
        Bool, Int, Real, BitVec,
        And, Or, Not, Implies, Xor, If,
        ForAll, Exists,
        IntSort, BoolSort, RealSort, BitVecSort,
        is_true, is_false, is_int_value, is_rational_value,
        simplify
    )
    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False


class Z3Driver:
    """Driver class managing Z3 contexts and solvers."""

    def __init__(self):
        self.contexts: Dict[int, Context] = {}
        self.solvers: Dict[int, Solver] = {}
        self.variables: Dict[int, Dict[str, Any]] = {}  # context_id -> name -> var
        self.next_context_id = 1
        self.next_solver_id = 1

    def handle_command(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Handle a single command and return the response."""
        cmd = request.get("cmd")
        request_id = request.get("id", 0)

        try:
            if cmd == "ping":
                return {"id": request_id, "ok": True, "pong": True}

            elif cmd == "new_context":
                return self._new_context(request_id, request)

            elif cmd == "del_context":
                return self._del_context(request_id, request)

            elif cmd == "new_solver":
                return self._new_solver(request_id, request)

            elif cmd == "del_solver":
                return self._del_solver(request_id, request)

            elif cmd == "assert":
                return self._assert(request_id, request)

            elif cmd == "check":
                return self._check(request_id, request)

            elif cmd == "get_model":
                return self._get_model(request_id, request)

            elif cmd == "push":
                return self._push(request_id, request)

            elif cmd == "pop":
                return self._pop(request_id, request)

            elif cmd == "reset":
                return self._reset(request_id, request)

            else:
                return {"id": request_id, "error": f"Unknown command: {cmd}"}

        except Exception as e:
            return {
                "id": request_id,
                "error": f"{type(e).__name__}: {str(e)}",
                "traceback": traceback.format_exc()
            }

    def _new_context(self, request_id: int, request: Dict) -> Dict:
        """Create a new Z3 context."""
        ctx_id = self.next_context_id
        self.next_context_id += 1

        ctx = Context()
        self.contexts[ctx_id] = ctx
        self.variables[ctx_id] = {}

        return {"id": request_id, "ok": True, "context_id": ctx_id}

    def _del_context(self, request_id: int, request: Dict) -> Dict:
        """Delete a context."""
        ctx_id = request.get("context_id")
        if ctx_id in self.contexts:
            del self.contexts[ctx_id]
            if ctx_id in self.variables:
                del self.variables[ctx_id]
        return {"id": request_id, "ok": True}

    def _new_solver(self, request_id: int, request: Dict) -> Dict:
        """Create a new solver for a context."""
        ctx_id = request.get("context_id")
        if ctx_id not in self.contexts:
            return {"id": request_id, "error": f"Context {ctx_id} not found"}

        solver_id = self.next_solver_id
        self.next_solver_id += 1

        ctx = self.contexts[ctx_id]
        solver = Solver(ctx=ctx)
        self.solvers[solver_id] = solver

        return {
            "id": request_id,
            "ok": True,
            "solver_id": solver_id,
            "context_id": ctx_id
        }

    def _del_solver(self, request_id: int, request: Dict) -> Dict:
        """Delete a solver."""
        solver_id = request.get("solver_id")
        if solver_id in self.solvers:
            del self.solvers[solver_id]
        return {"id": request_id, "ok": True}

    def _assert(self, request_id: int, request: Dict) -> Dict:
        """Add an assertion to a solver."""
        solver_id = request.get("solver_id")
        ctx_id = request.get("context_id")
        expr_json = request.get("expr")

        if solver_id not in self.solvers:
            return {"id": request_id, "error": f"Solver {solver_id} not found"}
        if ctx_id not in self.contexts:
            return {"id": request_id, "error": f"Context {ctx_id} not found"}

        solver = self.solvers[solver_id]
        ctx = self.contexts[ctx_id]
        variables = self.variables[ctx_id]

        expr = self._parse_expr(expr_json, ctx, variables)
        solver.add(expr)

        return {"id": request_id, "ok": True}

    def _check(self, request_id: int, request: Dict) -> Dict:
        """Check satisfiability."""
        solver_id = request.get("solver_id")

        if solver_id not in self.solvers:
            return {"id": request_id, "error": f"Solver {solver_id} not found"}

        solver = self.solvers[solver_id]
        result = solver.check()

        if result == sat:
            return {"id": request_id, "ok": True, "result": "sat"}
        elif result == unsat:
            return {"id": request_id, "ok": True, "result": "unsat"}
        else:
            return {
                "id": request_id,
                "ok": True,
                "result": "unknown",
                "reason": str(solver.reason_unknown())
            }

    def _get_model(self, request_id: int, request: Dict) -> Dict:
        """Get the model after a sat result."""
        solver_id = request.get("solver_id")

        if solver_id not in self.solvers:
            return {"id": request_id, "error": f"Solver {solver_id} not found"}

        solver = self.solvers[solver_id]
        model = solver.model()

        # Convert model to JSON
        model_dict = {}
        for decl in model.decls():
            name = decl.name()
            value = model[decl]
            model_dict[name] = self._value_to_json(value)

        return {"id": request_id, "ok": True, "model": model_dict}

    def _push(self, request_id: int, request: Dict) -> Dict:
        """Push a scope."""
        solver_id = request.get("solver_id")

        if solver_id not in self.solvers:
            return {"id": request_id, "error": f"Solver {solver_id} not found"}

        self.solvers[solver_id].push()
        return {"id": request_id, "ok": True}

    def _pop(self, request_id: int, request: Dict) -> Dict:
        """Pop n scopes."""
        solver_id = request.get("solver_id")
        n = request.get("n", 1)

        if solver_id not in self.solvers:
            return {"id": request_id, "error": f"Solver {solver_id} not found"}

        self.solvers[solver_id].pop(n)
        return {"id": request_id, "ok": True}

    def _reset(self, request_id: int, request: Dict) -> Dict:
        """Reset a solver."""
        solver_id = request.get("solver_id")

        if solver_id not in self.solvers:
            return {"id": request_id, "error": f"Solver {solver_id} not found"}

        self.solvers[solver_id].reset()
        return {"id": request_id, "ok": True}

    def _parse_expr(self, expr_json: Dict, ctx: Context, variables: Dict) -> Any:
        """Parse a JSON expression into a Z3 expression."""
        expr_type = expr_json.get("type")

        # Literals
        if expr_type == "bool_lit":
            return expr_json["value"]

        if expr_type == "int_lit":
            return expr_json["value"]

        if expr_type == "real_lit":
            num = expr_json["numerator"]
            den = expr_json["denominator"]
            return num / den if den != 1 else float(num)

        # Variables/Constants
        if expr_type == "const":
            name = expr_json["name"]
            sort = expr_json["sort"]

            if name in variables:
                return variables[name]

            # Create new variable
            if sort == "bool":
                var = Bool(name, ctx=ctx)
            elif sort == "int":
                var = Int(name, ctx=ctx)
            elif sort == "real":
                var = Real(name, ctx=ctx)
            elif isinstance(sort, dict) and sort.get("type") == "bitvec":
                var = BitVec(name, sort["bits"], ctx=ctx)
            else:
                raise ValueError(f"Unknown sort: {sort}")

            variables[name] = var
            return var

        # Boolean operations
        if expr_type == "and":
            args = [self._parse_expr(a, ctx, variables) for a in expr_json["exprs"]]
            return And(*args) if args else True

        if expr_type == "or":
            args = [self._parse_expr(a, ctx, variables) for a in expr_json["exprs"]]
            return Or(*args) if args else False

        if expr_type == "not":
            return Not(self._parse_expr(expr_json["expr"], ctx, variables))

        if expr_type == "implies":
            a = self._parse_expr(expr_json["antecedent"], ctx, variables)
            b = self._parse_expr(expr_json["consequent"], ctx, variables)
            return Implies(a, b)

        if expr_type == "iff":
            a = self._parse_expr(expr_json["left"], ctx, variables)
            b = self._parse_expr(expr_json["right"], ctx, variables)
            return a == b

        # Arithmetic
        if expr_type == "add":
            args = [self._parse_expr(a, ctx, variables) for a in expr_json["exprs"]]
            result = args[0]
            for arg in args[1:]:
                result = result + arg
            return result

        if expr_type == "sub":
            a = self._parse_expr(expr_json["left"], ctx, variables)
            b = self._parse_expr(expr_json["right"], ctx, variables)
            return a - b

        if expr_type == "mul":
            args = [self._parse_expr(a, ctx, variables) for a in expr_json["exprs"]]
            result = args[0]
            for arg in args[1:]:
                result = result * arg
            return result

        if expr_type == "div":
            a = self._parse_expr(expr_json["numerator"], ctx, variables)
            b = self._parse_expr(expr_json["denominator"], ctx, variables)
            return a / b

        if expr_type == "neg":
            return -self._parse_expr(expr_json["expr"], ctx, variables)

        # Comparisons
        if expr_type == "eq":
            a = self._parse_expr(expr_json["left"], ctx, variables)
            b = self._parse_expr(expr_json["right"], ctx, variables)
            return a == b

        if expr_type == "lt":
            a = self._parse_expr(expr_json["left"], ctx, variables)
            b = self._parse_expr(expr_json["right"], ctx, variables)
            return a < b

        if expr_type == "le":
            a = self._parse_expr(expr_json["left"], ctx, variables)
            b = self._parse_expr(expr_json["right"], ctx, variables)
            return a <= b

        if expr_type == "gt":
            a = self._parse_expr(expr_json["left"], ctx, variables)
            b = self._parse_expr(expr_json["right"], ctx, variables)
            return a > b

        if expr_type == "ge":
            a = self._parse_expr(expr_json["left"], ctx, variables)
            b = self._parse_expr(expr_json["right"], ctx, variables)
            return a >= b

        # Quantifiers
        if expr_type == "forall":
            bound_vars = []
            for var_name, var_sort in expr_json["vars"]:
                if var_sort == "int":
                    v = Int(var_name, ctx=ctx)
                elif var_sort == "bool":
                    v = Bool(var_name, ctx=ctx)
                elif var_sort == "real":
                    v = Real(var_name, ctx=ctx)
                else:
                    raise ValueError(f"Unknown sort in quantifier: {var_sort}")
                bound_vars.append(v)
                variables[var_name] = v

            body = self._parse_expr(expr_json["body"], ctx, variables)
            return ForAll(bound_vars, body)

        if expr_type == "exists":
            bound_vars = []
            for var_name, var_sort in expr_json["vars"]:
                if var_sort == "int":
                    v = Int(var_name, ctx=ctx)
                elif var_sort == "bool":
                    v = Bool(var_name, ctx=ctx)
                elif var_sort == "real":
                    v = Real(var_name, ctx=ctx)
                else:
                    raise ValueError(f"Unknown sort in quantifier: {var_sort}")
                bound_vars.append(v)
                variables[var_name] = v

            body = self._parse_expr(expr_json["body"], ctx, variables)
            return Exists(bound_vars, body)

        # Conditional (if-then-else)
        if expr_type == "ite":
            cond = self._parse_expr(expr_json["condition"], ctx, variables)
            then_branch = self._parse_expr(expr_json["then"], ctx, variables)
            else_branch = self._parse_expr(expr_json["else"], ctx, variables)
            return If(cond, then_branch, else_branch)

        raise ValueError(f"Unknown expression type: {expr_type}")

    def _value_to_json(self, value: Any) -> Dict:
        """Convert a Z3 value to JSON."""
        if is_true(value):
            return {"type": "bool", "value": True}
        if is_false(value):
            return {"type": "bool", "value": False}
        if is_int_value(value):
            return {"type": "int", "value": value.as_long()}
        if is_rational_value(value):
            return {
                "type": "real",
                "numerator": value.numerator_as_long(),
                "denominator": value.denominator_as_long()
            }
        # Fallback: string representation
        return {"type": "unknown", "repr": str(value)}


def main():
    """Main entry point."""
    if not Z3_AVAILABLE:
        # Send error and exit
        error_response = {
            "id": 0,
            "error": "Z3 Python bindings not available. Install with: pip install z3-solver"
        }
        print(json.dumps(error_response), flush=True)
        sys.exit(1)

    driver = Z3Driver()

    # Send ready signal
    print(json.dumps({"ready": True, "version": "0.1.0"}), flush=True)

    # Main loop: read JSON commands, write JSON responses
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        try:
            request = json.loads(line)
        except json.JSONDecodeError as e:
            response = {"id": 0, "error": f"Invalid JSON: {str(e)}"}
            print(json.dumps(response), flush=True)
            continue

        response = driver.handle_command(request)
        print(json.dumps(response), flush=True)


if __name__ == "__main__":
    main()
