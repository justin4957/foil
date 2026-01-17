#!/bin/bash
# Epic #144 Full Validation Script
# Runs comprehensive validation of all phases and generates a report

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODAL_LOGIC_DIR="$(dirname "$SCRIPT_DIR")"

cd "$MODAL_LOGIC_DIR"

echo "=========================================="
echo "Epic #144 Validation Suite"
echo "=========================================="
echo ""
echo "Building modal_logic package..."
gleam build

echo ""
echo "Running full Epic #144 validation..."
echo ""

# Run the validation test and capture output
gleam test --target erlang 2>&1 | grep -A 1000 "EPIC #144 FULL VALIDATION REPORT" | head -200

echo ""
echo "=========================================="
echo "Validation Complete"
echo "=========================================="
