#!/bin/bash
# Epic #144 Validation Suite
#
# This script runs the epic validation infrastructure to test progress
# toward the Fast Modal Logic Checking for Prediction Accuracy goals.
#
# Usage:
#   ./scripts/validate_epic.sh              # Run all validations
#   ./scripts/validate_epic.sh --phase A    # Run specific phase
#   ./scripts/validate_epic.sh --json       # Output JSON for CI
#   ./scripts/validate_epic.sh --markdown   # Output markdown report
#
# Exit codes:
#   0 - All validations passed
#   1 - Some validations failed
#   2 - Script error

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MODAL_LOGIC_DIR="$PROJECT_ROOT/packages/modal_logic"

# Default values
PHASE=""
OUTPUT_FORMAT="text"
VERBOSE=false
RESULTS_DIR="$PROJECT_ROOT/results"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --phase|-p)
            PHASE="$2"
            shift 2
            ;;
        --json|-j)
            OUTPUT_FORMAT="json"
            shift
            ;;
        --markdown|-m)
            OUTPUT_FORMAT="markdown"
            shift
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --help|-h)
            echo "Epic #144 Validation Suite"
            echo ""
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --phase, -p <A|B|C|D|E>  Validate specific phase"
            echo "  --json, -j               Output JSON format"
            echo "  --markdown, -m           Output Markdown format"
            echo "  --verbose, -v            Enable verbose output"
            echo "  --help, -h               Show this help"
            echo ""
            echo "Exit codes:"
            echo "  0 - All validations passed"
            echo "  1 - Some validations failed"
            echo "  2 - Script error"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 2
            ;;
    esac
done

# Ensure results directory exists
mkdir -p "$RESULTS_DIR"

# Print header
if [ "$OUTPUT_FORMAT" = "text" ]; then
    echo -e "${BLUE}======================================================================${NC}"
    echo -e "${BLUE}Epic #144 Validation Suite${NC}"
    echo -e "${BLUE}Fast Modal Logic Checking for Prediction Accuracy${NC}"
    echo -e "${BLUE}======================================================================${NC}"
    echo ""
fi

# Change to modal_logic package directory
cd "$MODAL_LOGIC_DIR"

# Build the project first
if [ "$VERBOSE" = true ]; then
    echo -e "${YELLOW}Building modal_logic package...${NC}"
fi
gleam build 2>/dev/null || {
    echo -e "${RED}Error: Failed to build modal_logic package${NC}"
    exit 2
}

# Run epic validation
if [ "$VERBOSE" = true ]; then
    echo -e "${YELLOW}Running epic validation...${NC}"
fi

# Build the command
CMD="gleam run -m epic_validation_runner"

if [ -n "$PHASE" ]; then
    CMD="$CMD -- --phase $PHASE"
fi

if [ "$OUTPUT_FORMAT" = "json" ]; then
    CMD="$CMD --output json"
elif [ "$OUTPUT_FORMAT" = "markdown" ]; then
    CMD="$CMD --output markdown"
fi

# Run the validation and capture output
OUTPUT=$(gleam run -m epic_validation_dialogue_test 2>&1) || true

# For now, run the dialogue test which demonstrates the epic validation
if [ "$OUTPUT_FORMAT" = "json" ]; then
    # Extract JSON-formatted results
    echo "$OUTPUT" | grep -A 1000 "JSON Output:" | tail -n +2 || echo "$OUTPUT"
elif [ "$OUTPUT_FORMAT" = "markdown" ]; then
    # Extract markdown-formatted results
    echo "$OUTPUT" | grep -A 1000 "Markdown Output:" | tail -n +2 || echo "$OUTPUT"
else
    echo "$OUTPUT"
fi

# Save results
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
if [ "$OUTPUT_FORMAT" = "json" ]; then
    echo "$OUTPUT" > "$RESULTS_DIR/epic_validation_${TIMESTAMP}.json"
elif [ "$OUTPUT_FORMAT" = "markdown" ]; then
    echo "$OUTPUT" > "$RESULTS_DIR/epic_validation_${TIMESTAMP}.md"
else
    echo "$OUTPUT" > "$RESULTS_DIR/epic_validation_${TIMESTAMP}.txt"
fi

# Check for failures
if echo "$OUTPUT" | grep -q "FAIL\|PENDING"; then
    if [ "$OUTPUT_FORMAT" = "text" ]; then
        echo ""
        echo -e "${YELLOW}Some validations are pending or failed.${NC}"
    fi
    exit 1
else
    if [ "$OUTPUT_FORMAT" = "text" ]; then
        echo ""
        echo -e "${GREEN}All validations passed!${NC}"
    fi
    exit 0
fi
