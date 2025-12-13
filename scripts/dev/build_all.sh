#!/bin/bash
# Build all packages in the monorepo

set -e

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

echo "Building Modal Logic Engine packages..."
echo ""

# Build packages
for pkg in anthropic_gleam z3_gleam modal_logic; do
    echo "Building $pkg..."
    cd "$ROOT_DIR/packages/$pkg"
    gleam build
    echo "✓ $pkg built successfully"
    echo ""
done

# Build apps
for app in analyst; do
    echo "Building $app..."
    cd "$ROOT_DIR/apps/$app"
    gleam build
    echo "✓ $app built successfully"
    echo ""
done

echo "All packages built successfully!"
