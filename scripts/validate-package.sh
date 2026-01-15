#!/bin/bash
# Validate package for Hex publication
# Usage: ./scripts/validate-package.sh packages/anthropic_gleam

set -e

PACKAGE_DIR=$1

if [ -z "$PACKAGE_DIR" ]; then
  echo "Usage: ./scripts/validate-package.sh <package_directory>"
  echo "Example: ./scripts/validate-package.sh packages/anthropic_gleam"
  exit 1
fi

if [ ! -d "$PACKAGE_DIR" ]; then
  echo "❌ Directory not found: $PACKAGE_DIR"
  exit 1
fi

cd "$PACKAGE_DIR"

PACKAGE_NAME=$(basename "$PACKAGE_DIR")

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🔍 Validating $PACKAGE_NAME for Hex publication"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Check for required files
echo "📁 Checking required files..."
[ -f "gleam.toml" ] || { echo "❌ Missing gleam.toml"; exit 1; }
[ -f "README.md" ] || { echo "❌ Missing README.md"; exit 1; }
[ -f "LICENSE" ] || { echo "⚠️  Missing LICENSE (recommended)"; }
echo "✅ Required files present"
echo ""

# Check gleam.toml metadata
echo "📋 Checking gleam.toml metadata..."
grep -q "^name = " gleam.toml || { echo "❌ Missing name"; exit 1; }
grep -q "^version = " gleam.toml || { echo "❌ Missing version"; exit 1; }
grep -q "^description = " gleam.toml || { echo "❌ Missing description"; exit 1; }
grep -q "^licences = " gleam.toml || { echo "❌ Missing licences"; exit 1; }
grep -q "^repository = " gleam.toml || { echo "❌ Missing repository"; exit 1; }

VERSION=$(grep "^version = " gleam.toml | sed 's/version = "\(.*\)"/\1/')
echo "✅ Metadata complete (version: $VERSION)"
echo ""

# Check for path dependencies (should use Hex dependencies)
echo "🔗 Checking dependencies..."
if grep -q "path = " gleam.toml; then
  echo "⚠️  Path dependencies found (should use Hex dependencies for publication):"
  grep "path = " gleam.toml
  echo ""
else
  echo "✅ No path dependencies"
  echo ""
fi

# Run format check
echo "✨ Checking code formatting..."
if gleam format --check src test 2>&1 | grep -q "error:"; then
  echo "❌ Code not formatted. Run: gleam format src test"
  exit 1
else
  echo "✅ Code properly formatted"
  echo ""
fi

# Build package
echo "🔨 Building package..."
if gleam build 2>&1 | grep -q "error:"; then
  echo "❌ Build failed"
  exit 1
else
  echo "✅ Build successful"
  echo ""
fi

# Run tests
echo "🧪 Running tests..."
TEST_OUTPUT=$(gleam test 2>&1)
if echo "$TEST_OUTPUT" | grep -q "error:"; then
  echo "❌ Tests failed"
  echo "$TEST_OUTPUT"
  exit 1
else
  TEST_COUNT=$(echo "$TEST_OUTPUT" | grep -o "[0-9]* passed" | head -1 | grep -o "[0-9]*" || echo "unknown")
  echo "✅ Tests passed ($TEST_COUNT tests)"
  echo ""
fi

# Build documentation
echo "📚 Building documentation..."
if gleam docs build 2>&1 | grep -q "error:"; then
  echo "❌ Documentation build failed"
  exit 1
else
  echo "✅ Documentation built successfully"
  echo ""
fi

# Check README length
README_LINES=$(wc -l < README.md)
if [ "$README_LINES" -lt 20 ]; then
  echo "⚠️  README is short ($README_LINES lines). Consider adding more documentation."
else
  echo "✅ README has good documentation ($README_LINES lines)"
fi
echo ""

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ Package validation passed!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Ready to publish $PACKAGE_NAME@$VERSION to Hex!"
echo ""
echo "Next steps:"
echo "  1. Review package contents: gleam publish --dry-run"
echo "  2. Publish to Hex: gleam publish"
echo "  3. Tag release: git tag -a ${PACKAGE_NAME}-v${VERSION}"
echo ""
