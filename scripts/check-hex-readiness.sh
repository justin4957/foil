#!/bin/bash
# Check all packages for Hex publication readiness
# Usage: ./scripts/check-hex-readiness.sh

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🔍 Checking Hex Publication Readiness"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

PACKAGES=(
  "packages/anthropic_gleam"
  "packages/z3_gleam"
  "packages/modal_logic"
)

RESULTS=()

for PKG in "${PACKAGES[@]}"; do
  if [ ! -d "$PKG" ]; then
    echo "⚠️  Skipping $PKG (not found)"
    continue
  fi

  PKG_NAME=$(basename "$PKG")
  echo "Checking $PKG_NAME..."

  cd "$PKG"

  # Run quick validation
  ERRORS=0

  # Required files
  [ ! -f "gleam.toml" ] && { echo "  ❌ Missing gleam.toml"; ERRORS=$((ERRORS+1)); }
  [ ! -f "README.md" ] && { echo "  ❌ Missing README.md"; ERRORS=$((ERRORS+1)); }

  # Metadata
  grep -q "^description = " gleam.toml 2>/dev/null || { echo "  ❌ Missing description"; ERRORS=$((ERRORS+1)); }
  grep -q "^licences = " gleam.toml 2>/dev/null || { echo "  ❌ Missing licences"; ERRORS=$((ERRORS+1)); }

  # Path dependencies
  if grep -q "path = " gleam.toml 2>/dev/null; then
    echo "  ⚠️  Has path dependencies (update for publication)"
    ERRORS=$((ERRORS+1))
  fi

  # Tests
  if ! gleam test >/dev/null 2>&1; then
    echo "  ❌ Tests failing"
    ERRORS=$((ERRORS+1))
  fi

  cd ../..

  if [ $ERRORS -eq 0 ]; then
    echo "  ✅ Ready for publication"
    RESULTS+=("✅ $PKG_NAME")
  else
    echo "  ❌ Not ready ($ERRORS issues)"
    RESULTS+=("❌ $PKG_NAME ($ERRORS issues)")
  fi

  echo ""
done

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

for RESULT in "${RESULTS[@]}"; do
  echo "$RESULT"
done

echo ""

# Count ready packages
READY_COUNT=$(echo "${RESULTS[@]}" | grep -o "✅" | wc -l | xargs)
TOTAL_COUNT=${#RESULTS[@]}

echo "Ready for publication: $READY_COUNT/$TOTAL_COUNT"
echo ""

if [ "$READY_COUNT" -eq "$TOTAL_COUNT" ]; then
  echo "🎉 All packages ready for Hex publication!"
  echo ""
  echo "Recommended publication order:"
  echo "  1. anthropic_gleam (no internal deps)"
  echo "  2. z3_gleam (no internal deps)"
  echo "  3. modal_logic (depends on above)"
else
  echo "Fix issues before publishing."
fi
