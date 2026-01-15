#!/bin/bash
# Publish package to Hex
# Usage: ./scripts/publish-package.sh packages/anthropic_gleam 0.1.0

set -e

PACKAGE_DIR=$1
VERSION=$2

if [ -z "$PACKAGE_DIR" ] || [ -z "$VERSION" ]; then
  echo "Usage: ./scripts/publish-package.sh <package_directory> <version>"
  echo "Example: ./scripts/publish-package.sh packages/anthropic_gleam 0.1.0"
  exit 1
fi

if [ ! -d "$PACKAGE_DIR" ]; then
  echo "❌ Directory not found: $PACKAGE_DIR"
  exit 1
fi

PACKAGE_NAME=$(basename "$PACKAGE_DIR")

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📦 Publishing $PACKAGE_NAME@$VERSION to Hex"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Step 1: Validate package
echo "Step 1: Validating package..."
./scripts/validate-package.sh "$PACKAGE_DIR"
echo ""

# Step 2: Update version in gleam.toml
echo "Step 2: Updating version to $VERSION..."
cd "$PACKAGE_DIR"
sed -i '' "s/^version = .*/version = \"$VERSION\"/" gleam.toml
echo "✅ Version updated"
echo ""

# Step 3: Dry run
echo "Step 3: Running dry-run..."
echo "Package contents:"
gleam publish --dry-run || { echo "❌ Dry-run failed"; exit 1; }
echo ""

# Step 4: Confirm publication
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Ready to publish $PACKAGE_NAME@$VERSION to Hex"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
read -p "Continue with publication? (y/N) " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "❌ Publication cancelled"
  exit 1
fi

# Step 5: Publish
echo ""
echo "Step 4: Publishing to Hex..."
gleam publish || { echo "❌ Publication failed"; exit 1; }
echo ""

# Step 6: Git tag
echo "Step 5: Creating git tag..."
cd ../..
TAG_NAME="${PACKAGE_NAME}-v${VERSION}"
git tag -a "$TAG_NAME" -m "Release $PACKAGE_NAME v$VERSION"
echo "✅ Created tag: $TAG_NAME"
echo ""

# Step 7: Success
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🎉 Successfully published $PACKAGE_NAME@$VERSION!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Package URL: https://hex.pm/packages/$PACKAGE_NAME"
echo "Docs URL: https://hexdocs.pm/$PACKAGE_NAME"
echo ""
echo "Next steps:"
echo "  1. Push git tag: git push origin $TAG_NAME"
echo "  2. Create GitHub release"
echo "  3. Announce in Gleam community"
echo "  4. Update dependents if needed"
echo ""
