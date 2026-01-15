# Hex Publication Strategy

## Overview

This document outlines the strategy for publishing Foil packages to Hex.pm (the package manager for BEAM languages). Based on ecosystem research, all three packages fill significant gaps in the Gleam ecosystem.

## Ecosystem Analysis (January 2026)

### Research Findings

**Anthropic/Claude Clients**:
- ❌ **No Gleam packages** found on Hex
- ✅ Elixir package exists: [anthropix](https://hex.pm/packages/anthropix)
- ✅ Official SDKs: Python, TypeScript, Java, C#, PHP
- **Gap**: Gleam developers have no native Claude API client

**Z3 SMT Solver Bindings**:
- ❌ **No Gleam packages** found on Hex
- ✅ Python bindings: [z3-solver](https://pypi.org/project/z3-solver/)
- ✅ Official bindings: C, C++, Python, .NET, Java, OCaml
- **Gap**: Gleam developers cannot use Z3 for verification

**Modal Logic Libraries**:
- ❌ **No specialized modal logic packages** for Gleam
- ✅ General logic libraries exist in Gleam ecosystem
- ✅ Academic conference: [AiML 2026](http://www.mail-archive.com/math.logic@mailman.rrz.uni-hamburg.de/msg01143.html) (Advances in Modal Logic)
- **Gap**: No modal logic tools for Gleam developers

### Conclusion

**All three packages are unique contributions to the Gleam ecosystem:**
1. `anthropic_gleam` - First Gleam client for Claude API
2. `z3_gleam` - First Gleam bindings for Z3
3. `modal_logic` - First modal logic library for Gleam

## Publication Strategy

### Package 1: anthropic_gleam

**Value Proposition**: First and only Gleam client for Anthropic's Claude API

**Target Audience**:
- Gleam developers building AI-powered applications
- BEAM developers preferring Gleam over Elixir
- Developers wanting type-safe LLM integration

**Unique Features**:
- Full Messages API support with typed interfaces
- Streaming responses with real-time events
- Tool use capabilities
- Automatic retry logic with exponential backoff
- Type-safe error handling

**Dependencies**: gleam_stdlib, gleam_json, gleam_http, gleam_httpc, gleam_erlang

**Publication Priority**: HIGH (fills major ecosystem gap)

---

### Package 2: z3_gleam

**Value Proposition**: First and only Gleam bindings for Z3 theorem prover

**Target Audience**:
- Gleam developers doing formal verification
- Researchers using SMT solvers
- Developers building constraint solving applications

**Unique Features**:
- Type-safe SMT expression construction
- Modal logic support with Kripke semantics
- Countermodel extraction
- Performance benchmarking utilities

**Dependencies**: gleam_stdlib, gleam_json, gleam_erlang

**Publication Priority**: HIGH (unique offering)

---

### Package 3: modal_logic

**Value Proposition**: First comprehensive modal logic library for Gleam

**Target Audience**:
- Philosophy researchers
- Logic students and educators
- Developers building reasoning systems

**Unique Features**:
- 7 modal systems (K, T, K4, S4, S5, KD, KD45)
- LLM-powered natural language translation
- Z3-backed formal verification
- Pattern library and complexity analysis

**Dependencies**: gleam_stdlib, gleam_json

**Publication Priority**: MEDIUM (depends on z3_gleam and anthropic_gleam)

---

## Pre-Publication Checklist

### For Each Package

#### 1. Version & Metadata ✅
- [x] Version set to 0.1.0 (initial release)
- [x] License: MIT
- [x] Description: Clear, concise
- [x] Repository: Correct GitHub URL
- [ ] Links: Add website, documentation, sponsor links
- [ ] Keywords/topics for discoverability

#### 2. Documentation
- [ ] Complete README with examples
- [ ] API documentation generated (`gleam docs`)
- [ ] Usage examples
- [ ] Installation instructions
- [ ] License file
- [ ] Changelog

#### 3. Code Quality
- [ ] All tests passing
- [ ] Code formatted (`gleam format`)
- [ ] No warnings (or documented)
- [ ] Type-safe public API
- [ ] Backward compatibility plan

#### 4. Dependencies
- [ ] All dependencies published to Hex
- [ ] Version constraints appropriate
- [ ] No path dependencies (for published packages)

#### 5. Package Structure
- [ ] Standard src/ and test/ layout
- [ ] No unnecessary files in package
- [ ] Build artifacts excluded
- [ ] Appropriate .gitignore

## Repository Separation Plan

### Current Structure (Monorepo)

```
foil/
├── packages/
│   ├── anthropic_gleam/  → Separate repo
│   ├── z3_gleam/         → Separate repo
│   └── modal_logic/      → Keep in foil repo
├── apps/
│   └── analyst/          → Keep in foil repo
└── docs/                 → Keep in foil repo
```

### Proposed Structure (After Separation)

**Repository 1**: `anthropic-gleam` (new standalone repo)
- Package: anthropic_gleam
- Purpose: Claude API client
- Audience: General Gleam developers
- URL: github.com/justin4957/anthropic-gleam

**Repository 2**: `z3-gleam` (new standalone repo)
- Package: z3_gleam
- Purpose: Z3 SMT solver bindings
- Audience: Verification developers
- URL: github.com/justin4957/z3-gleam

**Repository 3**: `foil` (existing repo)
- Package: modal_logic
- Apps: analyst
- Purpose: Modal logic analysis system
- Audience: Philosophy researchers
- Dependencies: anthropic_gleam@0.1.0, z3_gleam@0.1.0 (from Hex)

### Benefits of Separation

1. **Independent Versioning**: Each package can evolve independently
2. **Focused Issues**: Separate issue trackers for each concern
3. **Clearer Purpose**: Each repo has single responsibility
4. **Easier Discovery**: Developers find what they need
5. **Hex Publication**: Packages can be published without monorepo complexity

## Developer Utilities to Add

### 1. Publish Script

```bash
#!/bin/bash
# scripts/publish.sh

set -e

PACKAGE_NAME=$1
VERSION=$2

if [ -z "$PACKAGE_NAME" ] || [ -z "$VERSION" ]; then
  echo "Usage: ./scripts/publish.sh <package_name> <version>"
  echo "Example: ./scripts/publish.sh anthropic_gleam 0.1.0"
  exit 1
fi

cd "packages/$PACKAGE_NAME"

# Pre-publish checks
echo "Running pre-publish checks..."
gleam format --check src test
gleam build
gleam test
gleam docs build

# Update version
echo "Updating version to $VERSION..."
sed -i '' "s/^version = .*/version = \"$VERSION\"/" gleam.toml

# Publish to Hex
echo "Publishing $PACKAGE_NAME@$VERSION to Hex..."
gleam publish

echo "✅ Published $PACKAGE_NAME@$VERSION successfully!"
```

### 2. Pre-Publish Validation Script

```bash
#!/bin/bash
# scripts/validate-package.sh

PACKAGE_DIR=$1

cd "$PACKAGE_DIR"

echo "🔍 Validating package for Hex publication..."

# Check for required files
echo "Checking required files..."
[ -f "gleam.toml" ] || { echo "❌ Missing gleam.toml"; exit 1; }
[ -f "README.md" ] || { echo "❌ Missing README.md"; exit 1; }
[ -f "LICENSE" ] || { echo "❌ Missing LICENSE"; exit 1; }

# Check gleam.toml metadata
echo "Checking gleam.toml metadata..."
grep -q "^description = " gleam.toml || { echo "❌ Missing description"; exit 1; }
grep -q "^licences = " gleam.toml || { echo "❌ Missing licences"; exit 1; }
grep -q "^repository = " gleam.toml || { echo "❌ Missing repository"; exit 1; }

# Run tests
echo "Running tests..."
gleam test || { echo "❌ Tests failed"; exit 1; }

# Check formatting
echo "Checking code formatting..."
gleam format --check src test || { echo "❌ Code not formatted"; exit 1; }

# Build documentation
echo "Building documentation..."
gleam docs build || { echo "❌ Doc build failed"; exit 1; }

echo "✅ Package validation passed!"
```

### 3. Version Bump Utility

```bash
#!/bin/bash
# scripts/bump-version.sh

PACKAGE=$1
TYPE=$2  # major, minor, patch

case $TYPE in
  major|minor|patch)
    cd "packages/$PACKAGE"
    # Extract current version
    CURRENT=$(grep "^version = " gleam.toml | sed 's/version = "\(.*\)"/\1/')
    echo "Current version: $CURRENT"

    # Calculate new version (simplified)
    # Real implementation would parse semver properly
    echo "New version calculation to be implemented"
    ;;
  *)
    echo "Usage: ./scripts/bump-version.sh <package> <major|minor|patch>"
    exit 1
    ;;
esac
```

### 4. Dependency Checker

```bash
#!/bin/bash
# scripts/check-deps.sh

echo "Checking for path dependencies..."

find packages -name "gleam.toml" -exec grep -H "path =" {} \; || echo "✅ No path dependencies found"

echo ""
echo "Checking dependency versions..."

gleam deps list
```

## Publication Workflow

### Step 1: Prepare Package

```bash
# Navigate to package
cd packages/anthropic_gleam

# Run validation
../../scripts/validate-package.sh .

# Update version if needed
../../scripts/bump-version.sh anthropic_gleam patch

# Update CHANGELOG
vi CHANGELOG.md
```

### Step 2: Build and Test

```bash
# Clean build
gleam clean
gleam build

# Run tests
gleam test

# Build docs
gleam docs build

# Preview docs
gleam docs serve
```

### Step 3: Publish

```bash
# Dry run (check what would be published)
gleam publish --dry-run

# Actual publish
gleam publish

# Tag release
git tag -a anthropic_gleam-v0.1.0 -m "anthropic_gleam v0.1.0"
git push origin anthropic_gleam-v0.1.0
```

### Step 4: Post-Publication

```bash
# Verify on Hex
open https://hex.pm/packages/anthropic_gleam

# Update dependents (modal_logic)
cd packages/modal_logic
# Change path dependency to hex dependency
sed -i '' 's/anthropic_gleam = { path = .* }/anthropic_gleam = "~> 0.1"/' gleam.toml

# Test with hex dependency
gleam deps update
gleam test
```

## Package-Specific Preparation

### anthropic_gleam

**Pre-Publication Tasks**:
1. Add comprehensive README with:
   - Quick start example
   - All API features
   - Streaming examples
   - Tool use examples
   - Error handling patterns

2. Add CHANGELOG.md

3. Update gleam.toml:
   ```toml
   links = [
     { title = "Documentation", href = "https://hexdocs.pm/anthropic_gleam" },
     { title = "Sponsor", href = "https://github.com/sponsors/justin4957" }
   ]
   ```

4. Add keywords for discoverability

5. Verify all examples work

---

### z3_gleam

**Pre-Publication Tasks**:
1. Add README with:
   - Z3 installation instructions
   - SMT solving examples
   - Modal logic examples
   - Kripke frame examples

2. Add CHANGELOG.md

3. Document Z3 version compatibility

4. Add troubleshooting section

5. System requirements documentation

---

### modal_logic

**Pre-Publication Tasks**:
1. Update dependencies to use published packages:
   ```toml
   anthropic_gleam = "~> 0.1"
   z3_gleam = "~> 0.1"
   ```

2. Add comprehensive README

3. Add examples directory

4. Document all 9 implemented features (from PRs 108-124)

5. Add quickstart guide

## Publication Order

**Correct Order** (dependency-based):

1. **anthropic_gleam** (no internal dependencies)
2. **z3_gleam** (no internal dependencies)
3. **modal_logic** (depends on both above)

## Hex Package Naming

- ✅ `anthropic_gleam` - Clear, follows Gleam naming conventions
- ✅ `z3_gleam` - Clear, indicates Z3 bindings
- ✅ `modal_logic` - Clear, descriptive

Alternative names considered:
- `gleam_anthropic` ❌ (less clear)
- `gleam_z3` ❌ (less conventional)
- `foil` ❌ (too vague for modal_logic)

## Version Strategy

### Initial Release: 0.1.0

**Reasoning**:
- Development complete
- Comprehensive test coverage
- Production-ready code
- Not yet battle-tested in wild

**Roadmap**:
- 0.1.x: Bug fixes, minor improvements
- 0.2.0: New features, backward compatible
- 1.0.0: Stable API, production-proven

### Semantic Versioning

Following SemVer 2.0:
- **MAJOR**: Incompatible API changes
- **MINOR**: New functionality, backward compatible
- **PATCH**: Bug fixes, backward compatible

## Developer Utilities

### Missing Utilities in Ecosystem

Based on research, these utilities would benefit Gleam developers:

#### 1. Gleam Hex Publisher (gleam_hex_publisher)

**What**: Automated Hex publication workflow
**Why**: No standardized tool exists
**Features**:
- Pre-publication validation
- Automatic version bumping
- Changelog generation
- Git tagging
- Post-publication verification

#### 2. Gleam Package Template Generator

**What**: Scaffold new Gleam packages
**Why**: Reduces setup friction
**Features**:
- Standard project structure
- Pre-configured gleam.toml
- CI/CD templates
- README template
- LICENSE file

#### 3. Gleam Dependency Analyzer

**What**: Analyze and optimize dependencies
**Why**: No visual dependency tree tool
**Features**:
- Dependency tree visualization
- Circular dependency detection
- Update recommendations
- Security audit integration

## Post-Publication Maintenance

### Monitoring

- **Hex Stats**: Track downloads, stars
- **Issues**: Monitor GitHub issues for bugs
- **Discussions**: Engage with community
- **Updates**: Keep dependencies current

### Support Channels

- GitHub Issues: Bug reports, feature requests
- GitHub Discussions: Questions, community
- Discord/Gleam Community: Real-time help

### Deprecation Policy

If packages ever need deprecation:
1. Add deprecation notice in README
2. Publish final version with deprecation warning
3. Recommend alternative package
4. Maintain for 6 months minimum

## Security

### API Keys

**Important**: Never commit API keys to repository!

For anthropic_gleam README:
```markdown
## Security

⚠️ **Never hardcode API keys in your code!**

Use environment variables:
```gleam
import gleam/erlang/os

let api_key = os.get_env("ANTHROPIC_API_KEY")
```
```

### Vulnerability Reporting

Add SECURITY.md:
```markdown
# Security Policy

## Reporting a Vulnerability

Please report security vulnerabilities to:
- Email: security@example.com
- Private GitHub Security Advisory

Do not open public issues for security vulnerabilities.
```

## Sources

Research sources consulted:
- [Gleam Package Index](https://packages.gleam.run/)
- [anthropix (Elixir)](https://hex.pm/packages/anthropix)
- [Z3 Theorem Prover](https://github.com/Z3Prover/z3)
- [AiML 2026 Conference](http://www.mail-archive.com/math.logic@mailman.rrz.uni-hamburg.de/msg01143.html)
- [Anthropic Client SDKs](https://docs.anthropic.com/claude/reference/client-sdks)

## Next Steps

1. **Create separate repositories** for anthropic_gleam and z3_gleam
2. **Run validation scripts** on all packages
3. **Update READMEs** with comprehensive documentation
4. **Add CHANGELOGs**
5. **Publish to Hex** in correct order
6. **Update modal_logic** to use Hex dependencies
7. **Announce** on Gleam community channels
