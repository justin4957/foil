# Package Publication Guide

## Quick Reference

### Validation

```bash
# Check all packages
./scripts/check-hex-readiness.sh

# Validate specific package
./scripts/validate-package.sh packages/anthropic_gleam
```

### Publication

```bash
# Publish with automation
./scripts/publish-package.sh packages/anthropic_gleam 0.1.0

# Manual publication
cd packages/anthropic_gleam
gleam publish --dry-run  # Preview
gleam publish            # Actual publish
```

## Pre-Publication Checklist

### ✅ anthropic_gleam

- [x] Version: 0.1.0
- [x] Description: Clear and concise
- [x] License: MIT
- [x] Repository: Correct GitHub URL
- [x] README.md: Comprehensive (301 lines)
- [x] CHANGELOG.md: Created
- [x] Tests: All passing
- [x] Documentation: Complete
- [x] No path dependencies
- [x] Code formatted

**Status**: ✅ Ready for Hex publication

**Unique Value**: First Gleam client for Claude API

---

### ✅ z3_gleam

- [x] Version: 0.1.0
- [x] Description: Clear and concise
- [x] License: MIT
- [x] Repository: Correct GitHub URL
- [x] README.md: Exists
- [x] CHANGELOG.md: Created
- [x] Tests: All passing
- [x] Documentation: Complete
- [x] No path dependencies
- [x] Code formatted

**Status**: ✅ Ready for Hex publication

**Unique Value**: First Gleam bindings for Z3

---

### ✅ modal_logic

- [x] Version: 0.1.0
- [x] Description: Clear and concise
- [x] License: MIT
- [x] Repository: Correct GitHub URL
- [x] README.md: Comprehensive
- [x] CHANGELOG.md: Created
- [x] Tests: All passing (555 tests!)
- [x] Documentation: Extensive
- [x] Code formatted

**Status**: ✅ Ready for Hex publication (after anthropic_gleam and z3_gleam)

**Note**: Currently uses path dependencies. Update to Hex dependencies after publishing other packages.

**Unique Value**: First comprehensive modal logic library for Gleam

## Ecosystem Gaps Filled

### Research Summary (January 2026)

**Anthropic/Claude Clients**:
- Existing: [anthropix](https://hex.pm/packages/anthropix) (Elixir only)
- **Gap**: No Gleam package
- **Our Solution**: `anthropic_gleam` - First Gleam client

**Z3 SMT Solver Bindings**:
- Existing: Python, .NET, Java, OCaml (official)
- **Gap**: No Gleam bindings
- **Our Solution**: `z3_gleam` - First Gleam bindings

**Modal Logic Libraries**:
- Existing: General logic libraries in Gleam
- **Gap**: No specialized modal logic package
- **Our Solution**: `modal_logic` - First modal logic library

### Sources

Research conducted using:
- [Gleam Package Index](https://packages.gleam.run/)
- [Hex.pm](https://hex.pm/) search
- [Anthropic Client SDKs](https://docs.anthropic.com/claude/reference/client-sdks) (official docs)
- [Z3 GitHub](https://github.com/Z3Prover/z3)
- Web search for "Gleam Hex packages" + technology names

**Conclusion**: All three packages fill real gaps in the Gleam ecosystem and provide value to the broader community, not just foil users.

## Publication Commands

### Dry Run (Preview)

```bash
cd packages/anthropic_gleam
gleam publish --dry-run
```

**Output shows**:
- Files that will be included
- Package size
- Dependencies
- Validation results

### Actual Publication

```bash
gleam publish
```

**Interactive prompts**:
1. Confirm package name
2. Confirm version
3. Review licenses
4. Confirm publication

**After success**:
- Package available at `https://hex.pm/packages/<name>`
- Documentation at `https://hexdocs.pm/<name>`
- Can be installed: `gleam add <name>`

## Post-Publication Steps

### 1. Verify on Hex

```bash
# Check package page
open https://hex.pm/packages/anthropic_gleam

# Check documentation
open https://hexdocs.pm/anthropic_gleam

# Test installation in new project
mkdir test-install && cd test-install
gleam new test_project
cd test_project
gleam add anthropic_gleam
gleam build
```

### 2. Create GitHub Release

```bash
git tag -a anthropic_gleam-v0.1.0 -m "anthropic_gleam v0.1.0"
git push origin anthropic_gleam-v0.1.0

gh release create anthropic_gleam-v0.1.0 \
  --title "anthropic_gleam v0.1.0" \
  --notes-file packages/anthropic_gleam/CHANGELOG.md
```

### 3. Update Dependents

If modal_logic depends on anthropic_gleam:

```bash
cd packages/modal_logic

# Update gleam.toml
# Change: anthropic_gleam = { path = "../anthropic_gleam" }
# To: anthropic_gleam = "~> 0.1"

gleam deps update
gleam test  # Verify still works
```

### 4. Announce

**Gleam Discord**:
```
🎉 New package on Hex: anthropic_gleam!

First Gleam client for Anthropic's Claude API.

Features:
- Streaming support
- Tool use
- Automatic retries
- Type-safe error handling

https://hex.pm/packages/anthropic_gleam

Feedback welcome!
```

**Twitter/Social**:
```
Excited to announce anthropic_gleam - the first @gleamlang client for @AnthropicAI's Claude API!

✨ Streaming support
🔧 Tool use
🔄 Auto-retry
🎯 Type-safe

https://hex.pm/packages/anthropic_gleam

#gleam #claude #ai
```

## Developer Utilities Added

### 1. validate-package.sh

**Purpose**: Validate package before publication

**Features**:
- Check required files (gleam.toml, README, LICENSE)
- Validate metadata completeness
- Detect path dependencies
- Run format check
- Run build
- Run tests
- Build documentation
- Check README length

**Usage**:
```bash
./scripts/validate-package.sh packages/anthropic_gleam
```

### 2. publish-package.sh

**Purpose**: Automate publication workflow

**Features**:
- Run validation
- Update version in gleam.toml
- Dry-run publication
- Interactive confirmation
- Actual publication
- Git tagging
- Success instructions

**Usage**:
```bash
./scripts/publish-package.sh packages/anthropic_gleam 0.1.0
```

### 3. check-hex-readiness.sh

**Purpose**: Check all packages at once

**Features**:
- Validate all packages in monorepo
- Summary report
- Readiness count
- Publication order recommendation

**Usage**:
```bash
./scripts/check-hex-readiness.sh
```

## Recommended Publication Order

**Order**: Based on dependencies

1. **anthropic_gleam** (no internal dependencies)
   - Can be published immediately
   - Used by modal_logic

2. **z3_gleam** (no internal dependencies)
   - Can be published immediately
   - Used by modal_logic

3. **modal_logic** (depends on both above)
   - Publish after anthropic_gleam and z3_gleam are on Hex
   - Update gleam.toml to use Hex dependencies first

## Version Strategy

### Initial: 0.1.0

**Why 0.1.0?**
- Signals initial release
- Not yet battle-tested in production
- API may evolve based on feedback
- Follows semantic versioning conventions

### Roadmap

- **0.1.x**: Bug fixes, patches
- **0.2.0**: New features, backward compatible
- **1.0.0**: Stable API, production-proven

## Package Differentiation

### anthropic_gleam

**Category**: API Client
**Audience**: Any Gleam developer needing LLM integration
**Competition**: None in Gleam (anthropix in Elixir)
**Positioning**: "The Gleam client for Claude"

### z3_gleam

**Category**: Formal Verification
**Audience**: Verification engineers, researchers
**Competition**: None in Gleam
**Positioning**: "Bring Z3's power to Gleam"

### modal_logic

**Category**: Domain-Specific Library
**Audience**: Philosophy researchers, logic educators
**Competition**: None in Gleam
**Positioning**: "Modal logic analysis for Gleam"

## Troubleshooting

### "Package name already taken"

- Choose alternative name
- Contact Hex support if squatted
- Add suffix: anthropic_gleam_client

### "Version already published"

- Cannot republish same version
- Bump to next patch version
- Yank bad version if necessary: `gleam publish --revert 0.1.0`

### "Tests failing in CI but pass locally"

- Check Gleam/OTP versions match
- Verify all dependencies available
- Check for environment-specific code

### "Documentation build fails"

- Ensure all public functions documented
- Fix any broken doc links
- Validate code examples compile

## Maintenance Plan

### Regular Updates

- Monitor for Gleam version updates
- Keep dependencies current
- Respond to community issues
- Release patches for bugs

### Breaking Changes

If API must change:
1. Deprecate old API
2. Add deprecation warnings
3. Maintain for 2 minor versions
4. Document migration path
5. Bump major version

## Success Metrics

### Week 1
- ✅ Published to Hex
- ✅ Documentation live on hexdocs.pm
- ✅ Announced in community

### Month 1
- 📊 50+ downloads per package
- 🐛 No critical bugs
- 💬 Community feedback

### Month 3
- 📊 500+ downloads
- ⭐ GitHub stars
- 🤝 Community contributions

### Month 6
- 📊 2000+ downloads
- 🔄 Regular updates
- 🏭 Production usage examples

## See Also

- [HEX_PUBLICATION.md](HEX_PUBLICATION.md) - Detailed publication strategy
- [REPOSITORY_SEPARATION.md](REPOSITORY_SEPARATION.md) - Repo separation guide
- [CI/CD.md](CI_CD.md) - CI/CD for separated repositories
