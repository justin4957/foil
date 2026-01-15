# Repository Separation Guide

## Overview

This guide explains the strategy for separating `anthropic_gleam` and `z3_gleam` into standalone repositories for Hex publication, while keeping `modal_logic` in the main foil repository.

## Why Separate?

### Benefits

1. **Independent Evolution**: Each package can version and release independently
2. **Clearer Purpose**: Single-purpose repositories are easier to understand
3. **Focused Community**: Issues and PRs are specific to each package
4. **Hex Publication**: Easier to publish without monorepo complexity
5. **Broader Adoption**: General-purpose packages (anthropic, z3) useful beyond modal logic
6. **Smaller Clones**: Users only clone what they need

### Ecosystem Value

**anthropic_gleam**:
- First Gleam client for Claude API (no alternatives exist)
- Useful for ANY Gleam project needing LLM integration
- Not specific to modal logic

**z3_gleam**:
- First Gleam bindings for Z3 (no alternatives exist)
- Useful for verification, constraint solving, SMT
- Not specific to modal logic

**modal_logic**:
- Specific to foil project
- Integrates both above packages
- Makes sense to keep in foil repo

## Separation Plan

### Phase 1: Create Standalone Repositories

#### anthropic-gleam Repository

```bash
# Create new repo
gh repo create justin4957/anthropic-gleam \
  --public \
  --description "Gleam client for Anthropic's Claude API" \
  --clone

cd anthropic-gleam

# Copy package contents
cp -r ../foil/packages/anthropic_gleam/* .

# Initialize git (if not cloned)
git init
git add .
git commit -m "Initial commit: anthropic_gleam v0.1.0"
git remote add origin git@github.com:justin4957/anthropic-gleam.git
git push -u origin main

# Add topics for discoverability
gh repo edit --add-topic gleam,anthropic,claude,llm,api-client,ai
```

#### z3-gleam Repository

```bash
# Create new repo
gh repo create justin4957/z3-gleam \
  --public \
  --description "Gleam bindings for Z3 theorem prover" \
  --clone

cd z3-gleam

# Copy package contents
cp -r ../foil/packages/z3_gleam/* .

# Initialize git
git init
git add .
git commit -m "Initial commit: z3_gleam v0.1.0"
git remote add origin git@github.com:justin4957/z3-gleam.git
git push -u origin main

# Add topics
gh repo edit --add-topic gleam,z3,smt,theorem-prover,formal-verification
```

### Phase 2: Publish to Hex

#### Publish anthropic_gleam

```bash
cd anthropic-gleam

# Final validation
gleam format src test
gleam build
gleam test
gleam docs build

# Publish
gleam publish

# Tag release
git tag -a v0.1.0 -m "anthropic_gleam v0.1.0"
git push origin v0.1.0

# Create GitHub release
gh release create v0.1.0 \
  --title "anthropic_gleam v0.1.0" \
  --notes "Initial release of Gleam client for Claude API"
```

#### Publish z3_gleam

```bash
cd z3-gleam

# Final validation
gleam format src test
gleam build
gleam test
gleam docs build

# Publish
gleam publish

# Tag release
git tag -a v0.1.0 -m "z3_gleam v0.1.0"
git push origin v0.1.0

# Create GitHub release
gh release create v0.1.0 \
  --title "z3_gleam v0.1.0" \
  --notes "Initial release of Gleam bindings for Z3"
```

### Phase 3: Update modal_logic Dependencies

#### Update gleam.toml

```toml
# Before (path dependencies)
[dependencies]
anthropic_gleam = { path = "../anthropic_gleam" }
z3_gleam = { path = "../z3_gleam" }

# After (Hex dependencies)
[dependencies]
anthropic_gleam = "~> 0.1"
z3_gleam = "~> 0.1"
```

#### Update and Test

```bash
cd packages/modal_logic

# Update dependencies
gleam deps update

# Verify still works
gleam build
gleam test

# Commit changes
git add gleam.toml manifest.toml
git commit -m "chore: update to use published Hex packages"
```

### Phase 4: Publish modal_logic

```bash
cd packages/modal_logic

# Validate
gleam format src test
gleam build
gleam test
gleam docs build

# Publish
gleam publish

# Tag
git tag -a modal_logic-v0.1.0 -m "modal_logic v0.1.0"
git push origin modal_logic-v0.1.0
```

## File Migration Checklist

### What to Copy to New Repos

#### anthropic-gleam

- ✅ `src/` directory (all source code)
- ✅ `test/` directory (all tests)
- ✅ `gleam.toml` (package manifest)
- ✅ `README.md` (documentation)
- ✅ `LICENSE` file
- ✅ `.gitignore`
- ➕ `CHANGELOG.md` (create new)
- ➕ `.github/workflows/ci.yml` (create new, simplified)

#### z3-gleam

- ✅ `src/` directory
- ✅ `test/` directory
- ✅ `gleam.toml`
- ✅ `README.md`
- ✅ `LICENSE`
- ✅ `.gitignore`
- ➕ `CHANGELOG.md` (create new)
- ➕ `.github/workflows/ci.yml` (create new with Z3 install)

### What NOT to Copy

- ❌ `build/` directory (generated)
- ❌ Monorepo-specific scripts
- ❌ Other package references
- ❌ Foil-specific documentation

## CI/CD for Separated Repos

### anthropic-gleam CI

```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '27.2'
          gleam-version: '1.13.0'
      - run: gleam deps download
      - run: gleam test
      - run: gleam format --check src test
```

### z3-gleam CI

```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '27.2'
          gleam-version: '1.13.0'
      - name: Install Z3
        run: |
          sudo apt-get update
          sudo apt-get install -y z3
      - run: gleam deps download
      - run: gleam test
      - run: gleam format --check src test
```

## Maintaining Sync (Optional)

If you want to keep developing in monorepo and sync to separated repos:

### Approach 1: Git Subtree

```bash
# In foil repo, add remotes for separated repos
git remote add anthropic git@github.com:justin4957/anthropic-gleam.git
git remote add z3 git@github.com:justin4957/z3-gleam.git

# Push subtree
git subtree push --prefix=packages/anthropic_gleam anthropic main
git subtree push --prefix=packages/z3_gleam z3 main
```

### Approach 2: Manual Sync

```bash
# Copy changes manually when ready to release
rsync -av packages/anthropic_gleam/ ../anthropic-gleam/ --exclude build

cd ../anthropic-gleam
git add .
git commit -m "Sync from foil repo"
git push
```

### Approach 3: Develop in Separated Repos

```bash
# Use Hex packages in foil
cd foil/packages/modal_logic
# gleam.toml uses anthropic_gleam = "~> 0.1" from Hex

# Develop anthropic_gleam separately
cd ../../anthropic-gleam
# Make changes, publish new version
gleam publish
```

## Version Management

### Independent Versioning

Each package follows its own semver:

- `anthropic_gleam`: 0.1.0 → 0.2.0 → 1.0.0
- `z3_gleam`: 0.1.0 → 0.2.0 → 1.0.0
- `modal_logic`: 0.1.0 → 0.2.0 → 1.0.0

### Updating modal_logic

When anthropic_gleam releases 0.2.0:

```toml
# modal_logic/gleam.toml
[dependencies]
anthropic_gleam = "~> 0.2"  # Update to allow 0.2.x
```

## Communication Strategy

### Announcing Separation

**GitHub Issue** in foil repo:
```markdown
## 📦 Package Separation Announcement

We're separating `anthropic_gleam` and `z3_gleam` into standalone repositories!

### Why?
- Broader community benefit
- Independent evolution
- Easier discovery
- Hex publication

### New Repositories
- [anthropic-gleam](https://github.com/justin4957/anthropic-gleam)
- [z3-gleam](https://github.com/justin4957/z3-gleam)

### Migration
`modal_logic` will use published Hex packages instead of path dependencies.

No breaking changes for foil users!
```

### Gleam Community Announcement

**Gleam Discord/Forum**:
```markdown
🎉 New Gleam Packages on Hex!

I'm excited to announce two new packages for the Gleam ecosystem:

**anthropic_gleam** - Claude API client
- First Gleam client for Anthropic's Claude
- Streaming, tool use, retries
- Type-safe error handling
- https://hex.pm/packages/anthropic_gleam

**z3_gleam** - Z3 SMT solver bindings
- First Gleam bindings for Z3
- SMT solving, modal logic, verification
- https://hex.pm/packages/z3_gleam

Both are production-ready with comprehensive tests and docs!
```

## Timeline

### Week 1: Preparation
- Day 1-2: Create new repositories
- Day 3-4: Copy files and setup CI
- Day 5: Final testing and validation

### Week 2: Publication
- Day 1: Publish anthropic_gleam
- Day 2: Publish z3_gleam
- Day 3: Update modal_logic dependencies
- Day 4: Publish modal_logic
- Day 5: Announcements and promotion

### Week 3+: Maintenance
- Monitor for issues
- Respond to community questions
- Plan next releases

## Rollback Plan

If issues arise after publication:

1. **Yank Version**: `gleam publish --revert 0.1.0`
2. **Fix Issues**: Make corrections
3. **Publish Patch**: Release 0.1.1 with fixes
4. **Communicate**: Update GitHub and Hex docs

## Success Metrics

### Short-term (1 month)
- Packages published to Hex
- Documentation on hexdocs.pm
- GitHub repos have stars/watchers
- No critical bugs reported

### Medium-term (3 months)
- 100+ downloads per package
- Community contributions (issues, PRs)
- Other packages using our packages as deps
- Positive community feedback

### Long-term (6 months)
- 1000+ downloads
- Active maintenance
- Version 1.0.0 releases
- Adoption in production apps

## See Also

- [HEX_PUBLICATION.md](HEX_PUBLICATION.md) - Publication strategy and checklists
- [CI/CD.md](CI_CD.md) - Continuous integration for separated repos
- [DEVELOPMENT.md](DEVELOPMENT.md) - Development workflow
