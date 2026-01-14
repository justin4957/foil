# CI/CD Pipeline Documentation

## Overview

The Foil project uses GitHub Actions for automated testing, building, code quality checks, and deployment. The CI/CD pipeline ensures code quality, prevents regressions, and enables confident iteration.

## Workflows

### 1. Main CI Workflow (`ci.yml`)

**Triggers**:
- Push to `main` branch
- Push to any `feature/**` branch
- Pull requests to `main`

**Jobs**:

#### Test Jobs (Parallel)
- `test-modal-logic` - Test modal_logic package
- `test-anthropic` - Test anthropic_gleam package
- `test-z3` - Test z3_gleam package (with Z3 installation)

#### Build Jobs (Sequential)
- `build-packages` - Build all 3 packages (needs: tests)
- `build-analyst` - Build analyst app + escript (needs: build-packages)

#### Code Quality Jobs (Parallel)
- `lint-modal-logic` - Check formatting for modal_logic
- `lint-anthropic` - Check formatting for anthropic_gleam
- `lint-z3` - Check formatting for z3_gleam

#### Summary
- `ci-success` - Final success gate (needs: all jobs)

**Duration**: ~2-3 minutes total

---

### 2. Coverage Workflow (`coverage.yml`)

**Triggers**:
- Push to `main`
- Pull requests to `main`

**Features**:
- Runs full test suite
- Generates coverage statistics
- Posts coverage report to PR comments
- Updates GitHub Step Summary

**Report Includes**:
- Total test count
- Test module count
- Source file count
- Estimated coverage percentage

---

### 3. Performance Benchmark Workflow (`performance.yml`)

**Triggers**:
- Push to `main`
- Pull requests to `main`
- Weekly schedule (Mondays at 00:00 UTC)

**Features**:
- Runs benchmark tests
- Measures test suite performance
- Tracks verification time targets
- Posts benchmark report to PR comments
- Weekly automated runs for trend tracking

**Performance Targets**:
- Average verification: <2s
- Simple formulas: <500ms
- Complex formulas: <5s
- Test suite: <60s total

---

## CI/CD Architecture

```
┌─────────────────────────────────────────┐
│         GitHub Event (Push/PR)          │
└─────────────────┬───────────────────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
    ▼             ▼             ▼
┌────────┐  ┌────────┐  ┌────────┐
│ Test   │  │ Test   │  │ Test   │
│modal   │  │anthro  │  │ z3     │
│_logic  │  │pic     │  │_gleam  │
└───┬────┘  └───┬────┘  └───┬────┘
    │           │           │
    └───────────┼───────────┘
                │
    ┌───────────┼───────────┐
    │           │           │
    ▼           ▼           ▼
┌────────┐  ┌────────┐  ┌────────┐
│ Lint   │  │ Build  │  │Coverage│
│Checks  │  │Packages│  │Report  │
└───┬────┘  └───┬────┘  └────────┘
    │           │
    └───────────┼───────────┐
                │           │
                ▼           ▼
           ┌────────┐  ┌────────┐
           │ Build  │  │Benchmark
           │Analyst │  │Report  │
           └───┬────┘  └────────┘
               │
               ▼
           ┌────────┐
           │Success │
           └────────┘
```

## Job Details

### Test Jobs

Each package is tested independently in parallel:

```yaml
test-modal-logic:
  - Setup Gleam + OTP
  - Download dependencies
  - Run: gleam test
  - Duration: ~30s
```

**Advantages**:
- Fast parallel execution
- Isolated test environments
- Clear failure identification
- Independent package validation

### Build Jobs

Sequential build ensures dependency order:

```yaml
build-packages:
  needs: [test-modal-logic, test-anthropic, test-z3]
  - Build all 3 packages
  - Duration: ~15s

build-analyst:
  needs: [build-packages]
  - Build analyst app
  - Build escript
  - Duration: ~20s
```

### Lint Jobs

Formatting checks run in parallel:

```yaml
lint-modal-logic:
  - Check: gleam format --check src test
  - Duration: ~10s
```

**What's Checked**:
- Consistent indentation
- Gleam style guide compliance
- Code formatting standards

## Badge Status

Add to README.md:

```markdown
[![CI Status](https://github.com/justin4957/foil/actions/workflows/ci.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/ci.yml)
[![Coverage](https://github.com/justin4957/foil/actions/workflows/coverage.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/coverage.yml)
[![Performance](https://github.com/justin4957/foil/actions/workflows/performance.yml/badge.svg)](https://github.com/justin4957/foil/actions/workflows/performance.yml)
```

## Local Testing

Run the same checks locally before pushing:

### Full Test Suite
```bash
# Test all packages
cd packages/modal_logic && gleam test
cd packages/anthropic_gleam && gleam test
cd packages/z3_gleam && gleam test

# Build analyst
cd apps/analyst && gleam build
```

### Format Check
```bash
# Check formatting
cd packages/modal_logic && gleam format --check src test

# Auto-format
gleam format src test
```

### Build Check
```bash
# Build all packages
cd packages/modal_logic && gleam build
cd packages/anthropic_gleam && gleam build
cd packages/z3_gleam && gleam build
```

## CI/CD Best Practices

### For Contributors

1. **Run tests locally** before pushing
2. **Format code** with `gleam format src test`
3. **Check CI status** on your PR
4. **Fix failures quickly** to unblock reviews
5. **Don't force push** after CI starts

### For Maintainers

1. **Require CI success** before merging
2. **Monitor performance** trends weekly
3. **Update versions** in workflow env vars
4. **Review coverage** reports regularly
5. **Investigate failures** promptly

## Troubleshooting

### Common Issues

**Tests failing locally but pass in CI**:
- Check Gleam/OTP versions match
- Verify dependencies are up to date
- Check for environment-specific issues

**Formatting check failing**:
- Run `gleam format src test` locally
- Commit formatted files
- Push changes

**Build failing for analyst**:
- Check package dependencies are built
- Verify Z3 is installed (for z3_gleam)
- Check manifest.toml is committed

**Coverage report not appearing**:
- Check PR comment permissions
- Verify GITHUB_TOKEN has write access
- Check workflow logs for errors

## Performance Monitoring

### Weekly Benchmarks

Automatic weekly runs track performance trends:

**Metrics Tracked**:
- Test suite execution time
- Individual test module times
- Verification performance (estimated)
- Build times per package

**Alerts Triggered**:
- Test suite >60s (threshold exceeded)
- Individual test >5s (slow test detected)
- Build time >2min (dependency issues)

### Regression Detection

Performance benchmarks alert on regressions >10%:

```yaml
- name: Check for regressions
  run: ./scripts/check_performance.sh
```

Future enhancement: Store historical data and generate trend charts.

## Future Enhancements

### Planned Improvements

1. **Code Coverage Integration**
   - Codecov or Coveralls integration
   - Line-by-line coverage tracking
   - Coverage badges per package
   - Coverage requirements (90%+)

2. **Deployment Automation**
   - Staging deployment on PR merge
   - Production deployment on main merge
   - Automated rollback on failure
   - Blue-green deployment strategy

3. **Security Scanning**
   - Dependency vulnerability scanning
   - SAST (static analysis security testing)
   - Secret detection
   - License compliance checking

4. **Release Automation**
   - Automated version bumping
   - Changelog generation
   - GitHub release creation
   - Package publishing to Hex

5. **Integration Testing**
   - End-to-end API tests
   - WebSocket integration tests
   - Database integration tests (PostgreSQL)
   - External dataset tests

## Maintenance

### Updating Gleam Version

1. Update `GLEAM_VERSION` in workflow env
2. Test locally with new version
3. Update in all workflows consistently
4. Document breaking changes

### Updating OTP Version

1. Update `OTP_VERSION` in workflow env
2. Verify compatibility with Gleam version
3. Test all packages
4. Update documentation

### Adding New Packages

1. Add test job for new package
2. Add build step in build-packages
3. Add lint job for formatting
4. Update ci-success dependencies
5. Document in this file

## CI/CD Metrics

### Current Stats

**Pipeline Performance**:
- Total duration: ~2-3 minutes
- Parallel test execution: ~35s
- Sequential build: ~35s
- Lint checks: ~10s

**Test Coverage** (estimated):
- modal_logic: 90%+
- anthropic_gleam: 85%+
- z3_gleam: 85%+
- Overall: ~90%

**Success Rate**:
- Main branch: >99%
- Feature branches: >95%
- Time to feedback: <3 minutes

## See Also

- [Testing Guide](../packages/modal_logic/docs/TESTING.md) - Comprehensive testing documentation
- [Development Guide](DEVELOPMENT.md) - Setup and workflow
- [Contributing Guide](CONTRIBUTING.md) - Contribution guidelines
- [Performance Benchmarks](../packages/modal_logic/test/benchmark_test.gleam) - Benchmark tests
