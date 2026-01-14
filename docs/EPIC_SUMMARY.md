# Epic: Foil System Enhancement Initiative

**Status**: Planning Complete
**Duration**: 12 months (5 phases)
**Priority**: High
**Generated**: January 14, 2026

---

## Overview

This epic captures the strategic enhancement of the Foil Modal Logic Engine based on multi-LLM consensus analysis. The initiative focuses on **narrowly but productively** increasing system capacity through practical, implementable features that leverage existing infrastructure.

---

## Goals

1. **3x User Adoption**: Increase active users by 300% through improved UX
2. **10x Capability**: Increase daily formula analysis throughput by 10x
3. **90%+ Quality**: Achieve 90%+ test coverage with <1% error rate
4. **Sub-2s Performance**: Maintain <2s average verification time

---

## Epics & Stories

### Epic 1: Foundation & Quick Wins (Months 1-2)

**Goal**: Deliver immediate value and foundational improvements

#### Story 1.1: Modal Logic System Profiles
- **User Story**: As a new user, I want to select a pre-configured modal logic system (K, T, S4, S5, etc.) so I don't have to manually configure settings
- **Acceptance Criteria**:
  - [ ] 7 profile configuration files (one per modal system)
  - [ ] API endpoint: `POST /api/profiles/:system`
  - [ ] CLI command: `foil --profile s5 analyze`
  - [ ] Web UI dropdown for profile selection
  - [ ] Documentation for each profile
- **Complexity**: Low (1-2 weeks)
- **Value**: ↑↑ User adoption

#### Story 1.2: Enhanced Error Reporting
- **User Story**: As a user encountering errors, I want clear, actionable error messages with suggestions so I can quickly resolve issues
- **Acceptance Criteria**:
  - [ ] Structured error codes (ParseError, TranslationError, VerificationError, ConfigurationError)
  - [ ] Context-aware messages with line numbers and suggestions
  - [ ] Logging levels (DEBUG, INFO, WARN, ERROR)
  - [ ] Error catalog documentation
  - [ ] Integration across all interfaces (API, CLI, Web UI)
- **Complexity**: Low-Medium (2-3 weeks)
- **Value**: ↑↑ User adoption, ↑↑ Quality

#### Story 1.3: Formula Pattern Library
- **User Story**: As a user writing formulas, I want access to common patterns and auto-suggestions so I can write formulas faster and with fewer errors
- **Acceptance Criteria**:
  - [ ] Pattern database with categories (epistemic, deontic, temporal, alethic)
  - [ ] API endpoint: `POST /api/suggest` (partial formula → suggestions)
  - [ ] Web UI integration with dropdown suggestions
  - [ ] At least 20 common patterns documented
  - [ ] Pattern search and filtering
- **Complexity**: Medium (3-4 weeks)
- **Value**: ↑↑↑ User adoption, ↑↑ Capability

#### Story 1.4: CI/CD Pipeline
- **User Story**: As a developer, I want automated testing and deployment so I can confidently iterate and deploy new features
- **Acceptance Criteria**:
  - [ ] GitHub Actions workflow configured
  - [ ] Unit tests run on every commit
  - [ ] Integration tests with PostgreSQL and Z3
  - [ ] Coverage reporting (target: 90%)
  - [ ] Automated deployment to staging/production
  - [ ] Performance regression tests
- **Complexity**: Medium (4-5 weeks)
- **Value**: ↑↑↑ Quality, ↑ Capability

---

### Epic 2: Strategic Enhancements (Months 3-4)

**Goal**: Multiply existing capabilities for research and enterprise use

#### Story 2.1: Dataset-Specific Templates
- **User Story**: As a domain researcher, I want pre-built modal templates for my dataset (FOLIO, LogiQA, InPhO) so I can immediately apply modal logic to my domain problems
- **Acceptance Criteria**:
  - [ ] FOLIO templates for logical reasoning
  - [ ] LogiQA templates for multi-hop inference
  - [ ] InPhO templates for philosophical arguments
  - [ ] CLI: `foil analyze --dataset=FOLIO --template=belief`
  - [ ] API: `POST /api/analyze/dataset/:name`
  - [ ] Template documentation with examples
- **Complexity**: Medium (3-4 weeks)
- **Value**: ↑↑ User adoption, ↑↑↑ Capability

#### Story 2.2: Batch Verification
- **User Story**: As a researcher, I want to verify multiple formulas across different modal systems simultaneously so I can systematically analyze logical properties
- **Acceptance Criteria**:
  - [ ] Batch API: `POST /api/batch/verify`
  - [ ] Comparison endpoint: `POST /api/compare/systems`
  - [ ] Parallel execution using OTP concurrency
  - [ ] Result aggregation with summary statistics
  - [ ] CLI: `foil batch verify formulas.json --systems K,T,S4`
  - [ ] JSON input/output format documented
- **Complexity**: Medium (3-4 weeks)
- **Value**: ↑↑ User adoption, ↑↑↑ Capability

#### Story 2.3: API Rate Limiting & Authentication
- **User Story**: As a system administrator, I want rate limiting and authentication so I can deploy a public API with controlled access
- **Acceptance Criteria**:
  - [ ] JWT-based authentication
  - [ ] Rate limiting tiers (Free: 100 req/min, Pro: 1000 req/min)
  - [ ] API key management: `POST /api/auth/keys`
  - [ ] Usage analytics dashboard
  - [ ] Documentation for authentication flow
- **Complexity**: Medium (2-3 weeks)
- **Value**: ↑ User adoption (enterprise), ↑↑ Quality

---

### Epic 3: User Experience Enhancements (Months 5-6)

**Goal**: Lower barriers to adoption through improved UX

#### Story 3.1: Formula Complexity Metrics
- **User Story**: As a user with slow verification, I want to see complexity metrics and optimization suggestions so I can write more efficient formulas
- **Acceptance Criteria**:
  - [ ] Metrics: modal depth, operator count, quantifier nesting
  - [ ] Estimated verification time prediction
  - [ ] Optimization suggestions based on equivalences
  - [ ] API: `POST /api/analyze/complexity`
  - [ ] Web UI display of metrics and suggestions
- **Complexity**: Medium (3-4 weeks)
- **Value**: ↑ User adoption, ↑↑ Capability

#### Story 3.2: Interactive Formula Editor
- **User Story**: As a Web UI user, I want syntax highlighting and autocompletion so I can write formulas efficiently with fewer errors
- **Acceptance Criteria**:
  - [ ] Monaco Editor integration
  - [ ] Syntax highlighting for modal operators
  - [ ] Autocompletion from pattern library
  - [ ] Real-time error checking with squiggles
  - [ ] Theme support (light/dark)
  - [ ] Keyboard shortcuts and accessibility (WCAG 2.1)
- **Complexity**: Medium (3-4 weeks)
- **Value**: ↑↑ User adoption, ↑↑ Quality

---

### Epic 4: Advanced Features (Months 7-9)

**Goal**: Transformational enhancements for education and extensibility

#### Story 4.1: Proof Visualization
- **User Story**: As a student or educator, I want interactive proof visualizations so I can understand how the verifier arrives at its conclusions
- **Acceptance Criteria**:
  - [ ] Proof tree extraction from Z3
  - [ ] D3.js interactive visualization
  - [ ] Kripke frame visualization
  - [ ] Step-by-step debugger with state inspection
  - [ ] WebSocket real-time updates
  - [ ] Playback controls (forward/backward)
- **Complexity**: High (6-8 weeks)
- **Value**: ↑↑↑ User adoption (education), ↑↑↑ Capability

#### Story 4.2: Plugin System
- **User Story**: As a researcher with specialized logic needs, I want to add custom logic systems through plugins so I can extend the system without modifying core code
- **Acceptance Criteria**:
  - [ ] Plugin API specification
  - [ ] Dynamic loading/unloading of plugins
  - [ ] Plugin registry and discovery
  - [ ] Example plugins: temporal logic (LTL, CTL)
  - [ ] Plugin development guide
  - [ ] Security sandboxing
- **Complexity**: High (8-10 weeks)
- **Value**: ↑↑ User adoption, ↑↑↑ Capability (extensibility)

---

### Epic 5: AI Integration (Months 10-12)

**Goal**: Intelligent assistance through machine learning

#### Story 5.1: ML-Powered Logic Prediction
- **User Story**: As a user, I want intelligent recommendations for logic systems and validity predictions so I can work more efficiently
- **Acceptance Criteria**:
  - [ ] Logic system classifier (NL → recommended system)
  - [ ] Validity predictor (formula + system → probability)
  - [ ] Training pipeline using FOLIO, LogiQA, InPhO datasets
  - [ ] API: `POST /api/predict/outcome`
  - [ ] Model accuracy: 80%+ on test set
  - [ ] Confidence scores displayed to users
- **Complexity**: High (10-12 weeks)
- **Value**: ↑↑↑ User adoption (smart assistance), ↑↑ Capability

---

## Roadmap Summary

| Phase | Duration | Focus | Stories | Value |
|-------|----------|-------|---------|-------|
| **Phase 1** | Months 1-2 | Foundation | 1.1-1.4 | Quick wins, CI/CD |
| **Phase 2** | Months 3-4 | Strategic | 2.1-2.3 | Enterprise, research |
| **Phase 3** | Months 5-6 | UX | 3.1-3.2 | Adoption barriers |
| **Phase 4** | Months 7-9 | Advanced | 4.1-4.2 | Education, plugins |
| **Phase 5** | Months 10-12 | AI | 5.1 | ML-powered features |

---

## Dependencies

### Technical Dependencies
- **Gleam/OTP**: Core runtime (existing)
- **Claude API**: LLM translation (existing)
- **Z3 Solver**: Verification engine (existing)
- **PostgreSQL**: Persistence (existing)
- **Redis**: Caching (optional, recommended)
- **Monaco Editor**: Web UI editor (new)
- **D3.js**: Visualization (new)
- **ML Frameworks**: TensorFlow/PyTorch (new, Phase 5)

### Team Dependencies
- **Gleam Developers**: 2-3 FTE (all phases)
- **Frontend Developer**: 1 FTE (Phases 3-4)
- **ML Engineer**: 0.5 FTE (Phase 5 only)
- **DevOps**: 0.5 FTE (Phase 1, ongoing)

---

## Success Metrics

### Phase-by-Phase Targets

| Phase | User Adoption | Capability | Quality | Performance |
|-------|--------------|------------|---------|-------------|
| **Phase 1** | +50% | +50% | 80% coverage | <3s avg |
| **Phase 2** | +100% | +200% | 85% coverage | <2.5s avg |
| **Phase 3** | +150% | +400% | 90% coverage | <2s avg |
| **Phase 4** | +250% | +800% | 90% coverage | <2s avg |
| **Phase 5** | +300% | +1000% | 90% coverage | <2s avg |

### Key Performance Indicators (KPIs)

1. **User Adoption**
   - Active users per month
   - New user registrations
   - User retention rate

2. **Capability**
   - Formulas analyzed per day
   - Batch processing volume
   - Dataset integrations

3. **Quality**
   - Test coverage percentage
   - User-reported bugs
   - API success rate
   - System uptime

4. **Performance**
   - Average verification time
   - API response time
   - P95/P99 latency

---

## Risks & Mitigation

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Z3 proof extraction slow | High | Medium | Caching, timeouts, complexity limits |
| ML predictions unreliable | Medium | High | Conservative thresholds, user overrides |
| Plugin security vulnerabilities | High | Medium | Sandboxing, code review, permissions |
| Gleam ecosystem immaturity | Medium | Low | Comprehensive docs, API stability |
| User adoption slower than expected | High | Medium | Focus on UX, templates, tutorials |

---

## Budget Estimate

### Development Costs (12 months)
- **Team**: 4 FTE × $100k/year = $400k
- **Infrastructure**: $2.5k/month × 12 = $30k
- **AI APIs**: $750/month × 12 = $9k
- **Contingency**: 10% = $44k

**Total**: ~$483k

### Cost Breakdown by Phase
- **Phase 1**: $80k (CI/CD, quick wins)
- **Phase 2**: $80k (strategic features)
- **Phase 3**: $80k (UX enhancements)
- **Phase 4**: $120k (advanced features, longer timeline)
- **Phase 5**: $123k (ML integration, training)

---

## Next Steps

1. **Stakeholder Review**: Present plan to stakeholders, gather feedback
2. **Resource Allocation**: Confirm team availability, budget approval
3. **Phase 1 Kickoff**: Begin with Story 1.1 (Modal Logic System Profiles)
4. **Sprint Planning**: Set up 2-week sprints, define sprint goals
5. **Regular Reviews**: Bi-weekly sprint reviews, monthly retrospectives

---

## Related Documents

- [Full Strategic Plan](STRATEGIC_PLAN.md) - Detailed analysis and recommendations
- [Foil Synopsis](foil_synopsis.md) - Current system overview
- [Testing Guide](TESTING.md) - Existing test infrastructure
- [Development Guide](DEVELOPMENT.md) - Setup and workflow

---

*This epic was generated from multi-agent consensus analysis (Claude, GPT-4o, DeepSeek, Gemini) on January 14, 2026.*
