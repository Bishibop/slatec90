# Documentation Update Plan

**Date**: July 26, 2025  
**Purpose**: Comprehensive plan to update all documentation to reflect current system

## Current System Status

We have successfully:
1. Implemented a generic, metadata-driven validator system
2. Renamed files for cleaner, more intuitive names
3. Completed 9 functions (7 from Phase 0 + PYTHAG + CDIV)
4. Established a flexible, non-phase-based architecture

## Documentation Categories & Actions

### 1. **MUST UPDATE** - References Old System

#### README.md
- **Issues**: References non-existent test helpers (optimized_test_helper.py, slatec_test_helper.py)
- **Actions**: 
  - Replace with orchestrator usage: `python slatec_orchestrator.py --function FUNCNAME`
  - Update progress numbers (9 completed, not 10)
  - Add section on new architecture
  - Update prerequisites and quick start

#### COMPREHENSIVE_MIGRATION_GUIDE.md & MIGRATION_GUIDE.md
- **Issues**: Both reference old test helpers
- **Actions**: 
  - Consider merging these (redundant content)
  - Update with orchestrator commands
  - Add metadata system explanation

### 2. **ARCHIVE** - Phase-Based Documents (Move to `docs/archive/`)

These documents describe the old phase-based approach:
- PHASE_0_IMPLEMENTATION_PLAN.md
- PHASE_0_IMPLEMENTATION_STATUS.md  
- PHASE_0_SETUP.md
- phase_0_design_review.md
- PHASE_0_SYSTEM_DESIGN.md (keep parts about error handling)

### 3. **UPDATE** - Technical Guides

#### SLATEC_MODERNIZATION_GUIDE.md
- Add section on metadata-driven validation
- Update with lessons from PYTHAG/CDIV
- Include IEEE special value handling

#### SLATEC_TEST_GENERATION_GUIDE.md
- Update to describe test_generator.py
- Add constraint validation patterns
- Include examples from completed functions

#### SLATEC_VALIDATION_REPORT.md
- Update with generic validator architecture
- Add performance metrics from completed functions

### 4. **CREATE NEW** - Current System Documentation

#### SYSTEM_ARCHITECTURE.md
Document the current system:
- Component overview (orchestrator, test gen, modernizer, validator)
- Metadata system explanation
- Function processing pipeline
- Usage examples

#### QUICKSTART.md
Simple guide for new users:
- Installation
- Environment setup (.env file)
- Basic commands
- Common workflows

### 5. **KEEP AS-IS** - Reference Documents

These remain valuable without changes:
- SLATEC_COMPLEXITY_CATEGORIZATION.md
- SLATEC_ERROR_HANDLING_MIGRATION.md
- KNOWLEDGEBASE.md
- STATEFUL_FUNCTIONS_ISSUE.md
- fortran_validator/SLATEC_X_FUNCTIONS_ANALYSIS.md

### 6. **KEEP** - Journal/Historical

Valuable historical record:
- journal/JOURNAL.md
- journal/FORTRAN_VALIDATOR.md
- journal/PHASE_0_COMPLETE.md
- journal/TEST_DATA_DEBACLE_2025-07-23.md
- journal/GENERIC_VALIDATOR_IMPLEMENTATION.md (new)

### 7. **REMOVE** - Outdated/Experimental

- journal/PERFORMANCE_OPTIMIZATIONS.md (about old test helpers)
- openai_experiment/*.md (old experiments)

## Proposed Directory Structure

```
slatec_test/
├── README.md (updated)
├── QUICKSTART.md (new)
├── SYSTEM_ARCHITECTURE.md (new)
├── SLATEC_ITERATIVE_MASTER_PLAN.md (already updated)
├── docs/
│   ├── guides/
│   │   ├── SLATEC_MODERNIZATION_GUIDE.md (updated)
│   │   ├── SLATEC_TEST_GENERATION_GUIDE.md (updated)
│   │   └── SLATEC_VALIDATION_GUIDE.md (updated from REPORT)
│   ├── reference/
│   │   ├── SLATEC_COMPLEXITY_CATEGORIZATION.md
│   │   ├── SLATEC_ERROR_HANDLING_MIGRATION.md
│   │   ├── KNOWLEDGEBASE.md
│   │   └── STATEFUL_FUNCTIONS_ISSUE.md
│   └── archive/
│       └── phase_based/
│           ├── PHASE_0_*.md (all phase docs)
│           └── old_test_helpers/
│               ├── COMPREHENSIVE_MIGRATION_GUIDE.md
│               └── MIGRATION_GUIDE.md
├── journal/
│   └── (all journal entries)
└── fortran_validator/
    └── README.md (update with validator architecture)
```

## Priority Order

1. **Immediate**: Update README.md and create QUICKSTART.md
2. **High**: Create SYSTEM_ARCHITECTURE.md
3. **Medium**: Update technical guides
4. **Low**: Archive old documents

## Key Messages to Emphasize

1. **Generic Architecture**: No phases, flexible function lists
2. **Metadata-Driven**: Single source of truth for function signatures
3. **Continuous Evolution**: Tools improve in-place
4. **Current Progress**: 9 functions complete, system fully operational
5. **Easy to Use**: Simple orchestrator commands

This plan will give us clean, current documentation that accurately reflects the system and makes it easy for new contributors to understand and use.