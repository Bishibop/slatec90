# Documentation Guide

## Active Documentation

### Root Directory
- **`README.md`** - Main project overview and quick start
- **`CURRENT_STATE.md`** - Detailed implementation status and architecture
- **`MIGRATION_QUICKSTART.md`** - Quick start guide for running migrations

### Reference Documentation (`claude_docs/`)
- **`DEPENDENCY_MAP.md`** - Complete function dependency graph
- **`SLATEC_MIGRATION_PLAN.md`** - Original comprehensive migration strategy
- **`REPO_MAP.xml`** - Complete SLATEC codebase analysis

### Implementation Guides (`claude_docs/guides/`)
- **`TEST_CASE_GENERATION_GUIDE.md`** - Patterns for generating test cases
- **`TESTING_IMPLEMENTATION_GUIDE.md`** - Practical testing approach
- **`AI_ONBOARDING.md`** - Quick reference for AI assistants

### Examples (`claude_docs/examples/`)
- **`ENORM_MIGRATION_EXAMPLE.md`** - Complete migration example

## Archived Documentation (`claude_docs/archive/`)
- `AUTOMATED_MIGRATION_PIPELINE.md` - Original complex pipeline (superseded by simple_migration_pipeline.py)
- `PHASE2_RESULTS.md` - Early analysis results
- `SLATEC_TESTING_FRAMEWORK.md` - Testing framework (incorporated into pipeline)

## Implementation Files

### Core Pipeline
- **`simple_migration_pipeline.py`** - Main migration script
- **`llm_config.py`** - OpenAI configuration
- **`strip_markdown.py`** - Helper utilities

### Test Scripts (`test_scripts/`)
- Various test and debugging scripts used during development

## Quick Reference

For **getting started**: Read `README.md` then `MIGRATION_QUICKSTART.md`

For **understanding the system**: Read `CURRENT_STATE.md`

For **function dependencies**: See `claude_docs/DEPENDENCY_MAP.md`

For **test patterns**: See `claude_docs/guides/TEST_CASE_GENERATION_GUIDE.md`