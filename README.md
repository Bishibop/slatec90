# AI-Assisted Legacy Codebase Migration

*Exploring how AI can accelerate the modernization of legacy codebases*

## The Legacy Migration Problem

Millions of lines of legacy code across industries need modernization. The traditional approach is manual, expensive, error-prone, and requires deep domain expertise.

**The Question**: Can AI meaningfully assist in legacy code migration while maintaining correctness?

## Our AI-Assisted Migration Approach

**Hypothesis**: AI can accelerate legacy codebase modernization when paired with robust validation.

**Core Strategies**:
- **LLM-powered code translation** and modernization
- **Legacy-as-Oracle validation**: Using original code to validate migrations
- **Automated discovery** and metadata extraction
- **Zero-trust verification**: Every AI migration must prove correctness

## SLATEC Test Case

**Why SLATEC**: 700+ mathematical functions with well-defined correctness criteria
**Migration Target**: Fortran 77 → Fortran 90/95
**Validation Advantage**: Mathematical functions have clear right/wrong answers
**Scale**: Real-world complexity with manageable scope

## Technical Implementation

- **AI Migration Pipeline**: LLM translates legacy → modern code
- **Validation Framework**: Automated comparison system
- **Oracle Strategy**: Legacy code generates test cases for migration validation
- **Metadata-Driven Architecture**: Scales to large codebases

## What We Learned

- **AI Migration Patterns**: Where LLMs excel vs struggle in code translation
- **Validation Strategies**: Effective approaches for ensuring migration correctness
- **Scalability Factors**: What makes AI-assisted migration practical for large codebases
- **Quality Assurance**: Building trust in AI-generated code changes

## Getting Started

### See AI Migration in Action

```bash
cd fortran_validator
make           # Build the validator
./validator < test_cases.txt
```

### Test Format

The validator uses simple text-based test cases:

```
FUNCTION: PYTHAG

TEST_START
Simple 3-4-5 triangle
PARAMS: 3.0 4.0
TEST_END
```

### Example Output

```
============================================================
VALIDATING FUNCTION: PYTHAG
============================================================
PASS: Simple 3-4-5 triangle
  F77 result:      5.00000000E+00
  Modern result:   5.00000000E+00
  Perfect match ✓
```

### Run Full Migration Pipeline

Experience the complete AI-assisted migration process:

```bash
python slatec_orchestrator.py --function FUNCNAME
```

### Explore the Approach

- **`fortran_validator/`**: Pure Fortran validation framework with F77-as-Oracle
- **`modernizer.py`**: LLM-powered F77→F90 translation engine
- **`test_generator.py`**: AI-generated comprehensive test suites
- **`modern/`**: 18 successfully migrated functions

## Applications Beyond SLATEC

- **Enterprise Legacy Systems**: Apply methodology to business-critical code
- **Language Migrations**: Adapt approach for different language pairs
- **Framework Modernization**: Migrate to modern libraries/frameworks
- **Technical Debt Reduction**: Systematic approach to code modernization

## Project Structure

```
slatec_test/
├── slatec_orchestrator.py      # Main AI migration automation pipeline
├── modernizer.py               # LLM-powered F77→F90 translation engine  
├── test_generator.py           # AI-generated comprehensive test suites
├── test_parameter_validator.py # Automatic test parameter validation
├── config.json                 # AI provider configuration
├── requirements.txt            # Python dependencies
├── src/                        # Original SLATEC F77 source files (Oracle)
├── modern/                     # AI-migrated Fortran 90/95 implementations
├── fortran_validator/          # F77-as-Oracle validation framework
├── test_cases/                 # Generated test cases
├── logs/                       # Migration results and analysis
└── docs/                       # Technical documentation
```

## Prerequisites

- **Fortran Compiler**: gfortran 8.0+ (or compatible Fortran compiler)
- **Python**: 3.8+ with pip
- **Build Tools**: make, git
- **AI API**: OpenAI or Google Gemini API key for code generation

## Installation & Setup

### 1. Clone and Navigate

```bash
git clone <repository-url>
cd slatec_test
```

### 2. Install Python Dependencies

```bash
pip install -r requirements.txt
```

### 3. Set up AI Provider

Create `config.json` in the root directory:

```json
{
  "llm_provider": "gemini",  // or "openai"
  "gemini_model": "gemini-2.5-flash",
  "openai_model": "o3-mini"
}
```

Add your API key as an environment variable:

```bash
# For OpenAI
export OPENAI_API_KEY="sk-your-key-here"

# For Gemini
export GEMINI_API_KEY="your-key-here"
```

### 4. Build the Validator

```bash
cd fortran_validator
make clean && make
```

### 5. Verify Installation

Test the validator with existing functions:

```bash
./validator < ../test_cases/pythag_tests.txt
```

You should see validation results for the PYTHAG function.