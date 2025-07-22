# SLATEC Migration Pipeline Quick Start

## Setup

1. **Set your OpenAI API key:**
   ```bash
   export OPENAI_API_KEY='your-api-key-here'
   ```
   
   Or better, use a `.env` file:
   ```bash
   echo "OPENAI_API_KEY=your-key-here" > .env
   ```

2. **Install dependencies:**
   ```bash
   pip install openai python-dotenv
   ```

## Running the Migration

```bash
# Run the simple migration pipeline
python simple_migration_pipeline.py
```

This will:
1. Start with functions that have NO dependencies (denorm, ppsgf, pythag)
2. Use OpenAI's o3-mini model for intelligent code conversion
3. Generate test cases automatically
4. Run iterative refinement if tests fail
5. Save debug info for any failures

## What the Pipeline Does

1. **Reads F77 source** from `src/`
2. **Generates test cases** using o3-mini to analyze code paths
3. **Gets reference values** by compiling and running original F77
4. **Generates modern F90** code using o3-mini with proper module structure
5. **Tests and refines** up to 5 iterations with error feedback
6. **Saves results** to `modern/` with F77-compatible wrappers

## Directory Structure

```
slatec_test/
├── src/               # Original F77 files (don't modify)
├── modern/            # Generated F90 files
├── logs/              # Debug logs for failures
└── lib/               # Compiled libraries (future)
```

## Cost Estimates

Using o3-mini (as of January 2025):
- Input: $1.10 per million tokens
- Output: $4.40 per million tokens
- ~$0.001-0.005 per function migration
- Much cheaper than o1 (~93% less)
- Better reasoning than gpt-3.5-turbo for code tasks

## Starting Simple

The pipeline starts with the simplest functions:
- `denorm` - Euclidean norm (double precision)
- `ppsgf` - Very short (25 lines)
- `pythag` - Pythagorean sum

These have NO dependencies, making them ideal test cases.

## Monitoring Progress

Watch for:
- ✓ Successful migrations
- ✗ Failed migrations (check logs/)
- Iteration count (fewer is better)
- Total cost (displayed at end)

## Next Steps

Once simple functions work:
1. Add more complex functions to the list
2. Handle dependency chains
3. Build the two-library system
4. Scale to all 736 functions

## Troubleshooting

### o3-mini Access
- Need API tier 3-5 for o3-mini access
- Available in ChatGPT Plus/Pro/Team
- May need organization verification

### Common Issues
- **Compilation errors**: Check `modern/` for generated code
- **Test failures**: Check `logs/` for debug information
- **Token limits**: o3-mini supports up to 5000 completion tokens
- **Markdown in output**: Pipeline includes `strip_markdown.py` to handle this

## Example Output

```
==============================================================
Migrating: denorm
==============================================================
Dependencies: None

Generating test cases...
[LLM] Calling o3-mini with 3905 chars, max_tokens=5000...
[LLM] Using reasoning effort: medium
[LLM] Got response: 783 chars
Generated 8 test cases

Generating reference values from F77...
Generating modern Fortran version...
[LLM] Calling o3-mini with 4052 chars, max_tokens=5000...
[LLM] Got response: 2065 chars

Iteration 1:
  ✓ All 8 tests passed!

✓ Successfully migrated denorm in 1 iterations
```

## Key Features Added

1. **Error Feedback**: Compilation and runtime errors are captured and sent to LLM
2. **Markdown Stripping**: Handles LLM responses with ```fortran blocks
3. **Cost Tracking**: Monitors API usage costs
4. **Debug Logging**: Failed migrations save detailed info to `logs/`