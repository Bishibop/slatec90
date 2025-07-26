# Phase 0 Setup Instructions

## Prerequisites

1. **Python 3.8+** with pip
2. **gfortran** compiler
3. **OpenAI API key** for o3-mini model access

## Installation

### 1. Install Python Dependencies

```bash
pip install -r requirements.txt
```

### 2. Set Up Environment Variables

Create a `.env` file in the project root:

```bash
cp .env.example .env
```

Edit `.env` and add your OpenAI API key:
```
OPENAI_API_KEY=your-api-key-here
```

Optional environment variables:
- `OPENAI_MODEL`: Override default model (default: o3-mini)
- `OPENAI_TEMPERATURE`: Override temperature (default: 0.1)

### 3. Verify Setup

Test that the orchestrator runs:
```bash
python3 phase_0_orchestrator.py --help
```

## Usage

### Test Single Function
```bash
python3 phase_0_orchestrator.py --function PIMACH
```

### Run All Functions (Sequential)
```bash
python3 phase_0_orchestrator.py --sequential
```

### Run All Functions (Parallel)
```bash
python3 phase_0_orchestrator.py
```

## Configuration

The system uses configuration from multiple sources (in order of precedence):
1. Environment variables (`.env` file)
2. Command-line arguments
3. `config.json` file
4. Default values

### Environment Variables
- `OPENAI_API_KEY`: Required - Your OpenAI API key
- `OPENAI_MODEL`: Optional - Model to use (default: o3-mini)
- `OPENAI_TEMPERATURE`: Optional - Temperature setting (default: 0.1)

### config.json
Optional configuration file for non-sensitive settings:
```json
{
    "source_dir": "src",
    "modern_dir": "modern/phase_0",
    "test_dir": "test_cases/phase_0",
    "log_dir": "logs/phase_0",
    "work_dir": "work/phase_0",
    "max_iterations": 5,
    "parallel_workers": 4,
    "validator_executable": "fortran_validator/mega_validator_full"
}
```

## Directory Structure

After running, the following directories will be populated:
- `modern/phase_0/`: Modernized F90 implementations
- `logs/phase_0/`: Detailed logs and analysis
- `work/phase_0/`: Progress tracking and compiled objects
- `test_cases/phase_0/`: Test cases (already created)

## Monitoring Progress

### Check Progress
```bash
cat work/phase_0/progress.json | jq
```

### View Logs
```bash
# Latest log
tail -f logs/phase_0/phase_0_*.log

# Function-specific analysis
cat logs/phase_0/pimach_analysis.json | jq
```

## Troubleshooting

### "OPENAI_API_KEY environment variable not set"
Make sure you've created `.env` file and added your API key:
```bash
echo "OPENAI_API_KEY=sk-..." > .env
```

### Module not found errors
Install dependencies:
```bash
pip install -r requirements.txt
```

### Compilation errors
Make sure gfortran is installed:
```bash
gfortran --version
```

## Security Notes

- Never commit `.env` file (it's in `.gitignore`)
- Keep your API key secure
- Use environment variables for all sensitive data