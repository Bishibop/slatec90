"""
LLM Configuration for SLATEC Migration
Using OpenAI's o3-mini for cost-effective reasoning
"""

import os
from openai import OpenAI

# Try to load from .env file
try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    pass  # dotenv not installed, use environment variables

# Initialize OpenAI client (lazy loading)
_client = None

def get_client():
    global _client
    if _client is None:
        api_key = os.getenv("OPENAI_API_KEY")
        if not api_key:
            raise ValueError("OPENAI_API_KEY not set! Set it in .env file or environment")
        _client = OpenAI(api_key=api_key)
    return _client

def call_llm(prompt, model="o3-mini", reasoning_effort="medium", max_tokens=None):
    """
    Call OpenAI API with o3-mini or other models
    
    Args:
        prompt: The prompt to send
        model: Model to use (o3-mini, gpt-4-turbo-preview, gpt-3.5-turbo)
        reasoning_effort: For o3-mini only - "low", "medium", or "high"
        max_tokens: Maximum completion tokens
    """
    
    # Build messages
    messages = [{"role": "user", "content": prompt}]
    
    # Set default max_tokens based on model
    if max_tokens is None:
        if model in ["gpt-3.5-turbo", "gpt-4", "gpt-4-turbo-preview"]:
            max_tokens = 4000  # Safe limit for GPT models
        else:
            max_tokens = 5000  # Default for other models
    
    print(f"[LLM] Calling {model} with {len(prompt)} chars, max_tokens={max_tokens}...")
    
    try:
        client = get_client()
        if model == "o3-mini":
            # o3-mini specific parameters
            print(f"[LLM] Using reasoning effort: {reasoning_effort}")
            response = client.chat.completions.create(
                model=model,
                messages=messages,
                max_completion_tokens=max_tokens,
                reasoning_effort=reasoning_effort
            )
        else:
            # Other models (gpt-4, gpt-3.5-turbo)
            print(f"[LLM] Using temperature: 0.1")
            response = client.chat.completions.create(
                model=model,
                messages=messages,
                max_tokens=max_tokens,
                temperature=0.1  # Low for deterministic code generation
            )
            
        result = response.choices[0].message.content
        print(f"[LLM] Got response: {len(result)} chars")
        return result
        
    except Exception as e:
        print(f"[LLM] Error calling {model}: {e}")
        raise

# Model recommendations for different tasks
MODELS = {
    "code_generation": "o3-mini",  # Use o3-mini for everything
    "test_generation": "o3-mini",  
    "simple_analysis": "o3-mini"   
}

# Reasoning effort settings
REASONING_EFFORT = {
    "initial_generation": "medium",   # Balance speed and quality
    "bug_fixing": "high",            # Deep reasoning for fixing failures
    "simple_tasks": "low"            # Fast for simple transformations
}

# Cost tracking (approximate)
COSTS = {
    "o3-mini": {
        "input": 1.10 / 1_000_000,   # $1.10 per million input tokens
        "output": 4.40 / 1_000_000   # $4.40 per million output tokens
    },
    "gpt-4-turbo-preview": {
        "input": 10.00 / 1_000_000,  
        "output": 30.00 / 1_000_000
    },
    "gpt-3.5-turbo": {
        "input": 0.50 / 1_000_000,
        "output": 1.50 / 1_000_000
    }
}

def estimate_cost(prompt, response, model="o3-mini"):
    """Estimate cost for API call"""
    # Rough token estimation (1 token ≈ 4 chars)
    input_tokens = len(prompt) / 4
    output_tokens = len(response) / 4
    
    if model in COSTS:
        cost = (input_tokens * COSTS[model]["input"] + 
                output_tokens * COSTS[model]["output"])
        return cost
    return 0.0