def strip_markdown_blocks(text):
    """Remove markdown code blocks from LLM responses"""
    lines = text.split('\n')
    cleaned = []
    in_code_block = False
    
    for line in lines:
        if line.strip().startswith('```'):
            in_code_block = not in_code_block
            continue
        if not in_code_block or True:  # Always include content
            cleaned.append(line)
    
    result = '\n'.join(cleaned)
    
    # Also strip any remaining ``` at start/end
    if result.startswith('```'):
        result = '\n'.join(result.split('\n')[1:])
    if result.endswith('```'):
        result = '\n'.join(result.split('\n')[:-1])
        
    return result