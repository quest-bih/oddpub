import re
from sections import *

all_section_terms = (METHODS_TERMS + RESULTS_TERMS + DISCUSSION_TERMS + 
                        REFERENCES_TERMS + FUNDING + INTRODUCTION + CAS + 
                        ACNOWLEDGEMENTS + AUTH_CONT + ABBREVIATIONS + CONCLUSION + ABSTRACT +
                        LIMITATIONS + COI + SUPP_DATA + DATA_AVAILABILITY + ETHICS)

def remove_duplicate_pargraphs(text):
    """
    Removes duplicate paragraphs from text while preserving empty lines.
    
    Parameters:
    ----------
    text : str
        The input text to process.
        
    Returns:
    -------
    str
        The text with duplicate paragraphs removed, empty lines preserved.
    """
    # Split text into paragraphs and filter out duplicates while keeping empty lines
    paragraphs = [p for i, p in enumerate(text.split('\n')) 
                 if p.strip() == ''or p.strip() in all_section_terms or text.split('\n').index(p) == i]

    # Join paragraphs back into text
    return '\n'.join(paragraphs)
    
def extract_section(text, section_terms):
    """
    Extracts a specific section from the text based on provided section terms.
    A section starts with its terms (preceded by an empty line) and ends when another section begins.
    
    The function handles various section header formats:
    - Regular format (e.g., "Methods")
    - Spaced letters (e.g., "M E T H O D S")
    - Numbered sections (e.g., "1. Methods", "I. Methods")
    - Sections with pipes (e.g., "1 | INTRODUCTION")
    - Inline sections (e.g., "Methods: text continues...")
    
    Parameters:
    ----------
    text : str
        The input text from which to extract the section. Should be raw text content
        that may contain multiple sections.
    section_terms : list
        List of terms that indicate the start of the section. Case-insensitive matching
        is used for these terms.
        
    Returns:
    -------
    str
        The extracted section text, including the section header. Returns an empty string
        if the section is not found. For inline sections, returns only the header line.
        If multiple sections are found, returns the longest one.
        
    Notes:
    -----
    - The function first removes duplicate paragraphs from the input text
    - Section matching is case-insensitive
    - Section headers must be preceded by an empty line (except for inline sections)
    - Section ends when another known section header is encountered
    - When multiple sections match, the longest one is returned
    """
    
    # Create regex patterns for different formatting styles of the target section
    # Handle regular format, spaced letters, numbered sections, and sections with colons
    section_patterns = [
        # Regular format: "Methods"
        r'\n\s*(' + '|'.join(map(re.escape, section_terms)) + r')\s*[\n:]',
        
        # Spaced letters: "M E T H O D S"
        r'\n\s*(' + '|'.join(' '.join(term) for term in [[c for c in term] for term in section_terms]) + r')\s*[\n:]',
        
        # Numbered sections: "1. Methods", "I. Methods", etc.
        r'\n\s*(?:\d+\.|\[?\d+\]?\.?|[IVXivx]+\.)\s*(' + '|'.join(map(re.escape, section_terms)) + r')\s*[\n:]',
        
        # Numbered sections with pipe: "1 | INTRODUCTION"
        r'\n\s*(?:\d+\s*\|\s*)(' + '|'.join(map(re.escape, section_terms)) + r')\s*[\n:]',
        
        # inline matching
        r'\n\s*(' + '|'.join(map(re.escape, section_terms)) + r')[\s:]([^\n]+)'    
    ]
    
    # Find all occurrences of the target section using any of the patterns
    section_matches = []
    
    for pattern in section_patterns:
        matches = list(re.finditer(pattern, text, re.IGNORECASE))
        # print(matches)
        for match in matches:
            section_matches.append((match.start(), pattern))
    
    # Sort matches by position in text
    section_matches.sort(key=lambda x: x[0])
    
    # Create patterns for different formatting styles of the next section
    next_section_patterns = []
    for term in [term for term in all_section_terms if term not in section_terms]:
        # Regular format
        next_section_patterns.append(re.escape(term))
        # Spaced letters
        next_section_patterns.append(' '.join([c for c in term]))
    
    # Create patterns for next section (both regular and inline)
    next_section_pattern = r'\n\s*(?:\d+\.|\[?\d+\]?\.?|[IVXivx]+\.|\d+\s*\|\s*)?\s*(' + '|'.join(next_section_patterns) + r')\s*[\n:]'
    next_inline_pattern = r'\n\s*(' + '|'.join(next_section_patterns) + r')[\s:]'
    
    # Extract all sections and find the longest one
    extracted_sections = []
    
    for section_start, pattern in section_matches:
        # Find the next section after our target section (both regular and inline)
        remaining_text = text[section_start + 1:]
        next_section_match = re.search(next_section_pattern, remaining_text, re.IGNORECASE)
        next_inline_match = re.search(next_inline_pattern, remaining_text, re.IGNORECASE)
        
        # Reset matches if they start at position 0
        if next_section_match and next_section_match.start() == 0:
            next_section_match = None
        if next_inline_match and next_inline_match.start() == 0:
            next_inline_match = None
        
        # Calculate section end based on the earliest match
        section_end = None
        if next_section_match and next_inline_match:
            section_end = section_start + 1 + min(next_section_match.start(), next_inline_match.start())
        elif next_section_match:
            section_end = section_start + 1 + next_section_match.start()
        elif next_inline_match:
            section_end = section_start + 1 + next_inline_match.start()
            
        # Extract section text
        section_text = text[section_start:section_end].strip() if section_end else text[section_start:].strip()
        extracted_sections.append(section_text)
    
    # Return the longest extracted section
    if extracted_sections:
        return max(extracted_sections, key=len)
    else:
        return ""
    
def remove_references_section(text):
    """
    Removes the references section from a text document.
    
    This function identifies and removes the references section from the input text
    using the extract_section() function and predefined REFERENCES_TERMS. The function
    preserves all text before and after the references section.
    
    Args:
        text (str): Raw text content potentially containing a references section.
            The text can be in any format and may or may not contain a references section.
            
    Returns:
        str: The input text with the references section removed. If no references
            section is found, returns the original text unchanged.
            
    Example:
        >>> text = "Introduction\\n...\\nReferences\\n1. Smith et al...\\nConclusion"
        >>> result = remove_references_section(text)
        >>> print(result)
        "Introduction\\n...\\nConclusion"
    """
    # Use extract_section to get the references section
    references_section = extract_section(text, REFERENCES_TERMS)
    
    if not references_section:
        return text  # No references section found
    
    # Find where the references section starts in the original text
    ref_start = text.find(references_section)
    
    if ref_start == -1:
        return text  # Shouldn't happen if extract_section found something
      
    # remove only the reference section, keep the text before and after it 
    return text[:ref_start] + text[ref_start + len(references_section):]


# with open("pdfs/10.3390nu14010148/10.3390nu14010148.txt", "r") as f:
#     text = f.read()
#     text2 = remove_duplicate_pargraphs(text)
#     #save text2 to a file
#     with open("output_para.txt", "w") as f:
#         f.write(text2)

#     # print(extract_section(text,METHODS_TERMS))
    # print(extract_section(text,RESULTS_TERMS))
#     # print(extract_section(text,DISCUSSION_TERMS))
#     # print((extract_section(text,DATA_AVAILABILITY)))
#     # print(extract_section(text,REFERENCES_TERMS))
#     # print(remove_references_section(text))