import re
from unicodedata import normalize

def clean_string(text):
    """
    Cleans non-breaking spaces and removes non-ASCII characters.

    Args:
        text (str): The text to clean.

    Returns:
        str: Cleaned text with non-breaking spaces removed and non-ASCII
                characters stripped.
    """
    
    text =  text.replace('\xa0', ' ').strip().encode('ascii', 'ignore').decode("iso-8859-8")
    text = text.replace("Conicts of Interest", "Conflict of Interest")
    text = text.replace("condential","confidential")
    text = text.replace("supplement le","supplement file")
    text = text.replace("Charit ", "Charité ")
    text = text.replace("Charite ", "Charité ")
    text = text.replace("Charite ","Charité ")
    text = text.replace("nngen./","finngen.fi/")
    text = text.replace("Source Data le","Source Data file")
    text = text.replace("the ndings","the findings")
    text = text.replace("Data and code availability d","Data and code availability")
    text = text.replace(".gshare",".figshare")
    text = text.replace("/gshare","/figshare")
    text = text.replace("num ber","number")
    text = text.replace("Pzer","Pfizer")
    text = text.replace("(ttps","(https")
    text = text.replace("data les","data files")
    text = text.replace("data_les","data_files")
    text = text.replace("additional le","additional file")
    text = text.replace("data le","data file")
    text = text.replace("da ta","data")
    text = text.replace("/ m9","/m9")
    text = text.replace("Conict of Interest","Conflict of Interest")
    text = text.replace("pub licly","publicly")
    text = text.replace("comprise","compromise")
    text = text.replace("sensi ble","sensible")
    text = re.sub(r'www\.\s+', 'www.', text)

    return text


def format_url_string_pattern(input_string):
    """
    Formats a URL string into a regex pattern that allows for flexible matching
    by handling whitespace and newline characters, as well as special characters.

    Args:
        input_string (str): The URL string to format.

    Returns:
        str: A regex pattern for flexible matching of the URL.
    """
    join_non_special_char = r"(\s*|\\n*)"
    join_special_char = r"(\s*|\\n*)" + "\\"
    
    result = []
    
    def is_special_char(char):
        special_chars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        return char in special_chars
    
    for char in input_string:
        if is_special_char(char):
            result.append(join_special_char + char)
        else:
            result.append(join_non_special_char + char)
    
    return ''.join(result)

def extract_links(link_data):
    """
    Extracts unique URLs from a list of link data dictionaries.

    Args:
        link_data (list): A list of dictionaries containing link information.

    Returns:
        list: A list of unique URLs found in the link data.
    """
    links = [link['uri'] for link in link_data if 'uri' in link]
    return list(set(links))

def join_text(text):
    """
    Joins text by removing line breaks, special characters, and unnecessary whitespace.

    Args:
        text (str): The text to join.

    Returns:
        str: Joined and cleaned text.
    """
    text = text.replace("¼", "=")
    text = " ".join(text.split("\n"))
    text = text.replace("- ","").replace("  "," ")
    return text


def replace_text_with_links(text, links):
    """
    Replaces occurrences of links in the text with the actual link values.

    Args:
        text (str): The text to search and replace links in.
        links (list): A list of link strings to replace in the text.

    Returns:
        str: Text with links replaced by their actual values.
    """
    for link in links:
        if "penalty" in link or "ignorespaces" in link:
            continue
        try:
            link = link.replace("%20\\l%20", "#")
            pattern = format_url_string_pattern(str(link))
            text = re.sub(pattern, f" {link} ", text, flags=re.IGNORECASE)
        except re.error:
            print(f"Error with link: {link}")
    return text

def process_page_text(text,links):
    """
    Process text from a page by cleaning it and replacing any links.

    Args:
        text (str): The text to process
        links: links within the page

    Returns:
        str: The processed text with cleaned content and replaced links
    """
    text = clean_string(text)

    if links:
        text = replace_text_with_links(text, links)
    
    text = join_text(text + '\n')
    return text

def remove_unicode(text): 
    """
    Removes Unicode characters from text by encoding to ASCII and ignoring non-ASCII characters.

    Args:
        text (str): The text to remove Unicode characters from.

    Returns:
        str: Text with Unicode characters removed.
    """
    return normalize('NFKD', text).encode('ascii','ignore').decode('ascii')  