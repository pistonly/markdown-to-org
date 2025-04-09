import re
import sys
import logging

# Check Python version
if sys.version_info < (3, 6):
    print("Error: Python 3.6 or higher is required")
    sys.exit(1)

# Configure logging
logging.basicConfig(level=logging.DEBUG,
                   format='%(asctime)s - %(levelname)s - %(message)s')

def convert_headings(text, start_head :str = "*"):
    """
    Convert Markdown headings to Org mode headings
    # Heading -> * Heading
    ## Heading -> ** Heading
    """
    head_offset = len(start_head) - 1
    for i in range(6, 0, -1):
        pattern = r'^' + r'#' * i + r'\s+(.*?)$'
        replacement = r'*' * (i + head_offset) + r' \1'
        text = re.sub(pattern, replacement, text, flags=re.MULTILINE)
    return text


def convert_bold(text):
    """
    Convert Markdown bold syntax to Org mode bold syntax
    **text** or __text__ -> *text*
    Do not match four consecutive *
    First match the entire bold section, then decide whether to add spaces based on whether there are whitespace characters before and after
    """
    def add_space_if_needed(match):
        # Get the entire matched text
        full_text = match.group(0)
        # Get characters before and after the bold text
        before = match.group(1)
        after = match.group(3)

        # Add space if there is no whitespace before
        if not before.isspace():
            before = ' '
        # Add space if there is no whitespace after
        if not after.isspace():
            after = ' '
            
        result = f"{before}*{match.group(2)}*{after}"
        return result

    # Handle **text** case
    text = re.sub(r'(\s*)(?<!\*)\*\*(?!\*)(.*?)(?<!\*)\*\*(?!\*)(\s*)', add_space_if_needed, text)
    # Handle __text__ case
    text = re.sub(r'(\s*)__(.*?)__(\s*)', add_space_if_needed, text)
    return text

def convert_italic(text):
    """
    Convert Markdown italic syntax to Org mode italic syntax
    *text* or _text_ -> /text/
    Exclude underscores in paths (e.g., /path/to/file_name)
    """
    # Convert *italic* (but not ​**​bold​**​)
    text = re.sub(r'(?<!\*)\*(?!\*)([^\s*][^*]*[^\s*])(?<!\*)\*(?!\*)', r'/\1/', text)
    
    # # Convert _italic_ but exclude paths (no / or \ around)
    # text = re.sub(r'(?<![\\/\\w])_(?!_)([^_]+)(?<!_)_(?![\\/\\w])', r'/\1/', text)
    return text

def convert_code(text):
    """
    Convert Markdown code syntax to Org mode syntax
    `code` -> ~code~
    ```lang -> #+BEGIN_SRC lang
    ``` -> #+END_SRC
    """
    text = re.sub(r'(?<!`)`([^`]+)`(?!`)', r'~\1~', text)

    # Code blocks
    text = re.sub(r'```(\w*)\n(.*?)```',
                  r'#+BEGIN_SRC \1\n\2\n#+END_SRC',
                  text,
                  flags=re.DOTALL)
    return text

def convert_links(text):
    """
    Convert Markdown link syntax to Org mode link syntax
    [text](link) -> [[link][text]]
    """
    text = re.sub(r'\[(.*?)\]\((.*?)\)', r'[[\2][\1]]', text)
    return text


def convert_images(text):
    """
    Convert Markdown image syntax to Org mode image syntax
    ![alt text](image link) -> [[image link][alt text]]
    """
    text = re.sub(r'!\[(.*?)\]\((.*?)\)', r'[[\2][\1]]', text)
    return text


def convert_lists(text):
    """
    Convert Markdown list syntax to Org mode list syntax
    - item -> - item
    1. item -> 1. item
    * item -> - item
    """
    # Convert * to - for unordered lists
    text = re.sub(r'^(\s*)\*(\s+)', r'\1-\2', text, flags=re.MULTILINE)
    return text


def convert_horizontal_rule(text):
    """
    Convert Markdown horizontal rule to Org mode horizontal rule
    --- or *** or ___ -> -----
    """
    text = re.sub(r'^(\*\*\*|---|___)$', r'-----', text, flags=re.MULTILINE)
    return text


def convert_blockquotes(text):
    """
    Convert Markdown blockquotes to Org mode blockquotes
    > text -> #+BEGIN_QUOTE\ntext\n#+END_QUOTE
    """
    # Identify consecutive quote blocks
    in_quote = False
    lines = text.split('\n')
    result = []
    quote_lines = []

    for line in lines:
        quote_match = re.match(r'^>\s*(.*?)$', line)
        if quote_match:
            if not in_quote:
                in_quote = True
                quote_lines = []
            quote_lines.append(quote_match.group(1))
        else:
            if in_quote:
                in_quote = False
                result.append("#+BEGIN_QUOTE")
                result.extend(quote_lines)
                result.append("#+END_QUOTE")
            result.append(line)

    # Handle quote blocks at the end of the file
    if in_quote:
        result.append("#+BEGIN_QUOTE")
        result.extend(quote_lines)
        result.append("#+END_QUOTE")

    return '\n'.join(result)


def convert_tables(text):
    """
    Attempt to convert Markdown tables to Org mode tables
    Not perfect, but works for simple tables
    """
    lines = text.split('\n')
    in_table = False
    table_lines = []
    result = []

    for line in lines:
        if re.match(r'^\|.*\|$', line):
            if not in_table:
                in_table = True
            table_lines.append(line)
        else:
            if in_table:
                in_table = False
                # Remove separator line |---|---|
                table_lines = [
                    l for l in table_lines
                    if not re.match(r'^\|\s*[-:]+\s*\|', l)
                ]
                # Replace | at the beginning of each line with space
                table_lines = [l.replace('|', ' |', 1) for l in table_lines]
                result.extend(table_lines)
            result.append(line)

    # Handle tables at the end of the file
    if in_table:
        table_lines = [
            l for l in table_lines if not re.match(r'^\|\s*[-:]+\s*\|', l)
        ]
        table_lines = [l.replace('|', ' |', 1) for l in table_lines]
        result.extend(table_lines)

    return '\n'.join(result)


def convert_checkboxes(text):
    """
    Convert Markdown checkboxes to Org mode checkboxes
    - [ ] task -> - [ ] task
    - [x] task -> - [X] task
    """
    text = re.sub(r'- \[ \]', r'- [ ]', text)
    text = re.sub(r'- \[x\]', r'- [X]', text, flags=re.IGNORECASE)
    return text


def convert_markdown_to_org(markdown_text: str,
                            indent: str = "",
                            start_head="*"):
    """
    Convert Markdown text to Org mode text
    """
    text = markdown_text

    if indent:
        text_list = text.splitlines()
        text = "\n".join([line[len(indent):] for line in text_list])

    # Convert various syntax
    # NOTE: convert_lists should be invoked before convert_headings
    text = convert_lists(text)
    text = convert_headings(text, start_head)
    text = convert_blockquotes(text)
    text = convert_code(text)
    # NOTE: convert_italic should invoked before convert_bold
    text = convert_italic(text)
    text = convert_bold(text)
    text = convert_links(text)
    text = convert_images(text)
    text = convert_horizontal_rule(text)
    text = convert_tables(text)
    text = convert_checkboxes(text)

    return text


