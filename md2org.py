import re
import sys
import argparse
import logging

# 配置日志
logging.basicConfig(level=logging.DEBUG,
                   format='%(asctime)s - %(levelname)s - %(message)s')

def convert_headings(text, start_head :str = "*"):
    """
    将Markdown标题转换为Org模式标题
    # 标题 -> * 标题
    ## 标题 -> ** 标题
    """
    head_offset = len(start_head) - 1
    for i in range(6, 0, -1):
        pattern = r'^' + r'#' * i + r'\s+(.*?)$'
        replacement = r'*' * (i + head_offset) + r' \1'
        text = re.sub(pattern, replacement, text, flags=re.MULTILINE)
    return text


def convert_bold(text):
    """
    将Markdown加粗语法转换为Org模式加粗语法
    **文本** 或 __文本__ -> *文本*
    不匹配四个连续的*
    先匹配整个加粗部分，然后根据前后是否有空白字符来决定是否添加空格
    """
    def add_space_if_needed(match):
        # 获取匹配的整个文本
        full_text = match.group(0)
        # 获取加粗文本前后的字符
        before = match.group(1)
        after = match.group(3)

        # 如果前面不是空白字符，添加空格
        if not before.isspace():
            before = ' '
        # 如果后面不是空白字符，添加空格
        if not after.isspace():
            after = ' '
            
        result = f"{before}*{match.group(2)}*{after}"
        return result

    # 处理 **文本** 的情况
    text = re.sub(r'(\s*)(?<!\*)\*\*(?!\*)(.*?)(?<!\*)\*\*(?!\*)(\s*)', add_space_if_needed, text)
    # 处理 __文本__ 的情况
    text = re.sub(r'(\s*)__(.*?)__(\s*)', add_space_if_needed, text)
    return text

def convert_italic(text):
    """
    将Markdown斜体语法转换为Org模式斜体语法
    *文本* 或 _文本_ -> /文本/
    """
    # 需要注意不要匹配到已经转换的加粗文本
    text = re.sub(r'(?<!\*)\*(?!\*)(.*?)(?<!\*)\*(?!\*)', r'/\1/', text)
    text = re.sub(r'(?<!_)_(?!_)(.*?)(?<!_)_(?!_)', r'/\1/', text)
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
    将Markdown链接语法转换为Org模式链接语法
    [文本](链接) -> [[链接][文本]]
    """
    text = re.sub(r'\[(.*?)\]\((.*?)\)', r'[[\2][\1]]', text)
    return text


def convert_images(text):
    """
    将Markdown图片语法转换为Org模式图片语法
    ![替代文本](图片链接) -> [[图片链接][替代文本]]
    """
    text = re.sub(r'!\[(.*?)\]\((.*?)\)', r'[[\2][\1]]', text)
    return text


def convert_lists(text):
    """
    将Markdown列表语法转换为Org模式列表语法
    - 项目 -> - 项目
    1. 项目 -> 1. 项目
    """
    # 无序列表已经兼容，不需要修改
    # 有序列表也兼容，不需要修改
    return text


def convert_horizontal_rule(text):
    """
    将Markdown水平线转换为Org模式水平线
    --- 或 *** 或 ___ -> -----
    """
    text = re.sub(r'^(\*\*\*|---|___)$', r'-----', text, flags=re.MULTILINE)
    return text


def convert_blockquotes(text):
    """
    将Markdown引用转换为Org模式引用
    > 文本 -> #+BEGIN_QUOTE\n文本\n#+END_QUOTE
    """
    # 识别连续的引用块
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

    # 处理文件末尾的引用块
    if in_quote:
        result.append("#+BEGIN_QUOTE")
        result.extend(quote_lines)
        result.append("#+END_QUOTE")

    return '\n'.join(result)


def convert_tables(text):
    """
    尝试转换Markdown表格为Org模式表格
    不完美，但适用于简单表格
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
                # 移除分隔行 |---|---|
                table_lines = [
                    l for l in table_lines
                    if not re.match(r'^\|\s*[-:]+\s*\|', l)
                ]
                # 替换每行开头的 | 为空格
                table_lines = [l.replace('|', ' |', 1) for l in table_lines]
                result.extend(table_lines)
            result.append(line)

    # 处理文件末尾的表格
    if in_table:
        table_lines = [
            l for l in table_lines if not re.match(r'^\|\s*[-:]+\s*\|', l)
        ]
        table_lines = [l.replace('|', ' |', 1) for l in table_lines]
        result.extend(table_lines)

    return '\n'.join(result)


def convert_checkboxes(text):
    """
    将Markdown复选框转换为Org模式复选框
    - [ ] 任务 -> - [ ] 任务
    - [x] 任务 -> - [X] 任务
    """
    text = re.sub(r'- \[ \]', r'- [ ]', text)
    text = re.sub(r'- \[x\]', r'- [X]', text, flags=re.IGNORECASE)
    return text


def convert_markdown_to_org(markdown_text: str,
                            indent: str = "",
                            start_head="*"):
    """
    将Markdown文本转换为Org模式文本
    """
    text = markdown_text

    if indent:
        text_list = text.splitlines()
        text = "\n".join([line[len(indent):] for line in text_list])

    # 转换各种语法
    text = convert_headings(text, start_head)
    text = convert_blockquotes(text)
    text = convert_code(text)
    # NOTE: convert_italic should invoked before convert_bold
    text = convert_italic(text)
    text = convert_bold(text)
    text = convert_links(text)
    text = convert_images(text)
    text = convert_lists(text)
    text = convert_horizontal_rule(text)
    text = convert_tables(text)
    text = convert_checkboxes(text)

    return text


text = convert_markdown_to_org(text, indent=indent, start_head=start_head)
