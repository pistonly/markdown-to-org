import pypandoc


def convert_markdown_to_org_pandoc(markdown_text: str,
                                   indent: str = "",
                                   start_head: str = "*") -> str:
    text = markdown_text

    # process markdown source-block
    if "#+begin_src markdown" in text.split("\n")[0].lower() and "#+end_src" in text[-50:].lower():
        text = text.strip()
        text = "\n".join(text.split("\n")[1:-1])

    if indent:
        text_list = text.splitlines()
        text = "\n".join([line[len(indent):] for line in text_list])

    output = pypandoc.convert_text(text, "org", format="md")
    return output
