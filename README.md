# markdown-to-org

This is an Emacs package for converting Markdown text to Org-mode format.

## Installation

### Using MELPA

If you use MELPA, you can install it with the following command:

```elisp
M-x package-install RET markdown-to-org RET
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/pistonly/markdown-to-org.git
   ```

2. Add the files to your `load-path`:
   - Open your Emacs configuration file (usually `~/.emacs.d/init.el` or `~/.emacs`)
   - Add the following line, replacing the path with your actual repository path:
   ```elisp
   (add-to-list 'load-path "path/to/markdown-to-org")
   ```

3. Load the package in your Emacs configuration:
   ```elisp
   (require 'markdown-to-org)
   ```

4. Ensure Python dependencies:
   - Make sure Python 3.6 or higher is installed on your system
   - The package uses `markdown-to-org.py` for text processing

5. Verify the installation:
   - Restart Emacs
   - Run `M-x markdown-to-org-convert-region` or `M-x markdown-to-org-convert-buffer` to verify the commands are available

Note: Make sure you have:
- Emacs 24.1 or higher
- Org-mode 9.0 or higher
- ob-python 0.1 or higher

If you encounter any issues:
- Check if `load-path` is correctly set
- Verify all required files are in the correct location
- Check the `*Messages*` buffer (`C-h e`) for any error messages

## Usage

1. Open a buffer containing Markdown text in Emacs
2. Select the text region you want to convert
3. Run `M-x process-region-with-python-preset`
4. Select "md2org" from the menu
5. Follow the prompts to enter the indent level and starting heading level
6. The converted text will be displayed in a new buffer

## Features

- Convert Markdown to Org-mode format
- Support custom indent levels
- Support custom starting heading levels
- Provide additional text processing features (such as case conversion, word count, etc.)

## Dependencies

- Emacs 24.1 or higher
- Org-mode 9.0 or higher
- ob-python 0.1 or higher
- Python 3.6 or higher (required for text processing)

## License

GNU General Public License v3.0 