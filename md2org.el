(require 'ob-python)

(defun check-python-version ()
  "Check if Python 3 is available."
  (let ((python-version (shell-command-to-string "python3 --version 2>&1")))
    (if (string-match "Python 3" python-version)
        t
      (error "Python 3 is required but not found. Please install Python 3.6 or higher."))))

(defun read-python-script (python-file-name)
  "Read the python_scripts.py file from the current buffer's directory."
  (let* ((current-dir (file-name-directory (buffer-file-name)))
         (python-file (expand-file-name python-file-name current-dir)))
    (with-temp-buffer
      (insert-file-contents python-file)
      (buffer-string))))


(defvar python-process-presets
  `(("md2org" . ,(read-python-script "md2org.py"))
    ("To Uppercase" . "text = text.upper()")
    ("To Lowercase" . "text = text.lower()")
    ("Word Count" . "text = f'Word count: {len(text.split())}'")
    ("Line Count" . "text = f'Line count: {len(text.splitlines())}'")))

(defun get-md2org-parameters ()
  (interactive)
  (let ((indent (read-string "Indent level: " ""))
        (start-head (read-string "Start heading level: " "*")))
    (list indent start-head)))

(defun process-region-with-python-preset (beg end)
  "Process region with a preset Python function."
  (interactive "r")
  (check-python-version)  ; Check Python version before processing
  (let* ((region-text (buffer-substring-no-properties beg end))
         (preset-names (mapcar 'car python-process-presets))
         (selected (completing-read "Select processing method: " preset-names nil t))
         (python-code (cdr (assoc selected python-process-presets)))
         (full-code (if (string-equal selected "md2org")
                        (let ((indent-start-head (get-md2org-parameters)))
                          (format "text, indent, start_head = '''%s''', '''%s''', '''%s'''\n%s\n\nprint(text)"
                                  (replace-regexp-in-string "'" "\\\\'" region-text)
                                  (nth 0 indent-start-head)
                                  (nth 1 indent-start-head) python-code))
                      (format "text = '''%s'''\n%s\nprint(text)"
                              (replace-regexp-in-string "'" "\\\\'" region-text)
                              python-code)))
         (result (org-babel-eval "python3" full-code)))  
    ;; (message "%s" full-code)
    (with-current-buffer (get-buffer-create "*python-output*")
      (org-mode)
      (erase-buffer)
      (insert result)
      (pop-to-buffer (current-buffer)))))
