(require 'ob-python)

(defun read-python-script (python-file-name)
  "Read the python_scripts.py file from the current buffer's directory."
  (let* ((current-dir (file-name-directory (buffer-file-name)))
         (python-file (expand-file-name python-file-name current-dir)))
    (with-temp-buffer
      (insert-file-contents python-file)
      (buffer-string))))


(defvar python-process-presets
  `(("md2org" . ,(read-python-script "md2org.py"))
    ("转大写" . "text = text.upper()")
    ("转小写" . "text = text.lower()")
    ("单词计数" . "text = f'单词数量: {len(text.split())}'")
    ("行数统计" . "text = f'行数: {len(text.splitlines())}'")))

(defun get-md2org-parameters ()
  (interactive)
  (let ((indent (read-string "Indent level: " ""))
        (start-head (read-string "Start heading level: " "*")))
    (list indent start-head)))

(defun process-region-with-python-preset (beg end)
  "Process region with a preset Python function."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties beg end))
         (preset-names (mapcar 'car python-process-presets))
         (selected (completing-read "选择处理方式: " preset-names nil t))
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
         (result (org-babel-eval "python" full-code)))
    ;; (message "%s" full-code)
    (with-current-buffer (get-buffer-create "*python-output*")
      (org-mode)
      (erase-buffer)
      (insert result)
      (pop-to-buffer (current-buffer)))))
