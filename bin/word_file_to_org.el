(defun word-file-to-org (file)
  "Reads a docx file to an org buffer

Uses pandoc to accept all changes, display comments, and return an org buffer."
  (interactive)
  (let* ((buffer (replace-regexp-in-string ".docx" ".org" file)))
    (message (concat "Switching to buffer: " buffer))
    (switch-to-buffer buffer)
    (message (concat "Clearing buffer: " file))
    (erase-buffer)
    (message (concat "Converting to org: " file))
    (call-process "pandoc" file buffer t "-f" "docx" "-t" "org" "-s"
    "--track-changes" "accept")
    (message (concat "Text sent to " buffer))
  )
)

(defun org-to-word-file ()
  "Reads a org buffer and writes to a docx file

Uses pandoc to convert an org buffer to a docx file."
  (interactive)
  (let* ((buffer (buffer-name))
         (file (replace-regexp-in-string ".org" ".docx" buffer))
        )
    (message (concat "Converting to docx: " buffer))
    (call-process-region (point-min) (point-max) "pandoc" nil nil nil "-f" "org"
    "-t" "docx" "-o" file "-s")
    (message (concat "Text saved as: " file))
  )
)
