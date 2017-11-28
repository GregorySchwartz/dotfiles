(defun word-to-sane (file)
  "Reads a docx file to an org buffer

Uses pandoc to accept all changes, display comments, and return an org buffer."
  (interactive)
  (let (buffer '(replace-regexp-in-string "\.docx" ".org" file))
    (message (concat "Converting to org: " file))
    (call-process "pandoc" file buffer t "-f" "docx" "-t" "org") 
    (message (concat "Text sent to " buffer))
  )
)
