;;; ox-beamer-grid.el --- Beamer Back-End for Org Export Engine with Grid -*- lexical-binding: t; -*-

;; Gregory W. Schwartz

(require 'ox-latex)
(require 'ox-beamer)

(defcustom org-beamer-grid-default-options "texcoord,grid,gridunit=mm,gridcolor=red!30,subgridcolor=blue!30"
  "Default options string to use for grid."
  :group 'org-export-beamer
  :type '(string :tag "[options]"))

(defun org-beamer-export-to-pdf-grid (&optional async subtreep visible-only body-only ext-plist)
  "Export beamer to pdf with a grid."
  (interactive)
  (let ((org-latex-packages-alist (cons (list org-beamer-grid-default-options "eso-pic") org-latex-packages-alist)))
    (org-beamer-export-to-pdf async subtreep visible-only body-only ext-plist)
    )
  )

(org-export-define-derived-backend 'beamer-grid 'beamer
    :menu-entry
    '(?l 1
         ((?G "As PDF file with grid (Beamer)" org-beamer-export-to-pdf-grid))
     ))
