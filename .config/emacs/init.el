;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; We only run (package-refresh-contents) on first install each time
;; https://github.com/xxks-kkk/.emacs.d/commit/bf07269fe82a7e23896a330a9799cb1bb7543d53
(defun my-package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'my-package-install-refresh-contents))
(advice-add 'package-install :before 'my-package-install-refresh-contents)

;; Disable editing of init.el
(use-package emacs
  :config
  (load custom-file 'noerror)
  :custom
  (disabled-command-function nil)
  (custom-file "~/.emacs.d/emacs-custom.el")
  )

;; GUI
(use-package emacs
  :config
  ;; Dabbrev
  (require 'dabbrev)
  ;; Original size
  ;; Hide menu and scrollbar
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  ;; Show line numbers
  (global-display-line-numbers-mode 1)

  ;; Highlight current line
  (global-hl-line-mode 1)

  ;; Don't blink the cursor
  (blink-cursor-mode -1)

  ;; Enable smart parentheses
  (electric-pair-mode 1)

  ;; Enable which-key
  (which-key-mode 1)

  ;; Ask for y/n instead of yes/no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Font
  (add-to-list 'default-frame-alist
               '(font . "Iosevka Term Slab Compressed-11:weight=light:width=normal"))
  :custom
  (frame-resize-pixelwise t "Original size in pixels.")
  )

;; No message in scratch
(use-package emacs
  :custom
  (initial-scratch-message nil)
  )

;; Recent files
(use-package recentf
  :ensure t
  :init
  (setopt recentf-save-file (expand-file-name "~/.emacs.d/.cache/recentf"))
  (setopt recentf-max-saved-items 10000)
  (setopt recentf-max-menu-items 5000)

  (recentf-mode 1)
  :custom
  (remote-file-name-access-timeout 5 "Give up after 5 seconds checking for remote")

  ;; Cleanup the recent files list and synchronize it every 60 seconds.
  (recentf-auto-cleanup 60)

  ;; Save every five minutes
  (run-at-time "5 min" 300 'recentf-save-list)
  )

;; Recent files syncing so no overwriting
(use-package sync-recentf
  :ensure t
  )

;; Backups
(use-package emacs
  :config
  (defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.backup/per-session")))
    (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

  (add-hook 'before-save-hook  'force-backup-of-buffer)
  :custom
  ;; Backups
  (make-backup-files t "Make backup files")
  (version-control t "Use version numbers for backups.")
  (kept-new-versions 10 "Number of newest versions to keep.")
  (kept-old-versions 0 "Number of oldest versions to keep.")
  (delete-old-versions t "Don't ask to delete excess backup versions.")
  (backup-by-copying t "Copy all files, don't rename them.")
  (vc-make-backup-files t "Make backup files for vc")
  ;; Default and per-save backups go here:
  (backup-directory-alist '(("" . "~/.backup/per-save")) "Where to save backups"))

;; Repeats
(use-package emacs
  :config
  ;; Enable repeat mode for repeated execution
  (repeat-mode 1)
  )

;; Warnings
;; (use-package emacs
;;   :custom
;;   ;; Hide warnings when ready
;;   ; (warning-minimum-level :emergency "Warning level")
;;   )

;; Tabs
(use-package emacs
  :config
  (define-key global-map (kbd "TAB") 'tab-to-tab-stop)
  :custom
  ;; Tab and indent width. Use spaces instead of tabs.
  (indent-tabs-mode nil "No tabs.")
  (tab-width 2 "Tab width 2.")
  (standard-indent 2 "Indents at 2.")
  (evil-shift-width 2 "Tab widths at 2 in evil.")
  (tab-stop-list (number-sequence 2 120 2) "Tab widths at 2.")
  )

;; Authentication
(use-package emacs
  :custom
  ;; Change auth order.
  (auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc") "Order for auth files."))

(use-package general
  )

;; Load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  ;; j and k respect visual vs physical lines
  (setopt evil-respect-visual-line-mode t)
  (global-visual-line-mode 1)
  :config ;; tweak evil after loading it
  (evil-mode 1)

  ;;;; Evil keys
  (evil-set-leader 'normal (kbd "<SPC>"))  ;; sets the leader
  ;; Commands
  (evil-define-key '(normal visual) 'global (kbd "<leader>SPC") 'execute-extended-command)
  ;; Search
  (evil-define-key 'normal 'global (kbd "<leader>sc") 'evil-ex-nohighlight)
  ;; View
  (evil-define-key 'normal 'global (kbd "<leader>tw") 'whitespace-mode)
  ;; Files
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'recentf)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'evil-delete-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bs") 'scratch-buffer)
  ;; Help
  (evil-define-key 'normal 'global (kbd "<leader>hdk") 'describe-key)
  (evil-define-key 'normal 'global (kbd "<leader>hdf") 'describe-function)
  (evil-define-key 'normal 'global (kbd "<leader>hdv") 'describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader>hdm") 'describe-mode)
  ;; Applications
  (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>aem") 'mu4e)
  (evil-define-key 'normal 'global (kbd "<leader>are") 'elfeed)
  ;; Org roam
  (evil-define-key 'normal 'global (kbd "<leader>aorl") 'org-roam-buffer-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>aorf") 'org-roam-node-find)
  (evil-define-key 'normal 'global (kbd "<leader>aorg") 'org-roam-graph)
  (evil-define-key 'normal 'global (kbd "<leader>aori") 'org-roam-node-insert)
  (evil-define-key 'normal 'global (kbd "<leader>aorc") 'org-roam-capture)
  (evil-define-key 'normal 'global (kbd "<leader>aort") 'org-roam-tag-add)
  (evil-define-key 'normal 'global (kbd "<leader>aorj") 'org-roam-dailies-capture-today)
  ;; Snippets
  (evil-define-key 'normal 'global (kbd "<leader>is") 'yas-insert-snippet)
  ;; Restart
  (evil-define-key 'normal 'global (kbd "<leader>qr") 'restart-emacs)
  ;; Check parentheses
  (evil-define-key 'normal 'global (kbd "<leader>j(") 'check-parens)
  :custom
  ;; Required before evil and evil-collection
  (evil-want-keybinding nil "Required before evil.")
  (evil-want-integration t "Required before evil.")
  (evil-search-module 'evil-search "Use evil search.")
  (evil-ex-complete-emacs-commands nil "Ex evil.")
  (evil-vsplit-window-right t "Where to split window right.")
  (evil-split-window-below t "Where to split window below.")
  (evil-shift-round nil "How shift works in evil.")
  (evil-want-C-u-scroll t "Scrolling in evil.")
  (evil-undo-system 'undo-fu "Evil's undo system.")

  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  :custom
  ;; Required before evil and evil-collection
  (evil-want-keybinding nil "Required before evil-collection.")
  (evil-want-integration t "Required before evil-collection."))

(use-package evil-escape
  :ensure t
  :after evil
  :config
  (evil-escape-mode 1)
  :custom
  (evil-escape-key-sequence "kj" "Evil escape sequence.")
  (evil-escape-delay 0.2 "More delay between key presses.")
  )

;; For commenting lines
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1)
  )

;; Gruvbox theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

;; Undo
(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode 1)
  )

;; More info in completions
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :ensure t
  :defer t
  :bind (:map minibuffer-local-map
         ("C-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode 1))

;; Character movement
(use-package avy
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal 'global (kbd "<leader>jj") 'avy-goto-char-2)
  (evil-define-key 'normal 'global (kbd "<leader>jw") 'avy-goto-word-0)
  (evil-define-key 'normal 'global (kbd "<leader>jl") 'avy-goto-line)
  )

;; Window movement
(use-package ace-window
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal 'global
    (kbd "<leader>ww") 'ace-window
    (kbd "<leader>w?") 'aw-show-dispatch-help
    (kbd "<leader>wm") 'aw-move-window
    (kbd "<leader>wM") 'aw-swap-window
    (kbd "<leader>wo") 'delete-other-windows
    (kbd "<leader>w=") 'balance-windows
    (kbd "<leader>wv") 'evil-window-vsplit
    (kbd "<leader>wn") 'evil-window-new
    (kbd "<leader>wh") 'evil-window-left
    (kbd "<leader>wj") 'evil-window-down
    (kbd "<leader>wk") 'evil-window-up
    (kbd "<leader>wl") 'evil-window-right
    (kbd "<leader>wpm") (lambda () (interactive)(switch-to-buffer "*Messages*")))
  
  ;; Windows
  (winner-mode 1)
  :custom
  ;; Always dispatch
  (aw-dispatch-always t "Always dispatch.")
  ;; Use homerow instead of numbers
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use homerow instead of numbers.")
  ;; Always show homerow IDs
  (ace-window-display-mode 1 "Always show window IDs.")
  )

;; Magit
(use-package magit
  :ensure t
  :defer t
  :config
  )

;; Latex
(use-package auctex
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal 'LaTeX-mode-map (kbd "<leader>mb")
    (lambda ()
      "Save the buffer and run `TeX-command-run-all`."
      (interactive)
      (save-buffer)
      (TeX-command-run-all nil)))
  :custom
  ;; If you want to make AUCTeX aware of style files and multifile documents
  ;; right away, insert the following in your init file
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master 'shared)
  (TeX-engine 'luatex)
  (TeX-command-extra-options "-shell-escape")
  (TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (TeX-view-program-selection '((output-pdf "Evince")))
  (TeX-source-correlate-start-server t)  ; Automatically start server for
                                        ; syncing page number
  :hook
  (LaTeX-mode . turn-on-reftex)   ; with AUCTeX LaTeX mode
  (latex-mode . turn-on-reftex)  ; With emacs
  ;; Jump to location in page
  (LaTeX-mode . TeX-source-correlate-mode)  ; Jump to place in pdf file
  )

;; Pdfs
(use-package emacs
  :config
  ;; Load when needed
  (pdf-loader-install)

  ;; pdf-tools no line numbers
  ;; https://www.reddit.com/r/emacs/comments/sy1n1f/globallinummode_1_causing_issues_with_pdf_viewing/
  (defun my-turn-off-line-numbers ()
    "Disable line numbering in the current buffer."
    (display-line-numbers-mode -1))
  (add-hook 'pdf-view-mode-hook 'my-turn-off-line-numbers)

  ;; pdf-tools keybindings.
  (defun add-pdf-view-keys ()
    (define-key pdf-view-mode-map [mouse-2] 'pdf-annot-add-highlight-markup-annotation)
    )
  (add-hook 'pdf-view-mode-hook 'add-pdf-view-keys)
  )

;; Mail
(use-package org-msg
  :ensure t
  :defer t
  )

(use-package mu4e
  :ensure t
  :defer t
  :config
  (load-file "~/Nextcloud/emacs/mail.el")
  )

;; Spell checking
(use-package emacs
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :custom
  (ispell-dictionary "en_CA")
  )

;; Restarting emacs
(use-package restart-emacs
  :ensure t
  :custom
  ;; Remember frames
  (restart-emacs-restore-frames t)
  )

;; Smooth scrolling
(use-package emacs
  :custom
  (scroll-conservatively 101)
  )

;; For frequency of completions
(use-package subr-x)
(use-package cl-lib)
(use-package savehist
  :ensure t
  :config
  (savehist-mode 1))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :ensure t
  :config
  (global-corfu-mode 1)
  ;; To prevent pressing enter twice
  (keymap-set corfu-map "RET" #'corfu-send)
  ;; Remember previous completions
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  ;; Show popup information
  (corfu-popupinfo-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 1)
  (corfu-auto-prefix 1)
  )

;; Add extensions
(use-package cape
  :ensure t
 :defer t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-tex) ;; Enters quail which ;; messes up everything
  (add-hook 'completion-at-point-functions #'cape-dict)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  :custom
  (text-mode-ispell-word-completion nil)
)

;; Orderless sorting
(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless) "Completion style."))

;; Tabs
(use-package emacs
  :config
  (evil-define-key 'normal 'global (kbd "<leader>ll") 'tab-bar-switch-to-tab)
  (evil-define-key 'normal 'global (kbd "<leader>ld") 'tab-bar-close-tab)
  (evil-define-key 'normal 'global (kbd "<leader>ln") 'tab-bar-switch-to-next-tab)
  (evil-define-key 'normal 'global (kbd "<leader>lp") 'tab-bar-switch-to-prev-tab)
  )

;; Line breaking
(use-package emacs
  :config
  (auto-fill-mode 1)
  (defun turn-on-auto-fill-mode ()
    (auto-fill-mode 1))

  ;; Toggle display
  (evil-define-key 'normal 'global (kbd "<leader>tf") 'display-fill-column-indicator-mode)
  :custom
  (fill-column 80)
  (global-display-fill-column-indicator-mode 1)
  :hook
  (text-mode . turn-on-auto-fill-mode)
  (prog-mode . turn-on-auto-fill-mode)
  )

;; Recent file sorting
(use-package emacs
  :config
  ;; Sort by most recently used
  (fido-vertical-mode 1)
  (global-completion-preview-mode 1)
  ;; When to trigger completion-preview-mode
  (add-to-list 'completion-preview-commands 'org-self-insert-command)
  (add-to-list 'completion-preview-commands 'org-delete-backward-char)
  (add-to-list 'completion-preview-commands 'evil-delete-backward-char-and-join)
  ;; Sorting
  (defun my-icomplete-styles ()
    (setq-local completion-styles '(orderless)))
  (add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)
  ; Completions keymaps
  (keymap-set icomplete-minibuffer-map "C-j" #'icomplete-forward-completions)
  (keymap-set icomplete-minibuffer-map "C-k" #'icomplete-backward-completions)
  (keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)  ;; Tab complete actually moving forward in fido
  ;; Sort order by frequency
  ;; https://emacs.stackexchange.com/questions/74512/make-fido-mode-remember-which-command-i-chose
  (defvar my-fido-command-completions-alist nil
    "Mapping commands to an alist of successful COMPLETIONS.
  COMPLETIONS maps strings to the number of times these
  strings were chosen.")

  (defvar my-fido-this-command nil
    "Preserves `this-command' before the minibuffer is entered.
  The execution of commands in the minibuffer changes `this-command'.")

  (defun my-fido-mode-setup-function ()
    "Let `fido-mode' use `my-fido-this-command'."
    (if fido-mode
        (add-hook 'minibuffer-setup-hook #'my-fido-minibuffer-setup-function)
      (remove-hook 'minibuffer-setup-hook #'my-fido-minibuffer-setup-function)))

  (add-hook 'fido-mode-hook #'my-fido-mode-setup-function)

  (defun my-fido-minibuffer-setup-function ()
    "Preserve `this-command' for `my-fido-command-completions-alist'.
  Save `this-command' in `my-fido-this-command' before any command is
  executed in the minibuffer.
  That is the command for which we run completion in the minibuffer."
    (setq my-fido-this-command this-command))

  (defun my-fido-save-choice-on-ret ()
    "Increase the sorting weight for the chosen completion.
  The weights are stored in `my-fido-command-completions-alist'."
    (when-let (((commandp my-fido-this-command))
           (chosen (car completion-all-sorted-completions))
           ((stringp chosen)))
      (setq chosen (substring-no-properties chosen)) ;; strip non-needed char properties
      (cl-incf (alist-get chosen (alist-get my-fido-this-command my-fido-command-completions-alist) 0 nil 'string-equal))))

  (advice-add 'icomplete-fido-ret :before #'my-fido-save-choice-on-ret)

  (defun my-fido-sorted-completions (completions)
    "Resort COMPLETIONS with weights from `my-fido-command-completions-alist'."
    (when (and fido-mode (listp completions) (stringp (car completions)))
      (let ((weights (alist-get my-fido-this-command my-fido-command-completions-alist))
        (last-cdr (cdr (last completions)))
        (completions (cl-copy-list completions)))
        (setcdr (last completions) nil)
        (setq completions
          (sort
           completions
           (lambda (first second)
             (let ((first-weight (alist-get first weights 0 nil #'string-equal))
               (second-weight (alist-get second weights 0 nil #'string-equal)))
           (> first-weight second-weight)))
           ))
        (setcdr (last completions) last-cdr)))
    completions)

  (advice-add 'icomplete--sorted-completions :filter-return #'my-fido-sorted-completions)
  (customize-set-variable 'savehist-additional-variables (append savehist-additional-variables (list 'my-fido-command-completions-alist)))
  )

;; Ediff options
(use-package emacs
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  )

;; Tiny for expansions
(use-package tiny
  :ensure t
  :defer t
  )

;; Flymake
(use-package flymake
  :ensure t
  :defer t
  :config
  (flymake-mode 1))

;; Eshell
(use-package eshell
  :ensure nil
  :config
  ;; New eshell each time.
  (defun eshell-new ()
    (interactive)
    (eshell 'N) ; Forces new session.
    )
  (define-key global-map (kbd "C-a") 'eshell-new)
                                        ; Remove eshell window on exit.
  (defun eshell-remove-on-exit ()
    (when (not (one-window-p))
      (delete-window)))
  ;; Apply function to all eshell buffers.
  (defun apply-function-to-all-mode (f mode)
    "Apply function in all buffers in a certain mode."
    (interactive)
    (dolist ($buf (buffer-list))
      (with-current-buffer $buf
        (when (equal major-mode mode)
          (funcall f)
          ))))
  ;; Read eshell history in all eshell buffers.
  (defun reload-all-eshell-history ()
    "Read eshell history in all eshell buffers."
    (interactive)
    (apply-function-to-all-mode #'eshell-read-history 'eshell-mode))
  ;; Add to history every time after command is sent.
  (add-hook 'eshell-post-command-hook #'eshell-write-history t)
  (add-hook 'eshell-post-command-hook #'reload-all-eshell-history t)
  (add-hook 'eshell-post-command-hook #'reload-all-eshell-history t)

  (advice-add 'eshell-life-is-too-much :after 'eshell-remove-on-exit)
                                        ; Some aliases.
  (defalias 'eshell/open 'find-file)
  (defalias 'eshell/f 'find-file)
  (defalias 'eshell/F 'find-file-other-window)
  (defalias 'eshell/x 'eshell/exit)
  (defalias 'eshell/R 'R)
  :custom
  (multi-term-program "fish")
  (shell-file-name "bash")
  (explicit-shell-file-name "bash")
  )

;; Icons
;; Make sure to run M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  :defer t
  :if (display-graphic-p)
  )

;; Icons in dired
(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook dired-mode
  )

;; Writeroom mode for centered text
(use-package writeroom-mode
  :ensure t
  :defer t
  :config
  (global-writeroom-mode 1)
  :custom
  (writeroom-width 100)
  (writeroom-mode-line t)
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-maximize-window nil)
  (writeroom-major-modes '(text-mode prog-mode))
  )

(use-package plantuml-mode
  :ensure t
  :defer t
  :custom
  (plantuml-executable-path "plantuml")
  (plantuml-default-exec-mode 'executable)
  (plantuml-exec-mode 'executable)
  )

;; ;; Recover leader in built-in modes
;; (use-package emacs
;;   :config
;;   (defun my/dired-mode-bindings ()
;;     (keymap-set dired-mode-map "<SPC>" nil))

;;   (add-hook 'dired-mode-hook #'my/dired-mode-bindings)
;;   )

;; Recover leader in ediff
(use-package emacs
  :config
  (defun my/ediff-mode-bindings ()
    (keymap-set ediff-mode-map "<SPC>" nil))

  (add-hook 'ediff-startup-hook #'my/ediff-mode-bindings)
  )

;; Org noter
(use-package org-noter
  :ensure t
  :defer t
  :config
  ;; Org-noter keybindings.
  (defun add-org-noter-keys ()
    (define-key org-noter-doc-mode-map [C-mouse-2] 'org-noter-insert-precise-note)
    )
  (add-hook 'org-noter-doc-mode-hook 'add-org-noter-keys)
  )

;; Modes

;; Org mode
(use-package org
  :ensure t
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (ditaa . t)
     (dot . t)
     (gnuplot . t)
     (haskell . t)
     (latex . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     )
   )
  ;; Library of babel location.
  (org-babel-lob-ingest "~/git_repos/dotfiles/emacs/.library_of_babel.org")
  ;; Where the agenda files are located (all files in Nextcloud).
  (with-eval-after-load 'org-agenda
    (setq-default
     org-agenda-files
     (append (directory-files-recursively "~/Nextcloud/org/calendars/" "\\.org$")
             (directory-files-recursively "~/Nextcloud/org/general/" "\\.org$")
             (directory-files-recursively "~/Nextcloud/org/org-roam/" "\\.org$")
             (directory-files-recursively "~/OneDrive/work/" "\\.org$")
             (directory-files-recursively "~/Nextcloud/life/" "\\.org$")
             )
     )
                                      ; Remove unwanted folders.
    (setq-default org-agenda-files
                  (cl-remove-if '(lambda (x)
                                   (string-match
                                    (concat "^" (regexp-quote (expand-file-name
                                                               "~/OneDrive/work/website/")))
                                    x))
                                org-agenda-files)
                  )
    )

  ;; Export binding
  (evil-define-key 'normal org-mode-map (kbd "<leader>mee") 'org-export-dispatch)
  ;; Toggle link display
  (evil-define-key 'normal org-mode-map (kbd "<leader>tl") 'org-toggle-link-display)
  ;; Insert tag
  (evil-define-key 'normal org-mode-map (kbd "<leader>it") 'org-set-tags-command)
  
  ;; Allow more lines to be emphasized with org (If you want multiple lines for
  ;; inline underline, bold, etc.).
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
  (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))
  (org-element--set-regexps)

  ;; Disable C-a in org (need it for eshell).
  (define-key org-mode-map (kbd "C-a") nil)

  ;; Keybinding for moving lists or subtrees
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)

  ;; Keybinding for enter
  (evil-define-key 'normal org-mode-map (kbd "<RET>") 'org-return)

  ;; Start zotxt link.
  ;(add-hook 'org-mode-hook 'org-zotxt-mode)

  ;; Allow PDF files to be shown in org.
  (add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
  (add-to-list 'image-file-name-extensions "pdf")
  (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))
  (add-to-list 'org-babel-default-inline-header-args '(:eval . "never-export"))
  (add-to-list 'org-babel-default-lob-header-args '(:eval . "never-export"))

  ;; Delete a cell and move column.
  (defun org-table-collapse-cell ()
  (interactive)
  (save-excursion ;; Save point
    (org-table-blank-field) ;; Blank the cell
    (while (progn ;; Swap blank cell with a cell under it until the blank is at the bottom.
         (org-table--move-cell 'down)
         (org-table-align)
         (org-table-check-inside-data-field))))
    (org-table-next-field))

  ;; wrap reftex-citation with local variables for markdown format
  (defun markdown-reftex-citation ()
    (interactive)
    (let ((reftex-cite-format markdown-cite-format)
          (reftex-cite-key-separator "; @"))
      (reftex-citation)))

  ;; bind modified reftex-citation to C-c[, without enabling reftex-mode
  ;; https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
  (add-hook
   'markdown-mode-hook
   (lambda ()
     (define-key markdown-mode-map "\C-c[" 'markdown-reftex-citation)))

  ;; Org table keybindings.
  (define-key org-mode-map (kbd "M-C-k") nil)
  (define-key org-mode-map (kbd "M-C-j") nil)
  (define-key org-mode-map (kbd "M-C-h") nil)
  (define-key org-mode-map (kbd "M-C-l") nil)
  (define-key org-mode-map (kbd "M-C-k") #'org-table-move-cell-up)
  (define-key org-mode-map (kbd "M-C-j") #'org-table-move-cell-down)
  (define-key org-mode-map (kbd "M-C-h") #'org-table-move-cell-left)
  (define-key org-mode-map (kbd "M-C-l") #'org-table-move-cell-right)

  ;; Org-noter keybindings.
  (defun add-org-noter-keys ()
    (define-key org-noter-doc-mode-map [C-mouse-2] 'org-noter-insert-precise-note)
    )
  (add-hook 'org-noter-doc-mode-hook 'add-org-noter-keys)

  ;; For beamer.
                                      ; Custom environments.
  (add-hook 'org-beamer-mode-hook
            (lambda()
              (add-to-list 'org-beamer-environments-extra
                           '("tcolorboxenv" "T" "\\begin{tcolorbox}[%O,title=%h]" "\\end{tcolorbox}"))
              (add-to-list 'org-beamer-environments-extra
                           '("tcolorboxnotitleenv" "X" "\\begin{tcolorbox}[%O]" "\\end{tcolorbox}"))
              (add-to-list 'org-beamer-environments-extra
                           '("adjustwidth_wide" "w" "\\begin{adjustwidth}{-3em}{-3em}" "\\end{adjustwidth}"))
              (add-to-list 'org-beamer-environments-extra
                           '("footnote" "O" "\\begin{textblock*}{\\textwidth}(5mm, 85mm)\\tiny" "\\end{textblock*}"))))
                                      ; Beamer grid backend.
  (load-file "/home/gw/git_repos/dotfiles/emacs/ox-beamer-grid.el")
  :custom
  (org-export-backends '( ascii
                          beamer
                          html
                          icalendar
                          latex
                          man
                          md
                          twbs
                          re-reveal
                          odt
                          org
                          texinfo
                          ) "Languages.")

  ;; Root org directory.
  (org-directory "~/Nextcloud/org" "Root org directory.")
  (org-archive-location (concat org-directory "/archive.org::") "Archive directory.")

  (org-default-notes-file (concat org-directory "/notes.org") "Where the notes are located.")
  ;; Ignore #+STARTUP when org-agenda searches through files.
  (org-agenda-inhibit-startup t)

  ;; Ignore archives in agenda.
  (org-agenda-archives-mode nil)

  ;; Org refile options, from https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
  (org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (org-refile-use-outline-path t)                  ; Show full paths for refiling

  ;; Command for python.
  (org-babel-python-command "python3")

  ;; Options for pandoc
  (org-pandoc-options-for-docx '((reference-doc . "~/Nextcloud/pandoc/reference.docx")))

  ;; Markdown citations (https://gist.github.com/kleinschmidt/5ab0d3c423a7ee013a2c01b3919b009a)
  ;; reftex in markdown mode
  (reftex-default-bibliography '("~/Nextcloud/papers/global.bib"))

  ;; define markdown citation formats
  (markdown-cite-format
        '(
          (?\C-m . "[@%l]")
          (?p . "[@%l]")
          (?t . "@%l")
          )
        )
  ;; Bibliography in org.
  (org-ref-default-bibliography '("~/Nextcloud/papers/global.bib"))
  (org-ref-pdf-directory "~/Nextcloud/papers/")
  (org-ref-bibliography-notes "~/Nextcloud/papers/notes.org") ; For \autocite instead of cite:
  (org-ref-default-citation-link "autocite")

  ;; Start in org-indent-mode.
  ;; (add-hook 'org-mode-hook 'org-indent-mode)
  ;; Don't show images by default.
  (org-startup-with-inline-images nil)

  (imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))

  ;; Log times with TODOs.
  (org-log-done 'time)

  ;; Make image width 1/3 the size of the display.
  (org-image-actual-width nil)

  ;; No table of contents by default.
  (org-export-with-toc nil)

  ;; No section numbering by default.
  (org-export-with-section-numbers nil)

  ;; Smart quotation exports.
  (org-export-with-smart-quotes t)

  ;; Asynchronous exporting. Not working with colorboxes or bibliography right now.
  ;; (org-export-async-init-file nil)
  ;; (org-export-in-background t)

  ;; Do not change indentation in src blocks.
  (org-src-preserve-indentation t)

  ;; Don't indent based on header.
  (org-adapt-indentation nil)

  ;;; For specific files types.

  ;; Latex command.
  ;; Works with xelatex too with #+LATEX_COMPILER: xelatex
  (org-latex-compiler "lualatex")
  (org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))

  ;; Latex allow utf8.
  (org-latex-inputenc-alist '(("utf8")))
  (org-list-allow-alphabetical t)

  ;; Set the path for ditaa.
  (org-ditaa-jar-path "/usr/bin/ditaa")

  ;; Plantuml.
  (org-plantuml-exec-mode 'plantuml)

  ;; Org reveal. ; Not working due to Org 9.2
  (org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@4.1.0")
  (org-re-reveal-revealjs-version "4")
  (org-re-reveal-transition "fade")
  (org-re-reveal-transition-speed "fast")

  ;; Org letters.
  ;; No fold marks on the side.
  (org-koma-letter-use-foldmarks nil)

  ;; RETURN will follow links in org-mode files
  (org-return-follows-link t)
  )

;; Org roam
(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-directory (file-truename "~/Nextcloud/org/org-roam"))
  (org-roam-db-location (file-truename "~/Nextcloud/org/org-roam/org-roam.db"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (org-roam-db-autosync-mode 1)
  )

;; Org download
(use-package org-download
  :ensure t
  :defer t
  :after org
  :custom
  ;; Automatic image download directory.
  (org-download-image-dir "~/OneDrive/work/img/downloads" "Automatic image download directory.")
  ;; Image yank command
  (org-download-screenshot-method "import %s" "Image yang command.")

  (org-download-display-inline-images nil "Don't show inline images automatically in buffer.")

  (org-download-abbreviate-filename-function 'expand-file-name "Make sure download paths are absolute.")

  (org-download-image-attr-list '("#+attr_latex: :options [keepaspectratio, height=0.8\\textheight, width=0.8\\linewidth]") "Insert width automatically.")
  :hook 
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable)
  )

;; Org gcal
(use-package org-gcal
  :ensure t
  :defer t
  :config
  (load-file "~/Nextcloud/emacs/calendar_gcal.el.gpg")
  )

;; For nix files
(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

;; For yaml files
(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; For markdown files
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  ;; For key bindings
  ;; https://emacs.stackexchange.com/questions/12552/how-bind-keys-to-a-specific-snippet-in-yasnippet-folder
  (defun expand-yasnippet-hlfix ()
    "Expand the yasnippet named `hlfix'."
    (interactive)
    (yas-expand-snippet (yas-lookup-snippet "hlfix")))

  ;; Key bindings
  (evil-define-key 'normal 'LaTeX-mode-map (kbd "<leader>ih") 'expand-yasnippet-hlfix)

  ;; Needed to actually load the snippets
  (yas-recompile-all)
  (yas-reload-all)
  (yas-global-mode 1)
  :custom
  (yas-snippet-dirs '("~/git_repos/dotfiles/emacs/snippets/" "~/Nextcloud/emacs/snippets/"))
  (auto-completion-private-snippets-directory yas-snippet-dirs)
  ;; Don't auto indent.
  (yas-indent-line "fixed")
  )

;; Elfeed-Org
(use-package elfeed-org
  :ensure t
  :custom
  (rmh-elfeed-org-files (list "~/Nextcloud/emacs/feeds/pubmed.org"))
  )

;; Elfeed
(use-package elfeed
  :ensure t
  :init
  ;; Must load before elfeed
  (elfeed-org)
  :config
  (load-file "~/Nextcloud/emacs/feeds/elfeed-funcs.el")

  ;; Run 5 seconds after startup and keep doing it every hour
  (add-hook 'emacs-startup-hook (lambda () (run-at-time 5 3600 'elfeed-update))) 
  :custom
  (elfeed-search-title-max-width 160)
  )

;; Elfeed-Score
(use-package elfeed-score
  :ensure t
  :after elfeed
  :config
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  :custom
  (elfeed-score-serde-score-file "~/Nextcloud/emacs/feeds/elfeed-scoring.el")
  )
