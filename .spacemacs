;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     python
     nlinum
     yaml
     shell-scripts
     csv
     html
     javascript
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      spacemacs-default-company-backends '( company-capf
                                                            company-dabbrev-code
                                                            company-gtags
                                                            company-etags
                                                            company-keywords
                                                            company-files
                                                            company-dabbrev
                                                          )
     )
     helm
     better-defaults
     emacs-lisp
     git
     markdown
     mu4e
     (org :variables
          org-enable-reveal-js-support t
          org-enable-bootstrap-support t
     )
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-default-term-shell "/usr/bin/fish"
     )
     spell-checking
     syntax-checking
     ;; version-control
     erc
     (ranger :variables
             ranger-show-preview t
     )
     pdf-tools
     (haskell :variables
              haskell-completion-backend 'intero
              haskell-enable-hindent-style "johan-tibell"
     )
     ess
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t
     )
     bibtex
     pandoc
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '( ob-async
                                       ob-diagrams
                                       crosshairs
                                       epresent
                                       zotxt
                                       dna-mode
                                       vdiff
                                       company-eshell-autosuggest
                                       org-tree-slide
                                       langtool
                                     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '( "Fira Mono"
                               :size 11.0
                               :weight normal
                               :width normal
                               :powerline-scale 1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Add programs to the path.
  (add-to-list 'exec-path "~/.local/bin/")
  (add-to-list 'exec-path "~/git_repos/dotfiles/bin/")

  ;; Bar customization.
  (setq-default dotspacemacs-mode-line-unicode-symbols nil)
  (setq-default powerline-default-separator 'slant)
  (setq-default display-time-day-and-date t)
  (setq-default display-time-24hr-format t)
  (display-time)

  ;; Cursor type.
  (setq-default cursor-type 'box)
  (defun setBox (x)
    "Set the variable's second entry to a box."
    (setq-default x (list (car x) 'box))
  )
  (setq-default evil-normal-state-cursor (setBox evil-normal-state-cursor))
  (setq-default evil-insert-state-cursor (setBox evil-insert-state-cursor))
  (setq-default evil-replace-state-cursor (setBox evil-replace-state-cursor))
  (setq-default evil-visual-state-cursor (setBox evil-visual-state-cursor))

  ;; Persistent undo.
  (setq-default undo-tree-auto-save-history t)
  (setq-default undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; Backups
  (setq make-backup-files t)
  (setq version-control t     ;; Use version numbers for backups.
        kept-new-versions 10  ;; Number of newest versions to keep.
        kept-old-versions 0   ;; Number of oldest versions to keep.
        delete-old-versions t ;; Don't ask to delete excess backup versions.
        backup-by-copying t)  ;; Copy all files, don't rename them.
  (setq vc-make-backup-files t)
  ;; Default and per-save backups go here:
  (setq backup-directory-alist '(("" . "~/.backup/per-save")))

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

  ;; Tab and indent width. Use spaces instead of tabs.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default standard-indent 4)
  (setq-default haskell-indent-level 4)
  (setq-default haskell-indent-spaces 4)
  (setq-default evil-shift-width 4)
  (setq-default tab-stop-list (number-sequence 4 120 4))
  (define-key global-map (kbd "TAB") 'tab-to-tab-stop)

  ;; ESS disable underscore replacement with arrow.
  (add-hook 'ess-mode-hook
    (lambda ()
      (ess-smart-S-assign nil)))

  ;;
  (defvar my-offset 4 "My indentation offset. ")
  (defun backspace-whitespace-to-tab-stop ()
    "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
    (interactive)
    (if (or indent-tabs-mode
            (region-active-p)
            (save-excursion
              (> (point) (progn (back-to-indentation)
                                (point)))))
        (call-interactively 'backward-delete-char-untabify)
      (let ((movement (% (current-column) my-offset))
            (p (point)))
        (when (= movement 0) (setq movement my-offset))
        ;; Account for edge case near beginning of buffer
        (setq movement (min (- p 1) movement))
        (save-match-data
          (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
              (backward-delete-char (- (match-end 1) (match-beginning 1)))
            (call-interactively 'backward-delete-char))))))

  (global-set-key (kbd "DEL") 'backspace-whitespace-to-tab-stop)

  ;; Parentheses highlighting customization.
  (setq-default hl-paren-delay 0.01)

  ;; Native line numbers.
  (setq-default display-line-numbers t)

  ;; Escape sequence to get to normal mode.
  (setq-default evil-escape-key-sequence "kj")

  ;; Don't use the escape for these modes and functions.
  (setq evil-escape-excluded-major-modes '( dired-mode
                                            neotree-mode
                                            ranger-mode
                                            help-mode
                                            ibuffer-mode
                                            helm-mode
                                            ivy-mode
                                            pdf-view-mode
                                            doc-view-mode
                                          )
  )
  (setq evil-escape-inhibit-functions '( minibufferp
                                       )
  )

  ;; Next and previous visual-line.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Crosshairs highlighting. Unfortunately too slow when scrolling.
  ;; (spacemacs/toggle-highlight-current-line-globally-off)
  ;; (set-face-background 'col-highlight "#3c3836")
  ;; (crosshairs-mode 1)

  ;; Flycheck configuration.
  ;; No tool tips at all.
  (setq-default flycheck-display-errors-function 'flycheck-display-error-messages)

  ;; Terminal shell.
  (setq-default multi-term-program "/usr/bin/fish")
  ; New eshell each time.
  (defun eshell-new ()
    (interactive)
    (universal-argument (eshell t))
    )
  (define-key global-map (kbd "C-a") 'eshell-new)
  ; Some aliases.
  (defalias 'eshell/open 'find-file)
  (defalias 'eshell/f 'find-file)
  (defalias 'eshell/F 'find-file-other-window)
  (defalias 'eshell/x 'eshell/exit)
  (defalias 'eshell/R 'R)
  ; Disable company-mode in eshell.
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)) 'append)
  ; Make helm the default, not pcomplete.
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
  ; Company not working too well so this does nothing for now.
  (defun setup-company-eshell-autosuggest ()
    (with-eval-after-load 'company
      (setq-local company-backends '(company-eshell-autosuggest))
      (setq-local company-frontends '(company-preview-frontend))))

  (add-hook 'eshell-mode-hook 'setup-company-eshell-autosuggest)


  ;; Additional window evil bindings.
  (spacemacs/set-leader-keys "wn" 'split-window-below)
  (spacemacs/set-leader-keys "wN" 'split-window-below-and-focus)

  ;; Automatically auto fill.
  (add-hook 'text-mode-hook 'spacemacs/toggle-auto-fill-mode-on)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-auto-fill-mode-on)

  ;; Don't truncate lines. Must be put after showing the line fill.
  (spacemacs/toggle-truncate-lines-off)

  ;; Prettier doc-view.
  (setq-default doc-view-resolution 300)

  ;; Auto completion configurations.
  (global-company-mode)
  (setq-default company-idle-delay 0.2)
  (setq-default company-tooltip-limit 20)
  ; Completion key.
  (add-hook 'company-mode-hook
            (lambda()
              (global-set-key (kbd "<S-return>") 'company-complete)))
  ; Fix evil conflict.
  (evil-declare-change-repeat 'company-complete)

  ;; Grammar checking. Requires languagetool.
  (setq langtool-bin "/usr/bin/languagetool")

  ;; Make haskell have normal indentation.
  (defun custom-evil-open-above (count)
    "Insert a new line above point and switch to Insert state.
    The insertion will be repeated COUNT times."
    (interactive "p")
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (evil-insert-newline-above)
    (setq evil-insert-count count
          evil-insert-lines t
          evil-insert-vcount nil)
    (evil-insert-state 1)
    (when evil-auto-indent
      (indent-relative)))

  (defun custom-evil-open-below (count)
    "Insert a new line below point and switch to Insert state.
    The insertion will be repeated COUNT times."
    (interactive "p")
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (push (point) buffer-undo-list)
    (evil-insert-newline-below)
    (setq evil-insert-count count
          evil-insert-lines t
          evil-insert-vcount nil)
    (evil-insert-state 1)
    (when evil-auto-indent
      (indent-relative)))

  (defun rebind-evil-haskell ()
    (define-key evil-normal-state-map (kbd "O") 'custom-evil-open-above)
    (define-key evil-normal-state-map (kbd "o") 'custom-evil-open-below)
  )
  ;; Only add this indentation to haskell
  (add-hook 'haskell-mode-hook 'rebind-evil-haskell)

  ;; Add structured-haskell-mode to haskell-mode.
  ;; (add-hook 'haskell-mode-hook (lambda () (haskell-indentation-mode 0)))
  ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)

  ; The default program for haskell.
  (setq-default haskell-program-name "stack exec ghci")
  (setq-default haskell-process-type 'stack-ghci)

  ;; Default bib file for references in latex.
  (setq-default reftex-default-bibliography '("~/Dropbox/papers/global.bib"))
  (setq-default bibtex-completion-bibliography '("~/Dropbox/papers/global.bib"))

  ;; How to open the pdf with a bibtex file.
  (setq-default bibtex-completion-pdf-field "File")

  ;; Auto refresh pdfs.
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; Show images in ranger preview.
  (setq-default ranger-show-literal nil)

  ;; Hide erc messages.
  (setq-default erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; Mail.
  (with-eval-after-load 'mu4e
    (load-file "/home/gw/Dropbox/emacs/mail.el")
    )

  ;; Microsoft Office "docx" format.
  (load-file "/home/gw/git_repos/dotfiles/bin/word_file_to_org.el")

 ;; org-mode custom org directory.
 ;; Needs to load after the new org-mode (not the packaged org-mode).
 (with-eval-after-load 'org
   (setq-default org-export-backends '( ascii
                                        beamer
                                        html
                                        icalendar
                                        latex
                                        man
                                        md
                                        twbs
                                        reveal
                                        odt
                                        org
                                        texinfo
                                      )
   )
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (haskell . t)
      (shell . t)
      (R . t)
      (shell . t)
      (latex . t)
      (python . t)
      (gnuplot . t)
      (ditaa . t)
      (dot . t)
      (plantuml . t)
      (diagrams . t)
     )
   )
   ;; Library of babel location.
   (org-babel-lob-ingest "~/git_repos/dotfiles/emacs/.library_of_babel.org")
   ;; Automatic image download directory.
   (setq-default org-download-image-dir "./img")
   ;; Root org directory.
   (setq-default org-directory "~/Dropbox/org")
   (setq-default org-archive-location "~/Dropbox/org")
   ;; Where the notes are located.
   (setq-default org-default-notes-file (concat org-directory "/notes.org"))
   ;; Where the agenda files are located.
   (setq-default
    org-agenda-files
    (append (file-expand-wildcards org-directory)
            (append (file-expand-wildcards (concat org-directory "/general"))
                    (file-expand-wildcards (concat org-directory "/work"))
            )
    )
   )
   ;; Bibliography in org.
   (setq-default org-ref-default-bibliography '("~/Dropbox/papers/global.bib")
         org-ref-pdf-directory "~/Dropbox/papers/"
         org-ref-bibliography-notes "~/Dropbox/papers/notes.org"
         ; For \autocite instead of cite:
         org-ref-default-citation-link "autocite")
   ;; Start in org-indent-mode.
   ;; (add-hook 'org-mode-hook 'org-indent-mode)
   ;; Allow PDF files to be shown in org.
   (add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
   (add-to-list 'image-file-name-extensions "pdf")
   (setq-default imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))
   ;; Log times with TODOs.
   (setq-default org-log-done 'time)
   ;; Make image width 1/3 the size of the display.
   (setq-default org-image-actual-width (/ (display-pixel-width) 3))
   ; #+PROPERTY: header-args :eval never-export
   ; We set the above property in all files by default here.
   (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))
   (add-to-list 'org-babel-default-inline-header-args '(:eval . "never-export"))
   (add-to-list 'org-babel-default-lob-header-args '(:eval . "never-export"))
   ;; No table of contents by default.
   (setq-default org-export-with-toc nil)
   ;; No section numbering by default.
   (setq-default org-export-with-section-numbers nil)
   ;; Asynchronous exporting. Not working with colorboxes or bibliography right now.
   ; (setq-default org-export-in-background t)
   ;; Asynchronous source block execution. Does not support sessions yet, or
   ;; localize code.
   (require 'ob-async)
   ;(add-to-list 'org-babel-default-header-args '(:async . t))
   ;(add-to-list 'org-babel-default-inline-header-args '(:async . t))
   ;(add-to-list 'org-babel-default-lob-header-args '(:async . t))
   ;; Do not change indentation in src blocks.
   (setq-default org-src-preserve-indentation t)
   ;;;; For specific files types.
   ;; Latex command.
   (setq-default org-latex-pdf-process '("latexmk -pdf --shell-escape"))
   (setq-default org-list-allow-alphabetical t)
   ;; Set the path for ditaa.
   (setq org-ditaa-jar-path "/usr/bin/ditaa")
   ;; Haskell diagrams executable.
   (setq org-diagrams-executable "stack exec diagrams-builder-cairo --")
   ;; use runhaskell when ":results output"
   (defadvice org-babel-haskell-initiate-session
       (around org-babel-haskell-initiate-session-advice)
     (let* ((buff (get-buffer "*haskell*"))
            (proc (if buff (get-buffer-process buff)))
            (type (cdr (assoc :result-type params)))
            (haskell-program-name
             (if (equal type 'output) "~/git_repos/dotfiles/bin/ob-stack" "stack exec ghci")))
       (if proc (kill-process proc))
       (sit-for 0)
       (if buff (kill-buffer buff))
       ad-do-it))

   (ad-activate 'org-babel-haskell-initiate-session)
   ;; Command for python.
   (setq org-babel-python-command "python3") 
   ;; Org reveal.
   (setq-default org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
   ;; Org letters.
   ; No fold marks on the side.
   (setq-default org-koma-letter-use-foldmarks nil)
   ;; For beamer.
   ; Color box blocks.
   (add-hook 'org-beamer-mode-hook
        (lambda()
        (add-to-list 'org-beamer-environments-extra
                    '("tcolorboxenv" "T" "\\begin{tcolorbox}[%O,title=%h]" "\\end{tcolorbox}"))
        (add-to-list 'org-beamer-environments-extra
                    '("tcolorboxnotitleenv" "X" "\\begin{tcolorbox}[%O]" "\\end{tcolorbox}"))))
   ; Beamer grid backend.
   (load-file "/home/gw/git_repos/dotfiles/emacs/ox-beamer-grid.el")
  )
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(package-selected-packages
   (quote
    (org-mime ghub org-tree-slide langtool company-eshell-autosuggest vdiff evil-goggles org-category-capture ob-async ob-diagrams request-deferred spinner tablist ht alert log4e gntp simple-httpd json-snatcher json-reformat hydra parent-mode request helm-bibtex parsebib gitignore-mode fringe-helper git-gutter+ git-gutter flyspell-correct pkg-info epl flx iedit anzu goto-chg ctable diminish col-highlight vline web-completion-data dash-functional tern pos-tip bind-map bind-key biblio biblio-core packed auctex s auto-complete popup key-chord multiple-cursors undo-tree skewer-mode powerline highlight with-editor haml-mode js2-mode gruvbox-theme-theme company hl-line+ julia-mode nlinum magit-popup git-commit async ghc ess smartparens deferred evil flycheck markdown-mode dash winum unfill fuzzy helm-bbdb bbdb mu4e-maildirs-extension mu4e-alert yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic ivy haskell-mode f helm helm-core yasnippet avy projectile magit company-quickhelp zotxt yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters quelpa pug-mode popwin persp-mode pdf-tools pcre2el paradox pandoc-mode ox-twbs ox-reveal ox-pandoc orgit org-ref org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file nlinum-relative neotree mwim multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode link-hint less-css-mode json-mode js2-refactor js-doc intero insert-shebang info+ indent-guide ido-vertical-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md flyspell-correct-helm flycheck-pos-tip flycheck-haskell flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu ess-smart-equals ess-R-object-popup ess-R-data-view eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks epresent emmet-mode elisp-slime-nav dumb-jump dna-mode diff-hl define-word csv-mode crosshairs company-web company-tern company-statistics company-shell company-ghci company-ghc company-cabal company-auctex column-enforce-mode coffee-mode cmm-mode clean-aindent-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C")))))
