;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-complete-with-key-sequence-delay 0
                      auto-completion-idle-delay 0
     )
     ;; version-control
     better-defaults
     bibtex
     csv
     docker
     emacs-lisp
     erc
     ;ess keep those arrows away from me
     git
     haskell
     helm
     html
     javascript
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t
     )
     lsp
     markdown
     mu4e
     neotree
     nixos
     pandoc
     pdf
     (python :variables
             python-sort-imports-on-save t
             python-enable-yapf-format-on-save t
             python-backend 'lsp
     )
     shell-scripts
     spell-checking
     syntax-checking
     yaml
     (org :variables
          org-enable-reveal-js-support nil
          org-enable-bootstrap-support t
     )
     (ranger :variables
             ranger-show-preview t
     )
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-default-term-shell "/usr/bin/fish"
     )
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '( dna-mode
                                       epresent
                                       esh-autosuggest
                                       jupyter
                                       langtool
                                       lsp-haskell
                                       ob-async
                                       ob-diagrams
                                       org-tree-slide
                                       vdiff
                                       vlf
                                       zotxt
                                     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(importmagic)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gruvbox
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '( "Iosevka Slab"
                               :size 13.0
                               :weight light
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
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

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
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
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

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
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  (setq-default haskell-indent-level 2)
  (setq-default haskell-indent-spaces 2)
  (setq-default evil-shift-width 2)
  (setq-default tab-stop-list (number-sequence 2 120 2))
  (define-key global-map (kbd "TAB") 'tab-to-tab-stop)

  ;; Speed up tramp eshell.
  (defun spacemacs/title-prepare (title-format) "")

  ;; ESS disable underscore replacement with arrow.
  (add-hook 'ess-mode-hook
    (lambda ()
      (ess-smart-S-assign nil)))

  ;;
  (defvar my-offset 2 "My indentation offset. ")
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
    (eshell 'N) ; Forces new session.
    )
  (define-key global-map (kbd "C-a") 'eshell-new)
  ; Remove eshell window on exit.
  (defun eshell-remove-on-exit ()
    (when (not (one-window-p))
      (delete-window)))
  ; Apply function to all eshell buffers.
  (defun apply-function-to-all-mode (f mode)
    "Apply function in all buffers in a certain mode."
    (interactive)
    (dolist ($buf (buffer-list))
      (with-current-buffer $buf
        (when (equal major-mode mode)
          (funcall f)
          ))))
  ; Read eshell history in all eshell buffers.
  (defun reload-all-eshell-history ()
    "Read eshell history in all eshell buffers."
    (interactive)
    (apply-function-to-all-mode #'eshell-read-history 'eshell-mode))
  ; Add to history every time after command is sent.
  (add-hook 'eshell-post-command-hook #'eshell-write-history t)
  (add-hook 'eshell-post-command-hook #'reload-all-eshell-history t)

  (advice-add 'eshell-life-is-too-much :after 'eshell-remove-on-exit)
  ; Some aliases.
  (defalias 'eshell/open 'find-file)
  (defalias 'eshell/f 'find-file)
  (defalias 'eshell/F 'find-file-other-window)
  (defalias 'eshell/x 'eshell/exit)
  (defalias 'eshell/R 'R)
  ; Persistent helm history.
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'helm-ff-history))
  ; Make company and helm the default, not pcomplete.
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'company-complete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
  ; Disable some "features" from the shell layer.
  (defun spacemacs//toggle-shell-auto-completion-based-on-path ()
    "Deactivates automatic completion on remote paths.
Retrieving completions for Eshell blocks Emacs. Over remote
connections the delay is often annoying, so it's better to let
the user activate the completion manually."
    )

  (defun spacemacs//eshell-switch-company-frontend ()
    "Sets the company frontend to `company-preview-frontend' in e-shell mode."
    )

  ;; Additional window evil bindings.
  (spacemacs/set-leader-keys "wn" 'split-window-below)
  (spacemacs/set-leader-keys "wN" 'split-window-below-and-focus)

  ;; Automatically auto fill.
  (add-hook 'text-mode-hook 'spacemacs/toggle-auto-fill-mode-on)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-auto-fill-mode-on)

  ;; Don't truncate lines. Must be put after showing the line fill.
  (spacemacs/toggle-truncate-lines-off)

  ;; Projectile backend to respect ignored files.
  (setq-default projectile-enable-caching t)
  (setq-default projectile-indexing-method 'native)

  ;; Prettier doc-view.
  (setq-default doc-view-resolution 300)

  ;; Auto completion configurations.
  (global-company-mode)
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
  (setq-default haskell-process-path-ghci "stack")
  (setq-default haskell-process-args-ghci '("exec" "ghci"))

  ; lsp for haskell.
  (require 'lsp)
  (require 'lsp-ui)
  (require 'lsp-haskell)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'haskell-mode-hook 'lsp)
  (add-hook 'haskell-mode-hook 'flycheck-mode)

  ;; Python
  (setq-default python-tab-width 2)

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
                                        ;; reveal ; Not working due to Org 9.2
                                        odt
                                        org
                                        texinfo
                                      )
   )
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (R . t)
      (diagrams . t)
      (ditaa . t)
      (dot . t)
      (gnuplot . t)
      (haskell . t)
      (latex . t)
      (plantuml . t)
      (python . t)
      (shell . t)
      (jupyter . t)
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

   ;; Allow more lines to be emphasized with org (If you want multiple lines for
   ;; inline underline, bold, etc.).
   (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
   (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))
   (org-element--set-regexps)

   ;; Disable C-a in org (need it for eshell).
   (define-key org-mode-map (kbd "C-a") nil)

   ;; Bibliography in org.
   (setq-default org-ref-default-bibliography '("~/Dropbox/papers/global.bib")
         org-ref-pdf-directory "~/Dropbox/papers/"
         org-ref-bibliography-notes "~/Dropbox/papers/notes.org"
         ; For \autocite instead of cite:
         org-ref-default-citation-link "autocite")

   ;; Start zotxt link.
   (add-hook 'org-mode-hook 'org-zotxt-mode)

   ;; Start in org-indent-mode.
   ;; (add-hook 'org-mode-hook 'org-indent-mode)
   ;; Don't show images by default.
   (setq org-startup-with-inline-images nil)

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
   ; Disable for jupyter as they have their own async.
   (setq ob-async-no-async-languages-alist '("jupyter-haskell" "jupyter-python" "jupyter-R"))
   ;(add-to-list 'org-babel-default-header-args '(:async . t))
   ;(add-to-list 'org-babel-default-inline-header-args '(:async . t))
   ;(add-to-list 'org-babel-default-lob-header-args '(:async . t))

   ;; Do not change indentation in src blocks.
   (setq-default org-src-preserve-indentation t)

   ;; Don't indent based on header.
   (setq-default org-adapt-indentation nil)

   ;;;; For specific files types.

   ;; Latex command.
   (setq-default org-latex-pdf-process '("latexmk -pdf --shell-escape %f"))

   ;; Latex allow utf8.
   (setq-default org-latex-inputenc-alist '(("utf8")))
   (setq-default org-list-allow-alphabetical t)

   ;; Set the path for ditaa.
   (setq org-ditaa-jar-path "/usr/bin/ditaa")

   ;; Haskell diagrams executable.
   (setq org-diagrams-executable "stack exec diagrams-builder-cairo --")
   ;; use runhaskell when ":results output"
   ;; (defadvice org-babel-haskell-initiate-session
   ;;     (around org-babel-haskell-initiate-session-advice)
   ;;   (let* ((buff (get-buffer "*haskell*"))
   ;;          (proc (if buff (get-buffer-process buff)))
   ;;          (type (cdr (assoc :result-type params)))
   ;;          (haskell-program-name
   ;;           (if (equal type 'output) "~/git_repos/dotfiles/bin/ob-stack" "stack exec ghci")))
   ;;     (if proc (kill-process proc))
   ;;     (sit-for 0)
   ;;     (if buff (kill-buffer buff))
   ;;     ad-do-it))

   ;; (ad-activate 'org-babel-haskell-initiate-session)

   ;; Command for python.
   (setq org-babel-python-command "python3")

   ;; Org reveal. ; Not working due to Org 9.2
   ;; (setq-default org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")

   ;; Org letters.
   ; No fold marks on the side.
   (setq-default org-koma-letter-use-foldmarks nil)

   ;; For beamer.
   ; Custom environments.
   (add-hook 'org-beamer-mode-hook
        (lambda()
        (add-to-list 'org-beamer-environments-extra
                    '("tcolorboxenv" "T" "\\begin{tcolorbox}[%O,title=%h]" "\\end{tcolorbox}"))
        (add-to-list 'org-beamer-environments-extra
                    '("tcolorboxnotitleenv" "X" "\\begin{tcolorbox}[%O]" "\\end{tcolorbox}"))
        (add-to-list 'org-beamer-environments-extra
                    '("adjustwidth_wide" "w" "\\begin{adjustwidth}{-3em}{-3em}" "\\end{adjustwidth}"))))
   ; Beamer grid backend.
   (load-file "/home/gw/git_repos/dotfiles/emacs/ox-beamer-grid.el")
  )
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (json-mode zotxt yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vlf vi-tilde-fringe vdiff uuidgen use-package unfill toc-org tagedit symon string-inflection spaceline-all-the-icons smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode prettier-js popwin pippel pipenv pip-requirements persp-mode pcre2el password-generator paradox pandoc-mode ox-twbs ox-reveal ox-pandoc overseer orgit org-tree-slide org-ref org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file ob-diagrams ob-async nix-mode neotree nameless mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode markdown-toc magit-svn magit-gitflow macrostep lsp-ui lsp-haskell lorem-ipsum livid-mode live-py-mode link-hint langtool jupyter json-snatcher json-reformat json-navigator js2-refactor js-doc insert-shebang indent-guide impatient-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-nixos-options helm-mu helm-mode-manager helm-make helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets gruvbox-theme google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-haskell flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu ess-R-data-view eshell-z eshell-prompt-extras esh-help esh-autosuggest erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks epresent emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-modeline dockerfile-mode docker dna-mode diminish define-word cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-shell company-quickhelp company-nixos-options company-lsp company-ghci company-cabal company-auctex company-anaconda column-enforce-mode cmm-mode clean-aindent-mode centered-cursor-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
