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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(systemd
     rust
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; (auto-completion :variables
     ;;                  auto-completion-return-key-behavior nil
     ;;                  auto-completion-tab-key-behavior 'complete
     ;;                  auto-completion-complete-with-key-sequence nil
     ;;                  auto-completion-enable-help-tooltip t
     ;;                  auto-completion-enable-sort-by-usage t
     ;;                  auto-completion-enable-snippets-in-popup t
     ;;                  auto-completion-complete-with-key-sequence-delay 0
     ;;                  auto-completion-idle-delay 0
     ;;                  auto-completion-use-company-box t
     ;;                  )
     ;; version-control
     better-defaults
     bibtex
     csv
     dap
     docker
     eglot
     (elfeed :variables rmh-elfeed-org-files (list "~/Nextcloud/emacs/feeds/pubmed.org"))
     emacs-lisp
     erc
     ess
     finance
     git
     graphviz
     (haskell :variables haskell-completion-backend 'lsp)
     ;;helm
     compleseus
     html
     javascript
     json
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t
            latex-build-engine 'luatex
            latex-refresh-preview t
            latex-backend nil
            )
     ;; (lsp :variables
     ;;      lsp-lens-enable t
     ;;      lsp-use-lsp-ui t
     ;;      lsp-ui-doc-enable t
     ;;      )
     markdown
     mu4e
     ;; multiple-cursors ;; overwrite keybindings
     treemacs
     nixos
     pandoc
     pdf
     (python :variables
             python-sort-imports-on-save t
             python-formatter 'yapf
             python-enable-yapf-format-on-save t
             python-backend 'lsp
             )
     semantic
     ;; get spell check from languagetool, but now not disabled as
     ;; no dictionary add and the like
     (spell-checking :variables
                     enable-flyspell-auto-completion nil
                     ispell-dictionary "en_CA"
                     )
     syntax-checking
     yaml
     (org :variables
          org-enable-reveal-js-support t
          org-enable-bootstrap-support t
          org-enable-hugo-support t
          org-enable-roam-support t
          org-roam-directory (file-truename "~/Nextcloud/org/org-roam")
          org-roam-db-location (file-truename "~/Nextcloud/org/org-roam/org-roam.db")
          org-enable-roam-protocol t
          org-enable-roam-ui t
          )
     (ranger :variables
             ranger-show-preview t
             )
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom
     ;;        shell-default-shell 'eshell
     ;;        shell-default-term-shell "fish"
     ;;        )
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '( academic-phrases
                                       beacon
                                       ;; company-fuzzy
                                       citeproc
                                       ;; company-shell
                                       cape
                                       corfu
                                       dna-mode
                                       elfeed-score
                                       ellama
                                       emacs-everywhere
                                       epresent
                                       esh-autosuggest
                                       ;; flycheck-languagetool
                                       flycheck-vale
                                       flyspell-correct-avy-menu
                                       git-auto-commit-mode
                                       gruvbox-theme
                                       ;; jupyter
                                       oauth2
                                       ob-async
                                       ob-diagrams
                                       ;; org-caldav
                                       ;; org-gcal
                                       org-msg ;; Until it is fixed with new mu4e
                                       org-noter
                                       (org-pandoc-import :location (recipe
                                                                     :fetcher
                                                                     github
                                                                     :repo
                                                                     "tecosaur/org-pandoc-import"
                                                                     :files ("*.el" "filters" "preprocessors")))
                                       org-roam-bibtex
                                       org-tree-slide
                                       ormolu
                                       slurm-mode
                                       tiny
                                       vdiff
                                       vlf
                                       (explain-pause-mode :location (recipe :fetcher github :repo "lastquestion/explain-pause-mode"))
                                       yasnippet
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
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(gruvbox-dark-medium
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme 'doom

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '( "Iosevka Term Slab Compressed"
                                :size 11.0
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

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
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

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
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 100

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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

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

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

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
  (add-to-list 'exec-path "~/.nix-profile/bin/")

  ;; ;; EXWM
  ;; (require 'exwm)
  ;; (require 'exwm-config)
  ;; (require 'exwm-systemtray)
  ;; (exwm-config-default)
  ;; (exwm-systemtray-enable)
  ;; ; randr
  ;; (require 'exwm-randr)
  ;; (defun exwm-change-screen-hook ()
  ;;   (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
  ;;         default-output)
  ;;     (with-temp-buffer
  ;;       (call-process "xrandr" nil t nil)
  ;;       (goto-char (point-min))
  ;;       (re-search-forward xrandr-output-regexp nil 'noerror)
  ;;       (setq default-output (match-string 1))
  ;;       (forward-line)
  ;;       (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
  ;;           (call-process "xrandr" nil nil nil "--output" default-output "--auto")
  ;;         (call-process
  ;;          "xrandr" nil nil nil
  ;;          "--output" (match-string 1) "--primary" "--auto"
  ;;          "--output" default-output "--off")
  ;;         (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))
  ;; (add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)
  ;; (exwm-randr-enable)

  ;; Bar customization.
  (setq-default dotspacemacs-mode-line-unicode-symbols nil)
  (setq-default powerline-default-separator 'slant)
  (setq-default display-time-day-and-date t)
  (setq-default display-time-24hr-format t)
  (display-time)

  ;; Centered buffer
  (setq-default writeroom-width 100)
  (setq-default writeroom-mode-line t)
  (setq-default writeroom-maximize-window nil)
  (global-writeroom-mode)

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

  ;; Cursor highlighting
  (beacon-mode 1)

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

  ;; Ellama key combo
  (define-key global-map (kbd "M-C-e") 'ellama-improve-grammar)

  ;; Change auth order.
  (setq-default auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

  ;; Speed up tramp eshell.
  (defun spacemacs/title-prepare (title-format) "")

  ;; Tramp shell, allow for remote paths.
  (add-hook 'tramp--startup-hook '(lambda ()
                                    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
                                    ;; Tramp shell, allow for local display
                                    (add-to-list 'tramp-remote-process-environment
                                                 (format "DISPLAY=%s" (getenv "DISPLAY")))
                                    ))

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
                                            mu4e-view-mode
                                            mu4e-headers-mode
                                            )
        )
  (setq evil-escape-inhibit-functions '( minibufferp
                                         )
        )

  ;; Does not work with dd at beginning of line with whitespace enabled.
  ;; Next and previous visual-line.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Crosshairs highlighting. Unfortunately too slow when scrolling.
  ;; (spacemacs/toggle-highlight-current-line-globally-off)
  ;; (set-face-background 'col-highlight "#3c3836")
  ;; (crosshairs-mode 1)

  ;; Ediff
                                        ; Colors to be readable
  ;; (add-hook 'ediff-load-hooks
  ;;           (function (lambda ()
  ;;                       (set-face-foreground ediff-current-diff-face-A "White")
  ;;                       (set-face-background ediff-current-diff-face-A "dark red")
  ;;                       (set-face-foreground ediff-current-diff-face-B "White")
  ;;                       (set-face-background ediff-current-diff-face-B "medium blue")
  ;;                       (set-face-foreground ediff-current-diff-face-C "White")
  ;;                       (set-face-background ediff-current-diff-face-C "dark magenta")
  ;;                       (set-face-foreground ediff-fine-diff-face-A "White")
  ;;                       (set-face-background ediff-fine-diff-face-A "brown")
  ;;                       (set-face-foreground ediff-fine-diff-face-B "White")
  ;;                       (set-face-background ediff-fine-diff-face-B "brown")
  ;;                       (set-face-foreground ediff-fine-diff-face-C "White")
  ;;                       (set-face-background ediff-fine-diff-face-C "brown"))))

  ;; Flycheck configuration.
  ;; No tool tips at all.
                                        ; (setq-default flycheck-display-errors-function 'flycheck-display-error-messages)
  ;; When to check, was too slow as the default value was '(save idle-change new-line mode-enabled)
  ;; (setq-default flycheck-check-syntax-automatically '(save mode-enable))
  ;; Setup flycheck-vale
  (with-eval-after-load 'flycheck
    (flycheck-vale-setup)
    ;; (flycheck-languagetool-setup)
    ;; (load-file "~/.config/emacs/flycheck-languagetool.el")
    )
  ;; Only on save
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; Max errors
  (setq-default flycheck-checker-error-threshold 1000)
  ;; Choose modes
  (add-to-list 'flycheck-global-modes 'markdown-mode)

  ;; flyspell
  (require 'flyspell-correct-avy-menu)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

  ;; Whitespace mode configuration.
                                        ; Always enable it.
  (spacemacs/toggle-whitespace-globally-on)
                                        ; Customize look.
  (setq-default whitespace-style (remove 'lines whitespace-style))
  (push 'lines-tail whitespace-style)
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray25")
  (set-face-attribute 'whitespace-newline nil :background nil :foreground "gray25")
  (set-face-attribute 'whitespace-tab nil :background nil :foreground "gray25")

  ;; Terminal shell.
  (setq-default multi-term-program "fish")
  (setq-default shell-file-name "bash")
  (setq-default explicit-shell-file-name "bash")
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
  ;; For no helm, disable company
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (eshell-cmpl-initialize)
  ;;             (company-mode -1)
  ;;             'append
  ;;             )
  ;;           )
  ;; Persistent helm history.
  ;; (with-eval-after-load 'desktop
  ;;   (add-to-list 'desktop-globals-to-save 'helm-ff-history))
  ;; Make helm the default, not pcomplete, and disable company-mode.
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (eshell-cmpl-initialize)
  ;;             (company-mode -1)
  ;;             (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  ;;             (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  ;;           'append
  ;;           )
  ;; For company completion in eshell. Unused for now, using helm.
  ;;   ; Disable some "features" from the shell layer.
  ;;   (defun spacemacs//toggle-shell-auto-completion-based-on-path ()
  ;;     "Deactivates automatic completion on remote paths.
  ;; Retrieving completions for Eshell blocks Emacs. Over remote
  ;; connections the delay is often annoying, so it's better to let
  ;; the user activate the completion manually."
  ;;     )

  ;; (defun spacemacs//eshell-switch-company-frontend ()
  ;;   "Sets the company frontend to `company-preview-frontend' in e-shell mode."
  ;;   )

  ;; vterm auto insert
  (add-hook 'vterm-mode-hook (lambda ()
                               (interactive)
                               (evil-insert-state)))

  ;; Helm configurations.
  ;; Do not open new frame for helm.
  ;; (setq helm-show-completion-display-function #'helm-show-completion-default-display-function)

  ;; Vertico configurations

  ;; Disable case sensitivity
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  ;; completion-preview-mode

  ;; https://thanosapollo.org/posts/emacs-built-in-completions-video/
  ;; Not in use right now as it has no columns.
  (setf completion-styles '(basic flex)
        completion-auto-select t ;; Show completion on first call
        completion-auto-help 'lazy ;; Display *Completions* upon first request
        completions-format 'one-column ;; Use only one column
        completions-sort 'historical ;; Order based on minibuffer history
        completions-max-height 20 ;; Limit completions to 15 (completions start at line 5)
        completion-preview-minimum-symbol-length 2 ;; Minimum length of the symbol at point for showing completion preview
        completion-ignore-case t)

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
                                        ; Projectile to avoid remote directories
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  ;; Magit remember credentials
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)

  ;; Prettier doc-view.
  (setq-default doc-view-resolution 300)

  ;; Auto completion configurations.

                                        ; Corfu
  (global-corfu-mode)
  ;; Disable minibuffer completion which messes up a lot
  (setq global-corfu-minibuffer nil)

  ;; Other modes
  (setq corfu-echo-mode t)
  (setq corfu-history-mode t)
  (setq corfu-popupinfo-mode t)

  (setopt corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (setopt corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (setopt corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (setopt corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (setopt corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (setopt corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setopt tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (setopt text-mode-ispell-word-completion nil)

  ;; dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)

  ;; Optionally use the `orderless' completion style.
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (setopt completion-styles '(orderless basic))
  (setopt completion-category-defaults nil)
  (setopt completion-category-overrides '((file (styles partial-completion))))

  (setq corfu-auto        t
        corfu-auto-delay  0
        corfu-auto-prefix 0)

  (add-hook 'corfu-mode-hook
            (lambda ()
              ;; Settings only for Corfu
              (setq-local completion-styles '(basic)
                          completion-category-overrides nil
                          completion-category-defaults nil)))

  ;; Transfer to minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "C-/" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  ;; Keymap
  (keymap-unset evil-insert-state-map "C-k")

  ;; Free the RET key for less intrusive behavior.
  ;; Option 1: Unbind RET completely
  ;; (keymap-unset corfu-map "RET")
  ;; Option 2: Use RET only in shell modes
  (keymap-unset corfu-map "RET")
  (keymap-set corfu-map "C-j" #'corfu-next)
  (keymap-set corfu-map "C-k" #'corfu-previous)

  ;; Cape
  (add-hook 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
  (add-hook 'completion-at-point-functions (cape-company-to-capf #'company-keywords))
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-tex)

  ;; Capitals
  (setq-default dabbrev-case-fold-search nil)

  ;; With eglot
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       (cape-company-to-capf #'company-yasnippet)
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

  ;; For eshell
  (defun eshell-completions ()
    "Use specific completions in eshell mode"
    (interactive)
    (setq-local completion-at-point-functions (list #'cape-file))
    (define-key eshell-mode-map (kbd "M-p") #'cape-history)
    (define-key eshell-hist-mode-map (kbd "M-p") #'cape-history)
    )
  (add-hook 'eshell-mode-hook #'eshell-completions)

  ;; (setq-local completion-at-point-functions
  ;;             (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))

  ;; Use Company backends as Capfs.
  ;; (setq-local completion-at-point-functions
  ;;             (mapcar #'cape-company-to-capf
  ;;                     (list #'company-files
  ;;                           #'company-keywords
  ;;                           #'company-dabbrev
  ;;                           #'company-yasnippet
  ;;                           )))

  ;; Dictionary location
  ;; (setq cape-dict-file "/run/current-system/sw/share/hunspell/en_US.dic")

  ;; Not recommended, but necessary
  ;; (global-company-mode)

  ;; Eglot configuration for LSP
  (with-eval-after-load 'eglot
    (setq-default eglot-workspace-configuration '(
                                                  :texlab (:latexindent (:local "/home/gw/.config/latexindent/.indentconfig.yaml")))
                  )
    )


  ;; Allow for yasnippet in LaTeX
  (setq lsp-completion-provider :none)

  ;; Fix evil conflict.
  ;; (evil-declare-change-repeat 'company-complete)

  ;; Improved faces
  ;; (custom-set-faces
  ;;  '(company-tooltip-common
  ;;    ((t (:inherit company-tooltip :weight bold :underline nil))))
  ;;  '(company-tooltip-common-selection
  ;;    ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; Add company-files to sh-script mode.
  ;; (add-hook 'sh-mode-hook
  ;;           (lambda ()
  ;;             (company-mode-on)
  ;;             (add-to-list 'company-backends '(company-files
  ;;                                              company-shell
  ;;                                              company-shell-env
  ;;                                              company-keywords
  ;;                                              company-capf
  ;;                                              company-dabbrev-code
  ;;                                              company-etags
  ;;                                              company-dabbrev
  ;;                                              :with company-yasnippet))))

  ;; Fuzzy completion, put after all company configurations.
  ;; (global-company-fuzzy-mode 1)
  ;; (setq company-fuzzy-prefix-on-top t)

                                        ; Snippets
  (yas-global-mode 1)
  (setq snippet-dirs '("~/git_repos/dotfiles/emacs/snippets/" "~/Nextcloud/emacs/snippets/"))
  (setq yas-snippet-dirs (append yas-snippet-dirs snippet-dirs))
  (setq-default auto-completion-private-snippets-directory yas-snippet-dirs)
  ;; Don't auto indent.
  (setq-default yas-indent-line "fixed")
  ;; Needed to actually load the snippets
  (yas-reload-all)

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

  ;; Python
  (add-hook 'python-mode-hook '(lambda ()
                                 (setq python-indent 2)
                                 (setq python-indent-offset 2)
                                 (setq python-tab-width 2)))

  ;; Bash
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)

  ;; Dictionary and thesaurus lookup.
  (defun lookup-word (word)
    (interactive (list (save-excursion (car (ispell-get-word nil)))))
    (eww-browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

  (global-set-key (kbd "M-#") 'lookup-word)

  ;; Default bib file for references in latex.
  (setq-default reftex-default-bibliography '("~/Nextcloud/papers/global.bib"))
  (setq-default bibtex-completion-bibliography '("~/Nextcloud/papers/global.bib"))

  ;; Shell escape in latex
  (setq-default TeX-command-extra-options "-shell-escape")

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
    (load-file "~/Nextcloud/emacs/mail.el")
    )

  ;; Calendar
  (with-eval-after-load 'org-caldav
    (load-file "~/Nextcloud/emacs/calendar.el.gpg")
    )
  (with-eval-after-load 'org-gcal
    (load-file "~/Nextcloud/emacs/calendar_gcal.el.gpg")
    )

  ;; Hopefully temporary solutions to annoying issues.
                                        ; Fix issue with locked recentf.
  (cancel-timer recentf-auto-save-timer)

  ;; Elfeed
  (with-eval-after-load 'elfeed
    ;; Not until bugs fixed
    (load-file "~/Nextcloud/emacs/feeds/elfeed-funcs.el")
    (elfeed-score-enable)
    (setq elfeed-score-serde-score-file "~/Nextcloud/emacs/feeds/elfeed-scoring.el")
    (define-key elfeed-search-mode-map "=" elfeed-score-map)
    )

  ;; Org-roam
  (with-eval-after-load 'org-roam
                                        ; (setq org-roam-directory (file-truename "~/Nextcloud/org/org-roam"))
    (org-roam-db-autosync-mode)
    (setq org-roam-node-display-template "${title} ${tags}")
    )
                                        ; Org-roam buffer links do not work unless page-break-lines-mode is disabled.
  (global-page-break-lines-mode 0)

  ;; Start not fullscreen (above variables don't work)
  (spacemacs/toggle-fullscreen-frame-off)

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
                                         re-reveal
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
       ;; (jupyter . t)
       )
     )
    ;; Library of babel location.
    (org-babel-lob-ingest "~/git_repos/dotfiles/emacs/.library_of_babel.org")

    ;; Automatic image download directory.
    (setq-default org-download-image-dir "~/OneDrive/work/img/downloads")
    ;; Image yank command
    (setq-default org-download-screenshot-method "import %s")
    ;; Don't show inline images automatically in buffer
    (setq-default org-download-display-inline-images nil)
    ;; Make sure download paths are absolute
    (setq-default org-download-abbreviate-filename-function 'expand-file-name)
    ;; Insert width automatically
    (setq-default org-download-image-attr-list '("#+attr_latex: :options [keepaspectratio, height=0.8\\textheight, width=0.8\\linewidth]"))

    ;; Root org directory.
    (setq-default org-directory "~/Nextcloud/org")
    (setq-default org-archive-location (concat org-directory "/archive.org::"))

    ;; Where the notes are located.
    (setq-default org-default-notes-file (concat org-directory "/notes.org"))

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

    ;; Ignore #+STARTUP when org-agenda searches through files.
    (setq org-agenda-inhibit-startup t)

    ;; Ignore archives in agenda.
    (setq org-agenda-archives-mode nil)

    ;; Org refile options, from https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
    (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

    ;; Allow more lines to be emphasized with org (If you want multiple lines for
    ;; inline underline, bold, etc.).
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
    (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))
    (org-element--set-regexps)

    ;; Disable C-a in org (need it for eshell).
    (define-key org-mode-map (kbd "C-a") nil)

    ;; Bibliography in org.
    (setq-default org-ref-default-bibliography '("~/Nextcloud/papers/global.bib")
                  org-ref-pdf-directory "~/Nextcloud/papers/"
                  org-ref-bibliography-notes "~/Nextcloud/papers/notes.org"
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

    ;; Smart quotation exports.
    (setq-default org-export-with-smart-quotes t)

    ;; Asynchronous exporting. Not working with colorboxes or bibliography right now.
                                        ; (setq org-export-async-init-file nil)
                                        ; (setq-default org-export-in-background t)

    ;; Do not change indentation in src blocks.
    (setq-default org-src-preserve-indentation t)

    ;; Don't indent based on header.
    (setq-default org-adapt-indentation nil)

   ;;;; For specific files types.

    ;; Latex command.
    ;; Works with xelatex too with #+LATEX_COMPILER: xelatex
    ;;(setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))
    (setq org-latex-compiler "lualatex")

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

    ;; Options for pandoc
    (setq org-pandoc-options-for-docx '((reference-doc . "~/Nextcloud/pandoc/reference.docx")))

   ;;; Markdown citations (https://gist.github.com/kleinschmidt/5ab0d3c423a7ee013a2c01b3919b009a)
    ;; reftex in markdown mode
    (setq reftex-default-bibliography '("~/Nextcloud/papers/global.bib"))

    ;; define markdown citation formats
    (defvar markdown-cite-format)
    (setq markdown-cite-format
          '(
            (?\C-m . "[@%l]")
            (?p . "[@%l]")
            (?t . "@%l")
            )
          )

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

    ;; Org reveal. ; Not working due to Org 9.2
    (setq-default org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@4.1.0")
    (setq-default org-re-reveal-revealjs-version "4")
    (setq-default org-re-reveal-transition "fade")
    (setq-default org-re-reveal-transition-speed "fast")

    ;; Org letters.
                                        ; No fold marks on the side.
    (setq-default org-koma-letter-use-foldmarks nil)

    ;; Plantuml.
    (setq-default org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")

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

    ;; pdf-tools keybindings.
    (defun add-pdf-view-keys ()
      (define-key pdf-view-mode-map [mouse-2] 'pdf-annot-add-highlight-markup-annotation)
      )
    (add-hook 'pdf-view-mode-hook 'add-pdf-view-keys)

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
   '(org-emphasis-alist
     (quote
      (("*" bold)
       ("/" italic)
       ("_" underline)
       ("=" org-verbatim verbatim)
       ("~" org-code verbatim)
       ("+"
        (:strike-through t)))))
   '(package-selected-packages
     (quote
      (company-box tern systemd toml-mode racer helm-gtags ggtags flycheck-rust counsel-gtags cargo rust-mode yasnippet-snippets xterm-color paradox org-ref org-mime lsp-ui helm-make git-timemachine expand-region evil-nerd-commenter dumb-jump doom-modeline counsel ess flycheck company window-purpose helm multiple-cursors avy lsp-mode magit transient git-commit which-key zotxt yapfify yaml-mode ws-butler writeroom-mode with-editor winum web-mode web-beautify volatile-highlights vlf vi-tilde-fringe vdiff uuidgen use-package unfill toc-org tagedit symon swiper string-inflection spinner spaceline-all-the-icons smeargle slim-mode shrink-path shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters pytest pyenv-mode py-isort pug-mode prettier-js popwin pippel pipenv pip-requirements persp-mode pdf-tools pcre2el password-generator pandoc-mode ox-twbs ox-pandoc overseer orgit org-tree-slide org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file ob-diagrams nix-mode neotree nameless mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode markdown-toc magit-svn magit-gitflow macrostep lsp-haskell lorem-ipsum livid-mode live-py-mode link-hint langtool key-chord jupyter julia-mode json-navigator js2-refactor js-doc insert-shebang indent-guide impatient-mode imenu-list hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-nixos-options helm-mu helm-mode-manager helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-bibtex helm-ag haskell-snippets gruvbox-theme google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-messenger git-link gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-haskell flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu ess-R-data-view eshell-z eshell-prompt-extras esh-help esh-autosuggest erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks epresent emmet-mode elisp-slime-nav eldoc-eval editorconfig dotenv-mode dockerfile-mode docker dna-mode diminish define-word cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-shell company-quickhelp company-nixos-options company-lsp company-ghci company-cabal company-auctex company-anaconda column-enforce-mode cmm-mode clean-aindent-mode centered-cursor-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
