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
   '((python :variables python-backend 'anaconda)
     php
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (helm :variables
           helm-enable-auto-resize t
           helm-no-header t)

     (auto-completion :variables
                      ;; Set variables for auto completion key bindings
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory nil

                      ;; Show snippets in auto-completion popup
                      auto-completion-enable-snippets-in-popup t

                      ;; Enable the help tooltip for auto-completion use t for auto help
                      ;; or use 'manual for manual help(press M-h to get tooltip)
                      auto-completion-enable-help-tooltip t

                      ;; This puts most used completions first in auto completion box
                      auto-completion-enable-sort-by-usage t)

     ;; Install themes
     themes-megapack
     theming

     ;; unix password manager
     pass

     ;; Installs the games "2048-game" "Pacmacs" "Sudoku" "Tetris" "Typit"
     games
     ;; betterdefaults
     emacs-lisp
     git
     (markdown :variables
               ;;changes markdown preview engine from built in browser to vmd
               markdown-live-preview-engine 'vmd)
     neotree
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t
          ;; This adds support for twitter bootstrap
          org-enable-bootstrap-support t
          org-enable-journal-suppor t
          org-enable-hugo-support t
          ;; Tail org where to find TODO's for project files if a full path is given
          ;; then one file will hold all your project TODO's otherwise the file will
          ;; be
          org-projectile-file "TODOs.org"
          org-want-todo-bindings t
          org-enable--journal-encryption t)
     emoji

     ;; I need to go through this layer with the spacmacs manual
     html

     ;; LateX support
     latex

     ;; LateX bibliography support
     bibtex

     ;; document converter will convert to docx and alot of other stuff
     pandoc

     ;; adds poppler pdf support on doc-view mode
     pdf

     (c-c++ :variables
            c-c++-enable-clang-support t)
     (shell :variables
            shell-default-height 45
            shell-default-position 'bottom
            ;; change shell from bash with athame-readline back to regualar bash with readline
            shell-default-term-shell "/bin/bash"
            )
     (spell-checking :variables
                     ;;uncomment for autodictionary mode
                     ;;spell-checking-enable-auto-dictionary t

                     ;;uncomment to disable spellcheck by default
                     spell-checking-enable-by-default nil

                     ;;enable auto-completion popup when idle over unspelled word
                     enable-flyspell-auto-completion t)
     (latex :variables
            ;; Change latex build command here can be changed to any entity in Tex-command-list (Default: "LaTeX" == "Auctex command set")
            latex-build-command "LaTeX")
     ;; syntax-checking
     ;; version-control
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      frame-mode
                                      helm-pass
                                      password-store
                                      auth-source-pass
                                      pkgbuild-mode
                                        ;<2018-09-09 Sun>;frames-only-mode
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

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
   dotspacemacs-themes '(DarkFun
                         cyberpunk
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("TerminessTTF Nerd Font Mono Medium"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
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
   dotspacemacs-line-numbers 'relative

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

   ;; Use develop stable repository to fix org-projectile
   dotspacemacs-use-spacelpa t

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

  ;;set the default browser for viewing links in spacemacs
  (setq gnus-button-url 'browse-url-generic
        browse-url-generic-program "qutebrowser"
        browse-url-browserfunction gnus-button-url)

  ;; have i3 control the windows instead of emacs
  (defvar i3-use-frame-mode
    (s-contains? "i3" (shell-command-to-string "wmctrl -m")))

  (use-package frame-mode
    :if i3-use-frame-mode
    :demand t
    :config
    (progn
      (add-hook 'frame-mode-hook (lambda () (display-time-mode -1)))
      (frame-mode +1)
      (frame-keys-mode nil)))

  ;; Have Calendar always open in the same frame
  (push '("*Calendar*" . (display-buffer-same-window display-buffer-pop-up-window)) frame-mode-display-buffer-alist)

  ;;MAGIT
  ;;Tells Magit where the git repos are for the auto-complete feature
  (setq magit-repository-directories '("~/Programming/repos"))
  (setq magit-refresh-status-buffer nil)
  ;;uncomment line below for Magit SVN plugin
  ;;(defun dotspacemacs/user-init () (setq-defult git-enable-magit-svn-plugin t))
  ;;adds the ability to edit commits in Magit
  (global-git-commit-mode)

  ;; helm
  (use-package helm-pass)

  ;;Org

  (with-eval-after-load 'org

    ;;set the directory where all your org files will be stored.
    (setq org-directory "~/Documents/org")

    ;;set the org journal directory here
    (setq org-journal-dir "journal")
    ;; set the journal file format adding extions like .org to end of format
    ;; will break the compiler.
    (setq org-journal-file-format "%Y-%m-%d")
    (setq org-journal-date-prefix "")
    (setq org-journal-date-format "%A, %B %d %Y")
    (setq org-journal-time-prefix "* ")
    (setq org-journal-time-format "")

    ;; Set the program to open pdf files for BibTeX layer
    (setq org-ref-open-pdf-function
          (lambda (fpath)
            (start-process "zathura" "*helm-bibtex-zathura" "/usr/bin/zathura" fpath)))
    (setq org-catch-invisible-edits 'smart)

    ;; Set the org default bibliography
    (setq org-ref-default-bibliography '("~/Documents/org/References.bib")
          org-ref-pdf-direcory "~/Documents/org"
          org-ref-bibliography-notes "~/Documents/org/References.org::Bibliography")

    ;; add different bullets to org
    (setq org-bullets-bullet-list '("■" "○" "▶" "✿"))

    ;; set org TODO keyword workflow states, here is an example
    (setq org-todo-keywords
          '((sequence "TODO(t)"
                      "NEXT(n)"
                      "DELEGATED(x@!)" "|"
                      "CANCELLED(c@)"
                      "DONE(d)")

            (sequence "WAITING(w@!)"
                      "|"
                      "DISPOSED(d)")

            (sequence "SOMEDAY(s)" "|"
                      "NEVER(n)")

            (sequence "REFERENCE(r)" "|"
                      "TRASH(t)")

            (sequence "EXPIRED(e)"
                      "RESET(r)" "|"
                      "GONE")))

    ;; set the default org capture file
    (setq org-default-notes-file "~/Documents/org/Collection.org")

    ;;destroy org-capture frame after finalization
    (defadvice org-capture-finalize
        (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame"
      (if (equal "Org Select" (frame-parameter nil 'name))
          (delete-frame)))

    ;; destroy org-capture frame after kill
    (defadvice org-capture-kill
        (after delete-capture-frame activate)
      "Advise capture-kill to close the frame"
      (if (equal "Org Select" (frame-parameter nil 'name))
          (delete-frame)))

    ;; Keep org capture in single window
    (defun widen-org-capture-buffer ()
      (interactive)
      "Make the org capture buffer take up the entire frame"
      (delete-other-windows))
    (advice-add 'org-capture :after #'widen-org-capture-buffer)

    ;;Set the context tags(these tags represent where something can be done)
    (setq org-tag-alist '(("home" . ?h)
                          ("work" . ?w)
                          ("computer" . ?c)
                          ("school" . ?s)
                          ("errand" . ?e)
                          ("trip" . ?t)
                          ("vacation" . ?v)))

    ;; set your org capture templates here
    (setq org-capture-templates

          '(("t" "General task collection and generation")

            ;; General task collection not sure where to put it yet need processing
            ("tc" "Collect tasks for processing later" entry (file "Collection.org")
             "* TODO %? :%^{prompt|home|work|computer|school|errand}:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; Collect anything that takes more than two actions here.
            ("tp" "Project tasks(two actions or more)" entry (file+headline "myGTD.org" "Project")
             "* TODO %? [/] :%^{prompt|home|work|computer|school|errand}:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; Tasks with single actions
            ("ts" "Single action tasks" entry (file+headline "myGTD.org" "Tasks")
             "* TODO %? :%^{prompt|home|work|computer|school|errand}:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ("u" "URL Capture")

            ("uu" "Capture URL and Title of current webpage for reference" entry (file "References.org")
             "* REFERENCE %? :url::\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %(shell-command-to-string \"xsel -bo\")")

            ("us" "Capture URL, Title and Selection of current webpage for reference" entry (file "References.org")
             "* REFERENCE %? :url::\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %(shell-command-to-string \"xsel -bo\")\n %x")

            ("ut" "Capture URL and Title of current webpage for a \"TODO\"" entry (file "Collection.org")
             "* TODO %? :url:%^{prompt|home|work|computer|school|errand}:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %(shell-command-to-string \"xsel -bo\")")

            ("ux" "Capture URL, Title and Selection of current webpage for a \"TODO\"" entry (file "Collection.org")
             "* TODO %? :url:%^{prompt|home|work|computer|school|errand}:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %(shell-command-to-string \"xsel -bo\")\n %x")

            ("f" "Capture file links and region text")

            ("ff" "Capture a file to REFERENCE" entry (file "References.org")
             "* REFERENCE %? :file:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %a")

            ;; link file to a reference entry
            ("fs" "Capture a file+region(selection) to REFERENCE" entry (file "References.org")
             "* REFERENCE %?\n   :file:PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %i\n %a")

            ;; link file to a todo entry
            ("ft" "Capture a file TODO" entry (file "Collection.org")
             "* TODO %? :file:%^{prompt|home|work|computer|school|errand}:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %i\n %a")

            ;; link file to a todo entry
            ("fx" "Capture a file+region TODO" entry (file "Collection.org")
             "* TODO %? :file:%^{prompt|home|work|computer|school|errand}:\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:\n %i\n %a")

            ;; Holidays, Dentist, Doctor....
            ("c" "Calendar(main events)" entry (file+headline "myGTD.org" "Calendar")
             "* TODO %?\n   %^t\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; Add all fiscal due dates here e.g. taxes, credit card payments, insurance ...
            ("a" "Financial obligations(accounting))" entry (file+headline "myGTD.org" "financial")
             "* TODO %? :%^{prompt|home|work|computer|school|errand}:\n\n   SCHEDULED: %^t DEADLINE: %^t\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; Not today for whatever reason, wishes maybe here.
            ("s" "Maybe someday?" entry (file "Someday.org")
             "* SOMEDAY %?\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; Capture all those things borrowed with deadlines
            ("b" "Borrowed" entry (file+headline "myGTD.org" "Borrowed")
             "* TODO %?\n   SCHEDULED: %^t DEADLINE: %^t\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; Technical car stuff here
            ("m" "Car maintenance and repair" entry (file+headline "myGTD.org" "Car Maintenance/Repair")
             "* TODO %?\n   SCHEDULED: %^t DEADLINE: %^t\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; This is where you put things that are waiting on other people
            ("w" "Waiting on someone, *not me*" entry (file "Waiting.org")
             "* WAITING %?\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")

            ;; Capture a general reference
            ("r" "Capture a typed reference" entry (file "Reference.org")
             "* REFERENCE %?%^G\n   :PROPERTIES:\n   :CREATED:  %U\n   :EXPIRY:   +1y\n   :END:")
            ))

    ;; Set my org agenda views here.
    (setq org-agenda-custom-commands
          '(("x" "Agenda Block View"
             ((agenda)
              (tags-todo "home")
              (tags-todo "computer")
              (tags-todo "work")
              (tags-todo "errand")
              (tags-todo "school"))
             ((org-agenda-sorting-strategy '(todo-state-down))
              (org-agenda-category-filter-preset '("-Collection"
                                                   "-References"
                                                   "-Someday"
                                                   "-Waiting"))))

            ("y" "Appointments" agenda*)

            ("c" "Computer todos"
             tags-todo "computer"
             ((org-agenda-sorting-strategy '(todo-state-down))
              (org-agenda-category-filter-preset '("-Collection"
                                                   "-References"
                                                   "-Someday"
                                                   "-Waiting"))))

            ("n" "NEXT todos"
             todo "NEXT"
             ((org-agenda-category-filter-preset '("-Collection"
                                                   "-References"
                                                   "-Someday"
                                                   "-Waiting"))))

            ("h" "Home todos"
             tags-todo "home"
             ((org-agenda-sorting-strategy '(todo-state-down))
              (org-agenda-category-filter-preset '("-Collection"
                                                   "-References"
                                                   "-Someday"
                                                   "-Waiting"))))

            ("w" "Work todos"
             tags-todo "work"
             ((org-agenda-sorting-strategy '(todo-state-down))
              (org-agenda-category-filter-preset '("-Collection"
                                                   "-References"
                                                   "-Someday"
                                                   "-Waiting"))))

            ("e" "Errand todos"
             tags-todo "errand"
             ((org-agenda-sorting-strategy '(todo-state-down))
              (org-agenda-category-filter-preset '("-Collection"
                                                   "-References"
                                                   "-Someday"
                                                   "-Waiting"))))

            ("s" "School todos"
             tags-todo "school"
             ((org-agenda-sorting-strategy '(todo-state-down))
              (org-agenda-category-filter-preset '("-Collection"
                                                   "-References"
                                                   "-Someday"
                                                   "-Waiting"))))

            ("i" "Collection"
             todo "TODO"
             ((org-agenda-category-filter-preset '("+Collection"))))

            ("r" "References"
             todo "REFERENCE"
             ((org-agenda-category-filter-preset '("+References"))))

            ("o" "Someday"
             todo "SOMEDAY"
             ((org-agenda-category-filter-preset '("+Someday"))))

            ("v" "Waiting"
             todo "WAITING"
             ((org-agenda-category-filter-preset '("+Waiting"))))))

    ;; Change todo to done when all of it's children are finished
    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log_done org-log-states) ;turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    ;; this is the hook that makes the todo change from the above.
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

    ;; set to nil to make org statistics cookie count todo entries in subtrees not
    ;; just direct children.
    (setq org-hierarchical-todo-statistics nil)

    ;;set the defautlt location for agenda files
    (setq org-agenda-files '("~/Documents/org/myGTD.org"
                             "~/Documents/org/Collection.org"
                             "~/Documents/org/Read.org"
                             "~/Documents/org/Someday.org"
                             "~/Documents/org/References.org"))

    ;; Projectile configuration
    (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (mapcar '(lambda (file)
               (when (file-exists-p file)
                 (push file org-agenda-files)))
            (org-projectile-todo-files)))

    (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)

    ;; set the default location to store done tasks
    ;; (setq org-archive-location "archive.org::")

    ;; Don't erase all the meta info in the todo keep todo state and local tags
    (setq org-archive-save-context-info '(todo ltags))

    ;; Allow subtrees with tag archive to be cycled normally
    ;; do this so only headings in the archive file are officially "archived".
    ;; Use :ARCHIVE: to find old forgotten headings mark with org-expiry
    (setq org-cycle-open-archived-trees t)
    (setq org-sparse-tree-open-archived-trees t)
    (setq org-agenda-skip-archived-trees nil)
    (setq org-columns-skip-archived-trees nil)
    (setq org-export-with-archived-trees t)

    ;; This function moves all "DONE" "SOMEDAY" "WAITING" "TODO"s in agenda files to corresponding org files
    (defun my/org-archive-multi-tasks ()
      (interactive)
      (loop for (lowercase-todo-type  uppercase-todo-type) in '(("Trash" "/GONE")
                                                                ("Trash" "/TRASH")
                                                                ("Trash" "/DISPOSED")
                                                                ("Trash" "/CANCELLED")
                                                                ("Trash" "/WAITING")
                                                                ("Delegated" "/DELEGATED"))

            ;; Create string with variables to set org-archive-location
            do (setq org-archive-location (concat lowercase-todo-type ".org::"))
            (setq org-collection-file-list '("~/Documents/org/myGTD.org"
                                             "~/Documents/org/Collection.org"
                                             "~/Documents/org/References.org"
                                             "~/Documents/org/Someday.org"))

            (org-map-entries
             (lambda ()
               (org-archive-subtree)
               (setq org-map-continue-from (outline-previous-heading)))
             uppercase-todo-type org-collection-file-list))

      (setq org-archive-location '%s_archive::))

                                        ;(defun my/org-archive-done-tasks ()
                                        ;  (interactive)
                                        ;  (setq org-archive-save-context-info (todo ltags))
                                        ;  (setq org-archive-location "archive.org")
                                        ;  (org-map-entries
                                        ;   (lambda ()
                                        ;     (org-archive-subtree)
                                        ;     (setq org-map-continue-from (outline-previous-heading)))
                                        ;   "/DONE" 'agenda))



    ;; set autofill in org-mode for word processor like wordwrap functionality.
    (add-hook 'org-mode-hook
              (lambda ()
                ;; Enable fill column indicator
                (fci-mode t)
                ;; Set fill column to whatever you want
                (setq fill-column 80)
                ;;enable automatic line wrapping at fill column
                (auto-fill-mode t)
                (flyspell-mode 1)
                )
              )
    ;; Set up org-refile targets here
    (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))

    ;; Set to true to allow org-refile to create new nodes as new parents in org files.
    (setq org-refile-allow-creating-parent-nodes t)
    (setq org-refile-use-outline-path 'file)

    ;;Org-crypt (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;; Todo I need to set this key up and pinentry
    (setq org-crypt-key "emacs-journal")
    ;; GPG key to use for encryption
    ;; Either the key id or set to nil to use symmetric encryption
    ;; Attention must add:
    ;; # -*- buffer-auto-save-file-name: nil; -*-
    ;; to top of org files with encryption or youll get an
    ;; annoying message. You could also turn off auto save globally like this:
    ;; (setq auto-save-default nil)

    (defun my/org-expiry-delete-archive-reset ()
      (interactive "c/Delete, Archive, Reset:"
                   (cond ((char-equal ?D my-expiry-choice)
                          '(org-cut-subtree))
                         ((char-equal ?d my-expiry-choice)
                          '(org-cut-subtree))
                         ((char-equal ?A my-expiry-choice)
                          '(org-archive-subtree))
                         ((char-equal ?a my-expiry-choice)
                          '(org-archive-subtree))
                         ((char-equal ?R my-expiry-choice)
                          '(org-expiry-insert-created))
                         ((char-equal ?r my-expiry-choice)
                          '(org-expiry-insert-created &optional t))
                         (t (message "Unrecognized option bad char!")))))

    ;; Set the function to run when calling org-expiry
    (setq org-expiry-handler-function 'org-expiry-add-keyword)

    ;; "create" help r function for my/org-expiry-add-timestamp-1y-expiry ()
    (defun my/org-expiry-helper-insert-create ()
      (interactive)
      (save-excursion
        (org-back-to-heading)
        (org-expiry-insert-created)))

    ;; "insert" helper function for my/org-expiry-add-timestamp-1y-expiry ()
    (defun my/org-expiry-helper-insert-expiry ()
      (save-excursion
      (org-entry-put
       (point) org-expiry-expiry-property-name timestr)))

    ;; Add expiry mechanism to emacs items will expire in one year at which time
    ;; you may check for expired entries with M-x org-expiry-process-entries.
    (defun my/org-expiry-add-timestamp-1y-expiry ()
      (interactive)
      (save-excursion
      (when (string= (org-get-todo-state) "TODO")
        (let* ((d (org-entry-get (point) org-expiry-expiry-property-name)))
          (setq timestr "1y")
          (my/org-expiry-helper-insert-create)
          (my/org-expiry-helper-insert-expiry)
          ))))

    ;; Set timestamps inactive so everything doesn't show up in the agenda
    (setq org-expiry-inactive-timestamps t)

    (add-hook 'org-after-todo-state-change-hook 'my/org-expiry-add-timestamp-1y-expiry)

    ;; Add hook to insert timestamp when todo state is changed the heading is set to "TODO"

    ;; Make org treat isert of todo as state change
    ;; so that org-expire will automatically insert
    ;; a "CREATED" "EXPIRED" tag.
    (setq org-treat-insert-todo-heading-as-state-change t)

    ;; Set the amount of days org should start warning you before a deadline
    (setq org-deadline-warning-days 2)

    ;; Don't show scheduled items that are already "DONE".
    (setq org-agenda-skip-scheduled-if-done t)
    )

  ;; auto-completion
  ;; make company the global auto-completion plugin
  (global-company-mode)

  ;;configures nicer looking faces for auto-completion
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;;set the delay of autocompletion here 0 means instant
  (setq company-idle-delay 0)

  ;;Arch linux
  ;;Add a pkgbuild mode for Arch linux packages
  (use-package pkgbuild-mode)

  ;;Latex
  ;;Auto update pdf preview when file recompiled
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; Theming set backgound theme so terminal mode is transparent
  (defun set-background-for-terminal (&optional frame)
    (or frame (setq frame (selected-frame)))
    "unsets the background color in terminal mode"
    (if (display-graphic-p frame)
        (spacemacs/enable-transparency) (set-face-background 'default "unspecified-bg" frame)))
  (add-hook 'after-make-frame-functions 'set-background-for-terminal)
  (add-hook 'window-setup-hook 'set-background-for-terminal)

  ;; fix for pdf-tools auto revert specified by author https://github.com/politza/pdf-tools#known-problems
  (add-hook 'Tex-after-compilation-finished-function #'Tex-revert-document-buffer)

  ;; open buffers in emacs automatically
  (find-file-noselect "~/Documents/org/myGTD.org")
  (find-file-noselect "~/Documents/org/Collection.org")
  (find-file-noselect "~/Documents/org/References.org")
  (find-file-noselect "~/Documents/org/Read.org")
  (find-file-noselect "~/Documents/org/Delegated.org")
  (find-file-noselect "~/Documents/org/Waiting.org")
  (find-file-noselect "~/Documents/org/Someday.org")

  ;; Set defaul browser for emacs to call
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "qutebrowser")

  ;; Spaceline configuration
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (php-extras zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme helm-pass vmd-mode mmm-mode markdown-toc gh-md company-auctex auctex drupal-mode phpunit phpcbf php-auto-yasnippets php-mode pkgbuild-mode typit mmt sudoku pacmacs dash-functional 2048-game flyspell-popup company-quickhelp auth-source-pass password-store smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magithub markdown-mode ghub+ magit magit-popup git-commit apiwrap ghub let-alist with-editor web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data flycheck-pos-tip pos-tip flycheck flyspell-correct-helm flyspell-correct auto-dictionary ox-gfm org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot insert-shebang fish-mode company-shell xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help helm-company helm-c-yasnippet fuzzy disaster company-statistics company-c-headers company cmake-mode clang-format auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (yasnippet-snippets yapfify web-beautify symon string-inflection spaceline-all-the-icons pyvenv pytest pyenv-mode py-isort pippel pipenv pip-requirements password-generator pandoc-mode ox-twbs ox-reveal ox-pandoc ox-hugo overseer org-ref pdf-tools key-chord tablist org-brain nameless magit-svn live-py-mode kaolin-themes importmagic epc ctable concurrent deferred impatient-mode simple-httpd helm-xref helm-rtags helm-pydoc helm-purpose window-purpose imenu-list helm-bibtex parsebib google-c-style gitignore-templates frame-mode eziam-theme evil-org evil-lion evil-goggles evil-cleverparens paredit emojify ht emoji-cheat-sheet-plus editorconfig doom-themes all-the-icons memoize cython-mode counsel-projectile counsel swiper ivy company-rtags rtags company-php ac-php-core xcscope company-emoji company-anaconda centered-cursor-mode biblio biblio-core anaconda-mode pythonic font-lock+ dotenv-mode php-extras zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme helm-pass vmd-mode mmm-mode markdown-toc gh-md company-auctex auctex drupal-mode phpunit phpcbf php-auto-yasnippets php-mode pkgbuild-mode typit mmt sudoku pacmacs dash-functional 2048-game flyspell-popup company-quickhelp auth-source-pass password-store smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magithub markdown-mode ghub+ magit magit-popup git-commit apiwrap ghub let-alist with-editor web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data flycheck-pos-tip pos-tip flycheck flyspell-correct-helm flyspell-correct auto-dictionary ox-gfm org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot insert-shebang fish-mode company-shell xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help helm-company helm-c-yasnippet fuzzy disaster company-statistics company-c-headers company cmake-mode clang-format auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
)
