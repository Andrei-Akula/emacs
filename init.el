;; -*- coding: utf-8; lexical-binding: t -*-
;; Mostly based on Protesilaos' configuration https://protesilaos.com/emacs/dotemacs

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Apply only in Graphic mode
(when (display-graphic-p)
  ;; Every Emacs window should, by default occupy all the screen space it can
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(width . (text-pixels . 1200)))
  (add-to-list 'default-frame-alist '(height . (text-pixels . 900)))

  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'initial-frame-alist '(width . (text-pixels . 1200)))
  (add-to-list 'initial-frame-alist '(height . (text-pixels . 900)))

  ;; Display visited file's path in the frame title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  )

;; always start with the *scratch* buffer in Graphic
(when (display-graphic-p)
  (setq initial-buffer-choice t)
  (setq initial-major-mode 'lisp-interaction-mode)
  (setq initial-scratch-message
        (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
                'lisp-interaction-mode
                (propertize
                 (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
                 'face 'help-key-binding))))

;; Packages
;; Define and initialise package repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(setq package-install-upgrade-built-in t)

(require 'package)
;; (package-initialize)

;; use-package to simplify the config file - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; (setq use-package-always-ensure 't)
(setq package-native-compile t)
(eval-when-compile
  (require 'use-package))

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; MacOS
(when (eq system-type 'darwin)
  ;; Standard macOS conventions would have s-w close the current buffer, not the whole window.
  (bind-key "s-w" #'kill-current-buffer)
  ;;(setq ns-auto-hide-menu-bar t)
  )

;; MacOS
(use-package exec-path-from-shell
  :ensure t
  :if (and (display-graphic-p) (eq system-type 'darwin))
  :init
  (exec-path-from-shell-initialize))



(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)


;;; Essential configurations
(use-package emacs
  :ensure nil
  :demand t
  :config
;;;; General settings and common custom functions
  (setq delete-pair-blink-delay 0.1) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq help-window-select t)
  (setq next-error-recenter '(4)) ; center of the window
  (setq next-error-message-highlight t)
  (setq find-library-include-other-files nil) ; Emacs 29
  ;; (setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
  ;; (setq remote-file-name-inhibit-auto-save t)                 ; Emacs 30
  (setq tramp-connection-timeout (* 60 10)) ; seconds
  (setq save-interprogram-paste-before-kill t)
  (setq mode-require-final-newline t)
  (setq-default truncate-partial-width-windows nil)
  (setq-default truncate-lines t)
  (setq eval-expression-print-length nil)
  (setq kill-do-not-save-duplicates t)
  (setq duplicate-line-final-position -1 ; both are Emacs 29
        duplicate-region-final-position -1)
  (setq scroll-error-top-bottom t)
  (setq mark-even-if-inactive nil)
  (setq fast-but-imprecise-scrolling t)
  (setq load-prefer-newer t)
  ;; (setq echo-keystrokes-help nil) ; Emacs 30
  ;; (setq epa-keys-select-method 'minibuffer) ; Emacs 30

  ;; Keys I unbind here are either to avoid accidents or to bind them
  ;; elsewhere later in the configuration.
  :bind
  ( :map global-map
    ("<insert>" . nil)
    ("C-z" . nil) ; I have a window manager, thanks!
    ("C-x C-z" . nil) ; same idea as above
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-x C-c C-c" . save-buffers-kill-emacs) ; more cumbersome, less error-prone
    ("C-h h" . nil) ; Never show that "hello" file
    ("C-S-d" . duplicate-dwim) ; Duplicate the current line or region N times.
    ("C-x K" . kill-current-buffer)
    ("M-SPC" . cycle-spacing)
    ("M-z" . zap-up-to-char) ; NOT `zap-to-char'
    ("M-c" . capitalize-dwim)
    ("M-l" . downcase-dwim) ; "lower" case
    ("M-u" . upcase-dwim)
    ("C-x O" . next-multiframe-window)
    ("C-h K" . describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    ("C-h u" . apropos-user-option)
    ("C-h F" . apropos-function) ; lower case is `describe-function'
    ("C-h V" . apropos-variable) ; lower case is `describe-variable'
    ("C-h L" . apropos-library) ; lower case is `view-lossage'
    ("C-h c" . describe-char) ; overrides `describe-key-briefly'

    ;; :map prog-mode-map
    ;; ("<C-M-backspace>" . backward-kill-sexp)

    ;; Keymap for buffers (Emacs28)
    :map ctl-x-x-map
    ("f" . follow-mode)  ; override `font-lock-update'
    ("l" . visual-line-mode)))



(when (display-graphic-p)
  ;;;; Fonts
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Menlo-12")
    (set-face-attribute 'variable-pitch nil :font "PT Mono-12"))

  ;;; Font on Windows
  (when (memq system-type '(windows-nt ms-dos))
    (set-face-attribute 'default nil :font "Consolas-10")))

;;;; Themes
(setq custom-safe-themes t)

;;;; ef-themes
(use-package ef-themes
    :ensure t
    :if (display-graphic-p)
    :config
    ;; Add all your customizations prior to loading the themes
    (setq ef-themes-to-toggle '(ef-light ef-owl)
          ;; ef-themes-variable-pitch-ui t
          ;; ef-themes-mixed-fonts t
          )

    ;; Disable all other themes to avoid awkward blending:
    (mapc #'disable-theme custom-enabled-themes)

    ;; Load the theme of choice:
    (load-theme 'ef-light :no-confirm)

    (define-key global-map (kbd "<f5>") #'ef-themes-toggle))


;;;; Plain text (text-mode)
(use-package text-mode
  :ensure nil
  ;; The file names specified in that regular expression will be using text-mode when I visit them
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"

  ;; I still need to use double spaces for Elisp programming, otherwise the byte compiler produces
  ;; warnings. It is annoyingly pedantic, but here we are…

  :hook
  ((emacs-lisp-mode . (lambda () (setq-local sentence-end-double-space t))))

  :config
  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)
  (setq-default fill-column 100))


;;;; Mouse and mouse wheel behaviour
(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  ;; Some of these variables are defined in places other than
  ;; mouse.el, but this is fine.

  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale))
        mouse-drag-copy-region nil
        make-pointer-invisible t
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)

  (setq mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t)

  ;; Scrolling behaviour
  (setq-default scroll-preserve-screen-position t
                scroll-conservatively 1 ; affects `scroll-step'
                scroll-margin 0
                next-screen-context-lines 0))


;; This package maintains a list of recently opened files and makes it easy to visit them.  The
;; recent files list is automatically saved across Emacs sessions.

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
  (setq recentf-save-file-modes nil)
  ;; (setq recentf-keep nil)
  (setq recentf-auto-cleanup "11:00am")
  ;; (setq recentf-initialize-file-name-history nil)
  ;; (setq recentf-filename-handlers nil)
  ;; (setq recentf-show-file-shortcuts-flag nil)
  :bind ("C-x C-r" . recentf-open))


;; Bookmarks are compartments that store arbitrary information about a file or buffer. The records
;; are used to recreate that file/buffer inside of Emacs. Put differently, we can easily jump back
;; to a file or directory (or anything that has a bookmark recorder+handler, really). Use the
;; bookmark-set command (C-x r m) to record a bookmark and then visit one of your
;; bookmarks with bookmark-jump (C-x r b). Display a list of existing bookmark (C-x r l)

;;;; Built-in bookmarking framework (bookmark.el)
(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  ;; (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon

  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))



;; A common use-case is to write some text to a register and then insert that text by calling the
;; given register. This is much better than relying on the kill-ring, because registers are meant to
;; be overwritten by the user, whereas the kill-ring accumulates lots of text that we do not
;; necessarily need.

;; To me, registers are essential for keyboard macros. By default, registers do not persist between
;; Emacs sessions, though I do need to re-use them from time to time, hence the arrangement to
;; record them with savehist-mode

;;;; Registers (register.el)
(use-package register
  :ensure nil
  :defer t ; its commands are autoloaded, so this will be loaded then
  :config
  (setq register-preview-delay 0.8)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))


;; The “auto revert” facility makes Emacs update the contents of a saved buffer when its underlying
;; file is changed externally. This can happen, for example, when a git pull modifies the file we
;; are already displaying in a buffer. Emacs thus automatically reverts the buffer to reflect the
;; new file contents.

;;;; Auto revert mode
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))


;; Every graphical application I have ever used will delete the selected text upon the insertion of
;; new text. Emacs does not do this by default. With delete-selection-mode we get it.

;;;; Delete selection
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


;; With these settings in place, Emacs will use its own faces and frame infrastructure to display
;; tooltips. I prefer it this way because then we can benefit from the text properties that can be
;; added to these messages (e.g. a different colour or a slant).

;;;; Tooltips (tooltip-mode)
(use-package tooltip
  :ensure nil
  :if (display-graphic-p)
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))


;; With M-x world-clock we get a buffer with all cities and concomitant time zones specified in
;; zoneinfo-style-world-list. The contents are displayed according to the world-clock-time-format

;;;; World clock (M-x world-clock)
(use-package time
  :ensure nil
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("Canada/Pacific" "Canada/Pacific")
          ("America/Chicago" "Chicago")
          ("Brazil/Acre" "Rio Branco")
          ("America/Toronto" "Toronto")
          ("America/New_York" "New York")
          ("Canada/Atlantic" "Canada/Atlantic")
          ("Brazil/East" "Brasília")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tehran" "Tehran")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Yekaterinburg" "Yekaterinburg")
          ("Asia/Kolkata" "Kolkata")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Asia/Vladivostok" "Vladivostok")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z (%Z)	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))



;; Most buffers conform with rules we define in the display-buffer-alist (The prot-emacs-window.el
;; module). However, M-x man does not do this because it has its own behaviour. At least, it is
;; customisable. The Man-notify-method is a very old option, according to what the Help buffer is
;; telling me (check its documentation with C-h v or M-x describe-variable), so I suspect this was
;; never updated to conform with the newer display-buffer-alist…

;;;; `man' (manpages)
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'



;; The M-x proced command produces a listing of all running processes on the system. This is like
;; the top program on the command-line. While inside the *Proced* buffer, type C-h m (M-x
;; describe-mode) to learn about keys/commands you can use therein.

;;;; `proced' (process monitor, similar to `top')
(use-package proced
  :ensure nil
  :commands (proced)
  :config
  ;; (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))


;; The goto-chg package, authored by David Andersson and maintained by Vasilij Schneidermann, moves
;; the cursor to the point where the last change happened. Calling the command again cycles to the
;; point before that and so on. Simple and super effective.
(use-package goto-chg
  :ensure t
  :bind
  (("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))



;;; Shell (M-x shell)
(use-package shell
  :ensure nil
  :if (display-graphic-p)
  :bind
  ( ;; :map global-map
    ;; ("<f1>" . shell)
    :map shell-mode-map
    ("C-c C-k" . comint-clear-buffer)
    ("C-c C-w" . comint-write-output))
  :config
  ;; Check my .bashrc which handles `comint-terminfo-terminal':
  ;;
  ;; # Default pager.  The check for the terminal is useful for Emacs with
  ;; # M-x shell (which is how I usually interact with bash these days).
  ;; #
  ;; # The COLORTERM is documented in (info "(emacs) General Variables").
  ;; # I found the reference to `dumb-emacs-ansi' in (info "(emacs)
  ;; # Connection Variables").
  ;; if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
  ;; then
  ;;     export PAGER="cat"
  ;;     alias less="cat"
  ;;     export TERM=dumb-emacs-ansi
  ;;     export COLORTERM=1
  ;; else
  ;;     # Quit once you try to scroll past the end of the file.
  ;;     export PAGER="less --quit-at-eof"
  ;; fi

  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t) ; Emacs 29.1
  (setq shell-has-auto-cd nil) ; Emacs 29.1
  ;; (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
  (setq shell-completion-fignore '("~" "#" "%"))
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input)
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)

  (setq tramp-default-remote-shell "/bin/bash")
  (when (eq system-type 'darwin)
    (setq tramp-default-remote-shell "/bin/zsh"))

  (setq shell-font-lock-keywords
        '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
          ("^[^ \t\n]+:.*" . font-lock-string-face)
          ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

  ;; Support for OS-specific escape sequences such as what `ls
  ;; --hyperlink' uses.  I normally don't use those, but I am checking
  ;; this to see if there are any obvious advantages/disadvantages.
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output))



;; https://protesilaos.com/emacs/dotemacs#h:14b09958-279e-4069-81e3-5a16c9b69892

;;; General minibuffer settings
(use-package minibuffer
  :ensure nil
  :config
;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  (setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override everything.
  (setq completion-category-defaults nil)

  ;; A non-exhaustve list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  ;;
  (setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))


;;  It is a powerful pattern matching algorithm that parses user input and interprets it
;;  out-of-order, so that in pa will cover insert-pair as well as package-install. Components of the
;;  search are space-separated, by default, though we can modify the user option
;;  orderless-component-separator to have something else (but I cannot think of a better value). In
;;  the section about completion styles, I explain how I use orderless and why its power does not
;;  result in lots of false positives.

;;; Orderless completion style (and prot-orderless.el)
(use-package orderless
  :ensure t
  :demand t
  :after minibuffer
  :config
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ;; ("?" . nil)
          ))

;; settings to ignore letter casing
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)


;; Minibuffer prompts often have a default value. This is used when the user types RET without
;; inputting anything. The out-of-the-box behaviour of Emacs is to append informative text to the
;; prompt like (default some-default-value). With the tweak to minibuffer-default-prompt-format we
;; get a more compact style of [some-default-value]

(use-package minibuf-eldef
  :ensure nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29


;;;; `savehist' (minibuffer and related histories)
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  ;; (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  ;; (add-to-list 'savehist-additional-variables 'kill-ring)
  )



;; The built-in dabbrev package provides a text completion method that reads the contents of a
;; buffer and expands the text before the cursor to match possible candidates.
;; This is done with M-/ (dabbrev-expand)

(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand dabbrev-completion)
  :config
;;;; `dabbrev' (dynamic word completion (dynamic abbreviations))
  ;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode pdf-view-mode)))




;; Corfu (in-buffer completion popup)
;; Completion is triggered with the TAB key, which produces a popup where the cursor is. The
;; companion corfu-popupinfo-mode will show a secondary documentation popup if we move over a
;; candidate but do not do anything with it.
;; https://github.com/minad/corfu

(use-package corfu
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  ;; :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-min-width 20)
  ;; (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (setq corfu-auto t)                 ;; Enable auto completion
  ;; (setq corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (setq corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (setq corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (setq corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (setq corfu-on-exact-match nil)     ;; Configure handling of exact matches

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  ;; (with-eval-after-load 'savehist
  ;;   (corfu-history-mode 1)
  ;;   (add-to-list 'savehist-additional-variables 'corfu-history))
  )


;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  ;; (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))



;; The vertico package by Daniel Mendler displays the minibuffer in a vertical layout. Under the
;; hood, it takes care to be responsive and to handle even massive completion tables gracefully.

;;; Vertical completion layout (vertico)
(use-package vertico
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize t)
  (setq vertico-cycle t)

  (with-eval-after-load 'rfn-eshadow
    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))



;;; Enhanced minibuffer commands (consult.el)
;;; https://github.com/minad/consult
(use-package consult
  :ensure t
  :if (display-graphic-p)
  :bind
  ( :map global-map
    ("M-s d" . consult-find)
    ("M-s g" . consult-grep)
    ("M-s G" . consult-git-grep)
    ("M-s r" . consult-ripgrep)
    :map consult-narrow-map
    ("?" . consult-narrow-help)
    )
  :config
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key nil)
  ;; (setq consult-find-args
  ;;       (concat "find . -not ( "
  ;;               "-path */.git* -prune "
  ;;               "-or -path */.cache* -prune )"))

  ;; (setq consult-project-function nil) ; always work from the current directory (use `cd' to switch directory)

  ;; (require 'consult-imenu) ; the `imenu' extension is in its own file
  )


;;;; recentf
(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
    (message "Opening file...")
    (message "Aborting")))

;; use IDO in console
(unless (display-graphic-p)
  (require 'ido)
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t)
  (global-set-key (kbd "C-x C-r") 'ido-recentf-open))



;; The marginalia package, co-authored by Daniel Mendler and Omar Antolín Camarena, provides helpful
;; annotations to the side of completion candidates.  Annotations are provided on a per-category
;; basis. Categories are metadata associated with the completion table, which describe what the
;; candidates are about.

;;; Detailed completion annotations (marginalia.el)
(use-package marginalia
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . marginalia-mode)
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-max-relative-age 0)) ; absolute time


;;
;; Isearch
;;
;; https://protesilaos.com/emacs/dotemacs#h:e0f9c30e-3a98-4479-b709-7008277749e4
;; https://protesilaos.com/codelog/2023-06-10-emacs-search-replace-basics/
;;
(use-package isearch
  :ensure nil
  :demand t
  :config
  ;; isearch lax space
  (setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
        isearch-lax-whitespace t       ; M-s SPC toggle lax-whitespace searching
        isearch-regexp-lax-whitespace nil)
  ;; match counter shows the position of the current match relative to the total count (like 5/20)
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  ;; when we are repeating an isearch in the opposite direction move directly to the next/previous match
  (setq isearch-repeat-on-direction-change t)
  ;; tweaks for the occur buffer
  (setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
  (add-hook 'occur-mode-hook #'hl-line-mode))


;;; grep and xref
(use-package re-builder
  :ensure nil
  :commands (re-builder regexp-builder)
  :config
  (setq reb-re-syntax 'read))

(use-package xref
  :ensure nil
  :commands (xref-find-definitions xref-go-back)
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative))

(use-package grep
  :ensure nil
  :commands (grep lgrep rgrep)
  :config
  (setq grep-save-buffers nil)
  ;; (setq grep-use-headings t) ; Emacs 30

  (let ((executable (or (executable-find "rg") "grep"))
        (rgp (string-match-p "rg" grep-program)))
    (setq grep-program executable)
    (setq grep-template
          (if rgp
              "/usr/bin/rg -nH --null -e <R> <F>"
            "/usr/bin/grep <X> <C> -nH --null -e <R> <F>"))
    (setq xref-search-program (if rgp 'ripgrep 'grep))))


;;; wgrep (writable grep)
;; See the `grep-edit-mode' for the new built-in feature.
(unless (>= emacs-major-version 31)
  (use-package wgrep
    :ensure t
    :after grep
    :bind
    ( :map grep-mode-map
      ("e" . wgrep-change-to-wgrep-mode)
      ("C-x C-q" . wgrep-change-to-wgrep-mode)
      ("C-c C-c" . wgrep-finish-edit))
    :config
    (setq wgrep-auto-save-buffer t)
    (setq wgrep-change-readonly-file t)))


;;;; Handle performance for very long lines (so-long.el)
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))


;;;;
;;;; Dired
;;;;

;;; Dired file manager
(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  ;; when two Dired buffers are open side-by-side we get the other
  ;; buffer as the default target of the current rename or copy operation
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  ;; (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1
  (setq dired-mark-region t)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-aux
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("C-+" . dired-create-empty-file)
    ;; ("M-s f" . nil)
    )
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t)) ; Emacs 29

(use-package dired-x
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("I" . dired-do-info))
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil))



(when (eq system-type 'darwin)
    (setq dired-listing-switches "-AGFhlv"))

;; Visuals
(when (display-graphic-p)
  ;; Icons
  (unless (memq system-type '(windows-nt ms-dos))
    (let ((installed (package-installed-p 'all-the-icons)))
      (use-package all-the-icons
        :ensure t)
      (unless installed (all-the-icons-install-fonts)))
    ;; Icons in dired
    (use-package all-the-icons-dired
      :ensure t
      :after all-the-icons
      :hook (dired-mode . all-the-icons-dired-mode)))

  )


;;; General window and buffer configurations
(use-package uniquify
  :ensure nil
  :config
  ;;;; `uniquify' (unique names for buffers)
  ;; forward style, which is the closest to the actual file name.
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))


;;;; Line highlight
(use-package hl-line
  :ensure nil
  :commands (hl-line-mode)
  :config
  ;; The nil value for hl-line-sticky-flag makes the line highlight not show up in unfocused windows.
  (setq hl-line-sticky-flag nil))


(column-number-mode)

;;; Line numbers on the side of the window
(use-package display-line-numbers
  :ensure nil
  ;; use display-line-numbers-mode only in prog mode
  :hook (prog-mode . display-line-numbers-mode)
  ;; :bind
  ;; ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t))

;; (global-display-line-numbers-mode t)


;;; Directional window motions (windmove)
(use-package windmove
  :ensure nil
  :bind
  (:map window-prefix-map ; C-x w
   ("<up>" . windmove-up)
   ("<right>" . windmove-right)
   ("<down>" . windmove-down)
   ("<left>" . windmove-left))
  :config
  (setq windmove-create-window nil)) ; Emacs 27.1


;;;;
;;;; projects
;;;;

;;;; `ediff'
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))


;;;; `diff-mode'
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (setq diff-default-read-only t) ; C-x C-q Toggle read-only status of buffer
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  ;; (setq diff-refine nil)
  ;; (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax 'hunk-also))


;;; Version control framework (vc.el, vc-git.el, and more)
(use-package vc
  :ensure nil
  :bind
  (;; NOTE: I override lots of the defaults
   :map global-map
   ("C-x v B" . vc-annotate) ; Blame mnemonic
   ("C-x v e" . vc-ediff)
   ("C-x v k" . vc-delete-file) ; 'k' for kill==>delete is more common
   ("C-x v R" . vc-log-search)  ; git log --grep
   ("C-x v t" . vc-create-tag)
   ("C-x v d" . vc-diff)
   ("C-x v ." . vc-dir-root) ; `vc-dir-root' is from Emacs 28
   ("C-x v <return>" . vc-dir-root)
   :map vc-dir-mode-map
   ("t" . vc-create-tag)
   ("O" . vc-log-outgoing)
   ("o" . vc-dir-find-file-other-window)
   ("d" . vc-diff)         ; parallel to D: `vc-root-diff'
   ("k" . vc-dir-delete-file)
   ("/" . vc-revert)
   :map vc-git-stash-shared-map
   ("a" . vc-git-stash-apply-at-point)
   ("c" . vc-git-stash) ; "create" named stash
   ("k" . vc-git-stash-delete-at-point) ; symmetry with `vc-dir-delete-file'
   ("p" . vc-git-stash-pop-at-point)
   ("s" . vc-git-stash-snapshot)
   ;; :map vc-annotate-mode-map
   ;; ("M-q" . vc-annotate-toggle-annotation-visibility)
   ;; ("C-c C-c" . vc-annotate-goto-line)
   ;; ("<return>" . vc-annotate-find-revision-at-line)
   ;; :map log-edit-mode-map
   ;; ("M-s" . nil) ; I use M-s for my search commands
   ;; ("M-r" . nil) ; I use `consult-history'
   :map log-view-mode-map
   ("s" . vc-log-search)
   ("O" . vc-log-outgoing)
   ("I" . vc-log-incoming)
   ("+" . vc-update)
   ("P" . vc-push))
  :init
  (setq vc-follow-symlinks t)
  :config
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; I only use Git.  If I ever need another, I will include it here.
  ;; This may have an effect on performance, as Emacs will not try to
  ;; check for a bunch of backends.
  (setq vc-handled-backends '(Git))

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)
  ;; I can see the files from the Diff with C-c C-d
  ;; (remove-hook 'log-edit-hook #'log-edit-show-files)

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  ;; (setq vc-git-log-switches '("--stat"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        `("%d %h %ai %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  ;; These two are from Emacs 29
  (setq vc-git-log-edit-summary-target-len 50)
  (setq vc-git-log-edit-summary-max-len 70))



;;;; `project'
(use-package project
  :ensure nil
  :bind
  (("C-x p ." . project-dired)
   ("C-x p C-g" . keyboard-quit)
   ("C-x p <return>" . project-dired)
   ("C-x p <delete>" . project-forget-project))
  :config
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project")) ; Emacs 29
  )




;;
;; Programming languages
;;
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default js-switch-indent-offset 2)

(use-package typescript-mode
  :ensure t
  :custom (typescript-indent-level 2))

(use-package js2-mode
  :if (display-graphic-p)
  :hook (js2-mode . js2-imenu-extras-mode)
  :hook (js-mode . js2-minor-mode)
  :mode ("\\.js$" . js2-mode)
  :interpreter ("node" . js2-mode)
  :ensure t
  :custom
  (js2-mode-assume-strict t)
  (js2-warn-about-unused-function-arguments t)
  (js2-dynamic-idle-timer-adjust 5000))

(use-package xref-js2
  :if (display-graphic-p)
  :ensure t
  :hook (js2-mode . pt/js-hook)
  :custom
  (xref-js2-search-program 'rg)
  :config
  (defun pt/js-hook ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package web-mode
  :ensure t
  :if (display-graphic-p)
  :custom (web-mode-markup-indent-offset 2))


;;;; eglot

;; npm install -g typescript-language-server typescript
;; npm i -g vscode-langservers-extracted

;; add it to PATH var, e.g. export PATH=$PATH:/Users/Andrei_Akula/.nvm/versions/node/v20.12.0/bin/

;;;; Eglot (built-in client for the language server protocol)
(use-package eglot
  :if (display-graphic-p)
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :hook (
         (js-mode . eglot-ensure)
         ;; (js2-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :bind (:map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l r" . eglot-rename)
	      ("C-c l h" . eldoc)
	      ("C-c l f" . eglot-format)
	      ("C-c l F" . eglot-format-buffer)
	      ;; sometimes ionide acts up
	      ("C-c l R" . eglot-reconnect))
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))



;; *** end of refactoring ***



;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes a buffer-local variable when set.
(setq-default indent-tabs-mode nil)



;; treatment of whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)


(defun pt/indent-just-yanked ()
  "Re-indent whatever you just yanked appropriately."
  (interactive)
  (exchange-point-and-mark)
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))

(bind-key "C-c I" #'pt/indent-just-yanked)


(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; avy is a GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
  :ensure t
  :bind (("M-g l" . #'avy-goto-line)
         ("C-c j j" . #'avy-goto-line)
         ("M-g c" . #'avy-goto-char)
         ("M-g b" . #'avy-goto-char-2)
         ("M-g w" . #'avy-goto-word-1)))

;; Spelling
(when (eq system-type 'darwin)
  (setq ispell-program-name "/opt/homebrew/bin/ispell"))
(when (memq system-type '(windows-nt ms-dos))
  (setq ispell-program-name "D:/Program Files/hunspell-1.3.2-3-w32/bin/hunspell.exe"))





;; bs-show: 'a' toggles all buffers, and '+' then marks an entry to display in both views
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)



;; Org mode
(when (display-graphic-p)
  (require 'org)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-startup-indented t)

  ;; toggle a checkbox by a mouse
  (require 'org-mouse)

  (defadvice org-agenda (around split-vertically activate)
    (let ((split-height-threshold nil)) ; splitt only horizontally: split the window placing the new window to the right
      ad-do-it))

  ;; Overrides function-key-map for preferred input-method to translate input sequences to english,
  ;; so we can use Emacs bindings while non-default system layout is active
  ;; https://github.com/a13/reverse-im.el
  (require 'reverse-im)
  (reverse-im-activate "russian-computer"))




;;
;; custom-set-variables
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Org/work/epam.org" "/Users/Andrei_Akula/Org/work/work.org"))
 '(package-selected-packages '(all-the-icons-dired all-the-icons reverse-im)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
