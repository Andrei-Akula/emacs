;; -*- coding: utf-8; lexical-binding: t -*-
;; Andrei's Akila Emacs setup, based on Patrick Thomson's Emacs setup

(setq gc-cons-threshold 100000000)
(setq max-specpdl-size 5000)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)


;; Packages
;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file - https://github.com/jwiegley/use-package
;; you can use M-x describe-personal-keybindings to see all such keybindings you've set throughout your .emacs file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure 't)
(setq package-native-compile t)
(eval-when-compile
  (require 'use-package))


;; Apply only in Graphic mode
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))


;; Increases the maximum depth of evaluation for Lisp expressions, which can prevent errors in deeply nested expressions.
(setq max-lisp-eval-depth 2000)

;;
;; MacOS
;;
(when (eq system-type 'darwin)
  ;; Standard macOS conventions would have s-w close the current buffer, not the whole window.
  (bind-key "s-w" #'kill-current-buffer)
  ;;(setq ns-auto-hide-menu-bar t)
  )


(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;;
;; Fixing Emacs’s defaults
;;
(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t

 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore

 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t

 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil

 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil

 ;; Let C-k delete the whole line.
 ;;kill-whole-line t

 ;; search should be case-sensitive by default
 ;;case-fold-search nil

 ;; accept 'y' or 'n' instead of yes/no
 ;; the documentation advises against setting this variable
 ;; the documentation can get bent imo
 use-short-answers t

 ;; my source directory
 ;;default-directory "~/src/"

 ;; eke out a little more scrolling performance
 fast-but-imprecise-scrolling t

 ;; prefer newer elisp files
 load-prefer-newer t

 ;; when I say to quit, I mean quit
 ;;confirm-kill-processes nil

 ;; if native-comp is having trouble, there's not very much I can do
 native-comp-async-report-warnings-errors 'silent

 ;; unicode ellipses are better
 truncate-string-ellipsis "…"

 ;; I want to close these fast, so switch to it so I can just hit 'q'
 help-window-select t

 ;; keep the point in the same place while scrolling
 scroll-preserve-screen-position t

 ;; more info in completions
 completions-detailed t

 ;; highlight error messages more aggressively
 next-error-message-highlight t

 ;; don't let the minibuffer muck up my window tiling
 read-minibuffer-restore-windows t

 ;; scope save prompts to individual projects
 save-some-buffers-default-predicate 'save-some-buffers-root

 ;; don't keep duplicate entries in kill ring
 kill-do-not-save-duplicates t
 )

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes a buffer-local variable when set.
(setq-default indent-tabs-mode nil)


;; This command enables the Delete Selection mode. When this mode is active, if there is an active selection (highlighted text) and you
;; start typing, the selected text will be automatically deleted and replaced with the newly typed text. This also deactivates the
;; mark.
(delete-selection-mode t)
(column-number-mode)
(savehist-mode)

;; use display-line-numbers-mode only in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; treatment of whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)


(defun pt/indent-just-yanked ()
  "Re-indent whatever you just yanked appropriately."
  (interactive)
  (exchange-point-and-mark)
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))

(bind-key "C-c I" #'pt/indent-just-yanked)

;; By default, Emacs wraps long lines, inserting a little icon to indicate this. I find this a bit naff. What we can do to mimic more
;; modern behavior is to allow line truncation by default, but also allow touchpad-style scrolling of the document.
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)
(setq-default truncate-lines t)


;; Text manipulation
(setq-default fill-column 120 )

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; avy is a GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
  :bind (("M-g l" . #'avy-goto-line)
         ("C-c j j" . #'avy-goto-line)
         ("M-g c" . #'avy-goto-char)
         ("M-g b" . #'avy-goto-char-2)
         ("M-g w" . #'avy-goto-word-1)))

;;
;; Spelling
;;
(when (eq system-type 'darwin)
  (setq ispell-program-name "/opt/homebrew/bin/ispell"))
(when (memq system-type '(windows-nt ms-dos))
  (setq ispell-program-name "D:/Program Files/hunspell-1.3.2-3-w32/bin/hunspell.exe"))

;;
;; IDO
;;
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


;; bs-show: 'a' toggles all buffers, and '+' then marks an entry to display in both views
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

(bind-key "C-x K" #'kill-current-buffer)

;; auto-refresh buffer if file hase changed
;;(global-auto-revert-mode t)

;;
;; Dired
;;
(setq
  ;; Why wouldn't you create destination directories when copying files, Emacs?
 dired-create-destination-dirs 'ask
 ;; Before the existence of this option, you had to either hack
 ;; dired commands or use the dired+ library, the maintainer
 ;; of which refuses to use a VCS. So fuck him.
 dired-kill-when-opening-new-dired-buffer t
 ;; Update directory listings automatically (again, why isn't this default?)
 dired-do-revert-buffer t
 ;; Sensible mark behavior
 dired-mark-region t
 dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
 ;; when two Dired buffers are open side-by-side we get the other
 ;; buffer as the default target of the current rename or copy operation
 dired-dwim-target t
 )

;; MacOS ls doesn't know --group-directories-first --time-style=long-iso
(when (eq system-type 'darwin)
  (setq dired-listing-switches "-AGFhlv"))

;;
;; Visuals
;;
(when (display-graphic-p)
  ;; Start window size
  (set-frame-size (selected-frame) 160 46)

  ;; Every Emacs window should, by default occupy all the screen space it can
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Display visited file's path in the frame title
  (setq frame-title-format
	'((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; Fonts
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Menlo-12")
    (set-face-attribute 'variable-pitch nil :font "PT Mono-12"))

  ;;; Font on Windows
  (when (memq system-type '(windows-nt ms-dos))
    (set-face-attribute 'default nil :font "Consolas-10"))

  ;; Icons
  (unless (memq system-type '(windows-nt ms-dos))
    (let ((installed (package-installed-p 'all-the-icons)))
      (use-package all-the-icons)
      (unless installed (all-the-icons-install-fonts)))
    ;; Icons in dired
    (use-package all-the-icons-dired
      :after all-the-icons
      :hook (dired-mode . all-the-icons-dired-mode)))

  ;;
  ;; Themes
  (setq custom-safe-themes t)

  (use-package ef-themes
    :ensure t
    :config
    ;; Add all your customizations prior to loading the themes
    (setq ef-themes-to-toggle '(ef-light ef-owl))
  
    ;; Disable all other themes to avoid awkward blending:
    (mapc #'disable-theme custom-enabled-themes)
  
    ;; Load the theme of choice:
    (load-theme 'ef-light :no-confirm)
  
    (define-key global-map (kbd "<f5>") #'ef-themes-toggle))
  )


;;
;; Org mode
;;
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

  ;; Overrides function-key-map for preferred input-method to translate input sequences to english, so we can use Emacs bindings while non-default system layout is active
  ;; https://github.com/a13/reverse-im.el
  (require 'reverse-im)
  (reverse-im-activate "russian-computer"))

;;
;; Programming languages
;;
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default js-switch-indent-offset 2)

(when (display-graphic-p)
  (use-package typescript-mode
    :custom (typescript-indent-level 2))

  (use-package js2-mode
    :hook (js2-mode . js2-imenu-extras-mode)
    :mode ("\\.js$" . js2-mode)
    :ensure t
    :custom
    (js2-mode-assume-strict t)
    (js2-warn-about-unused-function-arguments t)
    )

  (use-package xref-js2
    :ensure t
    :hook (js2-mode . pt/js-hook)
    :custom
    (xref-js2-search-program 'rg)
    :config
    (defun pt/js-hook ()
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

  (use-package web-mode
    :custom (web-mode-markup-indent-offset 2))

  )

;;
;; Recent files
;;
(require 'recentf)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
    (message "Opening file...")
    (message "Aborting")))

;; get rid of `find-file-read-only' and replace it with something  more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; enable recent files mode.
(recentf-mode t)
;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

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
