;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((hostname (system-name)))
  (cond
   ;; nixos .env path
   ((string= hostname "nixos") (setq env-var-filepath "~/Desktop/nixos/home/emacs/.env"))

   ;; gnu guix system .env path
   ((string= hostname "x200") (setq env-var-filepath "~/.emacs.d/.env"))

   ;; backup .env path
   (t (setq env-var-filepath "~/.emacs.d/.env"))))

(defun load-env-vars (file)
  (with-temp-buffer
    (insert-file-contents file)
    (dolist (line (split-string (buffer-string) "\n" t))
      (let ((key-value (split-string line "=" t)))
        (setenv (car key-value) (cadr key-value))))))

;; use .env
(load-env-vars env-var-filepath)

; system
(setq config-name (getenv "CONFIG_NAME"))
(setq local-directory (getenv "LOCAL_DIRECTORY"))
(setq snippets-directory (getenv "SNIPPETS_DIRECTORY"))

(setq gpg-key (getenv "GPG_KEY")) ; GnuPG Key ID
(setq banner-filepath (getenv "BANNER_FILEPATH"))

; babel
(setq tangle-filepath (getenv "TANGLE_FILEPATH"))

;; fixes override-global-map issue on NixOS
(require 'bind-key)

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

(use-package emacs
  :init
  (setq mac-option-modifier 'meta)  ; use Alt/Option as meta key
  (setq mac-command-modifier 'meta) ; retain command key as meta key
  :preface
  (defvar my/indent-width 2)
  (defun my/split-and-follow-vertically ()
    "Split window vertically (below)."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun my/split-and-follow-horizontally ()
    "Split window horizontally (right)."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (setq user-full-name "pokey")
  (setq frame-title-format '("Emacs " emacs-version))
  (setq ring-bell-function 'ignore)
  (setq-default default-directory local-directory)
  (setq frame-resize-pixelwise t)
  (setq scroll-conservatively 101)
  (setq scroll-preserve-screen-position t)
  (setq auto-window-vscroll nil)
  (setq hscroll-step 1)
  (setq scroll-step 1)
  (setq hscroll-margin 0)
  (setq load-prefer-newer t)
  (setq inhibit-compacting-font-caches t)
  (setq echo-keystrokes 0.02)
  (setq kill-buffer-query-functions nil)
  (setq delete-by-moving-to-trash t)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'scroll-right 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (unless (display-graphic-p)
    (global-set-key (kbd "C-h") #'backward-kill-word))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq initial-scratch-message "*scratch*")

  (setq max-lisp-eval-depth 10000)
  (set-default 'truncate-lines nil)

  (setq jit-lock-defer-time 0)
  (setq fast-but-imprecise-scrolling t)
  (xterm-mouse-mode +1)
  (bind-key* (kbd "<f4>") #'(lambda ()
                              (interactive)
                              (if (one-window-p)
                                  (kill-current-buffer)
                                (kill-buffer-and-window)))))

(use-package uniquify
  :ensure nil
  :config
  (setq-default uniquify-buffer-name-style 'forward))

;; Overriding built-in function
;; (defun use-fancy-splash-screens-p ()
;;   "Never display splash screen with Emacs PNG logo."
;;   nil)

;; hide extra info for NixOs
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "to-be-dumped.el")))

(use-package simple
  :ensure nil
  :config
  (column-number-mode +1))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

(use-package files
  :ensure nil
  :config
  (remove-hook 'find-file-hook 'vc-refresh-state) ; makes open files faster
  (setq confirm-kill-processes nil)
  (setq create-lockfiles nil) ; don't create .# files (crashes 'npm start')
  (setq make-backup-files nil)
  (setq revert-without-query '(".*"))
  (global-set-key (kbd "<f5>") #'(lambda ()
                                   (interactive)
                                   (revert-buffer)
                                   (message "Refreshing buffer...done"))))

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-interval 2)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers nil)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode -1)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.4))

(use-package js
  :ensure nil
  ;; :mode ("\\.jsx?\\'" . js-jsx-mode)
  :config
  (setq js-indent-level my/indent-width)
  (add-hook 'flycheck-mode-hook
            #'(lambda ()
                (let* ((root (locate-dominating-file
                              (or (buffer-file-name) default-directory)
                              "node_modules"))
                       (eslint (and root (expand-file-name "node_modules/.bin/eslint" root))))
                  (when (and eslint (file-executable-p eslint))
                    (setq-local flycheck-javascript-eslint-executable eslint))))))

(use-package cc-vars
  :ensure nil
  :mode ("\\.inl\\'" . c++-mode)
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (c++-mode  . "bsd")
                          (c-mode    . "bsd")
                          (other     . "k&r")))
  (setq-default c-basic-offset my/indent-width))

(use-package cc-mode
  :ensure nil
  :config
  (define-key c++-mode-map ":" nil)
  (add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                          comment-end   ""))))

(use-package python
 :ensure nil
  :mode ("\\.gyp\\'" . python-mode)
  :config
  (setq python-indent-offset my/indent-width)
  (setq python-shell-interpreter "python3"))

(use-package ruby-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package css-mode ; inherited by less-css-mode
  :ensure nil
  :config
  (setq css-indent-offset my/indent-width))

(use-package sh-script
  :ensure nil
  :config
  (with-eval-after-load 'company
    (add-hook 'sh-mode-hook #'(lambda () (company-mode -1)))))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package frame
  :preface
  (defconst small-fonts-list '("Consolas" "Ubuntu Mono" "Fixedsys Excelsior" "Inconsolata"))
  (defconst tight-fonts-list '("Consolas" "Ubuntu Mono" "Monaco" "Comic Mono"))
  (defun my/set-default-fonts (english-font chinese-font font-size font-weight)
    "Set the default Latin and CJK font families, as well as the line height."
    (interactive)
    (defvar is-using-undersized-font nil)
    (if (member english-font small-fonts-list)
        (progn
          (setq font-size (round (* font-size 1.1)))
          (setq is-using-undersized-font t))
      (setq is-using-undersized-font nil))
    (when (member english-font (font-family-list))
      (set-face-attribute 'default nil :family english-font :height font-size :weight font-weight))
    (when (member chinese-font (font-family-list))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family chinese-font
                                             :size (*(/ font-size 10) 1.0)))))
    (setq-default line-spacing (if (member english-font tight-fonts-list) 2 1)))
  (defun my/set-big-fonts ()
    (interactive)
    (my/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 95 'normal)
    (when (member "Inconsolata" (font-family-list))
      (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0))
    (when (member "Segoe UI Variable Static Small" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI Variable Static Small" :height 95 :weight 'normal)))
  (defun my/set-small-fonts ()
    (interactive)
    (my/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 85 'normal)
    (when (member "Inconsolata" (font-family-list))
      (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0))
    (when (member "Segoe UI Variable Static Small" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI Variable Static Small" :height 0.9 :weight 'normal)))
  :ensure nil
  :config
  (setq default-frame-alist (append (list '(width . 74) '(height . 35) '(internal-border-width . 2))))
  (if (display-graphic-p)
      (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)))
  (blink-cursor-mode -1)
  (setq blink-cursor-blinks -1) ; blink forever
  (my/set-big-fonts))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package flyspell
  :ensure nil
  :hook ((markdown-mode . flyspell-mode)
         (org-mode      . flyspell-mode))
  ;:config
  ;(setq ispell-program-name "/usr/bin/aspell")
  )

;; paranthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :config
  (setq electric-pair-preserve-balance nil))

;; (use-package saveplace
;;   :ensure nil
;;   :config
;;   (save-place-mode +1))

(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.local/lib/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude "/usr/lib/.*")
  (add-to-list 'recentf-exclude "/usr/include/.*")
  (recentf-mode +1))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package xref
  :ensure nil
  :preface
  (defun my/xref-reposition-in-new-buffer (func &rest args)
    "When xref opens a new buffer, reposition the cursor at 1/4 window height from top.
This follows the UX design of Visual Studio Code."
    (let ((original-buf (current-buffer)))
      (apply func args)
      (unless (eq (current-buffer) original-buf)
        (recenter-top-bottom (/ (window-body-height) 4)))))
  :config
  (advice-add 'xref-find-definitions :around #'my/xref-reposition-in-new-buffer)
  (setq xref-after-jump-hook '(xref-pulse-momentarily))
  (setq xref-after-return-hook '(xref-pulse-momentarily))
  (setq xref-prompt-for-identifier nil))

(use-package tooltip
  :ensure nil
  :config
  (tooltip-mode -1))

(use-package view
  :ensure nil
  :config
  (with-eval-after-load 'evil-collection
    (add-hook 'view-mode-hook
              #'(lambda () (interactive)
                  (evil-collection-define-key '(motion normal) 'view-mode-map (kbd "SPC") nil)))))

(use-package help-mode
  :ensure nil
  :config
  (with-eval-after-load 'evil-collection
    (add-hook 'help-mode-hook
              #'(lambda () (interactive)
                  (evil-collection-define-key '(motion normal) 'help-mode-map (kbd "SPC") nil)))))

(use-package diff-mode
  :ensure nil
  :config
  (with-eval-after-load 'evil-collection
    (add-hook 'diff-mode-hook
              #'(lambda () (interactive)
                  (evil-collection-define-key '(motion normal) 'diff-mode-map  (kbd "SPC") nil)))))

(use-package whitespace
  :ensure nil
  :config
  (delete 'lines whitespace-style)
  (delete 'newline-mark whitespace-style)
  (bind-key* (kbd "<f7>") #'(lambda ()
                              (interactive)
                              (whitespace-mode 'toggle))))

(use-package doom-themes
  :custom-face
  (region                         ((t (:extend nil))))
  (highlight-symbol-face          ((t (:background "#355266" :distant-foreground "#bbbbbb"))))
  (highlight                      ((t (:foreground "#4db2ff" :background nil :underline t)))) ; link hover
  (link                           ((t (:foreground "#3794ff"))))
  (evil-ex-substitute-replacement ((t (:strike-through nil))))
  (vertical-border                ((t (:foreground "black" :background "black"))))
  (fringe                         ((t (:background nil))))
  :config
  ;(setq doom-themes-enable-bold nil)
  ;(setq doom-themes-enable-italic nil)
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15
        doom-modeline-bar-width 0
        doom-modeline-minor-modes nil
        doom-modeline-lsp t
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-major-mode-color-icon t))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package undo-fu
  :config
  (setq undo-fu-ignore-keyboard-quit t))

(use-package evil
  :after undo-fu
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-shift-width my/indent-width)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
  :hook (after-init . evil-mode)
  :preface
  (defun my/paste-with-ctrl-shift-v ()
    "Paste with Ctrl-Shift-v, as inspired by Windows Terminal shortcut."
    (interactive)
    (evil-normal-state nil)
    (evil-paste-after 1)
    (evil-insert-state nil)
    (right-char))
  (defun my/pulse-line ()
    "Flash highlight the current line with region face"
    (interactive)
    (pulse-momentary-highlight-one-line (point) 'region))
  :config
  (setq evil-insert-state-cursor '(bar . 1))
  (setq evil-kill-on-visual-paste nil)
  (setq-default evil-symbol-word-search t)
  (define-key evil-normal-state-map (kbd "C-w C-o") #'(lambda () (interactive) (neotree-hide) (delete-other-windows)))
  (define-key evil-normal-state-map (kbd "C-o") #'(lambda () (interactive) (evil-jump-backward) (my/pulse-line)))
  (define-key evil-normal-state-map (kbd "C-i") #'(lambda () (interactive) (evil-jump-forward) (my/pulse-line)))
  (bind-key* (kbd "M-<up>") #'(lambda () (interactive) (scroll-down 2)))
  (bind-key* (kbd "M-<down>") #'(lambda () (interactive) (scroll-up 2)))
  (bind-key* (kbd "M-<left>") #'(lambda () (interactive) (scroll-right 2)))
  (bind-key* (kbd "M-<right>") #'(lambda () (interactive) (scroll-left 2)))
  (if (display-graphic-p)
      (define-key evil-normal-state-map (kbd "z <return>") #'evil-scroll-line-to-top)
    (define-key evil-normal-state-map (kbd "z RET") #'evil-scroll-line-to-top))
  (define-key evil-insert-state-map (kbd "C-n") nil) ; avoid conflict with company tooltip selection
  (define-key evil-insert-state-map (kbd "C-p") nil) ; avoid conflict with company tooltip selection
  (define-key evil-normal-state-map (kbd "C-S-c") #'evil-yank)
  (define-key evil-insert-state-map (kbd "C-S-v") #'my/paste-with-ctrl-shift-v)
  (define-key evil-normal-state-map (kbd "u") #'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") #'undo-fu-only-redo)
  (define-key evil-normal-state-map (kbd "C-u") #'(lambda () (interactive) (evil-scroll-up 0) (recenter)))
  (define-key evil-normal-state-map (kbd "C-d") #'(lambda () (interactive) (evil-scroll-down 0) (recenter)))
  (unless (display-graphic-p)
    (evil-define-key '(motion normal) profiler-report-mode-map (kbd "TAB") #'profiler-report-toggle-entry))
  (evil-define-key '(motion normal) prog-mode-map (kbd "gd") #'xref-find-definitions)
  (evil-define-key '(motion normal) prog-mode-map (kbd "gD") #'xref-find-references)
  ;; (evil-define-key 'motion prog-mode-map (kbd "gd") #'lsp-bridge-find-def)
  ;; (evil-define-key 'motion prog-mode-map (kbd "gD") #'lsp-bridge-find-references)
  (with-eval-after-load 'lsp-ui
    (add-hook 'buffer-list-update-hook
              #'(lambda ()
                  (when (bound-and-true-p lsp-ui-mode)
                    (define-key evil-normal-state-local-map (kbd "gr") #'lsp-ui-peek-find-references)
                    (define-key evil-normal-state-local-map (kbd "gD") #'lsp-ui-peek-find-references)))))
                                        ;(evil-ex-define-cmd "q" #'kill-current-buffer)
                                        ;(evil-ex-define-cmd "wq" #'(lambda () (interactive) (save-buffer) (kill-current-buffer)))
  ;; (evil-set-leader '(motion normal) (kbd "SPC"))
  ;; (evil-define-key '(motion normal) 'global
  ;;   (kbd "<leader>s")     #'counsel-grep-or-swiper
  ;;   (kbd "<leader>w")     #'save-buffer
  ;;   (kbd "<leader>f")     #'counsel-projectile-find-file
  ;;   (kbd "<leader>F")     #'projectile-ripgrep
  ;;   (kbd "<leader>r")     #'ranger
  ;;   (kbd "<leader><tab>") #'my/lsp-execute-code-action
  ;;   (kbd "<leader>TAB")   #'my/lsp-execute-code-action
  ;;   (kbd "<leader>e")     #'my/neotree-project-toggle
  ;;   (kbd "<leader>q")     #'save-buffers-kill-emacs)
  )

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (setq evil-collection-company-use-tng nil)
;;   (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode +1))

(use-package evil-matchit
  :hook ((web-mode
          html-mode
          mhtml-mode
          js-mode
          typescript-mode
          ) . turn-on-evil-matchit-mode))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (local-unset-key (kbd "f"))
  (define-key magit-mode-map (kbd "<f5>") #'(lambda ()
                                              (interactive)
                                              (magit-refresh)
                                              (message "Refreshing Magit...done"))))

(use-package git-gutter
  :unless (string= (system-name) "nixos")
  :if (display-graphic-p)
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :unless (string= (system-name) "nixos")
  :if (display-graphic-p)
  :config
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "..XXXXX."
    "..XXXXX."
    "..XXXXX."
    "..XXXXX.")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "..XXXXX."
    "..XXXXX."
    "..XXXXX."
    "..XXXXX.")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."
    "XXXXX..."
    "XXXXXX.."
    "XXXXXXX."
    "XXXXXXXX"
    "XXXXXXX."
    "XXXXXX.."
    "XXXXX..."
    "XXXX...."
    "XXX....."
    "XX......"
    "X......."))

(use-package diff-hl
  :unless (display-graphic-p)
  :custom-face
  (diff-hl-insert ((t (:background nil :slant normal))))
  (diff-hl-delete ((t (:background nil :slant normal))))
  (diff-hl-change ((t (:background nil :slant normal))))
  :config
  (setq diff-hl-margin-symbols-alist '((insert  . "┃") ; U+02503 (box drawings heavy vertical)
                                       (delete  . "▶")
                                       (change  . "┃")
                                       (unknown . "┃")
                                       (ignored . "┃")))
  (diff-hl-margin-mode)
  (setq diff-hl-flydiff-delay 0.05)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

(use-package blamer
  ;:bind (("C-c g" . blamer-mode))
  :config
  (setq blamer-idle-time 0.05)
  (setq blamer-author-formatter "%s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter ": %s")
  (setq blamer-max-commit-message-length 100)
  (setq blamer-min-offset 70))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-height 15)
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
        '((counsel-rg            . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (swiper                . ivy--regex-plus)
          (t                     . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (if (display-graphic-p)
      (progn
        (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-next-line)
        (define-key ivy-minibuffer-map (kbd "<return>") #'ivy-alt-done)
        (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-immediate-done))
    (progn
      (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-next-line)
      (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
      (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)))
  (define-key ivy-minibuffer-map (kbd "<backtab>") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-c m") #'ivy-mark)
  (define-key ivy-minibuffer-map (kbd "C-c u") #'ivy-unmark))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s")
  (setq counsel-fzf-cmd "fd -H -c never \"%s\"")
  (global-set-key (kbd "C-S-p") #'counsel-M-x))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package projectile
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix " ")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (setq prescient-sort-length-enable nil)
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy counsel)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper
               counsel-grep
               counsel-rg
               counsel-projectile-rg
               ivy-switch-buffer
               counsel-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
          web-mode        ; ts-ls/HTML/CSS
          rust-mode       ; rust-analyzer
          go-mode         ; gopls
          ) . lsp-deferred)
  :preface
  (defun my/lsp-execute-code-action ()
    "Execute code action with pulse-line animation."
    (interactive)
    (my/pulse-line)
    (call-interactively 'lsp-execute-code-action))
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
  :commands lsp
  :config
  (add-hook 'java-mode-hook #'(lambda () (when (eq major-mode 'java-mode) (lsp-deferred))))
  (global-unset-key (kbd "<f2>"))
  (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-links nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.25)
  (setq lsp-auto-execute-action nil)
  (with-eval-after-load 'lsp-clangd
    (setq lsp-clients-clangd-args '("--header-insertion=never" "-j=4" "-background-index")))
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-sideline-global ((t (:italic t))))
  (lsp-ui-peek-highlight  ((t (:foreground unspecified :background unspecified :inherit isearch))))
  :config
  (with-eval-after-load 'evil
    (add-hook 'buffer-list-update-hook
              #'(lambda ()
                  (when (bound-and-true-p lsp-ui-mode)
                    (evil-define-key '(motion normal) 'local (kbd "K")
                      #'(lambda () (interactive) (lsp-ui-doc-glance) (my/pulse-line)))))))
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-enhanced-markdown nil)
  (setq lsp-ui-doc-delay 0.01)
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-text-scale-level -1.0)
    (setq lsp-ui-doc-max-width 80)
    (setq lsp-ui-doc-max-height 25)
    (setq lsp-ui-doc-position 'at-point))
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (setq lsp-ui-sideline-diagnostic-max-line-length 80)
  (setq lsp-ui-sideline-diagnostic-max-lines 2)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (call-interactively #'lsp-workspace-restart)))
  (pyvenv-mode +1))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.0)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60)
  (setq company-tooltip-limit 7)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (unless (display-graphic-p)
    (define-key company-active-map (kbd "C-h") #'backward-kill-word)
    (define-key company-active-map (kbd "C-w") #'backward-kill-word))
  (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (if (display-graphic-p)
      (define-key company-active-map (kbd "<tab>") 'company-select-next)
    (define-key company-active-map (kbd "TAB") 'company-select-next))
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package company-box
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-enable nil)
  (setq company-box-scrollbar nil)
  (setq company-box-frame-behavior 'default))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode))
  :custom-face
  (flycheck-error   ((t (:inherit error :underline t))))
  (flycheck-warning ((t (:inherit warning :underline t))))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.1)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-indication-mode nil)
  (define-key flycheck-mode-map (kbd "<f8>") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "S-<f8>") #'flycheck-previous-error)
  (flycheck-define-checker proselint
    "A linter for prose. Install the executable with `pip3 install proselint'."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (markdown-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

(use-package markdown-mode
  :custom-face
  (markdown-code-face ((t (:background unspecified :inherit lsp-ui-doc-background)))))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :config
  (setq typescript-indent-level my/indent-width))

;; (use-package rust-mode)

;; (use-package flycheck-rust
;;   :config
;;   (with-eval-after-load 'rust-mode
;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; (use-package go-mode
;;   :config
;;   (with-eval-after-load 'evil
;;     (evil-define-key '(motion normal) go-mode-map (kbd "gd") #'xref-find-definitions)
;;     ;; (evil-define-key '(motion normal) go-mode-map (kbd "gd") #'lsp-bridge-find-def)
;;     (evil-define-key '(motion normal) go-mode-map (kbd "K") #'(lambda () (interactive) (lsp-ui-doc-glance) (my/pulse-line)))))

(use-package lua-mode)

(use-package json-mode)

(use-package vimrc-mode)

(use-package cmake-font-lock)

(use-package yaml-mode)

;; (use-package haskell-mode)

;; (use-package rjsx-mode
;;   :mode ("\\.jsx?\\'" . rjsx-mode)
;;   :custom-face
;;   (js2-error   ((t (:inherit default :underscore nil))))
;;   (js2-warning ((t (:inherit default :underscore nil))))
;;   :config
;;   (define-key rjsx-mode-map "<" nil)
;;   (define-key rjsx-mode-map (kbd "C-d") nil)
;;   (define-key rjsx-mode-map ">" nil))

;; (use-package web-mode
;;   :mode (("\\.html?\\'" . web-mode)
;;          ("\\.css\\'"   . web-mode)
;;          ("\\.jsx?\\'"  . web-mode)
;;          ("\\.tsx?\\'"  . web-mode)
;;          ("\\.json\\'"  . web-mode))
;;   :config
;;   (setq web-mode-markup-indent-offset my/indent-width)
;;   (setq web-mode-code-indent-offset my/indent-width)
;;   (setq web-mode-css-indent-offset my/indent-width)
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;; (use-package emmet-mode
;;   :hook ((html-mode
;;           css-mode
;;           js-mode
;;           js-jsx-mode
;;           typescript-mode
;;           web-mode
;;           ) . emmet-mode)
;;   :config
;;   (setq emmet-insert-flash-time 0.001) ; effectively disabling it
;;   (add-hook 'js-jsx-mode-hook #'(lambda ()
;;                                   (setq-local emmet-expand-jsx-className? t)))
;;   (add-hook 'web-mode-hook #'(lambda ()
;;                                (setq-local emmet-expand-jsx-className? t))))

(use-package cpp-auto-include
  :bind (:map c++-mode-map ("C-c i" . cpp-auto-include/ensure-includes-for-file)))

(use-package format-all
  :preface
  (defun my/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (let ((windowstart (window-start)))
      (if (derived-mode-p 'prolog-mode)
          (prolog-indent-buffer)
        (format-all-buffer))
      (set-window-start (selected-window) windowstart)))
  (defalias 'format-document #'my/format-code)
  :config
  (global-set-key (kbd "<f6>") #'my/format-code)
  (global-set-key (kbd "C-M-l") #'my/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter)
  (add-hook 'python-mode-hook #'(lambda ()
                                  (setq-local format-all-formatters '(("Python" yapf)))))
  (add-hook 'sql-mode-hook #'(lambda ()
                               (setq-local format-all-formatters '(("SQL" pgformatter))))))

;; background color for hex codes
(use-package rainbow-mode
  :config
  (bind-key* (kbd "C-c r") #'rainbow-mode))

;; highlight specified keywords
(use-package hl-todo
  :custom-face
  (hl-todo                        ((t (:inverse-video nil :italic t :bold nil))))
  :config
  (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
  (add-to-list 'hl-todo-keyword-faces '("WHY" . "#7cb8bb"))
  (global-hl-todo-mode +1))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq dired-listing-switches "-lat") ; sort by date (new first)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (with-eval-after-load 'evil-collection
    (add-hook 'dired-mode-hook
              #'(lambda () (interactive)
                  (evil-collection-define-key '(motion normal) 'dired-mode-map  (kbd "SPC") nil)))))

(use-package ranger
  :after dired
  :config
  (setq ranger-width-preview 0.5)
  (setq ranger-width-parents 0.167)
  (setq ranger-preview-delay 0.02)
  (setq ranger-show-hidden t)
  (with-eval-after-load 'evil
    (evil-define-key '(motion normal) 'ranger-mode-map (kbd "H") #'evil-window-top)
    (evil-define-key '(motion normal) 'ranger-mode-map (kbd "L") #'evil-window-bottom))
  (define-key ranger-mode-map (kbd "d") #'dired-flag-file-deletion)
  (define-key ranger-mode-map (kbd "u") #'dired-unmark)
  (define-key ranger-mode-map (kbd "U") #'dired-unmark-all-marks)
  (define-key ranger-mode-map (kbd "x") #'dired-do-flagged-delete)
  (define-key ranger-mode-map (kbd "i") #'dired-toggle-read-only)
  (define-key ranger-mode-map (kbd "m") #'dired-mark)
  (define-key ranger-mode-map (kbd "R") #'dired-do-rename)
  (define-key ranger-mode-map (kbd "C") #'dired-do-copy)
  (define-key ranger-mode-map (kbd "C-h") nil))

;; garbage collection
(use-package gcmh
  ;; :hook (emacs-startup-hook . gcmh-mode)
  :demand t
  :config
  (setq gcmh-low-cons-threshold (* 16 1024 1024))
  (gcmh-mode +1))

;; global clipboard for terminal mode
(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode +1))

(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :config
  (setq etcc-use-color t)
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner banner-filepath)
  (setq dashboard-banner-logo-title config-name)
  (setq dashboard-init-info "")
  (setq dashboard-items nil)
  (setq dashboard-set-footer t)
  (setq dashboard-footer-icon "")
  (setq dashboard-footer-messages '("")))

;; (use-package emojify
;;   :config
;;   (when (member "Segoe UI Emoji" (font-family-list))
;;     (set-fontset-font
;;      t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
;;   (setq emojify-display-style 'unicode)
;;   (setq emojify-emoji-styles '(unicode))
;;   (bind-key* (kbd "C-c .") #'emojify-insert-emoji)) ; override binding in any mode

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 0.8))

(use-package ivy-rich
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1))

(use-package minions
  :config
  (setq minions-mode-line-lighter "")
  (setq minions-mode-line-delimiters '("" . ""))
  (setq-default mode-line-buffer-identification '("%b // " (:eval (projectile-project-name))))
  (minions-mode +1))

(use-package neotree
  :after projectile
  :preface
  (defun my/neotree-project-toggle ()
    "Open NeoTree using the projectile root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (when (neo-global--window-exists-p)
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find projectile project root."))))
  :custom-face
  (neo-dir-link-face  ((t (:inherit variable-pitch))))
  (neo-header-face    ((t (:inherit variable-pitch))))
  (neo-banner-face    ((t (:inherit variable-pitch))))
  (neo-root-dir-face  ((t (:inherit variable-pitch))))
  (neo-file-link-face ((t (:inherit variable-pitch))))
  :config
  (add-hook 'neotree-mode-hook (lambda ()
                                 (hl-line-mode +1)
                                 (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                                 (with-eval-after-load 'evil
                                   (define-key evil-normal-state-local-map (kbd "H") 'evil-window-top))
                                 (setq-local line-spacing 1)))
  (setq neo-theme 'icons)
  (setq neo-autorefresh t) ; neotree.el: change delay to (run-with-idle-timer 0.1 ...)
  (setq neo-show-hidden-files t)
  (setq neo-window-width 30))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda ()
                       (setq-local evil-auto-indent nil)
                       (setq-local olivetti-body-width (+ fill-column 5)))))
  :config
  (require 'org-tempo)
  (setq org-link-descriptive t) ; nil indicates full links rather than the normal condensed displaying
  (setq org-startup-folded nil)
  (setq org-todo-keywords '((sequence "TODO" "PROG" "DONE")))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-html-checkbox-type 'html)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "TAB" 'org-cycle))

;(use-package org-bullets
;  :hook (org-mode . org-bullets-mode))

(use-package ox
  :ensure nil
  :config
  (setq org-export-with-smart-quotes t))

(defun my-org-roam-backlinks-section (backend)
  "Append a backlinks section with actual HTML links for org-roam nodes in HTML export."
  (when (org-roam-node-at-point)
    (let* ((node (org-roam-node-at-point))
           (backlinks (org-roam-backlinks-get node))
           (backlinks-html (mapconcat
                            (lambda (link)
                              (let* ((source-node (org-roam-backlink-source-node link))
                                     (file-path (org-roam-node-file source-node))
                                     (html-file-name (concat (file-name-base file-path) ".html")))
                                (format "<li><a href=\"%s\">%s</a></li>"
                                        html-file-name
                                        (org-roam-node-title source-node))))
                            backlinks "\n"))
           (backlinks-section (if backlinks-html
                                  (concat "<h2>Backlinks</h2><ul>\n" backlinks-html "\n</ul>")
                                "")))
      ;; Insert raw HTML if backlinks exist
      (when (and backlinks-section (not (string-empty-p backlinks-section)))
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n#+BEGIN_EXPORT html\n" backlinks-section "\n#+END_EXPORT\n")))))))

(add-hook 'org-export-before-processing-hook #'my-org-roam-backlinks-section)

(setq org-publish-project-alist
      `(("my-org-site"
         :base-directory ,local-directory
         :base-extension "org"
         :publishing-directory ,(expand-file-name "site/" local-directory)
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\" />"
         :html-postamble nil
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :org-html-htmlize-output-type 'css
         :force t)

        ("my-org-site-static"
         :base-directory ,(expand-file-name "site-static/" local-directory)
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg"
         :publishing-directory ,(expand-file-name "site/" local-directory)
         :recursive t
         :publishing-function org-publish-attachment)

        ("my-org-site-all" :components ("my-org-site" "my-org-site-static"))))

(setq org-publish-current-project 'my-org-site-all)
;(setq org-export-with-id-links t)
;(setq org-export-with-section-numbers nil)
;(setq org-export-with-toc t)

(use-package org-fragtog
    :hook (org-mode . org-fragtog-mode)
    :config
    ;(setq org-latex-create-formula-image-program 'dvisvgm) ;; sharper
    (setq org-preview-latex-default-process 'dvipng)
    )
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (add-hook 'org-mode-hook 'org-indent-mode) ; indent headings

(defun my/org-export-to-pdf-and-open ()
  "Export current Org buffer to a PDF and open it with Zathura on Linux or Preview on macOS."
  (interactive)
  (let ((output-file (org-latex-export-to-pdf)))
    (when output-file
      (cond
       ;; macOS
       ((eq system-type 'darwin)
        (start-process "open-pdf" "*Open PDF*" "open" "-a" "Preview" output-file))
       
       ;; GNU/Linux
       ((eq system-type 'gnu/linux)
        (start-process "zathura" "*Zathura*" "zathura" output-file))
       
       ;; Fallback
       (t
        (message "No PDF viewer configured for this system."))))))

  
  (use-package yasnippet
    :ensure t
    :config
    (setq yas-snippet-dirs
          '(snippets-directory))
    (yas-global-mode 1))

  (use-package org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-star 'replace)
    (setq org-modern-replace-stars "♠♣♥♦"))

(require 'ox-latex)
(plist-put org-format-latex-options :scale 0.4)

(require 'org-crypt)
(setq org-crypt-key gpg-key)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-tag-matcher "crypt")
(setq org-crypt-disable-auto-save t)
(add-hook 'org-mode-hook (lambda () (org-crypt-use-before-save-magic)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory local-directory)
  :config
  (org-roam-setup)
  (setq org-roam-dailies-directory "daily/")
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(
        ;; normal notes
        ("d" "default" plain "%?" :target
         (file+head "${slug}.org" "#+title: ${title}")
         :unnarrowed t)

        ;; agenda notes
        ("a" "agenda" plain "%?" :target
         (file+head "agenda/${slug}.org" "#+title: ${title}")
         :unnarrowed t)))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-transclusion
  :after org
  :hook (org-mode . org-transclusion-mode))

(use-package writeroom-mode
  :defer t)

(defun my/arrange ()
      "Re-arrange open windows."
      (interactive)
      (let ((current-window (selected-window)))
        (delete-other-windows)
        (split-window-horizontally)
        (other-window 1)
        (switch-to-buffer (other-buffer))
        (other-window -1)
        (select-window current-window)))

  (defun my/bottom-terminal ()
    (interactive)
    (split-window-vertically (- (/ (window-total-height) 5)))
    (other-window 1)
    (ansi-term (getenv "SHELL"))
    (other-window 1))

(defun my/ide-layout ()
  "Simple IDE layout."
  (interactive)
  (my/neotree-project-toggle)
  (other-window 1)
  (my/bottom-terminal)
  (other-window 1))

;;   (kbd "<leader><tab>") #'my/lsp-execute-code-action
;;   (kbd "<leader>TAB")   #'my/lsp-execute-code-action

(use-package general
  :config
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   ;; emacs
   "q" '(save-buffers-kill-emacs :which-key "quit emacs")
   "e" '(my/neotree-project-toggle :which-key "neotree")
   "u" '(org-babel-tangle :which-key "tangle (update)")
   "f s" '(save-buffer :which-key "save buffer")
   "f o" '(org-open-at-point :which-key "open point")
   "f f" '(find-file :which-key "find file")
   "r u" '(my/elfeed-update-and-show :which-key "update and show elfeed")
   "g" '(counsel-grep-or-swiper :which-key "grep (counsel)")
   "r" '(ranger :which-key "ranger")

   ;; projectile
   "P f" '(counsel-projectile-find-file :which-key "find file")
   "P g" '(projectile-ripgrep :which-key "grep (projectile)")

   ;; git/magit
   ; "g" '(magit-status :which-key "magit status")

   ;; buffers
   "b" '(switch-to-buffer :which-key "switch buffer")
   "k" '(kill-buffer :which-key "kill buffer")
   "d" '(image-dired :which-key "image dired")

   ;; todo
   "o" '(org-todo :which-key "cycle todo status")

   ;; encryption
   "p" '(org-decrypt-entry :which-key "PGP decrypt")

   ;; latex
   "l p" '(org-latex-preview :which-key "latex preview")
   "l e" '(org-latex-export-to-pdf :which-key "latex export")
   "l o" '(my/org-export-to-pdf-and-open :which-key "latex open")

   ;; agenda
   "a" '(org-agenda :which-key "agenda")

   ;; toggles
   "t f" '(toggle-frame-fullscreen :which-key "toggle fullscreen")
   "t i" '(my/ide-layout :which-key "ide layout")
   "t w" '(writeroom-mode :which-key "writeroom-mode")

   ;; music
   "m b" '(emms-smart-browse :which-key "emms browse")
   "m s" '(emms-stop :which-key "emms stop")
   "m p" '(emms-browser-display-playlist :which-key "emms playlist")

   ;; window
   "w a" '(my/arrange :which-key "arrange horizontally")
   "w o" '(other-window :which-key "other window")
   "w r" '(my/split-and-follow-horizontally :which-key "split right")
   "w b" '(my/split-and-follow-vertically :which-key "split below")

   ;; roam
   "n f" '(org-roam-node-find :which-key "roam find")
   "n i" '(org-roam-node-insert :which-key "roam insert")
   "n r" '(org-roam-node-random :which-key "random node")
   "n d N" '(org-roam-dailies-capture-today :which-key "capture today")
   "n d Y" '(org-roam-dailies-capture-yesterday :which-key "capture yesterday")
   "n d T" '(org-roam-dailies-capture-tomorrow :which-key "capture tomorrow")
   "n d n" '(org-roam-dailies-goto-today :which-key "goto today")
   "n d y" '(org-roam-dailies-goto-yesterday :which-key "goto yesterday")
   "n d t" '(org-roam-dailies-goto-tomorrow :which-key "goto tomorrow")
   "n d d" '(org-roam-dailies-goto-date :which-key "goto date")
   "n u" '(org-roam-ui-open :which-key "org-roam ui")
   ))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '(("https://xkcd.com/rss.xml" xkcd)
          ("https://hnrss.org/frontpage" hackernews)
          ("https://rss.arxiv.org/rss/q-fin" arxiv q-fin)
          ("https://stephango.com/feed.xml" kepano)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://stallman.org/rss/rss.xml" stallman)
          ("https://lukesmith.xyz/index.xml" smith)
          )))

(defun my/elfeed-update-and-show ()
  "Update elfeed and open the elfeed buffer."
  (interactive)
  (elfeed-update)
  (elfeed))

(general-define-key
 :keymaps 'elfeed-search-mode-map
 :states '(normal motion)
 "r" 'elfeed-update
 "RET" 'elfeed-search-show-entry)

(general-define-key
 :keymaps 'eww-mode-map
 :states 'normal
 "q" 'eww-back-url)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.05))

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-agenda-custom-commands
        '(("n" "Next View"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Schedule"
                                  :time-grid t
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 0)
                           (:habit t)
                           (:name "Due Today"
                                  :deadline today
                                  :order 2)
                           (:name "Due Soon"
                                  :deadline future
                                  :order 8)
                           (:name "Overdue"
                                  :deadline past
                                  :order 7)
                           (:discard (:anything t))
                           ))))
            (todo "" ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:name "Inbox"
                                :order 0
                                )
                         (:discard (:todo "TODO"))
                         (:auto-category t
                                         :order 9)
                         ))))))
          ("t" "Todo View"
           (
            (todo "" ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:name "Inbox"
                                :order 0
                                )
                         (:auto-category t
                                         :order 9)
                         ))))))
          ))
  (org-super-agenda-mode))

(setq org-agenda-files (list (concat local-directory "/agenda")))
(setq org-agenda-timegrid-use-ampm t)

(defun refresh-org-agenda-buffers ()
  "Refresh all `org-agenda` buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'org-agenda-mode)
        (org-agenda-redo)))))

(defun run-refresh-org-agenda-at-next-minute ()
  "Run the refresh-org-agenda-buffers function at the start of the next minute."
  (let* ((current-seconds (string-to-number (format-time-string "%S")))
         (seconds-to-next-minute (- 60 current-seconds)))
    (run-at-time seconds-to-next-minute 60 'refresh-org-agenda-buffers)))

(run-refresh-org-agenda-at-next-minute)

(general-define-key
 :states '(normal motion visual)
 :keymaps 'image-dired-thumbnail-mode-map
 "j" 'image-dired-display-next
 "k" 'image-dired-display-previous
 "l" 'image-transform-reset-to-original
 "h" 'image-dired-thumbnail-display-external
 "q" '(lambda () (interactive) (kill-this-buffer) (delete-other-windows)))

(general-define-key
 :states '(normal motion visual)
 :keymaps 'dired-mode-map
 "q" '(lambda () (interactive) (kill-this-buffer) (delete-other-windows)))

(general-define-key
 :states '(normal motion visual)
 :keymaps 'image-dired-display-image-mode-map
 "q" '(lambda () (interactive) (kill-this-buffer) (delete-other-windows)))

(general-define-key
 :states '(normal motion visual)
 :keymaps 'image-dired-image-mode-map
 "q" '(lambda () (interactive) (kill-this-buffer) (delete-other-windows)))

(setq large-file-warning-threshold nil)

(use-package calc
  :defer t)

(use-package casual-calc
  :ensure nil
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu))
  :after (calc))

(provide 'init)

(use-package emms
  :config
  (emms-standard)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Desktop/orgfiles/music/")
  (emms-add-directory-tree "~/Desktop/orgfiles/music/")

  (emms-all)

  (add-hook 'emms-player-started-hook 'emms-show)
  :custom
  (emms-browser-covers #'emms-browser-cache-thumbnail-async))

  ;; emms-smart-browse / emms-browser keybindings
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'emms-browser-mode-map
    "q" 'emms-browser-bury-buffer
    "<return>" 'emms-browser-toggle-subitems
    "p" 'emms-browser-add-tracks-and-play)

;;; init.el ends here
