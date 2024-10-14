(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")
                        ("gnu" . "https://elpa.gnu.org/packages/")))
;(package-initialize)
(unless package-archive-contents
(package-refresh-contents))

;; use-package initialization
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;(setq package-check-signature nil) ; may be necessary to uncomment for initial installation on some systems
(use-package gnu-elpa-keyring-update
  :ensure t)

(defun load-env-vars (file)
  (with-temp-buffer
    (insert-file-contents file)
    (dolist (line (split-string (buffer-string) "\n" t))
      (let ((key-value (split-string line "=" t)))
        (setenv (car key-value) (cadr key-value))))))

;; use .env
(load-env-vars "~/Desktop/nixos/home/emacs/.env")
(setq local-directory (getenv "LOCAL_DIRECTORY"))
(setq remote-directory (getenv "REMOTE_DIRECTORY"))
(setq gpg-key (getenv "GPG_KEY")) ; GnuPG Key ID

(setq my-org-roam-directory local-directory) 
(setq org-agenda-files (list (concat my-org-roam-directory "/agenda")))

(add-hook 'text-mode-hook 'flyspell-mode) ; enable spell check by default
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun my-save-word ()
(interactive)
(let ((current-location (point))
       (word (flyspell-get-word)))
  (when (consp word)    
    (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package doom-themes
:config
(setq doom-themes-enable-bold t
  doom-themes-enable-italic t)
(load-theme 'doom-spacegrey t))

(use-package doom-modeline
:ensure t
:init (doom-modeline-mode 1)
:config
(setq doom-modeline-height 15
  doom-modeline-bar-width 3
  doom-modeline-minor-modes nil
  doom-modeline-lsp t
  doom-modeline-enable-word-count nil
  doom-modeline-buffer-encoding nil
  doom-modeline-buffer-file-name-style 'truncate-upto-root
  doom-modeline-icon t
  doom-modeline-major-mode-icon nil
  doom-modeline-major-mode-color-icon t))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode nil)
(setq org-startup-indented t)
(global-visual-line-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq large-file-warning-threshold nil)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
    enable-recursive-minibuffers t))

(use-package company
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.5
    company-minimum-prefix-length 1))

(use-package company-math
:after company
:config
(add-to-list 'company-backends 'company-math-symbols-latex))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-latex-create-formula-image-program 'dvisvgm) ;; sharper
  )
(add-hook 'org-mode-hook 'org-fragtog-mode)

;(setq org-format-latex-options (plist-put org-format-latex-options :scale 0.4))

(defun my-org-export-to-pdf-and-open()
  "Export current org buffer to a PDF and open with zathura"
  (interactive)
  (let ((output-file (org-latex-export-to-pdf)))
    (when output-file
      (start-process "zathura" "*zathura*" "zathura" output-file))))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
    '("~/Desktop/nixos/home/emacs/snippets"))
  (yas-global-mode 1))

(use-package writeroom-mode
  :defer t)

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t))

(defun my-bottom-terminal ()
  (interactive)
  (split-window-vertically (- (/ (window-total-height) 5)))
  (other-window 1)
  (ansi-term (getenv "SHELL"))
  (other-window 1))

(defun my-ide-layout ()
  "Simple IDE layout."
  (interactive)

  ;; launch file tree
  (neotree-show)

  ;; navigate to main window then split to open terminal at the bottom
  (other-window 1)
  (my-bottom-terminal)
  
  ;; return to main window
  (other-window 1))

(defun my-arrange ()
(interactive)
(let ((current-window (selected-window)))
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (other-buffer))
  (other-window -1)
  (select-window current-window)))

(use-package evil
:init
(setq evil-want-integration t
  evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(use-package org
:defer t
:commands (org-capture org-agenda)
:config
(setq org-startup-folded 'showall))

;(add-to-list 'org-modules 'org-habit t)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace)
  (setq org-modern-replace-stars "♠♣♥♦"))

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
(org-roam-directory my-org-roam-directory)
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

(use-package general
  :config
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"

    ;; load files from remote computer
    "c" '(connect-to-remote-computer :which-key "connect (enable: TRAMP)")

    ;; pull/push orgfiles from remote computer
    "r l" '(rsync-pull :which-key "rsync pull")
    "r s" '(rsync-push :which-key "rsync push")

    ;; emacs
    "q" '(save-buffers-kill-emacs :which-key "quit emacs")
    "e" '(eval-buffer :which-key "eval buffer")
    "u" '(org-babel-tangle :which-key "tangle (update)")
    "f s" '(save-buffer :which-key "save buffer")
    "f o" '(org-open-at-point :which-key "open point")
    "f f" '(find-file :which-key "find file")
    "r u" '(my-elfeed-update-and-show :which-key "update and show elfeed")

    ;; magit
    "m s" '(magit-status :which-key "magit status")

    ;; layouts
    "i" '(my-ide-layout :which-key "IDE layout")

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
    "l o" '(my-org-export-to-pdf-and-open :which-key "latex open")

    ;; tables
    "t -" '(org-table-insert-hline :which-key "horizontal line")
    "t r" '(org-table-insert-row :which-key "insert row")
    "t c" '(org-table-insert-column :which-key "insert column")

    ;; agenda
    "a" '(org-agenda :which-key "agenda")

    ;; toggles
    "t w" '(writeroom-mode :which-key "writeroom-mode")
    "t a" '(my-arrange :which-key "arrange horizontally")
    "t f" '(toggle-frame-fullscreen :which-key "toggle fullscreen")
    "t p" '(ready-player-mode :which-key "toggle player")
    "t i" '(org-toggle-inline-images :which-key "toggle inline images")

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

(defun my-elfeed-update-and-show ()
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

(use-package magit
  :ensure t
  :init
  ;; Load magit at startup for faster access
  (add-hook 'after-init-hook 'magit-mode))

(defun connect-to-remote-computer ()
  (interactive)
  ;(setq my-org-roam-directory remote-directory)
  (find-file remote-directory)
  )

(defun rsync-pull ()
  (interactive)
  (term "~/Desktop/nixos/home/emacs/org-roam-pull.sh")
  (message "org-roam directory pulled from server"))

(defun rsync-push ()
  (interactive)
  (term "~/Desktop/nixos/home/emacs/org-roam-push.sh")
  (message "org-roam directory pushed to server"))

(use-package lsp-mode
  :hook ((c++-mode c-mode go-mode python-mode scheme-mode emacs-lisp-mode js-mode web-mode) . lsp-deferred)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package go-mode)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
    which-key-idle-secondary-delay 0.05))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "NixOS Configuration"
    dashboard-startup-banner "~/Desktop/nixos/home/emacs/banner.txt"
    dashboard-center-content t
    dashboard-set-footer nil
    dashboard-footer-messages nil
    dashboard-items nil
    initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;(add-hook 'dashboard-mode-hook 'disable-line-numbers)

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

(setq org-agenda-timegrid-use-ampm t)

(defun refresh-org-agenda-buffers ()
  "Refresh all org-agenda buffers."
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

(use-package org-drill
:config
)

(general-define-key
  :states '(normal motion visual)
  :keymaps 'image-dired-thumbnail-mode-map
  "j" 'image-dired-display-next-thumbnail-original
  "k" 'image-dired-display-previous-thumbnail-original
  "l" 'image-dired-display-current-image-full
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

(use-package visual-fill-column)
(use-package org-present
  :config
  ; keybinds
  (define-key org-present-mode-keymap (kbd "<f4>") 'org-present-quit)
  (define-key org-present-mode-keymap (kbd "<f5>") 'org-present-prev)
  (define-key org-present-mode-keymap (kbd "<f6>") 'org-present-next)

  ; settings
  (setq org-present-hide-stars-in-headings t))

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (visual-fill-column-mode 1)
                 (org-modern-mode 0)
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 ))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (visual-fill-column-mode 0)
                 (org-modern-mode 1)
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)
                 ))
     ))

(add-to-list 'load-path "~/Desktop/nixos/home/emacs/lisp/")

(load "ready-player.el")

;; ready-player config
(use-package ready-player
  :ensure nil
  :config
  (ready-player-mode +1))

(load "pomodoro.el")
(pomodoro-add-to-mode-line)
