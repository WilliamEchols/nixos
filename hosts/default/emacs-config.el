;; Top level variables
(setq my-org-roam-directory "~/Desktop/orgfiles")

;; Package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package initialization
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; default window size
(add-to-list 'default-frame-alist '(width . 190))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'initial-frame-alist '(top . 40))
(add-to-list 'initial-frame-alist '(left . 40))

;; theme and ui
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-moonlight t))

(use-package doom-modeline
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

;; nav completion
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
    enable-recursive-minibuffers t))

;; text completion
(use-package company
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.8
    company-minimum-prefix-length 1))

(use-package company-math
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(setq org-startup-indented t)
(global-visual-line-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

;; Evil mode
(use-package evil
  :init
  (setq evil-want-integration t
    evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;; Org-mode configuration
(use-package org
  :defer t
  :commands (org-capture org-agenda)
  :config
  (setq org-startup-folded 'showall))

;; Org-agenda files
(setq org-agenda-files (list (concat my-org-roam-directory "/agenda")))

;; Org-roam setup
(use-package org-roam
  :custom
  (org-roam-directory my-org-roam-directory)
  :config
  (org-roam-setup)
  (setq org-roam-dailies-directory "daily/")
  (org-roam-db-autosync-mode))

;; open shell at bottom
(defun open-terminal-split-bottom ()
  "open a new terminal split at the bottom."
  (interactive)
  (let ((height (/ (window-total-height) 6))) ; shell height
    (split-window-below (- height)))
  (other-window 1)
  (ansi-term (getenv "SHELL"))) ; Open ansi-term with the default shell

;; IDE layout
;; TODO - replace dired with a better explorer for sidebar tree navigation
(defun ide-layout ()
  "Simple IDE layout."
  (interactive)
  (let ((width (/ (window-total-width) 6)))
    (split-window-horizontally width))
  (let ((dired-buffer (dired default-directory)))
    (dired-hide-details-mode 1)
    (setq buffer-read-only nil))
  (other-window 1)
  (open-terminal-split-bottom)
  (other-window 2))

;; RSS feeds
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
    '("https://hnrss.org/frontpage")))

(defun elfeed-update-and-show ()
  "Update elfeed and open the elfeed buffer."
  (interactive)
  (elfeed-update)
  (elfeed))

;; Keybindings with general.el and which-key
(use-package general
  :config
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"

    ;; emacs
    "q" '(save-buffers-kill-emacs :which-key "quit emacs")
    "e" '(eval-buffer :which-key "eval buffer")
    "fs" '(save-buffer :which-key "save buffer")
    "fo" '(org-open-at-point :which-key "open point")
    "ff" '(find-file :which-key "find file")
    "ru" '(elfeed-update-and-show :which-key "update and show elfeed")

    ;; layouts
    "i" '(ide-layout :which-key "IDE layout")

    ;; buffers
    "b" '(switch-to-buffer :which-key "switch buffer")
    "k" '(kill-buffer :which-key "kill buffer")
    "d" '(image-dired :which-key "image dired")

    ;; todo
    "o" '(org-todo :which-key "cycle todo status")

    ;; latex
    "lp" '(org-latex-preview :which-key "preview")
    "le" '(org-latex-export-to-pdf :which-key "export")

    ;; tables
    "t-" '(org-table-insert-hline :which-key "horizontal line")
    "tr" '(org-table-insert-row :which-key "insert row")
    "tc" '(org-table-insert-column :which-key "insert column")

    ;; agenda
    "a" '(org-agenda :which-key "agenda")

    ;; roam
    "n f" '(org-roam-node-find :which-key "roam find")
    "n i" '(org-roam-node-insert :which-key "roam insert")
    "n d N" '(org-roam-dailies-capture-today :which-key "capture today")
    "n d Y" '(org-roam-dailies-capture-yesterday :which-key "capture yesterday")
    "n d T" '(org-roam-dailies-capture-tomorrow :which-key "capture tomorrow")
    "n d n" '(org-roam-dailies-goto-today :which-key "goto today")
    "n d y" '(org-roam-dailies-goto-yesterday :which-key "goto yesterday")
    "n d t" '(org-roam-dailies-goto-tomorrow :which-key "goto tomorrow")
    "n d d" '(org-roam-dailies-goto-date :which-key "goto date")
    ))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
    which-key-idle-secondary-delay 0.05))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "GNU/Emacs"
    dashboard-startup-banner "~/Desktop/nixos/hosts/default/emacs-banner.txt"
    dashboard-center-content t
    dashboard-set-footer nil
    dashboard-footer-messages nil
    dashboard-items nil
    initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

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

;; Image dired custom navigation
(general-define-key
  :states '(normal motion visual)
  :keymaps 'image-dired-thumbnail-mode-map
  "j" 'image-dired-display-previous
  "k" 'image-dired-display-next
  "l" 'image-dired-display-this
  "h" 'image-dired-thumbnail-display-external
  "q" '(lambda () (interactive) (kill-this-buffer) (delete-other-windows)))

;; environment variables from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; update maximum file size before opening warning
(setq large-file-warning-threshold 100000000)
