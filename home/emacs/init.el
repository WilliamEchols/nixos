;; package management
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

;; load environment variables from a file
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

;; ERC NickServ registration
(setq erc-nick (getenv "ERC_NICK"))
(setq erc-user-full-name (getenv "ERC_NAME"))
(setq erc-sasl-password (getenv "ERC_PASS"))

;; initialize
(setq my-org-roam-directory local-directory)
(setq browse-url-browser-function 'eww-browse-url) ; set eww as the default browser
(add-hook 'text-mode-hook 'flyspell-mode) ; enable spell check by default
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; function to add word to flyspell dictionary
(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))


;; default window size
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; theme and ui
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-moonlight t))

;; transparent background
(set-frame-parameter nil 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))

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
  (setq company-idle-delay 0.2
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

;; Org-bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◆" "♠" "♣" "♦")))

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

;; tree
(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t))

;; bottom terminal
(defun my-bottom-terminal ()
  (interactive)
  (split-window-vertically (- (/ (window-total-height) 5)))
  (other-window 1)
  (ansi-term (getenv "SHELL"))
  (other-window 1))

;; IDE layout
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

;; RSS feeds
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
    '(("https://xkcd.com/rss.xml" xkcd)
      ("https://hnrss.org/frontpage" hackernews)
      ("https://rss.arxiv.org/rss/q-fin" arxiv q-fin)
      ("https://stephango.com/feed.xml" kepano)
      ("https://planet.emacslife.com/atom.xml" emacs)
     )))

(defun elfeed-update-and-show ()
  "Update elfeed and open the elfeed buffer."
  (interactive)
  (elfeed-update)
  (elfeed))

(general-define-key
 :keymaps 'elfeed-search-mode-map
 :states '(normal motion)
 "r" 'elfeed-update
 "RET" 'elfeed-search-show-entry)

;; eww keybinds
(general-define-key
 :keymaps 'eww-mode-map
 :states 'normal
 "q" 'eww-back-url)

;; magit (git integration)
(use-package magit
  :ensure t
  :init
  ;; Load magit at startup for faster access
  (add-hook 'after-init-hook 'magit-mode))

;; Use tramp to remotely connect to server
(defun connect-to-remote-computer ()
  (interactive)
  (find-file remote-directory))

;; ERC (Emacs IRC Client)
(require 'erc)
(require 'erc-sasl)

(defun erc-sasl-authenticate ()
  "Perform SASL authentication."
  (erc-response-eval
   (if (and (featurep 'erc-sasl) erc-session-password)
       (erc-server-send (format "CAP REQ :sasl")
                        (erc-server-send (format "AUTHENTICATE PLAIN")
                                         (erc-server-send (base64-encode-string
                                                           (format "%s\0%s\0%s"
                                                                   erc-session-password
                                                                   erc-nick
                                                                   erc-session-password))))))
   (erc-server-send "CAP END")))

(setq erc-sasl-use-sasl t)

(defun my-erc-connect ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697
           :nick erc-nick
           :password erc-sasl-password
           :full-name erc-user-full-name))

;; TODO - programming language support (highlighting, etc) (ideally a unified package for multiple languages)
;; Do we need an LSP? elgot?

;; Keybindings with general.el and which-key
(use-package general
  :config
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"

    ;; connect to remote computer
    "c" '(connect-to-remote-computer :which-key "connect (TRAMP)")
    
    ;; emacs
    "q" '(save-buffers-kill-emacs :which-key "quit emacs")
    "e" '(eval-buffer :which-key "eval buffer")
    "fs" '(save-buffer :which-key "save buffer")
    "fo" '(org-open-at-point :which-key "open point")
    "ff" '(find-file :which-key "find file")
    "ru" '(elfeed-update-and-show :which-key "update and show elfeed")

    ;; magit
    "ms" '(magit-status :which-key "magit status")
    
    ;; layouts
    "i" '(my-ide-layout :which-key "IDE layout")

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
    "n r" '(org-roam-node-random :which-key "random node")
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
  (setq dashboard-banner-logo-title "GNU Emacs"
    dashboard-startup-banner "~/Desktop/nixos/home/emacs/banner.txt"
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

(general-define-key
  :states '(normal motion visual)
  :keymaps 'dired-mode-map
  "q" '(lambda () (interactive) (kill-this-buffer) (delete-other-windows)))

(general-define-key
  :states '(normal motion visual)
  :keymaps 'image-dired-image-mode-map
  "q" '(lambda () (interactive) (kill-this-buffer) (delete-other-windows)))

;; update maximum file size before opening warning
(setq large-file-warning-threshold 100000000)
