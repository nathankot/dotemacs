;;; package --- Nathan's Emacs
;;; Commentary:
;;; Extensive use of `use-package`

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(custom-safe-themes
     (quote
       ("1cd9defef2a98138c732728568b04043afd321eb802d25a254777de9b2463768"
        "8f1cedf54f137f71382e3367e1843d10e173add99abe3a5f7d3285f5cc18f1a9"
        "d6e27431f8cafb4a9136aebb1d4061f895b120bf88d34ff60c390d9905bd4e36"
        "e292ec749f785d348969926448e25790356a7ce1a8fda6e695f5e8b70bed786b"
        "8022cea21aa4daca569aee5c1b875fbb3f3248a5debc6fc8cf5833f2936fbb22"
        "c56d90af9285708041991bbc780a073182cbe91721de17c6e7a8aac1380336b3"
        "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482"
        "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f"
        default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(standard-indent 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#202020"))))
 '(flycheck-error ((t (:background "red"))))
 '(font-lock-comment-face ((t (:foreground "#5E5E5E"))))
 '(fringe ((t (:background "#202020"))))
 '(git-gutter:added ((t (:foreground "#8CAC8C" :inverse-video nil :weight bold))))
 '(git-gutter:deleted ((t (:foreground "#D9A0A0" :inverse-video nil :weight bold))))
 '(git-gutter:modified ((t (:foreground "#E090C7" :inverse-video nil :weight bold))))
 '(linum ((t (:background "#202020" :foreground "color-237"))))
 '(region ((t (:background "#000000"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#B98080")))))

;; Use UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(setenv "LC_CTYPE" "UTF-8")

;; Mac Specific
(push "/opt/boxen/homebrew/bin" exec-path)
(push "/usr/local/bin" exec-path)
(setenv "PATH" (concat (getenv "PATH") ":/opt/boxen/homebrew/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)

;; Other stuff
(setq inhibit-startup-screen t)
(menu-bar-mode -1) ;; Disable menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq-default tab-width 2 indent-tabs-mode nil) ;; Spaces
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(auto-save-mode nil) ;; Disable autosaving
(show-paren-mode t) ;; Show matching parens
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1) ;; Smooth scrolling
(setq gc-cons-threshold 20000000) ;; Increase garbage collection limit
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq blink-matching-paren nil)
(setq require-final-newline t)
(setq initial-scratch-message ";; Hello, NK")
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)
(setq tags-table-list '("./" "./.git"))
(setq large-file-warning-threshold 100000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

;; ESSENTIAL PACKAGES
;; ================================================================================

(use-package hc-zenburn-theme
  :ensure t
  :init (progn
          (add-to-list 'default-frame-alist '(background-color . "#313131")
          (load-theme 'hc-zenburn t))))

(use-package diminish
  :ensure t)

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package saveplace
  :init (progn
          (setq save-place-file "~/.emacs/saveplaces")
          (setq-default save-place t)))

(use-package evil
  :commands evil-mode
  :ensure t
  :init (progn
          (setq evil-want-C-u-scroll t
                evil-overriding-maps nil
                evil-intercept-maps nil
                evil-shift-width 2
                evil-esc-delay 0 ; Prevent esc from translating to meta key in terminal mode
                ; Cursor
                evil-emacs-state-cursor  '("red" box)
                evil-normal-state-cursor '("gray" box)
                evil-visual-state-cursor '("gray" box)
                evil-insert-state-cursor '("gray" bar)
                evil-motion-state-cursor '("gray" box))
          (evil-mode 1))
  :bind ("C-q" . delete-window)
  :config (progn

            (use-package evil-leader
              :ensure t
              :commands (global-evil-leader-mode evil-leader-mode)
              :init (global-evil-leader-mode)
              :config (progn
                        (evil-leader/set-leader ",")
                        (evil-leader/set-key "w" 'save-buffer)
                        (evil-leader/set-key "i" 'evil-window-move-far-left)
                        (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)))

            (use-package evil-search-highlight-persist
               :ensure t
               :commands global-evil-search-highlight-persist
               :init (global-evil-search-highlight-persist))

            (use-package evil-commentary
              :ensure t
              :commands evil-commentary-mode
              :init (evil-commentary-mode))

            (use-package evil-snipe
              :ensure t
              :commands (global-evil-snipe-mode)
              :init     (global-evil-snipe-mode 1))

            (use-package evil-surround
              :ensure t
              :commands (global-evil-surround-mode)
              :init (global-evil-surround-mode 1))

            (use-package evil-matchit
              :ensure t
              :commands (global-evil-matchit-mode)
              :init (global-evil-matchit-mode 1))

            (use-package evil-jumper
              :ensure t
              :commands (global-evil-jumper-mode)
              :init (global-evil-jumper-mode))

            ; Window management
            (define-key evil-normal-state-map (kbd "C-q") 'delete-window)
            (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
            (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)
            (define-key evil-normal-state-map (kbd "C-l") 'evil-window-increase-width)
            (define-key evil-normal-state-map (kbd "C-h") 'evil-window-decrease-width)

            (define-key evil-normal-state-map (kbd "-")
              (lambda ()
                (interactive)
                (split-window-vertically)
                (other-window 1)))

            (define-key evil-normal-state-map (kbd "|")
              (lambda ()
                (interactive)
                (split-window-horizontally)
                    (other-window 1)))

            (evil-set-initial-state 'magit-log-edit-mode 'insert)
            (evil-set-initial-state 'git-commit-mode 'insert)

            ; Buffer Management
            (define-key evil-visual-state-map (kbd "SPC") 'evil-search-forward)
            (define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)
            (define-key evil-visual-state-map (kbd "S-SPC") 'evil-search-backward)
            (define-key evil-normal-state-map (kbd "S-SPC") 'evil-search-backward)
            (define-key evil-normal-state-map (kbd "] l") 'occur-next)
            (define-key evil-normal-state-map (kbd "[ l") 'occur-prev)
            (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
            (evil-add-hjkl-bindings outline-mode-map 'emacs)))

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . " y")
  :idle-priority 2
  :commands (yas-global-mode yas-activate-extra-mode)
  :init (progn
          (setq yas-snippet-dirs
            '("~/.snippets/yasnippet-snippets"
               "~/.snippets/personal"))
          (add-hook 'web-mode-hook (lambda () (yas-activate-extra-mode 'js-mode))))
  :config (progn
            (evil-define-key 'insert yas-minor-mode-map (kbd "C-e") 'yas-expand))
  :idle (yas-global-mode 1))

(use-package smart-mode-line
  :ensure t
  :init (sml/setup)
  :config (sml/apply-theme 'respectful))

(use-package flx-ido
  :ensure t
  :commands (flx-ido-mode)
  :init (progn
          (setq ido-enable-flex-matching t)
          (setq ido-use-faces nil)
          (setq flx-ido-threshold 10000)
          (ido-mode 1)
          (ido-everywhere 1)
          (flx-ido-mode 1)))

(use-package popwin
  :ensure t
  :commands popwin-mode
  :init (popwin-mode 1)
  :config (progn
            (evil-define-key 'normal popwin:keymap (kbd "q") 'popwin:close-popup-window)
            (push '("^\\*helm.*\\*$" :regexp t :dedicated t) popwin:special-display-config)))

(use-package helm
  :ensure t
  :commands (helm-buffer-list helm-mode)
  :init (progn
          (require 'helm-config))
  :config (progn
            (define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)
            (define-key helm-map (kbd "C-b") 'helm-keyboard-quit)
            (define-key helm-map (kbd "C-p") 'helm-keyboard-quit)
            (define-key helm-map (kbd "C-j") 'helm-next-line)
            (define-key helm-map (kbd "C-k") 'helm-previous-line)
            (define-key helm-map (kbd "C-d") 'helm-buffer-run-kill-persistent)))

(use-package projectile
  :ensure t
  :commands (projectile-global-mode projectile-mode projectile-project-root)
  :diminish projectile-mode
  :init (progn
          (setq projectile-enable-caching t)
          (setq projectile-completion-system 'ido)
          (setq projectile-indexing-method 'alien)
          (projectile-global-mode +1))
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories ".cache")
            (add-to-list 'projectile-globally-ignored-directories ".tmp")
            (add-to-list 'projectile-globally-ignored-directories "tmp")
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (add-to-list 'projectile-globally-ignored-directories "bower_components")))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile)
  :bind ("C-p" . helm-projectile)
  :init (progn
          (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)))

(use-package helm-ag
  :ensure t
  :commands (helm-projectile-ag)
  :init (progn
          (define-key evil-normal-state-map (kbd "C-s") 'helm-projectile-ag)))

(use-package helm-dash
  :ensure t
  :init (progn
          (setq helm-dash-docsets-path "~/.docset")
          (evil-leader/set-key "f" 'helm-dash-at-point)
          (define-key evil-normal-state-map (kbd "C-f") 'helm-dash))
  :config (progn
            (defun js-doc()
              (interactive)
              (setq-local helm-dash-docsets '("AngularJS" "BackboneJS" "Lo-Dash" "Javascript" "NodeJS" "jQuery" "Chai"))
              (setq helm-current-buffer (current-buffer)))

            (defun web-doc()
              (interactive)
              (setq-local helm-dash-docsets '("Javascript" "HTML" "CSS" "Lo-Dash" "jQuery" "Bootstrap_3"))
              (setq helm-current-buffer (current-buffer)))

            (add-hook 'js-mode-hook 'js-doc)
            (add-hook 'web-mode-hook 'web-doc)))

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop)
  :init (progn
          (evil-leader/set-key "sb" 'helm-swoop)
          (evil-leader/set-key "sa" 'helm-multi-swoop)))

(use-package neotree
  :ensure t
  :commands (neotree-dir neo-global--window-exists-p)
  :init (progn
          (defun projectile-neotree-project-root ()
            (interactive)
            (if (neo-global--window-exists-p)
              (neotree-hide)
              (neotree-dir (projectile-project-root))))

          (evil-leader/set-key "k b" 'projectile-neotree-project-root)
          (evil-leader/set-key "k r" 'neotree-find))
  :config (progn
            (evil-add-hjkl-bindings neotree-mode-map 'normal)
            (evil-define-key 'normal neotree-mode-map "q" 'neotree-hide
                                                      "o" 'neotree-enter
                                                      "v" 'neotree-enter-vertical-split
                                                      "r" 'neotree-refresh
                                                      "h" 'neotree-hidden-file-toggle
                                                      (kbd "m d") 'neotree-delete-node
                                                      (kbd "m a") 'neotree-create-node
                                                      (kbd "m m") 'neotree-rename-node)))

(use-package perspective
  :ensure t
  :commands (persp-mode persp-kill persp-switch persp-next persp-prev)
  :init (progn
            (define-key evil-normal-state-map (kbd "C-@") 'persp-switch)
            (define-key evil-normal-state-map (kbd ")") 'persp-next)
            (define-key evil-normal-state-map (kbd "(") 'persp-prev)
            (persp-mode))
  :config (progn
            (use-package persp-projectile :ensure t)))

(use-package autopair
  :ensure t
  :diminish autopair-mode
  :init (autopair-global-mode))

(use-package editorconfig
  :ensure t
  :init (progn
          (add-to-list 'edconf-indentation-alist '(swift-mode swift-indent-offset))))

(use-package smex
  :ensure t
  :commands smex
  :bind ("M-x" . smex))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :commands git-gutter-mode
  :init (global-git-gutter-mode +1)
  :config (progn
            (git-gutter:linum-setup)
            (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
            (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
            (evil-leader/set-key "g a" 'git-gutter:stage-hunk)
            (evil-leader/set-key "g r" 'git-gutter:revert-hunk)))

(use-package linum-relative
  :ensure t
  :init (progn
          (setq linum-relative-format "%3s   ")
          (linum-on)
          (global-linum-mode)))

(use-package magit
  :ensure t
  :commands (magit-log magit-status magit-commit magit-commit-amend
             magit-diff-unstaged magit-diff-staged magit-blame-mode
             magit-stage-all)
  :init (progn
          (evil-leader/set-key
            "g l" 'magit-log
            "g c" 'magit-commit
            "g C" 'magit-commit-amend
            "g s" 'magit-status
            "g d" 'magit-diff-unstaged
            "g D" 'magit-diff-staged
            "g b" 'magit-blame-mode
            "g w" 'magit-stage-all)
          (add-to-list 'evil-insert-state-modes 'magit-commit-mode))
  :config (progn
            (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
            (evil-add-hjkl-bindings magit-status-mode-map 'emacs)
            (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs)
            (define-key magit-diff-mode-map (kbd "j") 'magit-goto-next-section)
            (define-key magit-diff-mode-map (kbd "k") 'magit-goto-previous-section)))

(require 'flycheck)

(define-key evil-normal-state-map (kbd "] e") 'next-error)
(define-key evil-normal-state-map (kbd "[ e") 'previous-error)

; Custom checkers
(flycheck-define-checker jsxhint
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" source)
  :error-patterns ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :predicate (lambda ()
               (and (executable-find "jsxhint")
                    (buffer-file-name)
                    (string-match ".*\.jsx?$" (buffer-file-name))))
  :modes (web-mode))

(add-to-list 'flycheck-checkers 'jsxhint)
(add-to-list 'flycheck-checkers 'swift)

(global-flycheck-mode)

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup
  :init (progn
          (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

(use-package helm-flycheck
  :ensure t
  :commands helm-flycheck
  :init (evil-leader/set-key "e" 'helm-flycheck))

(use-package emmet-mode
  :diminish " e"
  :ensure t
  :commands emmet-mode
  :init (progn
          (add-hook 'sgml-mode-hook 'emmet-mode)
          (add-hook 'css-mode-hook  'emmet-mode)
          (add-hook 'web-mode-hook 'emmet-mode)))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :init (progn
          (add-hook 'css-mode-hook 'rainbow-mode)
          (add-hook 'web-mode-hook 'rainbow-mode)
          (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))


;; LANGUAGE PACKS
;; ================================================================================

(use-package js2-mode
  :ensure t
  :diminish js2-minor-mode
  :commands (js2-mode js-mode js2-minor-mode)
  :init (progn
          (setq js2-highlight-level 3)
          (setq js2-mode-show-parse-errors nil)
          (setq js2-mode-show-strict-warnings nil)
          ; Use js2-mode as a minor mode (preferred way)
          (add-hook 'js-mode-hook 'js2-minor-mode)
          (add-to-list 'interpreter-mode-alist '("node" . js-mode))))

(use-package web-mode
  :ensure t
  :commands web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html.twig\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html.jsx\\'" . web-mode))
          (add-to-list 'magic-mode-alist '("\/\*\*.*@jsx" . web-mode)))
  :config (progn
            (define-key prog-mode-map (kbd "C-x /") 'web-mode-element-close)))

(use-package less-css-mode
  :ensure t
  :commands less-css-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))))

(use-package stylus-mode
  :ensure t
  :commands stylus-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.stylus\\'" . stylus-mode))))

(use-package php-mode
  :ensure t
  :commands php-mode)

(use-package markdown-mode
  :ensure t
  :commands markdown-mode)

(use-package lua-mode
  :ensure t
  :commands lua-mode)

(use-package swift-mode
  :ensure t
  :commands swift-mode)

(use-package puppet-mode
  :ensure t
  :commands puppet-mode)

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :diminish (interactive-haskell-mode electric-indent-mode)
  :init (progn
          (add-hook 'haskell-mode-hook (lambda ()
                                         (turn-on-haskell-indentation)
                                         (interactive-haskell-mode)
                                         (evil-leader/set-key
                                            "t" 'haskell-process-do-type
                                            "ghi" 'haskell-interactive-bring
                                            "ghk" 'haskell-session-kill
                                            "ghgi" 'haskell-navigate-imports
                                            "ghfi" 'haskell-mode-format-imports)))))



;; Company and it's backends
;; ================================================================================

(use-package company
  :ensure t
  :diminish " c"
  :commands (global-company-mode company-mode)
  :idle-priority 1
  :init (progn
          (setq company-dabbrev-downcase nil))
  :config (progn
            ; Swap some keybindings
            (define-key company-active-map (kbd "C-j") 'company-select-next)
            (define-key company-active-map (kbd "C-k") 'company-select-previous)
            (define-key company-active-map (kbd "C-i") 'company-select-next)
            (define-key company-active-map (kbd "C-o") 'company-select-previous)
            ; Okay lets setup company backends the way we want it, in a single place.
            (setq company-backends
              '( company-css
                 company-elisp
                 company-clang
                 company-xcode
                 company-tern
                 company-ghc
                 ( company-dabbrev-code
                   company-etags
                   company-gtags
                   company-keywords
                   company-files
                   company-dabbrev
                   :with company-yasnippet))))
  :idle (global-company-mode))

(use-package company-ghc
  :ensure t
  :init (progn
          (use-package ghc
            :ensure t
            :init (progn
                    (add-hook 'haskell-mode-hook 'ghc-init)))))

(use-package company-tern
  :ensure t
  :init (progn
          (use-package tern
            :diminish " T"
            :commands (tern-mode)
            :ensure t
            :init (progn
                    (add-hook 'js-mode-hook 'tern-mode)))))

;; Org Mode
;; ================================================================================
(setq org-log-done 'time)
(setq org-hide-leading-stars t)
(setq org-alphabetical-lists t)
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
(setq org-directory "~/org/")
(setq org-agenda-files (list "~/org/home.org" "~/org/work.org" "~/org/notes.org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-todo-keywords
  '((sequence "TODO" "DOING" "|" "DONE" "CANCELED" "DELEGATED")))

(setq org-todo-keyword-faces
  '(("TODO" . org-warning)
    ("DOING" . "white")
    ("DONE" . "green")
    ("DELEGATED" . "purple")
    ("CANCELED" . "red")))

(use-package evil-org
  :ensure t
  :init (progn
          (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)

          (evil-leader/set-key "o c" 'org-capture
                               "o a" 'org-agenda
                               "o t" 'org-todo-list)

          (evil-leader/set-key-for-mode 'org-mode
            "d" 'org-deadline
            "s" 'org-schedule
            "c" 'org-toggle-checkbox
            "o" (lambda ()
                  (interactive)
                  (evil-org-eol-call (quote org-insert-heading-respect-content))))

          (evil-define-key 'normal org-mode-map
            (kbd "m") 'org-set-tags
            (kbd "+") 'org-priority-up)

          (evil-define-key 'normal org-agenda-mode-map
            (kbd "d") 'org-agenda-deadline
            (kbd "s") 'org-agenda-schedule
            (kbd "+") 'org-priority-up
            (kbd "q") 'org-agenda-Quit)))

(add-hook 'org-mode-hook 'org-indent-mode)

;; Ledger-mode
;; ================================================================================
(use-package ledger-mode
  :ensure t
  :config (progn
            (evil-leader/set-key-for-mode 'ledger-mode
              "o" 'ledger-add-transaction
              "r" 'ledger-reconcile)))

;; Initialize by starting an org mode perspective

(persp-switch "org")

(if (file-exists-p "~/org/home.org")
  (progn
    (find-file "~/org/home.org")))

(if (file-exists-p "~/org/work.org")
  (progn
    (split-window-right)
    (find-file "~/org/work.org")))

(persp-switch "main")

(provide 'init)
;;; init.el ends here
