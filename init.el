;;; package --- Nathan's Emacs
;;; Commentary:
;;; Extensive use of `use-package`

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/vendor/use-package")
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(custom-safe-themes
     (quote
       ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))

 '(safe-local-variable-values (quote ((c-file-offsets (innamespace . 0))))))

;; Use UTF-8 encoding
(setq debug-on-error nil)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(setenv "LC_CTYPE" "UTF-8")

;; OSX Specific
(if (eq system-type 'darwin)
  (progn
    (setq interprogram-cut-function
      (lambda (text &optional push)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc)))))
    (defun pbpaste ()
      "Call pbpaste"
      (interactive)
      (shell-command-to-string "pbpaste"))))

;; Other stuff
(setq inhibit-startup-screen t)
(menu-bar-mode -1) ;; Disable menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq-default tab-width 2 indent-tabs-mode nil) ;; Spaces
(setq standard-indent 2)
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
(setq large-file-warning-threshold 100000000)
(column-number-mode 1)

(use-package exec-path-from-shell
  :ensure t
  :init (progn
          (exec-path-from-shell-initialize)))

;; Theme.
;; ================================================================================
(use-package hc-zenburn-theme
  :load-path "vendor/hc-zenburn-theme"
  :init (progn
          (require 'hc-zenburn-theme)
          (load-theme 'hc-zenburn t)))

(use-package smart-mode-line
  :ensure t
  :init (progn
          (setq sml/shorten-directory t)
          (setq sml/shorten-modes t)
          (setq sml/name-width 30)
          (setq sml/numbers-separator "")
          (setq sml/show-trailing-N nil)
          (setq sml/show-frame-identification nil)
          (setq sml/mule-info nil)
          (setq sml/show-client nil)
          (setq sml/show-remote nil)
          (setq sml/position-percentage-format nil)
          (sml/setup))
  :config (sml/apply-theme 'respectful))


;; EVIL
;; ================================================================================
(use-package evil
  :ensure t
  :init (progn
          (setq evil-want-C-u-scroll t
                evil-overriding-maps nil
                evil-intercept-maps nil
                evil-esc-delay 0 ; Prevent esc from translating to meta key in terminal mode
                ; Cursor
                evil-emacs-state-cursor  '("red" box)
                evil-normal-state-cursor '("gray" box)
                evil-visual-state-cursor '("gray" box)
                evil-insert-state-cursor '("gray" bar)
                evil-motion-state-cursor '("gray" box))
          (evil-mode 1))
  :config (progn
            (use-package evil-leader
              :ensure t
              :init (global-evil-leader-mode)
              :config (progn
                        (evil-leader/set-leader ",")
                        (evil-leader/set-key "w" 'save-buffer)
                        (evil-leader/set-key "i" 'evil-window-move-far-left)
                        (evil-leader/set-key "a" 'align-regexp)
                        (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)
                        (evil-leader/set-key "m i" (lambda () (interactive) (shell-make "install")))
                        (evil-leader/set-key "m r" (lambda () (interactive) (shell-make "run")))
                        (evil-leader/set-key "m b" (lambda () (interactive) (shel-make "build")))
                        (evil-leader/set-key "m c" (lambda () (interactive) (shel-make "clean")))
                        (evil-leader/set-key "m s" (lambda () (interactive) (shel-make "setup")))
                        (evil-leader/set-key "m t" (lambda () (interactive) (shell-make "test")))))

            (use-package evil-search-highlight-persist
               :ensure t
               :init (global-evil-search-highlight-persist))

            (use-package evil-commentary
              :ensure t
              :init (evil-commentary-mode))

            (use-package evil-snipe
              :ensure t
              :diminish evil-snipe-mode
              :init     (progn
                          (evil-snipe-mode 1)
                          (evil-snipe-override-mode 1)))

            (use-package evil-surround
              :ensure t
              :init (global-evil-surround-mode 1))

            (use-package evil-matchit
              :ensure t
              :init (global-evil-matchit-mode 1))

            (use-package evil-jumper
              :ensure t
              :init (global-evil-jumper-mode))

            ; Window management
            (global-set-key (kbd "C-q") 'delete-window)
            (global-set-key (kbd "C-j") 'evil-window-next)
            (global-set-key (kbd "C-k") 'evil-window-prev)
            (define-key evil-normal-state-map (kbd "C-q") 'delete-window)
            (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
            (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)
            (define-key evil-normal-state-map (kbd "C-l") 'evil-window-increase-width)
            (define-key evil-normal-state-map (kbd "C-h") 'evil-window-decrease-width)
            (define-key evil-insert-state-map (kbd "C-j") 'comment-indent-new-line)

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

            ; Buffer Management
            (define-key evil-visual-state-map (kbd "SPC") 'evil-search-forward)
            (define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)
            (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
            (evil-add-hjkl-bindings outline-mode-map 'emacs)
            (evil-add-hjkl-bindings occur-mode-map 'emacs)
            (define-key evil-visual-state-map (kbd "C-i") 'indent-region)))


;; Visual utilities
;; ================================================================================
(use-package diminish
  :ensure t
  :config (progn
            (diminish 'isearch-mode " ?")))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :init (progn
          (add-hook 'css-mode-hook 'rainbow-mode)
          (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))

(use-package linum-relative
  :demand
  :ensure t
  :init (progn
          (global-linum-mode)
          (setq linum-relative-format "%3s   "))
  :config (progn
            (linum-relative-on)))

(use-package popwin
  :ensure t
  :commands (popwin-mode)
  :init (popwin-mode 1)
  :config (progn
            (evil-define-key 'normal popwin:keymap (kbd "q") 'popwin:close-popup-window)
            ;; Let's override the popwin defaults
            (setq popwin:special-display-config  '(("^\\*magit:.*\\*$" :regexp t :position top :height 20)
                                                   ("^\\*helm.*\\*$" :regexp t :position bottom)
                                                   ("^\\*shell:.*\\*$" :regexp t :position bottom :noselect t :tail t :stick t)
                                                   (help-mode :position bottom :noselect t :stick t)
                                                   (completion-list-mode :noselect t)
                                                   (grep-mode :noselect t)
                                                   (occur-mode :noselect t)
                                                   ("*Warnings*" :noselect t)
                                                   ("*GHC Error*" :noselect t)
                                                   ("*Miniedit Help*" :noselect t)
                                                   ("*undo-tree*" :width 60 :position right)))))

(use-package autopair
  :ensure t
  :diminish autopair-mode
  :init (autopair-global-mode))


;; Misc.
;; ================================================================================
(use-package undo-tree :diminish undo-tree-mode)

(use-package editorconfig
  :ensure t
  :config (progn
            (add-to-list 'edconf-indentation-alist '(swift-mode swift-indent-offset))
            (add-to-list 'edconf-indentation-alist '(haskell-mode haskell-indent-spaces))
            (add-to-list 'edconf-indentation-alist '(evil-mode evil-shift-width))))

(use-package smex
  :ensure t
  :commands smex
  :bind (("M-x" . smex)
         ("≈" . smex)))

(use-package shell
  :functions shell-make
  :init (progn
          (evil-set-initial-state 'shell-mode 'normal)
          (add-hook 'shell-mode-hook 'read-only-mode)
          (defun shell-make (command)
            "Call `make *command*` in the projectile root directory under a buffer named '*shell:make*'"
           (interactive)
            (projectile-with-default-dir (projectile-project-root)
              (async-shell-command (format "make %s" command) (format "*shell:make %s*" command)))))
  :config (progn
            (evil-define-key 'normal shell-mode-map (kbd "q") 'delete-window)
            (define-key shell-mode-map (kbd "C-c C-c") (lambda () (interactive) (delete-process (buffer-name))))))


;; File and buffer management
;; ================================================================================
(use-package projectile
  :ensure t
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

(use-package neotree
  :ensure t
  :commands (neotree-dir neo-global--window-exists-p)
  :preface (progn
             (defun projectile-neotree-project-root ()
                (interactive)
                (if (neo-global--window-exists-p)
                  (neotree-hide)
                  (if (projectile-project-p)
                    (neotree-dir (projectile-project-root))
                    (neotree)))))
  :init (progn
          (evil-leader/set-key "k b" 'projectile-neotree-project-root)
          (evil-leader/set-key "k r" 'neotree-find))
  :config (progn
            (evil-add-hjkl-bindings neotree-mode-map 'normal)
            (evil-define-key 'normal neotree-mode-map
              "q" 'neotree-hide
              "o" 'neotree-enter
              "v" 'neotree-enter-vertical-split
              "r" 'neotree-refresh
              "h" 'neotree-hidden-file-toggle
              (kbd "m d") 'neotree-delete-node
              (kbd "m a") 'neotree-create-node
              (kbd "m m") 'neotree-rename-node)))

(use-package perspective
  :ensure t
  :init (progn
            (define-key evil-normal-state-map (kbd "C-@") 'persp-switch)
            (define-key evil-normal-state-map (kbd ")") 'persp-next)
            (define-key evil-normal-state-map (kbd "(") 'persp-prev)
            (evil-leader/set-key "p k" 'persp-kill)
            (persp-mode))
  :config (progn
            (use-package persp-projectile :ensure t)))


;; Git Utilities
;; ================================================================================
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1)
  :config (progn
            (git-gutter:linum-setup)
            (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
            (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
            (evil-leader/set-key "g a" 'git-gutter:stage-hunk)
            (evil-leader/set-key "g r" 'git-gutter:revert-hunk)))

(use-package magit
  :ensure t
  :init (progn
          (evil-set-initial-state 'git-commit-mode 'insert)
          (evil-set-initial-state 'git-commit-major-mode 'insert)
          (evil-set-initial-state 'text-mode 'insert) ;; Git-commit is text-mode major
          (evil-leader/set-key
            "g l" 'magit-log
            "g c" 'magit-commit
            "g C" 'magit-commit-amend
            "g s" 'magit-status
            "g d" 'magit-diff-unstaged
            "g D" 'magit-diff-staged
            "g b" 'magit-blame
            "g w" 'magit-stage-file))
  :config (progn
            (setq magit-keymaps
              '(magit-mode-map magit-log-mode-map magit-refs-mode-map
                 magit-diff-mode-map magit-stash-mode-map magit-blame-mode-map
                 magit-reflog-mode-map magit-status-mode-map magit-tag-section-map
                 magit-cherry-mode-map magit-hunk-section-map magit-file-section-map
                 magit-process-mode-map magit-stashes-mode-map magit-revision-mode-map
                 magit-log-read-revs-map magit-stash-section-map magit-staged-section-map
                 magit-remote-section-map magit-commit-section-map magit-branch-section-map
                 magit-stashes-section-map magit-log-select-mode-map magit-unpulled-section-map
                 magit-unstaged-section-map magit-unpushed-section-map magit-untracked-section-map
                 magit-module-commit-section-map))
            (dolist (map-name magit-keymaps)
              (let* ((map (symbol-value map-name)))
                (-when-let (def (lookup-key map "v"))
                  (define-key map "V" def)
                  (define-key map "v" nil))
                (-when-let (def (lookup-key map "k"))
                  (define-key map "K" def)
                  (define-key map "k" nil))
                (evil-add-hjkl-bindings map 'emacs
                  "v" 'evil-visual-char)))))


;; Flycheck
;; ================================================================================
(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . " f")
  :init (progn
          (setq flycheck-check-syntax-automatically '(mode-enabled idle-change))
          (global-flycheck-mode))
  :config (progn
            (define-key evil-normal-state-map (kbd "] e") 'next-error)
            (define-key evil-normal-state-map (kbd "[ e") 'previous-error)))


;; Yasnippet
;; ================================================================================
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . " y")
  :init (progn
          (setq yas-snippet-dirs '("~/.emacs.d/.snippets/yasnippet-snippets"
                                    "~/.emacs.d/.snippets/personal"))
          (yas-global-mode 1))
  :config (progn
            (evil-define-key 'insert yas-minor-mode-map (kbd "C-e") 'yas-expand)
            (define-key yas-keymap (kbd "C-e") 'yas-next-field-or-maybe-expand)
            (define-key yas-keymap (kbd "C-i") nil)
            (define-key yas-keymap (kbd "C-o") nil)
            (define-key yas-keymap (kbd "<DEL>") nil)))


;; Helm
;; ================================================================================
(use-package helm
  :ensure t
  :init (progn
          (require 'helm-config)
          (helm-mode 1))
  :config (progn
            (helm-autoresize-mode 1)
            (define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)

            (define-key helm-map (kbd "C-b") 'helm-keyboard-quit)
            (define-key helm-map (kbd "C-p") 'helm-keyboard-quit)
            (define-key helm-map (kbd "C-j") 'helm-next-line)
            (define-key helm-map (kbd "C-k") 'helm-previous-line)

            ; Helm buffers
            (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-buffers)

            (use-package helm-projectile
              :ensure t
              :bind ("C-p" . helm-projectile)
              :init (progn
                      (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile))
              :config (progn
                      (define-key helm-map (kbd "C-l") 'projectile-invalidate-cache)))

            (use-package helm-ag
              :ensure t
              :commands (helm-projectile-ag)
              :init (progn
                      (use-package grep)
                      (define-key evil-normal-state-map (kbd "C-s") 'helm-projectile-ag)))

            (use-package helm-dash
              :ensure t
              :init (progn
                      (setq helm-dash-docsets-path "~/.docset")
                      (setq helm-dash-common-docsets '("Lo-Dash" "HTML" "CSS"))
                      (evil-leader/set-key "f" 'helm-dash-at-point)
                      (define-key evil-normal-state-map (kbd "C-f") 'helm-dash))
              :config (progn
                        (add-hook 'prog-mode-hook
                          (lambda ()
                            (interactive)
                            (setq helm-current-buffer (current-buffer))))))

            (use-package helm-swoop
              :ensure t
              :commands (helm-swoop)
              :init (progn
                      (evil-leader/set-key "sb" 'helm-swoop)
                      (evil-leader/set-key "sa" 'helm-multi-swoop-all)))

            (use-package helm-flycheck
              :ensure t
              :commands helm-flycheck
              :init (evil-leader/set-key "e l" 'helm-flycheck))))



;; Company mode
;; ================================================================================
(use-package company
  :ensure t
  :diminish " c"
  :defines company-dabbrev-downcase
  :init (progn
          (setq company-dabbrev-downcase nil)
          (global-company-mode))
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
                 ;; company-sourcekit
                 ( company-capf
                   company-dabbrev-code
                   ;; company-etags
                   ;; company-gtags
                   company-keywords
                   company-files
                   company-dabbrev
                   :with company-yasnippet)))))


;; LANGUAGE PACKS
;; ================================================================================

(use-package js2-mode
  :ensure t
  :diminish js2-minor-mode
  :commands (js2-mode js-mode js2-minor-mode)
  :defines helm-dash-docsets
  :init (progn
          (use-package tern
            :diminish " T"
            :commands (tern-mode)
            :ensure t
            :init (progn
                    (add-hook 'js-mode-hook 'tern-mode)))
          (use-package company-tern
            :ensure t
            :config (progn
                      (add-to-list 'company-backends 'company-tern)))
          (setq js2-highlight-level 3)
          (setq js2-mode-show-parse-errors nil)
          (setq js2-mode-show-strict-warnings nil)
                                        ; Use js2-mode as a minor mode (preferred way)
          (add-hook 'js-mode-hook 'js2-minor-mode)
          (add-to-list 'interpreter-mode-alist '("node" . js-mode)))
  :config (progn
            (add-hook 'js2-minor-mode-hook
              (lambda ()
                (interactive)
                (setq-local helm-dash-docsets
                  '("AngularJS"
                     "BackboneJS"
                     "Lo-Dash"
                     "Javascript"
                     "NodeJS"
                     "jQuery"
                     "Chai"))))))

(use-package web-mode
  :ensure t
  :preface (progn
             (defun jsxhint-predicate ()
               (and (executable-find "jsxhint")
                 (buffer-file-name)
                 (string-match ".*\.jsx?$" (buffer-file-name)))))
  :commands web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html.twig\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html.jsx\\'" . web-mode))
          (add-to-list 'magic-mode-alist '("\/\*\*.*@jsx" . web-mode))

          (flycheck-define-checker jsxhint
            "A JSX syntax and style checker based on JSXHint."
            :command ("jsxhint" source)
            :error-patterns ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
            :predicate jsxhint-predicate
            :modes (web-mode))

          (add-to-list 'flycheck-checkers 'jsxhint)

          (use-package emmet-mode
            ;; Can't diminish this, because the logic relies on
            ;; reading the mode-line.
            ;; :diminish " e"
            :ensure t
            :commands emmet-mode
            :init (progn
                    (add-hook 'sgml-mode-hook 'emmet-mode)
                    (add-hook 'css-mode-hook  'emmet-mode)
                    (add-hook 'web-mode-hook 'emmet-mode)
                    (evil-define-key 'insert emmet-mode-keymap
                      (kbd "C-j") 'emmet-expand-line))))
  :config (progn
            (add-hook 'web-mode-hook
              (lambda ()
                (interactive)
                (setq-local helm-dash-docsets '("Javascript" "HTML" "CSS" "Lo-Dash" "jQuery" "Bootstrap_3"))))
            (add-hook 'web-mode-hook (lambda () (yas-activate-extra-mode 'js-mode)))
            (add-hook 'web-mode-hook 'rainbow-mode)
            (define-key prog-mode-map (kbd "C-x /") 'web-mode-element-close)))

(use-package fish-mode
  :ensure t
  :commands fish-mode)

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

(use-package scss-mode
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
          (setq scss-compile-at-save nil)))

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
  :load-path "vendor/swift-mode"
  :commands swift-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode)))
  :config (progn
            (add-hook 'swift-mode-hook
              (lambda () (interactive)
                (setq-local helm-dash-docsets '("iOS"))))))

(use-package dockerfile-mode
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))))

(use-package puppet-mode
  :ensure t
  :commands puppet-mode)

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package stack-mode
  :load-path "vendor/stack-ide/stack-mode"
  :init (progn
          (flycheck-disable-checker 'haskell-ghc)
          (flycheck-disable-checker 'haskell-stack-ghc)
          (add-hook 'haskell-mode-hook 'stack-mode)))

(use-package haskell-mode
  :ensure t
  :commands (haskell-mode haskell-interactive-mode)
  :init (progn
          (setq haskell-hoogle-url "https://www.stackage.org/lts/hoogle?q=%s")
          (setq haskell-process-type 'stack-ghci)
          (add-hook 'haskell-mode-hook (lambda () (turn-on-haskell-indentation)))
          (evil-define-key 'normal haskell-mode-map (kbd "?") 'hoogle)
          (evil-leader/set-key-for-mode 'haskell-mode
              "t" 'stack-mode-type
              "i" 'stack-mode-info)))

;; Org Mode
;; ================================================================================
(defvar org-log-done 'time)
(defvar org-hide-leading-stars nil)
(defvar org-alphabetical-lists t)
(defvar org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(defvar org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(defvar org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(defvar org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
(defvar org-directory "~/.org/")
(defvar org-agenda-files (list "~/.org/home.org" "~/.org/work.org" "~/.org/notes.org"))
(defvar org-default-notes-file (concat org-directory "/notes.org"))
(defvar org-enforce-todo-checkbox-dependencies t)
(defvar org-enforce-todo-dependencies t)
(defvar org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE" "CANCELED" "DELEGATED")))
(defvar org-agenda-files (quote ("~/.org/home.org" "~/.org/work.org" "~/.org/notes.org")))
(defvar org-todo-keyword-faces
  '(("TODO" . org-warning)
     ("DOING" . "white")
     ("DONE" . "green")
     ("DELEGATED" . "purple")
     ("CANCELED" . "red")))

(use-package evil-org
  :ensure t
  :init (progn
          (add-hook 'org-agenda-mode-hook
            (lambda ()
              (local-unset-key (kbd ",")) ;; Don't shadow the <leader>
              ;; Autosave:
              (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
              (auto-save-mode))))
  :config (progn
            (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)

            (evil-leader/set-key "o c" 'org-capture
              "o a" 'org-agenda
              "o t" 'org-todo-list)

            (evil-leader/set-key "o b"
              (lambda ()
                (interactive)
                (let ((persp (gethash "org" perspectives-hash)))
                  (if (null persp)
                                        ; When perspective doesn't exist
                    (progn
                      (persp-switch "org")
                      (if (file-exists-p "~/.org/home.org")
                        (progn
                          (find-file "~/.org/home.org")))
                      (if (file-exists-p "~/.org/work.org")
                        (progn
                          (split-window-right)
                          (find-file "~/.org/work.org"))))
                                        ; Or when it already exists
                    (persp-activate persp)))))

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
              (kbd "q") 'org-agenda-Quit
              (kbd "w") 'org-save-all-org-buffers)

            (evil-define-key 'normal evil-org-mode-map
              (kbd "ø") '(lambda () (interactive)
                           (evil-org-eol-call
                             '(lambda()
                                (org-insert-heading)
                                (org-metaright)))))))

(add-hook 'org-mode-hook 'org-indent-mode)

;; Ledger-mode
;; ================================================================================
(use-package ledger-mode
  :ensure t
  :commands ledger-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode)))
  :config (progn
            (evil-define-key 'normal ledger-mode-map
              (kbd "Y") 'ledger-copy-transaction-at-point
              (kbd "C") 'ledger-post-edit-amount
              (kbd "!") 'ledger-post-align-postings)

            (evil-leader/set-key-for-mode 'ledger-mode
              "n" 'ledger-add-transaction
              "r" 'ledger-reconcile
              "d" 'ledger-delete-current-transaction
              "?" 'ledger-display-balance-at-point)))

(use-package flycheck-ledger :ensure t)

;; Load any local configuration if it exists

(if (file-exists-p (expand-file-name ".emacs.el"))
  (progn
    (load (expand-file-name ".emacs.el"))))

(if (file-exists-p (expand-file-name "README.org"))
  (progn
    (add-to-list 'org-agenda-files (expand-file-name "README.org"))))

(if (file-exists-p (expand-file-name "project.org"))
  (progn
    (add-to-list 'org-agenda-files (expand-file-name "project.org"))))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
