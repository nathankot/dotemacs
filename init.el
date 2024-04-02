;;; package --- Nathan's Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" "434f947950121d38d6246a4712014447a367a8e1b3a8c818c0b70e9a464147e7" "e70e87ad139f94d3ec5fdf782c978450fc2cb714d696e520b176ff797b97b8d2" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(safe-local-variable-values
    '((lsp-rust-analyzer-checkonsave-features .
        ["ssr"])
       (lsp-rust-analyzer-rustfmt-extra-args .
         [])
       (lsp-rust-analyzer-rustfmt-override-command .
         ["leptosfmt"
           (\, "--stdin")
           (\, "--rustfmt")])
       (lsp-rust-features .
         ["ssr"])
       (lsp-enabled-clients ruby-ls)
       (lsp-enabled-clients solargraph-ls)
       (lsp-enabled-clients solargraph)
       (lsp-enabled-clients deno-ls tailwindcss)
       (lsp-enabled-clients deno-ls))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package dash :straight t)

(use-package exec-path-from-shell
  :straight t
  :init
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; Basic settings.
;; ================================================================================

(when (eq system-type 'darwin)
  (setq interprogram-cut-function
    (lambda (text &optional _)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc)))))
  (defun pbpaste ()
    "Call pbpaste and insert the results"
    (interactive)
    (insert (shell-command-to-string "pbpaste"))))

(setq debug-on-error nil)
(setq ad-redefinition-action 'accept)
(setq locale-coding-system 'utf-8)
(setq load-prefer-newer t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(setenv "LC_CTYPE" "UTF-8")
(setq inhibit-startup-screen t)
(setq-default tab-width 2 indent-tabs-mode nil) ;; Spaces
(setq standard-indent 2)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1) ;; Smooth scrolling
(setq gc-cons-threshold 134217728) ;; Increase garbage collection limit
(setq read-process-output-max (* 1024 1024))
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq blink-matching-paren nil)
(setq initial-scratch-message ";; Hello.")
(fset 'yes-or-no-p 'y-or-n-p)
(setq large-file-warning-threshold 100000000)
(setq truncate-lines t)
;; allow remembering risky variables
(defun risky-local-variable-p (sym &optional _ignored) nil)

;; Interactive helper functions.
;; ================================================================================

(defun print-point ()
  "Print the current point under the cursor.  Useful for debugging."
  (interactive)
  (message "%d" (point)))

(defun align-qmk-keycodes ()
  "Used to align keycodes in qmk_keyboard"
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   ",\\([ \t]+\\)" 1 1 t))

;; Theme.
;; ================================================================================

(use-package solarized-theme
  :straight t
  :init
  (load-theme 'solarized-light)
  :config
  (setq solarized-use-less-bold t))

;; (use-package hyperfuse-theme
;;   :load-path "vendor/hyperfuse-theme"
;;   :init
;;   (require 'hyperfuse-theme)
;;   ;; (load-theme 'hyperfuse t)
;;   )

;; (use-package kanagawa-theme
;;   :load-path "vendor/emacs-kanagawa-theme"
;;   :init
;;   (require 'kanagawa-theme)
;;   (load-theme 'kanagawa))

;; (use-package zenburn-theme
;;   :load-path "vendor/zenburn-emacs"
;;   :init
;;   (require 'zenburn-theme)
;;   (load-theme 'zenburn))

(use-package randomize-region
  :load-path "vendor/randomize-region"
  :init
  (require 'randomize-region))

(use-package diminish :straight t)

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history nil)
  :config
  ;; https://github.com/syl20bnr/spacemacs/issues/14064#issuecomment-723324830
  (defun undo-tree-overridden-undo-bindings-p () nil))

;; EVIL
;; ================================================================================

(use-package evil
  :straight t
  :preface
  (defun split-window-vertically-and-switch ()
    (interactive)
    (split-window-vertically)
    (other-window 1))

  (defun split-window-horizontally-and-switch ()
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun evil-apply-emacs-state-defaults (mode)
    (let ((map (symbol-value (intern (concat (symbol-name mode) "-map")))))
      (evil-define-key 'emacs map
        (dolist (k '("C-j" "C-k" "C-q"))
          (-when-let (def (lookup-key map (kbd k)))
            (define-key map (kbd k) nil)))
        (kbd "C-e") 'evil-scroll-line-down
        (kbd "C-y") 'evil-scroll-line-up
        (kbd "C-u") 'evil-scroll-up
        (kbd "C-d") 'evil-scroll-down)))

  (defun evil-apply-motion-state-defaults (mode)
    (let ((map (symbol-value (intern (concat (symbol-name mode) "-map")))))
      (dolist (k '("C-j" "C-k" "C-q" "h" "j" "k" "l" "v" "m" "p" "n" "z"))
        (-when-let (def (lookup-key map (kbd k)))
          (or (string-prefix-p "C-" k)
            (define-key map (kbd (upcase k)) def))
          (define-key map (kbd k) nil)))
      (evil-define-key 'motion map
        (kbd "m") nil
        (kbd "C-e") 'evil-scroll-line-down
        (kbd "C-y") 'evil-scroll-line-up
        (kbd "C-u") 'evil-scroll-up
        (kbd "C-d") 'evil-scroll-down
        (kbd "{") 'evil-backward-paragraph
        (kbd "}") 'evil-forward-paragraph
        (kbd "SPC") 'evil-search-forward
        (kbd "/") 'evil-search-forward
        (kbd "n") 'evil-search-next
        (kbd "M") 'evil-window-middle
        (kbd "H") 'evil-window-top
        (kbd "L") 'evil-window-bottom
        (kbd "gg") 'evil-goto-first-line
        (kbd "z L") 'evil-scroll-right
        (kbd "z H") 'evil-scroll-left
        (kbd ", SPC") 'evil-search-highlight-persist-remove-all
        (kbd ", ,") 'writeroom-mode
        (kbd "0") 'evil-beginning-of-line)))

  :init
  (setq evil-emacs-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-undo-system 'undo-tree)
  (global-undo-tree-mode 1)

  (setq
    evil-want-C-u-scroll t
    evil-overriding-maps nil
    evil-intercept-maps nil
    evil-esc-delay 0 ; Prevent esc from translating to meta key in terminal mode
                                        ; Cursor
    evil-emacs-state-cursor  '("red" box)
    evil-normal-state-cursor '("gray" box)
    evil-visual-state-cursor '("gray" box)
    evil-insert-state-cursor '("gray" bar)
    evil-motion-state-cursor '("gray" box))

  :config

  (add-function :after (symbol-function 'evil-set-initial-state)
    (lambda (mode state)
      (cond
        ((eq state 'emacs) (evil-apply-emacs-state-defaults mode))
        ((eq state 'motion) (evil-apply-motion-state-defaults mode)))))

  (use-package evil-leader
    :straight t
    :config
    (evil-leader/set-leader ",")
    (global-evil-leader-mode))

  (use-package evil-snipe
    :straight t
    :config
    (evil-snipe-mode 1)
    (evil-snipe-override-mode 1))

  (use-package evil-search-highlight-persist
    :straight t
    :config
    (global-evil-search-highlight-persist))

  (use-package evil-commentary
    :straight t
    :config
    (evil-commentary-mode))

  (use-package evil-surround
    :straight t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-matchit
    :straight t
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-visual-mark-mode
    :straight t
    :config
    ;; Marks aren't cleared after deletion until evil-visual-mark-mode re-renders:
    (advice-add #'evil-delete-marks :after
      (lambda (&rest args)
        (evil-visual-mark-render)))
    (evil-visual-mark-mode 1))

  ;; Remove pesky combos
  (global-unset-key (kbd "M-u"))
  (global-unset-key (kbd "M-h"))
  (global-unset-key (kbd "M-j"))
  (global-unset-key (kbd "M-k"))
  (global-unset-key (kbd "M-l"))
  (global-unset-key (kbd "C-w"))

  ;; Remove keys that we want to use
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-normal-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "C-w") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-emacs-state-map (kbd "C-w") nil)

  (define-key evil-normal-state-map (kbd "u") 'evil-undo)
  (define-key evil-normal-state-map (kbd "U") 'evil-undo)

  ;; Window management
  (global-set-key (kbd "C-q") 'delete-window)
  (global-set-key (kbd "C-j") 'evil-window-next)
  (global-set-key (kbd "C-k") 'evil-window-prev)
  (define-key evil-normal-state-map (kbd "C-q") 'delete-window)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-increase-width)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-decrease-width)

  (define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-insert-state-map (kbd "M-RET") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)

  (define-key evil-normal-state-map (kbd "-") 'split-window-vertically-and-switch)
  (define-key evil-motion-state-map (kbd "-") 'split-window-vertically-and-switch)
  (define-key evil-normal-state-map (kbd "|") 'split-window-horizontally-and-switch)
  (define-key evil-motion-state-map (kbd "|") 'split-window-horizontally-and-switch)

  (evil-leader/set-key "w" 'save-buffer)
  (evil-leader/set-key "i" 'evil-window-move-far-left)
  (evil-leader/set-key "a" 'align-regexp)
  (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)
  (evil-leader/set-key "`" (lambda () (interactive) (evil-delete-marks () t)))

  (evil-leader/set-key "m i" (lambda () (interactive) (shell-make "install")))
  (evil-leader/set-key "m r" (lambda () (interactive) (shell-make "run")))
  (evil-leader/set-key "m b" (lambda () (interactive) (shell-make "build")))
  (evil-leader/set-key "m c" (lambda () (interactive) (shell-make "clean")))
  (evil-leader/set-key "m s" (lambda () (interactive) (shell-make "setup")))
  (evil-leader/set-key "m t" (lambda () (interactive) (shell-make "test")))

  (evil-leader/set-key "u" 'universal-argument)

  ;; Buffer Management
  (define-key evil-visual-state-map (kbd "SPC") 'evil-search-forward)
  (define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)
  (define-key evil-visual-state-map (kbd "C-i") 'indent-region)
  (evil-leader/set-key "k b" 'kill-buffer)

  ;; Recreate unimpaired
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)

  (eval-after-load "debugger"
    '(progn (evil-set-initial-state 'debugger-mode 'emacs))))


;; Utilities
;; ================================================================================

(use-package projectile
  :straight t
  :preface
  (defun update-exec-path-projectile ()
    ;; Any relative exec paths should apply to the projectile root:
    ;; This for example allows lsp-mode to find the correct pylsp package
    ;; when a file is opened deep in the project hierarchy. Otherwise,
    ;; pylsp would not be found or the incorrect (global) one would be used.
    (dolist (path (seq-map (lambda (path) (concat (projectile-project-root) (substring path 2)))
                    (seq-filter (lambda (path) (string-prefix-p "./" path)) exec-path)))
      (push path exec-path)))
  :diminish projectile-mode
  :hook (projectile-mode . update-exec-path-projectile)
  :hook (projectile-after-switch-project . update-exec-path-projectile)
  :commands ( projectile-global-mode projectile-invalidate-cache
              projectile-find-other-file
              projectile-find-implementation-or-test
              projectile-find-implementation-or-test-other-window
              projectile-toggle-between-implementation-and-test
              projectile-invalidate-cache projectile-project-root )
  :bind ( ("C-p" . projectile-find-file)
          ("C-b" . projectile-switch-to-buffer) )
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (evil-leader/set-key "go" (lambda () (interactive) (projectile-find-other-file t)))
  (evil-leader/set-key "gt" 'projectile-toggle-between-implementation-and-test)
  (with-eval-after-load 'ivy (define-key ivy-minibuffer-map (kbd "C-l") 'projectile-invalidate-cache))

  :config
  (projectile-global-mode +1)
  (add-to-list 'projectile-project-root-files ".projectile")
  (add-to-list 'projectile-project-root-files ".git")
  (add-to-list 'projectile-project-root-files "glide.yaml")
  (add-to-list 'projectile-project-root-files "Gopkg.toml")
  (add-to-list 'projectile-project-root-files "go.mod")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories ".cask")
  (add-to-list 'projectile-globally-ignored-directories ".stack")
  (add-to-list 'projectile-globally-ignored-directories ".tmp")
  (add-to-list 'projectile-globally-ignored-directories "tmp")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "bower_components")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
                                        ; Register and support more project types

  ; Reset projectile project types (later has priority)

  (projectile-register-project-type 'xcode
    '("*.xcodeproj"))

  (projectile-register-project-type 'xcode-make
    '("Cartfile" "Makefile")
    :test "make test"
    :test-suffix "Spec")

  (projectile-register-project-type 'haskell
    '("stack.yaml")
    :test-suffix "Spec")

  (projectile-register-project-type 'js-make
    '("package.json" "Makefile")
    :test "make test")

  (projectile-register-project-type 'ruby-make
    '("Gemfile" "Makefile")
    :test-suffix "_spec"
    :test "make test")

  (projectile-register-project-type 'go '("glide.yaml")
    :test-suffix "_test")

  (projectile-register-project-type 'go '("Gopkg.toml")
    :test-suffix "_test")

  (projectile-register-project-type 'go '("go.mod")
    :test-suffix "_test")

  (projectile-register-project-type 'go-make '("glide.yaml" "Makefile")
    :test "make test"
    :test-suffix "_test")

  (projectile-register-project-type 'deno '("deno.json")
    :test "deno test"
    :test-suffix "_test"))

(use-package origami
  :straight t
  :hook (prog-mode . (lambda () (origami-mode 1))))

(use-package xref
  :config
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") 'xref-show-location-at-point)
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "[c") 'xref-prev-line)
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "]c") 'xref-next-line))

(use-package ivy
  :straight t
  :diminish ivy-mode
  :init
  (setq ivy-display-style 'fancy)
  (define-key evil-normal-state-map (kbd "DEL") 'ivy-resume)
  (evil-set-initial-state 'ivy-occur-mode 'normal)
  (evil-define-key 'normal ivy-occur-mode-map (kbd "RET") 'ivy-occur-press)
  (evil-define-key 'motion ivy-occur-mode-map (kbd "RET") 'ivy-occur-press)

  (use-package swiper
    :straight t
    :commands (swiper swiper-all)
    :init (evil-leader/set-key "s" 'swiper))

  (use-package counsel
    :straight t
    :diminish counsel-mode
    :preface
    (defun counsel-projectile-ag ()
      (interactive)
      (counsel-ag "" (projectile-project-root) "--nogroup"))
    :functions (counsel-projectile-ag)
    :bind (("C-s" . counsel-projectile-ag))
    :config
    (advice-add 'counsel--split-command-args :around
      (lambda (orig-fun &rest args)
        (message "counsel-ag argument parsing: %S" (apply orig-fun args))
        (apply orig-fun args))))

  (use-package wgrep
    :straight t
    :config
    (advice-add #'save-buffer :around
      (lambda (old-fun &rest args)
        (if (not (eq (current-local-map) wgrep-mode-map))
          (apply old-fun args)
          (wgrep-finish-edit)
          (wgrep-save-all-buffers)
          (wgrep-change-to-wgrep-mode))))

    (advice-add #'evil-quit :around
      (lambda (old-fun &rest args)
        (if (not (eq (current-local-map) wgrep-mode-map))
          (apply old-fun args)
          (wgrep-abort-changes)
          (evil-delete-buffer (current-buffer)))))

    (advice-add #'evil-save-and-close :around
      (lambda (old-fun &rest args)
        (if (not (eq (current-local-map) wgrep-mode-map))
          (apply old-fun args)
          (wgrep-finish-edit)
          (wgrep-save-all-buffers)
          (evil-delete-buffer (current-buffer))))))

  :config
  (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-occur)
  (define-key ivy-occur-grep-mode-map (kbd "C-w") 'ivy-wgrep-change-to-wgrep-mode)

  (define-key ivy-minibuffer-map (kbd "C-q") 'keyboard-escape-quit)
  (define-key ivy-minibuffer-map (kbd "C-p") 'keyboard-escape-quit)
  (define-key ivy-minibuffer-map (kbd "C-s") 'keyboard-escape-quit)
  (define-key ivy-minibuffer-map (kbd "C-b") 'keyboard-escape-quit)

  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-dispatching-done)
  (define-key ivy-minibuffer-map (kbd "M-RET") 'ivy-next-line-and-call)
  (define-key ivy-minibuffer-map (kbd "C-@") 'ivy-restrict-to-matches)

  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-i") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-previous-history-element))

(use-package display-line-numbers
  :straight t
  :init
  (setq display-line-numbers-type 'relative)
  (setq display-line-numbers-width-start t)
  (global-display-line-numbers-mode t))

(use-package popwin
  :straight t
  :init (setq popwin:special-display-config  '( ("^\\*shell:.*\\*$" :regexp t :position bottom :noselect t :height 10 :stick t)
                                                ("*Warnings*" :noselect t)
                                                ("*GHC Error*" :noselect t)
                                                ("*undo-tree*" :width 50 :position left)) )
  :config
  (evil-define-key 'normal popwin:keymap (kbd "q") 'popwin:close-popup-window)
  (evil-define-key 'emacs popwin:keymap (kbd "q") 'popwin:close-popup-window))

(use-package smartparens
  :diminish
  :straight t
  :init (use-package smartparens-config)
  :config
  (sp-local-pair 'makefile-mode "$(" ")")
  (sp-local-pair 'makefile-bsdmake-mode "$(" ")")
  (sp-local-pair 'swift-mode "\\(" nil :actions nil)
  (sp-local-pair 'swift-mode "\\(" ")")
  (sp-local-pair 'swift-mode "<" ">")
  (add-to-list 'sp-sexp-suffix (list 'js2-mode 'regexp ""))
  (add-to-list 'sp-sexp-suffix (list 'js2-minor-mode 'regexp ""))
  (evil-define-key 'insert smartparens-mode-map
    (kbd "C-l") 'sp-forward-sexp
    (kbd "C-h") 'sp-backward-sexp
    (kbd "C-k") 'sp-splice-sexp-killing-backward
    (kbd "C-j") 'sp-rewrap-sexp
    (kbd "C-c") 'sp-convolute-sexp
    (kbd "C-w") 'sp-kill-sexp
    (kbd "C-b") 'sp-backward-kill-sexp
    (kbd "M-i") 'sp-slurp-hybrid-sexp
    (kbd "M-o") 'sp-forward-barf-sexp))

(use-package string-inflection
  :straight t
  :commands (string-inflection-all-cycle string-inflection-underscore string-inflection-camelcase)
  :init
  (evil-leader/set-key "qs" 'string-inflection-underscore)
  (evil-leader/set-key "qc" 'string-inflection-camelcase)
  (evil-leader/set-key "qq" 'string-inflection-all-cycle))

(use-package dumb-jump
  :straight t
  :commands (dumb-jump-go)
  :init
  (evil-leader/set-key "jd" 'dumb-jump-go))

(use-package writeroom-mode
  :straight t
  :commands writeroom-mode
  :init
  (setq writeroom-restore-window-config t)
  (setq writeroom-width 120)
  (evil-leader/set-key "," 'writeroom-mode)

  :config
  (dolist (w '(evil-window-next evil-window-prev switch-to-buffer))
    (advice-add w :around
      (lambda (oldfun &rest args)
        "Ensure `writeroom-mode' is turned off and on around this function"
        (let ((has-writeroom (bound-and-true-p writeroom-mode)))
          (and has-writeroom (writeroom-mode -1))
          (apply oldfun args)
          (and has-writeroom (writeroom-mode 1))))))

  (dolist (w '(
                split-window-vertically
                split-window-horizontally
                persp-switch
                quit-window))
    (advice-add w :around
      (lambda (oldfun &rest args)
        "Ensure `writeroom-mode' is off, otherwise no-op"
        (let ((has-writeroom (bound-and-true-p writeroom-mode)))
          (and (not has-writeroom) (apply oldfun args))))))

  (add-to-list 'writeroom-global-effects
    (lambda (arg)
      (interactive)
      (git-gutter-mode (* -1 arg))
      (display-line-numbers-mode (* -1 arg))
      (flycheck-mode (* -1 arg)))))

(use-package editorconfig
  :straight t
  :diminish editorconfig-mode
  :config
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-attr-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-attr-value-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-code-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-css-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-markup-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-sql-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(js-mode js-indent-level js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(js2-mode js-indent-level js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(js2-minor-mode js-indent-level js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(swift-mode swift-mode:basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(haskell-mode haskell-indent-spaces haskell-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(evil-mode evil-shift-width)))

(use-package shell
  :functions shell-make
  :preface
  (defun shell-make (command)
    "Call `make *command*` in the projectile root directory under a buffer named '*shell:make*'"
    (interactive)
    (projectile-with-default-dir (projectile-project-root)
      (async-shell-command (format "make %s" command) (format "*shell:make %s*" command))))

  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :hook (shell-mode . read-only-mode)
  :hook (shell-mode . buffer-disable-undo)
  :hook (shell-mode . (lambda () (display-line-numbers-mode -1)))

  :config
  (evil-set-initial-state 'shell-mode 'emacs)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  ;; The assumption here is that I only use shell mode to run processes,
  ;; I don't use it for actuall shell access (have tmux for that.)
  (evil-define-key 'emacs shell-mode-map (kbd "q") 'delete-window)
  (define-key shell-mode-map (kbd "C-c C-c") (lambda () (interactive) (delete-process (buffer-name)))))

(use-package profiler
  :config
  (evil-set-initial-state 'profiler-report-mode 'motion))

(use-package perspective
  :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (define-key evil-normal-state-map (kbd "C-@") 'persp-switch)
  (evil-leader/set-key "p r" 'persp-rename)
  (evil-leader/set-key "p k" 'persp-kill)
  (evil-leader/set-key "p n" 'persp-next)
  (evil-leader/set-key "p p" 'persp-prev)
  (evil-leader/set-key (kbd ")") 'persp-next)
  (evil-leader/set-key (kbd "(") 'persp-prev)
  :config
  (persp-mode))

(use-package help-mode
  :config
  (evil-set-initial-state 'help-mode 'motion))

(use-package dired
  :config
  (evil-set-initial-state 'dired-mode 'motion)
  (evil-leader/set-key "kr" 'dired)
  (setq dired-use-ls-dired nil)
  (define-key dired-mode-map (kbd "(") nil)
  (define-key dired-mode-map (kbd "C-w") 'dired-toggle-read-only)
  (evil-define-key 'motion dired-mode-map
    (kbd "RET") 'dired-find-file
    (kbd ".") nil
    (kbd "m") nil
    (kbd "..") 'dired-up-directory
    (kbd "TAB") 'dired-hide-details-mode
    (kbd "r") 'revert-buffer
    (kbd "md") 'dired-do-delete
    (kbd "mc") 'dired-do-copy
    (kbd "mm") 'dired-do-rename
    (kbd "mad") 'dired-create-directory
    (kbd "maf") 'find-file))

(use-package git-gutter
  :straight t
  :diminish git-gutter-mode
  :config
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (evil-leader/set-key "g a" 'git-gutter:stage-hunk)
  (evil-leader/set-key "g r" 'git-gutter:revert-hunk))

(use-package magit
  :straight t
  :hook (git-commit-mode . (lambda () (auto-fill-mode 0)))
  :init
  (setq vc-handled-backends ())
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq git-commit-major-mode 'text-mode)
  (setq magit-refresh-status-buffer nil)
  (evil-leader/set-key
    "g l" 'magit-log
    "g c" 'magit-commit
    "g C" 'magit-commit-amend
    "g s" 'magit-status
    "g d" 'magit-diff-buffer-file
    "g b" (lambda () (interactive)
            (if (bound-and-true-p magit-blame-mode)
              (magit-blame-quit)
              (call-interactively 'magit-blame-addition)))
    "g w" 'magit-stage-file)

  :config
  (evil-set-initial-state 'text-mode 'insert)
  (evil-set-initial-state 'magit-cherry-mode 'motion)
  (evil-set-initial-state 'magit-diff-mode 'motion)
  (evil-set-initial-state 'magit-log-mode 'motion)
  (evil-set-initial-state 'magit-log-select-mode 'motion)
  (evil-set-initial-state 'magit-mode 'motion)
  (evil-set-initial-state 'magit-process-mode 'motion)
  (evil-set-initial-state 'magit-reflog-mode 'motion)
  (evil-set-initial-state 'magit-refs-mode 'motion)
  (evil-set-initial-state 'magit-revision-mode 'motion)
  (evil-set-initial-state 'magit-stash-mode 'motion)
  (evil-set-initial-state 'magit-stashes-mode 'motion)
  (evil-set-initial-state 'magit-status-mode 'motion)
  ;; (evil-set-initial-state 'magit-wazzup-mode 'motion)
  ;; (evil-set-initial-state 'magit-branch-manager-mode 'motion)
  ;; (evil-set-initial-state 'magit-key-mode 'motion)
  ;; (evil-set-initial-state 'magit-popup-mode 'motion)
  ;; (evil-set-initial-state 'magit-popup-sequence-mode 'motion)

  (define-key magit-file-section-map (kbd "C-j") nil)
  (define-key magit-hunk-section-map (kbd "C-j") nil)

  (evil-define-key 'emacs magit-file-section-map
    (kbd "RET") 'magit-diff-visit-file-other-window)

  (evil-define-key 'emacs magit-hunk-section-map
    (kbd "RET") 'magit-diff-visit-file-other-window)

  (evil-define-key 'normal magit-blame-mode-map
    (kbd "RET") 'magit-show-commit
    (kbd "y y") 'magit-blame-copy-hash)

  (evil-define-key 'emacs magit-status-mode-map
    (kbd "p") 'magit-push-popup
    (kbd "l") 'magit-log-popup)

  ; Remove the git commit diff hook so that commit buffers load a lot quicker.
  ; See: https://magit.vc/manual/magit/Performance.html
  (remove-hook 'server-switch-hook 'magit-commit-diff)

  (use-package git-rebase
    :config
    (evil-set-initial-state 'git-rebase-mode 'motion)
    (evil-define-key 'motion git-rebase-mode-map
      (kbd "s") 'git-rebase-squash
      (kbd "p") 'git-rebase-pick
      (kbd "r") 'git-rebase-reword
      (kbd "e") 'git-rebase-edit
      (kbd "K") 'git-rebase-kill-line
      (kbd "N") 'git-rebase-move-line-down
      (kbd "P") 'git-rebase-move-line-up
      (kbd "RET") 'git-rebase-show-commit)))

(use-package gist
  :straight t
  :config
  (evil-set-initial-state 'gist-list-mode 'motion))

(use-package git-link
  :straight t)

(use-package flycheck
  :straight t
  :diminish flycheck-mode
  :preface

  (defvar counsel-flycheck-history nil
    "History for `counsel-flycheck'")

  (defun counsel-flycheck ()
    (interactive)
    (if (not (bound-and-true-p flycheck-mode))
      (message "Flycheck mode is not available or enabled")
      (ivy-read "Error: "
        (let ((source-buffer (current-buffer)))
          (with-current-buffer (or (get-buffer flycheck-error-list-buffer)
                                 (progn
                                   (with-current-buffer
                                     (get-buffer-create flycheck-error-list-buffer)
                                     (flycheck-error-list-mode)
                                     (current-buffer))))
            (flycheck-error-list-set-source source-buffer)
            (flycheck-error-list-reset-filter)
            (revert-buffer t t t)
            (split-string (buffer-string) "\n" t " *")))
        :action (lambda (s &rest _)
                  (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                (pos (flycheck-error-pos error)) )
                    (goto-char (flycheck-error-pos error))))
        :history 'counsel-flycheck-history)))

  :init
  (setq flycheck-idle-change-delay 0.5)
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-highlighting-mode nil)
  (evil-leader/set-key "el" 'counsel-flycheck)

  :config
  (delete 'go-vet flycheck-checkers)
  (delete 'go-build flycheck-checkers)
  (delete 'go-test flycheck-checkers)
  (delete 'go-gofmt flycheck-checkers)
  (delete 'go-golint flycheck-checkers)
  (delete 'go-errcheck flycheck-checkers)
  (delete 'go-unconvert flycheck-checkers)
  (delete 'go-megacheck flycheck-checkers)
  (define-key evil-normal-state-map (kbd "] e") 'next-error)
  (define-key evil-normal-state-map (kbd "[ e") 'previous-error))

(use-package flyspell
  :straight t
  :hook (prog-mode . flyspell-prog-mode)
  :config
  ;; Following setup that uses run-together mode for aspell is taken from:
  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  (defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "-d en_US")))
    args))
  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
  ;; Please note when you use hunspell, ispell-extra-args will NOT be used.
  ;; Hack ispell-local-dictionary-alist instead.
  (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
  ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
  (defadvice ispell-word (around my-ispell-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      (setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)
      ))

  (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    )))

(use-package yasnippet
  :straight t
  :commands ( yas-global-mode yas-minor-mode
              yas-expand yas-expand-snippet
              yas-activate-extra-mode )
  :preface (defun yas-skip-and-clear-or-backward-delete-char (&optional field)
             "Clears unmodified field if at field start, skips to next tab.
Otherwise deletes a character normally by calling `backward-delete-char'."
             (interactive)
             (let ((field (or field
                            (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
               (cond
                 ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
                   (yas--skip-and-clear field)
                   (yas-next-field 1))
                 (t (call-interactively 'backward-delete-char)))))
  :diminish yas-minor-mode
  :init
  (evil-define-key 'insert yas-minor-mode-map (kbd "DEL") 'yas-skip-and-clear-or-backward-delete-char)
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-e") 'yas-expand)
  (setq yas-snippet-dirs
    '("~/.emacs.d/.snippets/yasnippet-snippets"
       "~/.emacs.d/.snippets/personal")))

(use-package counsel-dash
  :straight t
  :commands (counsel-dash
              counsel-dash-set-local-docsets
              counsel-dash-activate-local-docset
              counsel-dash-activate-docset
              counsel-dash-deactivate-docset
              counsel-dash-install-docset)
  :load-path "vendor/counsel-dash"
  :init
  (if (file-accessible-directory-p "/Volumes/Storage/.docset")
    (setq counsel-dash-docsets-path "/Volumes/Storage/.docset")
    (setq counsel-dash-docsets-path "~/.docset"))

  (setq counsel-dash-browser-func 'eww)
  (setq counsel-dash-common-docsets '("Emacs Lisp" "Swift" "iOS" "Javascript"))
  (define-key evil-normal-state-map (kbd "C-f") 'counsel-dash)
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
  (add-hook 'ruby-mode-hook (lambda () (setq-local counsel-dash-docsets '("Ruby"))))
  (add-hook 'dockerfile-mode-hook (lambda () (setq-local counsel-dash-docsets '("Docker"))))
  (add-hook 'js2-minor-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "NodeJS"))))
  (add-hook 'web-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "HTML""CSS"))))
  (add-hook 'scss-mode-hook (lambda () (setq-local counsel-dash-docsets '("CSS"))))
  (add-hook 'swift-mode-hook (lambda () (setq-local counsel-dash-docsets '("iOS" "Swift")))))

(use-package company
  :straight t
  :diminish
  :commands global-company-mode
  :defines company-dabbrev-downcase company-idle-delay company-tooltip-align-annotations
  :preface
  ; Fix fci-mode with company-mode
  (defvar-local company-fci-mode-on-p nil)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))
  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))
  :hook (company-completion-started . company-turn-off-fci)
  :hook (company-completion-finished . company-maybe-turn-on-fci)
  :hook (company-completion-cancelled . company-maybe-turn-on-fci)
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 1)
  :config
  (use-package company-emoji
    :straight (company-emoji :type git :host github :repo "dunn/company-emoji" :branch "trunk"))
  ; Swap some keybindings
  (define-key evil-insert-state-map (kbd "C-@") 'company-complete)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map (kbd "C-i") 'company-select-next)
  (define-key company-active-map (kbd "C-o") 'company-select-previous)
  ; Okay lets setup company backends the way we want it, in a single place.
  (setq company-backends
    '( company-css
       company-elisp
       company-clang
       company-capf
       company-files
       company-dabbrev-code
       company-keywords
       company-emoji
       company-yasnippet)))

(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook (compilation-filter . colorize-compilation-buffer))

(use-package prettier
  :straight t
  :diminish
  :mode ("\\.ts\\'" . prettier-mode)
  :mode ("\\.js\\'" . prettier-mode)
  :mode ("\\.mjs\\'" . prettier-mode)
  :mode ("\\.cjs\\'" . prettier-mode)
  :mode ("\\.jsx\\'" . prettier-mode)
  :mode ("\\.tsx\\'" . prettier-mode)
  :mode ("\\.css\\'" . prettier-mode)
  :mode ("\\.less\\'" . prettier-mode)
  :mode ("\\.scss\\'" . prettier-mode)
  :mode ("\\.md\\'" . prettier-mode)
  :mode ("\\.markdown\\'" . prettier-mode)
  :commands prettier-mode)

(use-package polymode
  :straight t
  :config
  ;; LSP integration is too smart, disabling it seems to
  ;; just use the host mode's LSP process for the entire buffer.
  (setq polymode-lsp-integration nil))

(use-package poly-markdown :straight t)

(use-package lsp-mode
  :straight t
  :commands (lsp-deferred lsp-goto-type-definition lsp-goto-implementation)
  :custom
  (lsp-inlay-hint-enable t)
  (lsp-response-timeout 10)
  (lsp-idle-delay 3)
  (lsp-print-performance nil)
  (lsp-auto-guess-root t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-lens-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-signature-render-documentation t)
  (lsp-signature-auto-activate t)
  (lsp-eldoc-enable-hover t)
  (lsp-log-io nil)
  ;; (lsp-log-io t "enable for debugging")
  (lsp-log-max nil "disable logging")

  :config
  (evil-leader/set-key "jt" 'lsp-goto-type-definition)
  (evil-leader/set-key "ji" 'lsp-goto-implementation)
  (evil-leader/set-key "jd" 'lsp-find-definition)
  (evil-leader/set-key "jr" 'lsp-find-references)
  (evil-leader/set-key "rr" 'lsp-rename)
  (evil-leader/set-key "?" 'lsp-describe-thing-at-point)

  ;; Workaround or doc links being removed:
  ;; https://github.com/emacs-lsp/lsp-ui/issues/452
  (defun markdown-raw-links (&rest ignore)
    "Convert link markup [ANCHOR](URL) to raw URL
     so lsp-ui-doc--make-clickable-link can find it"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-link-inline nil t)
        (replace-match (replace-regexp-in-string "\n" "" (match-string 6))))))
  (advice-add 'lsp--render-markdown :before #'markdown-raw-links)

  ;; Fix for typescript lsp:
  ;; https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
  ;; same definition as mentioned earlier
  (advice-add 'json-parse-string :around
    (lambda (orig string &rest rest)
      (apply orig (s-replace "\\u0000" "" string)
        rest)))
  ;; minor changes: saves excursion and uses search-forward instead of re-search-forward
  (advice-add 'json-parse-buffer :around
    (lambda (oldfn &rest args)
	    (save-excursion
        (while (search-forward "\\u0000" nil t)
          (replace-match "" nil t)))
		  (apply oldfn args)))

  ;; support translating uris that begin with deno:/ into temporary
  ;; files based on the response of deno/virtualTextDocument:
  ;;
  ;; https://deno.land/x/deno@v1.10.2/cli/lsp/README.md
  (advice-add 'lsp--locations-to-xref-items :around
    (lambda (oldfn &rest args)
      (mapcar
        (lambda (entry)
          (if (string-prefix-p "deno:/" (gethash "targetUri" entry))
            (let* ((uri (gethash "targetUri" entry))
                    (cleanedUri (string-trim-left uri "deno://?"))
                    (contents (lsp-request "deno/virtualTextDocument"
                                (list :textDocument (list :uri uri)))))
              (puthash
                "targetUri"
                (make-temp-file (file-name-base cleanedUri) nil nil contents)
                entry))))
        (car args))
      (apply oldfn args))))


;; LANGUAGE PACKS
;; ================================================================================

(use-package pine-script-mode
  :straight t
  :mode ("\\.pine\\'" . pine-script-mode)
  :init
  (add-hook 'pine-script-mode-hook
    (lambda ()
      (make-local-variable 'indent-tabs-mode)
      (make-local-variable 'tab-width)
      (setq indent-tabs-mode t)
      (setq tab-width 2))))

(use-package text-mode
  :preface (provide 'text-mode)
  :no-require t
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (setq-default fill-column 80))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode))

(use-package julia-mode
  :straight t)

(use-package js2-mode
  :straight t
  :diminish js2-minor-mode
  :commands (js2-mode js2-minor-mode)
  :mode ("\\.js\\'" . js-mode)
  :mode ("\\.ts\\'" . js-mode)
  :interpreter ("node" . js-mode)
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js-mode-hook #'lsp-deferred)
  (setq js2-highlight-level 3)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js-indent-level 2)

  :config
  (evil-define-key 'insert js2-minor-mode-map (kbd "RET") 'js2-line-break))

(use-package typescript-mode
  :straight t
  :init
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  :mode "\\.ts\\'")

(use-package coffee-mode
  :straight t
  :mode "\\.coffee\\'")

(use-package web-mode
  :straight t
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.liquid\\'"
  :mode "\\.mustache\\'"
  :mode "\\.hbs\\'"
  :mode "\\.tag\\'"
  :mode "\\.vue\\'"
  :mode "\\.djhtml\\'"
  :mode "\\.html?\\'"
  :mode "\\.html.twig\\'"
  :mode "\\.html.jsx\\'"
  :mode "\/\*\*.*@jsx"
  :mode "\\.jsx\\'"
  :mode "\\.tsx\\'"
  :preface (flycheck-define-checker jsxhint
             "A JSX syntax and style checker based on JSXHint."
             :command ("jsxhint" source)
             :error-patterns
             ((error line-start
                (1+ nonl)
                ": line " line
                ", col " column
                ", " (message) line-end))
             :predicate (lambda ()
                          (and (executable-find "jsxhint")
                            (buffer-file-name)
                            (string-match ".*\.jsx?$" (buffer-file-name))))
             :modes (web-mode))
  :init
  (add-hook 'web-mode-hook #'lsp-deferred)
  (use-package emmet-mode
    :straight t
    :commands emmet-mode
                                        ; emmet-mode still looks for the old expand-snippet function name
    :preface (defalias 'yas/expand-snippet 'yas-expand-snippet)
    :init
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)

    :config
    (add-hook 'emmet-mode-hook
      (lambda ()
        (if (string-match ".*\.[jt]sx$" (buffer-file-name))
          (setq emmet-expand-jsx-className? t)
          (setq emmet-expand-jsx-className? nil))))
    (advice-add #'yas--fallback :around
      (lambda (oldfun &rest args)
        (if (and (bound-and-true-p emmet-mode) (emmet-expr-on-line))
          (emmet-expand-yas)
          (apply oldfun args)))))

  :config
  (add-hook 'web-mode-hook (lambda () (yas-activate-extra-mode 'js-mode)))
  (add-to-list 'flycheck-checkers 'jsxhint)
  (define-key prog-mode-map (kbd "C-x /") 'web-mode-element-close)
  (define-key prog-mode-map (kbd "C-/")   'web-mode-element-close))

(use-package fish-mode
  :straight t
  :mode "\\.fish\\'")

(use-package less-css-mode
  :straight t
  :mode "\\.less\\'")

(use-package stylus-mode
  :straight t
  :mode "\\.stylus\\'")

(use-package scss-mode
  :straight t
  :mode "\\.sass\\'"
  :mode "\\.scss\\'"
  :init (setq scss-compile-at-save nil))

(use-package php-mode
  :straight t
  :mode "\\.php\\'")

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . poly-markdown-mode)
  :mode ("\\.markdown\\'" . poly-markdown-mode)
  :mode "^README\\'"
  :commands markdown-mode
  :config
  (add-hook 'markdown-mode-hook
    (lambda ()
      (turn-on-orgtbl)
      (add-hook 'before-save-hook 'convert-orgtbl-to-gfm nil t))))

(use-package lua-mode
  :straight t
  :mode "\\.lua\\'")

(use-package terraform-mode
  :straight t
  :mode "\\.tf\\'")

(use-package swift-mode
  :straight t
  :mode "\\.swift\\'"
  :hook (swift-mode . lsp-deferred)

  :init
  (add-to-list 'flycheck-checkers 'swift)

  :config
  (use-package lsp-sourcekit
    :straight t
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))))

(use-package dockerfile-mode
  :straight t
  :mode "^Dockerfile\\'"
  :mode "^Dockerfile.*\\'")

(use-package puppet-mode
  :straight t
  :mode "\\.pp\\'")

(use-package yaml-mode
  :straight t
  :mode "\\.yaml\\'"
  :mode "\\.yml\\'")

(use-package ruby-mode
  :straight t
  :mode "\\.rb\\'"
  :init
  (add-hook 'ruby-mode-hook #'lsp-deferred))

(use-package kotlin-mode
  :straight t
  :mode "\\.kt\\'")

(use-package rustic
  :straight t
  :mode ("\\.rs\\'" . poly-rust+web-mode)
  :custom
  (rustic-lsp-setup-p  nil)
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-rustfmt-extra-args ["+nightly"])
  (lsp-rust-analyzer-cargo-watch-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "always")
  (lsp-rust-analyzer-diagnostics-enable t)
  (lsp-rust-analyzer-max-inlay-hint-length 12)
  ;; (lsp-rust-analyzer-display-reborrow-hints "always")
  ;; (lsp-rust-analyzer-server-command '("rust-analyzer" "+nightly"))

  :hook
  (before-save . lsp-format-buffer)

  :init
  ;; formatting will be handled by lsp, NOT rust-mode
  (setq rust-format-on-save nil)
  (add-hook 'poly-rust+web-mode-hook #'lsp-deferred)
  :config
  (evil-leader/set-key-for-mode 'rust-mode "tt" 'rust-test)
  (evil-leader/set-key-for-mode 'rust-mode "m e" (lambda () (interactive) (lsp-rust-analyzer-expand-macro)))

  (define-hostmode poly-rust-hostmode
    :mode 'rust-mode)
  (define-innermode poly-web-rust-innermode
    :mode 'web-mode
    :head-matcher "view! {"
    :tail-matcher #'pm-forward-sexp-tail-matcher
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-rust+web-mode
    :hostmode 'poly-rust-hostmode
    :innermodes '(poly-web-rust-innermode)))

(use-package haskell-mode
  :straight t
  :commands haskell-mode
  :mode "\\.hs\\'"
  :mode "\\.lhs\\'"
  :mode "config/models\\'"
  :mode "config/routes\\'"
  :hook (haskell-mode . interactive-haskell-mode)
  :preface
  (defvar counsel-hoogle-history-input nil
    "Input history used by `ivy-read'.")
  (defun counsel-hoogle-function (s &rest _)
    (or
      (ivy-more-chars)
      (progn
        (counsel--async-command
          (concat
            haskell-hoogle-command
            " "
            (shell-quote-argument s)))
        nil)))
  (defun counsel-hoogle (&optional initial)
    "Query hoogle using counsel.
INITIAL will be used as the initial input, if given."
    (interactive)
    (ivy-read "Hoogle: "
      #'counsel-hoogle-function
      :dynamic-collection t
      :history 'counsel-hoogle-history-input
      :initial-input initial
      :action (lambda (s)
                (browse-url
                  (format haskell-hoogle-url
                    (url-hexify-string ivy-text))))))
  :init
  (setq haskell-hoogle-command "stack hoogle --no-setup -- -q --count=30")
  (setq haskell-hoogle-url "https://www.stackage.org/lts/hoogle?q=%s")

  :config
  (evil-define-key 'normal haskell-mode-map (kbd "?") 'counsel-hoogle)

  (use-package ormolu
    :straight t
    :hook (haskell-mode . ormolu-format-on-save-mode))

  (use-package lsp-haskell
    :straight t
    :hook (haskell-mode . lsp)
    :hook (haskell-literate-mode . lsp)
    :config
    (setq lsp-haskell-hlint-on t)
    (setq lsp-haskell-diagnostics-on-change nil)
    (setq lsp-haskell-format-on-import-on nil)
    (setq lsp-haskell-formatting-provider "ormolu"))

  (use-package haskell-interactive-mode
    :init
    :custom
    (haskell-process-type 'stack-ghci)
    (haskell-process-suggest-remove-import-lines nil)
    (haskell-process-auto-import-loaded-modules nil)
    (haskell-interactive-popup-errors nil)
    :config
    (use-package haskell-process)
    (evil-set-initial-state 'haskell-interactive-mode 'emacs)
    (evil-define-key 'normal haskell-mode-map (kbd "`") 'haskell-interactive-bring)
    (evil-leader/set-key-for-mode 'haskell-mode "hl" 'haskell-process-load-or-reload)
    (define-key haskell-interactive-mode-map (kbd "C-j") nil)
    (evil-define-key 'emacs haskell-interactive-mode-map
      (kbd "<up>") 'haskell-interactive-mode-history-previous
      (kbd "<down>") 'haskell-interactive-mode-history-next)))

(use-package elpy
  :hook (elpy-mode . lsp-deferred)
  :hook (elpy-mode . flycheck-mode)
  :custom
  (python-indent-offset 4)
  (python-guess-indent-offset nil)
  (elpy-shell-darwin-use-pty t)
  :straight t
  :init
  (elpy-enable)
  (evil-leader/set-key-for-mode 'python-mode "pp" 'elpy-shell-switch-to-shell)
  (evil-leader/set-key-for-mode 'python-mode "pr" 'elpy-shell--send-region-or-buffer-internal)
  (evil-leader/set-key-for-mode 'python-mode "ps" 'elpy-shell-send-statement-and-step)
  (setq elpy-modules (delq 'elpy-module-company elpy-modules))
  (setq elpy-modules (delq 'elpy-module-eldoc elpy-modules))
  (setq elpy-modules (delq 'elpy-module-django elpy-modules))
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  :config
  (evil-set-initial-state 'inferior-python-mode 'emacs))

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred)
  :hook (before-save . gofmt-before-save)
  :custom
  (lsp-go-hover-kind "NoDocumentation")
  ;; (lsp-go-gopls-server-args '("--debug=localhost:6060"))
  (lsp-go-gopls-server-args '("-remote=auto"))
  (lsp-go-codelenses nil)
  (lsp-go-symbol-matcher "CaseInsensitive")
  (lsp-go-directory-filters [
    "-node_modules"
    "-bin"
    "-docs"
    "-node"
    "-proto"
    "-resources"
    "-tmp"
  ])
  (gofmt-command "goimports")
  :init
  (use-package gotest
    :straight t
    :config
    (evil-leader/set-key-for-mode 'go-mode "tc" 'go-test-current-test)
    (evil-leader/set-key-for-mode 'go-mode "tf" 'go-test-current-file)))

(use-package c-mode
  :hook ((c-mode c++-mode) . #'lsp-deferred)
  :no-require t
  :init
  (use-package ccls
    :straight t
    :config
    (and (executable-find "ccls")
      (setq ccls-executable (executable-find "ccls")))))

(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :hook (css-mode . rainbow-mode)
  :hook (emacs-lisp-mode . rainbow-mode)
  :hook (web-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package ledger-mode
  :straight t
  :mode "\\.ledger\\'"
  :config
  (use-package flycheck-ledger :straight t)

  (setq ledger-reconcile-default-commodity "NZD")
  (setq ledger-reconcile-sort-key "(date)")
  (setq ledger-clear-whole-transactions t)

  (evil-define-key 'normal ledger-mode-map
    (kbd "Y") 'ledger-copy-transaction-at-point
    (kbd "!") 'ledger-post-align-postings)

  (evil-define-key 'emacs ledger-reconcile-mode-map
    (kbd "c") 'ledger-reconcile-toggle
    (kbd "SPC") 'ledger-reconcile-toggle)

  (evil-leader/set-key-for-mode 'ledger-reconcile-mode
    "w" 'ledger-reconcile-save)

  (evil-set-initial-state 'ledger-reconcile-mode 'motion)

  (evil-leader/set-key-for-mode 'ledger-mode
    "n" 'ledger-add-transaction
    "r" 'ledger-reconcile
    "c" 'ledger-toggle-current-transaction
    "d" 'ledger-delete-current-transaction
    "?" 'ledger-display-balance-at-point))

(use-package conf-mode
  :mode "\\.conf\\'"
  :mode "\\.ini\\'"
  :mode "\\.toml\\'")

(use-package protobuf-mode
  :straight t
  :mode "\\.proto\\'")

;; PROGRAMS
;; ================================================================================

(use-package org
  :commands (turn-on-orgtbl org-mode)
  :preface
  (defun convert-orgtbl-to-gfm ()
    "Convert an org-mode table into GH flavored markdown"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
               (rx bol (* (syntax whitespace)) "|"
                 (+ (+ "-") (group "+"))
                 (+ "-") "|"
                 (* "\s") eol)
               nil t)
        (replace-match (replace-regexp-in-string (rx "+") "|" (match-string-no-properties 0))))))

  :config
  (evil-define-key 'normal org-mode-map
    "TAB"          'org-cycle
    ">>"           'org-do-demote
    "<<"           'org-do-promote
    "p"            'org-move-item-up
    "n"            'org-move-item-down
    "t"            'org-todo
    (kbd "RET")    'org-open-at-point)

  (evil-define-key 'insert org-mode-map
    (kbd "M-RET") 'org-insert-heading))

;; Bootloader
;; ================================================================================

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1) ;; Disable menu bar
(auto-save-mode nil) ;; Disable autosaving
(show-paren-mode t) ;; Show matching parens
(global-auto-revert-mode 1)
(column-number-mode 1)

(evil-mode 1)

(smartparens-global-mode t)
(show-smartparens-global-mode t)
(global-git-gutter-mode +1)
(editorconfig-mode 1)
(yas-global-mode 1)

(popwin-mode 1)
(ivy-mode 1)
(counsel-mode 1)

(global-flycheck-mode)
(global-company-mode)

(provide 'init)
;;; init.el ends here
