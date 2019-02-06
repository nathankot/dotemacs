;;; package --- Nathan's Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; (package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (guide-key yasnippet yaml-mode writeroom-mode wgrep web-mode use-package sx stylus-mode smartparens smart-mode-line scss-mode robe rainbow-mode rainbow-delimiters puppet-mode projectile popwin php-mode perspective magit lua-mode less-css-mode ledger-mode js2-mode idle-highlight-mode htmlize helm-dash haskell-mode git-gutter gist flycheck-ledger flycheck-cask fish-mode expand-region exec-path-from-shell evil-surround evil-snipe evil-search-highlight-persist evil-visual-mark-mode evil-matchit evil-commentary emmet-mode editorconfig dockerfile-mode counsel company-tern coffee-mode cask))))

(require 'cask (concat (getenv "HOMEBREW_ROOT") "/share/emacs/site-lisp/cask/cask.el"))
(cask-initialize)

(use-package dash)
(use-package exec-path-from-shell :init (exec-path-from-shell-initialize))

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
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq blink-matching-paren nil)
(setq initial-scratch-message ";; Hello.")
(fset 'yes-or-no-p 'y-or-n-p)
(setq large-file-warning-threshold 100000000)
(setq truncate-lines t)

;; Interactive helper functions.
;; ================================================================================

(defun print-point ()
  "Print the current point under the cursor.  Useful for debugging."
  (interactive)
  (message "%d" (point)))

(defun align-keycodes ()
  "Used to align keycodes in qmk_keyboard"
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   ",\\([ \t]+\\)" 1 1 t))

;; Theme.
;; ================================================================================

(use-package hyperfuse-theme
  :load-path "vendor/hyperfuse-theme"
  :init
  (require 'hyperfuse-theme)
  (load-theme 'hyperfuse t))

(use-package randomize-region
  :load-path "vendor/randomize-region"
  :init
  (require 'randomize-region))

(use-package smart-mode-line
  :init
  (sml/setup)
  (sml/apply-theme 'respectful)

  :config
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes nil)
  (setq sml/name-width 30)
  (setq sml/numbers-separator "")
  (setq sml/show-trailing-N nil)
  (setq sml/show-frame-identification nil)
  (setq sml/mule-info nil)
  (setq sml/show-client nil)
  (setq sml/show-remote nil)
  (setq sml/position-percentage-format nil))


;; EVIL
;; ================================================================================

(use-package evil
  :preface
  (defun split-window-vertically-and-switch ()
    (interactive)
    (split-window-vertically)
    (other-window 1))

  (defun split-window-horizontally-and-switch ()
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun apply-emacs-defaults-to-mode (mode)
    (let ((keymap-symbol (intern (concat (symbol-name mode) "-map"))))
      (evil-delay
        `(and (boundp ',keymap-symbol) (keymapp (symbol-value ',keymap-symbol)))
        `(let ((map (symbol-value ',keymap-symbol)))
           (dolist (k '("h" "j" "k" "l" "v" "m" "p" "n" "z"))
             (-when-let (def (lookup-key map k))
               (define-key map (upcase k) def)
               (define-key map k nil)))

           (evil-add-hjkl-bindings map 'emacs
             "v" 'evil-visual-char)

           (evil-define-key 'emacs map
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
             (kbd "0") 'evil-beginning-of-line))
        'after-load-functions t nil
        (format "evil-define-emacs-defaults-in-%s" (symbol-name keymap-symbol)))))

  :init
  (setq evil-emacs-state-modes nil)
  (setq evil-motion-state-modes nil)

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

  (use-package evil-leader
    :commands (global-evil-leader-mode)
    :config (evil-leader/set-leader ","))

  (use-package evil-search-highlight-persist
    :commands (global-evil-search-highlight-persist))

  (use-package evil-commentary
    :commands evil-commentary-mode)

  (use-package evil-snipe
    :commands (evil-snipe-mode evil-snipe-override-mode))

  (use-package evil-surround
    :commands (global-evil-surround-mode evil-surround-mode))

  (use-package evil-matchit
    :commands (global-evil-matchit-mode evil-matchit-mode))

  (use-package evil-visual-mark-mode
    :commands evil-visual-mark-mode
    :config
    ;; Marks aren't cleared after deletion until evil-visual-mark-mode re-renders:
    (advice-add #'evil-delete-marks :after
      (lambda (&rest args)
        (evil-visual-mark-render))))

  :config
  (global-evil-leader-mode)
  (evil-snipe-mode 1)
  (global-evil-search-highlight-persist)
  (evil-commentary-mode)
  (evil-snipe-override-mode 1)
  (global-evil-surround-mode 1)
  (global-evil-matchit-mode 1)
  (evil-visual-mark-mode 1)

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
  (define-key evil-motion-state-map (kbd "C-w") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-emacs-state-map (kbd "C-w") nil)

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
  (define-key evil-emacs-state-map (kbd "-") 'split-window-vertically-and-switch)
  (define-key evil-normal-state-map (kbd "|") 'split-window-horizontally-and-switch)
  (define-key evil-emacs-state-map (kbd "|") 'split-window-horizontally-and-switch)

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

  ;; Default keys for emacs state
  (add-function :after (symbol-function 'evil-set-initial-state)
    (lambda (mode state)
      (when (or (eq state 'emacs) (eq state 'motion))
        (apply-emacs-defaults-to-mode mode)))))


;; Utilities
;; ================================================================================

(use-package projectile
  :diminish projectile-mode
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
  (evil-leader/set-key "g o" (lambda () (interactive) (projectile-find-other-file t)))
  (evil-leader/set-key "g t" 'projectile-toggle-between-implementation-and-test)
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

  (projectile-register-project-type 'go '("glide.yaml")
    :test-suffix "_test")

  (projectile-register-project-type 'go '("Gopkg.toml")
    :test-suffix "_test")

  (projectile-register-project-type 'go '("go.mod")
    :test-suffix "_test")

  (projectile-register-project-type 'go-make '("glide.yaml" "Makefile")
    :test "make test"
    :test-suffix "_test"))

(use-package ivy
  :diminish ivy-mode
  :commands (ivy-mode ivy-read)
  :init
  (setq ivy-display-style 'fancy)
  (define-key evil-normal-state-map (kbd "DEL") 'ivy-resume)
  (evil-set-initial-state 'ivy-occur-mode 'normal)
  (evil-define-key 'normal ivy-occur-mode-map (kbd "RET") 'ivy-occur-press)
  (evil-define-key 'normal ivy-mode-map (kbd "RET") 'ivy-occur-press)

  (use-package swiper
    :commands (swiper swiper-all)
    :init (evil-leader/set-key "s" 'swiper))

  (use-package counsel
    :commands (counsel-mode counsel-ag)
    :diminish counsel-mode
    :preface
    (defun counsel-projectile-ag ()
      (interactive)
      (counsel-ag "" (projectile-project-root)))
    :functions (counsel-projectile-ag)
    :bind (("C-s" . counsel-projectile-ag)))

  (use-package wgrep
    :commands (wgrep-change-to-wgrep-mode ivy-wgrep-change-to-wgrep-mode)
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
  :init
  (setq display-line-numbers-type 'relative)
  (setq display-line-numbers-width-start t)
  (global-display-line-numbers-mode t))

(use-package popwin
  :commands popwin-mode
  :init (setq popwin:special-display-config  '( ("^\\*shell:.*\\*$" :regexp t :position bottom :noselect t :height 10 :stick t)
                                                ("*Warnings*" :noselect t)
                                                ("*GHC Error*" :noselect t)
                                                ("*undo-tree*" :width 50 :position left)) )
  :config
  (evil-define-key 'normal popwin:keymap (kbd "q") 'popwin:close-popup-window)
  (evil-define-key 'emacs popwin:keymap (kbd "q") 'popwin:close-popup-window))

(use-package smartparens
  :commands (smartparens-global-mode show-smartparens-global-mode)
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
  :commands (string-inflection-all-cycle string-inflection-underscore string-inflection-camelcase)
  :init
  (evil-leader/set-key "qs" 'string-inflection-underscore)
  (evil-leader/set-key "qc" 'string-inflection-camelcase)
  (evil-leader/set-key "qq" 'string-inflection-all-cycle))

(use-package avy
  :commands (avy-goto-char-2)
  :init
  (evil-leader/set-key "j" 'avy-goto-char-2))

(use-package dumb-jump
  :commands (dumb-jump-go)
  :init
  (evil-leader/set-key "dj" 'dumb-jump-go))

(use-package writeroom-mode
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

  (dolist (w '(split-window-vertically split-window-horizontally persp-switch quit-window))
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

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package editorconfig
  :commands editorconfig-mode
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

  :init
  (evil-set-initial-state 'shell-mode 'emacs)

  :config
  ;; The assumption here is that I only use shell mode to run processes,
  ;; I don't use it for actuall shell access (have tmux for that.)
  (add-hook 'shell-mode-hook 'read-only-mode)
  (add-hook 'shell-mode-hook 'buffer-disable-undo)
  (add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))
  (evil-define-key 'emacs shell-mode-map (kbd "q") 'delete-window)
  (define-key shell-mode-map (kbd "C-c C-c") (lambda () (interactive) (delete-process (buffer-name)))))

(use-package profiler
  :ensure nil
  :init
  (evil-set-initial-state 'profiler-report-mode 'emacs))

(use-package perspective
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

(use-package dired
  :init
  (setq dired-use-ls-dired nil)
  (evil-leader/set-key "kr" 'dired)
  (evil-set-initial-state 'dired-mode 'emacs)

  :config
  (define-key dired-mode-map (kbd "(") nil)
  (define-key dired-mode-map (kbd "C-w") 'dired-toggle-read-only)
  (evil-define-key 'emacs dired-mode-map
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
  :diminish git-gutter-mode
  :commands global-git-gutter-mode
  :config
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (evil-leader/set-key "g a" 'git-gutter:stage-hunk)
  (evil-leader/set-key "g r" 'git-gutter:revert-hunk))

(use-package magit
  :commands ( magit-log magit-commit magit-commit-amend
              magit-status magit-diff-unstaged magit-diff-staged
              magit-blame magit-blame-quit magit-stage-file )
  :init
  (global-git-commit-mode)
  (setq vc-handled-backends ())
  (setq magit-completing-read-function 'ivy-completing-read)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (setq git-commit-major-mode 'text-mode)
  (evil-set-initial-state 'text-mode 'insert)

  (evil-set-initial-state 'magit-branch-manager-mode 'emacs)
  (evil-set-initial-state 'magit-cherry-mode 'emacs)
  (evil-set-initial-state 'magit-diff-mode 'emacs)
  (evil-set-initial-state 'magit-key-mode 'emacs)
  (evil-set-initial-state 'magit-log-mode 'emacs)
  (evil-set-initial-state 'magit-log-select-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-popup-sequence-mode 'emacs)
  (evil-set-initial-state 'magit-process-mode 'emacs)
  (evil-set-initial-state 'magit-rebase-mode 'emacs)
  (evil-set-initial-state 'magit-reflog-mode 'emacs)
  (evil-set-initial-state 'magit-refs-mode 'emacs)
  (evil-set-initial-state 'magit-revision-mode 'emacs)
  (evil-set-initial-state 'magit-stash-mode 'emacs)
  (evil-set-initial-state 'magit-stashes-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  (evil-set-initial-state 'magit-wazzup-mode 'emacs)

  (evil-leader/set-key
    "g l" 'magit-log
    "g c" 'magit-commit
    "g C" 'magit-commit-amend
    "g s" 'magit-status
    "g d" 'magit-diff-buffer-file
    "g b" (lambda () (interactive)
            (if (bound-and-true-p magit-blame-mode)
              (magit-blame-quit)
              (call-interactively 'magit-blame)))
    "g w" 'magit-stage-file)

  :config

  (add-hook 'git-commit-mode-hook
    (lambda () (auto-fill-mode 0)))

  (define-key magit-file-section-map (kbd "C-j") nil)
  (define-key magit-hunk-section-map (kbd "C-j") nil)

  (evil-define-key 'emacs magit-file-section-map
    (kbd "RET") 'magit-diff-visit-file-other-window)

  (evil-define-key 'emacs magit-hunk-section-map
    (kbd "RET") 'magit-diff-visit-file-other-window)

  (evil-define-key 'emacs magit-blame-mode-map
    (kbd "RET") 'magit-show-commit)

  (evil-define-key 'emacs magit-status-mode-map
    (kbd "p") 'magit-push-popup
    (kbd "l") 'magit-log-popup)

  (evil-define-key 'emacs git-rebase-mode-map
    (kbd "s") 'git-rebase-squash
    (kbd "p") 'git-rebase-pick
    (kbd "r") 'git-rebase-reword
    (kbd "e") 'git-rebase-edit
    (kbd "N") 'git-rebase-move-line-down
    (kbd "P") 'git-rebase-move-line-up
    (kbd "RET") 'git-rebase-show-commit))

(use-package gist
  :init
  (evil-set-initial-state 'gist-list-mode 'emacs))

(use-package git-link)

(use-package flycheck
  :diminish flycheck-mode
  :commands ( global-flycheck-mode flycheck-mode
              counsel-flycheck
              flycheck-define-checker counsel-flycheck
              flycheck-error-list-refresh
              flycheck-error-list-set-source
              flycheck-error-list-reset-filter )
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
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc))

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

(use-package turnip
  :config
  (evil-leader/set-key "ts" 'turnip-send-region))

(use-package flyspell
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
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
  :diminish (yas-minor-mode . " y")
  :init
  (evil-define-key 'insert yas-minor-mode-map (kbd "DEL") 'yas-skip-and-clear-or-backward-delete-char)
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-e") 'yas-expand)
  (setq yas-snippet-dirs
    '("~/.emacs.d/.snippets/yasnippet-snippets"
       "~/.emacs.d/.snippets/personal")))

(use-package counsel-dash
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
  (evil-leader/set-key "f" (lambda () (interactive) (counsel-dash (thing-at-point 'symbol))))
  (define-key evil-normal-state-map (kbd "C-f") 'counsel-dash)
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
  (add-hook 'ruby-mode-hook (lambda () (setq-local counsel-dash-docsets '("Ruby"))))
  (add-hook 'dockerfile-mode-hook (lambda () (setq-local counsel-dash-docsets '("Docker"))))
  (add-hook 'js2-minor-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "NodeJS"))))
  (add-hook 'web-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "HTML""CSS"))))
  (add-hook 'scss-mode-hook (lambda () (setq-local counsel-dash-docsets '("CSS"))))
  (add-hook 'swift-mode-hook (lambda () (setq-local counsel-dash-docsets '("iOS" "Swift")))))

(use-package company
  :diminish " c"
  :commands global-company-mode
  :defines company-dabbrev-downcase
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t)

  :config
  (use-package company-emoji)
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

;; LANGUAGE PACKS
;; ================================================================================

(use-package text-mode
  :preface (provide 'text-mode)
  :no-require t
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (setq-default fill-column 80))

(use-package js2-mode
  :diminish js2-minor-mode
  :commands (js2-mode js2-minor-mode)
  :mode ("\\.js\\'" . js-mode)
  :mode ("\\.ts\\'" . js-mode)
  :interpreter ("node" . js-mode)
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js-indent-level 2)

  (and (executable-find "tern")
    (progn
      (use-package tern
        :diminish " T"
        :commands (tern-mode tern-mode-enable)
        :init
        (add-hook 'js2-minor-mode-hook 'tern-mode-enable)
        (add-hook 'js-mode-hook 'tern-mode-enable)
        (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (tern-mode-enable)))))

      (use-package company-tern
        :config
        (add-to-list 'company-backends 'company-tern))))

  :config
  (evil-define-key 'insert js2-minor-mode-map (kbd "RET") 'js2-line-break))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :commands (tide-mode tide-setup)
  :preface (defun setup-tide-mode ()
             (interactive)
             (message "Setting up tide mode")
             (tide-setup)
             (tide-hl-identifier-mode +1))
  :init
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook
    (lambda ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))))

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package web-mode
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
  (use-package emmet-mode
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
  :mode "\\.fish\\'")

(use-package less-css-mode
  :mode "\\.less\\'")

(use-package stylus-mode
  :mode "\\.stylus\\'")

(use-package scss-mode
  :mode "\\.sass\\'"
  :mode "\\.scss\\'"
  :init (setq scss-compile-at-save nil))

(use-package php-mode
  :mode "\\.php\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :mode "\\.markdown\\'"
  :mode "^README\\'"
  :commands markdown-mode
  :config
  (add-hook 'markdown-mode-hook
    (lambda ()
      (turn-on-orgtbl)
      (add-hook 'before-save-hook 'convert-orgtbl-to-gfm nil t))))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package swift-mode
  :mode "\\.swift\\'"
  :init
  (use-package company-sourcekit
    :load-path "vendor/company-sourcekit"
    :init (setq company-sourcekit-use-yasnippet t)
    :config (add-to-list 'company-backends 'company-sourcekit))
  (add-to-list 'flycheck-checkers 'swift))

(use-package dockerfile-mode
  :mode "^Dockerfile\\'"
  :mode "^Dockerfile.*\\'")

(use-package puppet-mode
  :mode "\\.pp\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :mode "\\.yml\\'")

(use-package ruby-mode
  :mode "\\.rb\\'"
  :init (use-package robe
          :init (when (executable-find "pry") (add-hook 'ruby-mode-hook 'robe-mode))
          :config (when (executable-find "pry") (add-to-list 'company-backends 'company-robe))))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (use-package racer :commands racer-mode)
  :config (add-hook 'rust-mode-hook #'racer-mode))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :mode "\\.lhs\\'"
  :mode "config/models\\'"
  :mode "config/routes\\'"
  :commands counsel-hoogle
  :preface
  (defvar counsel-hoogle-history-input nil
    "Input history used by `ivy-read'.")
  (defun counsel-hoogle (&optional initial)
    "Query hoogle using counsel.
INITIAL will be used as the initial input, if given."
    (interactive)
    (ivy-read "Hoogle: "
      #'(lambda (s &rest _)
          (if (eq (length s) 0) nil
            (split-string
              (shell-command-to-string
                (concat
                  haskell-hoogle-command
                  " "
                  (shell-quote-argument s))) "\n")))
      :dynamic-collection t
      :history 'counsel-hoogle-history-input
      :initial-input initial
      :action (lambda (s)
                (browse-url
                  (format haskell-hoogle-url
                    (url-hexify-string ivy-text))))))
  :init
  (setq haskell-hoogle-command "stack hoogle -- --count=30")
  (setq haskell-hoogle-url "https://www.stackage.org/lts/hoogle?q=%s")
  (setq haskell-process-type 'stack-ghci)
  (and (executable-find "stylish-haskell") (setq haskell-stylish-on-save t))
  (use-package intero
    :init
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook 'intero-mode)
    (evil-set-initial-state 'intero-repl-mode 'emacs)

    :config
    (evil-define-key 'normal haskell-mode-map (kbd "M-i") 'intero-info)
    (evil-leader/set-key-for-mode 'haskell-mode "t" 'intero-type-at)
    (evil-leader/set-key-for-mode 'haskell-mode "f" 'intero-goto-definition))

  :config
  (evil-define-key 'normal haskell-mode-map (kbd "?") 'counsel-hoogle))

(use-package go-mode
  :mode "\\.go\\'"
  :init (use-package company-go
          :load-path "vendor/gocode/emacs-company"
          :init (setq company-go-show-annotation t)
          :config (add-to-list 'company-backends 'company-go))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (use-package flycheck-ledger)

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

  (evil-set-initial-state 'ledger-reconcile-mode 'emacs)

  (evil-leader/set-key-for-mode 'ledger-mode
    "n" 'ledger-add-transaction
    "r" 'ledger-reconcile
    "c" 'ledger-toggle-current-transaction
    "d" 'ledger-delete-current-transaction
    "?" 'ledger-display-balance-at-point))

(use-package conf-mode
  :ensure nil
  :mode "\\.conf\\'"
  :mode "\\.ini\\'"
  :mode "\\.toml\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

;; PROGRAMS
;; ================================================================================

(use-package sx
  :commands (sx-tab-newest sx-search sx-authenticate sx-ask
              sx-inbox sx-tab-month sx-tab-starred sx-tab-featured
              sx-tab-topvoted sx-tab-frontpage sx-tab-unanswered
              sx-tab-unanswered-my-tags))

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

(use-package org-capture
  :init
  (setq org-default-notes-file "~/Google Drive/notes.org")
  (evil-leader/set-key "c c" 'org-capture)
  (evil-leader/set-key "c r" (lambda () (interactive) (find-file org-default-notes-file)))
  (evil-leader/set-key "c n" (lambda () (interactive) (find-file org-default-notes-file)))
  (evil-leader/set-key-for-mode 'org-mode "r" 'org-capture-refile)
  (evil-leader/set-key-for-mode 'org-capture-mode "c" 'org-capture-finalize)
  (evil-leader/set-key-for-mode 'org-capture-mode "w" 'org-capture-finalize)
  (evil-leader/set-key-for-mode 'org-capture-mode "k" 'org-capture-kill)

  (setq org-capture-templates '( ("t" "todo" entry (file+headline org-default-notes-file "Tasks")
                                   "* TODO %? %U %a")
                                 ("n" "note" entry (file+datetree org-default-notes-file)
                                   "* %? :NOTE:\n%U\n%a\n"))))

;; Bootloader
;; ================================================================================

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1) ;; Disable menu bar
(auto-save-mode nil) ;; Disable autosaving
(show-paren-mode t) ;; Show matching parens
(global-auto-revert-mode 1)
(column-number-mode 1)
(global-undo-tree-mode 1)

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
