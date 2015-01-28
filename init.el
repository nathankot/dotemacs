;;; Package: Nathan's Emacs
;;; Commentary:
;;; Extensive use of `use-package`

(custom-set-variables
  '(custom-safe-themes
     (quote
       ("c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482"
        "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f"
        default))))

(custom-set-faces)

;; Env
(setenv "PATH" (concat (getenv "PATH") ":/opt/boxen/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/boxen/homebrew/bin")))
(menu-bar-mode -1) ;; Disable menu bar
(setq-default tab-width 2 indent-tabs-mode nil) ;; Spaces
(auto-save-mode -1) ;; Disable autosaving
(show-paren-mode t) ;; Show matching parens
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1) ;; Smooth scrolling
(setq gc-cons-threshold 20000000) ;; Increase garbage collection limit

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

(use-package darkmine-theme
  :ensure t
  :init (load-theme 'darkmine t))

(use-package relative-line-numbers
  :ensure t
  :init
  (progn
    ;; Function for displaying line numbers
    (defun linum-format-func (line)
      (concat " " (number-to-string (abs line)) " "))
    (setq relative-line-numbers-format 'linum-format-func)
    (add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
    (add-hook 'prog-mode-hook 'line-number-mode t)
    (add-hook 'prog-mode-hook 'column-number-mode t)))

(use-package company
  :ensure t
  :commands global-company-mode
  :config (progn
            (use-package company-tern
              :ensure t
              :init (add-to-list 'company-backends 'company-tern)
              :config (add-hook 'js-mode-hook (lambda() (tern-mode t)))))
  :idle (global-company-mode))

(use-package smart-mode-line
  :ensure t
  :init (sml/setup)
  :config (sml/apply-theme 'respectful))

(use-package evil
  :commands evil-mode
  :ensure t
  :init (progn
          (setq evil-want-C-u-scroll t
                evil-overriding-maps nil
                evil-intercept-maps nil
                evil-shift-width 2)
          (evil-mode 1))
  ; These aren't exactly evil-specific, but they are fundamental to the workflow.
  :bind ("C-q" . delete-window)
  :config (progn
            (global-evil-search-highlight-persist t)

            (use-package evil-leader
              :ensure t
              :commands evil-mode
              :init (global-evil-leader-mode)
              :config (progn
                        (evil-leader/set-leader ",")
                        (evil-leader/set-key "w" 'save-buffer)
                        (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)))

            (use-package evil-commentary
              :ensure t
              :commands evil-mode
              :init (evil-commentary-default-setup))

            (use-package evil-snipe
              :ensure t
              :commands (evil-snipe-mode evil-mode)
              :init     (global-evil-snipe-mode 1))

            (use-package evil-surround
              :ensure t
              :commands (evil-mode evil-surround-mode)
              :init (global-evil-surround-mode 1))

            (define-key evil-normal-state-map (kbd "C-q") 'delete-window)
            (define-key evil-normal-state-map (kbd "C-v") 'split-window-vertically)
            (define-key evil-normal-state-map (kbd "C-V") 'split-window-horizontally)
            (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
            (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)
            (define-key evil-normal-state-map (kbd "C-l") 'evil-window-increase-width)
            (define-key evil-normal-state-map (kbd "C-h") 'evil-window-decrease-width)
            (define-key evil-visual-state-map (kbd "SPC") 'evil-search-forward)
            (define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)
            (define-key evil-visual-state-map (kbd "S-SPC") 'evil-search-backward)
            (define-key evil-normal-state-map (kbd "S-SPC") 'evil-search-backward)
            (define-key evil-normal-state-map (kbd "] l") 'occur-next)
            (define-key evil-normal-state-map (kbd "[ l") 'occur-prev)))

(use-package ido
  :commands ido-mode
  :ensure t
  :init   (progn
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces nil))
            (ido-mode 1)
            (ido-everywhere 1))

(use-package flx
  :ensure t
  :config (progn
            (use-package flx-ido
              :ensure t
              :commands ido-mode
              :init (flx-ido-mode 1))))

(use-package helm
  :ensure t
  :init (progn
          (require 'helm-config)
          ; Make sure that helm always displayed below
          ; the current window
          (setq helm-display-function (lambda (buf)
            (split-window-vertically)
            (other-window 1)
            (switch-to-buffer buf))))
  :config (progn
            (define-key helm-map (kbd "C-p") 'helm-keyboard-quit)
            (define-key helm-map (kbd "C-j") 'helm-next-line)
            (define-key helm-map (kbd "C-k") 'helm-previous-line)
            (define-key helm-map (kbd "C-d") 'helm-buffer-run-kill-persistent)))

;; Projectile
(use-package projectile
  :ensure t
  :bind (("C-p" . helm-projectile)
         ("C-P" . helm-projectile-ag))
  :init (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories ".cache")
            (add-to-list 'projectile-globally-ignored-directories ".tmp")
            (add-to-list 'projectile-globally-ignored-directories "tmp")
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (add-to-list 'projectile-globally-ignored-directories "bower_components")
            (projectile-global-mode)
            (use-package helm-projectile :ensure t)
            (use-package helm-ag :ensure t)))

;; Smex
(use-package smex
  :ensure t
  :commands smex
  :init (progn
          (define-key evil-motion-state-map (kbd ":") 'smex)
          (define-key evil-motion-state-map (kbd "C-;") 'evil-ex)))

;; Git gutter +
; (define-key evil-normal-state-map (kbd "] c") 'git-gutter+-next-hunk)
; (define-key evil-normal-state-map (kbd "[ c") 'git-gutter+-previous-hunk)
(evil-leader/set-key "g a" 'git-gutter+-stage-hunks)
(add-hook 'after-init-hook 'global-git-gutter+-mode)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
; (define-key evil-normal-state-map (kbd "] e") 'flycheck-next-error)
; (define-key evil-normal-state-map (kbd "[ e") 'flycheck-previous-error)
; New jsx checker
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-to-list 'flycheck-checkers 'jsxhint-checker)
; Select the jsx checker under correct circumstances
(add-hook 'web-mode-hook
  (lambda ()
    (when (equal web-mode-content-type "jsx")
      ;; enable flycheck
      (flycheck-select-checker 'jsxhint-checker)
                    (flycheck-mode))))

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
; Setup jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'magic-mode-alist '("\/\*\*.*@jsx" . web-mode))
(eval-after-load 'web-mode
  '(define-key prog-mode-map (kbd "C-x /") 'web-mode-element-close))

;; Magit keybindings
(evil-leader/set-key "g c" 'magit-commit)
(evil-leader/set-key "g C" 'magit-commit-amend)
(evil-leader/set-key "g s" 'magit-status)
(evil-leader/set-key "g d" 'magit-diff-unstaged)
(evil-leader/set-key "g D" 'magit-diff-staged)
(evil-leader/set-key "g b" 'magit-blame-mode)

;; Auto-pair
(require 'autopair)
(autopair-global-mode)

;; Emmet
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)

;; PHP
(require 'php-mode)
