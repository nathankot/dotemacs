;;; package --- Nathan's Emacs
;;; Commentary:
;;; Extensive use of `use-package`

;;; Code:
(custom-set-variables
  '(custom-safe-themes
     (quote
       ("c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482"
        "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f"
        default))))

(custom-set-faces)

;; Use UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(setenv "LC_CTYPE" "UTF-8")

;; Auto-fill on text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Other stuff
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

;; ESSENTIAL PACKAGES
;; ================================================================================

(use-package darkmine-theme
  :ensure t
  :init (load-theme 'darkmine t))

(use-package diminish
  :ensure t
  :init (progn
          (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
          (eval-after-load "company" '(diminish 'company-mode " c"))
          (eval-after-load "yasnippet" '(diminish 'yas-minor-mode " y"))
          (eval-after-load "flycheck" '(diminish 'flycheck-mode " f"))
          (eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
          (eval-after-load "autopair" '(diminish 'autopair-mode))
          (eval-after-load "projectile" '(diminish 'projectile-mode))))

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

            (use-package evil-matchit
              :ensure t
              :commands (global-evil-matchit-mode evil-matchit-mode)
              :init (global-evil-matchit-mode 1))

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

(use-package company
  :ensure t
  :commands global-company-mode
  :idle-priority 1
  :config (progn
            (use-package company-tern
              :commands tern-mode
              :ensure t
              :init (progn
                      (add-to-list 'company-backends 'company-tern)
                      (add-hook 'js-mode-hook (lambda() (tern-mode t)))))
            (define-key company-active-map (kbd "C-i") 'company-select-next)
            (define-key company-active-map (kbd "C-o") 'company-select-previous)
            (add-hook 'less-css-mode-hook (lambda () (set-q-local company-backends '((company-css))))))
  :idle (global-company-mode))

(use-package yasnippet
  :ensure t
  :idle-priority 2
  :commands yas-global-mode
  :init (progn
          (setq yas-snippet-dirs
            '("~/.snippets/yasnippet-snippets"
              "~/.snippets/personal")))
  :config (progn
            (push '(company-semantic :with company-yasnippet) company-backends)
            (evil-define-key 'insert yas-minor-mode-map (kbd "C-e") 'yas-expand)
            (add-hook 'web-mode-hook (lambda () (yas-activate-extra-mode 'js-mode))))
  :idle (yas-global-mode 1))

(use-package smart-mode-line
  :ensure t
  :init (sml/setup)
  :config (sml/apply-theme 'respectful))

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

(use-package projectile
  :ensure t
  :bind ("C-p" . helm-projectile)
  :init (progn
          (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
          (define-key evil-normal-state-map (kbd "C-s") 'helm-projectile-ag))
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories ".cache")
            (add-to-list 'projectile-globally-ignored-directories ".tmp")
            (add-to-list 'projectile-globally-ignored-directories "tmp")
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (add-to-list 'projectile-globally-ignored-directories "bower_components")
            (projectile-global-mode)
            (use-package helm-projectile :ensure t)
            (use-package helm-ag :ensure t)))

(use-package perspective
  :ensure t
  :commands (persp-mode persp-kill persp-switch persp-next persp-prev)
  :init (persp-mode)
  :config (progn
            (use-package persp-projectile :ensure t)
            (define-key evil-normal-state-map (kbd "C-@") 'persp-switch)
            (define-key evil-normal-state-map (kbd "TAB") 'persp-next)
            (define-key evil-normal-state-map (kbd "DEL") 'persp-prev)))

(use-package autopair
  :ensure t
  :init (autopair-global-mode))

(use-package editorconfig
  :ensure t)


;; NON-ESSENTIAL PACKAGES (NO ENSURE)
;; ================================================================================

(use-package smex
  :commands smex
  :bind ("M-x" . smex)
  :init (progn
          (define-key evil-motion-state-map (kbd ":") 'smex)
          (define-key evil-normal-state-map (kbd "C-;") 'evil-ex)
          (define-key evil-motion-state-map (kbd "C-;") 'evil-ex)))

(use-package git-gutter
  :commands git-gutter-mode
  :init (global-git-gutter-mode +1)
  :config (progn
            (git-gutter:linum-setup)
            (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
            (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
            (evil-leader/set-key "g a" 'git-gutter:stage-hunk)
            (evil-leader/set-key "g r" 'git-gutter:revert-hunk)))

(use-package linum-relative
  :init (progn
          (setq linum-relative-format "%3s   ")
          (linum-on)
          (global-linum-mode)))

(use-package magit
  :commands (magit-log magit-status magit-commit magit-commit-amend
             magit-diff-unstaged magit-diff-staged magit-blame-mode
             magit-stage-all)
  :init (progn
          (evil-leader/set-key "g l" 'magit-log)
          (evil-leader/set-key "g c" 'magit-commit)
          (evil-leader/set-key "g C" 'magit-commit-amend)
          (evil-leader/set-key "g s" 'magit-status)
          (evil-leader/set-key "g d" 'magit-diff-unstaged)
          (evil-leader/set-key "g D" 'magit-diff-staged)
          (evil-leader/set-key "g b" 'magit-blame-mode)
          (evil-leader/set-key "g w" 'magit-stage-all)
          (add-to-list 'evil-insert-state-modes 'magit-commit-mode))
  :config (progn
            (define-key magit-diff-mode-map (kbd "j") 'magit-goto-next-section)
            (define-key magit-diff-mode-map (kbd "k") 'magit-goto-previous-section)
            (define-key magit-status-mode-map (kbd "j") 'next-line)
            (define-key magit-status-mode-map (kbd "k") 'previous-line)))

(use-package flycheck
  :commands global-flycheck-mode
  :idle (global-flycheck-mode)
  :config (progn
            (define-key evil-normal-state-map (kbd "] e") 'next-error)
            (define-key evil-normal-state-map (kbd "[ e") 'previous-error)

            (use-package helm-flycheck
              :ensure t
              :commands helm-flycheck
              :init (evil-leader/set-key "e" 'helm-flycheck))

            ; Custom checkers
            (flycheck-define-checker jsxhint-checker
              "A JSX syntax and style checker based on JSXHint."
              :command ("jsxhint" source)
              :error-patterns
              ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
              :modes (web-mode))
            (add-to-list 'flycheck-checkers 'jsxhint-checker)

            ; Checkers for formats
            (when 'web-mode-hook
              (add-hook 'web-mode-hook
              (lambda ()
                (when (equal 'web-mode-content-type "jsx")
                  (flycheck-select-checker 'jsxhint-checker)
                  (flycheck-mode)))))))

(use-package emmet
  :commands emmet-mode
  :init (progn
          (add-hook 'sgml-mode-hook 'emmet-mode)
          (add-hook 'css-mode-hook  'emmet-mode)
          (add-hook 'web-mode-hook 'emmet-mode)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))


;; LANGUAGE PACKS
;; ================================================================================

(use-package js2-mode
  :commands js2-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
          (add-to-list 'interpreter-mode-alist '("node" . js2-mode))))

(use-package web-mode
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
          (add-to-list 'magic-mode-alist '("\/\*\*.*@jsx" . web-mode)))
  :config (progn
            (define-key prog-mode-map (kbd "C-x /") 'web-mode-element-close)))

(use-package less-css-mode
  :commands less-css-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))))

(use-package php-mode)

(provide 'init)
;;; init.el ends here
