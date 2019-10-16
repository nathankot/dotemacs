;;; hyperfuse-theme.el

;; Copyright (C)2017 Nathan Kot

;; Author: Nathan Kot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An higher contrast version of the Hyperfuse theme

;;; Credits:


;; Forked from Nantas Nardelli's <nantas.nardelli@gmail.com> Hyperfuse theme
;; which can be found at: https://github.com/edran/hyperfuse-emacs

;;; Code:

(deftheme hyperfuse "To match the Hyperfuse keyset colorway")

;;; Color Palette

(defvar hyperfuse-colors-alist
  '(("hyperfuse-fg+1"      . "#6e3f6c")
    ("hyperfuse-fg"        . nil)
    ("hyperfuse-fg-1"      . "magenta")
    ("hyperfuse-bg-2"      . "#9C9C9C")
    ("hyperfuse-bg-1"      . "white")
    ("hyperfuse-bg-05"     . "white")
    ("hyperfuse-bg"        . nil)
    ("hyperfuse-bg+05"     . "#FFFEFF")
    ("hyperfuse-bg+1"      . "#F8F8F8")
    ("hyperfuse-bg+2"      . "#FEFEFE")
    ("hyperfuse-bg+3"      . "#ffffff")
    ("hyperfuse-red+1"     . "brightred")
    ("hyperfuse-red"       . "red")
    ("hyperfuse-red-1"     . "#C99090")
    ("hyperfuse-red-2"     . "#B98080")
    ("hyperfuse-red-3"     . "#A97070")
    ("hyperfuse-red-4"     . "#996060")
    ("hyperfuse-orange"    . "#B89279")
    ("hyperfuse-yellow"    . "yellow")
    ("hyperfuse-yellow-1"  . "brightyellow")
    ("hyperfuse-yellow-2"  . "#7D755D")
    ("hyperfuse-green"     . "brightgreen")
    ("hyperfuse-cyan"      . "cyan")
    ("hyperfuse-blue+1"    . "#84D9D3")
    ("hyperfuse-blue"      . "blue")
    ("hyperfuse-blue-1"    . "brightblue")
    ("hyperfuse-blue-2"    . "brightcyan")
    ("hyperfuse-blue-3"    . "#699598")
    ("hyperfuse-blue-4"    . "#577C7E")
    ("hyperfuse-blue-5"    . "#3D5758")
    ("hyperfuse-magenta"   . "magenta"))
  "List of Hyperfuse colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro hyperfuse-with-color-variables (&rest body)
  "`let' bind all colors defined in `hyperfuse-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   hyperfuse-colors-alist))
     ,@body))

;;; Theme Faces
(hyperfuse-with-color-variables
  (custom-theme-set-faces
   'hyperfuse
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,hyperfuse-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,hyperfuse-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-bg))))
   `(cursor ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-fg+1))))
   `(escape-glyph ((t (:foreground ,hyperfuse-yellow :bold t))))
   `(fringe ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-bg))))
   `(header-line ((t (:foreground ,hyperfuse-yellow :background ,hyperfuse-bg :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,hyperfuse-bg+2))))
   `(success ((t (:foreground ,hyperfuse-green :weight bold))))
   `(warning ((t (:foreground ,hyperfuse-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,hyperfuse-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,hyperfuse-green))))
   `(compilation-error-face ((t (:foreground ,hyperfuse-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,hyperfuse-fg))))
   `(compilation-info-face ((t (:foreground ,hyperfuse-blue))))
   `(compilation-info ((t (:foreground ,hyperfuse-green :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,hyperfuse-green))))
   `(compilation-line-face ((t (:foreground ,hyperfuse-yellow))))
   `(compilation-line-number ((t (:foreground ,hyperfuse-yellow :background ,hyperfuse-bg))))
   `(compilation-message-face ((t (:foreground ,hyperfuse-blue))))
   `(compilation-warning-face ((t (:foreground ,hyperfuse-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,hyperfuse-green :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,hyperfuse-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,hyperfuse-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,hyperfuse-fg))))
   `(grep-error-face ((t (:foreground ,hyperfuse-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,hyperfuse-blue))))
   `(grep-match-face ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(match ((t (:background ,hyperfuse-bg+2 :foreground ,hyperfuse-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,hyperfuse-fg :weight bold :background ,hyperfuse-cyan))))
   `(isearch-fail ((t (:foreground ,hyperfuse-bg-05 :background ,hyperfuse-red-4))))
   `(lazy-highlight ((t (:foreground ,hyperfuse-bg-05 :weight bold :background ,hyperfuse-blue-2))))

   `(menu ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-bg+2))))
   `(minibuffer-prompt ((t (:foreground ,hyperfuse-fg-1))))
   `(mode-line-buffer-id ((t (:foreground ,hyperfuse-fg-1 :weight bold))))
   `(mode-line ((,class (:foreground ,hyperfuse-fg-1 :background ,hyperfuse-bg-1 :box (:line-width -1 :style released-button))) (t :inverse-video t)))
   `(mode-line-inactive ((t (:foreground ,hyperfuse-fg-1 :background ,hyperfuse-bg-05 :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,hyperfuse-bg+2))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,hyperfuse-bg+2))))
   `(trailing-whitespace ((t (:background ,hyperfuse-red))))
   `(vertical-border ((t (:foreground ,hyperfuse-bg+2))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,hyperfuse-blue :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,hyperfuse-bg-2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,hyperfuse-bg-2))))
   `(font-lock-doc-face ((t (:foreground ,hyperfuse-bg-2))))
   `(font-lock-keyword-face ((t (:foreground ,hyperfuse-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,hyperfuse-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,hyperfuse-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,hyperfuse-red+1))))
   `(font-lock-type-face ((t (:foreground ,hyperfuse-blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,hyperfuse-yellow-1))))
   `(font-lock-variable-name-face ((t (:foreground ,hyperfuse-yellow))))
   `(font-lock-constant-face ((t (:foreground ,hyperfuse-red-4))))
   `(font-lock-warning-face ((t (:foreground ,hyperfuse-red-1 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

;;;; Third-party

;;;;; evil mode
   `(evil-search-highlight-persist-highlight-face ((t (:foreground ,hyperfuse-bg-05 :weight bold :background ,hyperfuse-blue-2))))

;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,hyperfuse-green))))
   `(android-mode-error-face ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,hyperfuse-fg))))
   `(android-mode-verbose-face ((t (:foreground ,hyperfuse-green))))
   `(android-mode-warning-face ((t (:foreground ,hyperfuse-yellow))))

;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,hyperfuse-cyan :weight bold))))

;;;;; company-mode
   `(company-tooltip ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,hyperfuse-bg :background ,hyperfuse-cyan))))
   `(company-tooltip-mouse ((t (:background ,hyperfuse-bg+1))))
   `(company-tooltip-common ((t (:foreground ,hyperfuse-green))))
   `(company-tooltip-common-selection ((t (:foreground ,hyperfuse-green))))
   `(company-tooltip-annotation ((t (:foreground ,hyperfuse-red :background ,hyperfuse-bg+1))))
   `(company-scrollbar-fg ((t (:background ,hyperfuse-bg-2))))
   `(company-scrollbar-bg ((t (:background ,hyperfuse-bg-1))))
   `(company-preview ((t (:background ,hyperfuse-green))))
   `(company-preview-common ((t (:background ,hyperfuse-bg+1))))

;;;;; lsp-ui
   `(lsp-ui-doc-background ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-bg+1))))

;;;;; magit
   `(magit-item-highlight    ((t (:background ,hyperfuse-bg+2))))
   `(magit-process-ok        ((t (:foreground ,hyperfuse-green))))
   `(magit-process-ng        ((t (:foreground ,hyperfuse-red))))
   `(magit-branch            ((t (:foreground ,hyperfuse-blue-5))))
   `(magit-branch-remote     ((t (:foreground ,hyperfuse-green))))
   `(magit-section-title     ((t (:foreground ,hyperfuse-yellow))))
   `(magit-section-highlight ((t (:background ,hyperfuse-bg))))
   `(magit-section-heading   ((t (:foreground ,hyperfuse-red-3))))
   `(magit-hash              ((t (:foreground ,hyperfuse-cyan))))

   `(magit-log-author ((t (:foreground ,hyperfuse-fg+1))))
   `(magit-log-date   ((t (:foreground ,hyperfuse-bg-2))))
   `(magit-log-sha1   ((t (:foreground ,hyperfuse-cyan))))

   `(magit-diff-file-heading           ((t (:foreground nil :background nil))))
   `(magit-diff-file-heading-highlight ((t (:foreground nil :background nil))))
   `(magit-diff-hunk-heading           ((t (:foreground ,hyperfuse-yellow-1 :background nil))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,hyperfuse-yellow :background nil))))
   `(magit-diff-context                ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-bg-05))))
   `(magit-diff-context-highlight      ((t (:foreground ,hyperfuse-fg-1 :background ,hyperfuse-bg+05))))
   `(magit-diff-removed                ((t (:foreground ,hyperfuse-red :background nil))))
   `(magit-diff-removed-highlight      ((t (:foreground ,hyperfuse-red :background nil))))
   `(magit-diff-added                  ((t (:foreground ,hyperfuse-green :background nil))))
   `(magit-diff-added-highlight        ((t (:foreground ,hyperfuse-green :background nil))))
   `(magit-diff-changed                ((t (:foreground ,hyperfuse-yellow :background nil))))
   `(magit-diff-changed-highlight      ((t (:foreground ,hyperfuse-yellow :background nil))))

;;;;; diff
   `(diff-added          ((t (:inherit magit-diff-added))))
   `(diff-changed        ((t (:inherit magit-diff-changed))))
   `(diff-removed        ((t (:inherit magit-diff-removed))))
   `(diff-refine-added   ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change  ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header         ((t (:inherit magit-diff-hunk-heading))))
   `(diff-file-header    ((t (:inherit magit-diff-file-heading))))

;;;;; diff-hl
   `(diff-hl-change  ((t (:inherit magit-diff-changed))))
   `(diff-hl-delete  ((t (:inherit magit-diff-removed))))
   `(diff-hl-insert  ((t (:inherit magit-diff-added))))
   `(diff-hl-unknown ((t (:inherit magit-diff-context))))

;;;;; ediff
   `(ediff-current-diff-A ((t (:inherit magit-diff-removed))))
   `(ediff-current-diff-Ancestor ((t (:inherit magit-diff-removed))))
   `(ediff-current-diff-B ((t (:inherit magit-diff-added))))
   `(ediff-current-diff-C ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-blue-5))))
   `(ediff-even-diff-A ((t (:background ,hyperfuse-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,hyperfuse-bg+1))))
   `(ediff-even-diff-B ((t (:background ,hyperfuse-bg+1))))
   `(ediff-even-diff-C ((t (:background ,hyperfuse-bg+1))))
   `(ediff-fine-diff-A ((t (:inherit magit-diff-removed))))
   `(ediff-fine-diff-Ancestor ((t (:inherit magit-diff-removed))))
   `(ediff-fine-diff-B ((t (:inherit magit-diff-added))))
   `(ediff-fine-diff-C ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,hyperfuse-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,hyperfuse-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,hyperfuse-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,hyperfuse-bg+2))))

;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:inherit magit-hash))))

;;;;; avy
   `(avy-lead-face ((t (:background ,hyperfuse-red-2 :foreground ,hyperfuse-fg+1))))
   `(avy-lead-face-0 ((t (:background ,hyperfuse-green :foreground ,hyperfuse-fg+1))))
   `(avy-lead-face-1 ((t (:background ,hyperfuse-cyan :foreground ,hyperfuse-fg+1))))
   `(avy-lead-face-2 ((t (:background ,hyperfuse-red+1 :foreground ,hyperfuse-fg+1))))

;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-red-1) :inherit unspecified))
      (t (:foreground ,hyperfuse-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-yellow) :inherit unspecified))
      (t (:foreground ,hyperfuse-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-cyan) :inherit unspecified))
      (t (:foreground ,hyperfuse-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,hyperfuse-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,hyperfuse-cyan :weight bold))))

;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hyperfuse-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hyperfuse-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hyperfuse-green :weight bold :underline t))))

;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-orange) :inherit unspecified))
      (t (:foreground ,hyperfuse-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-red) :inherit unspecified))
      (t (:foreground ,hyperfuse-red-1 :weight bold :underline t))))

;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,hyperfuse-green :weight bold :inverse-video nil))))
   `(git-gutter:deleted ((t (:foreground ,hyperfuse-red :weight bold :inverse-video nil))))
   `(git-gutter:modified ((t (:foreground ,hyperfuse-yellow :weight bold :inverse-video nil))))
   `(git-gutter:unchanged ((t (:foreground ,hyperfuse-fg :weight bold :inverse-video t))))

;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,hyperfuse-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,hyperfuse-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,hyperfuse-magenta :weight bold))))

;;;;; ivy-mode
   `(ivy-current-match ((t (:foreground ,hyperfuse-fg-1 :background ,hyperfuse-bg+3 :unfg+1ine nil))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,hyperfuse-bg+3 :weight bold :background ,hyperfuse-blue))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,hyperfuse-bg+3 :weight bold :background ,hyperfuse-blue-1))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,hyperfuse-bg+3 :weight bold :background ,hyperfuse-blue-2))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,hyperfuse-bg+3 :weight bold :background ,hyperfuse-blue-3))))

;;;;; hl-line-mode
   `(hl-line-face ((,class (:foreground ,hyperfuse-bg-2 :background ,hyperfuse-bg+1)) (t :weight bold)))
   `(hl-line ((,class (:foreground ,hyperfuse-bg-2 :background ,hyperfuse-bg+1)) (t :weight bold)))

;;;;; linum-relative
   `(linum-relative-current-face ((,class (:foreground ,hyperfuse-cyan :background ,hyperfuse-bg+1)) (t :weight bold)))

;;;;; linum-mode
   `(linum ((t (:foreground ,hyperfuse-bg-2 :background ,hyperfuse-bg))))

;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,hyperfuse-bg+1)) (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,hyperfuse-yellow))))
   `(ido-indicator ((t (:foreground ,hyperfuse-yellow :background ,hyperfuse-red-4))))

;;;;; js2-mode
   `(js2-warning ((t (:underline ,hyperfuse-orange))))
   `(js2-error ((t (:foreground ,hyperfuse-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,hyperfuse-green))))
   `(js2-jsdoc-type ((t (:foreground ,hyperfuse-green))))
   `(js2-jsdoc-value ((t (:foreground ,hyperfuse-green))))
   `(js2-function-param ((t (:foreground, hyperfuse-green))))
   `(js2-external-variable ((t (:foreground ,hyperfuse-orange))))

;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,hyperfuse-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,hyperfuse-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,hyperfuse-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,hyperfuse-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,hyperfuse-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,hyperfuse-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,hyperfuse-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,hyperfuse-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,hyperfuse-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,hyperfuse-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,hyperfuse-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,hyperfuse-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,hyperfuse-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,hyperfuse-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,hyperfuse-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,hyperfuse-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,hyperfuse-orange :weight normal))))

;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,hyperfuse-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,hyperfuse-fg :weight bold))))
   `(org-checkbox ((t (:background ,hyperfuse-bg+2 :foreground ,hyperfuse-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,hyperfuse-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,hyperfuse-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,hyperfuse-green))))
   `(org-formula ((t (:foreground ,hyperfuse-yellow-2))))
   `(org-headline-done ((t (:foreground ,hyperfuse-green))))
   `(org-hide ((t (:foreground ,hyperfuse-bg-1))))
   `(org-level-1 ((t (:foreground ,hyperfuse-orange))))
   `(org-level-2 ((t (:foreground ,hyperfuse-green))))
   `(org-level-3 ((t (:foreground ,hyperfuse-blue-1))))
   `(org-level-4 ((t (:foreground ,hyperfuse-yellow-2))))
   `(org-level-5 ((t (:foreground ,hyperfuse-cyan))))
   `(org-level-6 ((t (:foreground ,hyperfuse-green))))
   `(org-level-7 ((t (:foreground ,hyperfuse-red-4))))
   `(org-level-8 ((t (:foreground ,hyperfuse-blue-4))))
   `(org-link ((t (:foreground ,hyperfuse-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,hyperfuse-green))))
   `(org-scheduled-previously ((t (:foreground ,hyperfuse-red))))
   `(org-scheduled-today ((t (:foreground ,hyperfuse-blue+1))))
   `(org-sexp-date ((t (:foreground ,hyperfuse-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,hyperfuse-green))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,hyperfuse-orange))))
   `(org-todo ((t (:bold t :foreground ,hyperfuse-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,hyperfuse-red :weight bold :underline nil))))
   `(org-column ((t (:background ,hyperfuse-bg-1))))
   `(org-column-title ((t (:background ,hyperfuse-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,hyperfuse-bg+3 :background ,hyperfuse-red-1))))
   `(org-ellipsis ((t (:foreground ,hyperfuse-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,hyperfuse-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,hyperfuse-orange))))
   `(outline-2 ((t (:foreground ,hyperfuse-green))))
   `(outline-3 ((t (:foreground ,hyperfuse-blue-1))))
   `(outline-4 ((t (:foreground ,hyperfuse-yellow-2))))
   `(outline-5 ((t (:foreground ,hyperfuse-cyan))))
   `(outline-6 ((t (:foreground ,hyperfuse-green))))
   `(outline-7 ((t (:foreground ,hyperfuse-red-4))))
   `(outline-8 ((t (:foreground ,hyperfuse-blue-4))))

;;;;; structured haskell mode
   `(shm-current-face ((t (:background ,hyperfuse-bg-2))))
   `(shm-quarantine-face ((t (:background ,hyperfuse-blue-5))))

;;;;; perspective
   `(persp-selected-face ((t (:foreground ,hyperfuse-yellow-2 :inherit mode-line))))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,hyperfuse-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,hyperfuse-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,hyperfuse-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,hyperfuse-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,hyperfuse-green))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,hyperfuse-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,hyperfuse-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,hyperfuse-green))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,hyperfuse-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,hyperfuse-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,hyperfuse-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,hyperfuse-blue-5))))

;;;;; sh-mode
   `(sh-heredoc     ((t (:inherit font-lock-string-face))))
   `(sh-quoted-exec ((t (:inherit font-lock-comment-delimiter-face))))

;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,hyperfuse-red+1 :background ,hyperfuse-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,hyperfuse-bg+3 :weight bold))))

;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,hyperfuse-red+1 :background ,hyperfuse-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,hyperfuse-bg+3 :weight bold))))

;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))

;;;;; term
   `(term-color-black ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-fg-1))))
   `(term-color-red ((t (:foreground ,hyperfuse-red-2 :background ,hyperfuse-red-4))))
   `(term-color-green ((t (:foreground ,hyperfuse-green :background ,hyperfuse-green))))
   `(term-color-yellow ((t (:foreground ,hyperfuse-orange :background ,hyperfuse-yellow))))
   `(term-color-blue ((t (:foreground ,hyperfuse-blue-1 :background ,hyperfuse-blue-4))))
   `(term-color-magenta ((t (:foreground ,hyperfuse-magenta :background ,hyperfuse-red))))
   `(term-color-cyan ((t (:foreground ,hyperfuse-cyan :background ,hyperfuse-blue))))
   `(term-color-white ((t (:foreground ,hyperfuse-bg :background ,hyperfuse-bg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))

;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,hyperfuse-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,hyperfuse-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,hyperfuse-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,hyperfuse-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,hyperfuse-cyan))))

;;;;; which-func-mode
   `(which-func ((t (:foreground ,hyperfuse-green))))

))

;;; Theme Variables
(hyperfuse-with-color-variables
  (custom-theme-set-variables
   'hyperfuse
;;;;; ansi-color
   `(ansi-color-names-vector [,hyperfuse-bg ,hyperfuse-red ,hyperfuse-green ,hyperfuse-yellow ,hyperfuse-blue ,hyperfuse-magenta ,hyperfuse-cyan ,hyperfuse-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,hyperfuse-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,hyperfuse-red-1)
       ( 40. . ,hyperfuse-red)
       ( 60. . ,hyperfuse-orange)
       ( 80. . ,hyperfuse-yellow-2)
       (100. . ,hyperfuse-yellow-1)
       (120. . ,hyperfuse-yellow)
       (160. . ,hyperfuse-green)
       (260. . ,hyperfuse-cyan)
       (280. . ,hyperfuse-blue-2)
       (300. . ,hyperfuse-blue-1)
       (320. . ,hyperfuse-blue)
       (340. . ,hyperfuse-blue+1)
       (360. . ,hyperfuse-magenta)))
   `(vc-annotate-very-old-color ,hyperfuse-magenta)
   `(vc-annotate-background ,hyperfuse-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar hyperfuse-add-font-lock-keywords nil
  "Whether to add font-lock keywords for hyperfuse color names.
In buffers visiting library `hyperfuse-theme.el' the hyperfuse
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar hyperfuse-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after hyperfuse activate)
;;   "Maybe also add font-lock keywords for hyperfuse colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or hyperfuse-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "hyperfuse-theme.el")))
;;     (unless hyperfuse-colors-font-lock-keywords
;;       (setq hyperfuse-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car hyperfuse-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc hyperfuse-colors-alist))))))
;;     (font-lock-add-keywords nil hyperfuse-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after hyperfuse activate)
;;   "Also remove font-lock keywords for hyperfuse colors."
;;   (font-lock-remove-keywords nil hyperfuse-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'hyperfuse)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; hyperfuse-theme.el ends here
