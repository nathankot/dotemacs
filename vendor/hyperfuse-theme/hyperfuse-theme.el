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
    ("hyperfuse-fg-1"      . "brightmagenta")
    ("hyperfuse-bg-2"      . "#9C9C9C")
    ("hyperfuse-bg-1"      . "brightwhite")
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
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,hyperfuse-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,hyperfuse-yellow))))
   `(font-latex-italic-face ((t (:foreground ,hyperfuse-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,hyperfuse-orange))))
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
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,hyperfuse-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,hyperfuse-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,hyperfuse-green :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; diff
   `(diff-added ((,class (:foreground ,hyperfuse-green :background nil))
                 (t (:foreground ,hyperfuse-green :background nil))))
   `(diff-changed ((t (:foreground ,hyperfuse-yellow))))
   `(diff-removed ((,class (:foreground ,hyperfuse-red :background nil))
                   (t (:foreground ,hyperfuse-red-3 :background nil))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header ((,class (:background ,hyperfuse-bg+2))
                  (t (:background ,hyperfuse-fg :foreground ,hyperfuse-bg+2))))
   `(diff-file-header
     ((,class (:background ,hyperfuse-bg+2 :foreground ,hyperfuse-fg :bold t))
      (t (:background ,hyperfuse-fg :foreground ,hyperfuse-bg+3 :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,hyperfuse-blue-2 :background ,hyperfuse-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,hyperfuse-red+1 :background ,hyperfuse-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,hyperfuse-green :background ,hyperfuse-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,hyperfuse-yellow :background ,hyperfuse-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,hyperfuse-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,hyperfuse-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,hyperfuse-orange))))
   `(diredp-date-time ((t (:foreground ,hyperfuse-magenta))))
   `(diredp-deletion ((t (:foreground ,hyperfuse-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,hyperfuse-red))))
   `(diredp-dir-heading ((t (:foreground ,hyperfuse-blue :background ,hyperfuse-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,hyperfuse-cyan))))
   `(diredp-exec-priv ((t (:foreground ,hyperfuse-red))))
   `(diredp-executable-tag ((t (:foreground ,hyperfuse-green))))
   `(diredp-file-name ((t (:foreground ,hyperfuse-blue))))
   `(diredp-file-suffix ((t (:foreground ,hyperfuse-green))))
   `(diredp-flag-mark ((t (:foreground ,hyperfuse-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,hyperfuse-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,hyperfuse-red))))
   `(diredp-link-priv ((t (:foreground ,hyperfuse-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,hyperfuse-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,hyperfuse-orange))))
   `(diredp-no-priv ((t (:foreground ,hyperfuse-fg))))
   `(diredp-number ((t (:foreground ,hyperfuse-green))))
   `(diredp-other-priv ((t (:foreground ,hyperfuse-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,hyperfuse-red-1))))
   `(diredp-read-priv ((t (:foreground ,hyperfuse-green))))
   `(diredp-symlink ((t (:foreground ,hyperfuse-yellow))))
   `(diredp-write-priv ((t (:foreground ,hyperfuse-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-green))))
   `(ediff-current-diff-C ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-blue-5))))
   `(ediff-even-diff-A ((t (:background ,hyperfuse-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,hyperfuse-bg+1))))
   `(ediff-even-diff-B ((t (:background ,hyperfuse-bg+1))))
   `(ediff-even-diff-C ((t (:background ,hyperfuse-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,hyperfuse-fg :background ,hyperfuse-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,hyperfuse-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,hyperfuse-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,hyperfuse-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,hyperfuse-bg+2))))
;;;;; avy
   `(avy-lead-face ((t (:background ,hyperfuse-red-2 :foreground ,hyperfuse-fg+1))))
   `(avy-lead-face-0 ((t (:background ,hyperfuse-green :foreground ,hyperfuse-fg+1))))
   `(avy-lead-face-1 ((t (:background ,hyperfuse-cyan :foreground ,hyperfuse-fg+1))))
   `(avy-lead-face-2 ((t (:background ,hyperfuse-red+1 :foreground ,hyperfuse-fg+1))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,hyperfuse-fg))))
   `(egg-help-header-1 ((t (:foreground ,hyperfuse-yellow))))
   `(egg-help-header-2 ((t (:foreground ,hyperfuse-green))))
   `(egg-branch ((t (:foreground ,hyperfuse-yellow))))
   `(egg-branch-mono ((t (:foreground ,hyperfuse-yellow))))
   `(egg-term ((t (:foreground ,hyperfuse-yellow))))
   `(egg-diff-add ((t (:foreground ,hyperfuse-green))))
   `(egg-diff-del ((t (:foreground ,hyperfuse-red+1))))
   `(egg-diff-file-header ((t (:foreground ,hyperfuse-yellow-2))))
   `(egg-section-title ((t (:foreground ,hyperfuse-yellow))))
   `(egg-stash-mono ((t (:foreground ,hyperfuse-green))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,hyperfuse-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,hyperfuse-green))))
   `(elfeed-search-feed-face ((t (:foreground ,hyperfuse-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,hyperfuse-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,hyperfuse-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,hyperfuse-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,hyperfuse-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,hyperfuse-green :background ,hyperfuse-bg))))
   `(w3m-lnum-match ((t (:background ,hyperfuse-bg-1
                                     :foreground ,hyperfuse-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,hyperfuse-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,hyperfuse-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,hyperfuse-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,hyperfuse-yellow))))
   `(erc-keyword-face ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,hyperfuse-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,hyperfuse-green))))
   `(erc-pal-face ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,hyperfuse-orange :background ,hyperfuse-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,hyperfuse-green))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,hyperfuse-green :background ,hyperfuse-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,hyperfuse-red :background ,hyperfuse-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,hyperfuse-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,hyperfuse-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,hyperfuse-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,hyperfuse-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,hyperfuse-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,hyperfuse-green :weight bold))))
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
;;;;; full-ack
   `(ack-separator ((t (:foreground ,hyperfuse-fg))))
   `(ack-file ((t (:foreground ,hyperfuse-blue))))
   `(ack-line ((t (:foreground ,hyperfuse-yellow))))
   `(ack-match ((t (:foreground ,hyperfuse-orange :background ,hyperfuse-bg-1 :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,hyperfuse-green :weight bold :inverse-video nil))))
   `(git-gutter:deleted ((t (:foreground ,hyperfuse-red :weight bold :inverse-video nil))))
   `(git-gutter:modified ((t (:foreground ,hyperfuse-yellow :weight bold :inverse-video nil))))
   `(git-gutter:unchanged ((t (:foreground ,hyperfuse-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,hyperfuse-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,hyperfuse-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,hyperfuse-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground, hyperfuse-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,hyperfuse-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,hyperfuse-blue))))
   `(gnus-summary-high-read ((t (:foreground ,hyperfuse-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,hyperfuse-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,hyperfuse-blue))))
   `(gnus-summary-low-read ((t (:foreground ,hyperfuse-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,hyperfuse-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,hyperfuse-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,hyperfuse-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,hyperfuse-fg))))
   `(gnus-summary-selected ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,hyperfuse-blue))))
   `(gnus-cite-10 ((t (:foreground ,hyperfuse-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,hyperfuse-yellow))))
   `(gnus-cite-2 ((t (:foreground ,hyperfuse-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,hyperfuse-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,hyperfuse-green))))
   `(gnus-cite-5 ((t (:foreground ,hyperfuse-green))))
   `(gnus-cite-6 ((t (:foreground ,hyperfuse-green))))
   `(gnus-cite-7 ((t (:foreground ,hyperfuse-red))))
   `(gnus-cite-8 ((t (:foreground ,hyperfuse-red-1))))
   `(gnus-cite-9 ((t (:foreground ,hyperfuse-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,hyperfuse-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,hyperfuse-green))))
   `(gnus-group-news-3-empty ((t (:foreground ,hyperfuse-green))))
   `(gnus-group-news-4-empty ((t (:foreground ,hyperfuse-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,hyperfuse-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,hyperfuse-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,hyperfuse-bg+2))))
   `(gnus-signature ((t (:foreground ,hyperfuse-yellow))))
   `(gnus-x ((t (:background ,hyperfuse-fg :foreground ,hyperfuse-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,hyperfuse-blue))))
   `(guide-key/key-face ((t (:foreground ,hyperfuse-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,hyperfuse-green))))
;;;;; ivy-mode
   `(ivy-current-match ((t (:foreground ,hyperfuse-fg-1 :background ,hyperfuse-bg+3 :unfg+1ine nil))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,hyperfuse-bg+3 :weight bold :background ,hyperfuse-blue))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,hyperfuse-fg :weight bold :background ,hyperfuse-blue-1))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,hyperfuse-fg :weight bold :background ,hyperfuse-blue-2))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,hyperfuse-fg :weight bold :background ,hyperfuse-blue-3))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:foreground ,hyperfuse-bg-2 :background ,hyperfuse-bg+1)) (t :weight bold)))
   `(hl-line ((,class (:foreground ,hyperfuse-bg-2 :background ,hyperfuse-bg+1)) (t :weight bold)))
;;;;; linum-relative
   `(linum-relative-current-face ((,class (:foreground ,hyperfuse-cyan :background ,hyperfuse-bg+1)) (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,hyperfuse-bg+1)) (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,hyperfuse-yellow))))
   `(ido-indicator ((t (:foreground ,hyperfuse-yellow :background ,hyperfuse-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,hyperfuse-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,hyperfuse-green))))
   `(jabber-roster-user-online ((t (:foreground ,hyperfuse-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,hyperfuse-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,hyperfuse-green))))
   `(jabber-chat-prompt-local ((t (:foreground ,hyperfuse-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,hyperfuse-red+1))))
   `(jabber-activity-face((t (:foreground ,hyperfuse-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,hyperfuse-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
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
;;;;; linum-mode
   `(linum ((t (:foreground ,hyperfuse-bg-2 :background ,hyperfuse-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,hyperfuse-green :background ,hyperfuse-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,hyperfuse-red+1 :background ,hyperfuse-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,hyperfuse-blue+1 :background ,hyperfuse-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,hyperfuse-magenta :background ,hyperfuse-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,hyperfuse-yellow :background ,hyperfuse-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,hyperfuse-bg+2))))
   `(magit-section-title ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(magit-process-ok ((t (:foreground ,hyperfuse-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,hyperfuse-red :weight bold))))
   `(magit-branch ((t (:foreground ,hyperfuse-blue :weight bold))))
   `(magit-log-author ((t (:foreground ,hyperfuse-orange))))
   `(magit-log-sha1 ((t (:foreground, hyperfuse-orange))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,hyperfuse-green))))
   `(message-header-other ((t (:foreground ,hyperfuse-green))))
   `(message-header-to ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,hyperfuse-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,hyperfuse-green))))
   `(message-mml ((t (:foreground ,hyperfuse-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,hyperfuse-orange))))
   `(mew-face-header-from ((t (:foreground ,hyperfuse-yellow))))
   `(mew-face-header-date ((t (:foreground ,hyperfuse-green))))
   `(mew-face-header-to ((t (:foreground ,hyperfuse-red))))
   `(mew-face-header-key ((t (:foreground ,hyperfuse-green))))
   `(mew-face-header-private ((t (:foreground ,hyperfuse-green))))
   `(mew-face-header-important ((t (:foreground ,hyperfuse-blue))))
   `(mew-face-header-marginal ((t (:foreground ,hyperfuse-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,hyperfuse-red))))
   `(mew-face-header-xmew ((t (:foreground ,hyperfuse-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,hyperfuse-red))))
   `(mew-face-body-url ((t (:foreground ,hyperfuse-orange))))
   `(mew-face-body-comment ((t (:foreground ,hyperfuse-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,hyperfuse-green))))
   `(mew-face-body-cite2 ((t (:foreground ,hyperfuse-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,hyperfuse-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,hyperfuse-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,hyperfuse-red))))
   `(mew-face-mark-review ((t (:foreground ,hyperfuse-blue))))
   `(mew-face-mark-escape ((t (:foreground ,hyperfuse-green))))
   `(mew-face-mark-delete ((t (:foreground ,hyperfuse-red))))
   `(mew-face-mark-unlink ((t (:foreground ,hyperfuse-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,hyperfuse-green))))
   `(mew-face-mark-unread ((t (:foreground ,hyperfuse-red-2))))
   `(mew-face-eof-message ((t (:foreground ,hyperfuse-green))))
   `(mew-face-eof-part ((t (:foreground ,hyperfuse-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,hyperfuse-cyan :background ,hyperfuse-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,hyperfuse-bg :background ,hyperfuse-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,hyperfuse-bg :background ,hyperfuse-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,hyperfuse-blue))))
   `(mingus-pausing-face ((t (:foreground ,hyperfuse-magenta))))
   `(mingus-playing-face ((t (:foreground ,hyperfuse-cyan))))
   `(mingus-playlist-face ((t (:foreground ,hyperfuse-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,hyperfuse-yellow))))
   `(mingus-stopped-face ((t (:foreground ,hyperfuse-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,hyperfuse-yellow))))
   `(nav-face-button-num ((t (:foreground ,hyperfuse-cyan))))
   `(nav-face-dir ((t (:foreground ,hyperfuse-green))))
   `(nav-face-hdir ((t (:foreground ,hyperfuse-red))))
   `(nav-face-file ((t (:foreground ,hyperfuse-fg))))
   `(nav-face-hfile ((t (:foreground ,hyperfuse-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,hyperfuse-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,hyperfuse-green :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,hyperfuse-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,hyperfuse-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,hyperfuse-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,hyperfuse-green :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,hyperfuse-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,hyperfuse-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,hyperfuse-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,hyperfuse-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,hyperfuse-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,hyperfuse-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,hyperfuse-bg+1))))
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
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,hyperfuse-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,hyperfuse-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,hyperfuse-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,hyperfuse-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,hyperfuse-bg+3 :inherit mode-line-inactive))))
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
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,hyperfuse-blue))))
   `(rcirc-other-nick ((t (:foreground ,hyperfuse-orange))))
   `(rcirc-bright-nick ((t (:foreground ,hyperfuse-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,hyperfuse-blue-2))))
   `(rcirc-server ((t (:foreground ,hyperfuse-green))))
   `(rcirc-server-prefix ((t (:foreground ,hyperfuse-green))))
   `(rcirc-timestamp ((t (:foreground ,hyperfuse-green))))
   `(rcirc-nick-in-message ((t (:foreground ,hyperfuse-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,hyperfuse-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,hyperfuse-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,hyperfuse-green))))
   `(rpm-spec-doc-face ((t (:foreground ,hyperfuse-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,hyperfuse-red))))
   `(rpm-spec-macro-face ((t (:foreground ,hyperfuse-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,hyperfuse-red))))
   `(rpm-spec-package-face ((t (:foreground ,hyperfuse-red))))
   `(rpm-spec-section-face ((t (:foreground ,hyperfuse-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,hyperfuse-blue))))
   `(rpm-spec-var-face ((t (:foreground ,hyperfuse-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,hyperfuse-orange))))
   `(rst-level-2-face ((t (:foreground ,hyperfuse-green))))
   `(rst-level-3-face ((t (:foreground ,hyperfuse-blue-1))))
   `(rst-level-4-face ((t (:foreground ,hyperfuse-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,hyperfuse-cyan))))
   `(rst-level-6-face ((t (:foreground ,hyperfuse-green))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,hyperfuse-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,hyperfuse-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,hyperfuse-red+1 :background ,hyperfuse-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,hyperfuse-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,hyperfuse-red+1 :background ,hyperfuse-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,hyperfuse-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,hyperfuse-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,hyperfuse-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-red)))
      (t
       (:underline ,hyperfuse-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-orange)))
      (t
       (:underline ,hyperfuse-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-yellow)))
      (t
       (:underline ,hyperfuse-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hyperfuse-green)))
      (t
       (:underline ,hyperfuse-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; term
   `(term-color-black ((t (:foreground ,hyperfuse-fg
                                       :background ,hyperfuse-fg-1))))
   `(term-color-red ((t (:foreground ,hyperfuse-red-2
                                       :background ,hyperfuse-red-4))))
   `(term-color-green ((t (:foreground ,hyperfuse-green
                                       :background ,hyperfuse-green))))
   `(term-color-yellow ((t (:foreground ,hyperfuse-orange
                                       :background ,hyperfuse-yellow))))
   `(term-color-blue ((t (:foreground ,hyperfuse-blue-1
                                      :background ,hyperfuse-blue-4))))
   `(term-color-magenta ((t (:foreground ,hyperfuse-magenta
                                         :background ,hyperfuse-red))))
   `(term-color-cyan ((t (:foreground ,hyperfuse-cyan
                                       :background ,hyperfuse-blue))))
   `(term-color-white ((t (:foreground ,hyperfuse-bg
                                       :background ,hyperfuse-bg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,hyperfuse-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,hyperfuse-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,hyperfuse-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,hyperfuse-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,hyperfuse-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,hyperfuse-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,hyperfuse-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,hyperfuse-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,hyperfuse-green :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,hyperfuse-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,hyperfuse-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,hyperfuse-red-2))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,hyperfuse-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,hyperfuse-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,hyperfuse-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,hyperfuse-bg+1 :foreground ,hyperfuse-bg+1))))
   `(whitespace-hspace ((t (:background ,hyperfuse-bg+1 :foreground ,hyperfuse-bg+1))))
   `(whitespace-tab ((t (:background ,hyperfuse-red-1))))
   `(whitespace-newline ((t (:foreground ,hyperfuse-bg+1))))
   `(whitespace-trailing ((t (:background ,hyperfuse-red))))
   `(whitespace-line ((t (:background ,hyperfuse-bg :foreground ,hyperfuse-magenta))))
   `(whitespace-space-before-tab ((t (:background ,hyperfuse-orange :foreground ,hyperfuse-orange))))
   `(whitespace-indentation ((t (:background ,hyperfuse-yellow :foreground ,hyperfuse-red))))
   `(whitespace-empty ((t (:background ,hyperfuse-yellow))))
   `(whitespace-space-after-tab ((t (:background ,hyperfuse-yellow :foreground ,hyperfuse-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,hyperfuse-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,hyperfuse-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,hyperfuse-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,hyperfuse-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,hyperfuse-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,hyperfuse-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,hyperfuse-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,hyperfuse-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,hyperfuse-green))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,hyperfuse-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,hyperfuse-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,hyperfuse-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,hyperfuse-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,hyperfuse-green))))
   `(wl-highlight-message-header-contents ((t (:foreground ,hyperfuse-green))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,hyperfuse-green))))
   `(wl-highlight-message-signature ((t (:foreground ,hyperfuse-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,hyperfuse-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,hyperfuse-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,hyperfuse-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,hyperfuse-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,hyperfuse-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,hyperfuse-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,hyperfuse-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,hyperfuse-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,hyperfuse-green))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,hyperfuse-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,hyperfuse-bg-1 :foreground ,hyperfuse-bg-1))))
   )
  )

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
