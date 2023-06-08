;;; fore ver-theme.el --- think theme
;; Copyright (C) 2022 by Vernon Grant

;; Author: Vernon Grant <vernon@ruppell.io>
;; Version: 1.0.0
;; Keywords: theme, light, red
;; Homepage: https://github.com/VernonGrant/dotfiles

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
;;
;; Forever theme for Emacs, used by Vernon Grant.

;;; Code:

;; (require 'color)

(deftheme forever "A semi light theme.")

(let* (
      (fg              "#282828")
      (bg              "#eee9e9")
      (bg-darker       "#d3d3d3")
      (bg-lighter      "#fffafa")
      (whitespace      "#d9d9d9")
      (region          bg-darker)
      (cursor          "#cd1076")
      (attention       "#cd1076")
      (keyword         "#00008b")
      (constant        "#006400")
      (comment         "#838b83")
      (string          "#8b0000")
      (warning         "#D7875F"))

  (custom-theme-set-faces
   'forever

   ;; General
   `(default                              ((t (:background ,bg :foreground ,fg))))
   `(link                                 ((t (:foreground ,keyword :underline t))))
   `(region                               ((t (:background ,region))))
   `(match                                ((t (:background ,region))))
   `(cursor                               ((t (:background ,cursor))))
   `(success                              ((t (:foreground ,keyword))))
   `(hl-line                              ((t (:background ,bg-lighter))))
   `(font-lock-builtin-face               ((t (:foreground ,fg :bold t))))
   `(font-lock-comment-face               ((t (:foreground ,comment))))
   `(font-lock-doc-string-face            ((t (:foreground ,comment))))
   `(font-lock-doc-face                   ((t (:foreground ,comment))))
   `(font-lock-constant-face              ((t (:foreground ,constant :bold t))))
   `(font-lock-function-name-face         ((t (:foreground ,fg))))
   `(font-lock-keyword-face               ((t (:foreground ,keyword :bold t))))
   `(font-lock-reference-face             ((t (:foreground ,fg))))
   `(font-lock-string-face                ((t (:foreground ,string))))
   `(font-lock-type-face                  ((t (:foreground ,keyword))))
   `(font-lock-variable-name-face         ((t (:foreground ,fg))))
   `(font-lock-warning-face               ((t (:background ,region :foreground ,warning))))
   `(font-lock-preprocessor-face          ((t (:foreground ,keyword))))

   ;; Line numbers
   `(line-number                          ((t (:foreground ,comment :background ,bg-darker))))
   `(line-number-current-line             ((t (:foreground ,keyword :background ,bg-lighter :bold t))))

   ;; Whitespace
   `(whitespace                           ((t (:foreground ,whitespace))))
   `(whitespace-empty                     ((t (:foreground ,whitespace))))
   `(whitespace-indentation               ((t (:background ,bg))))
   `(whitespace-newline                   ((t (:foreground ,whitespace))))
   `(whitespace-space                     ((t (:foreground ,whitespace))))
   `(whitespace-tab                       ((t (:foreground ,whitespace))))
   `(whitespace-trailing                  ((t (:foreground ,warning))))

   ;; Help
   `(help-key-binding                     ((t (:foreground ,string))))

   ;; Flyspell
   `(flyspell-incorrect                   ((t (:underline ,t))))
   `(flyspell-duplicate                   ((t (:underline ,t))))

   ;; Mode Line
   `(mode-line                            ((t (:background ,bg-lighter))))
   `(mode-line-inactive                   ((t (:background ,bg-darker))))
   `(mode-line-buffer-id                  ((t (:foreground ,bg :background ,keyword :bold t))))

   ;; Column Indicator
   `(fill-column-indicator                ((t (:foreground ,fg))))

   ;; Fringes
   `(fringe                               ((t (:foreground ,fg :background ,bg-darker))))
   `(vertical-border                      ((t (:foreground ,fg :background ,bg-darker))))

   ;; Minibuffer
   `(minibuffer-prompt                    ((t (:foreground ,keyword :bold t))))

   ;; Matching Parenthesis
   `(show-paren-match                     ((t (:foreground ,attention :background ,region :bold t))))
   `(show-paren-match-face                ((t (:foreground ,keyword :background ,region :bold t))))

   ;; I-Search, Highlighting
   `(isearch                              ((t (:background ,region :bold t))))
   `(isearch-fail                         ((t (:foreground ,warning :background ,region :bold t))))
   `(lazy-highlight                       ((t (:foreground ,fg :background ,region :bold t))))
   `(highlight                            ((t (:foreground ,fg :background ,region))))

   ;; Completions, shows available completions
   `(completions-common-part              ((t (:foreground ,keyword :bold t))))
   `(completions-annotations              ((t (:foreground ,comment))))
   `(completions-first-difference         ((t (:bold t))))

   ;; Dired
   `(dired-directory                      ((t (:foreground ,keyword :bold t))))
   `(dired-ignored                        ((t (:foreground ,fg))))

   ;; Company
   `(company-tooltip                      ((t (:foreground ,fg :background ,bg ))))
   `(company-tooltip-selection            ((t (:background ,bg))))
   `(company-tooltip-common               ((t (:foreground ,keyword :bold t ))))
   `(company-tooltip-common-selection     ((t (:foreground ,keyword :background ,bg :bold t ))))
   `(company-scrollbar-bg                 ((t (:background ,bg))))
   `(company-scrollbar-fg                 ((t (:background ,fg))))
   `(company-tooltip-annotation           ((t (:foreground ,comment))))
   `(company-tooltip-annotation-selection ((t (:foreground ,comment))))

   ;; Org
   `(org-headline-done                    ((t (:foreground ,comment))))

   ;; Ido
   `(ido-subdir                           ((t (:foreground ,fg :bold t))))
   `(ido-first-match                      ((t (:foreground ,keyword))))
   `(ido-only-match                       ((t (:foreground ,keyword))))
   `(ido-indicator                        ((t (:foreground ,constant))))
   `(ido-incomplete-regexp                ((t (:foreground ,warning))))

   ;; GNU Make
   `(makefile-targets                     ((t (:foreground ,keyword))))

   ;; Flycheck
   `(flycheck-info                        ((t (:underline ,t))))
   `(flycheck-warning                     ((t (:underline ,t))))
   `(flycheck-error                       ((t (:underline ,t))))

   ;; Magit
   `(magit-section-highlight              ((t (:background ,region))))
   `(magit-section                        ((t (:background ,nil :foreground ,fg))))
   `(magit-diff-hunk-heading-highlight    ((t (:background ,region :foreground ,fg))))
   `(magit-diff-hunk-heading              ((t (:background ,nil :foreground ,fg))))
   `(magit-diff-context-highlight         ((t (:background ,region :foreground ,fg))))
   `(magit-diff-context                   ((t (:background ,nil :foreground ,fg))))

   ;; Web Mode
   `(web-mode-html-tag-bracket-face       ((t (:foreground ,fg))))
   `(web-mode-html-tag-face               ((t (:foreground ,fg))))
   `(web-mode-html-attr-name-face         ((t (:foreground ,keyword))))
   )
  )

;;;###autoload


(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
(provide-theme 'forever)

;;; vg-theme.el ends here
