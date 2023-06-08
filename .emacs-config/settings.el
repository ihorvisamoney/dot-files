;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable bell.
(setq-default visible-bell nil)
(setq-default ring-bell-function 'ignore)

;; Remove GUI menus and scroll bars.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Encoding.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Tab width.
(setq-default tab-width 4)

;; Disable line wrapping.
(set-default 'truncate-lines -1)

;; Show column and line numbers
(setq-default column-number-mode t)

;; Show clock in mode line.
(display-time)

;; Mark ring settings.
(setq-default set-mark-command-repeat-pop t)
(setq-default global-mark-ring-max 8)
(setq-default mark-ring-max 6)

;; Enable delete region selection deletion.
(delete-selection-mode 1)

;; Save backup files to specific folder.
(setq-default backup-directory-alist `(("." . "~/.emacs-saves")))

;; Set the tramp mode. /ssh:Name:path.
(setq-default tramp-default-method "ssh")

;; Toggle saving of minibuffer history.
(savehist-mode 1)

;; Automatically save bookmarks in custom file.
(setq-default bookmark-save-flag 1)

;; Minibuffer settings.
(setq-default resize-mini-windows t)
;; (setq-default max-mini-window-height 20) ;; Limit the minibuffer height.

;; Flyspell.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Move deleted files to trash.
(setq-default delete-by-moving-to-trash t)

;; Automatically load .dir-locals.
(defun safe-local-variable-p (sym val)
  "Automatically load .dir-locals.
SYM:
VAL:"
  t)

;; Prevent mouse click pasting clipboard into Emacs.
(setq-default x-select-enable-primary nil)

;; Org mode
(setq-default org-agenda-window-setup 'current-window)

;; Automatically reload changed files.
(global-auto-revert-mode t)

;; Indentation defaults.
(setq-default indent-tabs-mode nil)
(setq-default c-basic-indent 4)
(setq-default tab-width 4)

;; Automatically resize splits.
(setq window-combination-resize t)

;; Fill column ruler.
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-character 124)
(global-display-fill-column-indicator-mode)

;; Grep settings.
(setq-default grep-use-null-device nil)

;; Ibuffer.
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; Use Linux based system font.
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "Monospace" :height 94))

;; Use OSX based system font.
;; (set-face-attribute 'default nil :family "Menlo" :height 148 :weight 'normal)

;; Moves Emacs customization to separate file.el
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
(load custom-file 'noerror)

;; Line numbers
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Show matching parenthesis.
(show-paren-mode 1)

;; Automatically close parenthesis.
(electric-pair-mode t)

;; Highlight current line.
(global-hl-line-mode t)

;; Enable recent f.
(setq-default recentf-max-saved-items 50)
(setq-default recentf-max-menu-items 50)
(recentf-mode t)

;; I Search
(setq-default isearch-lazy-count t)
(setq-default isearch-allow-motion t)

;; Project.el
(setq-default project-switch-commands 'project-find-file)

;; Registers
(setq-default register-preview-delay 0)

;; Mac Emacs
;; Checkout: https://github.com/railwaycat/homebrew-emacsmacport
(setq-default mac-option-modifier 'meta)

(global-unset-key (kbd "C-q"))

;; Theme
(load-theme 'modus-operandi t)

;;;;;;;;;;;;;;;;
;; Path Setup ;;
;;;;;;;;;;;;;;;;

;; We have node and npm via nvm.
;; We have node and npm from dfn.
;; LSP uses the node version from dfn, so we should install our checkers and stuff globally

;; Node
;; /home/vernon/.nvm/versions/node/v18.16.0/bin
(setq exec-path (append exec-path '("/home/vernon/.nvm/versions/node/v18.16.0/bin")))
;; (setenv "PATH" (concat (getenv "PATH") ";/home/vernon/.nvm/versions/node/v18.16.0/bin"))

;;;;;;;;;;;;;;;;;
;; Compilation ;;
;;;;;;;;;;;;;;;;;

;; (setq-default compilation-window-height 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Display Options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq-default display-buffer-alist
;;               '(("\\*compilation\\*" (display-buffer-at-bottom)
;;                  (inhibit-same-window . t)
;;                  (window-height . fit-window-to-buffer))
;;                 ("\\*eldoc\\*"
;;                  (display-buffer-at-bottom)
;;                  (inhibit-same-window . t)
;;                  (window-height . fit-window-to-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Display Options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq-default display-buffer-alist
;;               '(("\\*compilation\\*" (display-buffer-at-bottom)
;;                  (inhibit-same-window . t)
;;                  (window-height . fit-window-to-buffer))
;;                 ("\\*eldoc\\*"
;;                  (display-buffer-at-bottom)
;;                  (inhibit-same-window . t)
;;                  (window-height . fit-window-to-buffer))))

;;;;;;;;;;;;;;;
;; Themeing  ;;
;;;;;;;;;;;;;;;

;; (load-theme 'gruvbox t)

;;;;;;;;;;;;;;;;;;;;;
;; Whitespace Mode ;;
;;;;;;;;;;;;;;;;;;;;;

;; Specify white space column.
(setq-default whitespace-line-column 80)

;; Define the whitespace style.
(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))

;; Make these characters represent white space.
(setq-default whitespace-display-mappings
      '(;; space -> · else .
        (space-mark 32 [183] [46])
        ;; new line -> ¬ else $
        (newline-mark ?\n [172 ?\n] [36 ?\n])
        ;; carriage return (Windows) -> ¶ else #
        (newline-mark ?\r [182] [35])
        ;; tabs -> » else >
        (tab-mark ?\t [187 ?\t] [62 ?\t])))

;; Don't enable white space for.
(setq-default whitespace-global-modes
              '(not shell-mode
                    help-mode
                    magit-mode
                    magit-diff-mode
                    ibuffer-mode
                    dired-mode
                    occur-mode))

;; Set white space actions.
(setq-default whitespace-action
              '(cleanup auto-cleanup))

;; Whitespace color changes for gruvbox.
(require 'color)
(let* ((ws-lighten 80) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#000000" ws-lighten)))
  (custom-set-faces
   `(whitespace-newline                ((t (:foreground ,ws-color :background nil))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color :background nil))))
   `(whitespace-space                  ((t (:foreground ,ws-color :background nil))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color :background nil))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color :background nil))))
   `(whitespace-tab                    ((t (:foreground ,ws-color :background nil))))
   `(whitespace-trailing               ((t (:foreground ,ws-color :background nil))))))

;; Enable white space mode globally.
(global-whitespace-mode t)

;;;;;;;;;;;;;;;
;; GDB Setup ;;
;;;;;;;;;;;;;;;

;; ;; GDB setup, close GDB by typing C-x 4 0 (kill-buffer-and-window).
;; (setq gdb-many-windows t)

;; ;; Start GDB debugging session.
;; (define-key global-map (kbd "C-c cc") 'gdb)

;; (defun vg-gdb-set-local-bindings()
;;   "Set key bindings specifically for gdb-mode.
;; Helps exit GDB and all its windows."
;;   (interactive)
;;       (local-set-key (kbd "C-c c c") 'kill-buffer-and-window))
;; (add-hook 'gdb-mode-hook 'vg-gdb-set-local-bindings)

;;;;;;;;;;;
;; Eldoc ;;
;;;;;;;;;;;

;; (add-to-list 'display-buffer-alist
;;              '("^\\*Open Recent\\*" display-buffer-reuse-window
;;                                      (inhibit-same-window . t)))

;; (add-to-list 'display-buffer-alist
;;              '(("\\*eldoc\\*"
;;                 (display-buffer-at-bottom)
;;                 ;; (display-buffer-below-selected display-buffer-at-bottom)
;;                 (inhibit-same-window . t)
;;                 (window-height . fit-window-to-buffer))))

;; (add-hook 'go-mode-hook 'eldoc-mode)
;; (add-hook 'c-mode-hook 'eldoc-mode)
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
