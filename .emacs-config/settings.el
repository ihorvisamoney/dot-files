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

;; Remove gaps
(setq-default frame-resize-pixelwise t)

;; Fringe mode

;; Remove title bar
;; (add-to-list 'default-frame-alist '(undecorated-round . t))

;; Enable precision scroll.
(setq-default pixel-scroll-precision-mode t)
(pixel-scroll-precision-mode)

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

;; Stop creating backup files.
(setq-default make-backup-files nil)

;; When backup files enabled, save them to a specific folder.
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
(setq-default org-agenda-window-setup 'other-window)
(setq-default org-highlight-latex-and-related '(latex script entities))
(setq-default org-src-fontify-natively t)

;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq-default org-latex-listings 'minted)

;; (setq-default org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

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

;; Fonts.
(setq-default line-spacing 0.35)
(set-face-attribute 'default nil :height 150)

;; (when (eq system-type 'gnu/linux)
;;     (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 104))
;; (when (eq system-type 'darwin)
;;     (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font Mono" :height 140))

;; Moves Emacs customization to separate file.el
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
(load custom-file 'noerror)

;; Show matching parenthesis.
(show-paren-mode 1)

;; Automatically close parenthesis.
(electric-pair-mode t)

;; Line numbers
;; (setq-default display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode 1)

;; Highlight current line.
;; (global-hl-line-mode t)

;; Enable recent f.
(setq-default recentf-max-saved-items 50)
(setq-default recentf-max-menu-items 50)
(recentf-mode t)

;; I Search
(setq-default isearch-lazy-count t)
(setq-default isearch-allow-motion t)
(setq-default search-whitespace-regexp ".*?")

;; Project.el
(setq-default project-switch-commands 'project-find-file)

;; Registers
(setq-default register-preview-delay 0)

;; Shells (Very Important for all shell based functions and operations)
(setq-default async-shell-command-buffer "new-buffer")

;; Mac Emacs
;; Checkout: https://github.com/railwaycat/homebrew-emacsmacport
(setq-default mac-option-modifier 'meta)
(global-unset-key (kbd "C-q"))

;;;;;;;;;;;;;;
;; Fake IDO ;;
;;;;;;;;;;;;;;

(setq-default completion-styles '(initials partial-completion flex)) ; > Emacs 27.1
(setq-default completion-cycle-threshold 10)
(setq-default completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default icomplete-compute-delay 0)
(setq-default icomplete-max-delay-chars 0)
(setq-default max-mini-window-height 0.35)
(fido-mode)
(fido-vertical-mode)

;;;;;;;;;;;;;;;;
;; Path Setup ;;
;;;;;;;;;;;;;;;;

;; We have node and npm via nvm.
;; We have node and npm from dfn.
;; LSP uses the node version from dfn, so we should install our checkers and stuff globally

;; Node
;; /home/vernon/.nvm/versions/node/v18.16.0/bin
;; (setq exec-path (append exec-path '("~/.nvm/versions/node/v18.16.0/bin")))

;; Push the node version manager path to the front so it takes presedence.
;; Push Scala Coursier on the our Emacs path.
(setenv "PATH" (concat
                ;; "/home/vernon/.cache/coursier/arc/https/github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u292-b10/OpenJDK8U-jdk_x64_linux_hotspot_8u292b10.tar.gz/jdk8u292-b10/bin:"
                ;; "/home/vernon/.local/share/coursier/bin:"
                "/Users/vernon/.nvm/versions/node/v18.16.0/bin:"
                (getenv "PATH")))

(setq exec-path (push
                 "/Users/vernon/.nvm/versions/node/v18.16.0/bin"
                 ;; (concat
                 ;;  ;; "/home/vernon/.cache/coursier/arc/https/github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u292-b10/OpenJDK8U-jdk_x64_linux_hotspot_8u292b10.tar.gz/jdk8u292-b10/bin:"
                 ;;  ;; "/home/vernon/.local/share/coursier/bin:"
                 ;;  "/home/vernon/.nvm/versions/node/v18.16.0/bin")
                 exec-path))

;; typescript-language-server --stdio

;; (getenv "PATH")
;; We can also append other paths, if needed.
;; (setq exec-path (append exec-path '("/home/vernon/.nvm/versions/node/v18.16.0/bin")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clipboard corrections inside tmux and wayland ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'gnu/linux)
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))

  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))

  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

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

;; Whitespace color changes.
(require 'color)
(let* (
       (ws-lighten 0) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#DDD" ws-lighten))
       ;; (ws-lighten 128) ;; Amount in percentage to lighten up black.
       ;; (ws-color (color-lighten-name "#a45a22" ws-lighten))
       )
  (custom-set-faces
   `(fill-column-indicator ((t (:foreground ,ws-color :background nil))))
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

;;;;;;;;;;;
;; Eglot ;;
;;;;;;;;;;;

;; TODO: Setup PHP LSP.
;; ;; npm install -g intelephense
;; (add-hook 'php-mode-hook 'eglot-ensure)

;; npm install -g vscode-html-language-server
(add-hook 'html-mode-hook 'eglot-ensure)

;; npm install -g yaml-language-server
(add-hook 'yaml-mode-hook 'eglot-ensure)

;; npm install -g vscode-json-languageservice
(add-hook 'json-mode-hook 'eglot-ensure)

;; npm install -g typescript typescript-language-server
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

;; brew install marksman
(add-hook 'markdown-mode-hook 'eglot-ensure)

;;; settings.el ends here
