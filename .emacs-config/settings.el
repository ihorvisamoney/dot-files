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
;; (add-to-list 'default-frame-alist '(undecorated . nil))

;; Remove gaps
(setq-default frame-resize-pixelwise t)

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

;; Automatically reload changed files.
(global-auto-revert-mode t)

;; Indentation defaults.
(setq-default indent-tabs-mode nil
              c-basic-indent 4
              tab-width 4)

;; Automatically resize splits.
(setq window-combination-resize t)

;; Fill column ruler.
(setq-default fill-column 80
              display-fill-column-indicator-character 124)
(global-display-fill-column-indicator-mode)

;; Grep settings.
(setq-default grep-use-null-device nil)

;; Xref
(setq-default xref-search-program 'ripgrep
              xref-truncation-width 200)

;; Ibuffer.
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; Fonts.
(setq-default line-spacing 0.35)
(set-face-attribute 'default nil :height 150)

;; Moves Emacs customization to separate file.el
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
(load custom-file 'noerror)

;; Show matching parenthesis.
(show-paren-mode 1)

;; Automatically close parenthesis.
(electric-pair-mode t)

;; Highlight current line.
(global-hl-line-mode t)

;; Enable recent f.
(setq-default recentf-max-saved-items 50
              recentf-max-menu-items 50)
(recentf-mode t)

;; I Search
(setq-default isearch-lazy-count t
              isearch-allow-motion t
              search-whitespace-regexp ".*?")

;; Registers
(setq-default register-preview-delay 0)

;; Shells (Very Important for all shell based functions and operations)
(setq-default async-shell-command-buffer "new-buffer")

;; Mac Emacs
;; Checkout: https://github.com/railwaycat/homebrew-emacsmacport
(setq-default mac-option-modifier 'meta)
(global-unset-key (kbd "C-q"))

;;;;;;;;;;;;;;;;;;;;;
;; Fake IDO (FIDO) ;;
;;;;;;;;;;;;;;;;;;;;;

(setq-default
  completion-cycle-threshold nil
  completions-format 'vertical
  completion-ignore-case t
  read-buffer-completion-ignore-case t
  read-file-name-completion-ignore-case t
  icomplete-compute-delay 0
  max-mini-window-height 0.35)

(fido-mode)
(fido-vertical-mode nil)

;;;;;;;;;;;;;;;;
;; Path Setup ;;
;;;;;;;;;;;;;;;;

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
       (ws-lighten 85) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#7f7f7f" ws-lighten)))
  (custom-set-faces
   `(fill-column-indicator ((t (:foreground ,ws-color :background nil))))
   `(whitespace-newline                ((t (:foreground ,ws-color :background "#ffffff"))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color :background "#ffffff"))))
   `(whitespace-space                  ((t (:foreground ,ws-color :background "#ffffff"))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color :background "#ffffff"))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color :background "#ffffff"))))
   `(whitespace-tab                    ((t (:foreground ,ws-color :background "#ffffff"))))
   `(whitespace-trailing               ((t (:foreground ,ws-color :background "#ffffff"))))))

;; Enable white space mode globally.
(global-whitespace-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Publishing Projects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vg-blog-publish-sitemap (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n"
          "#+SUBTITLE: Welcome to my personal blog where I cover a variety of programming related topics\n"
          "#+DESCRIPTION: Welcome to my personal blog where I cover a variety of programming related topics\n"
          "#+KEYWORDS: Emacs, Programming, Coding\n"
          "#+SETUPFILE: ./setup.org\n\n"
          (org-list-to-org list)))

(setq-default org-publish-project-alist
              (list
               ;; TODO: Implement go RSS feed generation post published.
               ;; Discovering Emacs:
               (list "vg_blog"
                     ;; Use the sitemap as the home page.
                     ;; :auto-preamble t
                     ;; :preparation-function
                     ;; :completion-function
                     :author "Vernon Grant"
                     :email "info@vernon-grant.com"
                     :base-directory "~/ProjectsP/vernon-grant/site/"
                     :base-extension "org"
                     :exclude "setup.org"
                     :publishing-directory "~/ProjectsP/vernon-grant/docs/"
                     :recursive t
                     :publishing-function 'org-html-publish-to-html
                     :headline-levels 4
                     :auto-sitemap t
                     :sitemap-title "Vernon Grant"
                     :sitemap-filename "index.org"
                     :sitemap-function 'vg-blog-publish-sitemap)
               ;; Attachments:
               (list "vg_assets"
                     :base-directory "~/ProjectsP/vernon-grant/site/"
                     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                     :publishing-directory "~/ProjectsP/vernon-grant/docs/"
                     :recursive t
                     :publishing-function 'org-publish-attachment)
               (list "vg" :components '("vg_assets" "vg_blog"))))

;;;;;;;;;;;;;
;; Removed ;;
;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Line numbers
;; (setq-default display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode 1)

;;;;;;;;;;;;;;;;;
;; Compilation ;;
;;;;;;;;;;;;;;;;;

;; (setq-default compilation-window-height 10)

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
