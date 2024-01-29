;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable bell.
(setq-default visible-bell nil)
(setq-default ring-bell-function 'ignore)

;; Remove GUI menus and scroll bars.
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Remove gaps.
(setq-default frame-resize-pixelwise t)

;; Set the frames title.
(setq-default frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; Make the title bar transparent!
(set-frame-parameter nil 'ns-appearance 'dark)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; TODO: Check to see if this makes sense.
;; Make C-k, kill the entire line (newline and all).
(setq-default kill-wholeline t)

;; Make resizing fonts not effect the window size.
(setq-default frame-inhibit-implied-resize t)

;; Enable precision scroll.
(setq-default pixel-scroll-precision-mode t)
(pixel-scroll-precision-mode)

;; No double spaceing for sentences please.
(setq sentence-end-double-space nil)

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
(make-directory "~/.emacs-saves/" t)
(make-directory "~/.emacs-autosaves/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs-autosaves/" t)))
(setq backup-directory-alist '(("." . "~/.emacs-saves/")))

;; Set the tramp mode. /ssh:Name:path.
(setq-default tramp-default-method "ssh")

;; Toggle saving of mini-buffer history.
(savehist-mode 1)

;; Automatically save bookmarks in custom file.
(setq-default bookmark-save-flag 1
              bookmark-sort-flag t)

;; Mini-buffer settings.
(setq-default resize-mini-windows t)
;; (setq-default max-mini-window-height 20) ;; Limit the mini-buffer height.

;; Flyspell.
;; We basically call Flyspell-buffer on save, check functions.el.
(setq-default flyspell-issue-message-flag nil)

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
(setq-default line-spacing 0.30)
(set-frame-font "Menlo 13" nil nil)

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
(setq-default recentf-max-saved-items 100
              recentf-max-menu-items 100)
(recentf-mode t)

;; Enable save place mode.
(save-place-mode t)

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

;; (setq-default switch-to-buffer-obey-display-actions t)
;; (setq-default custom-theme-directory "/Users/vernon/ProjectsP/volcano-theme/emacs")

;;;;;;;;;;;;;;;;
;; Path Setup ;;
;;;;;;;;;;;;;;;;

;; Push the node version manager path to the front so it takes presedence.
;; Push Scala Coursier on the our Emacs path.
(setenv "PATH" (concat
                "/Users/vernon/.nvm/versions/node/v18.16.0/bin:"
                "/Users/vernon/.composer/vendor/bin:"
                ;; "/home/vernon/.cache/coursier/arc/https/github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u292-b10/OpenJDK8U-jdk_x64_linux_hotspot_8u292b10.tar.gz/jdk8u292-b10/bin:"
                ;; "/home/vernon/.local/share/coursier/bin:"
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

;; (let ((ws-color (color-lighten-name "#ccc" 15)))
;; (let ((ws-color (color-lighten-name "#352718" 50)))
(let ((ws-color (color-lighten-name "#efe9dd" 0)))
  (custom-set-faces
   `(fill-column-indicator             ((t (:foreground ,ws-color :background nil))))
   `(whitespace-indentation            ((t (:background nil))))
   `(whitespace-newline                ((t (:foreground ,ws-color :background nil))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color :background nil))))
   `(whitespace-space                  ((t (:foreground ,ws-color :background nil))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color :background nil))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color :background nil))))
   `(whitespace-tab                    ((t (:foreground ,ws-color :background nil))))
   `(whitespace-trailing               ((t (:foreground ,ws-color :background nil))))))

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
