;;;;;;;;;;;;;;;;;;;;;;
;; Package Archives ;;
;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; add Melpa packages.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

;; TODO: Implement C-x C-F for finding files the normal way (required to create a file, maybe just manually call find file.)
;; TODO: Implement function that will grep the word under the cursor or the region. (C-x g)

(eval-when-compile
  ;; Automatically install use-package if needed.
  (unless (require 'use-package nil 'noerror) (package-install 'use-package))
  (require 'use-package))

(use-package markdown-mode   :ensure t)
(use-package php-mode        :ensure t)
(use-package json-mode       :ensure t)
(use-package composer        :ensure t)
(use-package yaml-mode       :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package typescript-mode :ensure t)
(use-package apache-mode     :ensure t)
(use-package nginx-mode      :ensure t)
(use-package toml-mode       :ensure t)
(use-package haskell-mode    :ensure t)
(use-package glsl-mode       :ensure t)
(use-package dotenv-mode     :ensure t)
(use-package wrap-region     :ensure t)
(use-package wgrep           :ensure t)
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.liquid" . web-mode))
  :config
  (setq-default web-mode-enable-auto-indentation nil))

(use-package spacious-padding
  :ensure t
  :init
  (spacious-padding-mode 1)
  :config
  (setq-default spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 4
         :tab-width 4
         :right-divider-width 15
         :scroll-bar-width 8)))

;; Terminal emulator.
(use-package vterm :ensure t)

;; Manage notes like a boss.
(use-package denote
  :ensure t
  :config
  (setq-default denote-directory "/Users/vernon/Notes/denote/"))

;; Bring back splits if needed.
(use-package winner
  :ensure t
  :init
  (winner-mode t))

(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-operandi-tinted t))

;; Keeps state across sessions.
;; (use-package desktop
;;   :ensure t
;;   :config
;;   (desktop-save-mode 1))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode t)
  :config
  (setq-default git-gutter:window-width 1))

;; Better minibuffer completions.
(use-package vertico
  :ensure t
  :config
  (setq-default
   vertico-resize nil
   vertico-cycle t
   vertico-count 12
   vertico-scroll-margin 2
   read-buffer-completion-ignore-case t
   read-file-name-completion-ignore-case t
   completion-ignore-case t
   completion-styles #'(flex))
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; Setup emacs configurations here.
;; (use-package emacs
;;   :ensure t)

(use-package project
  :ensure t
  :config
  (setq-default project-switch-commands 'project-find-file))

;; Shows buffers by project.
(use-package ibuffer-project
  :ensure t
  :config
  (add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
            (unless (eq ibuffer-sorting-mode 'project-file-relative)
              (ibuffer-do-sort-by-project-file-relative)))))

;; To execute code in org mode.
(use-package ob-php :ensure t)
(use-package ob-deno :ensure t)
(use-package ob-restclient :ensure t)
(use-package ob-typescript :ensure t)
(use-package org
  :ensure t
  :config
  (setq-default
   org-agenda-window-setup 'current-window
   org-highlight-latex-and-related '(latex script entities)
   org-html-validation-link nil
   org-publish-use-timestamps-flag nil
   org-src-fontify-natively t
   org-export-with-timestamps nil
   org-confirm-babel-evaluate nil
   org-html-postamble t
   org-html-postamble-format '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"updated\">Updated on: %C</p>
<p class=\"creator\">%c</p>
<p class=\"validation\">%v</p>")))
  ;; Enable babel languages:
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((deno . t)
     (php . t)
     (typescript . t))))

(use-package restclient
  :ensure t
  :config
  (setq-default restclient-log-request t))

;; Configure Elfeed
(use-package elfeed
  :ensure t
  :config
  (setq-default
   elfeed-show-entry-switch 'display-buffer
   elfeed-db-directory "~/.elfeed"
   elfeed-feeds '(
                  ;; Other blogs:
                  "https://dnaeon.github.io/feed.xml"
                  "https://coredumped.dev/index.xm"
                  "https://zenhabits.net/feed/"
                  "http://planet.sbcl.org/rss20.xml"
                  "http://planet.lisp.org/rss20.xml"
                  "https://borretti.me/feed"
                  "https://lisp-journey.gitlab.io/blog/index.xml"
                  "https://matt-rickard.com/feed"
                  "https://ferd.ca/feed.rss"
                  "https://joy.recurse.com/feed.atom"
                  "http://feeds.feedburner.com/freetechbooks"
                  "http://lambda-the-ultimate.org/rss.xml"
                  "http://nullprogram.com/feed/"
                  "https://blog.cleancoder.com/atom.xml"
                  "https://eli.thegreenplace.net/feeds/all.atom.xml"
                  "https://feeds.feedburner.com/TheDailyWtf"
                  "https://henrikwarne.com/feed/"
                  "https://irreal.org/blog/?feed=rss2"
                  "https://okmij.org/ftp/rss.xml"
                  "https://protesilaos.com/codelog.xml"
                  "https://sachachua.com/blog/feed/"
                  "https://www.johndcook.com/blog/feed/"
                  "https://www.masteringemacs.org/feed"
                  "https://writepermission.com/rss.xml"
                  "https://go.dev/blog/feed.atom")))

(use-package editorconfig
  :ensure t
  :init
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
        '("~/.emacs-config/snippets"))
  (yas-global-mode 1))

(use-package emmet-mode
  :ensure t
  :config
  (setq-default emmet-indent-after-insert nil))

(use-package magit :ensure t)

(use-package which-key
  :ensure t
  :config
  (setq-default which-key-idle-delay 1.0)
  (which-key-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; Blade:
(define-derived-mode blade-mode web-mode "Blade"
  "Major mode derived from `web-mode' for blade templates.")

(use-package company
  :ensure t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq-default
   company-echo-truncate-lines t
   company-format-margin-function #'company-text-icons-margin
   company-icon-margin 5
   company-idle-delay .2
   company-minimum-prefix-length 2
   company-search-regexp-function #'company-search-words-regexp
   company-selection-wrap-around nil
   company-show-quick-access nil
   company-text-icons-add-background t
   company-text-icons-format "  %s  "
   company-tooltip-margin 5
   company-tooltip-minimum-width 30)
  :hook (prog-mode . global-company-mode))

;; Setup of linters.
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package dired-sidebar
  :ensure t
  :config
  (setq-default dired-sidebar-theme 'ascii
                dired-sidebar-should-follow-file t))

(use-package eglot
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  :bind (:map eglot-mode-map
              ("s-l a" . eglot-code-actions)
              ("s-l r" . eglot-rename)
              ("s-l o" . eglot-code-action-organize-imports)
              ("s-l f" . eglot-format)
              ("s-l d" . eldoc))
  ;; Installing language servers:
  ;; Markdown   : brew install marksman
  ;; Typescript : npm i -g typescript typescript-language-server
  ;; JSON       : npm i -g vscode-json-languageservice
  ;; YAML       : npm i -g yaml-language-server
  ;; HTML       : npm i -g vscode-html-languageservice
  ;; PHP        : npm i -g intelephense
  :hook
  (yaml-mode . eglot-ensure)
  (json-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (markdown-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (js2-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (web-mode . eglot-ensure)
  (php-mode . eglot-ensure)
  (html-mode . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.2)
  (add-to-list 'eglot-server-programs '((php-mode :language-id "php") . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs '((web-mode :language-id "php") . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs '(html-mode . ("vscode-html-language-server" "--stdio"))))

(use-package eldoc-box
  :ensure t
  :init
  ;; (eldoc-box-hover-mode t)
  (eldoc-box-hover-at-point-mode t)
  :config
  (setq-default eldoc-box-max-pixel-height 350
                eldoc-box-max-pixel-width 350)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

;;;;;;;;;;;;;;;;;
;; Intrigue.el ;;
;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/Users/vernon/ProjectsP/intrigue.el/")
(require 'intrigue)
(setq-default intrigue-file-location "~/Dotfiles/.emacs-intrigue.el")
