;;;;;;;;;;;
;; Melpa ;;
;;;;;;;;;;;

(require 'package)
(package-initialize)

;; add Melpa packages.
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(eval-when-compile
  ;; Automatically install use-package if needed.
  (unless (require 'use-package nil 'noerror) (package-install 'use-package))
  (require 'use-package))

;; (use-package flymake-easy  :ensure t)
(use-package markdown-mode   :ensure t)
(use-package go-mode         :ensure t)
(use-package php-mode        :ensure t)
(use-package json-mode       :ensure t)
(use-package composer        :ensure t)
(use-package clojure-mode    :ensure t)
(use-package yaml-mode       :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package typescript-mode :ensure t)

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.liquid" . web-mode)))

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
  :ensure t)

(use-package helm-c-yasnippet
  :ensure t
  :after 'helm)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package zenburn-theme
;;   :ensure t
;;   :init
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package doom
;;   :ensure t
;;   :init
;;   :config
;;   (editorconfig-mode 1))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package helm
  ;; TAB: Shows the actions.
  :ensure t
  :init
  (helm-mode 1)
  :config
  (setq-default helm-split-window-in-side-p t   ; open helm buffer inside current window, not occupy whole other window
      helm-ff-search-library-in-sexp        t   ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8   ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t)

  ;; Enable helm mode.
  (helm-mode 1)

  ;; Helm resize restrictions.
  (setq-default helm-autoresize-max-height 50
                helm-autoresize-min-height 50)
    (helm-autoresize-mode t)

  ;; Basically bind everything.
  :bind
  (("M-x"     . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-q g"   . helm-do-grep-ag)
   ("C-q r"   . helm-recentf)
   ("C-q s"   . yas-insert-snippet)
   ("C-q e"   . emmet-expand-line)
   ("C-q p"   . project-switch-project)
   ("C-q f"   . project-find-file)
   ("C-q b"   . project-switch-to-buffer)
   ("C-q d"   . project-dired)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind
  (
   ("M-p" . flycheck-previous-error)
   ("<f5>" . flycheck-previous-error)
   ("M-n" . flycheck-next-error)
   ("<f6>" . flycheck-next-error)
   ("<f7>" . flycheck-list-errors)))

(use-package which-key
  :ensure t
  :config
  (setq-default which-key-idle-delay 1.0)
  (which-key-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq-default lsp-keymap-prefix "C-c l")
  (setq-default gc-cons-threshold 100000000)
  (setq-default read-process-output-max (* 1024 1024))
  :config
  (setq-default
   lsp-enable-on-type-formatting nil
   lsp-auto-guess-root t
   lsp-enable-snippet t
   lsp-keep-workspace-alive nil
   lsp-headerline-breadcrumb-enable nil)

  ;; Adds additional checkers along side lsp.
  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (when (derived-mode-p 'php-mode)
                (flycheck-add-next-checker 'lsp 'php-phpcs 'php))
              (when (derived-mode-p 'css-mode)
                (flycheck-add-next-checker 'lsp 'css-stylelint))
              (when (derived-mode-p 'js-mode)
                (flycheck-add-next-checker 'lsp 'javascript-eslint))
              (when (derived-mode-p 'web-mode)
                (flycheck-add-next-checker 'lsp 'php-phpcs 'javascript-eslint 'css-stylelint))
              ))
  :commands (lsp lsp-deferred)
  ;; Manually enable the language server when needed.
  ;; :bind
  ;; ("C-c l l" . lsp-mode)
  :hook ((js-mode         . lsp)
         (php-mode        . lsp)
         (web-mode        . lsp)
         (css-mode        . lsp)
         (json-mode       . lsp)
         (typescript-mode . lsp)
         (xml-mode        . lsp)
         (dockerfile-mode . lsp)
         (html-mode       . lsp)
         (yaml-mode       . lsp)
         (sh-mode         . lsp)
         ;; (markdown-mode   . lsp)
         (lsp-mode        . lsp-enable-which-key-integration)))

;; Sample:
;; (define-derived-mode shopify-mode web-mode "Shopify"
;;   "Major mode derived from `web-mode'.")
;; ;; Use shopify-cli / theme-check-language-server for Shopify's liquid syntax
;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration
;;     '(shopify-mode . "shopify"))

;;   (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection "theme-check-language-server")
;;                      :activation-fn (lsp-activate-on "shopify")
;;                      :server-id 'theme-check)))

;; Blade:
(define-derived-mode blade-mode web-mode "Blade"
  "Major mode derived from `web-mode' for blade templates.")


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  ;; Docs
  (setq-default
   lsp-ui-doc-enable t
   lsp-ui-doc-header t
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-delay 1.25
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-alignment 'frame)
  ;; Sideline
  (setq-default
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-delay 0.1
   lsp-ui-sideline-show-code-actions t)
  ;; TODO: Implement this.
  ;; lsp-ui-doc-focus-frame
  ;; lsp-ui-doc-unfocus-frame
  )

(use-package company
  :ensure t
  :bind (:map company-active-map
              ;; ("<tab>" . company-select-next)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq-default
   company-selection-wrap-around t
   company-minimum-prefix-length 2
   company-idle-delay 0.3)
  (global-company-mode t))

;; Setup of linters.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; Controls how the errors buffer is displayed.
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.2)))

  ;; Disable jshint since we prefer eslint checking
  ;; (setq-default flycheck-disabled-checkers
  ;;               (append flycheck-disabled-checkers '(javascript-jshint)))
  (flycheck-add-mode 'php-phpcs 'php-mode)
  (flycheck-add-mode 'php-phpcs 'web-mode)
  (setq-default flycheck-checker-error-threshold 400))
