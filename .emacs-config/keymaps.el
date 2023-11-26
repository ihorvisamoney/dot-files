;;; package --- Keybindings
;;
;;; Code:
;;
;;; Commentary:

;; Unfill paragraphs
(define-key global-map (kbd "M-Q") 'vg-unfill-paragraph)

;; Dired jump other window.
(define-key global-map (kbd "C-x J") (lambda ()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'dired-jump)))

;; Smarter default Emacs bindings:
(define-key global-map (kbd "C-x f") 'find-file)
(define-key global-map (kbd "C-z") 'vg-quick-zap-up-to-char)

(define-key global-map (kbd "C-x D") 'dired-other-window)
(define-key global-map (kbd "C-x B") 'switch-to-buffer-other-window)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-x 4 C-b") 'ibuffer-other-window)

;; Save current buffer:
(define-key global-map (kbd "<C-return>") 'save-buffer)
(eval-after-load 'tex-mode '(define-key latex-mode-map (kbd "<C-return>") 'save-buffer))

;; Delete characters and words:
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-h") 'vg-kill-word-negative)

;; Change window backward.
(define-key global-map (kbd "C-x O") 'vg-other-window-negative)

;; Duplicate things:
(define-key global-map (kbd "C-c d") 'vg-duplicate-current-line-or-region)

;; Function keys:
(define-key global-map (kbd "<f8>") 'elfeed)
(define-key global-map (kbd "<f9>") 'vg-toggle-transparency)
(define-key global-map (kbd "<f12>") 'vg-presentation-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Prefix Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add a prefix to copy important things to the clipboard:
;; - [x] Current line number and file path relative to the project.
;; - [ ] Current date.

;; Add our own prefix key
(define-prefix-command 'vg-personal-map)
(global-set-key (kbd "C-q") 'vg-personal-map)
(define-key vg-personal-map (kbd "r") 'recentf)
(define-key vg-personal-map (kbd "R") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'recentf)))

;; Project:
(define-prefix-command 'vg-project-map)
(define-key vg-personal-map (kbd "p") 'vg-project-map)
(define-key vg-project-map (kbd "!") 'project-shell-command)
(define-key vg-project-map (kbd "&") 'project-async-shell-command)
(define-key vg-project-map (kbd "k") 'project-kill-buffers)

(define-key vg-project-map (kbd "f") 'project-find-file)
(define-key vg-project-map (kbd "F") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-find-file)))

(define-key vg-project-map (kbd "d") 'project-find-dir)
(define-key vg-project-map (kbd "D") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-find-dir)))

(define-key vg-project-map (kbd "h") 'project-dired)
(define-key vg-project-map (kbd "H") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-dired)))

(define-key vg-project-map (kbd "e") 'project-eshell)
(define-key vg-project-map (kbd "E") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-eshell)))

(define-key vg-project-map (kbd "s") 'project-shell)
(define-key vg-project-map (kbd "S") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-shell)))

(define-key vg-project-map (kbd "g") 'project-find-regexp)
(define-key vg-project-map (kbd "G") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-find-regexp)))


(define-key vg-project-map (kbd "r") 'project-query-replace-regexp)
(define-key vg-project-map (kbd "R") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-query-replace-regexp)))

(define-key vg-project-map (kbd "p") 'project-switch-project)
(define-key vg-project-map (kbd "P") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-switch-project)))

(define-key vg-project-map (kbd "b") 'project-switch-to-buffer)
(define-key vg-project-map (kbd "B") (lambda()
                                       (interactive)
                                       (other-window-prefix)
                                       (call-interactively 'project-switch-to-buffer)))

;; Dir Locals:
(define-prefix-command 'vg-dir-locals-map)
(define-key vg-personal-map (kbd "l") 'vg-dir-locals-map)
(define-key vg-dir-locals-map (kbd "l") 'vg-project-tasks-run)
(define-key vg-dir-locals-map (kbd "r") 'vg-reload-dir-locals-for-current-buffer)
(define-key vg-dir-locals-map (kbd "R") 'vg-reload-dir-locals-for-all-buffer-in-this-directory)

;; Information:
(define-prefix-command 'vg-information-map)
(define-key vg-personal-map (kbd "/") 'vg-information-map)
(define-key vg-information-map (kbd "/") 'vg-info-copy-current-position)

;; Agenda:
(define-prefix-command 'vg-agenda-map)
(define-key vg-personal-map (kbd "a") 'vg-agenda-map)
(define-key vg-agenda-map (kbd "a") 'org-agenda-list)
(define-key vg-agenda-map (kbd "A") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'org-agenda-list)))

;; Intrigue:
(define-prefix-command 'vg-intrigue-map)
(define-key vg-personal-map (kbd "i") 'vg-intrigue-map)
(define-key vg-intrigue-map (kbd "i") 'intrigue-find)
(define-key vg-intrigue-map (kbd "I") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'intrigue-find)))
(define-key vg-intrigue-map (kbd "a") 'intrigue-add)
(define-key vg-intrigue-map (kbd "d") 'intrigue-remove)
(define-key vg-intrigue-map (kbd "n") 'intrigue-next)
(define-key vg-intrigue-map (kbd "p") 'intrigue-prev)

;; Open external tools:
(define-prefix-command 'vg-open-map)
(define-key vg-personal-map (kbd "o") 'vg-open-map)
(define-key vg-open-map (kbd "t") 'vg-open-kitty-here)
(define-key vg-open-map (kbd "f") 'vg-open-finder-here)
(define-key vg-open-map (kbd "e") 'make-frame)

;; Yas:
(define-prefix-command 'vg-yas-map)
(define-key vg-personal-map (kbd "y") 'vg-yas-map)
(define-key vg-yas-map (kbd "y") 'yas-insert-snippet)
(define-key vg-yas-map (kbd "n") 'yas-new-snippet)
(define-key vg-yas-map (kbd "N") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'yas-new-snippet)))
(define-key vg-yas-map (kbd "e") 'yas-visit-snippet-file)
(define-key vg-yas-map (kbd "E") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'yas-visit-snippet-file)))

;; Bookmarks:
(define-prefix-command 'vg-bookmark-map)
(define-key vg-personal-map (kbd "b") 'vg-bookmark-map)
(define-key vg-bookmark-map (kbd "b") 'bookmark-jump)
(define-key vg-bookmark-map (kbd "B") 'bookmark-jump-other-window)
(define-key vg-bookmark-map (kbd "m") 'bookmark-set)
(define-key vg-bookmark-map (kbd "d") 'bookmark-delete)
(define-key vg-bookmark-map (kbd "r") 'bookmark-rename)
(define-key vg-bookmark-map (kbd "e") 'edit-bookmarks)
(define-key vg-bookmark-map (kbd "E") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'edit-bookmarks)))

;; Denote:
(define-prefix-command 'vg-denote-map)
(define-key vg-personal-map (kbd "d") 'vg-denote-map)
(define-key vg-denote-map (kbd "d") 'denote-open-or-create)
(define-key vg-denote-map (kbd "D") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'denote-open-or-create)))
(define-key vg-denote-map (kbd "c") 'denote-create-note)
(define-key vg-denote-map (kbd "r") 'denote-rename-file)

;; Eglot:
(define-prefix-command 'vg-eglot-map)
(define-key vg-personal-map (kbd ".") 'vg-eglot-map)
(define-key vg-eglot-map (kbd ".") 'xref-find-definitions)
(define-key vg-eglot-map (kbd ">") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'xref-find-definitions)))
(define-key vg-eglot-map (kbd "a") 'eglot-code-actions)
(define-key vg-eglot-map (kbd "f") 'eglot-format-buffer)
(define-key vg-eglot-map (kbd "r") 'xref-find-references)
(define-key vg-eglot-map (kbd "R") (lambda()
                                        (interactive)
                                        (other-window-prefix)
                                        (call-interactively 'xref-find-references)))
(define-key vg-eglot-map (kbd "%") 'eglot-rename)
(define-key vg-eglot-map (kbd "k") 'eglot-shutdown)
(define-key vg-eglot-map (kbd "K") 'eglot-shutdown-all)

;; Magit:
(define-prefix-command 'vg-magit-map)
(define-key vg-personal-map (kbd "m") 'vg-magit-map)
(define-key vg-magit-map (kbd "m") 'magit-status)
(define-key vg-magit-map (kbd "p") 'magit-pull)
(define-key vg-magit-map (kbd "c") 'magit-checkout)
(define-key vg-magit-map (kbd "b") 'magit-blame)
(define-key vg-magit-map (kbd "l") 'magit-log)

;; Smerge:
(define-prefix-command 'vg-smerge-map)
(define-key vg-magit-map (kbd "s") 'vg-smerge-map)
(define-key vg-smerge-map (kbd "r") 'smerge-refine)
(define-key vg-smerge-map (kbd "p") 'smerge-prev)
(define-key vg-smerge-map (kbd "n") 'smerge-next)
(define-key vg-smerge-map (kbd "a") 'smerge-keep-all)
(define-key vg-smerge-map (kbd "u") 'smerge-keep-upper)
(define-key vg-smerge-map (kbd "b") 'smerge-keep-base)
(define-key vg-smerge-map (kbd "l") 'smerge-keep-lower)

;; Flycheck:
(define-prefix-command 'vg-flycheck-map)
(define-key vg-personal-map (kbd "f") 'vg-flycheck-map)
(define-key vg-flycheck-map (kbd "f") 'flycheck-next-error)
(define-key vg-flycheck-map (kbd "l") 'flycheck-list-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Specific Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Async Shell Commands.
(add-hook 'shell-mode-hook
          (lambda ()
            ;; Easily kill shell command buffers with q.
            (when (string-match-p (regexp-quote "Async Shell Command") (buffer-name))
              (local-set-key (kbd "q") #'kill-current-buffer))))
