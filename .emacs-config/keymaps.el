;; Function keys:
(define-key global-map (kbd "<f12>") 'vg-presentation-toggle)
(global-set-key (kbd "<f9>") 'vg-toggle-transparency)

;; This replaces the help map, but you can still use F1.
;; Useful backspace bindings.
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") (lambda ()
                              (interactive)
                              (kill-word -1)))
(global-set-key (kbd "C-x d") 'dired-other-window)
(global-set-key (kbd "C-x b") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-x C-f") 'find-file-other-window)

;; Save the current buffer.
(define-key global-map (kbd "<C-return>") 'save-buffer)

;; Keep save binding for Latex mode.
(eval-after-load 'tex-mode
  '(define-key latex-mode-map (kbd "<C-return>") 'save-buffer))

;; Use Ibuffer instead.
(define-key global-map (kbd "C-x C-b") 'ibuffer-other-window)

;; Add a comment box.
(define-key global-map (kbd "C-c b") 'comment-box)

;; Zap up to char quickly.
(defun vg-quick-zap-up-to-char (p c)
  "The same as zap up to char, but without the mini buffer prompt.
P: The prefix argument or the count.
C: The character to zap up to."
  (interactive "P\nc")
  (let ((cnt (cond ((null p) 1)
                   ((symbolp p) -1)
                   (t p))))
    (zap-up-to-char cnt c)))
(define-key global-map (kbd "C-z") 'vg-quick-zap-up-to-char)

;; Change window backward.
(define-key global-map (kbd "C-x O") (lambda ()
                                       (interactive)
                                       (other-window -1)))

;; Duplicate a line.
(define-key global-map (kbd "C-c d") (lambda ()
                                       (interactive)
                                       (move-beginning-of-line 1)
                                       (kill-line)
                                       (yank)
                                       (open-line 1)
                                       (forward-line 1)
                                       (yank)))

;; Elfeed
(global-set-key (kbd "s-0") 'elfeed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Prefix Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finding things:
(define-prefix-command 'vg-find-map)
(global-set-key (kbd "s-f") 'vg-find-map)
(define-key vg-find-map (kbd "f") 'vg-find-file-other-window)
(define-key vg-find-map (kbd "b") 'vg-find-buffer-other-window)
(define-key vg-find-map (kbd "p") 'vg-find-project-other-window)
(define-key vg-find-map (kbd "B") 'switch-to-buffer-other-window)
(define-key vg-find-map (kbd "d") 'vg-find-directory-other-window)
(define-key vg-find-map (kbd "D") 'vg-find-directory-other-window)
(define-key vg-find-map (kbd "s") 'project-find-regexp)

;; Add our own prefix key
(define-prefix-command 'vg-personal-map)
(global-set-key (kbd "s-q") 'vg-personal-map)
(define-key vg-personal-map (kbd "r") 'recentf)
(define-key vg-personal-map (kbd "p") 'project-other-window-command)

;; Tasks:
(define-prefix-command 'vg-tasks-map)
(define-key vg-personal-map (kbd "t") 'vg-tasks-map)
(define-key vg-tasks-map (kbd "t") 'vg-project-tasks-run)
(define-key vg-tasks-map (kbd "r") 'vg-reload-dir-locals-for-current-buffer)
(define-key vg-tasks-map (kbd "R") 'vg-reload-dir-locals-for-all-buffer-in-this-directory)

;; Agenda:
(define-prefix-command 'vg-agenda-map)
(define-key vg-personal-map (kbd "a") 'vg-agenda-map)
(define-key vg-agenda-map (kbd "a") 'org-agenda-list)

;; Intrigue:
(define-prefix-command 'vg-intrigue-map)
(define-key vg-personal-map (kbd "i") 'vg-intrigue-map)
(define-key vg-intrigue-map (kbd "i") 'intrigue-find)
(define-key vg-intrigue-map (kbd "a") 'intrigue-add)
(define-key vg-intrigue-map (kbd "d") 'intrigue-remove)
(define-key vg-intrigue-map (kbd "n") 'intrigue-next)
(define-key vg-intrigue-map (kbd "p") 'intrigue-prev)

;; Open external tools:
(define-prefix-command 'vg-open-map)
(define-key vg-personal-map (kbd "o") 'vg-open-map)
(define-key vg-open-map (kbd "t") 'vg-open-kitty-here)
(define-key vg-open-map (kbd "f") 'vg-open-finder-here)

;; Yas:
(define-prefix-command 'vg-yas-map)
(define-key vg-personal-map (kbd "y") 'vg-yas-map)
(define-key vg-yas-map (kbd "y") 'yas-insert-snippet)
(define-key vg-yas-map (kbd "n") 'yas-new-snippet)
(define-key vg-yas-map (kbd "e") 'yas-visit-snippet-file)

;; Bookmarks:
(define-prefix-command 'vg-bookmark-map)
(define-key vg-personal-map (kbd "b") 'vg-bookmark-map)
(define-key vg-bookmark-map (kbd "b") 'bookmark-jump-other-window)
(define-key vg-bookmark-map (kbd "m") 'bookmark-set)
(define-key vg-bookmark-map (kbd "d") 'bookmark-delete)
(define-key vg-bookmark-map (kbd "r") 'bookmark-rename)
(define-key vg-bookmark-map (kbd "e") 'edit-bookmarks)

;; Denote:
(define-prefix-command 'vg-denote-map)
(define-key vg-personal-map (kbd "d") 'vg-denote-map)
(define-key vg-denote-map (kbd "d") 'denote-open-or-create)
(define-key vg-denote-map (kbd "c") 'denote-create-note)
(define-key vg-denote-map (kbd "r") 'denote-rename-file)

;; Eglot:
(define-prefix-command 'vg-eglot-map)
(define-key vg-personal-map (kbd "e") 'vg-eglot-map)
(define-key vg-eglot-map (kbd "a") 'eglot-code-actions)
(define-key vg-eglot-map (kbd "f") 'eglot-format-buffer)
(define-key vg-eglot-map (kbd "d") 'xref-find-definitions)
(define-key vg-eglot-map (kbd "r") 'xref-find-references)
(define-key vg-eglot-map (kbd "R") 'eglot-rename)

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

;; Major search:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Specific Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Async Shell Commands.
(add-hook 'shell-mode-hook
          (lambda ()
            ;; Easily kill shell command buffers with q.
            (when (string-match-p (regexp-quote "Async Shell Command") (buffer-name))
              (local-set-key (kbd "q") #'kill-current-buffer))))

;;;;;;;;;;;;;;;;;;;;;
;; Prefix Bindings ;;
;;;;;;;;;;;;;;;;;;;;;

;; Org agenda
;; (global-set-key (kbd "s-1") 'org-agenda-list)

;; Bookmark manager
;; (global-set-key (kbd "s-2") 'edit-bookmarks)
