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

;; Personal mappings.
(global-set-key (kbd "s-p") 'project-other-window-command)
(global-set-key (kbd "s-r") 'recentf)
(global-set-key (kbd "s-y") 'yas-insert-snippet)
(global-set-key (kbd "s-b") 'bookmark-jump-other-window)
(global-set-key (kbd "s-B") 'bookmark-set)

;; Eglot.
(global-unset-key (kbd "s-l"))
(global-set-key (kbd "s-l r") 'eglot-rename)
(global-set-key (kbd "s-l a") 'eglot-code-actions)
(global-set-key (kbd "s-l f") 'eglot-format-buffer)
(global-set-key (kbd "s-l g") 'xref-find-references)

;; Version control (Magit, Smerge).
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-G r") 'smerge-refine)
(global-set-key (kbd "s-G p") 'smerge-prev)
(global-set-key (kbd "s-G n") 'smerge-next)
(global-set-key (kbd "s-G a") 'smerge-keep-all)
(global-set-key (kbd "s-G u") 'smerge-keep-upper)
(global-set-key (kbd "s-G m") 'smerge-keep-base)
(global-set-key (kbd "s-G l") 'smerge-keep-lower)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Specific Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Async Shell Commands.
(add-hook 'shell-mode-hook
          (lambda ()
            ;; Easily kill shell command buffers with q.
            (when (string-match-p (regexp-quote "Async Shell Command") (buffer-name))
              (local-set-key (kbd "q") #'kill-current-buffer))))

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

;;;;;;;;;;;;;;;;;;;;;
;; Prefix Bindings ;;
;;;;;;;;;;;;;;;;;;;;;

;; Org agenda
(global-set-key (kbd "s-1") 'org-agenda-list)

;; Bookmark manager
(global-set-key (kbd "s-2") 'edit-bookmarks)

;; Open terminal in current files folder.
(define-key global-map (kbd "s-3")
  (lambda ()
    (interactive)
    (vg-async-shell-command-no-window "kitty `pwd`")))

;; Open finder in current files folder.
(define-key global-map (kbd "s-4")
  (lambda ()
    (interactive)
    (when (eq system-type 'gnu/linux)
      (vg-async-shell-command-no-window "nautilus ."))
    (when (eq system-type 'darwin)
      (vg-async-shell-command-no-window "open ."))))

;; Elfeed
(global-set-key (kbd "s-0") 'elfeed)
