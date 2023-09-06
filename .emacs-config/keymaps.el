(define-key global-map (kbd "<f12>") 'vg-presentation-toggle)
(global-set-key (kbd "<f9>") 'vg-toggle-transparency)

;; Make things open in other windows by default.
(global-set-key (kbd "C-x d") 'dired-other-window)
;; (global-set-key (kbd "C-x b") 'switch-to-buffer-other-window)
;; (global-set-key (kbd "C-x C-f") 'find-file-other-window)

;; Personal mappings.
(global-set-key (kbd "s-p") 'project-other-window-command)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-a") 'org-agenda-list)
(global-set-key (kbd "s-A") 'org-agenda)
(global-set-key (kbd "s-r") 'recentf)
(global-set-key (kbd "s-b") 'bookmark-jump)
(global-set-key (kbd "s-B") 'bookmark-set)
;; You can do: -G\.el$ pattern to search only specific files.
;; see: https://github.com/emacsorphanage/helm-ag#keymap
;; (global-set-key (kbd "s-s") 'helm-do-ag-project-root)
(global-set-key (kbd "s-o") 'occur)
(global-set-key (kbd "s-e") 'emmet-expand-linge)
(global-set-key (kbd "s-y") 'yas-insert-snippet)
(global-set-key (kbd "s-m") 'man)
(global-set-key (kbd "s-;") 'comment-box)
(global-set-key (kbd "s-f") 'flycheck-list-errors)

;; Eglot
(global-unset-key (kbd "s-l"))
(global-set-key (kbd "s-l r") 'eglot-rename)
(global-set-key (kbd "s-l a") 'eglot-code-actions)
(global-set-key (kbd "s-l f") 'eglot-format-buffer)

;; Project.el can do this
;; (global-set-key (kbd "s-m") 'recompile)
;; (global-set-key (kbd "s-M") 'compile)

;; Window movement.
;; (global-set-key (kbd "s-x <right>") 'windmove-swap-states-right)
;; (global-set-key (kbd "s-x <left>") 'windmove-swap-states-left)

;; NOTE: Maybe move this to mode specific bindings?
;; Git smerge
(global-set-key (kbd "s-G r") 'smerge-refine)
(global-set-key (kbd "s-G p") 'smerge-prev)
(global-set-key (kbd "s-G n") 'smerge-next)
(global-set-key (kbd "s-G u") 'smerge-keep-upper)
(global-set-key (kbd "s-G m") 'smerge-keep-base)
(global-set-key (kbd "s-G l") 'smerge-keep-lower)

;; C-c ^ a         smerge-keep-all
;; C-c ^ b         smerge-keep-base
;; C-c ^ l         smerge-keep-lower
;; C-c ^ m         smerge-keep-upper

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Specific Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verb

(add-hook 'verb-mode-hook
          (lambda ()
            (local-set-key (kbd "s-c s") #'verb-send-request-on-point)))

(add-hook 'verb-response-body-mode-hook
          (lambda ()
            (local-set-key (kbd "s-c s") #'verb-re-send-request)
            (local-set-key (kbd "s-c h") #'verb-toggle-show-headers)))

;; Async Shell Commands

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

;; Quickly switch between the current and previous buffers.
;; (define-key global-map (kbd "C-q C-q") (lambda ()
;;                                        (interactive)
;;                                        (switch-to-buffer (other-buffer))))

;; Use Ibuffer instead.
(define-key global-map (kbd "C-x C-b") 'ibuffer-other-window)

;; Add a comment box.
(define-key global-map (kbd "C-c b") 'comment-box)

;; Search forward until a specific char is found.
;; (defun vg-quick-forward-to-char (p c)
;;   "Jump the cursor forward to a specific character."
;;   (interactive "P\nc")
;;   (let ((cnt (cond ((null p) 1)
;;                    ((symbolp p) -1)
;;                    (t p))))
;;     (search-forward (string c) nil t cnt)))
;; (define-key global-map (kbd "C-S-f") 'vg-quick-forward-to-char)

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

;; Move up by text block (Thanks Xah Lee).
(define-key global-map (kbd "C-S-p") (lambda (&optional n)
                                     (interactive "p")
                                     (let ((n (if (null n) 1 n))
                                           ($i 1))
                                       (while (<= $i n)
                                         (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
                                             (progn (skip-chars-backward "\n\t "))
                                           (progn (goto-char (point-min))
                                                  (setq $i n)))
                                         (setq $i (1+ $i))))))

;; Move down by text block (Thanks Xah Lee).
(define-key global-map (kbd "C-S-n") (lambda (&optional n)
                                     (interactive "p")
                                     (let ((n (if (null n) 1 n)))
                                       (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n))))

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

;; Open terminal in current files folder.
(define-key global-map (kbd "<f5>")
  (lambda ()
    (interactive)
    (vg-async-shell-command-no-window "kitty `pwd`")))

;; Open finder in current files folder.
(define-key global-map (kbd "<f6>")
  (lambda ()
    (interactive)
    (when (eq system-type 'gnu/linux)
      (vg-async-shell-command-no-window "nautilus ."))
    (when (eq system-type 'darwin)
      (vg-async-shell-command-no-window "open ."))))
