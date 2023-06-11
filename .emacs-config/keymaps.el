(define-key global-map (kbd "<f12>") 'vg-presentation-toggle)
(global-set-key (kbd "<f9>") 'vg-toggle-transparency)

;; Personal mappings.
(global-set-key (kbd "s-p") 'project-other-window-command)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-a") 'org-agenda-list)
(global-set-key (kbd "s-A") 'org-agenda)
(global-set-key (kbd "s-r") 'helm-recentf)
(global-set-key (kbd "s-b") 'helm-filtered-bookmarks)
(global-set-key (kbd "s-B") 'bookmark-set)
(global-set-key (kbd "s-s") 'helm-do-grep-ag)
(global-set-key (kbd "s-o") 'helm-occur)
(global-set-key (kbd "s-e") 'emmet-expand-line)
(global-set-key (kbd "s-y") 'yas-insert-snippet)
(global-set-key (kbd "s-Y") 'helm-yas-create-snippet-on-region)
(global-set-key (kbd "s-m") 'man)
(global-set-key (kbd "s-;") 'comment-box)

;; (call-interactively)
;; (global-set-key (kbd "s-p") (lambda ()
;;                               (interactive)
;;                               (call-interactively 'project-switch-project t )))

;; Helps move the cursor to the new split.
;; (global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
;; (global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

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
;; (define-key global-map (kbd "C-x C-b") 'ibuffer-other-window)

;; Compile, make.
;; (define-key global-map (kbd "C-c M") 'compile)
;; (define-key global-map (kbd "C-c m") 'recompile)

;; Open recent f.
;; (define-key global-map (kbd "C-c r") 'recentf-open-files)

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
    (vg-async-shell-command-no-window "nautilus .")))

(define-key global-map (kbd "C-c a") 'org-agenda-list)
