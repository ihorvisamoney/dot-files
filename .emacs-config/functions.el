(defun vg-presentation-toggle()
  "Change font size for presentation use."
  (interactive)
  (defvar vg-presentation-mode nil)
  (if vg-presentation-mode
      (progn
        (setq vg-presentation-mode nil)
        (set-face-attribute 'default nil :family "Monospace" :height 94)
        ;; (set-face-attribute 'default nil :family "Menlo" :height 148 :weight 'normal)

        )
    (progn
      (setq vg-presentation-mode t)
      (set-face-attribute 'default nil :family "Monospace" :height 140)
      ;; (set-face-attribute 'default nil :family "Menlo" :height 200 :weight 'normal)
      )))

(defun vg-toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 90) '(100 . 100)))))

(defun vg-async-shell-command-no-window (command)
  "Run the provided command in the background.
COMMAND: The shell command."
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun vg-ascii-table ()
    "Display basic ASCII table (0 thru 128)."
    (interactive)
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
    (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
    (save-excursion (let ((i -1))
      (insert "ASCII characters 0 thru 127.\n\n")
      (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
      (while (< i 31)
        (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                        (setq i (+ 1  i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)))
        (setq i (- i 96))))))

(defun vg-get-project-root ()
  (locate-dominating-file default-directory ".dir-locals.el"))

;;;;;;;;;;;;;;;;;
;; Note Taking ;;
;;;;;;;;;;;;;;;;;

;; ;; Open notes folder.
;; (define-key global-map (kbd "C-c n")
;;   (lambda ()
;;     (interactive)
;;     (find-file "~/Devenv/notes/")))

;; ;; Create a new note.
;; (define-key global-map (kbd "C-c N")
;;   (lambda ()
;;   (interactive)
;;   (let ((notes-file-extention ".md")
;;         (notes-file-heading-prefix "# ")
;;         (notes-directory "~/Devenv/notes/notebook/")
;;         (note-name (replace-regexp-in-string
;;                     " "
;;                     "-"
;;                     (downcase (read-string "Note Name: "))))
;;         (note-date-string (format-time-string "%Y-%m-%d-")))

;;     ;; Create the new note file.
;;     (find-file-other-window (concat
;;                 notes-directory
;;                 note-date-string
;;                 note-name
;;                 notes-file-extention))

;;     ;; Add title to note file.
;;     (insert (concat
;;              notes-file-heading-prefix)
;;             (replace-regexp-in-string "-" " " (capitalize note-name)))

;;     ;; Save note.
;;     (save-buffer))))
