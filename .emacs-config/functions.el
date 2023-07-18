(defun vg-presentation-toggle()
  "Change font size for presentation use."
  (interactive)
  (defvar vg-presentation-mode nil)
  (if vg-presentation-mode
      (progn
        (setq vg-presentation-mode nil)
        (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 104))
    (progn
      (setq vg-presentation-mode t)
      (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140))))

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
  ;; (let )
  ;; Use project.el to get the directory of the current project.
  ;; (concat (cdr (project-current)))
  ;; (if (vc-root-dir)
  ;;     (vc-root-dir)
  ;;   (locate-dominating-file default-directory ".dir-locals.el")
  ;;   )
  ;; (when (vc-root-dir))
  (locate-dominating-file default-directory ".dir-locals.el")
  )

(defun vg-func-region (start end func)
  "Run a FUNC over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun vg-url-encode (start end)
  "Urlencode the region between START and END in current buffer."
  (interactive "r")
  (vg-func-region start end #'url-hexify-string))

(defun vg-url-decode (start end)
  "De-urlencode the region between START and END in current buffer."
  (interactive "r")
  (vg-func-region start end #'url-unhex-string))

(defun vg-dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;; TODO: Maybe combine the following two functions.
(defun vg-shell-command-in-project-root (command)
  "Run the provided command in the project root directory.
COMMAND: The shell command."
  (interactive)

  ;; TODO: Handle this better. Just in case we run the command in a non project buffer.
  (when (vc-root-dir)
    (let*
        ;; TODO: make root directory more intelligent.
      ((project-root (vc-root-dir))
       (final-command (concat "cd" " " project-root " && " command)))
    (async-shell-command final-command)
    (message "Task running..."))))

(defun vg-shell-command-in-project-root-no-window (command)
  "Run the provided command in the project root directory.
COMMAND: The shell command."
  (interactive)

  ;; TODO: Handle this better. Just in case we run the command in a non project buffer.
  (when (vc-root-dir)
    (let*
        ;; TODO: make root directory more intelligent.
        ((project-root (vc-root-dir))
         (final-command (concat "cd" " " project-root " && " command)))
      (vg-async-shell-command-no-window final-command)
      (message "Task running..."))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Dir Locals Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun vg-reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;; (defun vg-reload-dir-locals-for-all-buffer-in-this-directory ()
;;   "For every buffer with the same `default-directory` as the current buffer,
;;  reload dir-locals."
;;   (interactive)
;;   (let ((dir default-directory))
;;     (dolist (buffer (buffer-list))
;;       (with-current-buffer buffer
;;         (when (equal default-directory dir)
;;           (vg-reload-dir-locals-for-current-buffer))))))

;; (add-hook 'emacs-lisp-mode-hook
;;           (defun enable-autoreload-for-dir-locals ()
;;             (when (and (buffer-file-name)
;;                        (equal dir-locals-file
;;                               (file-name-nondirectory (buffer-file-name))))
;;               (add-hook 'after-save-hook
;;                         'my-reload-dir-locals-for-all-buffer-in-this-directory
;;                         nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project specific bindings? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The .dir-locals.el file should hold a specially-constructed list, which maps
;; major mode names (symbols) to alists (see Association Lists in The Emacs Lisp
;; Reference Manual). Each alist entry consists of a variable name and the
;; directory-local value to assign to that variable, when the specified major
;; mode is enabled. Instead of a mode name, you can specify ‘nil’, which means
;; that the alist applies to any mode; or you can specify a subdirectory (a
;; string), in which case the alist applies to all files in that subdirectory.

;; List of tasks.

;; TODO: Define global tasks.
;; Define your global tasks here.
(defvar vg-global-tasks '(("List files" . "ls -la ./")))

;; These tasks should be defined within your projects .dir-locals.el
(defvar vg-project-tasks '(("Hello World" . "echo 'Hello World'")))

(defun vg-get-appended-tasks()
  "Append global and project task lists."
  (append vg-global-tasks vg-project-tasks))

(defun vg-project-tasks-run (choice)
  "Run global and project specific tasks.
CHOICE: The command key to run."
  (interactive
   (list (completing-read "Run Task: "
                          (vg-get-appended-tasks)
                           nil t)))
  (let ((command (cdr (assoc choice (vg-get-appended-tasks)))))
    (vg-shell-command-in-project-root command)))

(global-set-key (kbd "s-t") 'vg-project-tasks-run)

;;;;;;;;;;;;;
;; On Save ;;
;;;;;;;;;;;;;

;; This should be defined within your .dir-locals.el file for each mode.
(defvar vg-on-save-lambda (lambda() (message "The on save command has been called.")))

(defun vg-before-save-hook ()
  "Run the defined `vg-on-save-lambda` lambda on all defined major modes."
  (when (or
         (eq major-mode 'sh-mode)
         (eq major-mode 'js-mode)
         (eq major-mode 'c-mode)
         (eq major-mode 'c++-mode)
         (eq major-mode 'yaml-mode)
         (eq major-mode 'markdown-mode)
         (eq major-mode 'json-mode)
         (eq major-mode 'web-mode)
         (eq major-mode 'php-mode)
         (eq major-mode 'typescript-mode))
    (funcall vg-on-save-lambda)))

;; Call the before save functions.
(add-hook 'before-save-hook #'vg-before-save-hook)

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
