;;;;;;;;;;;;;;;;;;;;;
;; General Helpers ;;
;;;;;;;;;;;;;;;;;;;;;

(defun vg-unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun vg-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun vg-quick-zap-up-to-char (p c)
  "The same as zap up to char, but without the mini buffer prompt.
P: The prefix argument or the count.
C: The character to zap up to."
  (interactive "P\nc")
  (let ((cnt (cond ((null p) 1)
                   ((symbolp p) -1)
                   (t p))))
    (zap-up-to-char cnt c)))

(defun vg-kill-word-negative()
  (interactive)
  (kill-word -1))

(defun vg-other-window-negative()
  (interactive)
  (other-window -1))

(defun vg-open-kitty-here ()
  (interactive)
  (vg-async-shell-command-no-window "kitty `pwd`"))

(defun vg-open-finder-here ()
  (interactive)
    (when (eq system-type 'gnu/linux)
      (vg-async-shell-command-no-window "nautilus ."))
    (when (eq system-type 'darwin)
      (vg-async-shell-command-no-window "open .")))

(defun vg-presentation-toggle()
  "Change font size for presentation use."
  (interactive)
  (defvar vg-presentation-mode nil)
  (if vg-presentation-mode
      (progn
        (setq vg-presentation-mode nil)
        (set-face-attribute 'default nil :height 135))
    (progn
      (setq vg-presentation-mode t)
      (set-face-attribute 'default nil :height 165))))

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

;; TODO: Rewrite this function.
(defun vg-get-project-root ()
  "Return a project's root directory."
  (interactive)
  (or (vc-root-dir)
      (locate-dominating-file default-directory ".dir-locals.el")))

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

;; TODO: Can we close the shell command buffer with q (yes) but how to not end all processes.?
(defun vg-shell-command-in-project-root (command &optional hidden)
  "Run the provided command in the project root directory.
COMMAND: The shell command.
HIDDEN: If non nil, hide the command in the background."
  (interactive)
  (let* ((project-root (vg-get-project-root))
         (final-command (concat "cd" " " project-root " && " command)))
    (if project-root
        (if hidden
            (vg-async-shell-command-no-window final-command)
          (progn
            ;; TODO: Check if this other window command actually works.
            (async-shell-command final-command)
            (other-window 1)))
      (message "Project root could not be found..."))))

(defun vg-shell-command-in-kitty-project-root (command &optional)
  "Run the command in a new kitty window based in the project root directory.
COMMAND: The shell command."
  (interactive)
  (let* ((project-root (vg-get-project-root))
         (source-zshrc "source ~/.zshrc && ")
         (kitty-command (concat "kitty" " " "zsh -c \"" source-zshrc command " && exit; exec zsh\""))
         (final-command (concat "cd" " " project-root " && " kitty-command)))
    (if project-root
        (vg-async-shell-command-no-window final-command)
      (message "Project root could not be found..."))))

(defun vg-convert-markdown-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(defun vg-run-in-docker-container (container-name cmd)
  (vg-shell-command-in-kitty-project-root
   (mapconcat 'identity
              (list "docker exec -it" container-name "sh -c" (concat "'" cmd "'"))
              " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Information Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vg-info-copy-current-position()
  (interactive)
  (if (buffer-file-name (current-buffer))
      (let ((line-num (int-to-string (line-number-at-pos)))
            (file-path (buffer-file-name)))
        (with-temp-buffer (insert
                           (mapconcat
                            'identity
                            (list
                             (concat "Line Number: " line-num)
                             (concat "File Path: " file-path)) "\n"))
                          (call-interactively 'mark-whole-buffer)
                          (kill-ring-save (point-min) (point-max)))
        (message "Current file position information copied."))
    (message "Buffer has no file associated with it.")))

;;;;;;;;;;;;;;;;;;;;;;
;; Markdown Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun vg-markdown-to-pdf ()
  "Convert the current Markdown buffer to a PDF in the downloads folder."
  (interactive)
  (let ((file-path (buffer-file-name))
        (file-ext (url-file-extension buffer-file-name))
        (file-pdf-name (concat "~/Documents/" (file-name-base (buffer-file-name)) "-" (format-time-string "%Y-%m-%d-%H%M") ".pdf")))
    (if (and
         file-path
         (string-equal file-ext ".md")
         (string-equal major-mode "markdown-mode"))
        (shell-command (concat "pandoc -V geometry:margin=2cm -top-level-division=section --pdf-engine=xelatex -f markdown-implicit_figures -t pdf"
                               " -o "
                               file-pdf-name
                               " "
                               file-path))
      (message "This is not a markdown file!"))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Dir Locals Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun vg-reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer))
  (message "Tasks reloaded!"))

(defun vg-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory`, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (vg-reload-dir-locals-for-current-buffer)))))
  (message "Tasks reloaded for all buffers!"))

;; Automatically reload the dir-locals, maybe not.
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

;; TODO: Maybe implement global tasks, that are available across all files.

;; The .dir-locals.el file should hold a specially-constructed list, which maps
;; major mode names (symbols) to alists (see Association Lists in The Emacs Lisp
;; Reference Manual). Each alist entry consists of a variable name and the
;; directory-local value to assign to that variable, when the specified major
;; mode is enabled. Instead of a mode name, you can specify ‘nil’, which means
;; that the alist applies to any mode; or you can specify a subdirectory (a
;; string), in which case the alist applies to all files in that sub-directory.

;; These tasks should be defined within your projects .dir-locals.el
(defvar vg-project-tasks '(("Hello World" . (lambda () (message "Hello Project World")))))

(defun vg-project-tasks-run (choice)
  "Run global and project specific tasks.
CHOICE: The command key to run."
  (interactive
   (list (completing-read "Run Task: "
                          vg-project-tasks
                          nil t)))
  (let ((f  (cdr (assoc choice vg-project-tasks))))
    (message "Task running...")
    (funcall f)))
(global-set-key (kbd "s-t") 'vg-project-tasks-run)

;;;;;;;;;;;;;
;; On Save ;;
;;;;;;;;;;;;;

;; This should be defined within your .dir-locals.el file for each mode. It
;; allows you to specify a lambda function to call on save.
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
         (eq major-mode 'scala-mode)
         (eq major-mode 'toml-mode)
         (eq major-mode 'php-mode)
         (eq major-mode 'go-mode)
         (eq major-mode 'typescript-mode))
    ;; Let's run a flyspell only on save, for performance reasons.
    ;; (flyspell-buffer)
    (funcall vg-on-save-lambda)
    (message "On save function has been called")))

;; Call the before save functions.
(add-hook 'before-save-hook #'vg-before-save-hook)
