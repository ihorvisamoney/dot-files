(defun vg-presentation-toggle()
  "Change font size for presentation use."
  (interactive)
  (defvar vg-presentation-mode nil)
  (if vg-presentation-mode
      (progn
        (setq vg-presentation-mode nil)
        (set-face-attribute 'default nil :height 150))
    (progn
      (setq vg-presentation-mode t)
      (set-face-attribute 'default nil :height 180))))

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

;; TODO: Can we close the shell command buffer with q?
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
         (kitty-command (concat "kitty" " " "bash -c \"" command "; exec bash\""))
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
    (hack-dir-local-variables-non-file-buffer)))

(defun vg-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory`, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (vg-reload-dir-locals-for-current-buffer))))))

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


;;;;;;;;;;;;;;;;
;; Tab Groups ;;
;;;;;;;;;;;;;;;;

(defun vg-tab-bar-groups-current-tab ()
  "Retrieve original data about the current tab."
  (assq 'current-tab (funcall tab-bar-tabs-function)))

(defun vg-tab-bar-groups-tab-group-name (&optional tab)
  "The group name of the given TAB (or the current tab)."
  (alist-get 'group (or tab (vg-tab-bar-groups-current-tab))))

(defun vg-tab-bar-groups-parse-groups ()
  "Build an alist of tabs grouped by their group name.

Successive tabs that don't belong to a group are grouped under
intermitting nil keys.

For example, consider this list of tabs: groupA:foo, groupB:bar,
baz, qux, groupC:quux, quuz, groupB:corge, groupA:grault.

Calling this function would yield this result:

'((groupA (foo grault))
  (groupB (bar corge))
  (nil (baz qux))
  (groupC (grault))
  (nil (quuz)))"
  (let* ((tabs (frame-parameter (selected-frame) 'tabs))
         (result '()))
    (dolist (tab tabs)
      (let* ((group-name (vg-tab-bar-groups-tab-group-name tab))
             (group (and group-name (intern group-name)))
             (new-named-group-p (and group (null (assq group result))))
             (in-nil-group-p (and (consp (car result)) (null (caar result))))
             (new-nil-group-p (not (or group in-nil-group-p))))
        (if (or new-named-group-p new-nil-group-p)
            (push (cons group (list tab)) result)
          (nconc (alist-get group result) (list tab)))))
    (reverse result)))

(defvar vg-previous-tab-group nil)

(defun vg-switch-tab-group (choice)
  "Run global and project specific tasks.
CHOICE: The command key to run."
  (interactive
   (list (completing-read "Tab Groups: "
                          (vg-tab-bar-groups-parse-groups)
                          nil t)))
  (let* ((tabs (frame-parameter (selected-frame) 'tabs)))

    ;; Set the previous tab, if different.
    (let ((prev vg-previous-tab-group)
          (cur (1+ (tab-bar--current-tab-index))))
      (when (not (eql cur prev))
        (setq vg-previous-tab-group cur)))

    ;; Find and select the chosen tab.
    (dolist (tab tabs)
      (let* ((group-name (vg-tab-bar-groups-tab-group-name tab))
             (group (and group-name (intern group-name))))
        (when (string= group-name choice)
          (tab-bar-select-tab (1+ (tab-bar--tab-index tab))))))))

(defun vg-switch-to-previous-tab-group ()
  "Switch to the previous active tab group."
  (interactive)
  (let ((prev vg-previous-tab-group)
        (cur (1+ (tab-bar--current-tab-index))))
    (setq vg-previous-tab-group cur)
    (tab-bar-select-tab prev)))

;;;;;;;;;;;;;
;; On Save ;;
;;;;;;;;;;;;;

;; This should be defined within your .dir-locals.el file for each mode. It
;; allows you to specify a lambda function to call on save.
(defvar vg-on-save-lambda (lambda() (message "The on save command has been called.")))

(defun vg-before-save-hook ()
  "Run the defined `vg-on-save-lambda` lambda on all defined major modes."
  (when (or
         (eq major-mode 'fundamental-mode)
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
    (funcall vg-on-save-lambda)))

;; Call the before save functions.
(add-hook 'before-save-hook #'vg-before-save-hook)

;;;;;;;;;;;;;
;; Removed ;;
;;;;;;;;;;;;;

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
