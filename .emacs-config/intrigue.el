(require 'project)

(setq-default intrigue-file-location "~/Dotfiles/.emacs-intrigue.el")

;; TODO: Add function of cycling between intrigue files.

;; Open essence file in other window.
;; (setq-default intrigue-use-other-window t)

(setq-default intrigue--files nil)

(defun intrigue--get-project-key()
  (project-root (project-current nil)))

(defun intrigue--load()
  "Hello world."
  (let* ((saved (with-temp-buffer
                  (insert-file-contents intrigue-file-location)
                  (goto-char (point-min))
                  (set 'intrigue--files (read (current-buffer))))))))

(intrigue--load)

(defun intrigue--save()
  "Hello world."
  (when intrigue--files
    (with-temp-file intrigue-file-location (prin1 intrigue--files (current-buffer)))))

(defun intrigue-add()
  "Hello world."
  (interactive)
  (let* ((p-key (intrigue--get-project-key))
         (f-name (file-name-nondirectory (buffer-file-name)))
         (f-path (buffer-file-name))
         (p-list (assoc p-key intrigue--files)))
    (if p-list
        (let ((p-files (cdr p-list)))
          (when (not (assoc f-name p-files))
            (add-to-list 'p-files (cons f-name f-path))
            (setcdr p-list p-files)))
      (add-to-list 'intrigue--files (cons p-key (list (cons f-name f-path))))))
  (intrigue--save)
  (message "File added to intrigue"))

(defun intrigue-remove (choice)
  "Hello world.
CHOICE: hello world."
  (interactive
   (list (completing-read "Remove Intrigue Entry: "
                          (cdr (assoc (intrigue--get-project-key) intrigue--files))
                          nil t)))
  (let* ((p-files (assoc (intrigue--get-project-key) intrigue--files)))
    (assoc-delete-all choice p-files))
  (intrigue--save)
  (message "File removed from intrigue"))

(defun intrigue-find (choice)
  "Hello world.
CHOICE: hello world."
  (interactive
   (list (completing-read "Intrigue Entries: "
                          (cdr (assoc (intrigue--get-project-key) intrigue--files))
                          nil t)))
  (let* ((p-files (cdr (assoc (intrigue--get-project-key) intrigue--files)))
         (f-name (car (assoc choice p-files)))
         (f-path (cdr (assoc choice p-files)))
         (current? (string= f-path (buffer-file-name))))
    (when (not current?)
      (find-file f-path)
      ;; (if intrigue-use-other-window
      ;;     (find-file-other-window f-path)
      ;;   (find-file f-path))
      )))

(defun intrigue-next()
  (interactive)
  (message "This function needs to be implemented!"))

(defun intrigue-previous()
  (interactive)
  (message "This function needs to be implemented!"))
