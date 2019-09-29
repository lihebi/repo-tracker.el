;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'magit)

(defcustom repo-tracker-repos '()
  "A list of repo paths to track."
  :group 'repo-tracker
  :type '(list string))

(defvar repo-tracker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'repo-tracker-refresh)
    map)
  "Keymap used in repo-tracker buffer.")

(defun repo-tracker-refresh ()
  "Refresh repo-tracker buffer."
  (interactive)
  (repo-tracker--hard-save-excursion
   (repo-tracker)))

(define-derived-mode repo-tracker-mode special-mode "repo-tracker"
  :group 'repo-tracker
  (setq truncate-lines t)
  (buffer-disable-undo))

(defun repo-tracker-buffer-display (name proc)
  "Run PROC in the buffer named NAME."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall proc))
      (repo-tracker-mode)
      (set-buffer-modified-p nil))
    ;; seems that I don't need to define a new mode, just use
    ;; local-set-key for key bindings
    ;; (local-set-key "q" 'scribble-quit-buffer-display)
    (pop-to-buffer buffer)))

(defmacro repo-tracker--hard-save-excursion (body)
  "Save excursion and run BODY.

This works even if buffer is erased."
  (let ((pos (point))
        (winpos (window-start)))
    body
    (set-window-start (selected-window) winpos)
    (goto-char pos)))

(defun repo-tracker--git-status (repo)
  "Run git status in REPO and return output strings as a list."
  (with-temp-buffer
    (cd repo)
    (magit-git-lines "status")))

(defun repo-tracker--git-fetch (repo)
  "Run git fetch in REPO."
  (with-temp-buffer
    (cd repo)
    (magit-git-fetch repo)))

(defun repo-tracker-fetch-all ()
  "Fetch all repos from upstream."
  (interactive)
  (dolist (repo repo-tracker-repos)
    (repo-tracker--git-fetch repo)))

(defun repo-tracker-colorize (str)
  "Colorize parts of STR."
  (reduce (lambda (acc reg)
            (replace-regexp-in-string
             reg (lambda (s)
                   (propertize s 'face 'font-lock-keyword-face))
             acc t))
          '("modified" "untracked")
          :initial-value str))

(defun repo-tracker ()
  "Show repo status."
  (interactive)
  (repo-tracker-buffer-display
   "*repo tracker*"
   (lambda ()
     (dolist (repo repo-tracker-repos)
       (insert (propertize repo 'face 'font-lock-constant-face))
       (insert "\n")
       ;; TODO high light different things differently
       ;; FIXME catch other types of dirtyness
       ;; TODO show only important messages
       (insert (repo-tracker-colorize (string-join (get-status repo) "\n")))
       (insert "\n")))))

;; (get-status "~/.hebi")

(provide 'repo-tracker)

;;; repo-tracker.el ends here
