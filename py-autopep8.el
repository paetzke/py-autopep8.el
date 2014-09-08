;;; py-autopep8.el --- Use autopep8 to beautify a Python buffer

;; Copyright (C) 2013-2014, Friedrich Paetzke <paetzke@fastmail.fm>

;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: http://paetzke.me/project/py-autopep8.el
;; Version: 0.5

;;; Commentary:

;; Provides the `py-autopep8' command, which uses the external "autopep8"
;; tool to tidy up the current buffer according to Python's PEP8.

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'before-save-hook 'py-autopep8-before-save)

;; To customize the behaviour of "autopep8" you can set the
;; py-autopep8-options e.g.

;;   (setq py-autopep8-options '("--max-line-length=100"))

;;; Code:

(defgroup py-autopep8 nil
  "Use autopep8 to beautify a Python buffer."
  :group 'convenience
  :prefix "py-autopep8-")


(defcustom py-autopep8-options nil
  "Options used for autopep8.

Note that `--in-place' is used by default."
  :group 'py-autopep8
  :type '(repeat (string :tag "option")))


(defun py-autopep8-apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in py-autopep8-apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in py-autopep8-apply-rcs-patch")))))))))


;;;###autoload
(defun py-autopep8 ()
  "Formats the current buffer according to the autopep8 tool."
  (interactive)
  (when (not (executable-find "autopep8"))
    (error "\"autopep8\" command not found. Install autopep8 with \"pip install autopep8\""))
  (let ((tmpfile (make-temp-file "autopep8" nil ".py"))
        (patchbuf (get-buffer-create "*autopep8 patch*"))
        (errbuf (get-buffer-create "*autopep8 Errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))
    (write-region nil nil tmpfile)
    (if (zerop (apply 'call-process "autopep8" nil errbuf nil
                      (append `("--in-place" ,tmpfile) py-autopep8-options)))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already autopep8ed"))
          (py-autopep8-apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied autopep8"))
      (error "Could not apply autopep8. Check *autopep8 Errors* for details"))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))


;;;###autoload
(defun py-autopep8-before-save ()
  (interactive)
  (when (eq major-mode 'python-mode)
    (condition-case err (py-autopep8)
      (error (message "%s" (error-message-string err))))))


(provide 'py-autopep8)


;;; py-autopep8.el ends here
