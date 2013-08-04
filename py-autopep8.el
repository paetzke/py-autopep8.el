;; py-autopep8.el
;;
;; Copyright (C) 2013, Friedrich Paetzke <f.paetzke@gmail.com>


(defun py--apply-rcs-patch (patch-buffer)
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
            (error "invalid rcs patch or internal error in py--apply-rcs-patch"))
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
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (incf line-offset len)
                (kill-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in py--apply-rcs-patch")))))))))


(defun python-fmt ()
  "Formats the current buffer according to the autopep8 tool."
  (interactive)
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
    (if (zerop (call-process "autopep8" nil errbuf nil "--in-place" tmpfile))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already autopep8ed"))
          (py--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied autopep8"))
      (message "Could not apply autopep8. Check errors for details"))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))


(defun python-fmt-before-save ()
  (interactive)
  (when (eq major-mode 'python-mode) (python-fmt)))


(provide 'py-autopep8)
