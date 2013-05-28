;; py-autopep8.el
;;
;; Copyright (C) 2013, Friedrich Paetzke <f.paetzke@gmail.com>


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
          (go--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied autopep8"))
      (message "Could not apply autopep8. Check errors for details"))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))


(defun python-fmt-before-save ()
  (interactive)
  (when (eq major-mode 'python-mode) (python-fmt)))


(provide 'py-autopep8)
