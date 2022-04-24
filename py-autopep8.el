;;; py-autopep8.el --- Use autopep8 to beautify a Python buffer -*- lexical-binding: t -*-

;; Copyright (C) 2022  Campbell Barton
;; Copyright (C) 2013-2015, Friedrich Paetzke <f.paetzke@gmail.com>

;; Author: Friedrich Paetzke <f.paetzke@gmail.com>

;; URL: http://paetzke.me/project/py-autopep8.el
;; Keywords: convenience
;; Version: 2016.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Provides the `py-autopep8' command, which uses the external "autopep8"
;; tool to tidy up the current buffer according to Python's PEP8.

;;; Usage

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; To customize the behaviour of "autopep8" you can set the
;; py-autopep8-options e.g.

;;   (setq py-autopep8-options '("--max-line-length=100"))

;;; Code:

(defgroup py-autopep8 nil
  "Use autopep8 to beautify a Python buffer."
  :group 'convenience)

(defcustom py-autopep8-command "autopep8"
  "The location of the autopep8 command (otherwise find in PATH)."
  :group 'py-autopep8
  :type 'string)

(defcustom py-autopep8-options nil
  "Options used for autopep8.

Note that `-' and '--exit-code' are used by default."
  :group 'py-autopep8
  :type '(repeat (string :tag "option")))

;; ---------------------------------------------------------------------------
;; Internal Functions

(defun py-autopep8--buffer-format-impl (stdout-buffer stderr-buffer)
  "Format current buffer using temporary STDOUT-BUFFER and STDERR-BUFFER."
  (when (not (executable-find py-autopep8-command))
    (user-error (format "%s command not found." py-autopep8-command)))

  ;; Set the default coding for the temporary buffers.
  (let ((sentinel-called nil)
        (command-with-args (append (list py-autopep8-command)
                                   py-autopep8-options
                                   (list "-" "--exit-code")))
        (this-buffer-coding buffer-file-coding-system)
        (stderr-as-string nil)

        ;; Set this for `make-process' as there are no files for autopep8
        ;; to use to detect where to read local configuration from,
        ;; it's important the current directory is used to look this up.
        (default-directory (file-name-directory (buffer-file-name))))

    (let ((proc
           (make-process
            :name "autopep8-proc"
            :buffer stdout-buffer
            :stderr stderr-buffer
            :coding (cons this-buffer-coding this-buffer-coding)
            :connection-type 'pipe
            :command command-with-args
            :sentinel
            (lambda (_proc _msg)
              (setq sentinel-called t)

              ;; Assign in the sentinel to prevent "Process .. finished"
              ;; being written to `stderr-buffer' otherwise it's difficult
              ;; to know if there was an error or not since an exit value
              ;; of 2 may be used for invalid arguments as well as to check
              ;; if the buffer was re-formatted.
              (unless (zerop (buffer-size stderr-buffer))
                (with-current-buffer stderr-buffer
                  (setq stderr-as-string (buffer-string))
                  (erase-buffer)))))))

      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc)

      (while (not sentinel-called)
        (accept-process-output))

      (let ((exit-code (process-exit-status proc)))
        (cond
         ((eq exit-code 0)
          ;; No difference.
          nil)
         ((or (not (eq exit-code 2)) stderr-as-string)
          (unless stderr-as-string
            (message "py-autopep8: error output\n%s"
                     (with-current-buffer stderr-buffer
                       (buffer-string))))
          (message "py-autopep8: Command %S failed with exit code %d!"
                   command-with-args exit-code)
          nil)
         (t
          (replace-buffer-contents stdout-buffer)
          t))))))

(defun py-autopep8--buffer-format ()
  "Format the current buffer."
  (let ((stdout-buffer nil)
        (stderr-buffer nil)
        (this-buffer (current-buffer)))
    (with-temp-buffer
      (setq stdout-buffer (current-buffer))
      (with-temp-buffer
        (setq stderr-buffer (current-buffer))
        (with-current-buffer this-buffer
          (py-autopep8--buffer-format-impl stdout-buffer stderr-buffer))))))

;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun py-autopep8-buffer ()
  "Use the \"autopep8\" tool to reformat the current buffer."
  (interactive)
  (py-autopep8--buffer-format)
  ;; Always return nil, continue to save.
  nil)

;;;###autoload
(defun py-autopep8-enable-on-save ()
  "Pre-save hook to be used before running autopep8."
  (interactive)
  (add-hook 'before-save-hook 'py-autopep8-buffer nil t))

(provide 'py-autopep8)
;;; py-autopep8.el ends here
