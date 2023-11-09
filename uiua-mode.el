;;; uiua-mode.el --- Uiua integration for Emacs -*- lexical-binding:t -*-
;;; Version: 0.0.1

;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/crmsnbleyd/uiua-mode
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, uiua

;;; Commentary:
;; `uiua-mode' is an emacs major mode for interacting with
;; and editing the uiua array language

;;; Code:
(message "Nothing yet")

(defcustom uiua-command
  (cond ((executable-find "uiua") "uiua")
	(t "python3"))
  "Default command to use Uiua."
  :version "27.1"
  :type 'string)

;; credits to https://github.com/haskell/haskell-mode
(defun uiua-mode--buffer-apply-command (cmd &optional args)
  "Execute shell command CMD with ARGS and current buffer as input and output.
Use buffer as input and replace the whole buffer with the
output.  If CMD fails the buffer remains unchanged."
  (set-buffer-modified-p t)
  (let* ((out-file (make-temp-file "uiua-output"))
	 (err-file (make-temp-file "uiua-error"))
	 (coding-system-for-read 'utf-8)
	 (coding-system-for-write 'utf-8))
    (unwind-protect
	(let* ((_errcode
		(apply 'call-process-region (point-min) (point-max) cmd nil
		       `((:file ,out-file) ,err-file)
		       nil args))
	       (err-file-empty-p
		(equal 0 (nth 7 (file-attributes err-file))))
	       (out-file-empty-p
		(equal 0 (nth 7 (file-attributes out-file)))))
	  (if err-file-empty-p
	      (if out-file-empty-p
		  (message "Error: %s produced no output and no errors, buffer is unchanged" cmd)
		;; Command successful, insert file with replacement to preserve
		;; markers.
		(insert-file-contents out-file nil nil nil t))
	    (progn
	      ;; non-null stderr, command must have failed
	      (with-current-buffer
		  (get-buffer-create "*uiua-mode*")
		(insert-file-contents err-file)
		(buffer-string))
	      (message "Error: %s ended with errors, leaving buffer alone, see *uiua-mode* buffer for stderr" cmd)
	      (with-temp-buffer
		(insert-file-contents err-file)
		;; use (warning-minimum-level :debug) to see this
		(display-warning cmd
				 (buffer-substring-no-properties (point-min) (point-max))
				 :debug)))))
      (ignore-errors
	(delete-file err-file))
      (ignore-errors
	(delete-file out-file)))))

(defun uiua-mode-format-buffer ()
  "Format buffer using the in-built formatter"
  (interactive)
  (unless uiua-command
    (error "Uiua binary not found, please set `uiua-command'"))
  (when (interactive-p) (message "Autoformatting code with %s fmt."
				 uiua-command))
  (uiua-mode--buffer-apply-command uiua-command "fmt"))

(defun uiua-mode--replace-region (beg end replacement)
  "Replace text in BUFFER in region (BEG END) with REPLACEMENT."
    (save-excursion
      (goto-char (point-min))
      (insert replacement)
      (delete-region beg end)))

(provide 'uiua-mode)

;;; uiua-mode.el ends here
