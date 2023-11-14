;;; uiua-ts-mode.el --- Uiua treesiter mode -*- lexical-binding:t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/crmsnbleyd/uiua-mode
;; Keywords: languages, uiua

;;; Commentary:
;; A major mode for Uiua that uses treesitter for parsing

;;; Code:
(require 'uiua-mode)

(defgroup uiua-ts nil
  "Settings for ts-powered Uiua"
  :prefix "uiua-ts-"
  :prefix "uiua-"
  :group 'uiua)

(defcustom uiua-ts-mode-hook nil
  "The hook that is called after starting uiua-ts-mode."
  :type 'hook)

;;;###autoload
(define-derived-mode uiua-ts-mode uiua-base-mode "Uiua"
  "Major mode for editing Uiua files.
Uses tree-sitter."
  :group 'uiua)

(provide 'uiua-ts-mode)

;;; uiua-ts-mode.el ends here
