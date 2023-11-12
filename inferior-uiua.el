;;; inferior-uiua.el --- Uiua integration for Emacs -*- lexical-binding:t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/crmsnbleyd/uiua-mode
;; Keywords: languages, uiua

;;; Commentary:
;; A major mode for the uiua inferior process buffer

;;; Code:
(require 'comint)
(require 'uiua-mode)

(defgroup uiua-inferior nil
  "Settings for repl interaction via `inferior-uiua-mode'."
  :prefix "uiua-inferior-"
  :prefix "uiua-"
  :group 'uiua)

(defcustom uiua-inferior-hook nil
  "The hook that is called after starting uiua-inferior."
  :type 'hook)

(provide 'inferior-uiua)

;;; inferior-uiua.el ends here
