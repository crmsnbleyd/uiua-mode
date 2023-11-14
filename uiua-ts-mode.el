;;; uiua-ts-mode.el --- Uiua treesiter mode -*- lexical-binding:t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/crmsnbleyd/uiua-mode
;; Keywords: languages, uiua

;;; Commentary:
;; A major mode for Uiua that uses treesitter for parsing

;;; Code:
(require 'uiua-mode)
(require 'treesit)

(defgroup uiua-ts nil
  "Settings for ts-powered Uiua"
  :prefix "uiua-ts-"
  :prefix "uiua-"
  :group 'uiua)

(defcustom uiua-ts-mode-hook nil
  "The hook that is called after starting uiua-ts-mode."
  :type 'hook)

(defvar uiua--ts-indent-rules
  `((uiua
     ((parent-is "program") parent-bol 0)
     ((query ((array (closeCurly)@curly :?
		     (closeBracket)@bracket :?)))
      parent 0)
     ((parent-is "array") parent 2))))

(defvar uiua--ts-font-lock-rules
  (treesit-font-lock-rules
   :language 'uiua
   :feature 'string
   '([(character) (string) (multiLineString)]
     @font-lock-string-face)

   :language 'uiua
   :feature 'number
   '((number) @uiua-number)

   :language 'uiua
   :feature 'monadic-function
   '((function _) @uiua-monadic-function)

   :language 'uiua
   :feature 'monadic-modifier
   '((modifier1 _) @uiua-monadic-modifier)

   :language 'uiua
   :feature 'dyadic-modifier
   '((modifier2 _) @uiua-dyadic-modifier)

   :language 'uiua
   :feature 'ocean-function
   :override t
   '((function [ "⋄" "~" "≊" "≃" "∸"
       "ab" "abyss"
       "de" "deep"
       "ro" "rock"
       "se" "seabed"
       "surface"])
     @uiua-ocean-function)

   :language 'uiua
   :override t
   :feature 'noadic
   '((constant _) @uiua-noadic-or-constant)

   :language 'uiua
   :override t
   :feature 'comment
   '((comment) @font-lock-comment-face)))

(defun uiua--ts-setup ()
  "Setup for uiua treesitter mode."
  (setq-local treesit-font-lock-settings
	      uiua--ts-font-lock-rules)
  (setq-local treesit-simple-indent-rules
	      uiua--ts-indent-rules)
  (setq-local treesit-font-lock-feature-list
	      '((comment string)
		(number)
		(monadic-function
		 monadic-modifier dyadic-modifier
		 ocean-function noadic)))
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode uiua-ts-mode uiua-base-mode "Uiua"
  "Major mode for editing Uiua files.
Uses tree-sitter."
  :group 'uiua
  (add-to-list
   'treesit-language-source-alist
   '(uiua . ("https://github.com/shnarazk/tree-sitter-uiua.git")))
  (setq-local font-lock-defaults nil) ; to let ts do the hard work
  (when (treesit-ready-p 'uiua)
    (treesit-parser-create 'uiua)
    (uiua--ts-setup)))

(provide 'uiua-ts-mode)

;;; uiua-ts-mode.el ends here
