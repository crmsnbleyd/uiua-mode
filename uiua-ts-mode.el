;;; uiua-ts-mode.el --- Uiua treesiter mode -*- lexical-binding:t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/crmsnbleyd/uiua-mode
;; Keywords: languages, uiua
;; Package-Requires: ((emacs "29.1") (uiua-mode "0.0.5"))
;; Version: 0.0.5

;;; Commentary:
;; A major mode for Uiua that uses treesitter for parsing

;;; Code:
(require 'uiua-mode)
(require 'treesit)

(defgroup uiua-ts nil
  "Settings for ts-powered Uiua."
  :prefix "uiua-ts-"
  :prefix "uiua-"
  :group 'uiua)

(defvar uiua-ts--indent-rules
  `((uiua
     ((parent-is "program") parent-bol 0)
     ((query ((array (closeCurly)@curly :?
		     (closeBracket)@bracket :?)))
      parent 0)
     ((query ((inlineFunction
	       (closeParen)@paren :?))
             parent 0)
      ((parent-is "inlineFunction") parent 2)
      ((parent-is "array") parent 2)))))

(defvar uiua-ts--font-lock-rules
  (treesit-font-lock-rules
   :language 'uiua
   :feature 'string
   '([(character) (string) (multiLineString)]
     @font-lock-string-face)

   :language 'uiua
   :feature 'number
   '((number) @uiua-number)

   :language 'uiua
   :feature 'default
   :override t
   '((function [ "." "∘" "id" "identity"])
     @default)

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
   :override t
   :feature 'noadic
   '((constant _) @uiua-noadic-or-constant)

   :language 'uiua
   :override t
   :feature 'comment
   '((comment) @font-lock-comment-face)))

(defun uiua-ts--setup ()
  "Setup for uiua treesitter mode."
  (setq-local treesit-font-lock-settings
	      uiua-ts--font-lock-rules)
  (setq-local treesit-simple-indent-rules
	      uiua-ts--indent-rules)
  (setq-local treesit-font-lock-feature-list
	      '((comment default string)
		(number)
		(monadic-function
		 monadic-modifier dyadic-modifier
		 noadic)))
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
    (uiua-ts--setup)))

(provide 'uiua-ts-mode)

;;; uiua-ts-mode.el ends here
