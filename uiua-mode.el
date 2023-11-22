;;; uiua-mode.el --- Uiua integration -*- lexical-binding:t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/crmsnbleyd/uiua-mode
;; Package-Requires: ((emacs "27.1") (reformatter "0.8"))
;; Keywords: languages, uiua
;; Version: 0.0.5

;;; Commentary:
;; `uiua-mode' is a major mode for interacting with
;; and editing the uiua array language

;;; Code:
(require 'face-remap)
(require 'seq)
(require 'reformatter)
(require 'regexp-opt)
(eval-when-compile (require 'rx))

(defgroup uiua nil
  "Major mode for editing the Uiua array language."
  :prefix "uiua-"
  :group 'languages)

(defcustom uiua-command
  "uiua"
  "Default command to use Uiua."
  :type 'string)

(defface uiua-number
  '((t (:inherit font-lock-function-name-face)))
  "Face used for numbers in Uiua.")

(defface uiua-noadic-or-constant
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for noadic functions and builtin contants.")

(defface uiua-monadic-function
  '((t (:inherit font-lock-builtin-face)))
  "Face used for Uiua in-built monadic functions.")

(defface uiua-dyadic-function
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for Uiua in-built dyadic functions.")

(defface uiua-monadic-modifier
  '((t (:inherit font-lock-type-face)))
  "Face used for Uiua in-built monadic modifiers.")

(defface uiua-dyadic-modifier
  '((t (:inherit font-lock-constant-face)))
  "Face used for Uiua in-built dyadic modifiers.")

(defvar uiua--*last-compiled-file* nil
  "Last compiled output of `uiua-standalone-compile'.")

(defun uiua-standalone-compile (arg)
  "Compile standalone executable with uiua stand.
If ARG is nil, prompts user for input and output names."
  (interactive "P")
  (save-buffer)
  (let* ((input-file-name (file-name-nondirectory (buffer-file-name)))
	 (executable-name (or uiua--*last-compiled-file*
			      (file-name-sans-extension input-file-name))))
  (when
   (null arg)
   (setf input-file-name
	 (read-string (format "File to compile (default %s): " input-file-name)
		      nil nil input-file-name))
   (setf executable-name
	 (read-string (format "Output name (default %s): " executable-name)
		      nil nil executable-name)))
  (compile (format "%s stand --name %s %s" uiua-command executable-name input-file-name))
  (setf uiua--*last-compiled-file* executable-name)))

;; Possible enhancement: macro to generate an `rx' form
(defun uiua--generate-keywords (prefix other-letters)
  "Generate all prefix strings of PREFIX ++ OTHER-LETTERS.

For example when called with \"LEN\" and \"GTH\", the generated
regex shall match (LEN LENG LENGT LENGTH)."
  (seq-reduce
   (lambda (word-list ch)
     (cons
      (format "%s%c"
	      (car word-list) ch)
      word-list))
   other-letters
   (list prefix)))

(defun uiua--generate-font-lock-matcher (glyphs &rest word-prefix-suffix-pairs)
  "Create a regex that matches any character in GLYPHS.
In addition, it matches words specified by WORD-PREFIX-SUFFIX-PAIRS such that
if they are a cons cell of the form (PREFIX . SUFFIX), both strings,
it matches any concatenation of the PREFIX and initial substring of SUFFIX,
and if it is a string, that literal string is matched.
If GLYPHS is nil, only the latter behaviour is displayed."
  (concat
   (if glyphs
       (concat (regexp-opt-charset glyphs) "\\|") "")
   (regexp-opt
    (mapcan
     (lambda (item)
       (if (stringp item)
	   (list item)
	 (uiua--generate-keywords (car item) (cdr item))))
     word-prefix-suffix-pairs))))

(defconst uiua--noadic-glyphs
  (list ?⚂ ?η ?π ?τ ?∞))

(defconst uiua--monadic-function-glyphs
  (list ?¬ ?± ?¯ ?⌵ ?√ ?○ ?⌊ ?⌈ ?\⁅
	?⧻ ?△ ?⇡ ?⊢ ?⇌ ?♭ ?¤ ?⋯ ?⍉
	?⍏ ?⍖ ?⊚ ?⊛ ?⊝ ?□ ?⊔))

(defconst uiua--dyadic-function-glyphs
  (list ?- ?= ?≠ ?< ?≤ ?> ?≥ ?+ ?×
	?÷ ?◿ ?ⁿ ?ₙ ?↧ ?↥ ?∠ ?ℂ
	?≍ ?⊟ ?⊂ ?⊏ ?⊡ ?↯ ?☇ ?↙ ?↘
	?↻ ?◫ ?▽ ?⌕ ?∊ ?⊗ ?⍤))

(defconst uiua--monadic-modifier-glyphs
  (list ?/ ?\\ ?∵ ?≡ ?∺ ?⊠ ?⍥
	?⊕ ?⊜ ?⊐ ?⍘ ?⋅ ?⟜ ?⊙ ?∩))

(defconst uiua--dyadic-modifier-glyphs
  (list ?⊃ ?⊓ ?⍜ ?⍢ ?⬚ ?∧ ?⍣))

(defconst uiua--primitives
  (list
   (list ?. ?, ?: ?\; ?∘ ?⸮)
   uiua--monadic-function-glyphs
   uiua--dyadic-function-glyphs
   uiua--monadic-modifier-glyphs
   uiua--dyadic-modifier-glyphs
   uiua--noadic-glyphs))

;; TODO use matching functions instead of regexes
;; in order to allow lowercase variables
(defvar uiua--font-lock-defaults
  `(((,(rx (seq upper (* alpha))) . 'default)
     (,(uiua--generate-font-lock-matcher
	uiua--monadic-modifier-glyphs
	"dip"
	'("rer" . "ank")
	'("pic" . "k")    ;; conflict with pi
	'("res" . "hape") ;; conflict with reach
	'("dro" . "p")	  ;; conflict with 'dr': dip reach
	'("rot" . "ate")  ;; conflict with rock
	'("sel" . "ect")  ;; conflict with seabed
	'("div" . "ide"))
      . 'uiua-monadic-modifier)
     (,(uiua--generate-font-lock-matcher
	uiua--monadic-function-glyphs
	"&ims" ;; conflict with &i
	'("abs" . "olute"); conflict with abbyss
	'("des" . "hape"); conflict with deep
	'("rev" . "erse"); conflict with reach
	'("ris" . "e") ;; conflict with `ri': reach identity
	'("rou" . "nd")) ; conflict with rock
      . 'uiua-monadic-function)
     ;; next three regices are shortcuts to match
     ;; [gdri]{2,} as planet notation
     ("i?\\([gdr][gdr]+\\)i?" 1 'uiua-monadic-modifier )
     ("i\\([gdr]\\)" 1 'uiua-monadic-modifier )
     ("\\([gdr]\\)i" 1 'uiua-monadic-modifier )
     (,(rx
        (seq
	 (opt (any "`¯"))
	 (one-or-more (any "0-9"))
	 (opt (group "." (one-or-more (any "0-9"))))))
      . 'uiua-number)
     (,(uiua--generate-font-lock-matcher
	uiua--monadic-function-glyphs
	"not"
	"fix"
	"`"
	'("bit" . "s")
	'("cei" . "ling")
	'("fal" . "l")
	'("fir" . "st")
	'("flo" . "or")
	'("len" . "gth")
	'("rang" . "e")
	'("sha" . "pe")
	'("sig" . "n")
	'("sin" . "e")
	'("sqr" . "t")
	'("tran" . "spose"))
      . 'uiua-monadic-function)
     (,(uiua--generate-font-lock-matcher
	uiua--dyadic-function-glyphs
	"equals" "add"
	"subtract" "deal"
	"send" "&fwa"
	"&imd" "&ime"
	"&tcpsrt"
	"&tcpswt"
	"!=" "*"
	"%"
	'("fin" . "d")
	'("joi" . "n")
	'("kee" . "p")
	'("les" . "s")
	'("tak" . "e")
	'("mat" . "ch")
	'("pow" . "er")
	'("rege" . "x")
	'("ass" . "ert")
	'("cou" . "ple")
	'("mem" . "ber")
	'("ind" . "exof")
	'("gre" . "ater")
	'("max" . "imum")
	'("min" . "imum")
	'("mod" . "ulus")
	'("win" . "dows")
	'("ata" . "ngent")
	'("mul" . "tiply")
	'("log" . "arithm")
	'("comp" . "lex"))
      . 'uiua-dyadic-function)
     (,(concat
        "\\(^\\|[^&a-zA-Z]\\)\\("
        (uiua--generate-font-lock-matcher
	 uiua--noadic-glyphs
	 (regexp-opt '("pi" "i" "e")))
        "\\)\\([^&a-zA-Z]\\|$\\)")
      2 'uiua-noadic-or-constant)
     (,(uiua--generate-font-lock-matcher
	uiua--dyadic-modifier-glyphs
	'("fol" . "d")
	'("for" . "k"))
      0 'uiua-dyadic-modifier t)
     (,(rx (or (seq "$" (* (not "\n"))) (and "@" (or "\\\\" (not "\\")))))
      0 font-lock-string-face t))
    nil nil nil))

(defvar uiua--syntax-table
  (let ((table (make-syntax-table)))
    ;; add all primitives as punctuation
    ;; so that they are parsed separately
    (dolist (i uiua--primitives)
      (dolist (j i)
        (modify-syntax-entry j "." table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?` "_ " table)
    table)
  "Syntax table for `uiua-mode'.")

;;;###autoload (autoload 'uiua-format-buffer "uiua-mode" nil t)
;;;###autoload (autoload 'uiua-format-on-save-mode "uiua-mode" nil t)
(reformatter-define uiua-format
  :program uiua-command
  :args (list "fmt" "--to-stdout" input-file)
  :stdin nil
  :stdout t
  :input-file (reformatter-temp-file)
  :lighter " UiuaFmt")

;;;###autoload
(define-derived-mode uiua-base-mode prog-mode "Uiua"
  "Generic Major mode for editing Uiua files."
  :syntax-table uiua--syntax-table
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*"))

(add-hook 'uiua-base-mode-hook 'uiua-format-on-save-mode)

;;;###autoload
(define-derived-mode uiua-mode uiua-base-mode "Uiua"
  "Major mode for editing Uiua files."
  (add-to-list 'hs-special-modes-alist '(uiua-mode "{\\|" "}\\|]" "#"))
  (setq-local font-lock-defaults uiua--font-lock-defaults))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ua\\'" . uiua-mode))

(provide 'uiua-mode)

;;; uiua-mode.el ends here
