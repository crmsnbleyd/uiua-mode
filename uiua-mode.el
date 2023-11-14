;;; uiua-mode.el --- Uiua integration -*- lexical-binding:t -*-
;;; Version: 0.0.5

;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/crmsnbleyd/uiua-mode
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, uiua

;;; Commentary:
;; `uiua-mode' is a major mode for interacting with
;; and editing the uiua array language

;;; Code:
(defgroup uiua nil
  "Major mode for editing the Uiua array language."
  :prefix "uiua-"
  :group 'languages)

(defcustom uiua-mode-hook nil
  "The hook that is called after loading `uiua-mode'."
  :type 'hook
  :group 'uiua)

(defcustom uiua-command
  (cond ((executable-find "uiua") "uiua")
	(t "/usr/bin/uiua"))
  "Default command to use Uiua."
  :group 'uiua-mode
  :version "27.1"
  :type 'string)

(defface uiua-number
  '((t (:inherit font-lock-function-name-face)))
  "Face used for numbers in Uiua."
  :group 'uiua)

(defface uiua-noadic-or-constant
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for noadic functions and builtin contants."
  :group 'uiua)

(defface uiua-monadic-function
  '((t (:inherit font-lock-builtin-face)))
  "Face used for Uiua in-built monadic functions."
  :group 'uiua)

(defface uiua-dyadic-function
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for Uiua in-built dyadic functions."
  :group 'uiua)

(defface uiua-ocean-function
  '((t (:inherit font-lock-constant-face)))
  "Face used for Uiua ocean functions."
  :group 'uiua)

(defface uiua-monadic-modifier
  '((t (:inherit font-lock-type-face)))
  "Face used for Uiua in-built monadic modifiers."
  :group 'uiua)

(defface uiua-dyadic-modifier
  '((t (:inherit font-lock-constant-face)))
  "Face used for Uiua in-built dyadic modifiers."
  :group 'uiua)

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

(defconst uiua--noadic-glyphs
  (list ?⚂ ?η ?π ?τ ?∞))

(defconst uiua--monadic-function-glyphs
  (list ?¬ ?± ?¯ ?⌵ ?√ ?○ ?⌊ ?⌈ ?\⁅
	?⧻ ?△ ?⇡ ?⊢ ?⇌ ?♭ ?⋯ ?⍉
	?⍏ ?⍖ ?⊚ ?⊛ ?⊝ ?□ ?⊔))

(defconst uiua--dyadic-function-glyphs
  (list ?- ?= ?≠ ?< ?≤ ?> ?≥ ?+ ?×
	?÷ ?◿ ?ⁿ ?ₙ ?↧ ?↥ ?∠ ?ℂ
	?≍ ?⊟ ?⊂ ?⊏ ?⊡ ?↯ ?↙ ?↘
	?↻ ?◫ ?▽ ?⌕ ?∊ ?⊗ ?⍤))

(defconst uiua--ocean-function-glyphs
  (list ?⋄ ?~ ?≊ ?≃ ?∸))

(defconst uiua--monadic-modifier-glyphs
  (list ?/ ?\\ ?∵ ?≡ ?∺ ?≐ ?⊞ ?⊠ ?⍥
	?⊕ ?⊜ ?⊐ ?⍘ ?⋅ ?⟜ ?⊙ ?∩))

(defconst uiua--dyadic-modifier-glyphs
  (list ?⊃ ?⊓ ?⍜ ?⍢ ?⬚ ?≑ ?∧ ?◳ ?? ?⍣))

(defconst uiua--primitives
  (list
   (list ?. ?, ?: ?\; ?∘ ?⸮)
   uiua--monadic-function-glyphs
   uiua--dyadic-function-glyphs
   uiua--ocean-function-glyphs
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
	uiua--ocean-function-glyphs
	'("ab" . "yss")
	'("de" . "ep")
	'("ro" . "ck")
	'("se" . "abed")
	"surface")
      . 'uiua-ocean-function)
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
	(rx-to-string '(seq "fo"
			    (or (seq "r" (zero-or-one "k"))
				(seq "l" (zero-or-one "d"))))))
      0 'uiua-dyadic-modifier t)
     (,(rx (or "$" (and "@" (or "\\\\" (not "\\")))))
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

(defun uiua--buffer-apply-command (cmd &optional args)
  "Execute shell command CMD with ARGS and current buffer as input and output.
Use buffer as input and replace the whole buffer with the
output.  If CMD fails the buffer remains unchanged."
  (set-buffer-modified-p t)
  (let* ((out-file (make-temp-file "uiua-output"))
	 (err-file (make-temp-file "uiua-error"))
	 (coding-system-for-read 'utf-8)
	 (coding-system-for-write 'utf-8))
    (unwind-protect
	(let ((curr-file (buffer-file-name))
	      (curr-bufname (buffer-name))
	      (message-log-max nil))
	  (set-visited-file-name out-file)
	  (with-temp-message "" (save-buffer))
	  (set-visited-file-name curr-file)
	  (rename-buffer curr-bufname))
	(let* ((_errcode
		(apply 'call-process cmd nil
		       `(,(get-buffer-create "*uiua-output*") ,err-file)
		       nil
		       (append args (list out-file))))
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

(defun uiua-process-load-file ()
  "Load the file currently open in buffer."
  (interactive))

(defun uiua-format-buffer ()
  "Format buffer using the in-built formatter."
  (interactive)
  (let ((original-text-scale (if text-scale-mode text-scale-mode-amount 0)))
    (when (called-interactively-p "interactive")
      (unless uiua-command
	(error "Uiua binary not found, please set `uiua-command'"))
      (message "Autoformatting code with %s fmt."
	       uiua-command))
    (uiua--buffer-apply-command uiua-command (list "fmt"))
    (text-scale-set original-text-scale)))

(defun uiua--replace-region (beg end replacement)
  "Replace text in BUFFER in region (BEG END) with REPLACEMENT."
    (save-excursion
      (goto-char (point-min))
      (insert replacement)
      (delete-region beg end)))

;;;###autoload
(define-derived-mode uiua-base-mode prog-mode "Uiua"
  "Generic Major mode for editing Uiua files."
  :syntax-table uiua--syntax-table
  :group 'uiua
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*"))

;;;###autoload
(define-derived-mode uiua-mode uiua-base-mode "Uiua"
  "Major mode for editing Uiua files."
  :group 'uiua
  (add-to-list 'hs-special-modes-alist '(uiua-mode "{\\|" "}\\|]" "#"))
  (setq-local font-lock-defaults uiua--font-lock-defaults))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ua\\'" . uiua-mode))

(provide 'uiua-mode)

;;; uiua-mode.el ends here
