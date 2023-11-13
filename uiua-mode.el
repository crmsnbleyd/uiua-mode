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

(defconst uiua--monadic-function-glyphs
  (list ?¬ ?± ?¯ ?⌵ ?√ ?○ ?⌊ ?⌈ ?\⁅
	?⧻ ?△ ?⇡ ?⊢ ?⇌ ?♭ ?⋯ ?⍉
	?⍏ ?⍖ ?⊚ ?⊛ ?⊝ ?□ ?⊔))

;; note: here, - should come first
;; because we create a regex from this list
;; and if it comes in between, we have a range
;; in character class, e.g [a-z]
;; while [-az] matches only `-' `a' `z'
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
   (list ?⚂ ?η ?π ?τ ?∞)))

(defvar uiua--font-lock-defaults
  `(((,(rx (or "$" (and "@" (or "\\\\" (not "\\")))))
      . font-lock-string-face)
     (,(rx (seq upper (* alpha))) . 'default)
     ;; so that 'greater', meaning '≥' does
     ;; not collide with 'gr', meaning '⋅⟜'
     (,(uiua--generate-font-lock-matcher nil '("gre" . "ater"))
      . 'uiua-dyadic-function)
     (,(uiua--generate-font-lock-matcher
	nil '("ris" . "e")) ;; conflict with `ri': reach identity
      . 'uiua-monadic-function)
     (,(uiua--generate-font-lock-matcher
	nil
	"dip" '("div" . "ide")
	'("sel" . "ect") ;; conflict with seabed
	'("pic" . "k") ;; conflict with pi
	'("res" . "hape") ;; conflict with reach
	'("dro" . "p") ;; conflict with 'dr': dip reach
	'("rot" . "ate") ;; conflict with rock
	'("ind" . "exof"))
      . 'uiua-monadic-modifier)
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
	'("sig" . "n")
	'("abs" . "olute"); conflict with abbyss
	'("sqr" . "t")
	'("sin" . "e")
	'("flo" . "or")
	'("cei" . "ling")
	'("rou" . "nd") ; conflict with rock
	'("len" . "gth")
	'("sha" . "pe")
	'("rang" . "e")
	'("fir" . "st")
	'("rev" . "erse"); conflict with reach
	'("des" . "hape"); conflict with deep
	'("bit" . "s")
	'("tran" . "spose")
	'("fal" . "l")
	)
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
	"equals"
	"add"
	"subtract"
	"!="
	"*"
	"%"
	'("les" . "s")
	'("mul" . "tiply")
	'("mod" . "ulus")
	'("pow" . "er")
	'("log" . "arithm")
	'("min" . "imum")
	'("max" . "imum")
	'("ata" . "ngent")
	'("comp" . "lex")
	'("mat" . "ch")
	'("cou" . "ple")
	'("joi" . "n")
	'("tak" . "e")
	'("win" . "dows")
	'("kee" . "p")
	'("fin" . "d")
	'("mem" . "ber"))
      . 'uiua-dyadic-function)
     (,(uiua--generate-font-lock-matcher
	uiua--monadic-modifier-glyphs)
      . 'uiua-monadic-modifier)
     (,(concat "[" uiua--dyadic-modifier-glyphs "]") . 'uiua-dyadic-modifier))
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
(defun uiua--generate-keyword-regex (prefix other-letters)
  "Generate a valid regex string.
When given a string PREFIX and OTHER-LETTERS, generates a
regex string that matches all words starting with PREFIX
and the rest of the word being any initial sublist of OTHER-LETTERS.

For example when called with \"LEN\" and \"GTH\", the generated
regex shall match (LEN LENG LENGT LENGTH)."
  (let ((suffix-length (length other-letters))
	(res (list)))
    (dotimes (_ suffix-length) (setf res (cons "\\)?" res)))
    ;; loop across characters from OTHER-LETTERS in reverse
    (while (> suffix-length 0)
      (setf res
	    (cons
	     (format "\\(%c"
		     (aref other-letters
			   (cl-decf suffix-length)))
	     res)))
    (apply 'concat prefix res)))

(defun uiua--generate-font-lock-matcher (glyphs &rest word-prefix-suffix-pairs)
  "Create a regex that matches any character in GLYPHS.
In addition, it matches words specified by WORD-PREFIX-SUFFIX-PAIRS such that
if they are a cons cell of the form (PREFIX . SUFFIX), both strings,
it matches any concatenation of the PREFIX and initial substring of SUFFIX,
and if it is a string, that literal string is matched.
If GLYPHS is nil, only the latter behaviour is displayed."
  (string-remove-suffix
   "\\|"
   (let ((pair-matching-regex
	  (mapcar
	   (lambda (pair-or-string)
	     (concat
	      (if (consp pair-or-string)
		  (uiua--generate-keyword-regex
		   (car pair-or-string) (cdr pair-or-string))
		pair-or-string)
	      "\\|"))
	   word-prefix-suffix-pairs)))
     (if glyphs
	 (apply 'concat "[" glyphs "]\\|" pair-matching-regex)
         (apply 'concat pair-matching-regex)))))

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
(define-derived-mode uiua-mode prog-mode "Uiua"
  "Major mode for editing Uiua files."
  :syntax-table uiua--syntax-table
  :group 'uiua
  (setq-local font-lock-defaults uiua--font-lock-defaults))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ua\\'" . uiua-mode))

(provide 'uiua-mode)

;;; uiua-mode.el ends here
