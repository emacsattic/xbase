;; xbase.el --- A mode for editing xBase programs.
;; $Id$

;; Copyright (C) 2002, Mike Romberg <romberg@fsl.noaa.gov>

;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; The indentation engine used here is a version of the one
;; found in Fred White's <fwhite@world.std.com> basic-mode.el.
;; It has been modified slightly to work with xbase code.

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

(defgroup xbase nil
  "Mode for editing Xbase source."
  :group 'languages
  :prefix "xbase-")

(defcustom xbase-mode-indent 3
  "*Default indentation per nesting level"
  :type 'integer
  :group 'xbase)

(defun xbase-indent-line (&optional whole-exp)
  "Indent current line as xBase code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (xbase-indent-level))
	(pos (- (point-max) (point)))
        (beg (progn
               (beginning-of-line)
               (point))))
    (skip-chars-forward " \t")
    (unless (zerop (- indent (current-column)))
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

(defconst xbase-defun-start-regexp
  "^[ \t]*\\([Pp]rocedure\\|[Ff]unction\\)[ \t]+\\(\\w+\\)[ \t]*(?")

(defconst xbase-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Ss]ub\\|[Ff]unction\\|[Pp]roperty\\|[Tt]ype\\)")


;; Includes the compile-time #if variation.
(defconst xbase-if-regexp "^[ \t]*#?[Ii]f")
(defconst xbase-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst xbase-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst xbase-continuation-regexp "^.*\\_[ \t]*$")
(defconst xbase-label-regexp "^[ \t]*[a-zA-Z0-9_]+:$")

(defconst xbase-select-regexp "^[ \t]*do[ \t]+case")
(defconst xbase-case-regexp "^[ \t]*case")
(defconst xbase-select-end-regexp "^[ \t]*[Ee]ndcase")

(defconst xbase-for-regexp "^[ \t]*[Ff]or")
(defconst xbase-next-regexp "^[ \t]*[Nn]ext")

(defconst xbase-do-regexp "^[ \t]*[Dd]o[ \t]*[Ww]hile")
(defconst xbase-loop-regexp "^[ \t]*[Dd]o[ \t]*[Ww]hile")

(defconst xbase-while-regexp "^[ \t]*[Dd]o[ \t]*[Ww]hile")
(defconst xbase-wend-regexp "^[ \t]*enddo")

(defconst xbase-with-regexp "^[ \t]*[Ww]ith")
(defconst xbase-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith")

(defconst xbase-blank-regexp "^[ \t]*$")
(defconst xbase-comment-regexp "^[ \t]*\\s<.*$")


(defun xbase-find-matching-while ()
  (xbase-find-matching-stmt xbase-while-regexp xbase-wend-regexp))

(defun xbase-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))	; previous-line depends on goal column
  (while (and (not (bobp))
	      (or (looking-at xbase-blank-regexp)
		  (looking-at xbase-comment-regexp)))
    (forward-line -1)))

(defun xbase-find-original-statement ()
  ;; If the current line is a continuation from the previous, move
  ;; back to the original stmt.
  (let ((here (point)))
    (xbase-previous-line-of-code)
    (while (and (not (bobp))
		(looking-at xbase-continuation-regexp))
      (setq here (point))
      (xbase-previous-line-of-code))
    (goto-char here)))

(defun xbase-find-matching-stmt (open-regexp close-regexp)
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (xbase-previous-line-of-code)
      (xbase-find-original-statement)
      (cond ((looking-at close-regexp)
	     (setq level (+ level 1)))
	    ((looking-at open-regexp)
	     (setq level (- level 1)))))))

(defun xbase-find-matching-if ()
  (xbase-find-matching-stmt xbase-if-regexp xbase-endif-regexp))

(defun xbase-find-matching-select ()
  (xbase-find-matching-stmt xbase-select-regexp xbase-select-end-regexp))

(defun xbase-find-matching-for ()
  (xbase-find-matching-stmt xbase-for-regexp xbase-next-regexp))

(defun xbase-indent-level ()
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((looking-at xbase-defun-start-regexp)
             0)

	    ;; The outdenting stmts, which simply match their original.
	    ((or (looking-at xbase-else-regexp)
		 (looking-at xbase-endif-regexp))
             (message "else,endif")
	     (xbase-find-matching-if)
	     (current-indentation))

	    ;; All the other matching pairs act alike.
	    ((looking-at xbase-next-regexp) ; for/next
	     (xbase-find-matching-for)
	     (current-indentation))

	    ((looking-at xbase-wend-regexp) ; while/wend
	     (xbase-find-matching-while)
	     (current-indentation))

	    ((looking-at xbase-select-end-regexp) ; select case/end select
	     (xbase-find-matching-select)
	     (current-indentation))

	    ;; A case of a select is somewhat special.
	    ((looking-at xbase-case-regexp)
	     (xbase-find-matching-select)
	     (+ (current-indentation) xbase-mode-indent))


	    (t
	     ;; Other cases which depend on the previous line.
	     (xbase-previous-line-of-code)
             (cond

              (t
               (message "other")
               (xbase-find-original-statement)
               (let ((indent (current-indentation)))
                 ;; All the various +indent regexps.
                 (cond ((looking-at xbase-defun-start-regexp)
                        (+ indent xbase-mode-indent))

                       ((or (looking-at xbase-if-regexp)
                            (looking-at xbase-else-regexp))
                        (+ indent xbase-mode-indent))

                       ((or (looking-at xbase-select-regexp)
                            (looking-at xbase-case-regexp))
                        (+ indent xbase-mode-indent))

                       ((or (looking-at xbase-do-regexp)
                            (looking-at xbase-for-regexp)
                            (looking-at xbase-while-regexp)
                            (looking-at xbase-with-regexp))
                        (+ indent xbase-mode-indent))

                       (t
                        ;; By default, just copy indent from prev line.
                        indent))))))))))

(defvar xbase-mode-hook nil
  "*List of functions to call when enterning xbase mode.")

(defvar xbase-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Insert")))
    (define-key map "\t" 'xbase-indent-line)
    map)
    "Keymap for xbase major mode.")

(if xbase-mode-map
    nil
  (setq xbase-mode-map (make-keymap)))

(defvar xbase-keywords
  (concat "\\<\\("
          "if\\|endif\\|else\\|do\\|while\\|enddo\\|for\\|next\\|goto\\|"
          "begin\\|sequence\\|on\\|"
          "say\\|clear\\|to\\|set\\|case\\|endcase\\|"
          "return\\|procedure\\|function\\|parameters\\|"
          "from"
          "\\)\\>"))
(defvar xbase-builtins
  (concat "\\<\\("
          "use\\|seek\\|index\\|close\\|save\\|restore\\|select\\|read\\|"
          "sele\\|exit"
          "\\)\\>"))
;; font-lock-function-name-face
;; font-lock-builtin-face
;; font-lock-variable-name-face
;; font-lock-constant-face
;; font-lock-type-face
(defvar xbase-font-lock-keywords
  `(("^\\s-*\\*.*" 0 font-lock-comment-face t)
    ("&&.*" 0 font-lock-comment-face t)
    ("\\<\\(public\\|private\\|declare\\)\\>" . font-lock-type-face)
    (,xbase-builtins . font-lock-builtin-face)
    ("\\.\\(f\\|t\\)\\." 1 font-lock-constant-face)
    (,xbase-keywords . font-lock-keyword-face)
    ("\\<\\(function\\|procedure\\)\\>\\s-\\<\\(.*\\)\\>"
     2 font-lock-function-name-face t)
    (,(concat "\\<\\(public\\|private\\|declare\\|use\\|close\\|from\\)\\>"
              "\\s-\\<\\(.*\\)\\>") 2 font-lock-variable-name-face)
    ("\'[^\n]*\'" 0 font-lock-string-face)
    "Default font-lock keywords for xbase mode."))

(defun xbase-mode ()
  "Major mode for editing xbase files."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'xbase-mode)
  (setq mode-name "xBase")
  (setq indent-line-function 'xbase-indent-line)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'xbase-indent-level)
  (setq font-lock-defaults '(xbase-font-lock-keywords nil t))
  (font-lock-mode)
  (run-hooks 'xbase-mode-hook))

(provide 'xbase)

;;; xbase.el ends here.