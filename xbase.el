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
  "^[ \t]*\\(procedure\\|function\\)[ \t]+\\(\\w+\\)[ \t]*(?")

(defconst xbase-defun-end-regexp
  "^[ \t]*end")                         ; TODO: not used at the moment, will
					; probably need expanding.


;; Includes the compile-time #if variation.
(defconst xbase-if-regexp "^[ \t]*#?if")
(defconst xbase-else-regexp "^[ \t]*#?else\\(if\\)?")
(defconst xbase-endif-regexp "[ \t]*#?end[ \t]*if")

(defconst xbase-continuation-regexp "^.*\\_[ \t]*$") ; TODO: What is this for?

(defconst xbase-do-case-regexp "^[ \t]*do[ \t]+case")
(defconst xbase-case-regexp "^[ \t]*\\(case\\|otherwise\\)")
(defconst xbase-do-case-end-regexp "^[ \t]*endcase")

(defconst xbase-for-regexp "^[ \t]*for")
(defconst xbase-next-regexp "^[ \t]*next")

(defconst xbase-do-regexp "^[ \t]*do[ \t]*while") ; TODO: What is this for?
(defconst xbase-loop-regexp "^[ \t]*do[ \t]*while") ; TODO: What is this for?

(defconst xbase-while-regexp "^[ \t]*do[ \t]*while")
(defconst xbase-wend-regexp "^[ \t]*enddo")

(defconst xbase-with-regexp "^[ \t]*with") ; TODO: Clipper/harbour has no "with".
(defconst xbase-end-with-regexp "^[ \t]*end[ \t]+with")

(defconst xbase-blank-regexp "^[ \t]*$")
(defconst xbase-comment-regexp "^[ \t]*\\s<.*$")


(defun xbase-find-matching-while ()
  (xbase-find-matching-stmt xbase-while-regexp xbase-wend-regexp))

(defun xbase-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
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

(defun xbase-find-matching-do-case ()
  (xbase-find-matching-stmt xbase-do-case-regexp xbase-do-case-end-regexp))

(defun xbase-find-matching-for ()
  (xbase-find-matching-stmt xbase-for-regexp xbase-next-regexp))

(defun xbase-indent-level ()
  (save-excursion
    (beginning-of-line)
    (let ((case-fold-search t))         ; Case insensitive search.
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
            
            ((looking-at xbase-do-case-end-regexp) ; do case/end case
             (xbase-find-matching-do-case)
             (current-indentation))
            
            ;; A case of a case is somewhat special.
            ((looking-at xbase-case-regexp)
             (xbase-find-matching-do-case)
             (+ (current-indentation) xbase-mode-indent))
            
            
            (t
             ;; Other cases which depend on the previous line.
             (xbase-previous-line-of-code)
             (cond (t
                    (message "other")
                    (xbase-find-original-statement)
                    (let ((indent (current-indentation)))
                      ;; All the various +indent regexps.
                      (cond ((looking-at xbase-defun-start-regexp)
                             (+ indent xbase-mode-indent))
                            
                            ((or (looking-at xbase-if-regexp)
                                 (looking-at xbase-else-regexp))
                             (+ indent xbase-mode-indent))
                            
                            ((or (looking-at xbase-do-case-regexp)
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

;; font-lock-function-name-face "Face name to use for function names.")
;; font-lock-comment-face "Face name to use for comments."

;; font-lock-string-face  "Face name to use for strings.")
;; font-lock-keyword-face "Face name to use for keywords.")
;; font-lock-builtin-face "Face name to use for builtins.")
;; font-lock-variable-name-face "Face name to use for variable names.")
;; font-lock-type-face       "Face name to use for type and class names.")
;; font-lock-constant-face   "Face name to use for constant and label names.")
;; font-lock-warning-face         "Face name to use for things that should stand out.")
;; font-lock-reference-face  "This variable is obsolete.  Use font-lock-constant-face.")

(defvar xbase-font-lock-keywords
  `(("\\<\\(function\\|procedure\\)\\>\\s-\\<\\(\\w*\\)\\>"
     2 font-lock-function-name-face t)

    ;; Fontify function macro names.
    ("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)

    ;; Fontify symbol names in #elif or #if ... defined preprocessor directives
    ("^#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t)))

    ;; Fontify otherwise as symbol names, and the preprocessor directive names.
    ("^#[ \t]*\\(\\sw+\\)\\>[ \t!]*\\(\\sw+\\)?"
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t))

    ;; Comments
    ("\\(\\*\\|//\\|&&\\).*$" 0 font-lock-comment-face t)
    ;; This does not yet work on multi line comments
    ("/\\*.*\\*/" 0 font-lock-comment-face t)

    ("\\<\\(public\\|private\\|local\\|static\\)\\>" . font-lock-type-face)

    (,xbase-builtins . font-lock-builtin-face)

    ("\\.\\(f\\|t\\)\\." 0 font-lock-constant-face)
    ("\\<nil\\>" 0 font-lock-constant-face)

    (,xbase-keywords . font-lock-keyword-face)

    ("\'[^\n]*\'" 0 font-lock-string-face)
    ("\\[[^\n]*\\]" 0 font-lock-string-face)

    "Default font-lock keywords for xbase mode."))

;;;###autoload
(defun xbase-mode ()
  "Major mode for editing xbase files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map xbase-mode-map)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'font-lock-defaults)
  (setq major-mode           'xbase-mode
        mode-name            "Xbase"
        indent-line-function 'xbase-indent-line
        font-lock-defaults '(xbase-font-lock-keywords nil t))
  (font-lock-mode)                      ; TODO: I don't think this should be turned on here.
  (run-hooks 'xbase-mode-hook))

(provide 'xbase)

;;; xbase.el ends here.
