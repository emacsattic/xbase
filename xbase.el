;;; xbase.el --- A mode for editing Xbase programs.
;; Copyright (C) 2002 Mike Romberg <romberg@fsl.noaa.gov>
;; $Id$

;; xbase.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; xbase.el is blah blah blah.

;;; THANKS:
;;
;; 

;;; BUGS:
;;
;; o The indentation code is easily confused by multi-statement lines. For
;;   example, this code:
;;
;;      For n := 1 To 10; Next
;;
;;   will look like an unclosed For...Next loop.

;;; TODO:
;;
;; o Fix all bugs.
;;
;; o Fully document.
;;
;; o Add OO oriented indentation support.

;;; INSTALLATION:
;;
;;

;;; Code:

;; Things we need:
(eval-when-compile
  (require 'cl))
(require 'font-lock)

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; General customize options.

(defgroup xbase nil
  "Mode for editing Xbase source."
  :group 'languages
  :prefix "xbase-")

(defcustom xbase-mode-indent 3
  "*Default indentation per nesting level"
  :type 'integer
  :group 'xbase)

(defcustom xbase-indent-rules
;;   Rule Name            RegExp                                                      Opening Rule         Closing Rule       Offset   Subsequent Offset
  '((xbase-if             "^[\t ]*#?if"                                               nil                  xbase-endif        nil      nil)
    (xbase-else           "^[\t ]*#?else\\(if\\)?"                                    xbase-if             xbase-endif        nil      nil)
    (xbase-endif          "^[\t ]*#?end[\t ]*if"                                      xbase-if             nil                nil      nil)
    (xbase-do-case        "^[\t ]*do[\t ]+case"                                       nil                  xbase-end-case     nil      nil)
    (xbase-case           "^[\t ]*\\(case\\|otherwise\\)"                             xbase-do-case        xbase-end-case     +        nil)
    (xbase-end-case       "^[\t ]*endcase"                                            xbase-do-case        nil                nil      nil)
    (xbase-for            "^[\t ]*for"                                                nil                  xbase-next         nil      nil)
    (xbase-next           "^[\t ]*next"                                               xbase-for            nil                nil      nil)
    (xbase-do-while       "^[\t ]*\\(do[\t ]*\\)?while"                               nil                  xbase-enddo        nil      nil)
    (xbase-enddo          "^[\t ]*enddo"                                              xbase-do-while       nil                nil      nil)
    (xbase-begin-sequence "^[\t ]*begin[\t ]+sequence"                                nil                  xbase-end-sequence nil      nil)
    (xbase-break          "^[\t ]*break"                                              xbase-begin-sequence xbase-end-sequence +        -)
    (xbase-recover        "^[\t ]*recover"                                            xbase-begin-sequence xbase-end-sequence nil      nil)
    (xbase-end-sequence   "^[\t ]*end[\t ]+sequence"                                  xbase-begin-sequence nil                nil      nil)
    (xbase-text           "^[\t ]*text"                                               nil                  xbase-endtext      nil      0)
    (xbase-endtext        "^[\t ]*endtext"                                            xbase-text           nil                nil      nil)
    (xbase-defun          "^[\t ]*\\(static\\|init\\|exit\\)?[\t ]*\\(procedure\\|function\\)[\t ]+\\(\\w+\\)[\t ]*(?" xbase-defun xbase-defun nil nil)
    (xbase-local          "^[\t ]*local"                                              xbase-defun          xbase-defun        nil      nil))
  "*Rules for indenting Xbase code."
  :type '(repeat (list    :tag "Indentation rule"
                  (symbol :tag "Rule name")
                  (regexp :tag "Regular expression for matching code")
                  (symbol :tag "Name of opening rule")
                  (symbol :tag "Name of closing rule")
                  (choice :tag "Extra offset for this statement"
                   (const :tag "Add one extra level of indentation" +)
                   (const :tag "Remove one level of indentation" -)
                   (const :tag "Use the calculated indentation level" nil))
                  (choice :tag "Extra offset for subsequent lines of code"
                   (const :tag "Add one extra level of indentation" +)
                   (const :tag "Remove one level of indentation" -)
                   (const :tag "Remove all indentation" 0)
                   (const :tag "Use the calculated indentation level" nil))))
  :group 'xbase)

;; Indentation rules functions:

(defsubst xbase-rule (rule)
  (if (symbolp rule)
      (assoc rule xbase-indent-rules)
    rule))

(defsubst xbase-rule-name (rule)
  (nth 0 (xbase-rule rule)))

(defsubst xbase-rule-regexp (rule)
  (nth 1 (xbase-rule rule)))

(defsubst xbase-rule-opening-rule (rule)
  (nth 2 (xbase-rule rule)))

(defsubst xbase-rule-closing-rule (rule)
  (nth 3 (xbase-rule rule)))

(defsubst xbase-rule-offset (rule)
  (nth 4 (xbase-rule rule)))

(defsetf xbase-rule-offset (rule) (store)
  `(setf (nth 4 (xbase-rule ,rule)) ,store))

(defsubst xbase-rule-subsequent-offset (rule)
  (nth 5 (xbase-rule rule)))

(defsetf xbase-rule-subsequent-offset (rule) (store)
  `(setf (nth 5 (xbase-rule ,rule)) ,store))

(defsubst xbase-rule-opening-p (rule)
  (let ((rule (xbase-rule rule)))
    (and (not (xbase-rule-opening-rule rule))
         (xbase-rule-closing-rule rule))))

(defsubst xbase-rule-closing-p (rule)
  (let ((rule (xbase-rule rule)))
    (and (xbase-rule-opening-rule rule)
         (not (xbase-rule-closing-rule rule)))))

(defsubst xbase-rule-interim-p (rule)
  (let ((rule (xbase-rule rule)))
    (and (xbase-rule-opening-rule rule)
         (xbase-rule-closing-rule rule))))

(defsubst xbase-rule-opening-regexp (rule)
  (let ((rule (xbase-rule rule)))
    (if (xbase-rule-opening-p rule)
        (xbase-rule-regexp rule)
      (xbase-rule-regexp (xbase-rule (xbase-rule-opening-rule rule))))))

(defsubst xbase-rule-closing-regexp (rule)
  (let ((rule (xbase-rule rule)))
    (if (xbase-rule-closing-p rule)
        (xbase-rule-regexp rule)
      (xbase-rule-regexp (xbase-rule (xbase-rule-closing-rule rule))))))

(defun xbase-set-offset (rule-name offset)
  (let ((rule (xbase-rule rule-name)))
    (if rule
        (setf (xbase-rule-offset rule) offset)
      (error "%s is not a valid indent rule" rule-name))))

(defun xbase-set-subsequent-offset (rule-name offset)
  (let ((rule (xbase-rule rule-name)))
    (if rule
        (setf (xbase-rule-subsequent-offset rule) offset)
      (error "%s is not a valid indent rule" rule-name))))

;; Functions for calculating indentation.

(defun xbase-calculate-indent-with-offset (indent offset)
  (cond ((null offset)                  ; No offset.
         indent)
        ((numberp offset)               ; Specific column.
         (- offset xbase-mode-indent))
        ((fboundp offset)               ; + and - are used as functions.
         (+ indent (funcall offset xbase-mode-indent)))
        (t
         (error "'%' is not a valid offset"))))

(defun xbase-continuation-line-p ()
  "Is the current line of code a continuation of the previous line?"
  (save-excursion
    (beginning-of-line)
    (unless (bobp)
      (forward-line -1)
      (looking-at "^.*;[\t ]*$"))))

(defun xbase-beginning-of-line ()
  "Goto the start of the current line of code."
  (beginning-of-line)
  (while (and (not (bobp)) (xbase-continuation-line-p))
    (forward-line -1)))

(defun xbase-previous-line ()
  "Goto the start of the previous line of code."
  (xbase-beginning-of-line)
  (unless (bobp)
    (forward-line -1)
    (xbase-beginning-of-line)))

(defun xbase-find-matching-statement (rule)
  (let ((level            1)
        (case-fold-search t)            ; Xbase is case insensitive.
        (open-re          (xbase-rule-opening-regexp rule))
        (close-re         (xbase-rule-closing-regexp rule)))
    (beginning-of-line)
    (while (and (not (bobp)) (not (zerop level)))
      (xbase-previous-line)
      (cond ((looking-at close-re)
             (incf level))
            ((looking-at open-re)
             (decf level))))))
    
(defun xbase-current-line-match ()
  "Does the current line match anything in `xbase-indent-rules'?"
  (save-excursion
    (xbase-beginning-of-line)
    (let ((case-fold-search t))         ; Xbase is case insensitive.
      (loop for rule in xbase-indent-rules
            when (looking-at (xbase-rule-regexp rule))
            return rule))))

(defun xbase-find-statement-backward ()
  "Find a statement, looking at the current line and then working backwards."
  (loop for match = (xbase-current-line-match) then (xbase-current-line-match)
        until (or (bobp) match)
        do (xbase-previous-line)
        finally return match))

(defun xbase-find-some-statement-backward (test)
  (loop for match = (xbase-find-statement-backward) then (xbase-find-statement-backward)
        while (and (not (bobp)) match (not (funcall test match)))
        do (progn
             (when (xbase-rule-closing-p match)
               (xbase-find-matching-statement match))
             (xbase-previous-line))
        finally return match))

(defun xbase-find-opening-statement-backward ()
  (xbase-find-some-statement-backward #'(lambda (rule) (xbase-rule-opening-p rule))))


(defun xbase-find-opening/interim-statement-backward ()
  (xbase-find-some-statement-backward #'(lambda (rule) (not (xbase-rule-closing-p rule)))))

(defun xbase-some-statement-indentation (statement-type)
  (save-excursion
    (let ((match (unless (bobp)
                   (xbase-previous-line)
                   (funcall statement-type))))
      (cond (match
             (message "Matched to %s" (xbase-rule-name match))
             (xbase-calculate-indent-with-offset (current-indentation) (xbase-rule-subsequent-offset match)))
            (t
             (message "No matches found")
             (- xbase-mode-indent))))))

(defun xbase-previous-opening-statement-indentation ()
  (xbase-some-statement-indentation #'xbase-find-opening-statement-backward))

(defun xbase-previous-opening/interim-statement-indentation ()
  (xbase-some-statement-indentation #'xbase-find-opening/interim-statement-backward))

(defun xbase-matching-statement-indentation (rule)
  (save-excursion
    (xbase-find-matching-statement rule)
    (message "Matched to %s" (xbase-rule-name (xbase-current-line-match)))
    (current-indentation)))

(defun xbase-indent-level ()
  (save-excursion
    (let ((match (xbase-current-line-match)))
      (if match
          ;; We're on a statement.
          (cond ((xbase-rule-opening-p match)
                 ;; It's a block opening, indent it relative to the previous
                 ;; block opening statement.
                 (+ (xbase-previous-opening/interim-statement-indentation) xbase-mode-indent))
                ((xbase-rule-closing-p match)
                 ;; It's a closing statement, indent it to the same indentation
                 ;; level as its opening statement.
                 (xbase-matching-statement-indentation match))
                (t
                 ;; It's an "interim" statement.
                 (xbase-calculate-indent-with-offset (xbase-previous-opening-statement-indentation) (xbase-rule-offset (xbase-current-line-match)))))
        ;; We're on a "normal" line of code, indent it to the previous
        ;; opening/interim statement.
        (+ (xbase-previous-opening/interim-statement-indentation) xbase-mode-indent)))))

(defun xbase-indent-line (&optional whole-exp)
  "Indent current line of Xbase code.

Note: WHOLE-EXP is currently ignored."
  (interactive "p")
  (let ((indent (xbase-indent-level))
        (pos    (- (point-max) (point)))
        (beg    (progn
                  (beginning-of-line)
                  (point))))
    (skip-chars-forward " \t")
    (unless (zerop (- indent (current-column)))
      (delete-region beg (point))
      (indent-to indent))
    (when (> (- (point-max) pos) (point))
      (setf (point) (- (point-max) pos)))))

;; xbase-mode customize options.

(defcustom xbase-mode-hook nil
  "*List of hooks to execute on entry to `xbase-mode'."
  :type  'hook
  :group 'xbase)

;; xbase-mode keyboard map.

(defvar xbase-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Nothing special at the moment.
    map)
    "Keymap used in `xbase-mode'.")

;; xbase-mode non-customizable variables.

(defvar xbase-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_  "w"      st)
    (modify-syntax-entry ?\' "\""     st) ; "'" is a string delimiter.
    (modify-syntax-entry ?/  ". 124b" st) ; Enable "//" and "/**/" comments.
    (modify-syntax-entry ?*  ". 23"   st) ; Ditto.
    (modify-syntax-entry ?&  ". 12b"  st) ; Enable "&&" commands.
    (modify-syntax-entry ?\n "> b"    st) ; New line ends "//" and "&&" comments.
    st)
  "`xbase-mode' syntax table.")

;; xbase-mode font lock customize options.

(defcustom xbase-font-lock-statements
  '("announce"
    "begin" "sequence" "break" "recover" "using" "end" "sequence"
    "declare"
    "default"
    "do"
    "case" "otherwise" "endcase"
    "while" "exit" "loop" "enddo"
    "procedure"
    "field" "in" "local" "memvar" "return"
    "external"
    "for" "to" "step" "next"
    "static" "function" "local" "static"
    "create" "class" "inherit" "from" "method" "access" "assign" "endclass"
    "if" "else" "elseif" "endif"
    "init"
    "parameters"
    "private"
    "public"
    "request")
  "*Xbase statements for font locking."
  :type  '(repeat string)
  :group 'xbase)

(defcustom xbase-font-lock-directives
  '("command" "xcommand" "translate" "xtranslate"
    "define"
    "error"
    "ifdef" "ifndef" "else" "endif"
    "include"
    "stdout"
    "undef")
  "*Xbase directives for font locking."
  :type  '(repeat string)
  :group 'xbase)

(defcustom xbase-font-lock-commands
  '("text" "endtext")                   ; TODO: Lots more to add.
  "*Xbase commands for font locking."
  :type  '(repeat string)
  :group 'xbase)

(defcustom xbase-keyword-face 'font-lock-keyword-face
  "*Face to use for Xbase keywords."
  :type  'face
  :group 'xbase)

(defcustom xbase-directive-face 'font-lock-keyword-face
  "*Face to use for Xbase pre-processor directives."
  :type  'face
  :group 'xbase)

(defcustom xbase-command-face 'font-lock-keyword-face
  "*Face to use for Xbase commands."
  :type  'face
  :group 'xbase)

(defcustom xbase-function-name-face 'font-lock-function-name-face
  "*Face to use for function names."
  :type  'face
  :group 'xbase)

(defcustom xbase-variable-name-face 'font-lock-variable-name-face
  "*Face to use for variable names."
  :type  'face
  :group 'xbase)

(defcustom xbase-constant-face 'font-lock-constant-face
  "*Face to use for constants."
  :type  'face
  :group 'xbase)

;; xbase-mode code.

;;;###autoload
(defun xbase-mode ()
  "Major mode for editing Xbase source files.

Special commands:

\\{xbase-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map xbase-mode-map)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'font-lock-defaults)
  (setq major-mode           'xbase-mode
        mode-name            "Xbase"
        indent-line-function 'xbase-indent-line
        font-lock-defaults   (list
                              (list

                               ;; The first few entries deal with lists that
                               ;; the user can configure.
                               
                               ;; User configurable list of statements.
                               (list (regexp-opt xbase-font-lock-statements 'words) 1 xbase-keyword-face)
                               
                               ;; User configurable list of pre-processor directives.
                               (list (concat "#" (regexp-opt xbase-font-lock-directives 'words)) 1 xbase-directive-face)
                               
                               ;; user configurable list of commands.
                               (list (regexp-opt xbase-font-lock-commands 'words) 1 xbase-command-face)

                               ;; Now for some "hard wired" rules.
                               
                               ;; "defun" function names.
                               (list "\\<\\(function\\|procedure\\|method\\|access\\|assign\\|class\\|inherit\\|from\\)\\>\\s-\\<\\(\\w*\\)\\>" 2 xbase-function-name-face)

                               ;; #define constant name.
                               (list "#[ \t]*define[ \t]+\\(\\sw+\\)" 1 xbase-variable-name-face)

                               ;; Common constants.
                               (list "\\(\\.\\(f\\|\\t\\)\\.\\|\\<\\(nil\\|self\\|super\\)\\>\\)" 0 xbase-constant-face)

                               )
                              nil t))
  (set-syntax-table xbase-mode-syntax-table)
  (run-hooks 'xbase-mode-hook))
  
;;; xbase.el ends here
