;;;; opfr.lisp -- "Outer-Parentheses-Free REPL"
;;;; http://rpw3.org/hacks/lisp/opfr.lisp

;;;; Copyright (c) 1996, 1997, 2000, 2001, 2003, 2009
;;;; Rob Warnock <rpw3@rpw3.org>.
;;;; All Rights Reserved.

;;; Permission to use, copy, modify, and/or distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;;; USAGE:
;;; 1. Load (or compile & then load) this file.
;;; 2. To enable auto-abort to top-level on error [with printing of the
;;;    condition] do (SETF *DEBUGGER-HOOK* 'OPFR:OPFR-DEBUGGER-STUB).
;;; 3. If you're running under CMUCL [see following portability note], to
;;;    enable a brief stack backtrace prior to the auto-abort to top-level,
;;;    set OPFR:*OPFR-BACKTRACE-LENGTH* to a small integer [8-10 seems good].
;;; 4. If desired, customize the prompts by redefining #'OPFR-PROMPT
;;;    and/or #'OPFR-PROMPT2.
;;; 4. Call #'OPFR:OPFR-REPL. Exit with <EOF> or :Q or :QUIT.

;;;; OVERVIEW:
;;; OPFR provides a simple command-line read-eval-print loop (REPL) for
;;; programs written in Common Lisp where the primary user community
;;; is uncomfortable using normal Lisp symbolic expressions (s-exprs).
;;; The simplicity and utility of OPFR derives from the observation
;;; that most such people are actually surprisingly accepting of an
;;; s-expr-based interface *provided* that they are not required
;;; to manually type the outer pair of parentheses, *even if* any
;;; sub-expressions are still in pure Lisp s-expr form!! This effect
;;; is even stronger when the majority of the command functions in the
;;; using application are provided as pre-defined functions or macros
;;; which only seldom require the typing of sub-expressions.
;;;
;;; Note: If users are going to be defining their own global variables,
;;; you may find it helpful to offer "global lexicals" [provided by
;;; a macro such as DEFLEX <http://rpw3.org/hacks/lisp/deflex.lisp>]
;;; to avoid the "dynamic surprise" that often occurs when CL-naive
;;; users type "DEFVAR X 3" and then later try to LET-bind "X".
;;;
;;; This short example compares the syntaxes. First, normal Common Lisp:
;;;
;;;   > (+ 1 2)
;;;   3
;;;   > (defvar x 34)
;;;   X
;;;   > (defvar y 25)
;;;   Y
;;;   > (expt x y)
;;;   193630125104980427932766033374162714624
;;;   > (expt x (- y 12))
;;;   81138303245565435904
;;;   > 
;;;
;;; Now, exactly the same sequence of operations using OPFR syntax:
;;;
;;;   opfr> + 1 2
;;;   3
;;;   opfr> defvar x 34
;;;   opfr> defvar y 25
;;;   opfr> expt x y
;;;   193630125104980427932766033374162714624
;;;   opfr> expt x (- y 12)
;;;   81138303245565435904
;;;   opfr>
;;;
;;; Note: The average Lisp programmer will see no significant advantage to
;;; the OPFR syntax (and some disadvantages, such as the need to resolve
;;; the ambiguity of a naked symbol -- should OPFR print its value as
;;; a global variable or call it as a "command" function?), especially
;;; since sub-expressions must still be fully-parenthesized (as in the
;;; last example above). Nevertheless, experience with real users has
;;; shown that the acceptance of the OPFR syntax is *enormously* greater
;;; than the "pure" Lisp s-expr. [Go figure... (*sigh*)]

;;;; PORTABILITY NOTE:
;;; This version of OPFR has been developed & used almost exclusively
;;; under CMUCL, and relies on that environment for several useful features
;;; [e.g., the functions #'EXT:INTERACTIVE-EVAL and #'DEBUG:BACKTRACE].
;;; While a cursory attempt was made before this release to conditionalize
;;; away and/or stub out the CMUCL-specific portions [*very* lightly tested
;;; under an old CLISP], some functionality *will* be lost on other CLs
;;; [e.g., the REPL history variables noted in CLHS "25.1.1 Top level loop"
;;; will not be maintained].

;;;; KNOWN BUGS & LIMITATIONS:
;;; 1. OPFR-READ terminates read when an unescaped newline is seen, and
;;;    uses "\" to escape newlines [like Tcl], allowing multi-line input.
;;;    Unfortunately, this means means that correctly implementing leading
;;;    single-escapes [see CLHS "2.1.4.6 Single Escape Character"] would
;;;    have required re-implementing most of the Common Lisp reader, which
;;;    was not done. Thus, the token "\abc" will read as the symbol ABC
;;;    instead of aBC. Workaround: Use a pair of multiple escape characters
;;;    (vertical bars "|") when escaping the first character of a token
;;;    is required. [Note that this is not usually a serious limitation,
;;;    since the CL reader is used for tokens *not* starting with "\", so
;;;    '\abc will be read as (QUOTE \ABC) == (QUOTE |aBC|) anyway.]
;;; 2. A "\" directly before a newline will "escape" the newline, and will
;;;    print a secondary prompt [see #'OPFR-PROMPT2], whereas a "\" before
;;;    whitespace and/or a comment introduced by semicolon (";") will *also*
;;;    have the effect of escaping the following newline, but will *not*
;;;    result in a secondary prompt. [This could be fixed, but it would
;;;    be messy.]

#+cmu
(ext:file-comment
  "$Header: /u/lisp/local/lib/RCS/opfr.lisp,v 1.7 2009/03/08 02:50:56 rpw3 Exp rpw3 $")

(defpackage #:org.rpw3.opfr
  (:nicknames #:opfr)
  (:use #:cl)
  (:export		; pretty much everything
   #:opfr-read
   #:opfr-repl-fixups
   #:opfr-prompt
   #:opfr-prompt2
   #:opfr-prompt-read
   #:opfr-prompt-read/cmd
   #:opfr-mv-eval-print
   #:opfr-repl
   #:opfr-debugger-stub
   #+cmu #:*opfr-backtrace-length*
   ))

(in-package #:opfr)

(eval-when (:load-toplevel :execute)
  (provide :opfr))	; Enable use as a REQUIRE-style module

;;; Gather sexps into a list until an unescaped newline is seen.
(defun opfr-read (&optional (s nil) eof-error-p (eof :eof))
  (loop for c = (peek-char nil s eof-error-p)
        and cmd = '() then cmd do
    (case c
      ((nil)			; EOF, we're done.
       (return (if (null cmd) eof (reverse cmd))))
      ((#\newline)		; EOL, done. Note: Empty line => NIL, not "".
       (read-char s)
       (return (reverse cmd)))
      ;; still necessary, since CL (peek-char t) eats linefeed (*sigh*)
      ((#\space #\tab)		; eat whitespace
       (read-char s))
      ((#\;)			; eat comments
       (loop do (read-char s) until (eql (peek-char nil s) #\newline)))
      ((#\\)
       (read-char s)
       (cond
         ((eql (peek-char nil s) #\newline)
          (read-char s)
          (opfr-prompt2))	; remind user command still needs a <CR>
         ;; XXX BUG! This is *not* correct, since "\" was eaten already,
         ;; and thus "\ABC" will read incorrectly.
         (t
          (push (read-preserving-whitespace) cmd))))
      (( #|(|# #\) )		; [stupid Vi!]
       (warn "Ignoring unmatched close parenthesis.")
       (read-char s))
      (t			; something substantial to gather
       (push (read-preserving-whitespace) cmd)))))

;;; "The usual fixups" -- a handful of minor rewrites that have been
;;; shown to be helpful to users in the past, even though they break
;;; pure compatibility with a standard Lisp REPL. (All have workarounds
;;; for the determined Lisper.)
;;;
(defun opfr-repl-fixups (cmd)
  (let ((item (car cmd)))
    (cond
      ;; Currently, we only "fixup" singleton lists.
      ((or (null cmd) (consp (cdr cmd)))
       cmd)			; stet
      ;; If user typed just a function, allow it to be called as a "command".
      ;; If user types a non-function symbol, just return it.
      ((symbolp item)
       (if (fboundp item)
         cmd
         item))
      ;; Conversely, if the user types a form *with* the outer parens
      ;; supplied [as is all too frequent by accident when Lisp users
      ;; type at an OPFR], strip the extra level of parens for him.
      ;; Note: This prevents directly calling a procedure-returning
      ;; procedure, but that's not legal in CL anyway (only in Scheme),
      ;; and can be done with FUNCALL. Ditto for literal values
      ;; (which aren't legal either in the function position).
      (t
       item))))

(defun opfr-prompt2 ()		; for continuation lines
  (princ #\tab)
  (force-output))

(defun opfr-prompt ()		; redefine to override
  (format t "~&opfr> ")
  (force-output))

;; Use this one to get bare "read" function.
(defun opfr-prompt-read ()
  (opfr-prompt)
  (opfr-read))

;; Use this one to get "usual fixups" on value read in a "command" context,
;; e.g., singleton non-procedure values are returned, not called, etc.
(defun opfr-prompt-read/cmd (eof)
  (opfr-prompt)
  (let ((cmd (opfr-read nil nil eof)))
    (if (eq cmd eof)
      cmd
      (opfr-repl-fixups cmd))))

;;; In CMUCL, use INTERACTIVE-EVAL instead of plain EVAL so that the
;;; history variables [*, **, ***, +, ++, +++, etc.] get updated.
(defun opfr-mv-eval-print (form)
  (format t "~&~{~S~%~}"
          (multiple-value-list (#+cmu ext:interactive-eval #-cmu eval form))))

;;; 2006-10-27T06:07:16Z -- New auto-backtrace format
;;; If *opfr-backtrace-length* is non-zero, a backtrace of that
;;; length will be printed on error. [A typical number is 8 to 10.]
#+cmu
(defvar *opfr-backtrace-length* 0)

;;; 2006-10-27T06:07:16Z -- New auto-backtrace format
;;; To use: (setf *debugger-hook* #'opfr-debugger-stub)
(defun opfr-debugger-stub (c hook)
  (declare (ignore hook))
  (when (find-restart 'abort c)
    (format *error-output* "~&~a~%   [Condition of type ~a]~%" c (type-of c))
    #+cmu
    (when (and (numberp *opfr-backtrace-length*)
               (plusp *opfr-backtrace-length*))
      ;; Show some stack, but not too much.
      (format *error-output* "~&~%Backtrace:~%")
      (debug:backtrace *opfr-backtrace-length*))
    (abort c)))

;;; 2006-12-15T23:32:20Z -- We used to wrap the inner LET in a
;;; (handler-case (let ...) (error (c) (invoke-debugger c)))
;;; but that was counterproductive, since HANDLER-CASE had already
;;; left the dynamic environment *before* INVOKE-DEBUGGER got called!
;;; So use HANDLER-BIND, instead.
;;;
;;; 2009-02-17/rpw3@rpw3.org -- Rip out HANDLER-BIND entirely.
;;; Now just let standard exceptions happen, but the caller can tweak
;;; behavior by setting *DEBUGGER-HOOK* [e.g., to #'OPFR-DEBUGGER-STUB]
;;; before calling OPFR-REPL.
;;;
(defun opfr-repl ()
  (loop with eof = (load-time-value (list :eof)) do
    (restart-case
        (catch #+cmu 'lisp::top-level-catcher ; so "q" works in CMUCL debugger
               #-cmu 'top-level-catcher
          (let ((form (opfr-prompt-read/cmd eof)))
            (cond
              ((or (eq eof form)
                   ;; Several variations of the "quit" command
                   (find form '(:q :quit (:q) (:quit)) :test #'equal))
               (fresh-line)	; so caller's re-prompt gets a new line
               (return-from opfr-repl))
              ((null form) 'ignore) ; just re-prompt
              (t (opfr-mv-eval-print form)))))
      (abort ()
        :report (lambda (s) (format s "Return to OPFR REPL."))
        (values nil t))
      (continue ()
        :report (lambda (s) (format s "Return to OPFR REPL."))
        (values nil t)))))
