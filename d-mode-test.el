;;; d-mode-test.el --- Tests for D Programming Language major mode

;; Author:  Dmitri Makarov <dmakarov@alumni.stanford.edu>
;; Maintainer:  Russel Winder <russel@winder.org.uk>
;; Created:  April 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(when (require 'undercover nil t)
  (undercover "d-mode.el"))

(require 'd-mode nil t)

(defconst d-test-teststyle
  '((c-tab-always-indent           . t)
    (c-basic-offset                . 4)
    (c-comment-only-line-offset    . 0)
    (c-comment-prefix-regexp       . "\\(//+\\|\\**\\)[.!|]?")
    (c-hanging-braces-alist        . ((block-open after)
                                      (brace-list-open)
                                      (substatement-open after)
                                      (inexpr-class-open after)
                                      (inexpr-class-close before)
                                      ))
    (c-hanging-colons-alist        . ((member-init-intro before)
                                      (inher-intro)
                                      (case-label after)
                                      (label after)
                                      (access-key after)))
    (c-cleanup-list                . (scope-operator
                                      empty-defun-braces
                                      defun-close-semi))
    (c-offsets-alist
     . ((string                . c-lineup-dont-change)
        (c                     . c-lineup-C-comments)
        (defun-open            . 0)
        (defun-close           . 0)
        (defun-block-intro     . +)
        (class-open            . 0)
        (class-close           . 0)
        (inline-open           . 0)
        (inline-close          . 0)
        (func-decl-cont        . +)
        (knr-argdecl-intro     . +)
        (knr-argdecl           . 0)
        (topmost-intro         . 0)
        (topmost-intro-cont    . c-lineup-topmost-intro-cont)
        (member-init-intro     . +)
        (member-init-cont      . c-lineup-multi-inher)
        (inher-intro           . +)
        (inher-cont            . c-lineup-multi-inher)
        (block-open            . 0)
        (block-close           . 0)
        (brace-list-open       . 0)
        (brace-list-close      . 0)
        (brace-list-intro      . +)
        (brace-list-entry      . 0)
        (statement             . 0)
        (statement-cont        . +)
        (statement-block-intro . +)
        (statement-case-intro  . +)
        (statement-case-open   . 0)
        (substatement          . +)
        (substatement-open     . +)
        (substatement-label    . *)
        (case-label            . 0)
        (access-label          . -)
        (label                 . *)
        (do-while-closure      . 0)
        (else-clause           . 0)
        (catch-clause          . 0)
        (comment-intro         . c-lineup-comment)
        (arglist-intro         . +)
        (arglist-cont          . (c-lineup-gcc-asm-reg 0))
        (arglist-cont-nonempty . (c-lineup-gcc-asm-reg c-lineup-arglist))
        (arglist-close         . +)
        (stream-op             . c-lineup-streamop)
        (inclass               . +)
        (cpp-macro             . [0])
        (cpp-macro-cont        . +)
        (cpp-define-intro      . (c-lineup-cpp-define +))
        (friend                . 0)
        (objc-method-intro     . [0])
        (objc-method-args-cont . c-lineup-ObjC-method-args)
        (objc-method-call-cont . c-lineup-ObjC-method-call)
        (extern-lang-open      . 0)
        (extern-lang-close     . 0)
        (inextern-lang         . +)
        (namespace-open        . 0)
        (namespace-close       . 0)
        (innamespace           . +)
        (module-open           . 0)
        (module-close          . 0)
        (inmodule              . 0)
        (composition-open      . 0)
        (composition-close     . 0)
        (incomposition         . 0)
        (template-args-cont    . (c-lineup-template-args +))
        (inlambda              . c-lineup-inexpr-block)
        (lambda-intro-cont     . +)
        (inexpr-statement      . +)
        (inexpr-class          . +)
        ))
    (c-echo-syntactic-information-p . t)
    (c-indent-comment-alist . nil)
    )
  "Style for testing.")

(c-add-style "teststyle" d-test-teststyle)

(defun make-test-buffers (filename)
  (let ((testbuf (get-buffer-create "*d-mode-test*"))
        (enable-local-eval t))
    ;; setup the test file buffer.
    (set-buffer testbuf)
    (kill-all-local-variables)
    (buffer-disable-undo testbuf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents filename)
    (setq buffer-read-only t) ; test that we make no (hidden) changes.
    (goto-char (point-min))
    (let ((c-default-style "TESTSTYLE")
          d-mode-hook c-mode-common-hook)
      (d-mode))
    (hack-local-variables)
    (list testbuf)))

(defun kill-test-buffers ()
  (let (buf)
    (if (setq buf (get-buffer "*d-mode-test*"))
        (kill-buffer buf))))

(defun do-one-test (filename)
  (interactive "fFile to test: ")

  (let* ((save-buf (current-buffer))
         (save-point (point))
         (font-lock-maximum-decoration t)
         (font-lock-global-modes nil)
         (enable-local-variables ':all)
         (buflist (make-test-buffers filename))
         (testbuf (car buflist))
         (pop-up-windows t)
         (linenum 1)
         test-error-found
         error-found-p
         expectedindent
         c-echo-syntactic-information-p
         font-lock-verbose
         last-result)

    (switch-to-buffer testbuf)

    ;; Record the expected indentation and reindent.  This is done
    ;; in backward direction to avoid cascading errors.
    (while (= (forward-line -1) 0)
      (back-to-indentation)
      (setq expectedindent (cons (current-column) expectedindent))
      (unless (eolp)
        ;; Do not reindent empty lines; the test cases might have
        ;; whitespace at eol trimmed away, so that could produce
        ;; false alarms.
        (let ((buffer-read-only nil))
          (if no-error
              (condition-case err
                  (c-indent-line)
                (error
                 (unless error-found-p
                   (setq error-found-p t)
                   (cc-test-log
                    "%s:%d: c-indent-line error: %s"
                    filename (1+ (count-lines (point-min) (c-point 'bol)))
                    (error-message-string err))
                   (when cc-test-last-backtrace
                     (cc-test-log "%s" cc-test-last-backtrace)
                     (setq cc-test-last-backtrace nil)))))
            (c-indent-line)))))

    (when (and error-found-p (not no-error))
      (set-buffer testbuf)
      (buffer-enable-undo testbuf)
      (set-buffer-modified-p nil)

      (error "Regression found in file %s" filename))

    (set-buffer save-buf)
    (goto-char save-point)
    (when (and (not error-found-p) (interactive-p))
      (kill-test-buffers))
    (not error-found-p)))

(ert-deftest d-mode-basic ()
  (should (equal (do-one-test "tests/I0039.d") t)))

(provide 'd-mode-test)
