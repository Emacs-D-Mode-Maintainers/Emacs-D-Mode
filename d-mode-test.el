;;; d-mode-test.el --- Tests for D Programming Language major mode

;; Copyright (c) 2015 Dmitri Makarov

;; Author:  Dmitri Makarov <dmakarov@alumni.stanford.edu>
;; Maintainer:  Russel Winder <russel@winder.org.uk>
;; Created:  April 2015
;; Version: 201512060752

;;;; NB Version number is date and time yyyymmddhhMM in GMT (aka UTC).

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

;;; Uage:

;;; Commentary:

;;; Bugs:
;; Bug tracking is currently handled using the GitHub issue tracker at
;; https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/issues

;;; Versions:
;;  This mode is available on MELPA which tracks the mainline Git repository on GitHub, so there is a rolling release
;;  system based on commits to the mainline.

;;; Notes:

;;; TODO:
;;   Issues with this code are managed via the project issue management
;;   on GitHub: https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/issues?state=open

;;; History
;;   History is tracked in the Git repository rather than in this file.
;;   See https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/commits/master

;;----------------------------------------------------------------------------
;;; Code:

(when (require 'undercover nil t)
  (undercover "d-mode.el"))

(require 'd-mode nil t)

(defconst d-test-teststyle
  '((c-tab-always-indent . t)
    (c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (c-comment-prefix-regexp . "\\(//+\\|\\**\\)[.!|]?")
    (c-hanging-braces-alist . ((block-open after)
                               (brace-list-open)
                               (substatement-open after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)))
    (c-hanging-colons-alist . ((member-init-intro before)
                               (inher-intro)
                               (case-label after)
                               (label after)
                               (access-key after)))
    (c-cleanup-list . (scope-operator empty-defun-braces defun-close-semi))
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
        (inexpr-class          . +)))
    (c-echo-syntactic-information-p . t)
    (c-indent-comment-alist . nil))
  "Style for testing.")

(c-add-style "teststyle" d-test-teststyle)

(defun make-test-buffer (filename)
  (let ((testbuf (get-buffer-create "*d-mode-test*"))
        (enable-local-eval t))
    ;; setup the test file buffer.
    (set-buffer testbuf)
    (kill-all-local-variables)
    (buffer-disable-undo testbuf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents filename)
    ;; test that we make no (hidden) changes.
    (setq buffer-read-only t)
    (goto-char (point-min))
    (let ((c-default-style "TESTSTYLE")
          d-mode-hook
          c-mode-common-hook)
      (d-mode))
    (hack-local-variables)
    testbuf))

(defun kill-test-buffer ()
  (let (buf)
    (if (setq buf (get-buffer "*d-mode-test*"))
        (kill-buffer buf))))

(defun d-test-message (msg &rest args)
  (if noninteractive
      (send-string-to-terminal
       (concat (apply 'format msg args) "\n"))
    (apply 'message msg args)))

(defun do-one-test (filename)
  (interactive "fFile to test: ")
  (let* ((save-buf (current-buffer))
         (save-point (point))
         (font-lock-maximum-decoration t)
         (font-lock-global-modes nil)
         (enable-local-variables ':all)
         (testbuf (make-test-buffer filename))
         (pop-up-windows t)
         (linenum 1)
         error-found
         expectedindent
         c-echo-syntactic-information-p)

    (switch-to-buffer testbuf)
    (syntax-ppss (point-max))
    ;; extract the run command and expected output if any.
    (let* ((contents (buffer-substring-no-properties 1 (point-max)))
           (run-str (if (string-match "^// #run: \\(.+\\)$" contents)
                        (match-string 1 contents)))
           (out-str (if (string-match "^// #out: \\(.+\\)$" contents)
                        (match-string 1 contents))))
      (when run-str
	(let ((result (eval (car (read-from-string run-str)))))
	  (when out-str
	    (let ((expect (car (read-from-string out-str))))
	      (unless (equal result expect)
		(setq error-found (format "\nExpected: %s\nGot     : %s" expect result))))))))

    (when error-found
      (set-buffer testbuf)
      (buffer-enable-undo testbuf)
      (set-buffer-modified-p nil)

      (error "Regression found in file %s: %s" filename error-found))

    (set-buffer save-buf)
    (goto-char save-point)
    (when (and (not error-found) (interactive-p))
      (kill-test-buffer))
    (not error-found)))

(defun d-test-get-compilation-lines ()
  "Get list of line numbers of lines recognized as errors by `compilation-mode'.

Called from the #run snippet of individual test files."
  (compilation-mode)
  (let (buffer-read-only)
    (compilation-parse-errors (point-min) (point-max)))
  (let (error-list)
    (while (condition-case nil
  	       (progn (compilation-next-error 1) t)
  	     (error nil))
      (setq error-list (cons (line-number-at-pos) error-list)))
    (reverse error-list)))

;; Run the tests
(ert-deftest d-mode-basic ()
  (should (equal (do-one-test "tests/I0021.d") t))
  (should (equal (do-one-test "tests/I0039.d") t))
  (should (equal (do-one-test "tests/I0064.d") t))
  (should (equal (do-one-test "tests/I0069.txt") t))
  (should (equal (do-one-test "tests/I0072.txt") t))
  )

;;----------------------------------------------------------------------------

(provide 'd-mode-test)

;;; d-mode-test.el ends here
