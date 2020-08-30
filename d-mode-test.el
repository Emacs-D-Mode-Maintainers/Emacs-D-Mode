;;; d-mode-test.el --- Tests for D Programming Language major mode

;; Copyright (c) 2015 Dmitri Makarov

;; Author:  Dmitri Makarov <dmakarov@alumni.stanford.edu>
;; Maintainer:  Russel Winder <russel@winder.org.uk>
;; Created:  April 2015
;; Version: 201512060752

;;;; NB Version number is date and time yyyymmddhhMM in GMT (aka UTC).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
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

(load (expand-file-name "coverage/undercover.el"))
(if (getenv "D_MODE_COVERAGE")
    (progn
      ;; Generate a coverage report viewable in Emacs.
      (require 'undercover)
      (setq undercover-force-coverage t)
      (undercover "d-mode.el"
                  (:report-file "coverage/.resultset.json")
		  (:report-format 'simplecov)
		  (:send-report nil))
      )
  (unless (getenv "D_MODE_NO_COVERAGE")
    (when (require 'undercover nil t)
      (undercover "d-mode.el"))))

(require 'd-mode nil t)

(def-edebug-spec d--static-if (sexp form &optional form))
(def-edebug-spec d--if-version>= (sexp form &optional form))

(require 'htmlfontify)

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
        (topmost-intro-cont    . 1)
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
        (inexpr-class          . +)
        (knr-argdecl-intro     . 0)))
    (c-echo-syntactic-information-p . t)
    (c-indent-comment-alist . nil))
  "Style for testing.")

(c-add-style "teststyle" d-test-teststyle)

(defvar-local d-test-orig-filename nil)

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
    (setq-local d-test-orig-filename filename)
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
	 error-found-p
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
                (error "\nExpected: %s\nGot     : %s" expect result))))))
      t)

    (set-buffer save-buf)
    (goto-char save-point)
    (when (and (not error-found-p) (interactive-p))
      (kill-test-buffer))
    (not error-found-p)))

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

(require 'imenu)

(defun d-test--imenu-to-lines (x)
  "Extracts line numbers from one possibly-nested imenu--index-alist element X."
  (if (imenu--subalist-p x)
      (apply #'append (mapcar #'d-test--imenu-to-lines (cdr x)))
    (list (line-number-at-pos (cdr x)))))

(defun d-test-get-imenu-lines ()
  "Get list of line numbers of lines recognized as imenu entries.

Called from the #run snippet of individual test files."
  (imenu--make-index-alist t)
  (sort (d-test--imenu-to-lines (cons nil imenu--index-alist)) '<))

(defun d-test-save-result (filename)
  "In case of an unexpected result, save it to a file.

FILENAME is the original (versioned) file name."
  (write-region
   nil nil
   (concat
    (file-name-sans-extension filename)
    ".res"
    (file-name-extension filename t))))

(defun d-test-indent ()
  "Re-indent the current file.

If the resulting indentation ends up being different, raise an error."
  (condition-case nil
      (progn (c-indent-region (point-min) (point-max)) t)
    (error
     (let ((orig (buffer-string)))
       (let (buffer-read-only)
	 (c-indent-region (point-min) (point-max))
         (d-test-save-result d-test-orig-filename))
       (error (concat "Test case has been indented differently.\n"
		      "Expected:\n--------------------\n%s\n--------------------\n"
		      "Got:     \n--------------------\n%s\n--------------------\n")
	      orig (buffer-string))))))

(defun d-test-fontification ()
  "Test fontification the current file.

Compares fontification against a test file (same file name, with
a '.html' suffix).  If the result ends up being different from
the reference file, raise an error."
  ;; Work around 24.3 oddity
  (when (and
	 (boundp 'c-standard-font-lock-fontify-region-function)
	 (null c-standard-font-lock-fontify-region-function))
    (setq c-standard-font-lock-fontify-region-function
	  (default-value 'font-lock-fontify-region-function)))

  (let* ((hfy-optimisations '(body-text-only merge-adjacent-tags))
         (html-filename (concat filename ".html"))
	 (actual (with-current-buffer (htmlfontify-buffer nil "test.d") (buffer-string)))
	 (expected (with-temp-buffer
		     (insert-file-contents html-filename)
		     (buffer-string))))
    (unless (equal actual expected)
      (with-temp-buffer (insert actual) (d-test-save-result html-filename))
      (error (concat "Test case has been fontified differently.\n"
		     "Expected:\n--------------------\n%s\n--------------------\n"
		     "Got:     \n--------------------\n%s\n--------------------\n")
	     expected actual))))

(defun d-test-get-expected-result (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((min-ver
            (if (re-search-forward "^// #min-version: \\(.+\\)$" nil t)
                (match-string 1)
              "0")))
      (version<= min-ver emacs-version))))

(defmacro d-test-dir (dir)
  "Register all test files from DIR with ert."
  (apply #'nconc
         '(progn)
         (mapcar
          (lambda (filename)
            (let ((path (expand-file-name filename dir)))
              (cond
               ((string-match-p "\\`\\.\\.?\\'" filename)
                nil)
               ((string-match-p "\\.res" filename)
                nil)
               ((string-match-p "\\.\\(d\\|txt\\)\\'" filename)
                `((ert-deftest ,(intern (file-name-sans-extension filename)) ()
                    :expected-result (if (d-test-get-expected-result ,path) :passed :failed)
                    (should (do-one-test ,path)))))
               ((string-match-p "\\.d\\.html\\'" filename)
                nil)
               (t
                (message "Ignoring test file with unknown extension: %s" filename)
                nil))))
          (directory-files dir))))
(d-test-dir "tests")

;;----------------------------------------------------------------------------

(provide 'd-mode-test)

;;; d-mode-test.el ends here
