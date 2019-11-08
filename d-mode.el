;;; d-mode.el --- D Programming Language major mode for (X)Emacs
;;;               Requires a cc-mode of version 5.30 or greater

;; Author:  William Baxter
;; Contributor:  Andrei Alexandrescu
;; Contributor:  Russel Winder
;; Maintainer:  Russel Winder <russel@winder.org.uk>
;;              Vladimir Panteleev <vladimir@thecybershadow.net>
;; Created:  March 2007
;; Version:  201911081541
;; Keywords:  D programming language emacs cc-mode
;; Package-Requires: ((emacs "25.1"))

;;;; NB Version number is date and time yyyymmddhhMM UTC.
;;;; A hook to update it automatically on save is available here:
;;;; https://gist.github.com/CyberShadow/28f60687c3bf83d32900cd6074c012cb

;; This file is not part of GNU Emacs.

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

;;; Usage:
;; Put these lines in your init file.
;;   (autoload 'd-mode "d-mode" "Major mode for editing D code." t)
;;   (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
;;
;; Alternatively you can load d-mode.el explicitly:
;;   (load "d-mode.el")
;;
;; cc-mode version 5.30 or greater is required.
;; You can check your cc-mode with the command M-x c-version.
;; You can get the latest version of cc-mode at http://cc-mode.sourceforge.net

;;; Commentary:
;;   This mode supports most of D's syntax, including nested /+ +/
;;   comments and backquote `string literals`.
;;
;;   This mode has been dubbed "2.0" because it is a complete rewrite
;;   from scratch.  The previous d-mode was based on cc-mode 5.28 or
;;   so.  This version is based on the cc-mode 5.30 derived mode
;;   example by Martin Stjernholm, 2002.

;;; Bugs:
;; Bug tracking is currently handled using the GitHub issue tracker at
;; https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/issues

;;; Versions:
;;  This mode is available on MELPA which tracks the mainline Git repository on GitHub, so there is a
;;  rolling release system based on commits to the mainline. For those wanting releases, the repository is
;;  tagged from time to time and this creates an entry in MELPA Stable and a tarball on GitHub.

;;; Notes:

;;; TODO:
;;   Issues with this code are managed via the project issue management
;;   on GitHub: https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/issues?state=open

;;; History:
;;   History is tracked in the Git repository rather than in this file.
;;   See https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/commits/master

;;; Code:

;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Required packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------------------------------

(require 'cc-mode)
(require 'cc-langs)

;; Needed to prevent
;;   "Symbol's value as variable is void: compilation-error-regexp-alist-alist" errors
(require 'compile)

;; Work around Emacs (cc-mode) bug #18845
(eval-when-compile
  (when (and (= emacs-major-version 24) (>= emacs-minor-version 4))
    (require 'cl)))

;; The set-difference function is used from the Common Lisp extensions.
(require 'cl-lib)

;; Used to specify regular expressions in a sane way.
(require 'rx)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
;; Comment out 'when-compile part for debugging
(eval-when-compile
  (require 'cc-fonts))


;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; cc-mode configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------------------------------

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'd-mode 'java-mode))

;; muffle the warnings about using free variables and undefined
;; functions
(defvar c-syntactic-element)
(declare-function c-populate-syntax-table "cc-langs.el" (table))

;; D has pointers
(c-lang-defconst c-type-decl-prefix-key
  d (concat "\\("
		   "[*(~]"
		   "\\|"
		   (c-lang-const c-type-decl-prefix-key)
		   "\\)"
		   "\\([^=]\\|$\\)"))

(c-lang-defconst c-decl-start-re
  d "[[:alpha:]_@~]")
  ;; d "[[:alpha:]_@]")

;; D has fixed arrays
(c-lang-defconst c-opt-type-suffix-key
  d "\\(\\[[^]]*\\]\\|\\.\\.\\.\\|\\*\\)")

(c-lang-defconst c-decl-prefix-re
  d "\\([{}();:,]+\\)")

(c-lang-defconst c-identifier-ops
  ;; For recognizing "~this", ".foo", and "foo.bar.baz" as identifiers
  d '((left-assoc ".")))

(c-lang-defconst c-after-id-concat-ops
  ;; Also for handling ~this
  d '("~"))

(c-lang-defconst c-string-escaped-newlines
  ;; Set to true to indicate the D handles backslash escaped newlines in strings
  d t)

(c-lang-defconst c-multiline-string-start-char
  ;; Set to true to indicate that D doesn't mind raw embedded newlines in strings
  d t)

(c-lang-defconst c-opt-cpp-prefix
  ;; Preprocessor directive recognizer.  D doesn't have cpp, but it has #line
  d "\\s *#\\s *")

(c-lang-defconst c-cpp-message-directives d nil)
(c-lang-defconst c-cpp-include-directives d nil)
(c-lang-defconst c-opt-cpp-macro-define d nil)
(c-lang-defconst c-cpp-expr-directives d nil)
(c-lang-defconst c-cpp-expr-functions d nil)

(c-lang-defconst c-assignment-operators
  ;; List of all assignment operators.
  d  '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" ">>>=" "&=" "^=" "^^="
       "|=" "~="))

(c-lang-defconst c-other-op-syntax-tokens
  "List of the tokens made up of characters in the punctuation or
parenthesis syntax classes that have uses other than as expression
operators."
 d (append '("/+" "+/" "..." ".." "!" "*" "&")
	    (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-block-comment-starter d "/*")
(c-lang-defconst c-block-comment-ender   d "*/")

(c-lang-defconst c-comment-start-regexp  d "/[*+/]")
(c-lang-defconst c-block-comment-start-regexp d "/[*+]")
(c-lang-defconst c-literal-start-regexp
  ;; Regexp to match the start of comments and string literals.
  d "/[*+/]\\|\"\\|`")

(c-lang-defconst c-block-prefix-disallowed-chars
  ;; Allow ':' for inherit list starters.
  d (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
		       '(?:)))

(c-lang-defconst c-post-protection-token
  d  ":")

;;----------------------------------------------------------------------------

;; Built-in basic types
(c-lang-defconst c-primitive-type-kwds
  d '("bool" "byte" "ubyte" "char" "delegate" "double" "float"
      "function" "int" "long" "short" "uint" "ulong" "ushort"
      "cent" "ucent" "real" "ireal" "idouble" "ifloat" "creal" "cfloat" "cdouble"
      "wchar" "dchar" "void" "string" "wstring" "dstring"))

;; Keywords that can prefix normal declarations of identifiers
(c-lang-defconst c-modifier-kwds
  d '("abstract" "deprecated" "extern"
      "final" "out" "lazy" "mixin" "override" "private"
      "protected" "package" "public" "ref" "scope" "static" "synchronized"
      "volatile" "__vector"))

(c-lang-defconst c-class-decl-kwds
  ;; Keywords introducing declarations where the following block (if any)
  ;; contains another declaration level that should be considered a class.
  d '("class" "struct" "union" "interface" "template"))

;; (c-lang-defconst c-brace-list-decl-kwds
;;   d '("enum"))

(c-lang-defconst c-type-modifier-kwds
  d nil)

(c-lang-defconst c-type-prefix-kwds
  ;; Keywords where the following name - if any - is a type name, and
  ;; where the keyword together with the symbol works as a type in
  ;; declarations.  In this case, like "mixin foo!(x) bar;"
  d    '("mixin" "align"))

;; Remove "enum" from d-mode's value.
;; By default this c-typedef-decl-kwds includes c-brace-list-decl-kwds,
;; which is '("enum") by default.
;; Instead, parse enums manually (see d-font-lock-enum-body) to avoid
;; confusion with manifest constants.
(c-lang-defconst c-typedef-decl-kwds
  ;; Keywords introducing declarations where the identifier(s) being
  ;; declared are types.
 d (append (c-lang-const c-class-decl-kwds)
	   '("typedef" "alias")))

(c-lang-defconst c-decl-hangon-kwds
  d '("export"))

(c-lang-defconst c-protection-kwds
  ;; Access protection label keywords in classes.
  d '("deprecated" "static" "extern" "final" "synchronized" "override"
      "abstract" "scope"
      "private" "package" "protected" "public" "export"))

(c-lang-defconst c-postfix-spec-kwds
 ;Keywords introducing extra declaration specifiers in the region
 ;between the header and the body (i.e. the "K&R-region") in
 ;declarations.
 d '("if" "in" "out" "body"))

(c-lang-defconst c-recognize-knr-p
  d t)

(c-lang-defconst c-type-list-kwds
  d nil)

(c-lang-defconst c-ref-list-kwds
  d '("import" "module"))

(c-lang-defconst c-colon-type-list-kwds
  ;; Keywords that may be followed (not necessarily directly) by a colon
  ;; and then a comma separated list of type identifiers.
  d  '("class" "enum" "interface"))

(c-lang-defconst c-paren-nontype-kwds
  ;;Keywords that may be followed by a parenthesis expression that doesn't
  ;; contain type identifiers.
  d '("version" "debug" "extern" "macro" "mixin" "pragma"))

(c-lang-defconst d-type-modifier-kwds
  ;; D's type modifiers.
  d '("const" "immutable" "inout" "shared"))

(c-lang-defconst d-type-modifier-key
  ;; Regex of `d-type-modifier-kwds'.
  d (c-make-keywords-re t
      (c-lang-const d-type-modifier-kwds)))

(c-lang-defconst d-common-storage-class-kwds
  ;; D's storage classes (keywords that can prefix or entirely
  ;; substitute a type in a parameter or variable declaration).
  d `(;; Constness
      ,@(c-lang-const d-type-modifier-kwds)
      ;; Storage classes that apply to either parameters and declarations
      "scope"))

(c-lang-defconst d-decl-storage-class-kwds
  d `(;; Common keywords
      ,@(c-lang-const d-common-storage-class-kwds)
      ;; auto (no-effect placeholder)
      "auto"
      ;; Storage class
      "extern" "static" "__gshared"))

(c-lang-defconst d-param-storage-class-kwds
  d `(;; Common keywords
      ,@(c-lang-const d-common-storage-class-kwds)
      ;; Function parameters
      "in" "out" "ref" "lazy"))

(c-lang-defconst d-storage-class-kwds
  d (c--delete-duplicates (append (c-lang-const d-decl-storage-class-kwds)
				  (c-lang-const d-param-storage-class-kwds))
			  :test 'string-equal))

(c-lang-defconst d-storage-class-key
  ;; Regex of `d-storage-class-kwds'.
  d (c-make-keywords-re t
      (c-lang-const d-storage-class-kwds)))

(c-lang-defconst c-paren-type-kwds
  ;; Keywords that may be followed by a parenthesis expression containing
  ;; type identifiers separated by arbitrary tokens.
  d (append (list "delete" "throw")
	    (c-lang-const d-type-modifier-kwds)))

;; D: Like `c-regular-keywords-regexp', but contains keywords which
;; cannot occur in a function type.  For Emacs 25 imenu.
(c-lang-defconst d-non-func-type-kwds-re
  d (concat "\\<"
	    (c-make-keywords-re t
	      (c--set-difference (c-lang-const c-keywords)
				 (append (c-lang-const c-primitive-type-kwds)
					 (c-lang-const d-decl-storage-class-kwds))
				 :test 'string-equal))))

;; D: Like `c-regular-keywords-regexp', but contains keywords which
;; cannot occur in a function name.  For Emacs 25 imenu.
(c-lang-defconst d-non-func-name-kwds-re
  d (concat "\\<"
	    (c-make-keywords-re t (c-lang-const c-keywords))))

(c-lang-defconst c-block-stmt-1-kwds
  ;; Statement keywords followed directly by a substatement.
  d '("do" "else" "finally" "try" "in" "body"))

(c-lang-defconst c-block-stmt-2-kwds
  ;; Statement keywords followed by a paren sexp and then by a substatement.
  d '("for" "if" "switch" "while" "catch" "synchronized" "scope"
      "foreach" "foreach_reverse" "with" "out" "invariant" "unittest"))

(c-lang-defconst c-simple-stmt-kwds
  ;; Statement keywords followed by an expression or nothing.
  d '("break" "continue" "goto" "return" "throw"))

(c-lang-defconst c-paren-stmt-kwds
  ;; Statement keywords followed by a parenthesis expression that
  ;; nevertheless contains a list separated with ';' and not ','."
  d '("for" "foreach" "foreach_reverse"))

(c-lang-defconst c-asm-stmt-kwds
  ;; Statement keywords followed by an assembler expression.
  d '("asm"))

(c-lang-defconst c-label-kwds
  ;; Keywords introducing colon terminated labels in blocks.
  d '("case" "default"))

(c-lang-defconst c-before-label-kwds
  ;; Keywords that might be followed by a label identifier.
  d '("goto" "break" "continue"))

(c-lang-defconst c-constant-kwds
  ;; Keywords for constants.
  d '("null" "true" "false"))

(c-lang-defconst c-primary-expr-kwds
  ;; Keywords besides constants and operators that start primary expressions.
  d '("this" "super"))

(c-lang-defconst c-inexpr-class-kwds
  ;; Keywords that can start classes inside expressions.
  d nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  ;; Keywords that can start brace list blocks inside expressions.
  d nil)

(c-lang-defconst c-other-decl-kwds
  d (c-lang-const d-storage-class-kwds))

(c-lang-defconst c-decl-start-kwds
  d '("else"))

(c-lang-defconst c-other-kwds
  ;; Keywords not accounted for by any other `*-kwds' language constant.
  d '("__gshared" "__traits" "assert" "cast" "in" "is" "nothrow" "pure" "ref"
      "sizeof" "typeid" "typeof"))


(c-lang-defconst c-recognize-post-brace-list-type-p
  ;; Set to t when we recognize a colon and then a type after an enum,
  ;; e.g., enum foo : int { A, B, C };"
  d t)

;; Enabled for java-mode, but we don't need it.
;; (We can't reuse this for D templates because this is hard-wired to
;; the < and > characters.)
(c-lang-defconst c-recognize-<>-arglists
  d nil)

;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cc-mode patches ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------------------------------

(defmacro d-make-keywords-re (adorn list)
  "Helper to precompute regular expressions for inline keyword lists." ;; checkdoc-params: (adorn list)
  (eval `(c-make-keywords-re ,adorn ,list 'd)))

;;----------------------------------------------------------------------------
;;; Workaround for special case of 'else static if' not being handled properly
(defun d-special-case-looking-at (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing cc-mode indentation in certain D constructs."
  (let ((rxp (car args)))
    (if (and (stringp rxp) (string= rxp "if\\>[^_]"))
        (or (apply orig-fun '("static\\>\\s-+if\\>[^_]"))
            (apply orig-fun '("version\\>[^_]"))
            (apply orig-fun '("debug\\>[^_]"))
            (apply orig-fun args))
      (apply orig-fun args))))

(defun d-around--c-add-stmt-syntax (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing cc-mode indentation in certain D constructs."
  (if (not (c-major-mode-is 'd-mode))
      (apply orig-fun args)
    (add-function :around (symbol-function 'looking-at)
		  #'d-special-case-looking-at)
    (unwind-protect
	(apply orig-fun args)
      (remove-function (symbol-function 'looking-at)
		       #'d-special-case-looking-at))))

(advice-add 'c-add-stmt-syntax :around #'d-around--c-add-stmt-syntax)

;;----------------------------------------------------------------------------
;;; Implements handling of D constructors
;;; Fixes e.g. indentation of contracts on constructors.

;; Make it so that inside c-forward-decl-or-cast-1,
;; "this" looks like a function identifier but not a type identifier.

(defun d-forward-name ()
  "Wraps `c-forward-name' for d-mode.

Fixes cc-mode handling of D constructors."
  (if (not (looking-at (d-make-keywords-re t '("this" "~this"))))
      (c-forward-name)
    (goto-char (match-end 1))
    t))

;;----------------------------------------------------------------------------

(defun d-forward-decl-or-cast-1 (preceding-token-end context last-cast-end)
  "Modified version of `c-forward-decl-or-cast-1' for d-mode." ;; checkdoc-params: (preceding-token-end context last-cast-end)
  ;; Move forward over a declaration or a cast if at the start of one.
  ;; The point is assumed to be at the start of some token.  Nil is
  ;; returned if no declaration or cast is recognized, and the point
  ;; is clobbered in that case.
  ;;
  ;; If a declaration is parsed:
  ;;
  ;;   The point is left at the first token after the first complete
  ;;   declarator, if there is one.  The return value is a list of 5 elements,
  ;;   where the first is the position of the first token in the declarator.
  ;;   (See below for the other four.)
  ;;   Some examples:
  ;;
  ;; 	 void foo (int a, char *b) stuff ...
  ;; 	  car ^                    ^ point
  ;; 	 float (*a)[], b;
  ;; 	   car ^     ^ point
  ;; 	 unsigned int a = c_style_initializer, b;
  ;; 		  car ^ ^ point
  ;; 	 unsigned int a (cplusplus_style_initializer), b;
  ;; 		  car ^                              ^ point (might change)
  ;; 	 class Foo : public Bar {}
  ;; 	   car ^   ^ point
  ;; 	 class PikeClass (int a, string b) stuff ...
  ;; 	   car ^                           ^ point
  ;; 	 enum bool;
  ;; 	  car ^   ^ point
  ;; 	 enum bool flag;
  ;; 	       car ^   ^ point
  ;;     void cplusplus_function (int x) throw (Bad);
  ;;      car ^                                     ^ point
  ;;     Foo::Foo (int b) : Base (b) {}
  ;; car ^                ^ point
  ;;
  ;;     auto foo = 5;
  ;;      car ^   ^ point
  ;;     auto cplusplus_11 (int a, char *b) -> decltype (bar):
  ;;      car ^                             ^ point
  ;;
  ;;
  ;;
  ;;   The second element of the return value is non-nil when a
  ;;   `c-typedef-decl-kwds' specifier is found in the declaration.
  ;;   Specifically it is a dotted pair (A . B) where B is t when a
  ;;   `c-typedef-kwds' ("typedef") is present, and A is t when some
  ;;   other `c-typedef-decl-kwds' (e.g. class, struct, enum)
  ;;   specifier is present.  I.e., (some of) the declared
  ;;   identifier(s) are types.
  ;;
  ;;   The third element of the return value is non-nil when the declaration
  ;;   parsed might be an expression.  The fourth element is the position of
  ;;   the start of the type identifier.  The fifth element is t if either
  ;;   CONTEXT was 'top, or the declaration is detected to be treated as top
  ;;   level (e.g. with the keyword "extern").
  ;;
  ;; If a cast is parsed:
  ;;
  ;;   The point is left at the first token after the closing paren of
  ;;   the cast.  The return value is `cast'.  Note that the start
  ;;   position must be at the first token inside the cast parenthesis
  ;;   to recognize it.
  ;;
  ;; PRECEDING-TOKEN-END is the first position after the preceding
  ;; token, i.e. on the other side of the syntactic ws from the point.
  ;; Use a value less than or equal to (point-min) if the point is at
  ;; the first token in (the visible part of) the buffer.
  ;;
  ;; CONTEXT is a symbol that describes the context at the point:
  ;; 'decl     In a comma-separated declaration context (typically
  ;;           inside a function declaration arglist).
  ;; '<>       In an angle bracket arglist.
  ;; 'arglist  Some other type of arglist.
  ;; 'top      Some other context and point is at the top-level (either
  ;;           outside any braces or directly inside a class or namespace,
  ;;           etc.)
  ;; nil       Some other context or unknown context.  Includes
  ;;           within the parens of an if, for, ... construct.
  ;; 'not-decl This value is never supplied to this function.  It
  ;;           would mean we're definitely not in a declaration.
  ;;
  ;; LAST-CAST-END is the first token after the closing paren of a
  ;; preceding cast, or nil if none is known.  If
  ;; `c-forward-decl-or-cast-1' is used in succession, it should be
  ;; the position after the closest preceding call where a cast was
  ;; matched.  In that case it's used to discover chains of casts like
  ;; "(a) (b) c".
  ;;
  ;; This function records identifier ranges on
  ;; `c-record-type-identifiers' and `c-record-ref-identifiers' if
  ;; `c-record-type-identifiers' is non-nil.
  ;;
  ;; This function might do hidden buffer changes.

  ;; D: The "else" following a "version" or "static if" can start a
  ;; declaration even without a { } block. For this reason, "else" is
  ;; in `c-decl-start-kwds'.
  ;; However, cc-mode invokes `c-forward-decl-or-cast-1' with point
  ;; at the "else" keyword, which, when followed by a function call,
  ;; is mis-parsed as a function declaration.
  ;; Fix this by moving point forward, past the "else" keyword, to
  ;; put cc-mode on the right track.
  (when (looking-at (d-make-keywords-re t '("else")))
    (goto-char (match-end 1))
    (c-forward-syntactic-ws))

  ;; D: Work around a cc-mode bug(?) in which the c-forward-annotation
  ;; calls in c-forward-decl-or-cast-1 do not advance the start
  ;; position, causing the annotation to be fontified as the function
  ;; name.
  (while (c-forward-annotation)
    (c-forward-syntactic-ws))

  (let (;; `start-pos' is used below to point to the start of the
	;; first type, i.e. after any leading specifiers.  It might
	;; also point at the beginning of the preceding syntactic
	;; whitespace.
	(start-pos (point))
	;; Set to the result of `c-forward-type'.
	at-type
	;; The position of the first token in what we currently
	;; believe is the type in the declaration or cast, after any
	;; specifiers and their associated clauses.
	type-start
	;; The position of the first token in what we currently
	;; believe is the declarator for the first identifier.  Set
	;; when the type is found, and moved forward over any
	;; `c-decl-hangon-kwds' and their associated clauses that
	;; occurs after the type.
	id-start
	;; These store `at-type', `type-start' and `id-start' of the
	;; identifier before the one in those variables.  The previous
	;; identifier might turn out to be the real type in a
	;; declaration if the last one has to be the declarator in it.
	;; If `backup-at-type' is nil then the other variables have
	;; undefined values.
	backup-at-type backup-type-start backup-id-start
	;; Set if we've found a specifier (apart from "typedef") that makes
	;; the defined identifier(s) types.
	at-type-decl
	;; Set if we've a "typedef" keyword.
	at-typedef
	;; Set if we've found a specifier that can start a declaration
	;; where there's no type.
	maybe-typeless
	;; Save the value of kwd-sym between loops of the "Check for a
	;; type" loop.  Needed to distinguish a C++11 "auto" from a pre
	;; C++11 one.
	prev-kwd-sym
	;; If a specifier is found that also can be a type prefix,
	;; these flags are set instead of those above.  If we need to
	;; back up an identifier, they are copied to the real flag
	;; variables.  Thus they only take effect if we fail to
	;; interpret it as a type.
	backup-at-type-decl backup-maybe-typeless
	;; Whether we've found a declaration or a cast.  We might know
	;; this before we've found the type in it.  It's 'ids if we've
	;; found two consecutive identifiers (usually a sure sign, but
	;; we should allow that in labels too), and t if we've found a
	;; specifier keyword (a 100% sure sign).
	at-decl-or-cast
	;; Set when we need to back up to parse this as a declaration
	;; but not as a cast.
	backup-if-not-cast
	;; For casts, the return position.
	cast-end
	;; Have we got a new-style C++11 "auto"?
	new-style-auto
	;; Set when the symbol before `preceding-token-end' is known to
	;; terminate the previous construct, or when we're at point-min.
	at-decl-start
	;; Set when we have encountered a keyword (e.g. "extern") which
	;; causes the following declaration to be treated as though top-level.
	make-top
	;; Save `c-record-type-identifiers' and
	;; `c-record-ref-identifiers' since ranges are recorded
	;; speculatively and should be thrown away if it turns out
	;; that it isn't a declaration or cast.
	(save-rec-type-ids c-record-type-identifiers)
	(save-rec-ref-ids c-record-ref-identifiers)
	;; Set when we parse a declaration which might also be an expression,
	;; such as "a *b".  See CASE 16 and CASE 17.
	maybe-expression)

    (save-excursion
      (goto-char preceding-token-end)
      (setq at-decl-start
	    (or (bobp)
		(let ((tok-end (point)))
		  (c-backward-token-2)
		  (member (buffer-substring-no-properties (point) tok-end)
			  c-pre-start-tokens)))))

    ;; Check for a type.  Unknown symbols are treated as possible
    ;; types, but they could also be specifiers disguised through
    ;; macros like __INLINE__, so we recognize both types and known
    ;; specifiers after them too.
    (while
	(let* ((start (point)) kwd-sym kwd-clause-end found-type noise-start)

	  (cond
	  ;; Look for a specifier keyword clause.
	   ((or (and (looking-at c-make-top-level-key)
		     (setq make-top t))
		(looking-at c-prefix-spec-kwds-re)
		(and (c-major-mode-is 'java-mode)
		 (looking-at "@[A-Za-z0-9]+")))
	    (save-match-data
	      (if (looking-at c-typedef-key)
		  (setq at-typedef t)))
	    (setq kwd-sym (c-keyword-sym (match-string 1)))
	    (save-excursion
	      (c-forward-keyword-clause 1)
	      (setq kwd-clause-end (point))))
	   ((and c-opt-cpp-prefix
		 (looking-at c-noise-macro-with-parens-name-re))
	    (setq noise-start (point))
	    (c-forward-noise-clause)
	    (setq kwd-clause-end (point))))

	  (when (setq found-type (c-forward-type t)) ; brace-block-too
	    ;; Found a known or possible type or a prefix of a known type.
	    (when (and (c-major-mode-is 'c++-mode) ; C++11 style "auto"?
		       (eq prev-kwd-sym (c-keyword-sym "auto"))
		       (looking-at "[=(]")) ; FIXME!!! proper regexp.
	      (setq new-style-auto t)
	      (setq found-type nil)
	      (goto-char start))	; position of foo in "auto foo"

	    (when at-type
	      ;; Got two identifiers with nothing but whitespace
	      ;; between them.  That can only happen in declarations.
	      (setq at-decl-or-cast 'ids)

	      (when (eq at-type 'found)
		;; If the previous identifier is a found type we
		;; record it as a real one; it might be some sort of
		;; alias for a prefix like "unsigned".
		(save-excursion
		  (goto-char type-start)
		  (let ((c-promote-possible-types t))
		    (c-forward-type)))))

	    (setq backup-at-type at-type
		  backup-type-start type-start
		  backup-id-start id-start
		  at-type found-type
		  type-start start
		  id-start (point)
		  ;; The previous ambiguous specifier/type turned out
		  ;; to be a type since we've parsed another one after
		  ;; it, so clear these backup flags.
		  backup-at-type-decl nil
		  backup-maybe-typeless nil))

	  (if (or kwd-sym noise-start)
	      (progn
		;; Handle known specifier keywords and
		;; `c-decl-hangon-kwds' which can occur after known
		;; types.

		(if (or (c-keyword-member kwd-sym 'c-decl-hangon-kwds)
			noise-start)
		    ;; It's a hang-on keyword or noise clause that can occur
		    ;; anywhere.
		    (progn
		      (if at-type
			  ;; Move the identifier start position if
			  ;; we've passed a type.
			  (setq id-start kwd-clause-end)
			;; Otherwise treat this as a specifier and
			;; move the fallback position.
			(setq start-pos kwd-clause-end))
		      (goto-char kwd-clause-end))

		  ;; It's an ordinary specifier so we know that
		  ;; anything before this can't be the type.
		  (setq backup-at-type nil
			start-pos kwd-clause-end)

		  (if found-type
		      ;; It's ambiguous whether this keyword is a
		      ;; specifier or a type prefix, so set the backup
		      ;; flags.  (It's assumed that `c-forward-type'
		      ;; moved further than `c-forward-keyword-clause'.)
		      (progn
			(when (c-keyword-member kwd-sym 'c-typedef-decl-kwds)
			  (setq backup-at-type-decl t))
			(when (c-keyword-member kwd-sym 'c-typeless-decl-kwds)
			  (setq backup-maybe-typeless t)))

		    (when (c-keyword-member kwd-sym 'c-typedef-decl-kwds)
		      ;; This test only happens after we've scanned a type.
		      ;; So, with valid syntax, kwd-sym can't be 'typedef.
		      (setq at-type-decl t))
		    (when (c-keyword-member kwd-sym 'c-typeless-decl-kwds)
		      (setq maybe-typeless t))

		    ;; Haven't matched a type so it's an unambiguous
		    ;; specifier keyword and we know we're in a
		    ;; declaration.
		    (setq at-decl-or-cast t)
		    (setq prev-kwd-sym kwd-sym)

		    (goto-char kwd-clause-end))))

	    ;; If the type isn't known we continue so that we'll jump
	    ;; over all specifiers and type identifiers.  The reason
	    ;; to do this for a known type prefix is to make things
	    ;; like "unsigned INT16" work.
	    (and found-type (not (eq found-type t))))))

    (cond
     ((eq at-type t)
      ;; If a known type was found, we still need to skip over any
      ;; hangon keyword clauses after it.  Otherwise it has already
      ;; been done in the loop above.
      (while
	  (cond ((looking-at c-decl-hangon-key)
		 (c-forward-keyword-clause 1))
		((and c-opt-cpp-prefix
		      (looking-at c-noise-macro-with-parens-name-re))
		 (c-forward-noise-clause))))
      (setq id-start (point)))

     ((eq at-type 'prefix)
      ;; A prefix type is itself a primitive type when it's not
      ;; followed by another type.
      (setq at-type t))

     ((not at-type)
      ;; Got no type but set things up to continue anyway to handle
      ;; the various cases when a declaration doesn't start with a
      ;; type.
      (setq id-start start-pos))

     ((and (eq at-type 'maybe)
	   (c-major-mode-is 'c++-mode))
      ;; If it's C++ then check if the last "type" ends on the form
      ;; "foo::foo" or "foo::~foo", i.e. if it's the name of a
      ;; (con|de)structor.
      (save-excursion
	(let (name end-2 end-1)
	  (goto-char id-start)
	  (c-backward-syntactic-ws)
	  (setq end-2 (point))
	  (when (and
		 (c-simple-skip-symbol-backward)
		 (progn
		   (setq name
			 (buffer-substring-no-properties (point) end-2))
		   ;; Cheating in the handling of syntactic ws below.
		   (< (skip-chars-backward ":~ \t\n\r\v\f") 0))
		 (progn
		   (setq end-1 (point))
		   (c-simple-skip-symbol-backward))
		 (>= (point) type-start)
		 (equal (buffer-substring-no-properties (point) end-1)
			name)
		 (goto-char end-2)
		 (progn
		   (c-forward-syntactic-ws)
		   (eq (char-after) ?\()))
	    ;; It is a (con|de)structor name.  In that case the
	    ;; declaration is typeless so zap out any preceding
	    ;; identifier(s) that we might have taken as types.
	    (goto-char type-start)
	    (setq at-type nil
		  backup-at-type nil
		  id-start type-start))))))

    ;; Check for and step over a type decl expression after the thing
    ;; that is or might be a type.  This can't be skipped since we
    ;; need the correct end position of the declarator for
    ;; `max-type-decl-end-*'.
    (let ((start (point)) (paren-depth 0) pos
	  ;; True if there's a non-open-paren match of
	  ;; `c-type-decl-prefix-key'.
	  got-prefix
	  ;; True if the declarator is surrounded by a parenthesis pair.
	  got-parens
	  ;; True if there is an identifier in the declarator.
	  got-identifier
	  ;; True if we find a number where an identifier was expected.
	  got-number
	  ;; True if there's a non-close-paren match of
	  ;; `c-type-decl-suffix-key'.
	  got-suffix
	  ;; True if there's a prefix match outside the outermost
	  ;; paren pair that surrounds the declarator.
	  got-prefix-before-parens
	  ;; True if there's a suffix match outside the outermost
	  ;; paren pair that surrounds the declarator.  The value is
	  ;; the position of the first suffix match.
	  got-suffix-after-parens
	  ;; True if we've parsed the type decl to a token that is
	  ;; known to end declarations in this context.
	  at-decl-end
	  ;; The earlier values of `at-type' and `type-start' if we've
	  ;; shifted the type backwards.
	  identifier-type identifier-start
	  ;; If `c-parse-and-markup-<>-arglists' is set we need to
	  ;; turn it off during the name skipping below to avoid
	  ;; getting `c-type' properties that might be bogus.  That
	  ;; can happen since we don't know if
	  ;; `c-restricted-<>-arglists' will be correct inside the
	  ;; arglist paren that gets entered.
	  c-parse-and-markup-<>-arglists
	  ;; Start of the identifier for which `got-identifier' was set.
	  name-start
	  ;; Position after (innermost) open parenthesis encountered in the
	  ;; prefix operators.
	  after-paren-pos)

      (goto-char id-start)

      ;; Skip over type decl prefix operators.  (Note similar code in
      ;; `c-forward-declarator'.)
      (if (and c-recognize-typeless-decls
	       (equal c-type-decl-prefix-key "\\<\\>"))
	  (when (eq (char-after) ?\()
	    (progn
	      (setq paren-depth (1+ paren-depth))
	      (forward-char)
	      (setq after-paren-pos (point))))
	(while (and (looking-at c-type-decl-prefix-key)
		    (if (and (c-major-mode-is 'c++-mode)
			     (match-beginning 3))
			;; If the third submatch matches in C++ then
			;; we're looking at an identifier that's a
			;; prefix only if it specifies a member pointer.
			(when (progn (setq pos (point))
				     (setq got-identifier (d-forward-name)))
			  (setq name-start pos)
			  (if (looking-at "\\(::\\)")
			      ;; We only check for a trailing "::" and
			      ;; let the "*" that should follow be
			      ;; matched in the next round.
			      (progn (setq got-identifier nil) t)
			    ;; It turned out to be the real identifier,
			    ;; so stop.
			    nil))
		      t))

	  (if (eq (char-after) ?\()
	      (progn
		(setq paren-depth (1+ paren-depth))
		(forward-char)
		(setq after-paren-pos (point)))
	    (unless got-prefix-before-parens
	      (setq got-prefix-before-parens (= paren-depth 0)))
	    (setq got-prefix t)
	    (goto-char (match-end 1)))
	  (c-forward-syntactic-ws)))

      (setq got-parens (> paren-depth 0))

      ;; Try to skip over an identifier.
      (or got-identifier
	  (and (looking-at c-identifier-start)
	       (setq pos (point))
	       (setq got-identifier (d-forward-name))
	       (setq name-start pos))
	  (when (looking-at "[0-9]")
	    (setq got-number t))) ; We've probably got an arithmetic expression.

      ;; Skip over type decl suffix operators and trailing noise macros.
      (while
	  (cond
	   ((and c-opt-cpp-prefix
		 (looking-at c-noise-macro-with-parens-name-re))
	    (c-forward-noise-clause))

	   ((and (looking-at c-type-decl-suffix-key)
		 ;; We avoid recognizing foo(bar) or foo() at top level as a
		 ;; construct here in C, since we want to recognize this as a
		 ;; typeless function declaration.
		 (not (and (c-major-mode-is 'c-mode)
			   (or (eq context 'top) make-top)
			   (eq (char-after) ?\)))))
	    (if (eq (char-after) ?\))
		(when (> paren-depth 0)
		  (setq paren-depth (1- paren-depth))
		  (forward-char)
		  t)
	      (when (if (save-match-data (looking-at "\\s("))
			(c-safe (c-forward-sexp 1) t)
		      (goto-char (match-end 1))
		      t)
		(when (and (not got-suffix-after-parens)
			   (= paren-depth 0))
		  (setq got-suffix-after-parens (match-beginning 0)))
		(setq got-suffix t))))

	   (t
	    ;; No suffix matched.  We might have matched the
	    ;; identifier as a type and the open paren of a
	    ;; function arglist as a type decl prefix.  In that
	    ;; case we should "backtrack": Reinterpret the last
	    ;; type as the identifier, move out of the arglist and
	    ;; continue searching for suffix operators.
	    ;;
	    ;; Do this even if there's no preceding type, to cope
	    ;; with old style function declarations in K&R C,
	    ;; (con|de)structors in C++ and `c-typeless-decl-kwds'
	    ;; style declarations.  That isn't applicable in an
	    ;; arglist context, though.
	    (when (and (= paren-depth 1)
			  (not got-prefix-before-parens)
			  (not (eq at-type t))
			  (or backup-at-type
			      maybe-typeless
			      backup-maybe-typeless
			      (when c-recognize-typeless-decls
				(and (memq context '(nil top))
				     ;; Deal with C++11's "copy-initialization"
				     ;; where we have <type>(<constant>), by
				     ;; contrasting with a typeless
				     ;; <name>(<type><parameter>, ...).
				     (save-excursion
				       (goto-char after-paren-pos)
				       (c-forward-syntactic-ws)
				       (or (c-forward-type)
					   ;; Recognize a top-level typeless
					   ;; function declaration in C.
					   (and (c-major-mode-is 'c-mode)
						(or (eq context 'top) make-top)
						(eq (char-after) ?\))))))))
			  (setq pos (c-up-list-forward (point)))
			  (eq (char-before pos) ?\)))
		 (c-fdoc-shift-type-backward)
		 (goto-char pos)
		 t)))

	(c-forward-syntactic-ws))

      (when (or (and new-style-auto
		     (looking-at c-auto-ops-re))
		(and (or maybe-typeless backup-maybe-typeless)
		     (not got-identifier)
		     (not got-prefix)
		     at-type))
	;; Have found no identifier but `c-typeless-decl-kwds' has
	;; matched so we know we're inside a declaration.  The
	;; preceding type must be the identifier instead.
	(c-fdoc-shift-type-backward))

      ;; Prepare the "-> type;" for fontification later on.
      (when (and new-style-auto
		 (looking-at c-haskell-op-re))
	(save-excursion
	  (goto-char (match-end 0))
	  (c-forward-syntactic-ws)
	  (setq type-start (point))
	  (setq at-type (c-forward-type))))

      ;; Move forward over any "WS" ids (like "final" or "override" in C++)
      (while (looking-at c-type-decl-suffix-ws-ids-key)
	(goto-char (match-end 1))
	(c-forward-syntactic-ws))

      (setq
       at-decl-or-cast
       (catch 'at-decl-or-cast

	 ;; CASE 1
	 (when (> paren-depth 0)
	   ;; Encountered something inside parens that isn't matched by
	   ;; the `c-type-decl-*' regexps, so it's not a type decl
	   ;; expression.  Try to skip out to the same paren depth to
	   ;; not confuse the cast check below.  If we don't manage this and
	   ;; `at-decl-or-cast' is 'ids we might have an expression like
	   ;; "foo bar ({ ..." which is a valid C++11 initialization.
	   (if (and (not (c-safe (goto-char (scan-lists (point) 1 paren-depth))))
		    (eq at-decl-or-cast 'ids))
	       (c-fdoc-shift-type-backward))
	   ;; If we've found a specifier keyword then it's a
	   ;; declaration regardless.
	   (throw 'at-decl-or-cast (memq at-decl-or-cast '(t ids))))

	 (setq at-decl-end
	       (looking-at (cond ((eq context '<>) "[,>]")
				 ((not (memq context '(nil top))) "[,\)]")
				 (t "[,;]"))))

	 ;; Now we've collected info about various characteristics of
	 ;; the construct we're looking at.  Below follows a decision
	 ;; tree based on that.  It's ordered to check more certain
	 ;; signs before less certain ones.

	 (if got-identifier
	     (progn

	       ;; CASE 2
	       (when (and (or at-type maybe-typeless)
			  (not (or got-prefix got-parens)))
		 ;; Got another identifier directly after the type, so it's a
		 ;; declaration.
		 (throw 'at-decl-or-cast t))

	       (when (and got-parens
			  (not got-prefix)
			  ;; (not got-suffix-after-parens)
			  (or backup-at-type
			      maybe-typeless
			      backup-maybe-typeless
			      (eq at-decl-or-cast t)
			      ;; Check whether we have "bar (gnu);" where we
			      ;; are directly inside a class (etc.) called "bar".
			      (save-excursion
				(and
				 (progn
				   (goto-char name-start)
				   (not (memq (c-forward-type) '(nil maybe))))
				 (progn
				  (goto-char id-start)
				  (c-directly-in-class-called-p
				   (buffer-substring
				    type-start
				    (progn
				      (goto-char type-start)
				      (c-forward-type)
				      (c-backward-syntactic-ws)
				      (point)))))))))
		 ;; Got a declaration of the form "foo bar (gnu);" or "bar
		 ;; (gnu);" where we've recognized "bar" as the type and "gnu"
		 ;; as the declarator, and in the latter case, checked that
		 ;; "bar (gnu)" appears directly inside the class "bar".  In
		 ;; this case it's however more likely that "bar" is the
		 ;; declarator and "gnu" a function argument or initializer
		 ;; (if `c-recognize-paren-inits' is set), since the parens
		 ;; around "gnu" would be superfluous if it's a declarator.
		 ;; Shift the type one step backward.
		 (c-fdoc-shift-type-backward)))

	   ;; Found no identifier.

	   (if backup-at-type
	       (progn

		 ;; CASE 3
		 (when (= (point) start)
		   ;; Got a plain list of identifiers. If a colon follows it's
		   ;; a valid label, or maybe a bitfield.  Otherwise the last
		   ;; one probably is the declared identifier and we should
		   ;; back up to the previous type, providing it isn't a cast.
		   (if (and (eq (char-after) ?:)
			    (not (c-major-mode-is 'java-mode)))
		       (cond
			;; If we've found a specifier keyword then it's a
			;; declaration regardless.
			((eq at-decl-or-cast t)
			 (throw 'at-decl-or-cast t))
			((and c-has-bitfields
			      (eq at-decl-or-cast 'ids)) ; bitfield.
			 (setq backup-if-not-cast t)
			 (throw 'at-decl-or-cast t)))

		     (setq backup-if-not-cast t)
		     (throw 'at-decl-or-cast t)))

		 ;; CASE 4
		 (when (and got-suffix
			    (not got-prefix)
			    (not got-parens))
		   ;; Got a plain list of identifiers followed by some suffix.
		   ;; If this isn't a cast then the last identifier probably is
		   ;; the declared one and we should back up to the previous
		   ;; type.
		   (setq backup-if-not-cast t)
		   (throw 'at-decl-or-cast t)))

	     ;; CASE 5
	     (when (eq at-type t)
	       ;; If the type is known we know that there can't be any
	       ;; identifier somewhere else, and it's only in declarations in
	       ;; e.g. function prototypes and in casts that the identifier may
	       ;; be left out.
	       (throw 'at-decl-or-cast t))

	     (when (= (point) start)
	       ;; Only got a single identifier (parsed as a type so far).
	       ;; CASE 6
	       (if (and
		    ;; Check that the identifier isn't at the start of an
		    ;; expression.
		    at-decl-end
		    (cond
		     ((eq context 'decl)
		      ;; Inside an arglist that contains declarations.  If K&R
		      ;; style declarations and parenthesis style initializers
		      ;; aren't allowed then the single identifier must be a
		      ;; type, else we require that it's known or found
		      ;; (primitive types are handled above).
		      (or (and (not c-recognize-knr-p)
			       (not c-recognize-paren-inits))
			  (memq at-type '(known found))))
		     ((eq context '<>)
		      ;; Inside a template arglist.  Accept known and found
		      ;; types; other identifiers could just as well be
		      ;; constants in C++.
		      (memq at-type '(known found)))))
		   (throw 'at-decl-or-cast t)
		 ;; CASE 7
		 ;; Can't be a valid declaration or cast, but if we've found a
		 ;; specifier it can't be anything else either, so treat it as
		 ;; an invalid/unfinished declaration or cast.
		 (throw 'at-decl-or-cast at-decl-or-cast))))

	   (if (and got-parens
		    (not got-prefix)
		    (memq context '(nil top))
		    (not (eq at-type t))
		    (or backup-at-type
			maybe-typeless
			backup-maybe-typeless
			(when c-recognize-typeless-decls
			  (or (not got-suffix)
			      (not (looking-at
				    c-after-suffixed-type-maybe-decl-key))))))
	       ;; Got an empty paren pair and a preceding type that probably
	       ;; really is the identifier.  Shift the type backwards to make
	       ;; the last one the identifier.  This is analogous to the
	       ;; "backtracking" done inside the `c-type-decl-suffix-key' loop
	       ;; above.
	       ;;
	       ;; Exception: In addition to the conditions in that
	       ;; "backtracking" code, do not shift backward if we're not
	       ;; looking at either `c-after-suffixed-type-decl-key' or "[;,]".
	       ;; Since there's no preceding type, the shift would mean that
	       ;; the declaration is typeless.  But if the regexp doesn't match
	       ;; then we will simply fall through in the tests below and not
	       ;; recognize it at all, so it's better to try it as an abstract
	       ;; declarator instead.
	       (c-fdoc-shift-type-backward)

	     ;; Still no identifier.
	     ;; CASE 8
	     (when (and got-prefix (or got-parens got-suffix))
	       ;; Require `got-prefix' together with either `got-parens' or
	       ;; `got-suffix' to recognize it as an abstract declarator:
	       ;; `got-parens' only is probably an empty function call.
	       ;; `got-suffix' only can build an ordinary expression together
	       ;; with the preceding identifier which we've taken as a type.
	       ;; We could actually accept on `got-prefix' only, but that can
	       ;; easily occur temporarily while writing an expression so we
	       ;; avoid that case anyway.  We could do a better job if we knew
	       ;; the point when the fontification was invoked.
	       (throw 'at-decl-or-cast t))

	     ;; CASE 9
	     (when (and at-type
			(not got-prefix)
			(not got-parens)
			got-suffix-after-parens
			(eq (char-after got-suffix-after-parens) ?\())
	       ;; Got a type, no declarator but a paren suffix. I.e. it's a
	       ;; normal function call after all (or perhaps a C++ style object
	       ;; instantiation expression).
	       (throw 'at-decl-or-cast nil))))

	 ;; CASE 9.5
	 (when (and (not context)	; i.e. not at top level.
		    (c-major-mode-is 'c++-mode)
		    (eq at-decl-or-cast 'ids)
		    after-paren-pos)
	   ;; We've got something like "foo bar (...)" in C++ which isn't at
	   ;; the top level.  This is probably a uniform initialization of bar
	   ;; to the contents of the parens.  In this case the declarator ends
	   ;; at the open paren.
	   (goto-char (1- after-paren-pos))
	   (throw 'at-decl-or-cast t))

	 ;; CASE 10
	 (when at-decl-or-cast
	   ;; By now we've located the type in the declaration that we know
	   ;; we're in.
	   (throw 'at-decl-or-cast t))

	 ;; CASE 11
	 (when (and got-identifier
		    (looking-at c-after-suffixed-type-decl-key)
		    (or (eq context 'top)
			make-top
			(and (eq context nil)
			     (match-beginning 1)))
		    (if (and got-parens
			     (not got-prefix)
			     (not got-suffix)
			     (not (eq at-type t)))
			;; Shift the type backward in the case that there's a
			;; single identifier inside parens.  That can only
			;; occur in K&R style function declarations so it's
			;; more likely that it really is a function call.
			;; Therefore we only do this after
			;; `c-after-suffixed-type-decl-key' has matched.
			(progn (c-fdoc-shift-type-backward) t)
		      got-suffix-after-parens))
	   ;; A declaration according to `c-after-suffixed-type-decl-key'.
	   (throw 'at-decl-or-cast t))

	 ;; CASE 12
	 (when (and (or got-prefix (not got-parens))
		    (memq at-type '(t known)))
	   ;; It's a declaration if a known type precedes it and it can't be a
	   ;; function call.
	   (throw 'at-decl-or-cast t))

	 ;; If we get here we can't tell if this is a type decl or a normal
	 ;; expression by looking at it alone.	(That's under the assumption
	 ;; that normal expressions always can look like type decl expressions,
	 ;; which isn't really true but the cases where it doesn't hold are so
	 ;; uncommon (e.g. some placements of "const" in C++) it's not worth
	 ;; the effort to look for them.)

;;;  2008-04-16: commented out the next form, to allow the function to recognize
;;;  "foo (int bar)" in CC (an implicit type (in class foo) without a semicolon)
;;;  as a(n almost complete) declaration, enabling it to be fontified.
	 ;; CASE 13
	 ;;	(unless (or at-decl-end (looking-at "=[^=]"))
	 ;; If this is a declaration it should end here or its initializer(*)
	 ;; should start here, so check for allowed separation tokens.	Note
	 ;; that this rule doesn't work e.g. with a K&R arglist after a
	 ;; function header.
	 ;;
	 ;; *) Don't check for C++ style initializers using parens
	 ;; since those already have been matched as suffixes.
	 ;;
	 ;; If `at-decl-or-cast' is then we've found some other sign that
	 ;; it's a declaration or cast, so then it's probably an
	 ;; invalid/unfinished one.
	 ;;	  (throw 'at-decl-or-cast at-decl-or-cast))

	 ;; Below are tests that only should be applied when we're certain to
	 ;; not have parsed halfway through an expression.

	 ;; CASE 14
	 (when (memq at-type '(t known))
	   ;; The expression starts with a known type so treat it as a
	   ;; declaration.
	   (throw 'at-decl-or-cast t))

	 ;; CASE 15
	 (when (and (c-major-mode-is 'c++-mode)
		    ;; In C++ we check if the identifier is a known type, since
		    ;; (con|de)structors use the class name as identifier.
		    ;; We've always shifted over the identifier as a type and
		    ;; then backed up again in this case.
		    identifier-type
		    (or (memq identifier-type '(found known))
			(and (eq (char-after identifier-start) ?~)
			     ;; `at-type' probably won't be 'found for
			     ;; destructors since the "~" is then part of the
			     ;; type name being checked against the list of
			     ;; known types, so do a check without that
			     ;; operator.
			     (or (save-excursion
				   (goto-char (1+ identifier-start))
				   (c-forward-syntactic-ws)
				   (c-with-syntax-table
				       c-identifier-syntax-table
				     (looking-at c-known-type-key)))
				 (save-excursion
				   (goto-char (1+ identifier-start))
				   ;; We have already parsed the type earlier,
				   ;; so it'd be possible to cache the end
				   ;; position instead of redoing it here, but
				   ;; then we'd need to keep track of another
				   ;; position everywhere.
				   (c-check-type (point)
						 (progn (c-forward-type)
							(point))))))))
	   (throw 'at-decl-or-cast t))

	 (if got-identifier
	     (progn
	       ;; CASE 16
	       (when (and got-prefix-before-parens
			  at-type
			  (or at-decl-end (looking-at "=[^=]"))
			  (memq context '(nil top))
			  (or (not got-suffix)
			      at-decl-start))
		 ;; Got something like "foo * bar;".  Since we're not inside
		 ;; an arglist it would be a meaningless expression because
		 ;; the result isn't used.  We therefore choose to recognize
		 ;; it as a declaration.  We only allow a suffix (which makes
		 ;; the construct look like a function call) when
		 ;; `at-decl-start' provides additional evidence that we do
		 ;; have a declaration.
		 (setq maybe-expression t)
		 (throw 'at-decl-or-cast t))

	       ;; CASE 17
	       (when (and (or got-suffix-after-parens
			      (looking-at "=[^=]"))
			  (eq at-type 'found)
			  (not (eq context 'arglist)))
		 ;; Got something like "a (*b) (c);" or "a (b) = c;".  It could
		 ;; be an odd expression or it could be a declaration.  Treat
		 ;; it as a declaration if "a" has been used as a type
		 ;; somewhere else (if it's a known type we won't get here).
		 (setq maybe-expression t)
		 (throw 'at-decl-or-cast t))

	       ;; CASE 17.5
	       (when (and c-asymmetry-fontification-flag
			  got-prefix-before-parens
			  at-type
			  (or (not got-suffix)
			      at-decl-start))
		 (let ((space-before-id
			(save-excursion
			  (goto-char name-start)
			  (or (bolp) (memq (char-before) '(?\  ?\t)))))
		       (space-after-type
			(save-excursion
			  (goto-char type-start)
			  (and (c-forward-type)
			       (progn (c-backward-syntactic-ws) t)
			       (or (eolp)
				   (memq (char-after) '(?\  ?\t)))))))
		   (when (not (eq (not space-before-id)
				  (not space-after-type)))
		     (setq maybe-expression t)
		     (throw 'at-decl-or-cast t)))))

	   ;; CASE 18
	   (when (and (not (memq context '(nil top)))
		      (or (and got-prefix (not got-number))
			  (and (eq context 'decl)
			       (not c-recognize-paren-inits)
			       (or got-parens got-suffix))))
	     ;; Got a type followed by an abstract declarator.  If `got-prefix'
	     ;; is set it's something like "a *" without anything after it.  If
	     ;; `got-parens' or `got-suffix' is set it's "a()", "a[]", "a()[]",
	     ;; or similar, which we accept only if the context rules out
	     ;; expressions.
	     (throw 'at-decl-or-cast t)))

	 ;; If we had a complete symbol table here (which rules out
	 ;; `c-found-types') we should return t due to the disambiguation rule
	 ;; (in at least C++) that anything that can be parsed as a declaration
	 ;; is a declaration.  Now we're being more defensive and prefer to
	 ;; highlight things like "foo (bar);" as a declaration only if we're
	 ;; inside an arglist that contains declarations.  Update (2017-09): We
	 ;; now recognize a top-level "foo(bar);" as a declaration in C.
	 ;; CASE 19
	 (or (eq context 'decl)
	     (and (c-major-mode-is 'c-mode)
		  (or (eq context 'top) make-top))))))

    ;; The point is now after the type decl expression.

    (cond
     ;; Check for a cast.
     ((save-excursion
	(and
	 c-cast-parens

	 ;; Should be the first type/identifier in a cast paren.
	 (> preceding-token-end (point-min))
	 (memq (char-before preceding-token-end) c-cast-parens)

	 ;; The closing paren should follow.
	 (progn
	   (c-forward-syntactic-ws)
	   (looking-at "\\s)"))

	 ;; There should be a primary expression after it.
	 (let (pos)
	   (forward-char)
	   (c-forward-syntactic-ws)
	   (setq cast-end (point))
	   (and (looking-at c-primary-expr-regexp)
		(progn
		  (setq pos (match-end 0))
		  (or
		   ;; Check if the expression begins with a prefix keyword.
		   (match-beginning 2)
		   (if (match-beginning 1)
		       ;; Expression begins with an ambiguous operator.  Treat
		       ;; it as a cast if it's a type decl or if we've
		       ;; recognized the type somewhere else.
		       (or at-decl-or-cast
			   (memq at-type '(t known found)))
		     ;; Unless it's a keyword, it's the beginning of a primary
		     ;; expression.
		     (not (looking-at c-keywords-regexp)))))
		;; If `c-primary-expr-regexp' matched a nonsymbol token, check
		;; that it matched a whole one so that we don't e.g. confuse
		;; the operator '-' with '->'.  It's ok if it matches further,
		;; though, since it e.g. can match the float '.5' while the
		;; operator regexp only matches '.'.
		(or (not (looking-at c-nonsymbol-token-regexp))
		    (<= (match-end 0) pos))))

	 ;; There should either be a cast before it or something that isn't an
	 ;; identifier or close paren.
	 (> preceding-token-end (point-min))
	 (progn
	   (goto-char (1- preceding-token-end))
	   (or (eq (point) last-cast-end)
	       (progn
		 (c-backward-syntactic-ws)
		 (if (< (skip-syntax-backward "w_") 0)
		     ;; It's a symbol.  Accept it only if it's one of the
		     ;; keywords that can precede an expression (without
		     ;; surrounding parens).
		     (looking-at c-simple-stmt-key)
		   (and
		    ;; Check that it isn't a close paren (block close is ok,
		    ;; though).
		    (not (memq (char-before) '(?\) ?\])))
		    ;; Check that it isn't a nonsymbol identifier.
		    (not (c-on-identifier)))))))))

      ;; Handle the cast.
      (when (and c-record-type-identifiers at-type (not (eq at-type t)))
	(let ((c-promote-possible-types t))
	  (goto-char type-start)
	  (c-forward-type)))

      (goto-char cast-end)
      'cast)

     (at-decl-or-cast
      ;; We're at a declaration.  Highlight the type and the following
      ;; declarators.

      (when backup-if-not-cast
	(c-fdoc-shift-type-backward t))

      (when (and (eq context 'decl) (looking-at ","))
	;; Make sure to propagate the `c-decl-arg-start' property to
	;; the next argument if it's set in this one, to cope with
	;; interactive refontification.
	(c-put-c-type-property (point) 'c-decl-arg-start))

      ;; Record the type's coordinates in `c-record-type-identifiers' for
      ;; later fontification.
      (when (and c-record-type-identifiers at-type ;; (not (eq at-type t))
		 ;; There seems no reason to exclude a token from
		 ;; fontification just because it's "a known type that can't
		 ;; be a name or other expression".  2013-09-18.
		 )
	(let ((c-promote-possible-types t))
	  (save-excursion
	    (goto-char type-start)
	    (c-forward-type))))

      (list id-start
	    (and (or at-type-decl at-typedef)
		 (cons at-type-decl at-typedef))
	    maybe-expression
	    type-start
	    (or (eq context 'top) make-top)))

     (t
      ;; False alarm.  Restore the recorded ranges.
      (setq c-record-type-identifiers save-rec-type-ids
	    c-record-ref-identifiers save-rec-ref-ids)
      nil))))

;;----------------------------------------------------------------------------

(defun d-around--c-forward-decl-or-cast-1 (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing cc-mode handling of D constructors."
  (cond
   ((not (c-major-mode-is 'd-mode))
    (apply orig-fun args))

   ;; D: The logic in cc-mode's `c-forward-decl-or-cast-1' will
   ;; recognize "someIdentifier in" as a variable declaration,
   ;; fontifying someIdentifier as a type. Prevent this here.
   ((and
     (looking-at c-identifier-start)
     (save-excursion
       (c-forward-token-2)
       (looking-at (d-make-keywords-re t '("is" "!is" "in" "!in")))))
    nil)

   ;; D: cc-mode gets confused due to "scope" being a keyword that can
   ;; both be part of declarations (as a storage class), and a
   ;; statement (e.g. "scope(exit)"). Disambiguate them here.
   ((and
     (looking-at (d-make-keywords-re t '("scope")))
     (save-excursion
       (c-forward-token-2)
       (looking-at "(")))
    nil)

   (t
    (apply #'d-forward-decl-or-cast-1 args))))

(advice-add 'c-forward-decl-or-cast-1 :around #'d-around--c-forward-decl-or-cast-1)

;;----------------------------------------------------------------------------

(defun d-around--c-get-fontification-context (orig-fun match-pos &rest args)
  ;; checkdoc-params: (orig-fun match-pos args)
  "Advice function for fixing cc-mode handling of D lambda parameter lists."
  (let ((res (apply orig-fun match-pos args)))
    (when (and
	   (c-major-mode-is 'd-mode)
	   (eq (car res) nil)
	   (save-excursion
	     (goto-char match-pos )
	     (c-backward-syntactic-ws)
	     (eq (char-before) ?\()))
      (setq res (cons 'arglist t)))
    res))
(advice-add 'c-get-fontification-context :around #'d-around--c-get-fontification-context)

;;----------------------------------------------------------------------------
;;; Fixes fontification of constructor parameter lists in D code.

(defun d-special-case-looking-at-2 (orig-fun regexp)
  ;; checkdoc-params: (orig-fun regexp)
  "Advice function for fixing cc-mode handling of D constructors."
  (if (and
       (eq regexp c-not-decl-init-keywords)
       (apply orig-fun (d-make-keywords-re t '("this")) nil)) ; looking-at "this"
      nil
    (apply orig-fun regexp nil)))

(defun d-around--c-font-lock-declarations (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing cc-mode handling of D constructors."
  (if (not (c-major-mode-is 'd-mode))
      (apply orig-fun args)
    (add-function :around (symbol-function 'looking-at)
		  #'d-special-case-looking-at-2)
    (unwind-protect
	(apply orig-fun args)
      (remove-function (symbol-function 'looking-at)
		       #'d-special-case-looking-at-2))))

(advice-add 'c-font-lock-declarations :around #'d-around--c-font-lock-declarations)

;;----------------------------------------------------------------------------
;; Borrowed from https://github.com/josteink/csharp-mode/blob/master/csharp-mode.el
(defun d--syntax-propertize-function (beg end)
  "Apply syntax table properties to special constructs in region BEG to END.
Currently handles `-delimited string literals."
  (save-excursion
    (goto-char beg)
    (while (search-forward "`" end t)
      (let ((in-comment-or-string-p (save-excursion
                                      (goto-char (match-beginning 0))
                                      (or (nth 3 (syntax-ppss))
                                          (nth 4 (syntax-ppss))))))
        (when (not in-comment-or-string-p)
          (let (done)
            (while (and (not done) (< (point) end))
              (skip-chars-forward "^`\\\\" end)
              (cond
               ((= (following-char) ?\\)
                (put-text-property (point) (1+ (point))
                                   'syntax-table (string-to-syntax "."))
                (forward-char 1))
               ((= (following-char) ?\`)
                (forward-char 1)
		(setq done t))))))))))

;;----------------------------------------------------------------------------

(defun d--on-func-identifier ()
  "Version of `c-on-identifier', but also match D constructors."

  (save-excursion
    (skip-syntax-backward "w_")

    (or
     ;; Check for a normal (non-keyword) identifier.
     (and (looking-at c-symbol-start)
	  (or
	   (looking-at (d-make-keywords-re t '("this" "~this")))
	   (not (looking-at c-keywords-regexp)))
	  (point)))))

(defun d-in-knr-argdecl (&optional lim)
  "Modified version of `c-in-knr-argdecl' for d-mode." ;; checkdoc-params: lim
  (save-excursion
    ;; If we're in a macro, our search range is restricted to it.  Narrow to
    ;; the searchable range.
    (let* ((start (point))
	   before-lparen
	   after-rparen
	   (pp-count-out 20)	; Max number of paren/brace constructs before
				; we give up.
	   knr-start
	   c-last-identifier-range)

      (catch 'knr
	(while (> pp-count-out 0) ; go back one paren/bracket pair each time.
	  (setq pp-count-out (1- pp-count-out))
	  (c-syntactic-skip-backward "^)]}=;")
	  (cond ((eq (char-before) ?\))
		 (setq after-rparen (point)))
		((eq (char-before) ?\])
		 (setq after-rparen nil))
		(t ; either } (hit previous defun) or = or no more
					; parens/brackets.
		 (throw 'knr nil)))

	  (if after-rparen
	      ;; We're inside a paren.  Could it be our argument list....?
	      (if
		  (and
		   (progn
		     (goto-char after-rparen)
		     (unless (c-go-list-backward) (throw 'knr nil)) ;
		     ;; FIXME!!!  What about macros between the parens?  2007/01/20
		     (setq before-lparen (point)))

		   ;; It can't be the arg list if next token is ; or {
		   (progn (goto-char after-rparen)
			  (c-forward-syntactic-ws)
			  (not (memq (char-after) '(?\; ?\{ ?\=))))

		   ;; Is the thing preceding the list an identifier (the
		   ;; function name), or a macro expansion?
		   (progn
		     (goto-char before-lparen)
		     (eq (c-backward-token-2) 0)
		     (or (eq (d--on-func-identifier) (point))
			 (and (eq (char-after) ?\))
			      (c-go-up-list-backward)
			      (eq (c-backward-token-2) 0)
			      (eq (d--on-func-identifier) (point)))))

		   ;; Check that we're outside of the template arg list (D-specific).
		   (progn
		     (setq knr-start
			   (progn (goto-char after-rparen)
				  (c-forward-syntactic-ws)
				  (when (eq (char-after) ?\()
				    (c-go-list-forward)
				    (c-forward-syntactic-ws))
				  (point)))
		     (<= knr-start start))

		   ;; (... original c-in-knr-argdecl logic omitted here ...)
		   t)
		  ;; ...Yes.  We've identified the function's argument list.
		  (throw 'knr knr-start)
		;; ...No.  The current parens aren't the function's arg list.
		(goto-char before-lparen))

	    (or (c-go-list-backward)	; backwards over [ .... ]
		(throw 'knr nil))))))))

(defun d-around--c-in-knr-argdecl (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing cc-mode indentation in certain D constructs."
  (apply
   (if (c-major-mode-is 'd-mode)
       #'d-in-knr-argdecl
     orig-fun)
   args))

(advice-add 'c-in-knr-argdecl :around #'d-around--c-in-knr-argdecl)

;;----------------------------------------------------------------------------
;; We can't include "enum" in `c-typedef-decl-kwds', as that will not
;; work well with D manifest constants (enum [TYPE] NAME = VALUE).
;; Instead, omit it from `c-typedef-decl-kwds' (which allows manifest
;; constants to be fontified properly), and handle actual enumerations
;; manually by adding fontification of the enum name as a type name to
;; our version of `c-font-lock-enum-body' below:

(defun d-font-lock-enum-body (limit)
  "Modified version of `c-font-lock-enum-body' for d-mode." ;; checkdoc-params: limit
  (while (c-syntactic-re-search-forward c-enum-clause-introduction-re limit t)
    (when (save-excursion
            (backward-char)
	    (when (c-backward-over-enum-header)
	      ;; Fontify type name here
	      (c-forward-token-2)       ; Over "enum"
	      (c-forward-syntactic-ws)
	      (c-fontify-types-and-refs ((id-start (point)))
		(when (c-forward-type)
		  (c-backward-syntactic-ws)
		  (c-put-font-lock-face id-start
					(point)
					'font-lock-type-face)))
	      t))
      ;; As in the original `c-font-lock-enum-body', fontify the body
      ;; (enum members).
      (c-forward-syntactic-ws)
      (c-font-lock-declarators limit t nil t)))
  nil)

(defun d-around--c-font-lock-enum-body (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing fontification for D enums."
  (apply
   (if (c-major-mode-is 'd-mode)
       #'d-font-lock-enum-body
     orig-fun)
   args))

(advice-add 'c-font-lock-enum-body :around #'d-around--c-font-lock-enum-body)

;;----------------------------------------------------------------------------

(defun d-forward-type (&optional brace-block-too)
  "Modified version of `c-forward-type' for d-mode." ;; checkdoc-params: brace-block-too
  (let ((start (point)) pos res name-res id-start id-end id-range saw-storage-class)

    ;; D: Parse storage classes and similar keywords.
    ;; Technically these are not part of the type, but we parse them here
    ;; because they can substitute the type declaration (for type inference).
    (while (and
            (looking-at (c-lang-const d-storage-class-key))

	    (save-excursion
              (goto-char (match-end 1))
              (c-forward-syntactic-ws)
	      (setq pos (point))
              (looking-at c-identifier-start))) ; Variable name or
                                        ; continuation, but NOT (
      (goto-char pos)
      (setq saw-storage-class t))

    (cond
     ;; D: "this" is not a type, even though it appears at the
     ;; beginning of a "function" (constructor) declaration.
     ((looking-at (d-make-keywords-re t '("this")))
      nil)

     ;; D: Storage class substituting the type (e.g. auto)
     ((and
       saw-storage-class
       (not (looking-at (c-lang-const d-type-modifier-key)))
       (save-excursion
	 (c-forward-token-2)            ; maybe variable/function name
	 (looking-at "[(;=]")))
      (setq res t))

     ;; D: const/immutable/...(...)
     ((looking-at (c-lang-const d-type-modifier-key))
      (when
	  (and
	   ;; Followed by a ( ?
	   (progn
	     (goto-char (match-end 1))
	     (c-forward-syntactic-ws)
	     (looking-at "("))
	   ;; Followed by a type in the parens?
	   (progn
	     (forward-char)
	     (c-forward-syntactic-ws)
	     (c-forward-type))
	   ;; Followed by a closing ) ?
	   (progn
	     (c-forward-syntactic-ws)
	     (looking-at ")")))
	(forward-char)
	(c-forward-syntactic-ws)
	(setq res 'prefix)))

     ;; Identifier
     ((progn
	(setq pos nil)
	(if (looking-at c-identifier-start)
	    (save-excursion
	      (setq id-start (point)
		    name-res (c-forward-name))
	      (when name-res
		(setq id-end (point)
		      id-range c-last-identifier-range))))
	(and (cond ((looking-at c-primitive-type-key)
		    (setq res t))
		   ((c-with-syntax-table c-identifier-syntax-table
		      (looking-at c-known-type-key))
		    (setq res 'known)))
	     (or (not id-end)
		 (>= (save-excursion
		       (save-match-data
			 (goto-char (match-end 1))
			 (c-forward-syntactic-ws)
			 (setq pos (point))))
		     id-end)
		 (setq res nil))))
      ;; Looking at a primitive or known type identifier.  We've
      ;; checked for a name first so that we don't go here if the
      ;; known type match only is a prefix of another name.

      (setq id-end (match-end 1))

      (when (and c-record-type-identifiers
		 (or c-promote-possible-types (eq res t)))
	(c-record-type-id (cons (match-beginning 1) (match-end 1))))

      (unless (save-match-data (c-forward-keyword-clause 1))
        (if pos
            (goto-char pos)
          (goto-char (match-end 1))
          (c-forward-syntactic-ws))))

     (name-res
      (cond ((eq name-res t)
	     ;; A normal identifier.
	     (goto-char id-end)
	     (if (or res c-promote-possible-types)
		 (progn
		   (c-add-type id-start id-end)
		   (when (and c-record-type-identifiers id-range)
		     (c-record-type-id id-range))
		   (unless res
		     (setq res 'found)))
	       (setq res (if (c-check-type id-start id-end)
			     ;; It's an identifier that has been used as
			     ;; a type somewhere else.
			     'found
			   ;; It's an identifier that might be a type.
			   'maybe))))
	    (t
	     ;; Otherwise it's an operator identifier, which is not a type.
	     (goto-char start)
	     (setq res nil)))))

    (when res
      ;; D: Skip over template parameters, if any
      (when (looking-at "!")
	(forward-char)
	(c-forward-syntactic-ws)
	(c-forward-sexp)
	(c-forward-syntactic-ws))

      ;; D: Descend into scope names
      (when (looking-at "[.]")
	(forward-char)
	(c-forward-syntactic-ws)
	(unless (d-forward-type)
	  (setq res nil)))

      ;; Step over any type suffix operator.  Do not let the existence
      ;; of these alter the classification of the found type, since
      ;; these operators typically are allowed in normal expressions
      ;; too.
      (when c-opt-type-suffix-key	; e.g. "..."
	(while (looking-at c-opt-type-suffix-key)
	  (goto-char (match-end 1))
	  (c-forward-syntactic-ws)))

      (when (and c-record-found-types (memq res '(known found)) id-range)
	(setq c-record-found-types
	      (cons id-range c-record-found-types))))

    ;;(message "c-forward-type %s -> %s: %s" start (point) res)

    (unless res
      (when saw-storage-class
	(goto-char start)))

    res))

(defun d-around--c-forward-type (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing fontification for D enums."
  (apply
   (if (c-major-mode-is 'd-mode)
       #'d-forward-type
     orig-fun)
   args))

(advice-add 'c-forward-type :around #'d-around--c-forward-type)

;;----------------------------------------------------------------------------

(c-lang-defconst d-flat-decl-maybe-block-kwds
  ;; Keywords which don't introduce a scope, and may or may not be
  ;; followed by a {...} block.
  d (append (c-lang-const c-modifier-kwds)
	    (list "else" ; for version / static if
		  "if" ; static if
		  "version")))
(c-lang-defconst d-flat-decl-maybe-block-re
  d (c-make-keywords-re t (c-lang-const d-flat-decl-maybe-block-kwds)))

(defun d-update-brace-stack (stack from to)
  "Modified version of `c-update-brace-stack' for d-mode." ;; checkdoc-params: (stack from to)
  ;; Given a brace-stack which has the value STACK at position FROM, update it
  ;; to its value at position TO, where TO is after (or equal to) FROM.
  ;; Return a cons of either TO (if it is outside a literal) and this new
  ;; value, or of the next position after TO outside a literal and the new
  ;; value.
  (let (match kwd-sym (prev-match-pos 1)
	      (s (cdr stack))
	      (bound-<> (car stack)))
    (save-excursion
      (cond
       ((and bound-<> (<= to bound-<>))
	(goto-char to))			; Nothing to do.
       (bound-<>
	(goto-char bound-<>)
	(setq bound-<> nil))
       (t (goto-char from)))
      (while (and (< (point) to)
		  (c-syntactic-re-search-forward
		   (if (<= (car s) 0)
		       c-brace-stack-thing-key
		     c-brace-stack-no-semi-key)
		   to 'after-literal)
		  (> (point) prev-match-pos)) ; prevent infinite loop.
	(setq prev-match-pos (point))
	(setq match (match-string-no-properties 1)
	      kwd-sym (c-keyword-sym match))
	(cond
	 ((and (equal match "{")
	       (progn (backward-char)
		      (prog1 (looking-at "\\s(")
			(forward-char))))
	  (setq s (if s
		      ;; D: Constructs such as "version", "static if", or
		      ;; "extern(...)" may or may not enclose their declarations
		      ;; in a {...} block. For this reason, we can't blindly
		      ;; update the cc-mode brace stack when we see these keywords
		      ;; (otherwise, if they are not immediately succeeded by a
		      ;; {...} block, then the brace stack change will apply to
		      ;; the next encountered {...} block such as that of a
		      ;; function's).
		      (if (save-excursion
			    (backward-char)
			    (c-backward-syntactic-ws)
			    (when (eq (char-before) ?\))
			      (c-backward-sexp)
			      (c-backward-syntactic-ws))
			    (c-backward-token-2)
			    (looking-at (c-lang-const d-flat-decl-maybe-block-re)))
			  ;; D: Keep the brace stack state from the parent
			  ;; context. I.e., the contents of a "static if" at the
			  ;; top level should remain top-level, but in a function,
			  ;; it should remain non-top-level.
			  s
			(cons (if (<= (car s) 0)
				  1
				(1+ (car s)))
			      (cdr s)))
		    (list 1))))
	 ((and (equal match "}")
	       (progn (backward-char)
		      (prog1 (looking-at "\\s)")
			(forward-char))))
	  (setq s
		(cond
		 ((and s (> (car s) 1))
		  (cons (1- (car s)) (cdr s)))
		 ((and (cdr s) (eq (car s) 1))
		  (cdr s))
		 (t s))))
	 ((and (equal match ":")
	       s
	       (eq (car s) 0))
	  (setq s (cons -1 (cdr s))))
	 ((and (equal match ",")
	       (eq (car s) -1)))	; at "," in "class foo : bar, ..."
	 ;; D: Ignore ")", which can be part of parameter lists
	 ((member match '(";" ","))
	  (when (and s (cdr s) (<= (car s) 0))
	    (setq s (cdr s))))
	 ((c-keyword-member kwd-sym 'c-flat-decl-block-kwds)
	  (push 0 s))))
      ;; The failing `c-syntactic-re-search-forward' may have left us in the
      ;; middle of a token, which might be a significant token.  Fix this!
      (c-beginning-of-current-token)
      (cons (point)
	    (cons bound-<> s)))))

(defun d-around--c-update-brace-stack (orig-fun &rest args)
  ;; checkdoc-params: (orig-fun args)
  "Advice function for fixing cc-mode handling of certain D constructs."
  (apply
   (if (c-major-mode-is 'd-mode)
       #'d-update-brace-stack
     orig-fun)
   args))

(advice-add 'c-update-brace-stack :around #'d-around--c-update-brace-stack)

;;----------------------------------------------------------------------------
;; Support for fontifying module name(s) after a module or import keyword.

(defun d-forward-module-clause ()
  "Fontify the module name(s) after a module or import keyword."
  (let (safe-pos pos)
    (goto-char (match-end 1))
    (while
	(progn
	  (c-forward-syntactic-ws)
	  (setq safe-pos (point))
	  (cond
	   ((looking-at c-identifier-start)
	    ;; identifier
	    (setq c-last-identifier-range nil)
	    (forward-char)
	    (c-end-of-current-token)
	    (when c-record-type-identifiers
	      (c-record-ref-id (cons safe-pos (point))))
	    t)
	   ;; . or , or = (keep fontifying)
	   ((memq (char-after) '(?. ?, ?=))
	    (forward-char)
	    t)
	   ;; ; or : or anything else weird
	   (t
	    nil))))
    (goto-char safe-pos)
    t))

;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; compilation-mode support ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------------------------------

;;; Patterns to recognize the compiler generated messages

(defun d-mode-add-dmd-message-pattern (expr level symbol)
  "Register DMD `compile' pattern for an error level.

EXPR is the `rx' message sub-expression indicating the error level LEVEL.
The expression is added to `compilation-error-regexp-alist' and
`compilation-error-regexp-alist-alist' as SYMBOL."
  (add-to-list
   'compilation-error-regexp-alist-alist
   `(,symbol
     ,(rx-to-string
      `(and
	line-start
	(group-n 1 (one-or-more any))		; File name
	"("
	(group-n 2 (one-or-more digit))		; Line number
	(zero-or-one
	 ","
	 (group-n 3 (one-or-more digit)))	; Column number
	"): "
	,expr
	(group-n 4 (one-or-more nonl))		; Message
	line-end))
     1 2 3 ,level 4))
  (add-to-list 'compilation-error-regexp-alist symbol))

(d-mode-add-dmd-message-pattern "Error: "          2 'dmd-error       )
(d-mode-add-dmd-message-pattern "Warning: "        1 'dmd-warning     )
(d-mode-add-dmd-message-pattern "Deprecation: "    1 'dmd-deprecation )
(d-mode-add-dmd-message-pattern '(one-or-more " ") 0 'dmd-continuation)

;; The following regexp recognizes messages generated by the D runtime for
;; unhandled exceptions (e.g. assert failures).

(add-to-list 'compilation-error-regexp-alist-alist
             '(d-exceptions
               "^[a-zA-Z0-9.]*?@\\(.*?\\)(\\([0-9]+\\)):"
               1 2 nil 2))
(add-to-list 'compilation-error-regexp-alist 'd-exceptions)


;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; imenu support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------------------------------

;; Old imenu implementation - regular expressions:

(eval-when-compile
  (defconst d--imenu-rx-def-start
    '(seq
      ;; Conditionals
      (zero-or-one
       "else"
       (zero-or-more space))
      (zero-or-one
       "version"
       (zero-or-more space)
       "("
       (zero-or-more space)
       (one-or-more (any "a-zA-Z0-9_"))
       (zero-or-more space)
       ")"
       (zero-or-more space))

      (zero-or-more
       (or
	word-start
	(or
	 ;; StorageClass
	 "deprecated"
	 "static"
	 "extern"
	 "abstract"
	 "final"
	 "override"
	 "synchronized"
	 "scope"
	 "nothrow"
	 "pure"
	 "ref"
	 (seq
	  (or
	   "extern"
	   "deprecated"
	   "package"
	   )
	  (zero-or-more space)
	  "("
	  (zero-or-more space)
	  (one-or-more (not (any "()")))
	  (zero-or-more space)
	  ")")

	 ;; VisibilityAttribute
	 "private"
	 "package"
	 "protected"
	 "public"
	 "export"
	 )

	;; AtAttribute
	(seq
	 "@"
	 (one-or-more (any "a-zA-Z0-9_"))
	 (zero-or-one
	  (zero-or-more space)
	  "("
	  (zero-or-more space)
	  (one-or-more (not (any "()")))
	  (zero-or-more space)
	  ")")))
       (zero-or-more space))

      )))

(defconst d-imenu-method-name-pattern
  (rx
   ;; Whitespace
   bol
   (zero-or-more space)

   (eval d--imenu-rx-def-start)

   ;; Type
   (group
    (one-or-more (any "a-zA-Z0-9_.*![]()")))
   (one-or-more space)

   ;; Function name
   (group
    (one-or-more (any "a-zA-Z0-9_")))
   (zero-or-more space)

   ;; Type arguments
   (zero-or-one
    "(" (zero-or-more (not (any ")"))) ")"
    (zero-or-more (any " \t\n")))

   ;; Arguments
   "("
   (zero-or-more (not (any "()")))
   (zero-or-more
    "("
    (zero-or-more (not (any "()")))
    ")"
    (zero-or-more (not (any "()"))))
   ")"
   (zero-or-more (any " \t\n"))

   ;; Pure/const etc.
   (zero-or-more
    (one-or-more (any "a-z@"))
    symbol-end
    (zero-or-more (any " \t\n")))

   (zero-or-more
    "//"
    (zero-or-more not-newline)
    (zero-or-more space))

   ;; ';' or 'if' or '{'
   (or
    ";"
    (and
     (zero-or-more (any " \t\n"))
     (or "if" "{")))
   ))

(defun d-imenu-method-index-function ()
  "Find D function declarations for imenu."
  (and
   (let ((pt))
     (setq pt (re-search-backward d-imenu-method-name-pattern nil t))
     ;; The method name regexp will match lines like
     ;; "return foo(x);" or "static if(x) {"
     ;; so we exclude type name 'static' or 'return' here
     (while (let ((type (match-string 1))
		  (name (match-string 2)))
              (and pt name
                   (save-match-data
		     (or
		      (string-match (c-lang-const d-non-func-type-kwds-re) type)
		      (string-match (c-lang-const d-non-func-name-kwds-re) name)))))
       (setq pt (re-search-backward d-imenu-method-name-pattern nil t)))
     pt)
   ;; Do not count invisible definitions.
   (let ((invis (invisible-p (point))))
     (or (not invis)
         (progn
           (while (and invis
                       (not (bobp)))
             (setq invis (not (re-search-backward
                               d-imenu-method-name-pattern nil 'move))))
           (not invis))))))

(defvar d-imenu-generic-expression
  `(("*Classes*"
     ,(rx
       bol
       (zero-or-more space)
       (eval d--imenu-rx-def-start)
       word-start
       "class"
       (one-or-more (syntax whitespace))
       (submatch
	(one-or-more
	 (any ?_
	      (?0 . ?9)
	      (?A . ?Z)
	      (?a . ?z)))))
     1)
    ("*Interfaces*"
     ,(rx
       bol
       (zero-or-more space)
       (eval d--imenu-rx-def-start)
       word-start
       "interface"
       (one-or-more (syntax whitespace))
       (submatch
	(one-or-more
	 (any ?_
	      (?0 . ?9)
	      (?A . ?Z)
	      (?a . ?z)))))
     1)
    ("*Structs*"
     ,(rx
       bol
       (zero-or-more space)
       (eval d--imenu-rx-def-start)
       word-start
       "struct"
       (one-or-more (syntax whitespace))
       (submatch
	(one-or-more
	 (any ?_
	      (?0 . ?9)
	      (?A . ?Z)
	      (?a . ?z)))))
     1)
    ("*Templates*"
     ,(rx
       bol
       (zero-or-more space)
       (eval d--imenu-rx-def-start)
       (zero-or-one
	"mixin"
	(one-or-more (syntax whitespace)))
       word-start
       "template"
       (one-or-more (syntax whitespace))
       (submatch
	(one-or-more
	 (any ?_
	      (?0 . ?9)
	      (?A . ?Z)
	      (?a . ?z)))))
     1)
    ("*Enums*"
     ,(rx
       bol
       (zero-or-more space)
       (eval d--imenu-rx-def-start)
       word-start
       "enum"
       (one-or-more (syntax whitespace))
       (submatch
	(one-or-more
	 (any ?_
	      (?0 . ?9)
	      (?A . ?Z)
	      (?a . ?z))))
       (zero-or-more (any " \t\n"))
       (or ":" "{"))
     1)
    ;; NB: We can't easily distinguish aliases declared outside
    ;; functions from local ones, so just search for those that are
    ;; declared at the beginning of lines.
    ("*Aliases*"
     ,(rx
       bol
       (eval d--imenu-rx-def-start)
       "alias"
       (one-or-more (syntax whitespace))
       (submatch
	(one-or-more
	 (any ?_
	      (?0 . ?9)
	      (?A . ?Z)
	      (?a . ?z))))
       (zero-or-more (syntax whitespace))
       (zero-or-one
        "("
        (zero-or-more (not (any "()")))
        ")"
        (zero-or-more (syntax whitespace)))
       "=")
     1)
    ("*Aliases*"
     ,(rx
       bol
       (eval d--imenu-rx-def-start)
       "alias"
       (one-or-more (syntax whitespace))
       (one-or-more
	(not (any ";")))
       (one-or-more (syntax whitespace))
       (submatch
	(one-or-more
	 (any ?_
	      (?0 . ?9)
	      (?A . ?Z)
	      (?a . ?z))))
       (zero-or-more (syntax whitespace))
       ";"
       (zero-or-more (syntax whitespace))
       (or
	eol
	"//"
	"/*")
       )
     1)
    (nil d-imenu-method-index-function 2)))

;;----------------------------------------------------------------------------
;; New imenu implementation - use cc-mode machinery:

(defun d-imenu-create-index-function ()
  "Create imenu entries for D-mode."
  (goto-char (point-min))
  (c-save-buffer-state
      (d-spots last-spot (d-blocks (make-hash-table)))
    (c-find-decl-spots
     (point-max)
     c-decl-start-re
     (eval c-maybe-decl-faces)
     (lambda (match-pos inside-macro toplev)
       (when toplev
	 (let* ((got-context
		 (c-get-fontification-context
		  match-pos nil toplev))
		(context (car got-context))
		(decl-or-cast
		 (when (eq context 'top)
		   (c-forward-decl-or-cast-1
		    match-pos
		    context
		    nil ; last-cast-end
		    ))))
	   (when (and decl-or-cast (not (eq (car decl-or-cast) last-spot)))
	     (let* ((decl-end (point))
		    (id-start (progn
				(goto-char (car decl-or-cast))
				(when (eq (char-after) ?=)
				  (c-backward-syntactic-ws)
				  (c-simple-skip-symbol-backward))
				(point)))
		    (id-end (progn
			      (goto-char id-start)
			      (when (d-forward-name)
				(c-backward-syntactic-ws)
				(point))))
		    (name (when id-end
			    (buffer-substring-no-properties id-start id-end)))
		    (id-prev-token (progn
				     (goto-char id-start)
				     (c-backward-syntactic-ws)
				     (let ((end (point)))
				       (when (c-simple-skip-symbol-backward)
					 (buffer-substring-no-properties (point) end)))))
		    (type-start (cadddr decl-or-cast))
		    (type-prev-token (when type-start
				       (goto-char type-start)
				       (c-backward-syntactic-ws)
				       (let ((end (point)))
					 (when (c-simple-skip-symbol-backward)
					   (buffer-substring-no-properties (point) end)))))
		    (next-char (when id-end
				 (goto-char id-end)
				 (c-forward-syntactic-ws)
				 (char-after)))
		    (res (cond
			  ((null name)
			   nil)
			  ((equal id-prev-token "else")
			   nil) ; false positive after else
			  ((equal name "{")
			   nil) ; false positive with decl-start keyword and {...} group
			  ((equal id-prev-token "enum")
			   '("Enums" t))
			  ((equal id-prev-token "class")
			   '("Classes" t))
			  ((equal id-prev-token "struct")
			   '("Structs" t))
			  ((equal id-prev-token "template")
			   '("Templates" t))
			  ((equal id-prev-token "alias")
			   '("Aliases" nil))
			  ((equal type-prev-token "alias")
			   '("Aliases" nil)) ; old-style alias
			  ((memq next-char '(?\; ?= ?,))
			   nil) ; '("variable" nil))
			  ((member name '("import" "if"))
			   nil) ; static import/if
			  ((memq next-char '(?\())
			   '(nil t)) ; function
			  (t ; unknown
			   (list id-prev-token nil))))
		    (kind (car res))
		    (have-block (cadr res))
		    (paren-state (when res (c-parse-state)))
		    (outer-brace match-pos)
		    d-context
		    d-fqname)

	       (when res
		 (when paren-state
		   ;; Find brace with known context
		   (while (and outer-brace
			       (not d-context))
		     (setq outer-brace (c-most-enclosing-brace paren-state outer-brace))
		     (setq d-context (gethash outer-brace d-blocks))))

		 (setq d-fqname (if d-context (concat d-context "." name) name))

		 (when have-block
		   (goto-char decl-end)
		   (when (and (c-syntactic-re-search-forward "[{};]" nil t)
			      (eq (char-before) ?{))
		     (puthash (1- (point)) d-fqname d-blocks)))

		 (setq last-spot (car decl-or-cast)
		       d-spots
		       (cons
			(if kind
			    (cons kind (list (cons d-fqname id-start)))
			  (cons d-fqname id-start))
			d-spots)))))))))
    (nreverse d-spots)))

;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Major mode definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------------------------------

(defcustom d-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in D mode.

Each list item should be a regexp matching a single identifier."
  :type '(repeat regexp)
  :group 'd-mode)

(c-lang-defconst c-basic-matchers-after
  d (append
     ;; D module and import statements
     (list (c-make-font-lock-BO-decl-search-function
            (c-make-keywords-re t (c-lang-const c-ref-list-kwds))
            '((c-fontify-types-and-refs ()
        	(d-forward-module-clause)
        	(if (> (point) limit) (goto-char limit))))))
     ;; cc-mode defaults
     (c-lang-const c-basic-matchers-after)))

(defconst d-font-lock-keywords-1 (c-lang-const c-matchers-1 d)
  "Minimal highlighting for D mode.")

(defconst d-font-lock-keywords-2 (c-lang-const c-matchers-2 d)
  "Fast normal highlighting for D mode.")

(defconst d-font-lock-keywords-3 (c-lang-const c-matchers-3 d)
  "Accurate normal highlighting for D mode.")

(defvar d-font-lock-keywords d-font-lock-keywords-3
  "Default expressions to highlight in D mode.")

(defun d-font-lock-keywords-2 ()
  "Function to get fast normal highlighting for D mode."
  (c-compose-keywords-list d-font-lock-keywords-2))
(defun d-font-lock-keywords-3 ()
  "Function to get accurate normal highlighting for D mode."
  (c-compose-keywords-list d-font-lock-keywords-3))
(defun d-font-lock-keywords ()
  "Function to get default expressions to highlight in D mode."
  (c-compose-keywords-list d-font-lock-keywords))

(defvar d-mode-syntax-table nil
  "Syntax table used in d-mode buffers.")
(or d-mode-syntax-table
    (setq d-mode-syntax-table
	 (let ((table (funcall (c-lang-const c-make-mode-syntax-table d))))
	   ;; Make it recognize D `backquote strings`
	   (modify-syntax-entry ?` "\"" table)

	   ;; Make it recognize D's nested /+ +/ comments
	   (modify-syntax-entry ?+  ". 23n"   table)
	   table)))

(defvar d-mode-abbrev-table nil
  "Abbreviation table used in d-mode buffers.")
(c-define-abbrev-table 'd-mode-abbrev-table
  ;; Use the abbrevs table to trigger indentation actions
  ;; on keywords that, if they occur first on a line, might alter the
  ;; syntactic context.
  ;; Syntax for abbrevs is:
  ;; ( pattern replacement command initial-count)
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar d-mode-map ()
  "Keymap used in d-mode buffers.")
(if d-mode-map
    nil
  (setq d-mode-map (c-make-inherited-keymap))
  ;; Add bindings which are only useful for D
  ;; (define-key d-mode-map "\C-c\C-e"  'd-cool-function)
  )

(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu.  The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  d `(["Comment Out Region"     comment-region
       (c-fn-region-is-active-p)]
      ["Uncomment Region"       (comment-region (region-beginning)
						(region-end) '(4))
       (c-fn-region-is-active-p)]
      ["Indent Expression"      c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region"  c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement"     c-beginning-of-statement t]
      ["Forward Statement"      c-end-of-statement t]
      "----"
      ("Toggle..."
       ["Syntactic indentation" c-toggle-syntactic-indentation
	:style toggle :selected c-syntactic-indentation]
       ["Electric mode"         c-toggle-electric-state
	:style toggle :selected c-electric-flag]
       ["Auto newline"          c-toggle-auto-newline
	:style toggle :selected c-auto-newline]
       ["Hungry delete"         c-toggle-hungry-state
	:style toggle :selected c-hungry-delete-key]
       ["Subword mode"          c-subword-mode
	:style toggle :selected (and (boundp 'c-subword-mode)
                                     c-subword-mode)])))

(easy-menu-define d-menu d-mode-map "D Mode Commands"
  (cons "D" (c-lang-const c-mode-menu d)))

;;----------------------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; Custom variables
;;;###autoload
(defcustom d-mode-hook nil
  "*Hook called by `d-mode'."
  :type 'hook
  :group 'c)

;;;###autoload
(define-derived-mode d-mode prog-mode "D"
  "Major mode for editing code written in the D Programming Language.

See http://dlang.org for more information about the D language.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `d-mode-hook'.

Key bindings:
\\{d-mode-map}"
  (c-initialize-cc-mode t)
  (setq local-abbrev-table d-mode-abbrev-table
        abbrev-mode t)
  (use-local-map d-mode-map)
  (c-init-language-vars d-mode)
  (when (fboundp 'c-make-noise-macro-regexps)
    (c-make-noise-macro-regexps))

  ;; Generate a function that applies D-specific syntax properties.
  ;; Concretely, inside back-quoted string literals the backslash
  ;; character '\' is treated as a punctuation symbol.  See help for
  ;; syntax-propertize-rules function for more information.
  (setq-local
   syntax-propertize-function
   #'d--syntax-propertize-function)

  (c-common-init 'd-mode)
  (easy-menu-add d-menu)
  (c-run-mode-hooks 'c-mode-common-hook 'd-mode-hook)
  (c-update-modeline)
  (if (fboundp 'c-get-fontification-context)
      (cc-imenu-init nil #'d-imenu-create-index-function)
    (cc-imenu-init d-imenu-generic-expression)))

;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Optional features ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------------------------------

;; Support for "Adjusting Alignment Rules for UCFS-Chains in D",
;; cf. https://stackoverflow.com/questions/25797945/adjusting-alignment-rules-for-ucfs-chains-in-d
;;
;; The code here was originally created by Sergei Nosov
;; (https://stackoverflow.com/users/1969069/sergei-nosov) based on the c-lineup-cascaded-calls code, see
;; StackOverflow, and then amended by Nordlöw (https://stackoverflow.com/users/683710/nordl%C3%B6w) it
;; provides a function that people can make use of in their d-mode-hook thus:
;;
;; (add-hook 'd-mode-hook 'd-setup-cascaded-call-indentation)

(defun d-setup-cascaded-call-indentation ()
  "Set up `d-lineup-cascaded-calls'."
  (add-to-list 'c-offsets-alist '(arglist-cont-nonempty . d-lineup-cascaded-calls))
  (add-to-list 'c-offsets-alist '(statement-cont . d-lineup-cascaded-calls)))

(defun d-lineup-cascaded-calls (langelem)
  "D version of `c-lineup-cascaded-calls'.

This version accounts for optional parenthesis and compile-time
parameters in function calls." ;; checkdoc-params: langelem

  (if (and (eq (c-langelem-sym langelem) 'arglist-cont-nonempty)
           (not (eq (c-langelem-2nd-pos c-syntactic-element)
                    (c-most-enclosing-brace (c-parse-state)))))
      ;; The innermost open paren is not our one, so don't do
      ;; anything. This can occur for arglist-cont-nonempty with
      ;; nested arglist starts on the same line.
      nil

    (save-excursion
      (back-to-indentation)
      (let ((operator (and (looking-at "\\.")
                           (regexp-quote (match-string 0))))
            (stmt-start (c-langelem-pos langelem)) col)

        (when (and operator
                   (looking-at operator)
                   (or (and
                        (zerop (c-backward-token-2 1 t stmt-start))
                        (eq (char-after) ?\()
                        (zerop (c-backward-token-2 2 t stmt-start))
                        (looking-at operator))
                       (and
                        (zerop (c-backward-token-2 1 t stmt-start))
                        (looking-at operator))
                       (and
                        (zerop (c-backward-token-2 1 t stmt-start))
                        (looking-at operator))
                       )
                   )
          (setq col (current-column))

          (while (or (and
                      (zerop (c-backward-token-2 1 t stmt-start))
                      (eq (char-after) ?\()
                      (zerop (c-backward-token-2 2 t stmt-start))
                      (looking-at operator))
                     (and
                      (zerop (c-backward-token-2 1 t stmt-start))
                      (looking-at operator))
                     (and
                      (zerop (c-backward-token-2 1 t stmt-start))
                      (looking-at operator))
                     )
            (setq col (current-column)))

          (vector col))))))

;;----------------------------------------------------------------------------

(defun d-lineup-arglists (elem)
  "Line up runtime argument list with compile-time argument list.

Works with: func-decl-cont." ;; checkdoc-params: (elem)
  (save-excursion
    (beginning-of-line)
    (c-backward-syntactic-ws)
    (let ((c (char-before)))
      (cond
       ((eq c ?\))
	(c-go-list-backward)
	(vector (current-column)))
       (t
	"+")))))

;;----------------------------------------------------------------------------

(provide 'd-mode)

;;; d-mode.el ends here
