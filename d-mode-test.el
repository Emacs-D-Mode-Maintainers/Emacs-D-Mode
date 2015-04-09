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

(ert-deftest d-mode-basic ()
  (should (equal (car '("abc" "edf")) "abc")))

(provide 'd-mode-test)
