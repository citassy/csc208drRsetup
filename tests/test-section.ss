#!r7rs

;;; Tests for the (csc-151 section) library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created August 23, 2011
;;; last revised January 24, 2019

(import (scheme base)
        (csc-151 extensions)
        (discrete testing))

(suite section ()
  (test typical
    (section expt <> 3)
    1 (procedure?)
    (lambda (cube)
      (suite section-typical-internal ()
        (test cube
          (cube 8)
          1 (exact-integer?)
          (lambda (result)
            (= result 512))))))
  (test no-slots
    (section expt 5 3)
    1 (procedure?)
    (lambda (thunk)
      (suite no-slots-internal ()
        (test thaw-thunk
          (thunk)
          1 (exact-integer?)
          (lambda (result)
            (= result 125))))))
  (test variable-arity
    (section - <> 259 <...>)
    1 (procedure?)
    (lambda (sectioned)
      (suite section-variable-arity ()
        (test no-extra-arguments
          (sectioned 821)
          1 (exact-integer?)
          (lambda (result)
            (= result (- 821 259))))
        (test some-extra-arguments
          (sectioned 723 482 805 263)
          1 (exact-integer?)
          (lambda (result)
            (= result (- 723 259 482 805 263)))))))
  (test all-slots
    (section <> <> <>)
    1 (procedure?)
    (lambda (binary-call)
      (suite all-slots-internal ()
        (test call
          (binary-call expt 291 880)
          1 (exact-integer?)
          (lambda (result)
            (= result (expt 291 880))))))))

;;; copyright © 2011, 2017, 2018, 2019 John David Stone

;;; This program is free software.
;;; You may redistribute it and/or modify it
;;; under the terms of the GNU General Public License
;;; as published by the Free Software Foundation -- 
;;; either version 3 of the License,
;;; or (at your option) any later version.

;;; This program is distributed
;;; in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY --
;;; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.

;;; You should have received a copy
;;; of the GNU General Public License
;;; along with this program.
;;; If not, it is available on the World Wide Web
;;; at https://www.gnu.org/licenses/gpl.html.
