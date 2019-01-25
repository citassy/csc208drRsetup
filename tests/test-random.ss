#!r7rs

;;; Tests for the (csc-151 random) library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created June 29, 2018
;;; last revised January 24, 2019

(import (scheme base)
        (csc-151 extensions)
        (discrete testing))

(suite random ()
  (test zero-arguments
    (random)
    1 (number?)
    (conjoin (left-section <= 0) (right-section < 1)))
  (test varying-results
    (values (random) (random))
    2 (number? number?)
    (negate =))
  (test bin-test
    (let ((bins (make-vector 10 0)))
      (do ((counter 0 (+ counter 1)))
          ((= counter 1000000) bins)
        (let ((bin (exact (floor (* (random) 10)))))
          (vector-set! bins bin (+ (vector-ref bins bin) 1)))))
    1 (vector?)
    (lambda (bins)
      (all (right-section <= 1000)
           (map (o abs (right-section - 100000))
                (vector->list bins)))))
  (test single-argument-one
    (random 1)
    1 ((conjoin integer? exact?))
    zero?)
  (test single-argument-two
    (random 2)
    1 ((conjoin integer? exact?))
    (section <= 0 <> 2))
  (test single-argument-large
    (random 1048)
    1 ((conjoin integer? exact?))
    (section <= 0 <> 1047))
  (test two-arguments-almost-equal
    (random 383 384)
    1 ((conjoin integer? exact?))
    (left-section = 383))
  (test two-arguments-separated
    (random 1158679 3034218)
    1 ((conjoin integer? exact?))
    (section <= 1158679 <> 3034217)))
  
;;; Copyright © 2018, 2019 John David Stone

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
