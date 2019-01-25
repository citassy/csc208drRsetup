#!r7rs

;;; A library for computing small powers of numbers.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created June 19, 2018
;;; last revised August 8, 2018

(define-library (discrete powers)
  (export square cube zenzizenzic sursolid)
  (import (scheme base))
  (begin

    (define (cube number)
      (* number number number))

    (define (zenzizenzic number)
      (square (square number)))

    (define (sursolid number)
      (* (square number) (cube number)))))

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
