#!r7rs

;;; Random-number generation for R7RS Scheme
;;;
;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu
;;;
;;; created June 27, 2018
;;; last revised August 8, 2018

(define-library (csc-151 random)
  (export random)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file))
  (begin
    (define random
      (let ((store-size 1048576)
            (unused 0))
        (let ((store (make-bytevector store-size)))
          (case-lambda
            (()
             (when (< unused 4)
               (let ((source (open-binary-input-file "/dev/urandom")))
                 (read-bytevector! store source)
                 (set! unused store-size)
                 (close-port source)))            
             (let ((number (+ (* (bytevector-u8-ref store (- unused 1))
                                 16777216)
                              (* (bytevector-u8-ref store (- unused 2))
                                 65536)
                              (* (bytevector-u8-ref store (- unused 3))
                                 256)
                              (bytevector-u8-ref store (- unused 4)))))
               (set! unused (- unused 4))
               (/ (inexact number) 4294967296.0)))
            ((bound)
             (exact (floor (* bound (random)))))
            ((lower upper)
             (+ lower (exact (floor (* (- upper lower) (random))))))))))))
                            
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

