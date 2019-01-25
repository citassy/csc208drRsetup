#!r7rs

;;; A library of utility procedures for calculations in discrete mathematics

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created June 25, 2018
;;; last revised October 30, 2018

(define-library (discrete utilities)
  (export assert cube natural-number? factorial fold-left
          fold-right)
  (import (scheme base)
          (scheme write))

  (begin

    ;; Extend Scheme to support assert-expressions.
    ;; An assert-expression evaluates its first subexpression
    ;; and raises an error if the value of the expression is #f.
    ;; The second subexpression, which is not evaluated,
    ;; should be an identifier
    ;; indicating the location of the failed assertion.

    (define-syntax assert
      (syntax-rules ()
        ((assert condition location)
         (unless condition
           (let ((condition-string (open-output-string)))
             (display 'condition condition-string)
             (display " in " condition-string)
             (display 'location condition-string)
             (newline condition-string)
             (let ((message (string-append "Assertion failed: "
                                           (get-output-string condition-string))))
               (close-port condition-string)
               (error message condition)))))))

    ;; Compute and return the cube of a given number.

    (define (cube number)
      (assert (number? number) cube)
      (* number number number))

    ;; Determine whether a given value is a natural number.

    (define (natural-number? something)
      (and (integer? something)
           (exact? something)
           (not (negative? something))))

    ;; Compute and return the factorial of a given natural number.

    (define (factorial number)
      (assert (natural-number? number) factorial)
      (let loop ((rest number)
                 (product-so-far 1))
        (if (zero? rest)
            product-so-far
            (loop (- rest 1) (* product-so-far rest)))))

    ;; The fold-left procedure reduces a given list
    ;; by applying a given binary procedure repeatedly,
    ;; starting with a given base value
    ;; and merging in elements of the list in right-to-left order.

    (define (fold-left proc ls starter)
      (let loop ((rest ls)
                 (so-far starter))
        (if (null? rest)
            so-far
            (loop (cdr rest) (proc (car rest) so-far)))))

    ;; The fold-right procedure reduces a given list
    ;; by applying a given binary procedure repeatedly,
    ;; starting with a given base value
    ;; and merging in elements of the list in left-to-right order.

    (define (fold-right proc ls starter)
      (let loop ((rest ls))
        (if (null? rest)
            starter
            (proc (car rest) (loop (cdr rest))))))))

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
