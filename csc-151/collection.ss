#!r7rs

;;; In the "Functional Problem Solving" course, CSC 151,
;;; at Grinnell College
;;; students learn a subset of the Racket programming language
;;; to express algorithms and implement programs.
;;; Some of the procedures and syntaxes
;;; that they use frequently and rely on
;;; are not predefined in R7RS (small) Scheme.
;;; I have implemented those procedures and syntaxes
;;; and collected them in this library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created June 28, 2018
;;; last revised August 8, 2018

(define-library (csc-151 collection)

  (export

    ;; Procedures ported from Professor Rebelsky's csc151 packages:

    ;; csc151/lists
    index-of iota reduce reduce-left reduce-right tally-all take-random

    ;; csc151/hop
    all any comparator left-section l-s right-section r-s

    ;; csc151/numbers
    decrement double increment

    ;; csc151/files
    file->chars file->lines file->words read-word read-until skip-char

    ;; An extra (used in the definition of read-until):

    char-in-string?

    ;; Reimplemented components of Professor Rebelsky's csc 151 packages:

    o section

    ;; Reimplemented Racket procedures and values:

    random eof pi gensym sort take drop null filter
    negate conjoin disjoin

    ;; Racket procedures that have different names under R7RS:

    (rename exact inexact->exact)
    (rename inexact exact->inexact))

  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme inexact)
          (csc-151 rebelsky)
          (csc-151 section)
          (csc-151 random))

  (begin

    ;;; The o procedure takes any number of procedures 
    ;;; and returns their composition.
    ;;;
    ;;; The procedures can be of any arity
    ;;; and return any number of values,
    ;;; but each procedure must be able to receive as arguments
    ;;; all of the values produced by the procedure
    ;;; with which it is composed.

    (define o
      (lambda procedures
        (lambda arguments
          (let loop ((rest procedures))
            (if (null? rest)
                (apply values arguments)
                (let-values ((recursive-results (loop (cdr rest))))
                  (apply (car rest) recursive-results)))))))


    ;; In Racket, the identifier eof is predefined
    ;; and denotes the (one and only) eof-object.
    ;; R7RS instead provides a constructor
    ;; that returns an eof-object when invoked,
    ;; but does not presuppose
    ;; that the constructor returns the same value
    ;; on each invocation
    ;; or that any of the values it returns
    ;; will be the same as an eof-object
    ;; returned by one of the input procedures.

    ;; For convenience, I have provided an eof-object
    ;; under the name Racket uses,
    ;; but programmers should always use the eof-object? predicate
    ;; to detect sentinel values
    ;; rather than testing for equality with eof.

    (define eof (eof-object))

    ;; pi is an inexact number
    ;; approximating the ratio
    ;; of the circumference of a circle to its diameter.

    (define pi (atan 0.0 -1.0))

    ;; The gensym procedure generates and returns a new symbol
    ;; each time it is invoked.

    (define gensym
      (let ((counter 0))
        (lambda ()
          (let ((counter-string (number->string counter)))
            (set! counter (+ counter 1))
            (string->symbol (string-append "g" counter-string))))))
      
    ;; Given a list and a binary procedure
    ;; that implements an ordering relation
    ;; on the elements of that list,
    ;; the sort procedure constructs and returns a list
    ;; containing the same elements
    ;; in the order specified by the relation.

    ;; This implementation uses the mergesort algorithm.

    (define (sort ls precedes?)
      (letrec ((revappend (lambda (tfel right)
                            (let loop ((rest-of-tfel tfel)
                                       (result right))
                              (if (null? rest-of-tfel)
                                  result
                                  (loop (cdr rest-of-tfel)
                                        (cons (car rest-of-tfel)
                                              result))))))
               (merge (lambda (left right)
                        (let loop ((so-far '())
                                   (rest-of-left left)
                                   (rest-of-right right))
                          (cond ((null? rest-of-left)
                                 (revappend so-far rest-of-right))
                                ((null? rest-of-right)
                                 (revappend so-far rest-of-left))
                                ((precedes? (car rest-of-left)
                                            (car rest-of-right))
                                 (loop (cons (car rest-of-left) so-far)
                                       (cdr rest-of-left)
                                       rest-of-right))
                                (else
                                 (loop (cons (car rest-of-right) so-far)
                                       rest-of-left
                                       (cdr rest-of-right)))))))
               (merge-pairwise (lambda (list-of-lists)
                                 (let loop ((rest list-of-lists)
                                            (so-far '()))
                                   (cond ((null? rest) so-far)
                                         ((null? (cdr rest))
                                          (cons (car rest) so-far))
                                         (else
                                          (loop (cons (merge (car rest)
                                                             (cadr rest))
                                                      so-far)
                                                (cddr rest))))))))
        (if (null? ls)
            '()
            (let loop ((to-be-merged (map list ls)))
              (if (null? (cdr to-be-merged))
                  (car to-be-merged)
                  (loop (merge-pairwise to-be-merged)))))))
                   
    ;; The take procedure constructs and returns a list
    ;; comprising a specified number of elements
    ;; from the beginning of a given list.

    (define (take ls num)
      (let loop ((rest ls)
                 (remaining num)
                 (so-far '()))
        (if (or (zero? remaining) (null? rest))
            (reverse so-far)
            (loop (cdr rest) (- remaining 1) (cons (car rest) so-far)))))

    ;; The drop procedure constructs and returns a list
    ;; similar to a given list,
    ;; but with a specified number of initial elements removed.

    (define (drop ls num)
      (if (or (zero? num) (null? ls))
          ls
          (drop (cdr ls) (- num 1))))

    ;; null is a more readable name for the null list.

    (define null '())

    ;; The filter procedure constructs and returns a list
    ;; comprising the elements of a given list
    ;; that satisfy a given unary predicate.

    (define (filter ok? ls)
      (let loop ((rest ls)
                 (so-far '()))
        (if (null? rest)
            (reverse so-far)
            (let ((first (car rest)))
              (loop (cdr rest)
                    (if (ok? first)
                        (cons first so-far)
                        so-far))))))

    ;; The negate procedure constructs and returns
    ;; the negation of a given predicate --
    ;; a new predicate that returns #t
    ;; if the given predicate, applied to the same arguments, returns #f
    ;; and vice versa.

    (define (negate predicate)
      (lambda arguments
        (not (apply predicate arguments))))

    ;; The conjoin procedure constructs and returns
    ;; the conjunction of two given predicates --
    ;; a new predicate that returns #t
    ;; if both of the given predicates, applied to the same arguments,
    ;; return #t
    ;; and returns #f otherwise.

    (define (conjoin left right)
      (lambda arguments
        (and (apply left arguments)
             (apply right arguments))))

    ;; The disjoin procedure constructs and returns
    ;; the disjunction of two given predicates --
    ;; a new predicate that returns #t
    ;; if either of the given predicates, applied to the same arguments,
    ;; returns #t
    ;; and returns #f otherwise.

    (define (disjoin left right)
      (lambda arguments
        (or (apply left arguments)
            (apply right arguments))))))

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
