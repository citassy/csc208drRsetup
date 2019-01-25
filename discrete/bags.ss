#!r7rs

;;; Bags

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 9, 2013
;;; last revised January 24, 2019

;;; A bag is an unordered collection of values,
;;; not necessarily distinct.
;;; This library implements a data type for bags
;;; and several basic operations.

(define-library (discrete bags)
  (export bag adjoin-to-bag                  ; constructor
          bag?                               ; type tester
          multiplicity bag-cardinality       ; member tallies
          bag-union bag-intersection         ; operations
          bag-difference
          bag-sunder
          power-bag
          bag-cartesian-product
          bag-cartesian-power
          generalized-bag-union              ; variable-valence extensions
          generalized-bag-intersection
          generalized-bag-cartesian-product
          subbag? bag=?                      ; relations
          bag-map bag-for-each               ; higher-order procedures
          list->bag bag->list                ; type conversions
          set->bag bag->set
          bag->string
          display-bag)                       ; output
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (csc-151 extensions)
          (discrete utilities)
          (discrete sets))
  (begin

    ;; We implement a bag as a record with one field,
    ;; which contains a list of the members of the bag,
    ;; in arbitrary order.

    (define-record-type <bag-rec>
      (make-bag-rec members)
      bag-rec?
      (members bag-rec-members))

    ;; The bag procedure takes any number of arguments
    ;; and returns a bag containing exactly those arguments as members.

    (define bag
      (lambda arguments
        (make-bag-rec arguments)))

    ;; Given a value val and a bag aro,
    ;; the adjoin-to-bag procedure returns a bag
    ;; with val and every member of aro as its members.

    (define adjoin-to-bag
      (lambda (val aro)
        (make-bag-rec (cons val (bag-rec-members aro)))))

    ;; The bag? predicate is simply a synonym for bag-rec?.

    (define bag? bag-rec?)

    ;; The multiplicity procedure
    ;; determines how many copies of a given value
    ;; are contained in a given bag.

    (define multiplicity
      (lambda (candidate aro)
        (let loop ((ls (bag-rec-members aro))
                   (so-far 0))
          (cond ((null? ls) so-far)
                ((equal? candidate (car ls))
                 (loop (cdr ls) (+ so-far 1)))
                (else (loop (cdr ls) so-far))))))

    ;; The bag-cardinality procedure
    ;; determines the number of members of a given bag.

    (define bag-cardinality
      (lambda (aro)
        (length (bag-rec-members aro))))

    ;; The bag-union procedure constructs
    ;; and returns the union of two bags.

    (define bag-union
      (lambda (left right)
        (make-bag-rec (append (bag-rec-members left)
                              (bag-rec-members right)))))

    ;; Given a value and a list
    ;; the remove-at-most-once procedure
    ;; constructs and returns a list similar to the given list
    ;; except that if the list contains a value
    ;; equal to the given value,
    ;; the first occurrence of that value in the list
    ;; has been deleted.
    ;; If the given value does not occur at all in the given list,
    ;; a copy of the given list is returned.

    (define remove-at-most-once
      (lambda (val ls)
        (let loop ((rest ls))
          (cond ((null? rest) '())
                ((equal? val (car rest)) (cdr rest))
                (else (cons (car rest) (loop (cdr rest))))))))

    ;; The bag-intersection procedure
    ;; constructs and returns the intersection of two bags.
    ;; If a given value occurs with multiplicity m in the first bag
    ;; and multiplicity n in the second,
    ;; it occurs with multiplicity min(m, n) in the result.

    (define bag-intersection
      (lambda (left right)
        (let kernel ((lrest (bag-rec-members left))
                     (rrest (bag-rec-members right))
                     (so-far '()))
          (cond ((null? lrest) (make-bag-rec so-far))
                ((member (car lrest) rrest)
                 (kernel (cdr lrest)
                         (remove-at-most-once (car lrest) rrest)
                         (cons (car lrest) so-far)))
                (else (kernel (cdr lrest) rrest so-far))))))

    ;; The bag-difference procedure takes two bags as arguments
    ;; and constructs and returns a bag
    ;; having as its members
    ;; all of the members of the first bag
    ;; that are not members of the second bag.
    ;; If a given value occurs with multiplicity m in the first bag
    ;; and multiplicity n in the second,
    ;; it occurs with multiplicity max(0, m - n) in the result.

    (define bag-difference
      (lambda (left right)
        (make-bag-rec (fold-right remove-at-most-once
                                  (bag-rec-members right)
                                  (bag-rec-members left)))))

    ;; The bag-sunder procedure
    ;; takes a bag and a unary predicate as arguments
    ;; and returns a bag having as its members
    ;; all of the members of the given bag
    ;; that satisfy the given predicate.

    (define bag-sunder
      (lambda (aro predicate)
        (make-bag-rec (filter predicate (bag-rec-members aro)))))

    ;; The power-bag procedure computes and returns
    ;; the bag of subbags of a given bag.

    (define power-bag
      (lambda (aro)
        (make-bag-rec
          (map make-bag-rec
               (fold-right (lambda (new base)
                             (append (map (section cons new <>) base)
                                     base))
                           (bag-rec-members aro)
                           '(()))))))

    ;; Given two bags,
    ;; the bag-cartesian-product procedure
    ;; computes and returns the bag of ordered pairs
    ;; in which the first element is a member of the first given bag
    ;; and the second element is a member of the second given bag.

    (define bag-cartesian-product
      (lambda (left right)
        (let ((right-members (bag-rec-members right)))
          (make-bag-rec
            (fold-right (lambda (left-item base)
                          (append (map (section list left-item <>) right-members)
                                  base))
                        (bag-rec-members left)
                        '())))))

    ;; Given a bag and a natural number,
    ;; the cartesian-power procedure computes and returns
    ;; the bag of sequences of members of the given bag
    ;; whose length is the given natural number.

    (define bag-cartesian-power
      (lambda (aro len)
        (let ((mems (bag-rec-members aro)))
          (make-bag-rec
            (let kernel ((remaining len))
              (if (zero? remaining)
                  '(())
                  (let ((lesser-power (kernel (- remaining 1))))
                    (apply append
                           (map (lambda (new) 
                                  (map (section cons new <>) lesser-power))
                                mems)))))))))

    ;; Bag union and intersection are commutative and associative,
    ;; so it makes sense to extend them to procedures of variable valence.

    ;; The generalized-bag-union procedure takes any number of arguments,
    ;; each of which must be a bag,
    ;; and returns the bag of all the members of all of the given bags.

    (define generalized-bag-union
      (lambda bags
        (make-bag-rec (apply append (map bag-rec-members bags)))))

    ;; The generalized-bag-intersection procedure takes one or more arguments,
    ;; each of which must be a bag,
    ;; and returns the bag of values
    ;; that are members of all of the given bags --
    ;; the values that all of those bags have in common.

    ;; In principle,
    ;; a call to generalized-bag-intersection with no arguments
    ;; should return a bag containing infinitely many copies of everything.
    ;; Because our implementation of bags
    ;; doesn't provide us with any way of representing that bag, however,
    ;; we'll require the caller to supply at least one argument.

    (define generalized-bag-intersection
      (lambda (first . rest)
        (fold-left bag-intersection rest first)))

    ;; Given any number k of bags,
    ;; the generalized-bag-cartesian-product procedure
    ;; computes and returns the bag of ordered k-tuples
    ;; in which each element is a member of the bag
    ;; in the corresponding position among the arguments.

    (define generalized-bag-cartesian-product
      (lambda bags
        (make-bag-rec
          (fold-right (lambda (fore base)
                        (apply append (map (lambda (item)
                                             (map (section cons item <>)
                                                  base))
                                           fore)))
                      (map bag-rec-members bags)
                      '(())))))

    ;; The subbag? predicate
    ;; tests whether one bag is a subbag of another --
    ;; specifically, whether the multiplicity of every member of the first bag
    ;; is less than or equal to its multiplicity in the second bag.

    (define subbag?
      (lambda (left right)
        (let kernel ((lrest (bag-rec-members left))
                     (rrest (bag-rec-members right)))
          (or (null? lrest)
              (and (member (car lrest) rrest)
                   (kernel (cdr lrest)
                           (remove-at-most-once (car lrest) rrest)))))))

    ;; The bag=? predicate
    ;; tests whether two bags have the same members
    ;; (with equal multiplicities).

    (define bag=?
      (lambda (left right)
        (let kernel ((lrest (bag-rec-members left))
                     (rrest (bag-rec-members right)))
          (if (null? lrest)
              (null? rrest)
              (and (member (car lrest) rrest)
                   (kernel (cdr lrest)
                           (remove-at-most-once (car lrest) rrest)))))))

    ;; The bag-map procedure
    ;; applies a given procedure
    ;; to every member of a given bag,
    ;; collecting the results in a new bag,
    ;; which it returns.
    ;; Unlike map for lists,
    ;; bag-map works only with unary procedures;
    ;; since bags are unordered,
    ;; there is no notion of "corresponding" members of different bags.

    (define bag-map
      (lambda (proc aro)
        (make-bag-rec (map proc (bag-rec-members aro)))))

    ;; The bag-for-each procedure applies a given procedure,
    ;; for its side effect only,
    ;; to every member of a given bag.
    ;; The return value is unspecified.
    ;; Like bag-map, bag-for-each works only with unary procedures.

    (define bag-for-each
      (lambda (proc aro)
        (for-each proc (bag-rec-members aro))))

    ;; The list->bag procedure converts any list
    ;; into a bag having the elements of the list as members.

    (define list->bag make-bag-rec)

    ;; The bag->list procedure converts any bag
    ;; into a list of its members, in arbitrary order.

    (define bag->list bag-rec-members)

    ;; The set->bag procedure converts any set
    ;; into a bag with the same members (each with multiplicity 1).

    (define set->bag
      (lambda (aro)
        (make-bag-rec (set->list aro))))

    ;; The bag->set procedure converts any bag
    ;; into a set with the same members (discarding duplicates).

    (define bag->set
      (lambda (aro)
        (apply set (bag-rec-members aro))))

    ;; The bag->string procedure constructs and returns
    ;; a string representation of a given bag.

    (define bag->string
      (lambda (aro)
        (let ((out (open-output-string)))
            (display-bag aro out)
            (let ((result (get-output-string out)))
              (close-port out)
              result))))


    ;; The display-bag procedure
    ;; writes an external representation of a gived bag
    ;; to a specified output port
    ;; (or to standard output, if no output port is specified).

    (define display-bag
     (case-lambda
        ((aro) (display-bag aro (current-output-port)))
        ((aro out)
         (let ((ls (bag-rec-members aro)))
           (if (null? ls)
               (display "⟦⟧" out)
               (begin
                 (display #\⟦ out)
                 (let loop ((first (car ls))
                            (rest (cdr ls)))
                   (cond ((set? first) (display-set first out))
                         ((bag? first) (display-bag first out))
                         (else (write first out)))
                   (unless (null? rest)
                     (display ", " out)
                     (loop (car rest) (cdr rest))))
                 (display #\⟧ out)))))))))

;;; copyright (C) 2013, 2014, 2016, 2018, 2019 John David Stone

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
