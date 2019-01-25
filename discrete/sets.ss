#!r7rs

;;; Sets

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 9, 2013
;;; last revised January 24, 2019

;;; A set is an unordered collection of distinct values.
;;; This library implements a data type for sets
;;; and several basic operations on them.

(define-library (discrete sets)
  (export
    set adjoin-to-set                           ; constructors
    set?                                        ; type tester
    in-set?                                     ; membership test
    cardinality                                 ; member tally
    set-union set-intersection                  ; operations
    set-difference set-symmetric-difference
    set-sunder
    power-set
    cartesian-product cartesian-power 
    generalized-union generalized-intersection  ; variable-valence extensions
    generalized-symmetric-difference
    generalized-cartesian-product 
    subset? set=? disjoint?                     ; relations
    grand-union                                 ; sets of sets
    set-map set-for-each                        ; higher-order procedures
    set-for-all set-exists
    list->set set->list set->string             ; type conversions
    display-set)                                ; output
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (csc-151 extensions)
          (discrete utilities))
  (begin

    ;; We implement a set as a record with one field.
    ;; This field will always contain a list,
    ;; without duplicates, of the members of the set,
    ;; in arbitrary order.

    ;; This record type definition implicitly defines
    ;; the constructor make-set-rec,
    ;; the type predicate set-rec?,
    ;; and the accessor procedure set-rec-members.
    ;; This library uses these procedures internally
    ;; but does not export them.
    ;; The procedures that it does export
    ;; provide a more natural programming interface
    ;; for most programmers who want to work with sets.

    (define-record-type <set-rec>
      (make-set-rec members)
      set-rec?
      (members set-rec-members))

    ;; The set? predicate is simply a synonym for set-rec?.

    (define set? set-rec?)

    ;; Given a value val and a list ls,
    ;; the adjoin-to-list procedure
    ;; tests whether val is identical with any element of ls.
    ;; If so, adjoin-to-list returns ls unchanged;
    ;; otherwise, it returns the result of consing val onto ls.

    (define adjoin-to-list
      (lambda (val ls)
        (if (member val ls)
            ls
            (cons val ls))))

    ;; The set procedure takes any number of arguments
    ;; and returns a set containing exactly those arguments as members.
    ;; We test each argument before adding it to the list of set members
    ;; in order to avoid adding duplicate values;
    ;; when we find a duplicate, we skip it.

    (define set
      (lambda arguments
        (make-set-rec (fold-right adjoin-to-list arguments null))))

    ;; Given a value val and a set aro,
    ;; the adjoin-to-set procedure returns a set
    ;; with val and every member of aro as its members.

    (define adjoin-to-set
      (lambda (val aro)
        (let ((ls (set-rec-members aro)))
          (if (member val ls)
              aro
              (make-set-rec (cons val ls))))))

    ;; The in-set? procedure determines whether
    ;; a given value is a member of a given set.

    (define in-set?
      (lambda (candidate aro)
        (if (member candidate (set-rec-members aro)) #t #f)))

    ;; The cardinality procedure determines
    ;; the number of members of a given set.
    ;; It relies on the invariant
    ;; that the list stored in the members field of a set record
    ;; has no duplicates.

    (define cardinality
      (lambda (aro)
        (length (set-rec-members aro))))

    ;; The set-union procedure
    ;; constructs and returns the union of two sets.

    (define set-union
      (lambda (left right)
        (make-set-rec (fold-right adjoin-to-list
                                  (set-rec-members left)
                                  (set-rec-members right)))))

    ;; The set-intersection procedure
    ;; constructs and returns the intersection of two sets.

    (define set-intersection
      (lambda (left right)
        (let ((right-list (set-rec-members right)))
          (set-sunder left (section member <> right-list)))))

    ;; The set-difference procedure
    ;; takes two sets as arguments
    ;; and constructs and returns a set
    ;; having as its members
    ;; all of the members of the first set
    ;; that are not members of the second set.

    (define set-difference
      (lambda (left right)
        (let ((right-list (set-rec-members right)))
          (make-set-rec
            (filter (negate (section member <> right-list))
                    (set-rec-members left))))))

    ;; The set-symmetric-difference procedure
    ;; constructs and returns the set of values
    ;; that are members of one of two given sets,
    ;; but not of both.

    (define set-symmetric-difference
      (lambda (left right)
        (set-union (set-difference left right)
                   (set-difference right left))))

    ;; The set-sunder procedure
    ;; takes a set and a unary predicate as arguments
    ;; and returns a set having as its members
    ;; all of the members of the given set
    ;; that satisfy the given predicate.

    (define set-sunder
      (lambda (aro predicate)
        (make-set-rec (filter predicate (set-rec-members aro)))))

    ;; The power-set procedure
    ;; computes and returns the set of subsets of a given set.

    (define power-set
      (lambda (aro)
        (make-set-rec
          (map make-set-rec
               (fold-right (lambda (new base)
                             (append base (map (section cons new <>) base)))
                           (set-rec-members aro)
                           (list null))))))

    ;; Given two sets, the cartesian-product procedure
    ;; computes and returns the set of ordered pairs
    ;; in which the first element is a member of the first given set
    ;; and the second element is a member of the second given set.

    (define cartesian-product
      (lambda (left right)
        (let ((right-list (set-rec-members right)))
          (make-set-rec
            (fold-right (lambda (left-item so-far)
                          (append (map (section list left-item <>) right-list)
                                  so-far))
                        (set-rec-members left)
                        null)))))

    ;; Given a set and a natural number,
    ;; the cartesian-power procedure
    ;; computes and returns the set of sequences
    ;; of members of the given set
    ;; whose length is the given natural number.

    (define cartesian-power
      (lambda (aro len)
        (let ((mems (set-rec-members aro)))
          (make-set-rec
            (let kernel ((remaining len))
              (if (zero? remaining)
                  '(())
                  (let ((lesser-power (kernel (- remaining 1))))
                    (apply append
                           (map (lambda (new)
                                  (map (section cons new <>) lesser-power))
                                mems)))))))))

    ;; Union, intersection, and symmetric difference
    ;; are commutative and associative,
    ;; so it makes sense to extend them to procedures of
    ;; variable valence.

    ;; The generalized-union procedure
    ;; takes any number of arguments,
    ;; each of which must be a set,
    ;; and returns the set of all the members
    ;; of all of the given sets.

    (define generalized-union
      (lambda sets
        (apply set (apply append (map set-rec-members sets)))))

    ;; The generalized-intersection procedure
    ;; takes one or more arguments,
    ;; each of which must be a set,
    ;; and returns the set of values that are members
    ;; of all of the given sets --
    ;; the values that all of those sets have in common.

    ;; In principle, a call to generalized-intersection with no arguments
    ;; should return the universe:
    ;; a set containing everything.
    ;; Because our implementation of sets
    ;; doesn't provide us with any way of representing that set, however,
    ;; we'll require the caller to supply at least one argument.

    (define generalized-intersection
      (lambda (first . rest)
        (let ((rest-lists (map set-rec-members rest)))
          (make-set-rec
            (filter (lambda (item)
                      (all (section member item <>) rest-lists))
                    (set-rec-members first))))))

    ;; The generalized-symmetric-difference procedure
    ;; takes any number of arguments,
    ;; each of which must be a set,
    ;; and returns the set of values
    ;; that are members of an odd number of the given sets.

    (define generalized-symmetric-difference
      (lambda sets
        (fold-right set-symmetric-difference sets (set))))

    ;; Given any number k of sets,
    ;; the generalized-cartesian-product procedure
    ;; computes and returns the set of ordered k-tuples
    ;; in which each element is a member of the set
    ;; in the corresponding position among the arguments.

    (define generalized-cartesian-product
      (lambda sets
        (make-set-rec
          (fold-right (lambda (fore base)
                        (apply append (map (lambda (item)
                                             (map (lambda (rest)
                                                    (cons item rest))
                                                  base))
                                           fore)))
                      (map set-rec-members sets)
                      (list null)))))

    ;; The subset? predicate
    ;; tests whether one set is a subset of another --
    ;; specifically, whether every member of the first set
    ;; is also a member of the second.

    (define subset?
      (lambda (left right)
        (let ((right-list (set-rec-members right)))
          (all (section member <> right-list) 
               (set-rec-members left)))))

    ;; The set=? predicate
    ;; tests whether two sets have the same members.

    (define set=?
      (lambda (left right)
        (and (subset? left right)
             (subset? right left))))

    ;; Given two sets, the disjoint? procedure
    ;; determines whether they are disjoint,
    ;; that is, whether they have no member in common.

    (define disjoint?
      (lambda (left right)
        (let ((right-list (set-rec-members right)))
          (not (any (section member <> right-list)
                    (set-rec-members left))))))

    ;; The grand-union procedure
    ;; constructs and returns the set
    ;; of all the members of members of a given set.

    (define grand-union
      (lambda (araro)
        (apply generalized-union (set-rec-members araro))))

    ;; The set-map procedure applies a given procedure
    ;; to every member of a given set,
    ;; collecting the results in a new set, which it returns.
    ;; Unlike map for lists,
    ;; set-map works only with procedures of valence 1;
    ;; since sets are unordered,
    ;; there is no notion of "corresponding" members of different sets.

    (define set-map
      (lambda (proc aro)
        (apply set (map proc (set-rec-members aro)))))

    ;; The set-for-each procedure applies a given procedure,
    ;; for its side effect only,
    ;; to every member of a given set.
    ;; The return value is unspecified.
    ;; Like set-map,
    ;; set-for-each works only with procedures of valence 1.

    (define set-for-each
      (lambda (proc aro)
        (for-each proc (set-rec-members aro))))

    ;; The set-for-all procedure
    ;; determines whether every member of a given set
    ;; satisfies a given unary predicate.

    (define set-for-all
      (lambda (predicate aro)
        (all predicate (set-rec-members aro))))

    ;; The set-exists procedure
    ;; determines whether at least one member of a given set
    ;; satisfies a given unary predicate.

    (define set-exists
      (lambda (predicate aro)
        (any predicate (set-rec-members aro))))

    ;; The list->set procedure converts any list
    ;; into a set having the elements of the list as members,
    ;; removing any duplicates.

    (define list->set
      (section apply set <>))

    ;; The set->list procedure converts any set
    ;; into a list of its members, in arbitrary order.

    (define set->list set-rec-members)

    ;; The set->string procedure
    ;; constructs and returns a string representation of a given set.

    (define set->string
      (lambda (aro)
        (let ((out (open-output-string)))
          (display-set aro out)
          (let ((result (get-output-string out)))
            (close-port out)
            result))))

    ;; The display-set procedure
    ;; writes an external representation of a set
    ;; to a specified output port
    ;; (or to standard output, if no output port is specified).

    (define display-set
      (case-lambda
        ((aro) (display-set aro (current-output-port)))
        ((aro out)
         (let ((ls (set-rec-members aro)))
           (if (null? ls)
               (display "{}" out)
               (begin
                 (display #\{ out)
                 (let loop ((first (car ls))
                            (rest (cdr ls)))
                   (if (set? first)
                       (display-set first out)
                       (write first out))
                   (unless (null? rest)
                     (display ", " out)
                     (loop (car rest) (cdr rest))))
                 (display #\} out)))))))))

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
