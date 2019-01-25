#!r7rs

;;; Relations as sets of pairs

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 3, 2013
;;; last revised November 25, 2018

;;; A relation is the denotation of a binary predicate.
;;; A value either bears the relation
;;; to another (or possibly the same) value,
;;; or fails to  bear that relation to that other value.
;;; This library implements data types and basic operations for relations.

(define-library (discrete relations)
  (export relation? null-relation bears-to? adjoin-to-relation
          relational-union relational-cardinality relational-intersection
          relational-difference relational-symmetric-difference
          generalized-relational-union generalized-relational-intersection
          generalized-relational-symmetric-difference relation-sunder
          subrelation? relation=? relation-disjoint? reflexive-relation?
          irreflexive-relation? symmetric-relation? asymmetric-relation?
          antisymmetric-relation? connected-relation? transitive-relation?
          intransitive-relation? equivalence-relation? equivalence-classes
          partial-ordering? total-ordering? converse-relation left-domain
          right-domain left-range right-range image preimage
          relational-product relational-power ancestral relational-for-each
          pair-set->relation relation->pair-set display-relation
          relation->string)
  (import (scheme base)
          (scheme write)
          (discrete sets)
          (discrete bags))
  (begin

    ;; In this library,
    ;; we implement a relation as a record with three fields:
    ;; two to specify its left and right domains,
    ;; and a third for a set of pairs
    ;; such that the car of each pair bears the relation to the cdr.

    (define-record-type <relation-rec>
      (make-relation-rec ld rd pairs)
      relation-rec?
      (ld relation-rec-left-domain)
      (rd relation-rec-right-domain)
      (pairs relation-rec-pairs))

    ;; The relation? predicate is simply a synonym for relation-rec?.

    (define relation? relation-rec?)

    ;; The null-relation procedure returns a null relation --
    ;; one that nothing bears to anything.

    (define null-relation
      (lambda (ld rd)
        (make-relation-rec ld rd (set))))

    ;; The bears-to? procedure determines whether
    ;; a given value bears a given relation
    ;; to another (or possibly the same) given value.

    (define bears-to?
      (lambda (rel left right)
        (in-set? (cons left right) (relation-rec-pairs rel))))

    ;; Given two values, left and right, and a relation rel,
    ;; the adjoin-to-relation procedure
    ;; returns a relation similar to rel
    ;; except possibly that left bears the new relation to right.

    (define adjoin-to-relation
      (lambda (left right rel)
        (make-relation-rec (adjoin-to-set left
                                          (relation-rec-left-domain rel))
                           (adjoin-to-set right
                                          (relation-rec-right-domain rel))
                           (adjoin-to-set (cons left right)
                                          (relation-rec-pairs rel)))))

    ;; The relational-union procedure
    ;; constructs and returns the union of two relations.

    (define relational-union
      (lambda (left right)
        (make-relation-rec (set-union (relation-rec-left-domain left)
                                      (relation-rec-left-domain right))
                           (set-union (relation-rec-right-domain left)
                                      (relation-rec-right-domain right))
                           (set-union (relation-rec-pairs left)
                                      (relation-rec-pairs right)))))

    ;; The relational-cardinality procedure
    ;; determines the number of pairs of values
    ;; that stand in a given relation.

    (define relational-cardinality
      (lambda (rel)
        (cardinality (relation-rec-pairs rel))))

    ;; The relational-intersection procedure
    ;; constructs and returns the intersection of two relations.

    (define relational-intersection
      (lambda (left right)
        (make-relation-rec (set-union (relation-rec-left-domain left)
                                      (relation-rec-left-domain right))
                           (set-union (relation-rec-right-domain left)
                                      (relation-rec-right-domain right))
                           (set-intersection (relation-rec-pairs left)
                                             (relation-rec-pairs right)))))

    ;; The relational-difference procedure
    ;; takes two relations, left and right, as arguments
    ;; and constructs and returns a relation
    ;; that a value x bears to another (or possibly the same) value y
    ;; if, and only if, x bears left to y but does not bear right to y.

    (define relational-difference
      (lambda (left right)
        (make-relation-rec (set-union (relation-rec-left-domain left)
                                      (relation-rec-left-domain right))
                           (set-union (relation-rec-right-domain left)
                                      (relation-rec-right-domain right))
                           (set-difference (relation-rec-pairs left)
                                           (relation-rec-pairs right)))))

    ;; The relational-symmetric-difference procedure
    ;; constructs and returns the relation
    ;; that a value x bears to another (or possibly the same) value y
    ;; if, and only if, x bears one but not both of two given relations to y.

    (define relational-symmetric-difference
      (lambda (left right)
        (make-relation-rec
         (set-union (relation-rec-left-domain left)
                    (relation-rec-left-domain right))
         (set-union (relation-rec-right-domain left)
                    (relation-rec-right-domain right))
         (set-symmetric-difference (relation-rec-pairs left)
                                   (relation-rec-pairs right)))))

    ;; Relational union, relational intersection,
    ;; and relational symmetric difference
    ;; are commutative and associative,
    ;; so it makes sense
    ;; to extend them to procedures of variable valence.

    ;; The generalized-relational-union procedure
    ;; takes any number of arguments,
    ;; each of which must be a relation,
    ;; and returns the relation
    ;; that a value x bears to another (or possibly the same) value y
    ;; if, and only if, x bears any of the given relations to y.

    (define generalized-relational-union
      (lambda rels
        (make-relation-rec (apply generalized-union
                                  (map relation-rec-left-domain rels))
                           (apply generalized-union
                                  (map relation-rec-right-domain rels))
                           (apply generalized-union
                                  (map relation-rec-pairs rels)))))

    ;; The generalized-relational-intersection procedure
    ;; takes one or more arguments,
    ;; each of which must be a relation,
    ;; and returns the relation
    ;; that a value x bears to another (or possibly the same) value y
    ;; if, and only if, x bears all of the given relations to y.

    ;; In principle, a call to generalized-relational-intersection
    ;; with no arguments
    ;; should return the universal relation:
    ;; a relation that everything bears to everything.
    ;; Because our implementation of relations doesn't provide us
    ;; with any way of representing that relation, however,
    ;; we'll require the caller to supply at least one argument.

    (define generalized-relational-intersection
      (lambda (first . rest)
        (make-relation-rec (apply generalized-union
                                  (relation-rec-left-domain first)
                                  (map relation-rec-left-domain rest))
                           (apply generalized-union
                                  (relation-rec-right-domain first)
                                  (map relation-rec-right-domain rest))
                           (apply generalized-intersection
                                  (relation-rec-pairs first)
                                  (map relation-rec-pairs rest)))))

    ;; The generalized-relational-symmetric-difference procedure
    ;; takes any number of arguments,
    ;; each of which must be a relation,
    ;; and returns the relation
    ;; that a value x bears to another value y
    ;; if, and only if, x bears an odd number of the given relations to y.

    (define generalized-relational-symmetric-difference
      (lambda rels
        (make-relation-rec (apply generalized-union
                                  (map relation-rec-left-domain rels))
                           (apply generalized-union
                                  (map relation-rec-right-domain rels))
                           (apply generalized-symmetric-difference
                                  (map relation-rec-pairs rels)))))

    ;; The relation-sunder procedure
    ;; takes a relation and a binary predicate as arguments
    ;; and returns a relation
    ;; that a value x bears to another (or possibly the same) value y
    ;; if x bears the given relation to y
    ;; and the given binary predicate returns a truish value
    ;; when applied to x and y.

    (define relation-sunder
      (lambda (rel predicate)
        (make-relation-rec
          (relation-rec-left-domain rel)
          (relation-rec-right-domain rel)
          (set-sunder (relation-rec-pairs rel)
                      (lambda (pr)
                        (predicate (car pr) (cdr pr)))))))

    ;; The subrelation? predicate
    ;; tests whether one relation is a subrelation of another --
    ;; specifically, whether a value x bears the second relation to a value y
    ;; whenever x bears the first relation to y.

    (define subrelation?
      (lambda (left right)
        (and (subset? (relation-rec-left-domain left)
                      (relation-rec-left-domain right))
             (subset? (relation-rec-right-domain left)
                      (relation-rec-right-domain right))
             (subset? (relation-rec-pairs left)
                      (relation-rec-pairs right)))))

    ;; The relation=? predicate
    ;; tests whether the same values stand in two relations.

    (define relation=?
      (lambda (left right)
        (and (set=? (relation-rec-left-domain left)
                    (relation-rec-left-domain right))
             (set=? (relation-rec-right-domain left)
                    (relation-rec-right-domain right))
             (set=? (relation-rec-pairs left) (relation-rec-pairs right)))))

    ;; Given two relations,
    ;; the relation-disjoint? procedure
    ;; determines whether they are disjoint,
    ;; that is, whether no value bears both to any value.

    (define relation-disjoint?
      (lambda (left right)
        (disjoint? (relation-rec-pairs left) (relation-rec-pairs right))))

    ;; A relation is "enclosed"
    ;; if its left and right domains are the same set.

    (define enclosed?
      (lambda (rel)
        (set=? (relation-rec-left-domain rel)
               (relation-rec-right-domain rel))))

    ;; The reflexive-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether every member of the (left) domain of that relation
    ;; bears the relation to itself.

    (define reflexive-relation?
      (lambda (rel)
        (set-for-all (lambda (item)
                       (bears-to? rel item item))
                     (relation-rec-left-domain rel))))

    ;; The irreflexive-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether any value bears that relation to itself,
    ;; returning #f if there is such a value and #t if there is not.

    (define irreflexive-relation?
      (lambda (rel)
        (not (set-exists (lambda (pr)
                           (equal? (car pr) (cdr pr)))
                         (relation-rec-pairs rel)))))

    ;; The symmetric-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether it is symmetric,
    ;; in the sense that,
    ;; if a value x bears the relation to another (or possibly the same) value y,
    ;; then y bears the same relation to x.

    (define symmetric-relation?
      (lambda (rel)
        (set-for-all (lambda (pr)
                       (bears-to? rel (cdr pr) (car pr)))
                     (relation-rec-pairs rel))))

    ;; The asymmetric-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether it is asymmetric,
    ;; in the sense that,
    ;; if a value x bears the relation to another (or possibly the same) value y,
    ;; then y does not bear the same relation to x.

    (define asymmetric-relation?
      (lambda (rel)
        (not (set-exists (lambda (pr)
                           (bears-to? rel (cdr pr) (car pr)))
                         (relation-rec-pairs rel)))))

    ;; The antisymmetric-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether it is antisymmetric,
    ;; in the sense that,
    ;; if a value x bears the relation to some value y distinct from x,
    ;; then y does not bear the same relation to x.

    (define antisymmetric-relation?
      (lambda (rel)
        (not (set-exists (lambda (pr)
                           (and (not (equal? (car pr) (cdr pr)))
                                (bears-to? rel (cdr pr) (car pr))))
                         (relation-rec-pairs rel)))))

    ;; The connected-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether the given relation
    ;; connects its domain --
    ;; that is, whether, for any members x and y (possibly identical) of the domain,
    ;; either x bears the given relation to y or y bears it to x.

    (define connected-relation?
      (lambda (rel)
        (set-for-all (lambda (left)
                       (set-for-all (lambda (right)
                                      (or (bears-to? rel left right)
                                          (bears-to? rel right left)))
                                    (relation-rec-right-domain rel)))
                     (relation-rec-left-domain rel))))

    ;; The transitive-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether it is transitive,
    ;; that is, whether a value x
    ;; bears the given relation to another (or possibly the same) value z
    ;; whenever there is a value y
    ;; such that x bears the relation to y and y bears it to z.

    (define transitive-relation?
      (lambda (rel)
        (let ((prs (relation-rec-pairs rel)))
          (set-for-all
            (lambda (left-pair)
              (set-for-all
                (lambda (right-pair)
                  (or (not (equal? (cdr left-pair) (car right-pair)))
                      (bears-to? rel (car left-pair) (cdr right-pair))))
                prs))
            prs))))

    ;; The intransitive-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether it intransitive,
    ;; that is, whether the conditions that
    ;; a value x bears the relation to another (or possibly the same) value y,
    ;; that y bears the relation to another (or possibly the same) value z,
    ;; and that x bears the relation to z are never all true simultaneously.

    (define intransitive-relation?
      (lambda (rel)
        (let ((prs (relation-rec-pairs rel)))
          (not (set-exists
                 (lambda (left-pair)
                   (set-exists
                     (lambda (right-pair)
                       (and (equal? (cdr left-pair) (car right-pair))
                            (bears-to? rel (car left-pair) (cdr right-pair))))
                     prs))
                 prs)))))

    ;; The equivalence-relation? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether the relation
    ;; is an equivalence relation on its domain,
    ;; that is, whether it is reflexive, symmetric, and transitive.

    (define equivalence-relation?
      (lambda (rel)
        (and (reflexive-relation? rel)
             (symmetric-relation? rel)
             (transitive-relation? rel))))

    ;; The equivalence-classes procedure
    ;; takes an equivalence relation as its argument
    ;; and returns a partition of the domain of that relation
    ;; in which the components are equivalence classes
    ;; induced by the given relation.

    (define equivalence-classes
      (lambda (rel)
        (list->set
          (map list->set
               (let outer ((rest (set->list (relation-rec-left-domain rel))))
                 (if (null? rest)
                     '()
                     (let ((item (car rest)))
                       (let inner ((components (outer (cdr rest))))
                         (cond ((null? components) (list (list item)))
                               ((bears-to? rel item (caar components))
                                (cons (cons item (car components))
                                      (cdr components)))
                               (else (cons (car components)
                                           (inner (cdr components)))))))))))))

    ;; The partial-ordering? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether the given relation is a partial ordering.

    (define partial-ordering?
      (lambda (rel)
        (and (reflexive-relation? rel)
             (antisymmetric-relation? rel)
             (transitive-relation? rel))))

    ;; The total-ordering? predicate
    ;; takes an enclosed relation as its argument
    ;; and determines whether the given relation is a total ordering
    ;; of the given set.

    (define total-ordering?
      (lambda (rel)
        (and (partial-ordering? rel)
             (connected-relation? rel))))

    ;; The converse-relation procedure
    ;; takes a relation as its argument
    ;; and returns the converse of that relation:
    ;; a relation that a value x bears to another (or possibly the same) value y
    ;; if, and only if, y bears the given relation to x.

    (define converse-relation
      (lambda (rel)
        (make-relation-rec (relation-rec-right-domain rel)
                           (relation-rec-left-domain rel)
                           (set-map (lambda (pr)
                                      (cons (cdr pr) (car pr)))
                                    (relation-rec-pairs rel)))))

    ;; The left-domain procedure
    ;; takes a relation as its argument
    ;; and returns the left domain on which that relation is defined.

    (define left-domain relation-rec-left-domain)

    ;; The right-domain procedure
    ;; takes a relation as its argument
    ;; and returns the right domain on which that relation is defined.

    (define right-domain relation-rec-right-domain)

    ;; The left-range procedure
    ;; takes a relation as argument
    ;; and returns the set of values that bear the relation to anything.

    (define left-range
      (lambda (rel)
        (set-map car (relation-rec-pairs rel))))

    ;; The right-range procedure
    ;; takes a relation as argument
    ;; and returns the set of values to which anything bears the relation.

    (define right-range
      (lambda (rel)
        (set-map cdr (relation-rec-pairs rel))))

    ;; The image procedure computes and returns (as a set)
    ;; the image of a given set under a given relation --
    ;; that is, the set of values
    ;; to which members of the given set bear the given relation.

    (define image
      (lambda (aro rel)
        (set-map cdr (set-sunder (relation-rec-pairs rel)
                                 (lambda (pr)
                                   (in-set? (car pr) aro))))))

    ;; The preimage procedure computes and returns (as a set)
    ;; the preimage of a given set under a given relation --
    ;; that is, the set of values
    ;; that bear the given relation to members of the given set.

    (define preimage
      (lambda (aro rel)
        (set-map car (set-sunder (relation-rec-pairs rel)
                                 (lambda (pr)
                                   (in-set? (cdr pr) aro))))))

    ;; The relational-product procedure
    ;; takes two relations as arguments
    ;; and returns the relation
    ;; that a value x bears to another (or possibly the same) value z
    ;; if, and only if, there is some value y
    ;; to which x bears the first given relation
    ;; and which bears the second given relation to z.

    ;; The order of the arguments in the call to this procedure
    ;; matches the alternative "vertical bar" notation
    ;; from the handout on relations,
    ;; which is the opposite of the order for the primary "ring" notation.
    ;; In other words, to get the effect of the handout's "R ∘ S",
    ;; write the call as (relational-product S R).

    ;; This procedure presupposes
    ;; that the right domain of its first argument
    ;; is equal, as a set,
    ;; to the left domain of its second argument.

    (define relational-product
      (lambda (left right)
        (make-relation-rec
          (relation-rec-left-domain left)
          (relation-rec-right-domain right)
          (grand-union
            (set-map (lambda (start)
                       (set-map (lambda (item)
                                  (cons start item))
                                (image (image (set start) left)
                                       right)))
                     (left-range left))))))

    ;; The relational-power procedure
    ;; takes an enclosed relation and a natural number as arguments
    ;; and returns the relational power of the relation
    ;; to the specified exponent.

    ;; Since we have no way to represent universal identity as a relation,
    ;; in this implementation the zeroth relational power
    ;; is the identity relation restricted to the left domain
    ;; of the given relation.

    (define relational-power
      (lambda (rel exponent)
        (let kernel ((remaining exponent))
          (if (zero? remaining)
              (make-relation-rec (relation-rec-left-domain rel)
                                 (relation-rec-right-domain rel)
                                 (set-map (lambda (item)
                                            (cons item item))
                                          (left-domain rel)))
              (relational-product rel (kernel (- remaining 1)))))))

    ;; The ancestral procedure computes and returns
    ;; the ancestral of a given enclosed relation.

    ;; In this implementation,
    ;; we start an accumulator
    ;; as the identity relation
    ;; restricted to the domain of the given relation,
    ;; and repeatedly form its relational product with the given relation,
    ;; updating the accumulator by taking its union with the new product.
    ;; Since all of our relations are finite,
    ;; eventually we reach a fixed point under this operation;
    ;; that's the ancestral.

    (define ancestral
      (lambda (rel)
        (let kernel ((acc (make-relation-rec
                           (relation-rec-left-domain rel)
                           (relation-rec-right-domain rel)
                           (set-map (lambda (item)
                                      (cons item item))
                                    (relation-rec-left-domain rel)))))
          (let ((next (relational-union acc (relational-product acc rel))))
            (if (relation=? next acc)
                acc
                (kernel next))))))

    ;; The relational-for-each procedure
    ;; applies a given binary procedure,
    ;; to any values x and y such that x bears a given relation to y.
    ;; The return value is unspecified.

    (define relational-for-each
      (lambda (proc rel)
        (set-for-each (lambda (pr)
                        (proc (car pr) (cdr pr)))
                      (relation-rec-pairs rel))))

    ;; The pair-set->relation procedure
    ;; converts any set of pairs
    ;; into a relation
    ;; that the car of any of those pairs bears to the corresponding cdr.

    (define pair-set->relation
      (lambda (pairs)
        (make-relation-rec (set-map car pairs) (set-map cdr pairs) pairs)))

    ;; The relation->pair-set procedure
    ;; converts any relation into a set of pairs
    ;; such that the car of any of those pairs bears the relation to the cdr.

    (define relation->pair-set relation-rec-pairs)

    ;; The display-relation procedure
    ;; writes an external representation of a relation
    ;; to a specified output port
    ;; (or to standard output, if no output port is specified).

    (define display-relation
      (lambda (rel . extras)
        (let ((out (if (null? extras)
                       (current-output-port)
                       (car extras)))
              (ls (set->list (relation->pair-set rel))))
          (if (null? ls)
              (display "{}" out)
              (begin
                (display #\{ out)
                (let loop ((first-car (caar ls))
                           (first-cdr (cdar ls))
                           (rest (cdr ls)))
                  (display #\< out)
                  (cond ((set? first-car) (display-set first-car out))
                        ((bag? first-car) (display-bag first-car out))
                        (else (write first-car out)))
                  (display ", " out)
                  (cond ((set? first-cdr) (display-set first-cdr out))
                        ((bag? first-cdr) (display-bag first-cdr out))
                        (else (write first-cdr out)))
                  (display ">")
                  (unless (null? rest)
                    (display ", " out)
                    (loop (caar rest) (cdar rest) (cdr rest))))
                (display #\} out))))))
    
    ;; The relation->string procedure
    ;; constructs and returns a string representation of a given relation.

    (define relation->string
      (lambda (rel)
        (let ((out (open-output-string)))
          (display-relation rel out)
          (let ((result (get-output-string out)))
            (close-port out)
            result))))))

;;; copyright © 2013, 2014, 2016, 2018 John David Stone

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
