#!r7rs

;;; Tests for the (csc-151 extensions) library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created June 29, 2018
;;; last revised January 24, 2019

(import (scheme base)
        (csc-151 extensions)
        (discrete testing))

(define true? (right-section eq? #t))
(define false? not)

(suite o ((sum-and-difference (lambda (left right)
                                (values (+ left right) (- left right)))))
  (test no-procedures
    (o)
    1 (procedure?)
    (lambda (result)
      (suite no-procedures-internal ()
        (test no-arguments
          (result)
          0 ())
        (test some-arguments
          (result 'dhhee 'eov 'nesrdar 'tsblae)
          4 (symbol? symbol? symbol? symbol?)
          (lambda results
            (equal? results (list 'dhhee 'eov 'nesrdar 'tsblae)))))))
  (test one-procedure
    (o sum-and-difference)
    1 (procedure?)
    (lambda (result)
      (suite one-procedure-internal ()
        (test sum-and-difference
          (result 611 181)
          2 (integer? integer?)
          (lambda results
            (equal? results (list 792 430)))))))
  (test several-procedures
    (o not even? square (section quotient <> 2))
    1 (procedure?)
    (lambda (result)
      (suite several-procedures-internal ()
        (test odd-square-half
          (result 842)
          1 (true?))
        (test even-square-half
          (result 353)
          1 (false?)))))
  (test minimal
    (o (lambda () (values)) (lambda () (values)))
    1 (procedure?)
    (lambda (result)
      (suite minimal-internal ()
        (test minimal-in-inaction
          (result)
          0 ()))))
 (test nullary-inner
    (o square (lambda () 465))
    1 (procedure?)
    (lambda (composer)
      (suite nullary-inner-internal ()
        (test square-of-constant
          (composer)
          1 ((right-section equal? (square 465)))))))
  (test no-intermediates
    (o (lambda () 'mtoas) (lambda (unua dua tria) (values)))
    1 (procedure?)
    (lambda (composer)
      (suite no-intermediates-internal ()
        (test invoke-outer
          (composer 'edf 'tsmt 'dgascn)
          1 ((right-section eq? 'mtoas))))))
  (test multiple-intermediates
    (o expt (lambda (num)
              (values (square num) (double num))))
    1 (procedure?)
    (lambda (composer)
      (suite multiple-intermediates-internal ()
        (test square-to-double-power
          (composer 15)
          1 ((right-section equal? (expt (square 15) (double 15))))))))
  (test variable-arity-inner
    (o square +)
    1 (procedure?)
    (lambda (composer)
      (suite variable-arity-inner-internal ()
        (test add-and-square
          (composer 76 958 576 250 263)
          1 ((right-section equal? (square (+ 76 958 576 250 263))))))))
  (test variable-arity-outer
    (o + (lambda arguments
           (apply values (map double arguments))))
    1 (procedure?)
    (lambda (composer)
      (suite variable-arity-outer-internal ()
        (test double-and-add
          (composer 989 69 577 820 475 478)
          1 ((right-section equal? (+ (double 989) (double 69)
                                      (double 577) (double 820)
                                      (double 475) (double 478)))))))))

(suite eof-constant ()
  (test is-eof?
    eof
    1 (eof-object?)))

(suite pi ()
  (test check-value
    pi
    1 ((right-section equal? 3.141592653589793))))

(suite gensym ()
  (test different-symbols?
    (values (gensym) (gensym))
    2 (symbol? symbol?)
    (negate eq?)))

(suite sort ()
  (test empty-list
    (sort null <)
    1 (null?))
  (test singleton
    (sort (list 'eneoai) (comparator string<? string->symbol))
    1 ((right-section equal? (list 'eneoai))))
  (test larger-bag
    (sort (iota 479) >)
    1 ((right-section equal? (reverse (iota 479)))))
  (test shuffled
    (sort (let ((vec (list->vector (iota 382))))
            (do ((index 381 (- index 1)))
                ((zero? index) (vector->list vec))
              (let* ((new-slot (random (+ index 1))))
                (unless (= new-slot index)
                  (let ((temp (vector-ref vec index)))
                    (vector-set! vec index (vector-ref vec new-slot))
                    (vector-set! vec new-slot temp))))))
          <)
    1 ((right-section equal? (iota 382))))
  (test all-equal
    (sort (make-list 89 423) >)
    1 ((right-section equal? (make-list 89 423))))
  (test random-elements
    (sort (list 5 7 2 8 6 2 4 1 9 2 3 7 2) <)
    1 ((right-section equal? (list 1 2 2 2 2 3 4 5 6 7 7 8 9)))))

(suite take ()
  (test all-of-list
    (take (list 'ree 'anoni 'trrri 'emht 'iwaom) 5)
    1 ((right-section equal? (list 'ree 'anoni 'trrri 'emht 'iwaom))))
  (test some-of-list
    (take (list 'rhirfht 'tre 'dgs 'gisn 'urshlom 'hun 'box) 4)
    1 ((right-section equal? (list 'rhirfht 'tre 'dgs 'gisn))))
  (test none-of-list
    (take (list 'crleat 'fmthbl 'tilei) 0)
    1 (null?))
  (test empty-list
    (take null 0)
    1 (null?)))

(suite drop ()
  (test all-of-list
    (drop (list 'npcbkie 'nhh 'oethoer 'cchooa 'eaoif) 5)
    1 (null?))
  (test some-of-list
    (drop (list 'las 'aaleri 'nmo 'fnoe 'oasn 'odh 'amcc) 4) 
    1 ((right-section equal? (list 'oasn 'odh 'amcc))))
  (test none-of-list
    (drop (list 'lrrw 'frbfun 'nrgte) 0)
    1 ((right-section equal? (list 'lrrw 'frbfun 'nrgte))))
  (test empty-list
    (drop null 0)
    1 (null?)))

(suite filter ()
  (test empty-list
    (filter (lambda (item) #t) null)
    1 (null?))
  (test singleton-list-satisfying
    (filter even? (list 112))
    1 ((right-section equal? (list 112))))
  (test singleton-list-unsatisfying
    (filter even? (list 423))
    1 (null?))
  (test all-elements-removed
    (filter (lambda (item) #f)
            (list 'ewhlrum 'toetl 'trrhpa 'eue 'oern))
    1 (null?))
  (test some-elements-removed
    (filter even? (list 399 542 442 789 897 126 486 908))
    1 ((right-section equal? (list 542 442 126 486 908))))
  (test no-elements-removed
    (filter (lambda (item) #t) (list 'witel 'ried 'asla 'toiev 'erfeh))
    1 ((right-section equal? (list 'witel 'ried 'asla 'toiev 'erfeh)))))

(suite negate ()
  (test nullary-predicate
    (negate (lambda () #f))
    1 (procedure?)
    (lambda (complement)
      (suite nullary-predicate-internal ()
        (test it-is-true
          (complement)
          1 (true?)))))
  (test non-zero?
    (negate zero?)
    1 (procedure?)
    (lambda (non-zero?)
      (suite non-zero?-internal ()
        (test zero
          (non-zero? 0)
          1 (false?))
        (test other
          (non-zero? 433)
          1 (true?)))))
  (test uneven?
    (negate even?)
    1 (procedure?)
    (lambda (uneven?)
      (suite uneven?-internal ()
        (test even
          (uneven? 788)
          1 (false?))
        (test odd
          (uneven? 453)
          1 (true?)))))
  (test non-ascending?
    (negate <)
    1 (procedure?)
    (lambda (non-ascending?)
      (suite non-ascending?-internal ()
        (test up
          (non-ascending? 716 722 927 959)
          1 (false?))
        (test down
          (non-ascending? 762 653 444 108)
          1 (true?))

        ;; Note that (negate <) is not the same as >
        ;; if there are more than two arguments!

        (test wobbly
          (non-ascending? 178 565 480 726)
          1 (true?))))))

(suite conjoin ()
  (test nullary-predicates
    (conjoin (lambda () #t) (lambda () #f))
    1 (procedure?)
    (lambda (conjoined)
      (suite nullary-predicates-internal ()
        (test it-is-false
          (conjoined)
          1 (false?)))))
  (test really-special?
    (conjoin (lambda (num)
               (zero? (modulo num 3)))
             odd?)
    1 (procedure?)
    (lambda (conjoined)
      (suite really-special?-internal ()
        (test both-satisfied
          (conjoined 15)
          1 (true?))
        (test left-satisfied
          (conjoined 192)
          1 (false?))
        (test right-satisfied
          (conjoined 889)
          1 (false?))
        (test neither-satisfied
          (conjoined 40)
          1 (false?)))))
  (test Pythagorean?
    (conjoin < (lambda (short-leg long-leg hypotenuse)
                 (= (+ (square short-leg) (square long-leg))
                    (square hypotenuse))))
    1 (procedure?)
    (lambda (conjoined)
      (suite Pythagorean?-internal ()
        (test unlikely
          (conjoined 188 635 663)
          1 (false?))
        (test better
          (conjoined 133 156 205)
          1 (true?))))))

(suite disjoin ()
  (test nullary-predicates
    (disjoin (lambda () #t) (lambda () #f))
    1 (procedure?)
    (lambda (disjoined)
      (suite nullary-predicates-internal ()
       (test it-is-true
         (disjoined)
         1 (true?)))))
  (test special?
    (disjoin (lambda (num)
               (zero? (modulo num 3)))
             odd?)
    1 (procedure?)
    (lambda (disjoined)
      (suite special?-internal ()
        (test both-satisfied
          (disjoined 249)
          1 (true?))
        (test left-satisfied
          (disjoined 276)
          1 (true?))
        (test right-satisfied
          (disjoined 17)
          1 (true?))
        (test neither-satisfied
          (disjoined 758)
          1 (false?)))))
  (test monotonic?
    (disjoin < >)
    1 (procedure?)
    (lambda (disjoined)
      (suite monotonic?-internal ()
        (test ascending
          (disjoined 219 312 488 698)
          1 (true?))
        (test descending
          (disjoined 805 534 471 153 53)
          1 (true?))
        (test wobbly
          (disjoined 54 1 232)
          1 (false?))))))

;;; copyright © 2018, 2019 John David Stone

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
