#!r7rs

;;; Tests for the (csc-151 rebelsky) library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created June 29, 2018
;;; last revised January 24, 2019

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme char)
        (csc-151 extensions)
        (discrete testing))

(suite index-of ((ls (list 'epsilon 'zeta 'eta 'theta 'iota)))
  (test null-list
    (index-of 'alpha null)
    1 ((right-section equal? -1)))
  (test singleton-list-sought-present
    (index-of 'beta (list 'beta))
    1 ((right-section equal? 0)))
  (test singleton-list-sought-absent
    (index-of 'gamma (list 'delta))
    1 ((right-section equal? -1)))
  (test sought-at-front
    (index-of 'epsilon ls)
    1 ((right-section equal? 0)))
  (test sought-at-rear
    (index-of 'iota ls)
    1 ((right-section equal? 4)))
  (test sought-in-middle
    (index-of 'eta ls)
    1 ((right-section equal? 2)))
  (test sought-absent
    (index-of 'kappa ls)
    1 ((right-section equal? -1))))

(suite iota ()
  (test one
    (iota 1)
    1 ((right-section equal? (list 0))))
  (test larger
    (iota 17)
    1 ((right-section equal?
                      (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))))

(suite reduce ()
  (test singleton
    (reduce * (list 23))
    1 ((right-section equal? 23)))
  (test longer-list
    (reduce + (list 17 49 83 22 16))
    1 ((right-section equal? 187))))

(suite reduce-left ()
  (test singleton
    (reduce-left / (list 23))
    1 ((right-section equal? 23)))
  (test longer-list
    (reduce-left - (list 17 49 83 22 16))
    1 ((right-section equal? -153)))
  (test reverser
    (reduce-left (lambda (base new) (cons new base))
                 (list null 'alpha 'beta 'gamma))
    1 ((right-section equal? (list 'gamma 'beta 'alpha)))))

(suite reduce-right ()
  (test singleton
    (reduce-right / (list 23))
    1 ((right-section equal? 23)))
  (test longer-list
    (reduce-right - (list 17 49 83 22 16))
    1 ((right-section equal? 45)))
  (test copier
    (reduce-right cons (list 'alpha 'beta 'gamma null))
    1 ((right-section equal? (list 'alpha 'beta 'gamma)))))

(suite tally-all () 
  (test null-list
    (tally-all null)
    1 (null?))
  (test singleton
    (tally-all (list 'alpha))
    1 ((right-section equal? (list (list 'alpha 1)))))
  (test longer-list
    (tally-all (list "alpha" "beta" "alpha" "gamma" "alpha" "beta" "alpha"))
    1 ((conjoin list? (section all list? <>)))
    (lambda (result)
      (equal? (sort result (comparator string<? car))
              (list (list "alpha" 4)
                    (list "beta" 2)
                    (list "gamma" 1))))))

(suite take-random ((source (list 41 83 60 14 9 22 43))
                    (duplicates (list 53 53 53 53 53)))
  (test null-list
    (take-random null 0)
    1 (null?))
  (test zero-selections
    (take-random source 0)
    1 (null?))
  (test one-selection
    (take-random source 1)
    1 (list?)
    (lambda (result)
      (if (member (car result) source) #t #f)))
  (test mid-selection
    (take-random source 4)
    1 (list?)
    (lambda (result)
      (all (lambda (element)
             (if (member element source) #t #f))
           result)))
  (test all-selections
    (take-random source (length source))
    1 (list?)
    (lambda (result)
      (equal? (sort result <) (sort source <))))
  (test with-duplicates
    (take-random duplicates 3)
    1 ((right-section equal? (list 53 53 53)))))

(define true? (right-section eq? #t))
(define false? not)

(suite all ()
  (test null-list
    (all even? null)
    1 (true?))
  (test all-satisfying
    (all even? (list 882 298 800 196))
    1 (true?))
  (test some-satisfying
    (all even? (list 279 586 984 212 841))
    1 (false?))
  (test none-satisfying
    (all even? (list 575 939 87))
    1 (false?)))

(suite any ()
  (test null-list
    (any odd? null)
    1 (false?))
  (test all-satisfying
    (any odd? (list 575 939 87))
    1 (true?))
  (test some-satisfying
    (any odd? (list 586 984 279 212 841))
    1 (true?))
  (test none-satisfying
    (any odd? (list 882 298 800 196))
    1 (false?)))
                                                    
(suite comparator ()
  (test example
    (comparator < car)
    1 (procedure?)
    (lambda (comp?)
      (suite comparator-internal ()
        (test affirmative
          (comp? (cons 25 'alpha) (cons 37 'beta))
          1 (true?))
        (test negative
          (comp? (cons 52 'gamma) (cons 11 'delta))
          1 (false?))))))

(suite left-section ()
  (test example
    (left-section - 23)
    1 (procedure?)
    (lambda (proc1)
      (suite left-section-internal ()
        (test arithmetic
          (proc1 47)
          1 (number?)
          (lambda (result)
            (= result -24)))))))

(suite l-s ()
  (test example
    (l-s cons 'alpha)
    1 (procedure?)
    (lambda (proc1)
      (suite l-s-internal ()
        (test list-construction
          (proc1 (list 'beta))
          1 (list?)
          (lambda (result)
            (equal? result (list 'alpha 'beta))))))))

(suite right-section ()
  (test example
    (right-section cons (list 'alpha))
    1 (procedure?)
    (lambda (proc1)
      (suite right-section-internal ()
        (test list-construction
          (proc1 'beta)
          1 (list?)
          (lambda (result)
            (equal? result (list 'beta 'alpha))))))))

(suite r-s ()
  (test example
    (r-s - 23)
    1 (procedure?)
    (lambda (proc1)
      (suite r-s-internal ()
        (test arithmetic
          (proc1 47)
          1 (number?)
          (lambda (result)
            (= result 24)))))))

(suite decrement ()
  (test zero
    (decrement 0)
    1 ((right-section equal? -1)))
  (test negative
    (decrement -5)
    1 ((right-section equal? -6)))
  (test large
    (decrement 1238098160987092837100)
    1 ((right-section equal? 1238098160987092837099))))

(suite double ()
  (test small
    (double 12)
    1 ((right-section equal? 24)))
  (test large
    (double -89298929989)
    1 ((right-section equal? -178597859978))))

(suite increment ()
  (test zero
    (increment -1)
    1 ((right-section equal? 0)))
  (test negative
    (increment -42)
    1 ((right-section equal? -41)))
  (test large
    (increment 987928837981723098099)
    1 ((right-section equal? 987928837981723098100))))

(define (temp-file-name)
  (string-append "/tmp/" (number->string (random 1000000000))))

(suite file->chars ((empty-file
                     (let ((fn (temp-file-name)))
                       (close-port (open-output-file fn))
                       fn))
                    (sample-file
                     (let* ((fn (temp-file-name))
                            (out (open-output-file fn)))
                       (display "This is the forest primeval." out)
                       (newline out)
                       (close-port out)
                       fn)))
  (test empty
    (file->chars empty-file)
    1 (null?))
  (test sample
    (file->chars sample-file)
    1 ((right-section equal?
                      (list #\T #\h #\i #\s #\space #\i #\s #\space
                            #\t #\h #\e #\space #\f #\o #\r #\e
                            #\s #\t #\space #\p #\r #\i #\m #\e
                            #\v #\a #\l #\. #\newline)))))

(suite file->lines ((empty-file
                     (let ((fn (temp-file-name)))
                       (close-port (open-output-file fn))
                       fn))
                    (sample-file
                     (let* ((fn (temp-file-name))
                            (out (open-output-file fn)))
                       (display "This is the forest primeval." out)
                       (newline out)
                       (display "The murmuring pines and the hemlocks ..." out)
                       (newline out)
                       (close-port out)
                       fn)))
  (test empty
    (file->lines empty-file)
    1 (null?))
  (test sample
    (file->lines sample-file)
    1 ((right-section equal?
                      (list "This is the forest primeval."
                            "The murmuring pines and the hemlocks ...")))))

(suite file->words ((empty-file
                     (let ((fn (temp-file-name)))
                       (close-port (open-output-file fn))
                       fn))
                    (sample-file
                     (let* ((fn (temp-file-name))
                            (out (open-output-file fn)))
                       (display "This is the forest primeval." out)
                       (newline out)
                       (display "The murmuring pines and the hemlocks ..." out)
                       (newline out)
                       (close-port out)
                       fn)))
  (test empty
    (file->words empty-file)
    1 (null?))
  (test sample
    (file->words sample-file)
    1 ((right-section equal?
                      (list "This" "is" "the" "forest" "primeval"
                            "The" "murmuring" "pines" "and" "the"
                            "hemlocks")))))

(suite read-word ((sample (open-input-string "teeter1--2totter")))
  (test terminated-by-non-word-whare
    (read-word sample)
    1 ((right-section equal? "teeter1")))
  (test terminated-by-eof
    (let ((result (read-word sample)))
      (close-port sample)
      result)
    1 ((right-section equal? "2totter"))))

(suite read-until ((sample
                    (open-input-string ":alpha@beta=gamma-delta0epsilon")))
  (test null-result
    (read-until sample #\:)
    1 ((right-section equal? "")))
  (test character-terminator
    (read-until sample #\@)
    1 ((right-section equal? ":alpha")))
  (test string-of-terminators-1
    (read-until sample "-=")
    1 ((right-section equal? "@beta")))
  (test string-of-terminators-2
    (read-until sample "-/")
    1 ((right-section equal? "=gamma")))
  (test termination-by-predicate
    (read-until sample char-numeric?)
    1 ((right-section equal? "-delta")))
  (test termination-by-eof
    (let ((result (read-until sample #\%)))
      (close-port sample)
      result)
    1 ((right-section equal? "0epsilon"))))

(suite skip-char ((sample (open-input-string "@")))
  (test no-match
    (skip-char sample #\:)
    1 (false?))
  (test match
    (skip-char sample #\@)
    1 (true?))
  (test end-of-file
    (let ((result (skip-char sample #\tab)))
      (close-port sample)
      result)
    1 (false?)))

(suite char-in-string? ()
  (test null-string
    (char-in-string? "" #\a)
    1 (false?))
  (test only-char-in-string
    (char-in-string? "b" #\b)
    1 (true?))
  (test char-at-front
    (char-in-string? "cdefg" #\c)
    1 (true?))
  (test char-at-rear
    (char-in-string? "hijkl" #\l)
    1 (true?))
  (test char-in-middle
    (char-in-string? "mnopq" #\o)
    1 (true?))
  (test char-missing
    (char-in-string? "rstuvwxyz" #\&)
    1 (false?)))

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
