#!r7rs

;;; This is the test program for the suite and test syntaxes.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; created March 9, 1999
;;; last revised August 8, 2018

;;; Import the syntaxes to be tested.

(import (scheme base)
        (discrete testing))

(suite suite-and-test-syntaxes

  ;; Define the local procedure exact-number?.

  ((exact-number? (lambda (something)
                    (and (number? something) (exact? something)))))

  ;; A minimal case: a do-nothing expression that always passes a
  ;; do-nothing test.

  (test do-nothing
    (values)
    0 ()
    (lambda () #t))

  ;; Test whether zero is even.

  (test predicted-success
    (test should-succeed
      0
      1 (exact-number?)
      even?)
    1 (boolean?)
    (lambda (result)
      (eq? result #t)))

  ;; Test whether zero is odd.
  ;; (The parameterize-expression redirects the failure report
  ;; that would otherwise be printed to standard output
  ;; into a string port, which is discarded.)

  (test predicted-failure
    (let ((out (open-output-string)))
      (parameterize ((current-output-port out))
        (let ((result (test should-fail
                        0
                        1 (exact-number?)
                        odd?)))
          (close-port out)
          result)))
    1 (boolean?)
    not)

  ;; Make sure that the test syntax deals correctly with multiple
  ;; values.

  (test multiple-values
    (values 946 192 172 135 52)
    5 (exact-number? exact-number? exact-number? exact-number?
       exact-number?)
    >)

  ;; Make sure that the variant syntax in which the joint test is
  ;; omitted works correctly, too.

  (test variant-syntax
    (values #f (list))
    2 (not null?)))

;;; An empty suite should report no errors.

(suite empty-suite ())

;;; copyright © 2011, 2016, 2018 John David Stone

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
