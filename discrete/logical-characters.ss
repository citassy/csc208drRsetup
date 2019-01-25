#!r7rs

;; The (discrete logical-characters) library
;; assigns names to the special Unicode characters
;; that occur in expressions of the propositional and predicate calculi.

(define-library (discrete logical-characters)
  (export verum falsum neg caret wedge impl iff uquant equant)
  (import (scheme base)
          (scheme char))
  (begin
    (define verum #\𝗧)
    (define falsum #\𝗙)
    (define neg #\¬)
    (define caret #\∧)
    (define wedge #\∨)
    (define impl #\→)
    (define iff #\↔)
    (define uquant #\∀)
    (define equant #\∃)))

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
