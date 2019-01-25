#!r7rs

;;; An evaluator for the propositional calculus

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 28, 2013
;;; last revised October 9, 2018

;;; This file provides procedures for evaluating a Boolean expression
;;; relative to an assignment of values to its variables.

(define-library (discrete propositional-evaluator)
  (export evaluate)
  (import (scheme base)
          (discrete propositional-parser))

  (begin

    ;; The evaluate procedure
    ;; determines the value of a given Boolean
    ;; expression relative to a given assignment of Boolean values
    ;; to the variables it contains.

    (define evaluate
      (lambda (expr assignment)
        (let kernel ((subexp expr))
          (cond ((literal-expression? subexp)
                 (literal-expression-polarity subexp))
                ((variable-expression? subexp)
                 (let* ((name (variable-expression-name subexp))
                        (pr (assoc name assignment)))
                   (if (pair? pr)
                       (cdr pr)
                       (error 'evaluate
                              (string-append "Unassigned variable "
                                             (expression->string subexp)
                                             ".")
                              expr
                              assignment))))
                ((negation-expression? subexp)
                 (not (kernel (negation-expression-negand subexp))))
                ((conjunction-expression? subexp)
                 (and (kernel (conjunction-expression-left subexp))
                      (kernel (conjunction-expression-right subexp))))
                ((disjunction-expression? subexp)
                 (or (kernel (disjunction-expression-left subexp))
                     (kernel (disjunction-expression-right subexp))))
                ((implication-expression? subexp)
                 (or (not (kernel (implication-expression-protasis subexp)))
                     (kernel (implication-expression-apodosis subexp))))
                ((equivalence-expression? subexp)
                 (eq? (kernel (equivalence-expression-left subexp))
                      (kernel (equivalence-expression-right subexp))))))))))

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
