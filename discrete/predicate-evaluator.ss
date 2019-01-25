#!r7rs

;;; An evaluator for the predicate calculus

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 7, 2013
;;; last revised January 24, 2019

;;; This file provides a procedure
;;; for evaluating a term or a statement of the predicate calculus
;;; relative to an assignment of values to the identifiers that occur in it.

(define-library (discrete predicate-evaluator)
  (export make-assignment assignment? assignment-universe
          assignment-variables assignment-functions
          assignment-predicates evaluate-term evaluate-statement)
  (import (scheme base)
          (csc-151 extensions)
          (discrete utilities)
          (discrete sets)
          (discrete relations)
          (discrete predicate-parser))
  (begin

    ;; As assignment is a record with four fields.
    ;; The first, universe, is a set of the values
    ;; constituting the universe of discourse.
    ;; The second, variables, is an association list
    ;; (that is, a list of pairs),
    ;; in which the car of each element
    ;; is a string giving the name of a variable
    ;; and the cdr is one of the values in the universe.
    ;; The third field, functions,
    ;; is also an association list,
    ;; in which the car of each pair is a string
    ;; giving the name of a function
    ;; and the cdr is a functional relation
    ;; representing the function denoted by that name.
    ;; The fourth field, predicates,
    ;; is also an association list,
    ;; in which the car of each pair is a string
    ;; giving the name of a predicate
    ;; and the cdr is a set of lists
    ;; representing the extension of that predicate.

    (define-record-type <assignment>
      (make-assignment universe variables functions predicates)
      assignment?
      (universe assignment-universe)
      (variables assignment-variables)
      (functions assignment-functions)
      (predicates assignment-predicates))

    ;; The apply-function procedure
    ;; takes as its arguments the denotation of a function name
    ;; (a functional relation,
    ;; in which the car of each pair in the underlying set of pairs
    ;; is a list of possible arguments to the function)
    ;; and a list of argument values.
    ;; It returns the value that the specified function
    ;; yields when applied to those values.

    ;; This function presupposes that the list of argument values
    ;; is the car of one of the pairs
    ;; in the set of pairs that underlies the relation.
    ;; It signals an error if this precondition isn't met.

    (define apply-function
      (lambda (func args)
        (let ((yield (assoc args (set->list (relation->pair-set func)))))
          (if (pair? yield)
              (cdr yield)
              (error 'apply-function "Domain error." func args)))))

    ;; The evaluate-term procedure
    ;; determines the value of a given term
    ;; relative to a given assignment of values
    ;; to the variables and function names it contains.

    (define evaluate-term
      (lambda (term assig)
        (cond ((simple-term? term)
               (let ((result (assoc (simple-term-name term)
                                    (assignment-variables assig))))
                 (if (pair? result)
                     (cdr result)
                     (error 'evaluate-term
                            (string-append "The variable "
                                           (term->string term)
                                           " is unassigned.")
                            term
                            assig))))
              ((function-term? term)
               (let ((func (assoc (function-term-function-name term)
                                  (assignment-functions assig))))
                 (if (pair? func)
                     (apply-function (cdr func)
                                     (map (lambda (term)
                                            (evaluate-term term assig))
                                          (function-term-arguments term)))
                     (error 'evaluate-term
                            (string-append "The function name "
                                           (function-term-function-name term)
                                           " is unassigned.")
                            term
                            assig)))))))

    ;; The replace-in-alist procedure
    ;; takes an association list as its first argument,
    ;; searches through it for a pair
    ;; whose car is its second argument,
    ;; and replaces that pair with a new pair
    ;; comprising its second and third arguments.
    ;; It returns a copy of the given association list
    ;; with the matching pair replaced with the new one.

    (define replace-in-alist
      (lambda (alist key new-val)
        (let kernel ((rest alist))
          (cond ((null? rest) '())
                ((equal? (caar rest) key)
                 (cons (cons key new-val) (cdr rest)))
                (else
                 (cons (car rest) (kernel (cdr rest))))))))  

    ;; The variant-assignments procedure
    ;; takes two arguments, an assignment and a variable,
    ;; and returns a list of assignments exactly like the given assignment
    ;; except possibly in the value assigned to the given variable.
    ;; There is one assignment on the list returned
    ;; for each value in the universe of discourse,
    ;; with that value being assigned to the given variable.

    (define variant-assignments
      (lambda (assig var)
        (let ((uni (assignment-universe assig)))
          (set->list
            (set-map (lambda (val)
                       (make-assignment uni
                                        (replace-in-alist
                                          (assignment-variables assig)
                                          var
                                          val)
                                        (assignment-functions assig)
                                        (assignment-predicates assig)))
                     uni)))))

    ;; The evaluate-statement procedure
    ;; determines the truth-value of a given statement of the predicate calculus
    ;; relative to a given assignment.

    (define evaluate-statement
      (lambda (stmt assig)
        (let kernel ((subexp stmt))
          (cond ((literal-statement? subexp)
                 (literal-statement-polarity subexp))
                ((atomic-statement? subexp)
                 (assert #f evaluate-statement))
                ((negation-statement? subexp)
                 (not (kernel (negation-statement-negand subexp))))
                ((conjunction-statement? subexp)
                 (and (kernel (conjunction-statement-left subexp))
                      (kernel (conjunction-statement-right subexp))))
                ((disjunction-statement? subexp)
                 (or (kernel (disjunction-statement-left subexp))
                     (kernel (disjunction-statement-right subexp))))
                ((implication-statement? subexp)
                 (or (not (kernel (implication-statement-protasis subexp)))
                     (kernel (implication-statement-apodosis subexp))))
                ((equivalence-statement? subexp)
                 (eq? (kernel (equivalence-statement-left subexp))
                      (kernel (equivalence-statement-right subexp))))
                ((universal-statement? subexp)
                 (all (section evaluate-statement
                               (universal-statement-body subexp)
                               <>)
                      (variant-assignments
                        assig
                        (universal-statement-variable subexp))))
                ((existential-statement? subexp)
                 (assert #f evaluate-statement))))))))

;;; copyright © 2013, 2014, 2016, 2018, 2019 John David Stone

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
