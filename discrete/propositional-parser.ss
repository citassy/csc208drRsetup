#!r7rs

;;; A parser for the propositional calculus

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 28, 2013
;;; last revised October 9, 2018

;;; This file provides a parsing procedure
;;; that takes a string representing a Boolean expression
;;; and returns a syntax tree for that Boolean expression.

(define-library (discrete propositional-parser)
  (export make-literal-expression literal-expression?
          literal-expression-polarity make-variable-expression
          variable-expression? variable-expression-name
          make-negation-expression negation-expression?
          negation-expression-negand make-conjunction-expression
          conjunction-expression? conjunction-expression-left
          conjunction-expression-right make-disjunction-expression
          disjunction-expression? disjunction-expression-left
          disjunction-expression-right make-implication-expression
          implication-expression? implication-expression-protasis
          implication-expression-apodosis make-equivalence-expression
          equivalence-expression? equivalence-expression-left
          equivalence-expression-right expression? expression->string
          parse)
  (import (scheme base)
          (discrete logical-characters)
          (discrete propositional-scanner))

  (begin

    ;; The grammar for the propositional calculus is as follows:

    ;; <expression> ::= <literal>
    ;;                | <variable>
    ;;                | ¬ <expression>
    ;;                | ( <expression> ∧ <expression> )
    ;;                | ( <expression> ∨ <expression> )
    ;;                | ( <expression> → <expression> )
    ;;                | ( <expression> ↔ <expression> )

    ;; The record types for syntax trees reflects this grammar.

    (define-record-type <literal-expression>
      (make-literal-expression polarity)
      literal-expression?
      (polarity literal-expression-polarity))

    (define-record-type <variable-expression>
      (make-variable-expression name)
      variable-expression?
      (name variable-expression-name))

    (define-record-type <negation-expression>
      (make-negation-expression negand)
      negation-expression?
      (negand negation-expression-negand))

    (define-record-type <conjunction-expression>
      (make-conjunction-expression left right)
      conjunction-expression?
      (left conjunction-expression-left)
      (right conjunction-expression-right))

    (define-record-type <disjunction-expression>
      (make-disjunction-expression left right)
      disjunction-expression?
      (left disjunction-expression-left)
      (right disjunction-expression-right))

    (define-record-type <implication-expression>
      (make-implication-expression protasis apodosis)
      implication-expression?
      (protasis implication-expression-protasis)
      (apodosis implication-expression-apodosis))

    (define-record-type <equivalence-expression>
      (make-equivalence-expression left right)
      equivalence-expression?
      (left equivalence-expression-left)
      (right equivalence-expression-right))

    ;; The expression? predicate
    ;; determines whether its argument belongs to
    ;; any of the expression types.

    (define expression?
      (lambda (something)
        (or (literal-expression? something)
            (variable-expression? something)
            (negation-expression? something)
            (conjunction-expression? something)
            (disjunction-expression? something)
            (implication-expression? something)
            (equivalence-expression? something))))

    ;; The expression->string procedure
    ;; constructs and returns a human-readable string representation
    ;; of a given expression.

    (define expression->string
      (lambda (expr)
        (cond ((literal-expression? expr)
               (if (literal-expression-polarity expr)
                   (string verum)
                   (string falsum)))
              ((variable-expression? expr)
               (variable-expression-name expr))
              ((negation-expression? expr)
               (string-append (string neg)
                              (expression->string
                               (negation-expression-negand expr))))
              ((conjunction-expression? expr)
               (string-append "("
                              (expression->string
                               (conjunction-expression-left expr))
                              (string #\space caret #\space)
                              (expression->string
                               (conjunction-expression-right expr))
                              ")"))
              ((disjunction-expression? expr)
               (string-append "("
                              (expression->string
                               (disjunction-expression-left expr))
                              (string #\space wedge #\space)
                              (expression->string
                               (disjunction-expression-right expr))
                              ")"))
              ((implication-expression? expr)
               (string-append "("
                              (expression->string
                               (implication-expression-protasis expr))
                              (string #\space impl #\space)
                              (expression->string
                               (implication-expression-apodosis expr))
                              ")"))
              ((equivalence-expression? expr)
               (string-append "("
                              (expression->string
                               (equivalence-expression-left expr))
                              (string #\space iff #\space)
                              (expression->string
                               (equivalence-expression-right expr))
                              ")")))))

    ;; The acquire-token procedure recovers a token
    ;; from a given source,
    ;; signalling an error if none is available.

    (define acquire-token
      (lambda (token-source)
        (when (token-source 'at-end?)
              (error 'acquire-token
                     "The end of the input was encountered unexpectedly."
                     token-source))            
        (token-source 'get)))

    ;; The match-and-discard procedure
    ;; gets a token from a given source and compares it
    ;; with the token that the parser expects to find.
    ;; If they don't match, an error is reported.

    (define match-and-discard
      (lambda (token-source match)
        (let ((discard (acquire-token token-source)))
          (unless (token=? match discard)
                  (error 'match-and-discard
                         (string-append "The token "
                                        (token->string discard)
                                        " does not match the expected token "
                                        (token->string match)
                                        ".")
                         token-source
                         match)))))

    ;; The parse-expression procedure parses an expression
    ;; from a given token source.

    (define parse-expression
      (lambda (token-source)

        ;; Get a token
        ;; and determine which of the analyses of expressions
        ;; should be used.

        (let ((current (acquire-token token-source)))
          (cond ((verum-token? current) (make-literal-expression #t))
                ((falsum-token? current) (make-literal-expression #f))
                ((variable-token? current)
                 (make-variable-expression (variable-token-name current)))
                ((paleft-token? current)
                 (let* ((left (parse-expression token-source))
                        (binop (acquire-token token-source))
                        (right (parse-expression token-source)))
                   (match-and-discard token-source (make-paright-token))
                   (cond ((and-token? binop)
                          (make-conjunction-expression left right))
                         ((or-token? binop)
                          (make-disjunction-expression left right))
                         ((impl-token? binop)
                          (make-implication-expression left right))
                         ((iff-token? binop)
                          (make-equivalence-expression left right))
                         (else
                          (error 'parse-expression
                                 (string-append "The token "
                                                (token->string binop)
                                                " is not a binary operator.")
                                 token-source)))))
                ((not-token? current)
                 (make-negation-expression (parse-expression token-source)))
                (else
                 (error 'parse-expression
                        (string-append "The token "
                                       (token->string current)
                                       " cannot begin an expression.")
                        token-source))))))

    ;; The parse procedure
    ;; takes a string representing a Boolean expression as its argument
    ;; and returns a syntax tree for that expression 

    (define parse
      (lambda (str)
        (let* ((token-source (scanner str))
               (syntax-tree (parse-expression token-source)))
          (if (token-source 'at-end?)
              syntax-tree
              (error 'parse
                     (string-append "There were extra, unusable tokens at "
                                    "the end of the expression."))))))))

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
