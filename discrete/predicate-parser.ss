#!r7rs

;;; A parser for the predicate calculus

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 7, 2013
;;; last revised November 25, 2018

;;; This file provides parsing procedures
;;; for extracting syntax trees
;;; for terms and statements of the predicate calculus
;;; from the strings that represent them.

(define-library (discrete predicate-parser)
  (export make-simple-term simple-term? simple-term-name make-function-term
          function-term? function-term-function-name function-term-arguments
          make-literal-statement literal-statement?
          literal-statement-polarity make-atomic-statement atomic-statement?
          atomic-statement-predicate atomic-statement-arguments
          make-negation-statement negation-statement?
          negation-statement-negand make-conjunction-statement
          conjunction-statement? conjunction-statement-left
          conjunction-statement-right make-disjunction-statement
          disjunction-statement? disjunction-statement-left
          disjunction-statement-right make-implication-statement
          implication-statement? implication-statement-protasis
          implication-statement-apodosis make-equivalence-statement
          equivalence-statement? equivalence-statement-left
          equivalence-statement-right make-universal-statement
          universal-statement? universal-statement-variable
          universal-statement-body make-existential-statement
          existential-statement? existential-statement-variable
          existential-statement-body term->string parse-term
          statement->string parse-statement parse)
  (import (scheme base)
          (scheme char)
          (discrete logical-characters)
          (discrete predicate-scanner))
  (begin          

    ;; The grammar for the propositional calculus is as follows:

    ;;      <term> ::= <identifier>
    ;;               | <identifier> <arguments>
    ;; <arguments> ::= ( )
    ;;               | ( <term> <term-list> )
    ;; <term-list> ::= 
    ;;               | , <term> <term-list>
    ;; <statement> ::= <literal>
    ;;               | <identifier> ( )
    ;;               | <identifier> ( <term> <term-list> )
    ;;               | ¬ <statement>
    ;;               | ( <statement> ∧ <statement> )
    ;;               | ( <statement> ∨ <statement> )
    ;;               | ( <statement> → <statement> )
    ;;               | ( <statement> ↔ <statement> )
    ;;               | ∀ <identifier> <statement>
    ;;               | ∃ <identifier> <statement>

    ;; In the rules that show an identifier
    ;; immediately preceding a left parenthesis,
    ;; the identifier must begin with a lower-case letter
    ;; in a term (indicating a function name),
    ;; an upper-case letter in a statement (indicating a predicate).

    ;; In a quantified statement,
    ;; when the variable of quantification immediately precedes an atomic statement,
    ;; the scanner will not correctly separate the variable of quantification
    ;; from the predicate at the beginning of the atomic statement
    ;; unless whitespace separates them.

    ;; The record types for syntax trees reflect this grammar.

    (define-record-type <simple-term>
      (make-simple-term name)
      simple-term?
      (name simple-term-name))

    (define-record-type <function-term>
      (make-function-term function-name arguments)
      function-term?
      (function-name function-term-function-name)
      (arguments function-term-arguments))

    (define-record-type <literal-statement>
      (make-literal-statement polarity)
      literal-statement?
      (polarity literal-statement-polarity))

    (define-record-type <atomic-statement>
      (make-atomic-statement predicate arguments)
      atomic-statement?
      (predicate atomic-statement-predicate)
      (arguments atomic-statement-arguments))

    (define-record-type <negation-statement>
      (make-negation-statement negand)
      negation-statement?
      (negand negation-statement-negand))

    (define-record-type <conjunction-statement>
      (make-conjunction-statement left right)
      conjunction-statement?
      (left conjunction-statement-left)
      (right conjunction-statement-right))

    (define-record-type <disjunction-statement>
      (make-disjunction-statement left right)
      disjunction-statement?
      (left disjunction-statement-left)
      (right disjunction-statement-right))

    (define-record-type <implication-statement>
      (make-implication-statement protasis apodosis)
      implication-statement?
      (protasis implication-statement-protasis)
      (apodosis implication-statement-apodosis))

    (define-record-type <equivalence-statement>
      (make-equivalence-statement left right)
      equivalence-statement?
      (left equivalence-statement-left)
      (right equivalence-statement-right))

    (define-record-type <universal-statement>
      (make-universal-statement variable body)
      universal-statement?
      (variable universal-statement-variable)
      (body universal-statement-body))

    (define-record-type <existential-statement>
      (make-existential-statement variable body)
      existential-statement?
      (variable existential-statement-variable)
      (body existential-statement-body))

    ;; The term? predicate determines
    ;; whether its argument belongs to either of the term types.

    (define term?
      (lambda (something)
        (or (simple-term? something)
            (function-term? something))))

    ;; The arguments->string procedure
    ;; constructs and returns a human-readable string representation
    ;; of the argument list in a function term or an atomic statement.

    (define arguments->string
      (lambda (args)
        (if (null? args)
            "()"
            (string-append
              "("
              (let loop ((first (car args))
                         (rest (cdr args)))
                (if (null? rest)
                    (term->string first)
                    (string-append (term->string first)
                                   ", "
                                   (loop (car rest) (cdr rest)))))
              ")"))))

    ;; The term->string procedure
    ;; constructs and returns a human-readable string representation
    ;; of a given term.

    (define term->string
      (lambda (term)
        (cond ((simple-term? term) (simple-term-name term))
              ((function-term? term)
               (string-append (function-term-function-name term)
                              (arguments->string
                                (function-term-arguments term)))))))

    ;; The acquire-token procedure recovers a token from a given source,
    ;; signalling an error if none is available.

    (define acquire-token
      (lambda (token-source)
        (when (token-source 'at-end?)
              (error 'acquire-token
                     "The end of the input was encountered unexpectedly."
                     token-source))            
        (token-source 'get)))

    ;; The match-and-discard procedure
    ;; gets a token from a given source
    ;; and compares it with the token that the parser expects to find.
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


    ;; The parse-term procedure parses a term from a given token source.

    (define parse-term
      (lambda (token-source)
        (let ((current (acquire-token token-source)))
          (if (and (identifier-token? current)
                   (not (char-upper-case?
                         (string-ref (identifier-token-name current) 0))))
              (if (token-source 'at-end?)
                  (make-simple-term (identifier-token-name current))
                  (let ((next (token-source 'peek)))
                    (if (paleft-token? next)
                        (make-function-term
                         (identifier-token-name current)
                         (parse-arguments token-source))
                        (make-simple-term
                         (identifier-token-name current)))))
              (error 'parse-term
                     (string-append "The token "
                                    (token->string current)
                                    " cannot begin a term.")
                     token-source)))))

    ;; The parse-arguments procedure
    ;; parses a list of terms from a given token source,
    ;; consuming the enclosing parentheses and the separating commas as well.

    (define parse-arguments
      (lambda (token-source)
        (token-source 'get)   ; Discard the left parenthesis.
        (if (and (not (token-source 'at-end?))
                 (paright-token? (token-source 'peek)))
            (begin
              (token-source 'get)  ; Discard the right parenthesis.
              '())
            (let loop ((reversed-list (list (parse-term token-source))))
              (let ((next (acquire-token token-source)))
                (cond ((paright-token? next)
                       (reverse reversed-list))
                      ((comma-token? next)
                       (loop (cons (parse-term token-source) reversed-list)))
                      (else
                       (error 'parse-arguments
                              (string-append "The token "
                                             (token->string next)
                                             " cannot follow a term "
                                             "in an argument list.")
                              token-source))))))))                            

    ;; The statement? predicate determines
    ;; whether its argument belongs to any of the statement types.

    (define statement?
      (lambda (something)
        (or (literal-statement? something)
            (atomic-statement? something)
            (negation-statement? something)
            (conjunction-statement? something)
            (disjunction-statement? something)
            (implication-statement? something)
            (equivalence-statement? something)
            (universal-statement? something)
            (existential-statement? something))))

    ;; The statement->string procedure
    ;; constructs and returns a human-readable string representation
    ;; of a given statement.

    (define statement->string
      (lambda (stmt)
        (cond ((literal-statement? stmt)
               (if (literal-statement-polarity stmt)
                   (string verum)
                   (string falsum)))
              ((atomic-statement? stmt)
               (string-append (atomic-statement-predicate stmt)
                              (arguments->string
                                (atomic-statement-arguments stmt))))
              ((negation-statement? stmt)
               (string-append (string neg)
                              (statement->string
                                (negation-statement-negand stmt))))
              ((conjunction-statement? stmt)
               (string-append "("
                              (statement->string
                               (conjunction-statement-left stmt))
                              (string #\space caret #\space)
                              (statement->string
                               (conjunction-statement-right stmt))
                              ")"))
              ((disjunction-statement? stmt)
               (string-append "("
                              (statement->string
                               (disjunction-statement-left stmt))
                              (string #\space wedge #\space)
                              (statement->string
                               (disjunction-statement-right stmt))
                              ")"))
              ((implication-statement? stmt)
               (string-append "("
                              (statement->string
                               (implication-statement-protasis stmt))
                              (string #\space impl #\space)
                              (statement->string
                               (implication-statement-apodosis stmt))
                              ")"))
              ((equivalence-statement? stmt)
               (string-append "("
                              (statement->string
                               (equivalence-statement-left stmt))
                              (string #\space iff #\space)
                              (statement->string
                               (equivalence-statement-right stmt))
                              ")"))
              ((universal-statement? stmt)
               (string-append (string uquant)
                              (universal-statement-variable stmt)
                              " "
                              (statement->string
                               (universal-statement-body stmt))))
              ((existential-statement? stmt)
               (string-append (string equant)
                              (existential-statement-variable stmt)
                              " "
                              (statement->string
                               (existential-statement-body stmt)))))))

    ;; The parse-statement procedure parses an statement
    ;; from a given token source.

    (define parse-statement
      (lambda (token-source)

        ;; Get a token and determine which of the analyses of statements
        ;; should be used.

        (let ((current (acquire-token token-source)))
          (cond ((verum-token? current) (make-literal-statement #t))
                ((falsum-token? current) (make-literal-statement #f))
                ((and (identifier-token? current)
                      (char-upper-case?
                       (string-ref (identifier-token-name current) 0)))
                 (make-atomic-statement (identifier-token-name current)
                                        (parse-arguments token-source)))
                ((paleft-token? current)
                 (let* ((left (parse-statement token-source))
                        (binop (acquire-token token-source))
                        (right (parse-statement token-source)))
                   (match-and-discard token-source (make-paright-token))
                   (cond ((and-token? binop)
                          (make-conjunction-statement left right))
                         ((or-token? binop)
                          (make-disjunction-statement left right))
                         ((impl-token? binop)
                          (make-implication-statement left right))
                         ((iff-token? binop)
                          (make-equivalence-statement left right))
                         (else
                          (error 'parse-statement
                                 (string-append "The token "
                                                (token->string binop)
                                                " is not a binary operator.")
                                 token-source)))))
                ((not-token? current)
                 (make-negation-statement (parse-statement token-source)))
                ((forall-token? current)
                 (let ((variable (acquire-token token-source)))
                   (if (and (identifier-token? variable)
                            (not (char-upper-case?
                                  (string-ref (identifier-token-name
                                               variable) 0))))
                       (make-universal-statement
                        (identifier-token-name variable)
                        (parse-statement token-source))
                       (error 'parse-statement
                              (string-append "The token "
                                             (token->string variable)
                                             " is not a variable of "
                                             "quantification.")
                              token-source))))
                ((exists-token? current)
                 (let ((variable (acquire-token token-source)))
                   (if (and (identifier-token? variable)
                            (not (char-upper-case?
                                  (string-ref (identifier-token-name
                                               variable) 0))))
                       (make-existential-statement
                        (identifier-token-name variable)
                        (parse-statement token-source))
                       (error 'parse-statement
                              (string-append "The token "
                                             (token->string variable)
                                             " is not a variable of "
                                             "quantification.")
                              token-source))))
                (else
                 (error 'parse-statement
                        (string-append "The token "
                                       (token->string current)
                                       " cannot begin a statement.")
                        token-source))))))

    ;; The parse procedure takes a string representing a statement as its argument
    ;; and returns a syntax tree for that statement.

    (define parse
      (lambda (str)
        (let* ((token-source (scanner str))
               (syntax-tree (parse-statement token-source)))
          (if (token-source 'at-end?)
              syntax-tree
              (error 'parse
                     (string-append "There were extra, unusable tokens at "
                                    "the end of the statement."))))))))

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
