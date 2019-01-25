#!r7rs

;;; A scanner for the propositional calculus

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 27, 2013
;;; last revised October 9, 2018

;;; This library provides a scanning procedure
;;; that converts strings containing Boolean expressions
;;; (in the notation of the propositional calculus)
;;; into token streams.

(define-library (discrete propositional-scanner)
  (export make-verum-token verum-token? make-falsum-token falsum-token?
          make-variable-token variable-token? variable-token-name
          make-paleft-token paleft-token? make-paright-token paright-token?
          make-not-token not-token? make-and-token and-token? make-or-token
          or-token? make-impl-token impl-token? make-iff-token iff-token?
          token? token->string token=? scanner)
  (import (scheme base)
          (scheme char)
          (scheme write)
          (discrete logical-characters))

  (begin

     ;; First, let's define a record type
     ;; for each kind of token that we'll need:

    (define-record-type <verum-token>
      (make-verum-token)
      verum-token?)

    (define-record-type <falsum-token>
      (make-falsum-token)
      falsum-token?)

    (define-record-type <variable-token>
      (make-variable-token name)
      variable-token?
      (name variable-token-name))

    (define-record-type <paleft-token>
      (make-paleft-token)
      paleft-token?)

    (define-record-type <paright-token>
      (make-paright-token)
      paright-token?)

    (define-record-type <not-token>
      (make-not-token)
      not-token?)

    (define-record-type <and-token>
      (make-and-token)
      and-token?)

    (define-record-type <or-token>
      (make-or-token)
      or-token?)

    (define-record-type <impl-token>
      (make-impl-token)
      impl-token?)

    (define-record-type <iff-token>
      (make-iff-token)
      iff-token?)

    ;; The token? predicate determines
    ;; whether its argument belongs to one of the preceding types.

    (define token?
      (lambda (something)
        (or (verum-token? something)
            (falsum-token? something)
            (variable-token? something)
            (paleft-token? something)
            (paright-token? something)
            (not-token? something)
            (and-token? something)
            (or-token? something)
            (impl-token? something)
            (iff-token? something))))

    ;; The token->string procedure
    ;; constructs and returns a human-readable string representation
    ;; of a given token.

    (define token->string
      (lambda (token)
        (string (cond ((verum-token? token) verum)
                      ((falsum-token? token) falsum)
                      ((variable-token? token) (variable-token-name token))
                      ((paleft-token? token) #\()
                      ((paright-token? token) #\))
                      ((not-token? token) neg)
                      ((and-token? token) caret)
                      ((or-token? token) wedge)
                      ((impl-token? token) impl)
                      ((iff-token? token) iff)))))

    ;; The token=? predicate determines whether two tokens are essentially
    ;; the same.
    
    (define token=?
      (lambda (left right)
        (or (and (verum-token? left) (verum-token? right))
            (and (falsum-token? left) (falsum-token? right))
            (and (variable-token? left)
                 (variable-token? right)
                 (string=? (variable-token-name left)
                           (variable-token-name right)))
            (and (paleft-token? left) (paleft-token? right))
            (and (paright-token? left) (paright-token? right))
            (and (not-token? left) (not-token? right))
            (and (and-token? left) (and-token? right))
            (and (or-token? left) (or-token? right))
            (and (impl-token? left) (impl-token? right))
            (and (iff-token? left) (iff-token? right)))))

    ;; The scanner is implemented as a procedure
    ;; that takes a string as its argument
    ;; and returns a "token source" object.

    ;; A token source is a procedure
    ;; that accepts any of three symbols
    ;; as arguments -- at-end?, get, and peek:
    ;;
    ;; * If it receives the symbol at-end?,
    ;;   the token source returns #t 
    ;;   if no more tokens are available,
    ;;   #f if at least one more token can be extracted from the string.
    ;;
    ;; * If it receives the symbol peek,
    ;;   the token source looks ahead to determine
    ;;   what token can be extracted next from the string
    ;;   and returns it (without yet reading it).
    ;;
    ;; * If it receives the symbol get,
    ;;   the token source reads the next token from the string
    ;;   and returns it.

    (define scanner
      (lambda (str)
        
        ;; We'll access the string through a string port.

        (let ((source (open-input-string str))
              
              ;; When we first peek at a token,
              ;; we'll have to get all of the characters that make it up
              ;; from the character source.
              ;; Since we can only get those characters
              ;; out of the string port
              ;; once, however,
              ;; we'll have to store the token
              ;; so that we can still return it
              ;; when the next peek or get message is received.
              ;; The local variable buffer holds the next available token,
              ;; if we have peeked at it,
              ;; or the symbol empty if we have not.

              (buffer 'empty))

          (letrec

              ;; The skip-whitespace procedure
              ;; discards whitespace from the character source.
              ;; Its postcondition is
              ;; that either the character source
              ;; has no more available characters,
              ;; or the next available character
              ;; is a non-whitespace character.

              ((skip-whitespace
                (lambda ()
                  (let ((next (peek-char source)))
                    (unless (eof-object? next)
                      (when (char-whitespace? next)
                        (read-char source)
                        (skip-whitespace))))))

               ;; The get-token procedure
               ;; consumes the text of one token
               ;; from the character source
               ;; and constructs and returns the appropriate token.
               ;; It is a precondition of this procedure
               ;; that the next available character can begin a token.

               (get-token
                (lambda ()
                  (let ((next (read-char source)))
                    (cond ((char=? next verum) (make-verum-token))
                          ((char=? next falsum) (make-falsum-token))
                          ((char-upper-case? next)
                            (make-variable-token (string next)))
                          ((char=? next #\() (make-paleft-token))
                          ((char=? next #\)) (make-paright-token))
                          ((char=? next neg) (make-not-token))
                          ((char=? next caret) (make-and-token))
                          ((char=? next wedge) (make-or-token))
                          ((char=? next impl) (make-impl-token))
                          ((char=? next iff) (make-iff-token))
                          (else
                           (error 'scanner
                                  (string-append
                                   "A token may not begin with the "
                                   (string next)
                                   " character.")
                                  str)))))))

            ;; Now we're ready to construct the token source
            ;; that the scanner returns.

            (lambda (message)
              
              ;; Always discard whitespace
              ;; before processing the message.

              (skip-whitespace)

              ;; One is at the end of the token source
              ;; if the buffer is empty 
              ;; and no more characters are available.

              (cond ((eq? message 'at-end?)
                     (and (eq? buffer 'empty)
                          (eof-object? (peek-char source))))
                  
                    ;; The peek message
                    ;; causes the token source
                    ;; to return the token in the buffer;
                    ;; if the buffer starts out empty,
                    ;; we first call get-token to fill it.

                    ((eq? message 'peek?)
                     (when (eqv? buffer 'empty)
                       (set! buffer (get-token)))
                     buffer)

                    ;; If there is a token in the buffer,
                    ;; the get message causes the token source
                    ;; to extract and return it,
                    ;; leaving the buffer empty.
                    ;; If the buffer starts out empty,
                    ;; the token source simply calls get-token
                    ;; and returns the result.

                    ((eq? message 'get)
                     (if (eqv? buffer 'empty)
                         (get-token)
                         (let ((result buffer))
                           (set! buffer 'empty)
                           result)))

                    ;; Any other alleged message is an error.

                    (else (error 'scanner-source
                                 (string-append "The token-source message "
                                                (symbol->string message)
                                                " was not recognized.")
                                 str))))))))))

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
