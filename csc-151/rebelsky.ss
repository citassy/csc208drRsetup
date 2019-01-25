#!r7rs

;;; My colleague Samuel Rebelsky has written several Racket packages
;;; for the "Functional Problem Solving" course, CSC 151,
;;; at Grinnell College.
;;; I have ported most of his code to R7RS (small) Scheme
;;; and collected it in this library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; reseda@grinnell.edu

;;; Samuel A. Rebelsky
;;; Department of Computer Science
;;; Grinnell College
;;; rebelsky@grinnell.edu

(define-library (csc-151 rebelsky)

  (export index-of iota reduce reduce-left reduce-right tally-all
          take-random all any comparator left-section l-s
          right-section r-s decrement double increment
          file->chars file->lines file->words read-word read-until
          skip-char char-in-string?)

  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme char)
          (csc-151 random))

  (begin

    ; +-------------------------------+----------------------------------
    ; | Private procedures and values |
    ; +-------------------------------+

    ;;; Constant:
    ;;;   FIRST-FIRST-ODDS
    ;;; Type:
    ;;;   real
    ;;; Summary:
    ;;;   The odds that we process the first element first when mapping
    ;;;   through a list.
    (define FIRST-FIRST-ODDS 0.5)

    ; +---------------------+--------------------------------------------
    ; | Exported procedures |
    ; +---------------------+

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   index-of
    ;;; Parameters:
    ;;;   val, a Scheme val
    ;;;   lst, a list
    ;;; Purpose:
    ;;;   Find the index of val in lst
    ;;; Produces:
    ;;;   index, an integer
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   if index is -1, val does not appear in lst.
    ;;;   Otherwise,
    ;;;     (list-ref lst index) is val
    ;;;     For no i smaller than index is (list-ref lst i) val.
    (define index-of
      (lambda (val lst)
        (let kernel ((pos 0)
                     (remaining lst))
          (cond
            ((null? remaining)
             -1)
            ((equal? val (car remaining))
             pos)
            (else
             (kernel (+ pos 1) (cdr remaining)))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   iota
    ;;; Parameters:
    ;;;   n, a positive integer
    ;;; Purpose:
    ;;;   Create a list of all non-negative integers less than n.
    ;;; Produces:
    ;;;   ints, a list of integers
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   (length ints) = n
    ;;;   ints has the form (0 1 2 3 ...)
    (define iota
      (lambda (n)
        (let kernel ((i 0))
          (if (= i n)
              '()
              (cons i (kernel (+ i 1)))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   reduce
    ;;; Parameters:
    ;;;   op, a binary procedure
    ;;;   lst, a list of values of the form (val1 val2 ... valn)
    ;;; Purpose:
    ;;;   Creates a new value by repeatedly applying op to the values
    ;;;   in lst.
    ;;; Produces:
    ;;;   result, a value
    ;;; Preconditions:
    ;;;   op must be applicable to pairs of elements of lst.
    ;;;   op must return the same types that it takes as input
    ;;; Postconditions:
    ;;;   In infix notation, result is val1 op val2 op val3 ... op valn
    ;;;   the order of the evalution of the operations is undefined
    (define reduce
      (lambda (fun lst)
        (cond
          ((null? (cdr lst))
           (car lst))
          ((< (random) FIRST-FIRST-ODDS)
           (reduce fun (cons (fun (car lst) (cadr lst))
                             (cddr lst))))
          (else
           (fun (car lst) (reduce fun (cdr lst)))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   reduce-left
    ;;; Parameters:
    ;;;   op, a binary procedure
    ;;;   lst, a list of values of the form (val1 val2 ... valn)
    ;;; Purpose:
    ;;;   Creates a new value by repeatedly applying op to the values
    ;;;   in lst.
    ;;; Produces:
    ;;;   result, a value
    ;;; Preconditions:
    ;;;   op must be applicable to pairs of elements of lst.
    ;;;   op must return the same types that it takes as input
    ;;; Postconditions:
    ;;;   result is (op (op (op val1 val2) val3) ... valn)
    (define reduce-left
      (lambda (fun lst)
        (cond
          ((null? (cdr lst))
           (car lst))
          (else
           (reduce-left fun (cons (fun (car lst) (cadr lst))
                                  (cddr lst)))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   reduce-right
    ;;; Parameters:
    ;;;   op, a binary procedure
    ;;;   lst, a list of values of the form (val1 val2 ... valn)
    ;;; Purpose:
    ;;;   Creates a new value by repeatedly applying op to the values
    ;;;   in lst.
    ;;; Produces:
    ;;;   result, a value
    ;;; Preconditions:
    ;;;   op must be applicable to pairs of elements of lst.
    ;;;   op must return the same types that it takes as input
    ;;; Postconditions:
    ;;;   result is (op val1 (op val2 (op val3 ... (op valn-1 valn))))
    (define reduce-right
      (lambda (fun lst)
        (cond
          ((null? (cdr lst))
           (car lst))
          (else
           (fun (car lst) (reduce-right fun (cdr lst)))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   tally-all
    ;;; Parameters:
    ;;;   lst, a list of values
    ;;; Purpose:
    ;;;   Tallies all of the values in lst
    ;;; Produces:
    ;;;   tallies, a list of (key count) lists.
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   * If val appears k times in lst, then (val k) appears in tallies
    ;;;   * If (val k) appears in tallies, then val appears k times in lst
    ;;;   * Each value in lst is the car of exactly one list in tallies.
    (define tally-all
      (let ((update-tallies
             (lambda (val tallies)
               (let kernel ((remaining tallies))
                 (if (null? remaining)
                     (cons (vector val 1) tallies)
                     (let ((current-tally (car remaining)))
                       (cond
                         ((equal? val (vector-ref current-tally 0))
                          (vector-set! current-tally 1 
                                       (+ (vector-ref current-tally 1) 1))
                          tallies)
                         (else
                           (kernel (cdr remaining))))))))))
        (lambda (lst)
          (let kernel ((rest lst)
                       (tallies '()))
            (if (null? rest)
                (map vector->list (reverse tallies))
                (kernel (cdr rest)
                        (update-tallies (car rest) tallies)))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   take-random
    ;;; Parameters:
    ;;;   lst, a list
    ;;;   n, a non-negative integer
    ;;; Purpose:
    ;;;   Grab n "randomly selected elements" from lst
    ;;; Produces:
    ;;;   elements, a list
    ;;; Preconditions:
    ;;;   n <= (length lst)
    ;;; Postconditions:
    ;;;   (length elements) = n
    ;;;   Every element in elements appears in lst.
    ;;;   Every element in elements represents a separate element of lst.
    (define take-random
      (lambda (lst n)
        (let kernel ((remaining lst)
                     (len (length lst))
                     (n n))
          (cond
            ((or (= n 0) (= len 0))
             '())
            ((<= (random) (/ n len))
             (cons (car remaining)
                   (kernel (cdr remaining)
                           (- len 1)
                           (- n 1))))
            (else        
             (kernel (cdr remaining)
                     (- len 1)         
                     n))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   all
    ;;; Parameters:
    ;;;   pred?, a unary predicate
    ;;;   lst, a list
    ;;; Purpose:
    ;;;   Determine if pred? holds for all the values in lst.
    ;;; Produces:
    ;;;   ok?, a Boolean
    ;;; Preconditions:
    ;;;   [Standard]
    ;;; Postconditions:
    ;;;   If there is an i such that (pred? (list-ref lst i))
    ;;;     fails to hold, then ok? is false.
    ;;;   Otherwise, ok? is true.
    (define all
      (lambda (pred? lst)
        (or (null? lst)
            (and (pred? (car lst))
                 (all pred? (cdr lst))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   any
    ;;; Parameters:
    ;;;   pred?, a unary predicate
    ;;;   lst, a list
    ;;; Purpose:
    ;;;   Determines if pred? holds for any of the values in lst
    ;;; Produces:
    ;;;   ok?, a Boolean
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   If there is an i s.t. (pred? (list-ref lst i)) holds, then
    ;;;     ok? is true.
    ;;;   If for all i, (pred? (list-ref list i)) does not hold, then
    ;;;     ok? is false.
    (define any
      (lambda (pred? lst)
        (and (not (null? lst))
             (or (pred? (car lst))
                 (any pred? (cdr lst))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   comparator
    ;;; Parameters:
    ;;;   compare?, a binary comparator
    ;;;   extract, a unary procedure
    ;;; Purpose:
    ;;;   Creates a comparator that takes two values, applies extract
    ;;;   to each, and then compares the results of both.
    ;;; Produces:
    ;;;   comp?, a binary comparator
    ;;; Preconditions:
    ;;;   compare? can be applied to the results of extract.
    ;;; Postconditions:
    ;;;   (comp? v1 v2) = (compare? (extract v1) (extract v2))
    (define comparator
      (lambda (compare? extract)
        (lambda (v1 v2)
          (compare? (extract v1) (extract v2)))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   left-section
    ;;; Parameters:
    ;;;   proc2, a two-parameter procedure
    ;;;   left, a value
    ;;; Purpose:
    ;;;   Creates a one-parameter procedure by filling in the first parameter
    ;;;    of proc2.
    ;;; Produces:
    ;;;   proc1, a one-parameter procedure
    ;;; Preconditions:
    ;;;   left is a valid first parameter for proc2.
    ;;; Postconditions:
    ;;;   (proc1 right) = (proc2 left right)
    (define left-section
      (lambda (proc2 left)
        (lambda (right) (proc2 left right))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   l-s
    ;;; Parameters:
    ;;;   proc2, a two-parameter procedure
    ;;;   left, a value
    ;;; Purpose:
    ;;;   Creates a one-parameter procedure by filling in the first parameter
    ;;;    of proc2.
    ;;; Produces:
    ;;;   proc1, a one-parameter procedure
    ;;; Preconditions:
    ;;;   left is a valid first parameter for proc2.
    ;;; Postconditions:
    ;;;   (proc1 right) = (proc2 left right)
    (define l-s left-section)

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   right-section
    ;;; Parameters:
    ;;;   proc2, a two-parameter procedure
    ;;;   right, a value
    ;;; Purpose:
    ;;;   Creates a one-parameter procedure by filling in the second parameter
    ;;;    of proc2.
    ;;; Produces:
    ;;;   proc1, a one-parameter procedure
    ;;; Preconditions:
    ;;;   left is a valid first parameter for proc2.
    ;;; Postconditions:
    ;;;   (proc1 left) = (proc2 left right)
    (define right-section
      (lambda (proc2 right)
        (lambda (left) (proc2 left right))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   r-s
    ;;; Parameters:
    ;;;   proc2, a two-parameter procedure
    ;;;   right, a value
    ;;; Purpose:
    ;;;   Creates a one-parameter procedure by filling in the second parameter
    ;;;    of proc2.
    ;;; Produces:
    ;;;   proc1, a one-parameter procedure
    ;;; Preconditions:
    ;;;   left is a valid first parameter for proc2.
    ;;; Postconditions:
    ;;;   (proc1 left) = (proc2 left right)
    (define r-s right-section)

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   decrement
    ;;; Parameters:
    ;;;   val, a number
    ;;; Purpose:
    ;;;   Subtract 1 from val
    ;;; Produces:
    ;;;   decrement, a number
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   decremented = (- val 1)
    ;;; Ponderings:
    ;;;   An obvious procedure, but one that is often useful.
    (define decrement
      (lambda (val)
        (- val 1)))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   increment
    ;;; Parameters:
    ;;;   val, a number
    ;;; Purpose:
    ;;;   Add 1 to val
    ;;; Produces:
    ;;;   incremented, a number
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   incremented = (+ val 1)
    ;;; Ponderings:
    ;;;   An obvious procedure, but one that is often useful.
    (define increment 
      (lambda (val)
        (+ val 1)))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   double
    ;;; Parameters:
    ;;;   val, a number
    ;;; Purpose:
    ;;;   Multiply val by 2
    ;;; Produces:
    ;;;   doubled, a number
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   doubled = (* val 2)
    ;;; Ponderings:
    ;;;   An obvious procedure, but one that is often useful.
    (define double
      (lambda (val)
        (* val 2)))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   file->chars
    ;;; Parameters:
    ;;;   fname, a string
    ;;; Purpose:
    ;;;   Reads and returns all of the characters in the file
    ;;; Produces:
    ;;;   chars, a list of characters
    ;;; Preconditions:
    ;;;   fname names a valid file
    ;;; Postconditions:
    ;;;   chars contains all of the characters in the file.
    (define file->chars
      (lambda (fname)
        (file->stuff fname read-char eof-object?)))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   file->lines
    ;;; Parameters:
    ;;;   fname, a string
    ;;; Purpose:
    ;;;   Reads and returns all of the lines in the file
    ;;; Produces:
    ;;;   lines, a list of strings
    ;;; Preconditions:
    ;;;   fname names a valid file
    ;;; Postconditions:
    ;;;   lines contains all of the lines in the file, in the order they appear,
    ;;;   but without newlines.
    (define file->lines
      (lambda (fname)
        (file->stuff fname read-line eof-object?)))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   file->words
    ;;; Parameters:
    ;;;   fname, a string
    ;;; Purpose:
    ;;;   Reads and returns all of the words in the file
    ;;; Produces:
    ;;;   words, a list of strings
    ;;; Preconditions:
    ;;;   fname names a valid file
    ;;; Postconditions:
    ;;;   words contains all of the words in the file, in the order they appear,
    ;;;   but without punctuation.
    (define file->words
      (lambda (fname)
        (file->stuff fname read-word (lambda (stuff) (equal? stuff "")))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   read-word
    ;;; Parameters:
    ;;;   port, an input port
    ;;; Purpose:
    ;;;   Reads the next word from the port
    ;;; Produces:
    ;;;   word, a string
    ;;; Preconditions:
    ;;;   port is open for reading
    ;;; Postconditions:
    ;;;   word is the next word in the port.
    (define read-word
      (lambda (port)
        (let kernel ((chars '()))
          (let ((ch (read-char port)))
            (cond
              ((eof-object? ch)
               (list->string (reverse chars)))
              ((word-char? ch)
               (kernel (cons ch chars)))
              ((null? chars)
               (kernel '()))
              (else
               (list->string (reverse chars))))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   read-until
    ;;; Parameters:
    ;;;   port, an input port
    ;;;   terminator, a character, string, or predicate
    ;;; Purpose:
    ;;;   Read the characters until you reach terminator (or eof) (or
    ;;;   the predicate holds..
    ;;; Produces:
    ;;;   str, a string
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   * str represents the sequence of characters from the file pointer
    ;;;     of port at the beginning until (a) the first occurence of terminator,
    ;;;     if terminator is a character, (b) a character that appears in terminator,
    ;;;     it terminator is a string, or (c) a character for which terminator
    ;;;     holds, if terminator is a predicate.  str does not include that
    ;;;     character.
    ;;;   * the file pointer has been advanced appropriately.
    (define read-until
      (lambda (port terminator)
        (let ((pred? (cond
                       ((char? terminator)
                         (lambda (ch) (char=? ch terminator)))
                       ((string? terminator)
                        (lambda (ch) (char-in-string? terminator ch)))
                       ((procedure? terminator)
                        terminator)
                       (else
                        (error "Invalid parameter"
                               terminator
                               "expected a character, string, or unary predicate")))))
          (let kernel ((chars '()))
            (let ((ch (peek-char port)))
              (if (or (eof-object? ch) (pred? ch))
                  (list->string (reverse chars))
                  (kernel (cons (read-char port) chars))))))))

    ;;; Package:
    ;;;   (csc-151 rebelsky)
    ;;; Procedure:
    ;;;   skip-char
    ;;; Parameters:
    ;;;   port, an open input port
    ;;;   ch, a character
    ;;; Purpose:
    ;;;   Skip over the next character in the input file if it is ch.
    ;;; Produces:
    ;;;   skipped?, a Boolean
    ;;; Preconditions:
    ;;;   [No additional]
    ;;; Postconditions:
    ;;;   * If (peek-char port) is ch, reads over the character and returns #t
    ;;;   * Otherwise, leaves the port unchanged and returns #f
    (define skip-char
      (lambda (port ch)
        (let ((next (peek-char port)))
          (cond
            ((and (not (eof-object? next))
                  (char=? next ch))
             (read-char port)
             #t)
            (else
             #f)))))

    ;;; Procedure:
    ;;;   char-in-string?
    ;;; Parameters:
    ;;;   str, a string
    ;;;   sought, a character
    ;;; Purpose:
    ;;;   Determine if sought occurs within str.
    ;;; Produces:
    ;;;   present?, a Boolean
    ;;; Ponderings:
    ;;;   The string-contains? procedure
    ;;;   that Professor Rebelsky actually used
    ;;;   in his definition of read-until
    ;;;   isn't in R7RS,
    ;;;   so I defined the special case he needed
    ;;;   as an extra in this library.
    ;;;                          -- JDS
    (define char-in-string?
      (lambda (str sought)
        (let loop ((remaining (string-length str)))
          (and (not (zero? remaining))
               (let ((next (- remaining 1)))
                 (or (char=? sought (string-ref str next))
                     (loop next)))))))))

    ; +------------------+-----------------------------------------------
    ; | Local Procedures |
    ; +------------------+

    ;;; Procedure:
    ;;;   file->stuff
    ;;; Parameters:
    ;;;   fname, a string that names a file
    ;;;   read-thing, a procedure that reads from an input port
    ;;;   end?, a predicate
    ;;; Purpose:
    ;;;   Repeatedly reads from the file named by fname until it
    ;;;   encounters a value for which end? holds.
    ;;; Produces:
    ;;;   stuff, a list of things.
    ;;; Philosophy:
    ;;;   read-chars, read-lines, and read-words all had
    ;;;   the same structure.  This procedure attempts to
    ;;;   unify those structures.
    (define file->stuff
      (lambda (fname read-thing end?)
        (let ((port (open-input-file fname)))
          (let kernel ()
            (let ((thing (read-thing port)))
              (cond
                ((end? thing)
                 (close-input-port port)
                 '())
                (else
                 (cons thing (kernel)))))))))

    ;;; Procedure:
    ;;;   word-char?
    ;;; Parameters:
    ;;;   ch, a character
    ;;; Purpose:
    ;;;   Determine if ch is "word character".
    ;;; Produces:
    ;;;   ok?, a Boolean
    (define word-char?
      (lambda (ch)
        (or (char-alphabetic? ch)
            (char-numeric? ch))))
  
;;; Copyright © 2018 Samuel Rebelsky, John David Stone

;;; This library is free software.
;;; You may redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation -- 
;;; either version 3 of the License,
;;; or (at your option) any later version.

;;; This library is distributed
;;; in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY --
;;; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.

;;; You should have received a copy
;;; of the GNU General Public License
;;; and the GNU Lesser General Public License
;;; along with this program.
;;; If not, they are available on the World Wide Web
;;; at https://www.gnu.org/licenses/gpl.html
;;; and https://www.gnu.org/licences/lgpl.html respectively.
