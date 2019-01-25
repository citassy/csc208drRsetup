#!r7rs

(define-library (discrete greeter)
  (export greeting)
  (import (scheme base)
          (scheme write))
  (begin
    (define greeting
      (lambda ()
        (display "Hello, world!")
        (newline)))))