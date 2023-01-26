#!/bin/env -S guile -e main -s
!#

;; Usage String and Text Based Procedures with Ports
(use-modules (ice-9 textual-ports))
;; Scanner Class
(load "GuileLox/scanner.scm")

;; Error Handling
(load "GuileLox/error.scm")

;; Represents STDIN when promptin user for input, this is a Guile Port
(define STDIN (current-input-port))
(define STDOUT (current-output-port))

(define (run source)
  (if (string? source)
      (display "Is a string!\n")
      (display "Is Not A String!\n"))
  (display "Running ")
  (display source)
  (newline)
  (print-char source)
  (newline))

(define (run-file path)
  (let ((fileport (open-file path "r")))
    (if (and (not lox-error?) (port? fileport))
        (run (get-string-all fileport))
        (display "Error: Something is wrong in 'run-file'\n"))))

(define read-user (lambda (port) 
                    (begin (display "> ") 
                           (get-line port))))


(define (prompt stop?)
  (if stop?
      -1
      (let ((user-input (read-user STDIN)))
        (if (string=? "" user-input)
            (prompt #f)
            (begin (run user-input)
                   (set! lox-error? #f)
                   (prompt #f))))))



(define (run-prompt)
  (prompt #f))


(define (main args)
  (let ((num_args (length args)))
    (cond ((> num_args 2) (begin (display "Usage: guilelox [script]\n")))
          ((= num_args 2) (run-file (list-ref args 1)))
          (else (run-prompt)))))
