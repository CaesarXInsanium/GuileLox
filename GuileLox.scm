#!/usr/bin/env -S guile -e main -s
!#

;; Usage String and Text Based Procedures with Ports
(use-modules (ice-9 textual-ports))
;; allows for defining records
(use-modules (srfi srfi-9))
;; allows for defining custome printer
(use-modules (srfi srfi-9 gnu))

(define lox-error? #f)

(define (lox-error line message)
  (report line "" message))

(define (report line where message)
  (let ((stderr (current-error-port)))
    (begin (display "[line " stderr)
           (display line stderr)
           (display "] Error" stderr)
           (display where stderr)
           (display ": " stderr)
           (display message stderr)
           (set! lox-error? #t)
           (if lox-error?
               -1
               0))))
;; Represents STDIN when promptin user for input, this is a Guile Port
(define STDIN (current-input-port))
(define STDOUT (current-output-port))

(define-record-type token
  (make-token type lexeme object line)
  token?
  (type token-type)
  (lexeme token-lexeme)
  (object token-object)
  (line token-line))

(set-record-type-printer! token (lambda (record port)
                                  (display "Token->")
                                  (display "Type: ")
                                  (display (token-type record))
                                  (display " Lexeme: ")
                                  (display (token-lexeme record))
                                  (display " Object: ")
                                  (display (token-object record))
                                  (display " LineNum: ")
                                  (display (token-line record))
                                  (newline)))

(define token-types #("LEFT_PAREN"
                      "RIGHT_PAREN"
                      "LEFT_BRACE"
                      "RIGHT_BRACE"
                      "COMMA"
                      "DOT"
                      "MINUS"
                      "PLUS"
                      "SEMICOLON"
                      "SLASH"
                      "STAR"

                      ;; Two Char
                      "BANG"
                      "BANG_EQUAL"
                      "EQUAL"
                      "EQUAL_EQUAL"
                      "GREATER"
                      "GREATER_EQUAL"
                      "LESS"
                      "LESS_EQUAL"

                      ;; Literals
                      "IDENTIFIER"
                      "STRING"
                      "NUMBER"

                      ;; KEYWORDS
                      "AND"
                      "CLASS"
                      "ELSE"
                      "FALSE"
                      "FUN"
                      "FOR"
                      "IF"
                      "NIL"
                      "OR"
                      "PRINT"
                      "RETURN"
                      "SUPER"
                      "THIS"
                      "TRUE"
                      "VAR"
                      "WHILE"
                      "EOF"))

(define START 0)
(define CURRENT 0)
(define LINE 0)
(define SOURCELEN 0)

(define (alpha? char)
  (or (or (and (char>=? char #\a) 
               (char<=? char #\z)) 
          (and (char>=? char #\A)
               (char<=? char #\Z)))
      (char=? char #\_)))

(define (digit? char)
  (and (char>=? char #\0) (char<=? char #\0)))

(define (alpha-numberic? char)
  (or (alpha? char) (digit? char)))

(define (at-end?)
  (>= CURRENT SOURCELEN))

;; Returns token type based on single char returns empty string otherwise
(define (char-tokentype char)
  (cond ((char=? char #\() (vector-ref token-types 0))
        ((char=? char #\)) (vector-ref token-types 1))
        ((char=? char #\{) (vector-ref token-types 2))
        ((char=? char #\}) (vector-ref token-types 3))
        ((char=? char #\,) (vector-ref token-types 4))
        ((char=? char #\.) (vector-ref token-types 5))
        ((char=? char #\-) (vector-ref token-types 6))
        ((char=? char #\+) (vector-ref token-types 7))
        ((char=? char #\;) (vector-ref token-types 8))
        ((char=? char #\\) (vector-ref token-types 9))
        ((char=? char #\*) (vector-ref token-types 10))
        (else " ")))

;; Returns token-type object
(define (scan-token chr)
  (make-token (char-tokentype chr) chr #f LINE))


;; take in source string and return list of token records
(define (check-types char)
  (display "Is char?: ")
  (display (char? char))
  (newline))

(define (scan-tokens source)
  (begin (set! SOURCELEN (string-length source))
         (map (lambda (char) (begin (set! START CURRENT)
                                    (scan-token char)))
              (string->list source))))
(define (run source)
  (if (string? source)
      (display "Is a string!\n")
      (display "Is Not A String!\n"))
  (display "Running ")
  (display source)
  (newline)
  (display (scan-tokens source))
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
