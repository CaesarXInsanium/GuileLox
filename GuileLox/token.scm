(define-module (GuileLox token))
;; allows for defining records

(use-modules (srfi srfi-9))
;; allows for defining custome printer
(use-modules (srfi srfi-9 gnu))

(define-record-type token
  (make-token type lexeme object line)
  token?
  (type token-type)
  (lexeme token-lexeme)
  (object token-object)
  (line token-line))

(set-record-type-printer! token (lambda (record port)
                                  (display "Token\t")
                                  (display "Type: ")
                                  (display (token-type record))
                                  (display " Lexeme: ")
                                  (display (token-lexeme record))
                                  (display " Object: ")
                                  (display (token-object record))
                                  (display " LineNum: ")
                                  (display (token-line record))
                                  (newline)))

(define-public nil '())

(export make-token
        token?
        token-type
        token-lexeme
        token-object
        token-line)
