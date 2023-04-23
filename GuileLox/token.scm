(define-module (GuileLox token))
;; allows for defining records

(use-modules (srfi srfi-9))
;; allows for defining custom printer
(use-modules (srfi srfi-9 gnu))
(use-modules (ice-9 format))

(use-modules (GuileLox token-type))

(define-record-type token
  (make-token type lexeme object line)
  token?
  (type token-type)
  (lexeme token-lexeme)
  (object token-object)
  (line token-line))


(set-record-type-printer! token (lambda (record port)
                                  (display (format #f "Token { type: ~s,\tlexeme: ~s,\tobject: ~:a,\tline: ~d }~%"
                                                   (tokentype->string (token-type record))
                                                   (token-lexeme record)
                                                   (token-object record)
                                                   (token-line record))
                                           port)))

(define-public nil '())

(export make-token
        token?
        token-type
        token-lexeme
        token-object
        token-line)
