(define-module (GuileLox scanner))

(use-modules (GuileLox error))
(use-modules (GuileLox token))
(use-modules (GuileLox token-type))
(use-modules (GuileLox char))


;; Scanner API

(define SOURCE "")
(define START 0)
(define CURRENT 0)
(define LINE 0)
(define SOURCELEN 0)
(define TOKENS '())

(define (inc x) (+ 1 x))
(define (at-end?)
  (>= CURRENT (- SOURCELEN 1)))

(define (string-loop)
  (if (not (char=? (string-ref SOURCE CURRENT) #\"))
      (begin (set! CURRENT (inc CURRENT)))))

(define (process-string)
  (display "Processing String"))
(define (process-number)
  (display "Processing Number"))
(define (process-identifier)
  (display "Processing Identifier"))

(define-public (char-token-type char)
  (cond ((char=? char #\() TOKEN-LEFT-PAREN)
        ((char=? char #\)) TOKEN-RIGHT-PAREN)
        ((char=? char #\{) TOKEN-LEFT-BRACE)
        ((char=? char #\}) TOKEN-RIGHT-BRACE)
        ((char=? char #\,) TOKEN-COMMA)
        ((char=? char #\.) TOKEN-DOT)
        ((char=? char #\-) TOKEN-MINUS)
        ((char=? char #\+) TOKEN-PLUS)
        ((char=? char #\;) TOKEN-SEMICOLON)
        ((char=? char #\/) TOKEN-SLASH)
        ((char=? char #\*) TOKEN-STAR)
        ((char=? char #\") (process-string))
        ((digit? char) (process-number))
        ((alpha-numberic? char) (process-identifier))
        (else TOKEN-ERROR))) 

;; Returns token-type object
(define (scan-token chr)
  (make-token (char-token-type chr) chr #f LINE))


;; take in source string and return list of token records

(define (add-token! ttype obj)
  (let ((text (substring SOURCE START CURRENT)))
    (append! TOKENS (make-token ttype text obj LINE))))

(define (edd-token! ttype) (add-token! ttype '()))

(define (advance) (begin (set! CURRENT (inc CURRENT))
                         (string-ref SOURCE CURRENT)))

(define (scan-token) (let ((c (advance)))
                       (cond ((char=? c #\() (edd-token! TOKEN-LEFT-PAREN))
                             ((char=? c #\)) (edd-token! TOKEN-RIGHT-PAREN))
                             ((char=? c #\{) (edd-token! TOKEN-LEFT-BRACE))
                             ((char=? c #\}) (edd-token! TOKEN-RIGHT-BRACE))
                             ((char=? c #\,) (edd-token! TOKEN-COMMA))
                             ((char=? c #\.) (edd-token! TOKEN-DOT))
                             ((char=? c #\-) (edd-token! TOKEN-MINUS))
                             ((char=? c #\+) (edd-token! TOKEN-PLUS))
                             ((char=? c #\;) (edd-token! TOKEN-SEMICOLON))
                             ((char=? c #\*) (edd-token! TOKEN-STAR))
                             (else (edd-token! TOKEN-ERROR)))))


(define-public (scanner source)
  (begin (set! SOURCE source)
         (set! SOURCELEN (string-length SOURCE))
         (set! LINE 1)
         (define (loop) (if (at-end?) TOKENS (begin (set! START CURRENT)
                                                    (scan-token)
                                                    (append! TOKENS (make-token TOKEN-EOF "" '() LINE))
                                                    (loop))))
         (loop)
         TOKENS))
