
(define lox-error? #f)
(define (lox-error line message)
  (report line "" message))
(define (report line where message)
  (let ((err (current-error-port)))
    (begin (display "[line " err)
           (display line err)
           (display "] Error" err)
           (display where err)
           (display ": " err)
           (display message err)
           (define lox-error? #t)
           (if lox-error?
               -1
               0))))
