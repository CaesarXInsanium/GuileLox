;; This bullshit has to match diretory name and filename
(define-module (GuileLox error))

(define-public lox-error? #f)

(define-public (lox-error line message)
  (report line "" message))

(define-public (report line where message)
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
