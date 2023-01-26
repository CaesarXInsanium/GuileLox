;; Scanner API

(define (print-char str)
  (let ((ls (string->list str)))
    (map (lambda (x) (begin (display x)
                            (display ", "))) ls)))
