(define-module (GuileLox scan))
(use-modules (GuileLox char))
(use-modules (GuileLox token))
(use-modules (GuileLox token-type))


(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define LINE 1)

(define (scan-string source)
  (if (char=? #\" (string-ref source 0))
      #\"
      (string)))


;; here we are dealing with the assumption that the first item in is the char we are focusing on
(define (scan source start line len)
  (let ((char (string-ref source start)))
    (cond ((char-single-token? char) (cons (make-token (char-single-token char) char #f line) 
                                           (scan (substring source (inc start)) (dec len))))
          ((quote-mark? char) (cons (make-token TOKEN-STRING char #f line) (scan (substring source (inc start)) line len)))
          (else (cons (make-token TOKEN-ERROR (string-ref source start) #f line) '())))))

(define-public (scan-tokens source)
  (let ((len (string-length source)))
    (scan source 0 1 len)))
