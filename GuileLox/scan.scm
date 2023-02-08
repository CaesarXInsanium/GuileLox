(define-module (GuileLox scan))
(use-modules (GuileLox char))
(use-modules (GuileLox token))
(use-modules (GuileLox token-type))

(define (inc x) (+ 1 x))
(define (dec x) (- x 1))

(define (scanner source)
  (begin (define tokens (make-list 1 #f))
         (define line 0)
         (define start 0)
         (define current 0)
         (define (peek)
           (if (at-end?) #\nul (string-ref source current)))
         (define (peek-next) 
           (if (>= (inc current) (string-length source)
                   #\nul
                   (string-ref source (inc current)))))
         
         (define (identifier)
           (define (loop)
             (if (alpha-numberic? (peek))
                 (begin (advance)
                        (loop))))
           (loop)
           (let ((text (substring source start current)))
             (add-token! (keyword->token-type text) text #f line)))
         
         
         (define (add-token!! type obj) 
           (list-append tokens (make-token type (substring start current) obj line)))

         (define (add-tokenn! type)
           (add-token!! type '()))

         (define (at-end?) (>= current (string-length source)))

         (define (match expected)
           (if (at-end?)
               #f
               (if (not (char=? (string-ref source current) expected))
                   #f
                   (begin (set! current (inc current))
                          #t))))
         (define (advance)
           (begin (set! current (inc current))
                  (string-ref source (dec current))))
         (define (scan-token) (let ((c (advance)))))
         (define (scan-tokens)
           (define (loop) 
             (if (not (at-end?)) 
                 (begin (set! start current)
                        (scan-token)
                        (loop))))
           (loop)
           (list-append tokens (make-token TOKEN-EOF "" '() line))
           tokens)))
