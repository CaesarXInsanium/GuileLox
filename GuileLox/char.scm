(define-module (GuileLox char))
;; related functions to char

(define-public (alpha? char)
  (or (or (and (char>=? char #\a) 
               (char<=? char #\z)) 
          (and (char>=? char #\A)
               (char<=? char #\Z)))
      (char=? char #\_)))

(define-public (digit? char)
  (and (char>=? char #\0) (char<=? char #\9)))

(define-public (alpha-numeric? char)
  (or (alpha? char) (digit? char)))

(define-public (alpha-symbol? char)
  (or (char=? char #\-) (char=? char #\_)))



(define-public (quote-mark? char) (or (char=? #\" char) (char=? #\' char)))

(define-public (whitespace? char)
               (or (char=? #\n)
                   (char=? #\t)))
                               
