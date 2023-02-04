(define-module (GuileLox char))
;; related functions to char

(define-public (alpha? char)
  (or (or (and (char>=? char #\a) 
               (char<=? char #\z)) 
          (and (char>=? char #\A)
               (char<=? char #\Z)))
      (char=? char #\_)))

(define-public (digit? char)
  (and (char>=? char #\0) (char<=? char #\0)))

(define-public (alpha-numberic? char)
  (or (alpha? char) (digit? char)))
