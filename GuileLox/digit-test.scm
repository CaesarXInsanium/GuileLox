(use-modules (srfi srfi-64)
             (GuileLox digit))

(define str "   asdsa54 76nhg 45 47hdff")
(define result (digit-extract str))

(display "Does Digit Extraction Work?\n")
(display (string=? "45" (car result)))
