(define-module (GuileLox scanner))

(use-modules (GuileLox error)
             (GuileLox token-type)
             (GuileLox token)
             (GuileLox char)
             (GuileLox alpha))

(use-modules (ice-9 textual-ports))

;; Recursive Scanner
(define-public (tokenize str)
  (string-split str (lambda (char) 
                      (or (char=? char #\space)
                          (char=? char #\newline)
                          (char=? char #\tab)))))

(define-public (scan-tokens port)
  (let ((char (lookahead-char port)))
    (if (eof-object? char)
        nil
        (cond ((alpha? char) (let ((word (list->string (scan-alpha port))))
                               (cons (make-token (keyword->token-type word) 
                                                 word nil 0)
                                     (scan-tokens port))))
              ((digit? char) (let ((numstr (list->string (scan-number port))))
                               (cons (make-token TOKEN_NUMBER numstr nil 0)
                                     (scan-tokens port))))
              ((char-single-token? char) (let ((char (get-char port))) 
                                           (cons (make-token (char-single-token char) (list->string (list char)) nil 0)
                                                 (scan-tokens port))))
              ((char-double-token? char) (let ((first (get-char port)) 
                                               (second (get-char port)))
                                           (cons (make-token (match-double-token first second)
                                                             (list->string (list first second))
                                                             nil
                                                             0)
                                                 (scan-tokens port))))
              ((whitespace? char) (let ((char (get-char port)))
                                    (scan-tokens port)))
              (else (error (format #f "Error with: ~a\n" char)))))))


(define-public (scan-alpha port)
  (cons (get-char port)
        (let ((char (lookahead-char port)))
          (if (or (alpha-numeric? char) (alpha-symbol? char))
              (scan-alpha port)
              nil))))

(define-public (scan-number port)
  (cons (get-char port)
        (let ((char (lookahead-char port)))
          (if (digit? char)
              (scan-number port)
              nil))))

(define-public (scan-double-token port)
  (define first (get-char port))
  (let ((second (lookahead-char port)))
    
    (cond (char=? second))))
