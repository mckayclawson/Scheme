;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1A Question1

; 2nd : List -> s-expression
(define (2nd L)
  (if (null? L)
      '()
      (if (eq? (cdr L) '())
          '()
          (cadr L))))

;(equal? (2nd '(x y z)) 'y)
;(equal? (2nd '(a a b b)) 'a)
;(equal? (2nd '(a)) '())
;(equal? (2nd '()) '())
;(equal? (2nd '(a (a b c) b)) '(a b c))