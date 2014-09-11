;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1A Question4

; index : Symbol List -> NatNum or Bool
(define (index sym syms)
  (indexa sym syms 0))

;(equal? (index 'x '()) #f)
;(equal? (index 'x '(x y z)) 0)
;(equal? (index 'x '(y x z)) 1)
;(equal? (index 'x '(y z x)) 2)
;(equal? (index 'x '(w y z)) #f)

; indexa : Symbol List NatNum -> NatNum or Bool
;(indexa sym syms a) = (a + zerobased index) if sym in syms, #f otherwise
(define (indexa sym syms a)
  (cond ((null? syms) #f)
        ((eq? sym (car syms)) a)
        (else (indexa sym (cdr syms) (+ a 1)))))

;(equal? (indexa 'x '() 0) #f)
;(equal? (indexa 'x '(x y z) 0) 0)
;(equal? (indexa 'x '(y x z) 0) 1)
;(equal? (indexa 'x '(y z x) 0) 2)
;(equal? (indexa 'x '(w y z) 0) #f)



