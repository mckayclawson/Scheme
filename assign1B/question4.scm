;Author: McKay Clawson, Professor Arthur Nunes-Harwitt
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1B Question4


;Author: Arthur Nunes-Harwitt
;Contributor: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1B Question3

;; An arithmetic expression (ArithExp) is one of the following.
;; a number n
;; a variable x
;; a sum with parts e1 and e2, 
;;   where e1 and e2 are arithmetic expressions
;; a product with parts e1 and e2,
;;   where e1 and e2 are arithmetic expressions


; variable?: Any -> Bool
(define variable? symbol?)
; (eq? (variable? 'x) #t)
; (eq? (variable? 3) #f)

; variable=?: VarExp * VarExp -> Bool
(define variable=? eq?)
; (variable=? 'x 'x)
; (not (variable=? 'x 'y))

;; a sum is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol +
; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) '+)))
; (eq? (sum? '(+ 2 3)) #t)
; (eq? (sum? '3) #f)

; make-sum: ArithExp * ArithExp -> SumExp
(define (make-sum e1 e2)
  (if (and (number? e1) (number? e2))
      (+ e1 e2)
      (if (eq? e1 0)
          e2
          (if (eq? e2 0)
              e1
              (list '+ e1 e2)))))
(equal? (make-sum '2 '3) 5)
(equal? (make-sum '2 '0) 2)
(equal? (make-sum (make-sum 'x 2) 0) '(+ x 2))
      
;; a product is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol *

; product?: Any -> Bool
(define (product? a) (and (pair? a) (eq? (car a) '*)))
; (eq? (product? '(* 2 3)) #t)
; (eq? (product? '3) #f)

; make-sum: ArithExp * ArithExp -> ProductExp
(define (make-product e1 e2) 
  (if (and (number? e1) (number? e2))
      (* e1 e2)
      (if (eq? e1 1)
          e2
          (if (eq? e2 1)
              e1
              (if (or (eq? e1 0) (eq? e2 0))
                  0
                  (list '* e1 e2))))))
  
 ;(equal? (make-product 2 3) '6)
 ;(equal? (make-product 'x 0) '0)
 ;(equal? (make-product 0 'x) '0)
 ;(equal? (make-product 'x 1) 'x)
 ;(equal? (make-product 1 'x) 'x)

; expt? : ArithExp -> Bool
(define (expt? e) (and (pair? e) (eq? (car e) '^)))
;(equal? (expt? '(^ x 2)) #t)
;(equal? (expt? '(* x 2)) #f)
;(equal? (expt? '(^ '(+ x 2) 2)) #t)

; make-expt : ArithExp * NatNum -> ExponentExp
(define (make-expt e n) 
  (if (eq? n 0)
      1
      (if (eq? n 1)
          e
          (if (and (number? e) (number? n))
              (expt e n)
              (list '^ e n)))))
          
;(equal? (make-expt '(+ x 2) 1) '(+ x 2))
;(equal? (make-expt '(+ x 2) 0) 1)
;(equal? (make-expt '(+ x 2) 2) '(^ (+ x 2) 2))
;(equal? (make-expt 'x 2) '(^ x 2))
;(equal? (make-expt 2 3) 8)

;; sums and products will use the same selectors
; arg1: SumExp or ProdExp -> ArithExp
(define (arg1 e) (car (cdr e)))
; (= (arg1 (make-sum 2 3)) 2)
; (= (arg1 (make-product 2 3)) 2)

; arg2: SumExp or ProdExp -> ArithExp
(define (arg2 e) (car (cdr (cdr e))))
; (= (arg2 (make-sum 2 3)) 3)
; (= (arg2 (make-product 2 3)) 3)

;deriv: ArithExp * VarExp -> ArithExp
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (variable=? exp var) 1 0))
        ((sum? exp) 
         (make-sum (deriv (arg1 exp) var) (deriv (arg2 exp) var)))
        ((product? exp)
         (make-sum (make-product (arg1 exp) (deriv (arg2 exp) var))
                   (make-product (arg2 exp) (deriv (arg1 exp) var))))
        ((expt? exp)
         (make-product (arg2 exp) (make-expt (arg1 exp) (- (arg2 exp) 1))))
        (else (error 'deriv "Unexpected Input, not an ArithExp"))))

; (= (deriv 1 'x) 0)
; (= (deriv 'y 'x) 0)
; (= (deriv 'x 'x) 1)
; (equal? (deriv (make-sum (make-product 'x 'x) (make-product (make-expt 'x 3) 5)) 'x) '(+ (+ x x) (* 5 (* 3 (^ x 2)))))
