;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1B Question5

;----------------- Code From Question 4 ---------------
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
;(equal? (make-sum '2 '3) 5)
;(equal? (make-sum '2 '0) 2)
;(equal? (make-sum (make-sum 'x 2) 0) '(+ x 2))
      
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

;; sums and products will use the same selectors
; arg1: SumExp or ProdExp -> ArithExp
(define (arg1 e) (car (cdr e)))
; (= (arg1 (make-sum 2 3)) 2)
; (= (arg1 (make-product 2 3)) 2)

; arg2: SumExp or ProdExp -> ArithExp
(define (arg2 e) (car (cdr (cdr e))))
; (= (arg2 (make-sum 2 3)) 3)
; (= (arg2 (make-product 2 3)) 3)

;----------------- Code From Question 4 ---------------



; zero-poly? : Poly -> Bool
(define (zero-poly? poly)
  (if (eq? poly '())
      #t
      #f))
;(eq? (zero-poly? '()) #t)
;(eq? (zero-poly? '(x)) #f)
;(eq? (zero-poly? '(1)) #f)


; poly<-const : Number -> Polynomial 
; (poly<-const 5) -> (* 5 1))
(define (poly<-const const)
  (if (number? const)
      (if (eq? const 0)
          '()
          (list const))
      (error 'poly<-const "bad constant")))
;(equal? (poly<-const 2) '(2))
;(equal? (poly<-const 0) '())

;poly<-var : -> Polynomial
(define (poly<-var) (list 0 1))
;(equal? (poly<-var) '(0 1))
;cant really have more than one test case for this function :)

; shift-left : Poly -> Poly
(define (shift-left p) (append '(0) p))
;(equal? (shift-left '(2 4 5 1)) '(0 2 4 5 1))
;(equal? (shift-left '(0)) '(0 0))
;(equal? (shift-left '()) '(0))

; shift-right : Poly -> Poly
(define (shift-right p)
  (if (null? p)
      '()
      (cdr p)))
;(equal? (shift-right '(2 4 5 1)) '(4 5 1))
;(equal? (shift-right '()) '())
;(equal? (shift-right '(0)) '())

; const-coeff : Poly -> Number
(define (const-coeff p)
  (if (null? p)
      0
      (if (pair? p)
          (car p)
          (error 'const-coeff "Must be a polynomial (non empty list of numbers)"))))
;(equal? (const-coeff '(1 2 3 4)) 1)
;(equal? (const-coeff '(4)) 4)

; add-const-poly : Poly * Number -> Poly
(define (add-const-poly p c)
  (if (null? p)
      (list c)
      (cons (+ (car p) c) (cdr p))))
;(equal? (add-const-poly '(1 2 3 4) 1) '(2 2 3 4))
;(equal? (add-const-poly '() 1) '(1))

; scale-poly : Poly * Number -> Poly
(define (scale-poly p c)
  (if (null? p)
      p
      (if (eq? c 0)
          '()
          (cons (* (car p) c) (scale-poly (cdr p) c)))))
;(equal? (scale-poly '(1 2 3 4) 1) '(1 2 3 4))
;(equal? (scale-poly '(1 2 3 4) 2) '(2 4 6 8))
;(equal? (scale-poly '(1) 2) '(2))
;(equal? (scale-poly '() 2) '())
;(equal? (scale-poly '(1) 0) '())

; add-poly : Poly * Poly -> Poly
(define (add-poly p1 p2)
  (cond ((null? p1) p2)
        ((null? p2) p1)
        (else (cons (+ (const-coeff p1) (const-coeff p2)) (add-poly (shift-right p1) (shift-right p2))))))

;(equal? (add-poly '(1 2 3 4 5) '(1 2 3)) '(2 4 6 4 5))
;(equal? (add-poly '(1 2 3) '(1 2 3 4 5)) '(2 4 6 4 5))
;(equal? (add-poly '(1 2 3 4 5) '(1 2 3 4 5)) '(2 4 6 8 10))
;(equal? (add-poly '() '(1 2 3)) '(1 2 3))
;(equal? (add-poly '(1 2 3) '()) '(1 2 3))
;(equal? (add-poly '(1 2 0 3) '(1 2)) '(2 4 0 3))
;(equal? (add-poly '() '()) '())

;mult-poly : Poly * Poly -> Poly
(define (mult-poly p1 p2)
  (cond ((null? p1) '())
        ((null? p2) '())
        (else (add-poly (mult-poly (shift-right p1) (shift-left p2)) (scale-poly p2 (const-coeff p1))))))
;(equal? (mult-poly '() '(1 2 3)) '())
;(equal? (mult-poly '(1 2 3) '()) '())
;(equal? (mult-poly '(1 2 3) '(1 2)) '(1 4 7 6))
;(equal? (mult-poly '(1 2) '(1 2 3)) '(1 4 7 6))
;(equal? (mult-poly '() '()) '())
;(equal? (mult-poly '(1) '(1 2)) '(1 2))
;(equal? (mult-poly '(1 2 3 4 5) '(1 2 3)) '(1 4 10 16 22 22 15))

; expt-poly : Poly * NatNum -> Poly
(define (expt-poly p n)
  (if (not (number? n))
      (error 'expt-poly "n is not a number")
      (if (< n 1)
          (error 'expt-poly "n must be greater than zero (natural number)")
          (if (= n 1)
              p
              (mult-poly p (expt-poly p (- n 1)))))))
;(equal? (expt-poly '(2) 2) '(4))
;(equal? (expt-poly '(2) 3) '(8))
;(equal? (expt-poly '(1 2 3) 2) '(1 4 10 12 9))
;(equal? (expt-poly '(1 2 3) 3) '(1 6 21 44 63 54 27))
;(equal? (expt-poly '(1 0 2) 2) '(1 0 4 0 4))

; poly<-exp
(define (poly<-exp exp var)
  (cond ((null? exp) '())
        ((number? exp) (poly<-const exp))
        ((variable? exp) (if (variable=? var exp) (poly<-var) (error 'poly<-exp "multivariate expression found")))
        ((sum? exp)
         (if (and (not (or (number? (arg1 exp)) (variable? (arg1 exp)))) (not (or (number? (arg2 exp)) (variable? (arg2 exp)))))
             (add-poly (poly<-exp (arg1 exp) var)  (poly<-exp (arg2 exp) var))
             (if (not (or (number? (arg1 exp)) (variable? (arg1 exp))))
                 (add-poly (poly<-exp (arg1 exp) var)
                                      (if (number? (arg2 exp))
                                          (poly<-const (arg2 exp))
                                          (if (variable=? var (arg2 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found"))))
                 (if (not (or (number? (arg2 exp)) (variable? (arg2 exp))))
                     (add-poly (if (number? (arg1 exp))
                                          (poly<-const (arg1 exp))
                                          (if (variable=? var (arg1 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found")))
                               (poly<-exp (arg2 exp)))
                     (if (number? (arg1 exp))
                         (add-poly (poly<-const (arg1 exp)) (if (number? (arg2 exp))
                                                                (poly<-const (arg2 exp))
                                                                (if (variable=? var (arg2 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found"))))
                         (add-poly (if (variable=? var (arg1 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found")) (if (number? (arg2 exp))
                                                                (poly<-const (arg2 exp))
                                                                (if (variable=? var (arg2 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found")))))))))
        ((product? exp)
         (if (and (not (or (number? (arg1 exp)) (variable? (arg1 exp)))) (not (or (number? (arg2 exp)) (variable? (arg2 exp)))))
             (mult-poly (poly<-exp (arg1 exp) var)  (poly<-exp (arg2 exp) var))
             (if (not (or (number? (arg1 exp)) (variable? (arg1 exp))))
                 (mult-poly (poly<-exp (arg1 exp) var)
                                      (if (number? (arg2 exp))
                                          (poly<-const (arg2 exp))
                                          (if (variable=? var (arg2 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found"))))
                 (if (not (or (number? (arg2 exp)) (variable? (arg2 exp))))
                     (mult-poly (if (number? (arg1 exp))
                                          (poly<-const (arg1 exp))
                                          (if (variable=? var (arg1 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found")))
                               (poly<-exp (arg2 exp) var))
                     (if (number? (arg1 exp))
                         (mult-poly (poly<-const (arg1 exp)) (if (number? (arg2 exp))
                                                                (poly<-const (arg2 exp))
                                                                (if (variable=? var (arg2 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found"))))
                         (mult-poly (if (variable=? var (arg1 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found")) (if (number? (arg2 exp))
                                                                (poly<-const (arg2 exp))
                                                                (if (variable=? var (arg2 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found")))))))))
        ((expt? exp)
         (if (not (variable? (arg1 exp)))
             (expt-poly (poly<-exp (arg1 exp) var) (arg2 exp))
             (expt-poly (if (variable=? var (arg1 exp)) (poly<-var) (error 'poly<-exp "multivariate expression found")) (arg2 exp))))))
;(equal? (poly<-exp 1 'x) '(1))
;(equal? (poly<-exp 'x 'x) '(0 1))
;(equal? (poly<-exp '(+ x x) 'x) '(0 2))
;(equal? (poly<-exp '(+ x 1) 'x) '(1 1))
;(equal? (poly<-exp '(+ x x) 'x) '(0 2))
;(equal? (poly<-exp '(+ (+ x 9) (+ 9 x)) 'x) '(18 2))
;(equal? (poly<-exp '(* x x) 'x) '(0 0 1))
;(equal? (poly<-exp '(* x 1) 'x) '(0 1))
;(equal? (poly<-exp '(* x 5) 'x) '(0 5))
;(equal? (poly<-exp '(* (* x 5) x) 'x) '(0 0 5))
;(equal? (poly<-exp '(* x x) 'x) '(0 0 1))
;(equal? (poly<-exp '(^ x 2) 'x) '(0 0 1))
;(equal? (poly<-exp '(^ x 1) 'x) '(0 1))
;(equal? (poly<-exp '(^ (+ x 2) 2) 'x) '(4 4 1))
;(equal? (poly<-exp '(^ (* x 2) 2) 'x) '(0 0 4))
;(equal? (poly<-exp '(^ (^ x 2) 2) 'x) '(0 0 0 0 1))
;(equal? (poly<-exp '(^ x 10) 'x) '(0 0 0 0 0 0 0 0 0 0 1))

; exp<-poly-tail : Poly * Var * NatNum -> ArithExp
(define (exp<-poly-tail p v n)
  (if (null? p)
      0
      (make-sum (make-product (car p) (make-expt v n)) (exp<-poly-tail (cdr p) v (+ n 1)))))
;(equal? (exp<-poly-tail '() 'x 0) 0)
;(equal? (exp<-poly-tail '(0 0 1) 'x 0) '(^ x 2))
;(equal? (exp<-poly-tail '(0 1 1) 'x 0) '(+ x (^ x 2)))
;(equal? (exp<-poly-tail '(1 1 1) 'x 0) '(+ 1 (+ x (^ x 2))))

; exp<-poly : Poly * Var -> ArithExp
(define (exp<-poly p v)
  (exp<-poly-tail p v 0))
;(equal? (exp<-poly '() 'x) 0)
;(equal? (exp<-poly '(0 0 1) 'x) '(^ x 2))
;(equal? (exp<-poly '(0 1 1) 'x) '(+ x (^ x 2)))
;(equal? (exp<-poly '(1 1 1) 'x) '(+ 1 (+ x (^ x 2))))

; integrate-polya : Poly * Number-> Poly
; n = the exponent of the car of the list
(define (integrate-polya p n)
    (if (or (null? p) (zero-poly? p))
      '()
      (if (= n 0)
          (cons (car p) (integrate-polya (cdr p) (+ n 1)))
          (cons (* (car p) (/ 1 n)) (integrate-polya (cdr p) (+ n 1))))))
;(equal? (integrate-polya '() 0) '())
;(equal? (integrate-polya '(0 0 1) 0) '(0 0 1/2))
;(equal? (integrate-polya '(0 0 0 1) 0) '(0 0 0 1/3))
;(equal? (integrate-polya '(0 1 1 1) 0) '(1 1/2 1/3))
  
; integrate-poly : Poly -> Poly
; is accumulative recursion the best approach here?
(define (integrate-poly p) 
  (if (zero-poly? p)
      '()
      (integrate-polya (shift-left p) 0)))
;(equal? (integrate-poly '()) '())
;(equal? (integrate-poly '(0 1)) '(0 0 1/2))
;(equal? (integrate-poly '(0 0 1)) '(0 0 0 1/3))
;(equal? (integrate-poly '(1 1 1)) '(0 1 1/2 1/3))

; integral : ArithExp * Variable -> ArithExp
(define (integral exp var)
  (exp<-poly (integrate-poly (poly<-exp exp var)) var))
;(equal? (integral 1 'x) 'x)
;(equal? (integral 0 'x) 0)
;(equal? (integral 'x 'x) '(* 1/2 (^ x 2)))
;(equal? (integral '(* x x) 'x) '(* 1/3 (^ x 3)))


      
        
  


 


       
       