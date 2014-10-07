;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 2A Question2

;------------------------------Code from Q1------------------------------------
;union : List * List -> List
(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (equal? (member (car set1) set2) #f)) (union (cdr set1) set2))
        (else (cons (car set1) (union (cdr set1) set2)))))
;(equal? (union '() '()) '())
;(equal? (union '(1) '()) '(1))
;(equal? (union '() '(1)) '(1))
;(equal? (union '(1 2 3 4) '(5 6 7 8 9)) '(1 2 3 4 5 6 7 8 9))
;(equal? (union '(1 2 3 4) '(5 2 6 3)) '(1 4 5 2 6 3))
;(equal? (union '(1 2 3 4) '(1 2 3 4)) '(1 2 3 4))
;(equal? (union '(1 2 3 4) '(3)) '(1 2 4 3))
;-----------------------------------------------------------------------------


(define (first-alpha grammar alpha)
  (first3 grammar alpha '()))
;(first-alpha '((S (E 'eof))(E (E '+ T)) (E (T)) (T (T '* F)) (T (F)) (F ('n)) (F ('x)) (F ('- F)) (F ( '\( E '\)))) '(S (E 'eof)))
;(first-alpha '((S (E 'eof))(E (T E1)) (E1 ('+ T E1)) (E1 ('())) (T (F T1)) (T1 ('* F T1)) (T1 ('())) (F ('n)) (F ('x)) (F ('- F)) (F ( '\( E '\)))) '(S (E 'eof)))
;(first3 '((S (E 'eof))(E (E + T)) (E (T)) (T (T * F)) (T (F)) (F ('n)) (F ('x)) (F ('- F)) (F ( '\( E '\)))) '(E 'eof) '())

(define (first3 grammar alpha seen)
  (if (null? alpha)
      (list '())
      (if (pair? (car alpha))
          (if (equal? (car (car alpha)) 'quote) 
              (cdr (car alpha)))
          (first-var3 grammar (filter (lambda (x) (equal? (car x) (car alpha))) grammar) seen))))
;(equal '(x) (first3 '((S (E 'eof)) (E (E + T)) (E (T)) (T (T * F)) (T (F)) (F ('n)) (F ('x)) (F ('- F)) (F ( '\( E '\)))) '((quote x) a b c) '()))


(define (first-var3 grammar rules seen)
  (if (null? rules)
      '()
      (if (equal? (member (car rules) seen) #f)
          (union (first3 grammar (car (cdr (car rules))) (cons seen (car rules))) (first-var3 grammar (cdr rules) (cons seen (car rules))))
          (first-var3 grammar (cdr rules) seen))))
      
      



;grammar
;'((S (E 'eof))(E (E + T)) (E (T)) (T (T * F)) (T (F)) (F ('n)) (F ('x)) (F ('- F)) (F ( '\( E '\))))
;'((S (E 'eof))(E (T E1)) (E1 ('+ T E1)) (E1 ()) (T (F T1)) (T1 ('* F T1)) (T1 ()) (F ('n)) (F ('x)) (F ('- F)) (F ( '\( E '\))))
;alpha
;'(E 'eof)
; 'ε
;(filter (lambda (x) (equal? (car x) 'E)) '((S (E 'eof))(E (E + T)) (E (T)) (T (T * F)) (T (F)) (F ('n)) (F ('x)) (F ('- F)) (F ( '\( E '\)))))