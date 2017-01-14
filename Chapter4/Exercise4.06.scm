; Exercise 4.6: Let expressions are derived expressions, because

; (let ((⟨var₁⟩ ⟨exp₁⟩) … (⟨varₙ⟩ ⟨expₙ⟩))
;   ⟨body⟩)
; is equivalent to

; ((lambda (⟨var₁⟩ … ⟨varₙ⟩)
;    ⟨body⟩)
;  ⟨exp₁⟩
;  …
;  ⟨expₙ⟩)
; Implement a syntactic transformation let->combination that reduces evaluating let expressions to evaluating combinations of the type shown above, and add the appropriate clause to eval to handle let expressions.
(load "/Users/soulomoon/git/SICP/Chapter4/ex4.03.rkt")

(define (let-empty-pairs? exp)
  ; (display (cadr exp))
  (null? (cadr exp)))
  
(define (let-body exp)
  (cddr exp))
(define (let-pa-pairs exp)
  (cadr exp))
(define (let-parameters exp)
  (map car (let-pa-pairs exp)))
(define (let-arguments exp)
  (map cadr (let-pa-pairs exp)))
  
(define (let->combination exp)
  ; (display (make-lambda (let-parameters exp) (let-body exp)))
  (if (let-empty-pairs? exp)
    (sequence->exp (let-body exp))
    (cons (make-lambda (let-parameters exp) (let-body exp))
          (let-arguments exp))))

(define (eval-let exp env)
(display (let->combination exp))
  (eval# (let->combination exp) env))

(define (make-let pairs . body)
  (cons 'let (cons pairs body)))

(put-syntax! 'let eval-let) 
; (let ((x 2)
;       (y 3))
;       (if x y))
; (interpret
;   '(let ((x 2)
;         (y 3))
;         (if x y)))
; (interpret
;   '(let ((x 2))
;     (let 
;         ((y 3))
;         (if y x))))

; (interpret '(let ()
;                   (if 1 1)))
; (interpret (make-let '((x 2) (y 3)) '(if 1 1) '(if x y)))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; 3
; ((lambda (x y) (if x y)) 2 3)3
; ((lambda (x) (let ((y 3)) (if y x))) 2)((lambda (y) (if y x)) 3)2
; (if 1 1)1
; ((lambda (x y) (if 1 1) (if x y)) 2 3)3
; > 