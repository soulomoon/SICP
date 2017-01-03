; Exercise 4.3: Rewrite eval so that the dispatch is done in data-directed style. Compare this with the data-directed differentiation procedure of Exercise 2.73. (You may use the car of a compound expression as the type of the expression, as is appropriate for the syntax! implemented in this section.)

; (define (eval# exp env)
;   (cond ((self-evaluating? exp) 
;          exp)
;         ((variable? exp) 
;          (lookup-variable-value exp env))
;         ((quoted? exp) 
;          (text-of-quotation exp))
;         ((assignment? exp) 
;          (eval-assignment exp env))
;         ((definition? exp) 
;          (eval-definition exp env))
;         ((if? exp) 
;          (eval-if exp env))
;         ((lambda? exp)
;          (make-procedure 
;           (lambda-parameters exp)
;           (lambda-body exp)
;           env))
;         ((begin? exp)
;          (eval-sequence 
;           (begin-actions exp) 
;           env))
;         ((cond? exp) 
;          (eval (cond->if exp) env))
;         ((application? exp)
;          (apply (eval (operator exp) env)
;                 (list-of-values 
;                  (operands exp) 
;                  env)))
;         (else
;          (error "Unknown expression 
;                  type: EVAL" exp))))


;; Syntax! table procedures for meta-circular evaluator
(load "/home/soulomoon/git/SICP/Chapter4/ex4.03.rkt")

(define (eval# exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get-syntax (type-tag exp)) ((get-syntax (type-tag exp)) exp env))
        ((application? exp)
         (apply# (eval# (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


; (put-syntax! 'quote text-of-quotation)
; (put-syntax! 'set! eval-assignment)
; (put-syntax! 'define eval-definition)
; (put-syntax! 'if? eval-if)
; (put-syntax! 'lambda?  (lambda (exp env) 
;                 (make-procedure 
;                   (lambda-parameters exp)
;                   (lambda-body exp)
;                   env)))
; (put-syntax! 'begin (lambda (exp env)
;               (eval-sequence 
;                 (begin-actions exp) 
;                 env)))
; (put-syntax! 'cond (lambda (exp env)
;               (eval (cond->if exp) env)))

(interpret '(list (quote '2) (quote '3)))

(list (quote '2) (quote '3))