; Exercise 4.20: Because internal definitions look sequential but are actually simultaneous, some people prefer to avoid them entirely, and use the special form letrec instead. Letrec looks like let, so it is not surprising that the variables it binds are bound simultaneously and have the same scope as each other. The sample procedure f above can be written without internal definitions, but with exactly the same meaning, as

; (define (f x)
;   (letrec
;       ((even?
;         (lambda (n)
;           (if (= n 0)
;               true
;               (odd? (- n 1)))))
;        (odd?
;         (lambda (n)
;           (if (= n 0)
;               false
;               (even? (- n 1))))))
;     ⟨rest of body of f⟩))
; Letrec expressions, which have the form

; (letrec ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expₙ⟩))
;   ⟨body⟩)
; are a variation on let in which the expressions ⟨expk⟩⟨expk⟩ that provide the initial values for the variables ⟨vark⟩⟨vark⟩ are evaluated in an environment that includes all the letrec bindings. This permits recursion in the bindings, such as the mutual recursion of even? and odd? in the example above, or the evaluation of 10 factorial with

; (letrec
;     ((fact
;       (lambda (n)
;         (if (= n 1)
;             1
;             (* n (fact (- n 1)))))))
;   (fact 10))
; Implement letrec as a derived expression, by transforming a letrec expression into a let expression as shown in the text above or in Exercise 4.18. That is, the letrec variables should be created with a let and then be assigned their values with set!.
; Louis Reasoner is confused by all this fuss about internal definitions. The way he sees it, if you don’t like to use define inside a procedure, you can just use let. Illustrate what is loose about his reasoning by drawing an environment diagram that shows the environment in which the ⟨rest of body of f⟩ is evaluated during evaluation of the expression (f 5), with f defined as in this exercise. Draw an environment diagram for the same evaluation, but with let in place of letrec in the definition of f.

(load "/Users/soulomoon/git/SICP/Chapter4/Exercise4.06.scm")

; 1
(define (letrec-empty-pairs? exp)
  ; (display (cadr exp))
  (null? (cadr exp)))
  
(define (letrec-body exp)
  (cddr exp))
(define (letrec-pa-pairs exp)
  (cadr exp))
(define (letrec-parameters exp)
  (map car (letrec-pa-pairs exp)))
(define (letrec-arguments exp)
  (map cadr (letrec-pa-pairs exp)))


(define (pairs->unassigned exp)
  (map 
    (lambda (para) (list para ''*unassigned*)) 
    (letrec-parameters exp)))

(define (exp->sets exp)
  (map 
    (lambda (para arg) (list 'set! para arg)) 
    (letrec-parameters exp)
    (letrec-arguments exp)))

(define (letrec->let exp)
  (apply 
    make-let
    (append 
      (cons (pairs->unassigned exp)
            (exp->sets exp))
      (letrec-body exp))))

(define (eval-letrec exp env)
(display (letrec->let exp))(newline )
  (eval# (letrec->let exp) env))

(define (make-letrec pairs . body)
  (cons 'letrec (cons pairs body)))

(put-syntax! 'letrec eval-letrec) 

(interpret
'(letrec
    ((fact
      (lambda (n)
        (if (= n 1)
            1
            (* n (fact (- n 1)))))))
  (fact 6)))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; 'ok
; (let ((fact '*unassigned*)) (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 6))
; ((lambda (fact) (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 6)) '*unassigned*)
; 720
; > 

; ; 2

; unfold the expression
; (define (f x)
;   (let
;       ((even?
;         (lambda (n)
;           (if (= n 0)
;               true
;               (odd? (- n 1)))))
;        (odd?
;         (lambda (n)
;           (if (= n 0)
;               false
;               (even? (- n 1))))))
;     ⟨rest of body of f⟩))

; (define f
;   (lambda (x)  
;     ((lambda (even? odd?)
;       ⟨rest of body of f⟩
;     )
;     (lambda (n)
;       (if (= n 0)
;           true
;           (odd? (- n 1))))
;     (lambda (n)
;       (if (= n 0)
;           false
;           (even? (- n 1)))))))

; (f 5)

; global env
; ((lambda (x)  
;  env1 
;  ((lambda (even? odd?)
;     env2
;     ⟨rest of body of f⟩)
;     (lambda (n)
;       (if (= n 0)
;           true
;           (odd? (- n 1))))
;     (lambda (n)
;       (if (= n 0)
;           false
;           (even? (- n 1))))))
;   5)

; we can see that the env of even? adn ood? as parameters is in env1, whichi there is no definitions of enven? and odd?