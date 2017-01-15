; Exercise 4.19: Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu Ator are arguing about the desired result of evaluating the expression

; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))
; Ben asserts that the result should be obtained using the sequential rule for define: b is defined to be 11, then a is defined to be 5, so the result is 16. Alyssa objects that mutual recursion requires the simultaneous scope rule for internal procedure definitions, and that it is unreasonable to treat procedure names differently from other names. Thus, she argues for the mechanism implemented in Exercise 4.16. This would lead to a being unassigned at the time that the value for b is to be computed. Hence, in Alyssa’s view the procedure should produce an error. Eva has a third opinion. She says that if the definitions of a and b are truly meant to be simultaneous, then the value 5 for a should be used in evaluating b. Hence, in Eva’s view a should be 5, b should be 15, and the result should be 20. Which (if any) of these viewpoints do you support? Can you devise a way to implement internal definitions so that they behave as Eva prefers?

; in order to accomplish real simultaneous, you have to change when to eval the value to be store in env,that is to eval it when it is call in the define body. here we use delay to do it, and that first we have to implement lookup to force the value out, and this mean that every value store in the env, have to be a delay object.
; here shows how to implement produce's in the interpretor to make it happen 

(load "/Users/soulomoon/git/SICP/Chapter4/Exercise4.06.scm")
(define (lookup-variable-value var env)
  ; (display env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (force (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
; delay the eval# produce when assignment and definition
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (delay (eval# (assignment-value exp) env))
  env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (delay (eval# (definition-value exp) env))
    env)
  'ok)

; to gain that you have every value store in the env to be a delay, change extend.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars (map (lambda (x) (delay x)) vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
; update the syntax
(install-syntax)
; update setup-environment
(define the-global-environment (setup-environment))

; test
(interpret 
'(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; 'done
; ((lambda (a) (define (f x) (define b (+ a x)) (define a 5) (+ a b)) (f 10)) 1)
; 20
; > 