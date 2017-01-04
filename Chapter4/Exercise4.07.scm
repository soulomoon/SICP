; Exercise 4.7: Let* is similar to let, except that the bindings of the let* variables are performed sequentially from left to right, and each binding is made in an environment in which all of the preceding bindings are visible. For example

; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))
; returns 39. Explain how a let* expression can be rewritten as a set of nested let expressions, and write a procedure let*->nested-lets that performs this transformation. If we have already implemented let (Exercise 4.6) and we want to extend the evaluator to handle let*, is it sufficient to add a clause to eval whose action is

; (eval (let*->nested-lets exp) env)
; or must we explicitly expand let* in terms of non-derived expressions?

(load "/home/soulomoon/git/SICP/Chapter4/Exercise4.06.scm")

(define (eval-let* exp env)
  (display (let*->nested-lets exp))
  (eval (let*->nested-lets exp) env))

(define (let*->nested-lets exp)
  (if (empty-pairs? exp)
    (make-let (let*-body exp) (let*-first-pair exp))
    (let ((first (let*-first-pair exp))
          (rest (let*-rest-exp exp)))
            (make-let (list first) (let*->nested-lets rest))
            )))
  

(define (empty-pairs? exp)
  (display exp)(newline )
  (null? (cadr exp)))

(define (let*-body exp)
  (cddr exp))

(define (let*-first-pair exp)
  (caadr exp))

(define (let*-rest-pair exp)
  (cdadr exp))

(define (let*-rest-exp exp)
  (make-let* (let*-rest-pair exp) (let*-body exp)))

(define (make-let* pairs body)
  (cons 'let* (cons pairs (cons body nil))))

(put-syntax! 'let* eval-let*) 

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

(interpret
  '(let* ((x 3)
        (y (+ x 2))
        (z (+ x y 5)))
    (* x z)))