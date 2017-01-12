; Exercise 4.11: Instead of representing a frame as a pair of lists, we can represent a frame as a list of bindings, where each binding is a name-value pair. Rewrite the environment operations to use this alternative representation.


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; solutions is to change the constructor and getter
; since it is not using data abstraction, you have to modify a lot
(define (make-frame variables values)
  (map cons variables values))
(define (first-pair frame)
  (car frame))
(define (rest-pairs frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) frame)))


(define (extend-environment base-env)
  (cons (make-frame vars vals) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car (first-pair frame)))
              (car (first-pair frame)))  
            (else (scan (rest-pairs frame)))))

    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car (first-pair frame)))
             (set-car! (first-pair frame)))
            (else (scan frame))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car (first-pair frame)))
             (set-car! (first-pair frame) val))
            (else (scan (rest-pair frame)))))
    (scan frame)))



(define variables '(1 2 3))
(define values '(a b c))

(define Aenvironment (list (make-frame variables values)))



(lookup-variable-value 2 Aenvironment)
