; Exercise 4.12: The procedures define-variable!, set-variable-value! and lookup-variable-value can be expressed in terms of more abstract procedures for traversing the environment structure. Define abstractions that capture the common patterns and redefine the three procedures in terms of these abstractions.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

  (define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))



(define (env-setter . variables)
  (let ((info (car variables))
        (vars (cdr variables)))


        (define (find_in_frame var vars frame)
          (cond
            ((null? vars) false)
            ((eq? var (car vars)) vars)
            (else (find_in_frame (cdr vars)))
          )
        )

    (define (scan vars vals null_command predict_command)
      (cond ((null? vars)
             (null_command vars vals))
             ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
                        
    (define (set-variable-value! var val env)
      (let ((frame first-frame env))
           (define (null_command vars vals)
             (add-binding-to-frame! 
              var val frame))
           (define (predict_command vars vals)
            (eq? var (car vars)))
           (scan (frame-variables frame) 
                 (frame-values frame)
                 null_command 
                 predict_command)))



  

'done)