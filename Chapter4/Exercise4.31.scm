; Exercise 4.31: The approach taken in this section is somewhat unpleasant, because it makes an incompatible change to Scheme. It might be nicer to implement lazy evaluation as an upward-compatible extension, that is, so that ordinary Scheme programs will work as before. We can do this by extending the syntax of procedure declarations to let the user control whether or not arguments are to be delayed. While we’re at it, we may as well also give the user the choice between delaying with and without memoization. For example, the definition

; (define (f a (b lazy) c (d lazy-memo))
;   ...)
; would define f to be a procedure of four arguments, where the first and third arguments are evaluated when the procedure is called, the second argument is delayed, and the fourth argument is both delayed and memoized. Thus, ordinary procedure definitions will produce the same behavior as ordinary Scheme, while adding the lazy-memo declaration to each parameter of every compound procedure will produce the behavior of the lazy evaluator defined in this section. Design and implement the changes required to produce such an extension to Scheme. You will have to implement new syntax procedures to handle the new syntax for define. You must also arrange for eval or apply to determine when arguments are to be delayed, and to force or delay arguments accordingly, and you must arrange for forcing to memoize or not, as appropriate.

(load "/Users/soulomoon/git/SICP/Chapter4/lazyeval.scm")

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval# (definition-value exp) env)
    env)
  'ok)
  
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body


(define (apply# procedure arguments env)
  ; (display "new")(display procedure)
  (cond ((primitive-procedure? procedure)
(display "apply#--------------------")(display procedure)(display arguments)(newline )
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((compound-procedure? procedure)
          ; (display (procedure-body procedure))
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (matching-paras 
            (procedure-parameters procedure) 
            arguments 
            env)
           (matching-args
            (procedure-parameters procedure)
            arguments
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY" 
                     procedure))))


(define (matching-paras paras args env)
  (map (lambda (p a)
        (cond 
            ((symbol? p) p)
            (else (car p))))
       paras args))
       
(define (matching-args paras args env)
  (map (lambda (p a)
        (cond 
            ((symbol? p) a)
            (else
              (if (or (eq? (cadr p) 'lazy-memo) (eq? (cadr p) 'lazy))
                  (cons (cadr p) (delay-it a env))
                  (error "wrong variable suffix" (cadr p))))))
       paras args))




(define (actual-value exp env)
  (force-selector (eval# exp env)))

(define (force-selector exp) 
  (cond
    ((lazy? exp) (force-it-without-memo (cdr exp)))
    ((lazy-memo? exp) (force-it (cdr exp)))
    (else (force-it exp))
  )
)

(define (lazy? obj) (tagged-list? obj 'lazy))
(define (lazy-memo? obj) (tagged-list? obj 'lazy-memo))



(interpret
'(begin
 (define (p1 (x lazy))
  (set! x (cons x '(2))) x)
 (display (p1 1))
 )
)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; eval#-----(begin (define (p1 (x lazy)) (set! x (cons x '(2))) x) (display (p1 1)))
; eval#-----(define (p1 (x lazy)) (set! x (cons x '(2))) x)
; eval#-----(lambda ((x lazy)) (set! x (cons x '(2))) x)
; eval#-----(display (p1 1))
; eval#-----display
; apply#--------------------(primitive #<procedure:mdisplay>)((p1 1))
; eval#-----(p1 1)
; eval#-----p1
; delay-it--------1
; eval#-----(set! x (cons x '(2)))
; eval#-----(cons x '(2))
; eval#-----cons
; apply#--------------------(primitive #<procedure:mcons>)(x '(2))
; eval#-----x
; eval#-----1
; eval#-----'(2)
; eval#-----x
; (1 2)
; > 