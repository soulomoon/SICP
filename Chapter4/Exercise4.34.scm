; Exercise 4.34: Modify the driver loop for the evaluator so that lazy pairs and lists will print in some reasonable way. (What are you going to do about infinite lists?) You may also need to modify the representation of lazy pairs so that the evaluator can identify them in order to print them.
(load "/Users/soulomoon/git/SICP/Chapter4/lazylist.scm")


(define (lazy? exp)
    ; (newline )(display (car exp))
    (tagged-list? exp 'lazy))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define input-prompt  " M-Eval input:")
(define output-prompt " M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output 
           (eval# input 
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond 
    ((compound-procedure? object)
        (display 
        (list 'compound-procedure
              (procedure-parameters object)
              (procedure-body object)
              '<procedure-env>)))
    ((lazy? object)
        ; (display (cadr object)) 
        (display
        (list 'lazy
              (force-it 
                (apply# 
                  (cadr object)
                  ; (interpret (list 'cons 1 1)) 
                  '((lambda (p q) p))
                  the-global-environment))
              '....many.more
              '<lazy-env>
        )
        ))
    (else (display object))))

(define (lazy? exp)
  (tagged-list? exp 'lazy)
)

  

(interpret '(display (cdr (cons 1 1))))

(force-it (apply# (cadr (interpret (list 'cons 1 1))) '((lambda (p q) p)) the-global-environment))
;(define a (interpret (list 'cons 1 1)))
;(eval# (cons a '((lambda (p q) p))) (procedure-environment a))
(interpret
(list 'display 
    (list 'cdr (list 'cons 1 1))))

(interpret
'(display (car (cons 1 1)))
)


(driver-loop)
