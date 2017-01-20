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

; the user return a procedure
; and you have to extract the value of the procedure. e.g. here the first one 
; try to use the car syntax of our definition outside of evaluator require you to hack the evaluator. 


; it get a correct result, but when you try to use (interpret '(cons 1 1))
; to replace 'a, it would be wrong
; because the lookup in env for 'a would get a lambda exp back
; but (interpret (list 'cons 1 1)) would rezult in returning a procedure, which is a evaluated lambda exp.
(display "----------------------")(newline )(newline )

(interpret
'(define a (cons 1 1))
)
(actual-value (list 'car 'a ) 
the-global-environment)

(display "----------------------")(newline )(newline )

; you could not use actual-value since procedure have to be apply inorder for the evaluator to work
(force-it 
  (apply# 
  (cadr (interpret (list 'cons 1 1))) 
  '((lambda (p q) p)) 
  the-global-environment))

(display "----------------------")(newline )(newline )

; you could use actual-value since it demand for eval# inside, but it makes it more complicated to deal with since you have to work in backwerds
(actual-value 
  (list 
    'car
    (list
      'list 
      ''lazy
      (make-lambda 
        (procedure-parameters (cadr (interpret (list 'cons 1 1))))
        (procedure-body (cadr (interpret (list 'cons 1 1)))) 
      )
    )
  )
  (procedure-environment (cadr (interpret (list 'cons 1 1)))))


; (driver-loop)

; it is a lot harder than it seems to be, because eval# in the user input return a procedure, but you can't eval# a procedure directly.
; you should either, use apply# or turn it back to lambda expression, in order to access it.
; notice there are two lambda in cons, first is for inner state taking in.
; in either case, you have to force it
; in order to get this Exercise done you have to understand the evaluator proccess well enough to mimik its behaviour , and even working backwerds.


; ---------------
; tracking the exp in actual-value:

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; ----------------------

; cons
; list
; 'lazy
; (lambda (m) (m x y))
; 'ok
; (car a)
; car
; (cadr z)
; cadr
; z
; a
; m
; (lambda (p q) p)
; x
; 1
; 1
; ----------------------

; cons
; list
; 'lazy
; (lambda (m) (m x y))
; m
; (lambda (p q) p)
; x
; 1
; 1
; ----------------------

; cons
; list
; 'lazy
; (lambda (m) (m x y))
; cons
; list
; 'lazy
; (lambda (m) (m x y))
; cons
; list
; 'lazy
; (lambda (m) (m x y))
; (car (list 'lazy (lambda (m) (m x y))))
; car
; (cadr z)
; cadr
; z
; (list 'lazy (lambda (m) (m x y)))
; list
; 'lazy
; (lambda (m) (m x y))
; m
; (lambda (p q) p)
; x
; 1
; 1
; > 