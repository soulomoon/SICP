; Exercise 3.36: Suppose we evaluate the following sequence of expressions in the global environment:

; (define a (make-connector))
; (define b (make-connector))
; (set-value! a 10 'user)
; At some time during evaluation of the set-value!, the following expression from the connector’s local procedure is evaluated:

; (for-each-except 
;   setter inform-about-value constraints)
; Draw an environment diagram showing the environment in which the above expression is evaluated.
  +-----------+
--|           |
  +-----------+
          ^
          |
        +-----------------------------+<------+
        |loop------------------------------->[*] [*]----> parameters:list
        |exception:'user              |                   body:....
        |procedure:inform-about-value |
        |list:'()                     |
        |                             |
        +-----------------------------+
           ^
           |
        +-----------+
        |items:'()  |          
        +-----------+
         (cond ((null? '()  ) 'done)
          ((eq? (car   '()) 'user) 
           (loop (cdr '())))
          (else (procedure (car '()  ))
                (loop (cdr '()  )))) 

(define (outside value) (+ value x))
(define x 1)
(define (runner)
(define x 2)
(outside 1))
(define (runner1)
(define x 2)
(outside x))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; > (runner)
; 2
; > (runner1)
; 3
; > 




