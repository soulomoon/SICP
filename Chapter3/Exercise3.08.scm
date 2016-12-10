; Exercise 3.8: When we defined the evaluation model in 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result. Define a simple procedure f such that evaluating

; (+ (f 0) (f 1))
; will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.

(define (filter c)
  (if (= c 0)
      +
      * ))
(define (maker)
  (let ((count 0))
    (lambda (x) 
      (let ((op (filter count)))
        (begin (set! count (+ count 1))
               (op x 0))))))
(define f (maker))
(define g (maker))

(+ (f 0) (f 1))
(+ (g 1) (g 0))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 0
; 1
; > 