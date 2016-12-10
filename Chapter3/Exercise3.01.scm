; Exercise 3.1: An accumulator is a procedure that is called repeatedly with a single numeric argument and accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum. Write a procedure make-accumulator that generates accumulators, each maintaining an independent sum. The input to make-accumulator should specify the initial value of the sum; for example

; (define A (make-accumulator 5))

; (A 10)
; 15

; (A 10)
; 25

(define (make-accumulator accumulation)
  (lambda (x) (begin (set! accumulation 
                           (+ accumulation x)) 
                     accumulation)))


(define A (make-accumulator 5))

(A 10)
15

(A 10)
25

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 15
; 15
; 25
; 25
; > 