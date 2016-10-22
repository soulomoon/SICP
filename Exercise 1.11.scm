; Exercise 1.11: A function ff is defined by the rule that f(n)=nf(n)
; =n if n<3n<3 and f(n)=f(n−1)+2f(n−2)+3f(n−3)f(n)=f(n−1)+2f(n−2)+3f
; (n−3) if n≥3n≥3. Write a procedure that computes ff by means of a
; recursive process. Write a procedure that computes ff by means
; of an iterative process.
#lang planet neil/sicp
; recursive
(define (double x) (+ x x))
(define (triple x) (+ x x x))

(define (f n)
    (cond ((< n 3) n)
    (else (+ (f (- n 1))
             (double (f (- n 2)))
             (triple (f (- n 3)))
             )))

; iterative
(define (fi n)
    (f-iter 2 1 0 n))

(define (f-iter a b c count)
    (cond ((= count 0) c)
    (else (f-iter (+ a (double b) (triple c))
                a
                b
                (- count 1)))
    ))