; Exercise 1.18. Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.40


#lang planet neil/sicp
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? n)
  (= (remainder n 2) 0))

(define (mul-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (mul-iter (double b) (halve counter) product))
        (else (mul-iter b
                 (- counter 1)
                 (+ b product)))))


(define (mul b n)
  (mul-iter b n 0))

(mul 21 11100)

realizing that I achieve it in 1.17