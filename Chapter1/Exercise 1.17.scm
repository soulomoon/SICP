; Exercise 1.17: The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication. In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication procedure (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:

; (define (* a b)
;   (if (= b 0)
;       0
;       (+ a (* a (- b 1)))))
; This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.

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