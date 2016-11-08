; Exercise 1.24: Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime? (the Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has Θ(logn)Θ(log⁡n) growth, how would you expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?
#lang racket
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- 4294967087 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (next n)
(if (= n 2) 3 (+ n 2)))

(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 100) (newline)(display n)(report-prime (- (runtime) start-time)))
        (else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (serch-for-prime n k)
  (timed-prime-test n)
  (cond  ((> k 2) (newline) (display " **************************** "))
          ((fast-prime? n 100) (serch-for-prime (+ n 1) (+ k 1)))
          (else (serch-for-prime (+ n 1) k))))

(serch-for-prime 1000000000000000000000000000 0)
(serch-for-prime 10000000000000000000000000000000 0)
(serch-for-prime 100000000000000000000000000000000000000000000000000 0)
(serch-for-prime 100000000000000000000000000000000000000000000000000000000000000000000 0)
(serch-for-prime 10000000000000000000000000000000000000000000000000000000000000000000000000000000000 0)

; Welcome to DrRacket, version 6.6 [3m].
; Language: racket, with debugging; memory limit: 128 MB.

; 1000000000000000000000000103 *** 10
; 1000000000000000000000000279 *** 21
; 1000000000000000000000000283 *** 23
;  ****************************
; 10000000000000000000000000000033 *** 23
; 10000000000000000000000000000057 *** 19
; 10000000000000000000000000000099 *** 17
;  ****************************
; 100000000000000000000000000000000000000000000000151 *** 39
; 100000000000000000000000000000000000000000000000447 *** 38
; 100000000000000000000000000000000000000000000000577 *** 35
;  ****************************
; 100000000000000000000000000000000000000000000000000000000000000000099 *** 55
; 100000000000000000000000000000000000000000000000000000000000000000151 *** 60
; 100000000000000000000000000000000000000000000000000000000000000000609 *** 59
;  ****************************
; 10000000000000000000000000000000000000000000000000000000000000000000000000000000391 *** 64
; 10000000000000000000000000000000000000000000000000000000000000000000000000000000457 *** 71
; 10000000000000000000000000000000000000000000000000000000000000000000000000000000667 *** 67
;  ****************************


; much faster but not linear with digit because large number adding need more time than smaller one