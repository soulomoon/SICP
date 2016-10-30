; Exercise 1.27: Demonstrate that the Carmichael numbers listed in Footnote 47 really do fool the Fermat test. That is, write a procedure that takes an integer nn and tests whether anan is congruent to aa modulo nn for every a<na<n, and try your procedure on the given Carmichael numbers.
; Exercise 1.27: Demonstrate that the Carmichael numbers listed in Footnote 47 really do fool the Fermat test. That is, write a procedure that takes an integer nn and tests whether anan is congruent to aa modulo nn for every a<na<n, and try your procedure on the given Carmichael numbers.
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
  (try-it (+ 1 (random (- n 1)))))

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
  (cond  ((> k 0) (newline) (display " **************************** "))
          ((fast-prime? n 100) (serch-for-prime (+ n 1) (+ k 1)))
          (else (serch-for-prime (+ n 1) k))))


(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (prime? x)
  (if (= (smallest-divisor x) x)
      true
      false))
(define (old-timed-prime-test n)
  (old-start-prime-test n (runtime)))
(define (old-start-prime-test n start-time)
  (cond ((prime? n) (newline)(display n)(report-prime (- (runtime) start-time)))
        (else false)))
(define (old-serch-for-prime n k)
  (old-timed-prime-test n)
  (cond  ((> k 0) (newline) (display " **************************** "))
          ((prime? n) (old-serch-for-prime (+ n 1) (+ k 1)))
          (else (old-serch-for-prime (+ n 1) k))))

(serch-for-prime 6601 0)
(old-serch-for-prime 6601 0)

; 6601 *** 0
;  ****************************
; 6607 *** 0
;  ****************************
; >