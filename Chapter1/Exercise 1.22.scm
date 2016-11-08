; Exercise 1.22. Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). The following timed-prime-test procedure, when called with an integer nn, prints nn and checks to see if nn is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.

; (define (runtime) (current-milliseconds))
; (define (timed-prime-test n)
;   (newline)
;   (display n)
;   (start-prime-test n (runtime)))

; (define (start-prime-test n start-time)
;   (if (prime? n)
;       (report-prime (- (runtime) start-time))
;       #f))

; (define (report-prime elapsed-time)
;   (display " *** ")
;   (display elapsed-time))


; Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of Θ(n−−√)Θ(n), you should expect that testing for primes around 10,000 should take about 1–√010 times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the n−−√n prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?
#lang racket
(define (runtime) (current-milliseconds))
(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? x)
  (if (= (smallest-divisor x) x)
      true
      false))

(define (timed-prime-test n)

  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n) (newline)(display n)(report-prime (- (runtime) start-time)))
        (else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (serch-for-prime n k)
  (timed-prime-test n)
  (cond  ((> k 2) (newline) (display " **************************** "))
          ((prime? n) (serch-for-prime (+ n 1) (+ k 1)))
          (else (serch-for-prime (+ n 1) k))))
(serch-for-prime 10000000000 0)
(serch-for-prime 100000000000 0)
(serch-for-prime 1000000000000 0)
(serch-for-prime 10000000000000 0)
(serch-for-prime 100000000000000 0)


; 10000000019 *** 0
; 10000000033 *** 0
; 10000000061 *** 15
;  ****************************
; 100000000003 *** 31
; 100000000019 *** 31
; 100000000057 *** 15
;  ****************************
; 1000000000039 *** 79
; 1000000000061 *** 78
; 1000000000063 *** 109
;  ****************************
; 10000000000037 *** 235
; 10000000000051 *** 234
; 10000000000099 *** 234
;  ****************************
; 100000000000031 *** 734
; 100000000000067 *** 719
; 100000000000097 *** 890
;  ****************************