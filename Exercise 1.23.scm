; Exercise 1.23. The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. This suggests that the values used for test-divisor should not be 2,3,4,5,6,…2,3,4,5,6,…, but rather 2,3,5,7,9,…2,3,5,7,9,… To implement this change, define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12 primes found in exercise 1.22. Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?
#lang racket
(define (square x) (* x x))

(define (next n)
(if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (runtime) (current-milliseconds))

(define (smallest-divisor n)
  (find-divisor n 2))

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


; new
; 10000000019 *** 8
; 10000000033 *** 7
; 10000000061 *** 5
;  ****************************
; 100000000003 *** 16
; 100000000019 *** 15
; 100000000057 *** 18
;  ****************************
; 1000000000039 *** 56
; 1000000000061 *** 50
; 1000000000063 *** 48
;  ****************************
; 10000000000037 *** 148
; 10000000000051 *** 147
; 10000000000099 *** 145
;  ****************************
; 100000000000031 *** 518
; 100000000000067 *** 499
; 100000000000097 *** 488
;  ****************************

; old
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

; not really a half, but doing much better than before,
; better for obvios reason that it skip half of the number of time to compare and calculate if not counting the one in the next is using,
; so the one in the next still have to us the count, but each step is two times than just adding one, and maybe of the referrence reson