; Exercise 1.28: One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat’s Little Theorem, which states that if nn is a prime number and aa is any positive integer less than nn, then aa raised to the (n−1)(n−1)-st power is congruent to 1 modulo nn. To test the primality of a number nn by the Miller-Rabin test, we pick a random number a<na<n and raise aa to the (n−1)(n−1)-st power modulo nn using the expmod procedure. However, whenever we perform the squaring step in expmod, we check to see if we have discovered a “nontrivial square root of 1 modulo nn,” that is, a number not equal to 1 or n−1n−1 whose square is equal to 1 modulo nn. It is possible to prove that if such a nontrivial square root of 1 exists, then nn is not prime. It is also possible to prove that if nn is an odd number that is not prime, then, for at least half the numbers a<na<n, computing an−1an−1 in this way will reveal a nontrivial square root of 1 modulo nn. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test. Check your procedure by testing various known primes and non-primes. Hint: One convenient way to make expmod signal is to have it return 0.

#lang racket
(define (square x) (* x x))
(define (oneor r m)
 (or (= r 1) (= r (- m 1))))

(define (remain-oneor? n m)
  (oneor (remainder n m) m))

(define (remain-one? x m)
  (= (remainder x m) 1))

(define (nontrival x n m)
  (and (not (remain-oneor? n m)) (remain-one? x m))
  )

(define (mulcheck-inner x n m)
  (cond
    ((nontrival x n m) 0)
    (else x)
    ))

(define (square-remainder n m)
  (remainder (square n) m))

(define (mulcheck n m)
  (mulcheck-inner (square-remainder n m) n m)
  )


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mulcheck (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (expmod-test-overall base exp m)
  ; check if k reach (n-1),if remainder p is 1 as fermat Theorem
  (if (= (remainder (expmod base exp m) m) 1)
      true
      false))


(define (mr-prime? n times)
    (cond
        ;if it run to the last test
        ((> times (/ (- n 1) 2))
         (expmod-test-overall (+ (random (- n 3)) 2) (- n 1) n))
        ; if it pass the test
        ((expmod-test-overall (+ (random (- n 3)) 2) (- n 1) n)
         (mr-prime? n (+ times 1)))
        (else false)
    ))


(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((mr-prime? n 0) (newline)(display n)(report-prime (- (runtime) start-time)))
        (else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (serch-for-prime n k)
  (timed-prime-test n)
  (cond  ((< k 1) (newline) (display " **************************** "))
          ((mr-prime? n 0) (serch-for-prime (+ n 1) (- k 1)))
          (else (serch-for-prime (+ n 1) k))))

(serch-for-prime 6601 1)
(serch-for-prime 6600 10)
(serch-for-prime 66000 10)
(serch-for-prime 660000 10)


6607 *** 13
 ****************************
6607 *** 12
6619 *** 11
6637 *** 12
6653 *** 14
6659 *** 12
6661 *** 11
6673 *** 13
6679 *** 11
6689 *** 13
6691 *** 11
 ****************************
66029 *** 159
66037 *** 155
66041 *** 160
66047 *** 170
66067 *** 174
66071 *** 160
66083 *** 153
66089 *** 154
66103 *** 152
66107 *** 156
 ****************************
660001 *** 1765
660013 *** 1845
660029 *** 1906
660047 *** 1883
660053 *** 1833
660061 *** 1912
660067 *** 1855
660071 *** 1904
660073 *** 1782
660097 *** 1767
 ****************************