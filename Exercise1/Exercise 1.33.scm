; Exercise 1.33: You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:

; the sum of the squares of the prime numbers in the interval aa to bb (assuming that you have a prime? predicate already written)
; the product of all the positive integers less than nn that are relatively prime to nn (i.e., all positive integers i<ni<n such that GCD(i,n)=1GCD(i,n)=1).
#lang planet neil/sicp
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

(define (prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (prime? n (- times 1)))
        (else false)))

(define (ifprime n)
    (prime? n 10)
)

(define (accumulate-iter filter combiner null-value term a next b)
    (define (iter a result)
        (if 
            (> a b)
            result
            (if (filter a) 
                (iter (next a) (combiner result (term a))) 
                (iter (next a) result )
            ) 
            
        )
    )
    (iter a null-value)
)

(define (filter-sum filter term a next b)
    (accumulate-iter filter + 0 term a next b)
)


(define (prime-squaresum a b) 
    (filter-sum ifprime square a inc b)
)


(define (filter-product filter term a next b)
    (accumulate-iter filter * 1 term a next b)
)


(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)

(define (coprime? a b)
    (if (= (gcd a b) 1)
        true
        false
    )
)

(define (coprime-product n)
    (define (coprime?-filter x)
        (coprime? x n)
    )
    (define (term x)
        x
    )
    (filter-product coprime?-filter term 1 inc n)
)

(coprime-product 10)


(prime-squaresum 2 5)

189
38