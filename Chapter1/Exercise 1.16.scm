; Exercise 1.16: Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt. (Hint: Using the observation that (bn/2)2=(b2)n/2(bn/2)2=(b2)n/2, keep, along with the exponent nn and the base bb, an additional state variable aa, and define the state transformation in such a way that the product abnabn is unchanged from state to state. At the beginning of the process aa is taken to be 1, and the answer is given by the value of aa at the end of the process. In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)
#lang planet neil/sicp
(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))
(define (expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (expt-iter (square b) (/ counter 2) product))
        (else (expt-iter b
                 (- counter 1)
                 (* b product)))))


(define (fast-expt b n)
  (expt-iter b n 1))
(fast-expt 2 100)