; Exercise 2.34: Evaluating a polynomial in xx at a given value of xx can be formulated as an accumulation. We evaluate the polynomial
; anxn+an−1xn−1+⋯+a1x+a0
; anxn+an−1xn−1+⋯+a1x+a0
; using a well-known algorithm called Horner’s rule, which structures the computation as
; (…(anx+an−1)x+⋯+a1)x+a0.
; (…(anx+an−1)x+⋯+a1)x+a0.
; In other words, we start with anan, multiply by xx, add an−1an−1, multiply by xx, and so on, until we reach a0a0.82

; Fill in the following template to produce a procedure that evaluates a polynomial using Horner’s rule. Assume that the coefficients of the polynomial are arranged in a sequence, from a0a0 through anan.

; (define 
;   (horner-eval x coefficient-sequence)
;   (accumulate 
;    (lambda (this-coeff higher-terms)
;      ⟨??⟩)
;    0
;    coefficient-sequence))
; For example, to compute 1+3x+5x3+x51+3x+5x3+x5 at x=2x=2 you would evaluate

; (horner-eval 2 (list 1 3 0 5 0 1))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))


(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
(accumulate + 0 (list 1 6 40 32))

Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
79
79
> 