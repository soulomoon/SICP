; Exercise 1.32.

; Show that sum and product (exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:


; (accumulate combiner null-value term a next b)
; Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate. Write two procedures, one that generates a recursive process and one iterative.

; Exercise 1.31:

; The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to ππ using the formula52
; π4=2⋅4⋅4⋅6⋅6⋅8⋅⋯3⋅3⋅5⋅5⋅7⋅7⋅⋯.
; π4=2⋅4⋅4⋅6⋅6⋅8⋅⋯3⋅3⋅5⋅5⋅7⋅7⋅⋯.
; If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
#lang planet neil/sicp
(define (accumulate combiner null-value term a next b)
    (if
        (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))
    )
)

(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
        (if 
            (> a b)
            result
            (iter (next a) (combiner result (term a)))
        )
    )
    (iter a null-value)
)

(define (sum term a next b)
    (accumulate-iter + 0 term a next b)
)

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)


