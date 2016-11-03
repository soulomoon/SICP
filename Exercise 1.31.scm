; Exercise 1.31:

; The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to ππ using the formula52
; π4=2⋅4⋅4⋅6⋅6⋅8⋅⋯3⋅3⋅5⋅5⋅7⋅7⋅⋯.
; π4=2⋅4⋅4⋅6⋅6⋅8⋅⋯3⋅3⋅5⋅5⋅7⋅7⋅⋯.
; If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
#lang planet neil/sicp
(define (factorial f a next b)
    (
        if
            (> a b)
            1
            (* (f a) (factorial f (next a) next b))
    )
)



(define (even? x) 
    (if 
        (= (remainder x 2) 0)
        true
        false)
    )

(define (pi-factorial n)
    (define (f-pi n)
        (if (even? n) 
            (/ (+ 2 n) (+ 1 n))    
            (/ (+ 1 n) (+ 2 n)) 
    ))
    
    (define (next x) (+ 1 x))
    
    (* (factorial f-pi 1.0 next n) 4)
)

(define (itrfactorial f a next b)
    (define (iter a result)
        (if
            (> a b)
            result
            (iter (next a) (* result (f a)))
        )
    )
    (iter a 1)
)

(define (iterpi-factorial n)
    (define (f-pi n)
        (if (even? n) 
            (/ (+ 2 n) (+ 1 n))    
            (/ (+ 1 n) (+ 2 n)) 
    ))
    
    (define (next x) (+ 1 x))
    
    (* (itrfactorial f-pi 1.0 next n) 4)
)
(pi-factorial 10)
(pi-factorial 100)
(pi-factorial 1000)
(pi-factorial 10000)
(pi-factorial 100000)
(pi-factorial 1000000)

(display "*******************")
(newline)

(iterpi-factorial 10)
(iterpi-factorial 100)
(iterpi-factorial 1000)
(iterpi-factorial 10000)
(iterpi-factorial 100000)
(iterpi-factorial 1000000)

3.275101041334807
3.1570301764551645
3.143160705532257
3.1417497057379635
3.1416083612780903
3.1415942243828017
*******************
3.2751010413348065
3.1570301764551654
3.1431607055322552
3.1417497057380084
3.141608361277941
3.141594224382854

