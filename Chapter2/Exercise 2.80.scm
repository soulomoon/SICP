; Exercise 2.80: Define a generic predicate =zero? that tests if its argument is zero, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.
; Exercise 2.79: Define a generic equality predicate equ? that tests the equality of two numbers, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.
(define (install-eq-package)
(define (attach-tag type-tag contents)
    (cond 
        ((or (symbol? contents) (numbers? contents)) contents) 
        (else (cons type-tag contents)) 
    )
)

(define (type-tag datum)
    (cond
        ((symbol? datum) ('symbol))
        ((number? datum) ('number))
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: 
                  TYPE-TAG" datum)))
)

(define (contents datum)
    (cond 
        ((or (symbol? contents) (numbers? contents)) contents) 
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: 
                CONTENTS" datum)))
)

(define (install_=zero? x y)
    (let 
        (
            (typex (type-tag x))
            (typey (type-tag y))
        )
        (apply-generic typex x y)
    )
    (if (= typex typey)
        (apply-generic '=zero? x y)
        false
    )
)

(put '=zero? '(=zero? =zero?)
    install_=zero?
)
