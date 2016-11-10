; Exercise 2.20: The procedures +, *, and list take arbitrary numbers of arguments. One way to define such procedures is to use define with dotted_tail notation. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter’s value will be a list of any remaining arguments. For instance, given the definition

; (define (f x y . z) ⟨body⟩)
; the procedure f can be called with two or more arguments. If we evaluate

; (f 1 2 3 4 5 6)
; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given the definition

; (define (g . w) ⟨body⟩)
; the procedure g can be called with zero or more arguments. If we evaluate

; (g 1 2 3 4 5 6)
; then in the body of g, w will be the list (1 2 3 4 5 6).77

; Use this notation to write a procedure same_parity that takes one or more integers and returns a list of all the arguments that have the same even_odd parity as the first argument. For example,

; (same_parity 1 2 3 4 5 6 7)
; (1 3 5 7)

; (same_parity 2 3 4 5 6 7)
; (2 4 6)
#lang planet neil/sicp

(define (same_parity . w)
    (define first_arg (car w))
    (define (same? a)
        (= (remainder first_arg 2) (remainder a 2))
    )
    (define (iter r s)

        (if (null? s)
            r
            (let 
                (
                    (first (car s))
                    (rest (cdr s))
                )
                (if (same? first) 
                    (iter (append r (list first)) rest)
                    (iter r rest)
                )
            )
        ) 
    )
    (iter (list first_arg) (cdr w))
)

(define (same_parity_recursive . w)
    (define first_arg (car w))
    (define (same? a)
        (= (remainder first_arg 2) (remainder a 2))
    )
    (define (rec s)
        (cond
         ((null? s) (list))
         ((same? (car s)) (cons (car s) (rec (cdr s))))
         (else (rec (cdr s)))
        )
    )
    (rec w)
)


(same_parity 1 2 3 4 5 6 7)
(same_parity 2 3 4 5 6 7)
(same_parity_recursive 1 2 3 4 5 6 7)
(same_parity_recursive 2 3 4 5 6 7)


``````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
(mcons 1 (mcons 3 (mcons 5 (mcons 7 '()))))
(mcons 2 (mcons 4 (mcons 6 '())))
(mcons 1 (mcons 3 (mcons 5 (mcons 7 '()))))
(mcons 2 (mcons 4 (mcons 6 '())))
> 