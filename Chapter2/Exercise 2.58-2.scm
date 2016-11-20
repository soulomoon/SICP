    ; Exercise 2.58: Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

    ; Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.
    ; The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?
    (define (deriv exp var)
    (cond ((number? exp) 0)
            ((variable? exp)
                (if (same-variable? exp var) 1 0))
            ((sum? exp)
                (make-sum (deriv (addend exp) var)
                        (deriv (augend exp) var)))

            ((product? exp)
                (make-sum
                    (make-product 
                        (multiplier exp)
                        (deriv (multiplicand exp) var))
                    (make-product 
                        (deriv (multiplier exp) var)
                        (multiplicand exp)))
            )

            ((exponentiation? exp)
                (make-product
                    (make-product
                        (exponent exp)
                        (make-exponentiation 
                            (base exp)
                            (- (exponent exp) 1)
                        )
                    )
                    (deriv
                        (base exp)
                        var
                    )
                )
            )
            (else (error "unknown expression 
                        type: DERIV" exp))))


    ; The variables are symbols. They are identified by the primitive predicate symbol?:
    (define (variable? x) (symbol? x))
    ; Two variables are the same if the symbols representing them are eq?:
    (define (same-variable? v1 v2)
    (and (variable? v1)
        (variable? v2)
        (eq? v1 v2)))
    ; Sums and products are constructed as lists:
    (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) 
            (+ a1 a2))
            (else (list a1 '+ a2))))


    (define (=number? exp num)
    (and (number? exp) (= exp num)))

    (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
                (=number? m2 0)) 
            0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) 
            (* m1 m2))
            (else (list m1 '* m2))))
    ; A sum is a list whose first element is the symbol +:
    (define (sum? x)
    (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))
    ; The addend is the second item of the sum list:
    (define (addend s) 
        (test car s)
    )
    ; The augend is the third item of the sum list:
    (define (augend s) 
        (test cddr s)
    )
    ; A product is a list whose first element is the symbol *:
    (define (product? x)
    (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
    ; The multiplier is the second item of the product list:
    (define (multiplier p) 
        (test car p)
    )
    ; The multiplicand is the third item of the product list:
    (define (multiplicand p)
        (test cddr p)
    )
    (define (test location var)
        (define (simplify x)
            (cond
                ((sum? x) (make-sum (addend x) (augend x)))
                ((product? x) (make-product (multiplier x) (multiplicand x)))
                (else 
                    (if (pair? x)
                        (if (pair? (car x)) (simplify (car x)) (car x))
                        x
                    )
                )
            )
        )
        (let ((item (location var)))
            (simplify item)
        )
    )


    (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

    (define (base s) (cadr s))
    (define (exponent s) (caddr s))

    (define (make-exponentiation base exponent)
    (cond ((=number? base 0) 0)
            ((=number? exponent 0) 1)
            ((=number? exponent 1) base)
            (else (list '** base exponent))))




    (display (deriv '(x + 3) 'x)) (newline)
    ; (+ 1 0)

    (display (deriv '((10 + 4 + 1) * x) 'x)) (newline)
    ; (+ (* x 0) (* 1 y))

    (display (deriv '(x * y * (x + 3 + 10 + 2)) 'x)) (newline)
    ; (+ (* (* x y) (+ 1 0))
    ;    (* (+ (* x 0) (* 1 y))
    ;       (+  x 3)))

    ; (display (deriv '(** x 3) 'x)) (newline)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 1
; y
; ((x * y) + (y * (3 + 10 + x)))
; > 