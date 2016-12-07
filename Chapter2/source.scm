; #lang racket
; Exercise 2.79: Define a generic equality predicate equ? that tests the equality of two numbers, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define level-table (make-table))
(define get-level (level-table 'lookup-proc))
(define put-level (level-table 'insert-proc!))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
    (cond 
        ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))
    )
)

(define (contents datum)
    (cond 
        ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))
    )
)

(define (install-integer-package)
  (define (negation x)
    (- 0 x)
  )
  (define (tag x)
    (cons 'integer x))
  (put 'negation '(integer)
    (lambda (x) (tag (negation x)))
  )
  (put '=zero? '(integer)
       (lambda (x) (tag (= x 0))))
  (put 'value '(integer)
    (lambda (x) x)
  )
  (put 'equ? '(integer integer)
       (lambda (x y) (tag (round (= x y)))))
  (put 'add '(integer integer)
       (lambda (x y) (tag (round (+ x y)))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (round (- x y)))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (round (* x y)))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (round (/ x y)))))
  (put 'make 'integer
       (lambda (x) (tag (round x))))
  
  'integer-package-done)
(install-integer-package)

(define (install-scheme-number-package)
  (define (negation x)
    (- 0 x)
  )
  (define (tag x)
    x)
  (put 'negation '(scheme-number)
    (lambda (x) (tag (negation x)))
  )
  (put '=zero? '(scheme-number)
       (lambda (x) (tag (= x 0))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'scheme-number-package-done)
(install-scheme-number-package)


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (equ? x y)
    (if (and (= (numer x) (numer y)) (= (denom x) (denom y)))
        true
        false
    )
  )
  (define (rational_zero? x)
    (= 0 (numer x))
  )
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'negation '(rational)
    (lambda (x) (tag (make-rat (negation (numer x)) (denom x))))
  )
  (put '=zero? '(rational) rational_zero?)

  (put 'equ? '(rational rational)
    equ?
  )
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  'rational-package-done)
(install-rational-package)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'negation '(rectangular)
    (lambda (x) 
      (tag 
        (make-from-real-imag 
          (negation (real-part x) 
          (negation (imag-part x)))))))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'rectangular-package-done)
(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'negation '(polar)
    (lambda (x) (tag (make-from-mag-ang (magnitude x) (negation (angle x)))))
  )
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'polar-package-done)
(install-polar-package)

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (if (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))
        true
        false
    )
  )
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put '=zero? '(complex)
       (lambda (z1) 
         (and (= (real-part z1) 0) (= (imag-part z1) 0))))
  (put 'negation '(complex)
    (lambda (x) (tag (negation x)))
  )
  (put 'equ? '(complex complex)
    equ?)

  (put 'real-part '(complex)
    (lambda (x) (real-part x))
  )
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'complex-package-done)

(install-complex-package)


(define (install-zero?-package)
    (define (ordinary_zero? x)
        (= x 0)
    )

    (define (complex_zero? x)
        (= x 0)
    )
    (put 'zero '(scheme-number) ordinary_zero?)
    (put 'zero '(complex) complex_zero?)
)

; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (error
;             "No method for these types: 
;              APPLY-GENERIC"
;             (list op type-tags))))))

(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))

(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (value n)
  (apply-generic 'value n)
)
 
(define (numer z) 
  (apply-generic 'numer z))
(define (denom z) 
  (apply-generic 'denom z))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (=zero? x) 
  (apply-generic '=zero? x))

(define (equ? x y) (apply-generic 'equ? x y))
(define (negation x) (apply-generic 'negation x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
              result
          )
          (cond 
              ((> (length args) 2) 
               (apply apply-generic 
                      (cons op 
                            (list (car args) 
                                  (apply apply-generic 
                                         (cons op 
                                               (cdr args)))) )))
              ((= (length args) 2)
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
    
                  (let ((level1 (get-level type1 'level))
                        (level2 (get-level type2 'level)))
                        (cond 
                              ((< level1 level2) 
                                (apply-generic 
                                  op (raise a1) a2))
                              ((> level1 level2) 
                                (apply-generic 
                                  op a1 (raise a2)))
                              (else
                              (error 
                                "No method for 
                                these types"
                                (list 
                                op 
                                type-tags)))))))
              (else (error 
               "No method for these types"
               (list op type-tags))))))))


(define (install_transform_package)
  ; raise
  (define integer->rational
    (lambda (x) (make-rational (value x) 1))
  )
  (define rational->scheme-number
    (lambda (x) (make-scheme-number (/ (numer x) (denom x))))
  )
  (define scheme-number->complex
    (lambda (x) (make-complex-from-real-imag x 0))
  )
  (define complex->polynomial
    (lambda (x) (make-polynomial 'x (make_sparse (make_term 0 (real-part x)))))
  )
  
  ; project 
  (define complex->scheme-number
    (lambda (x) (make-scheme-number (real-part x)))
  )
  (define scheme-number->rational
    (lambda (x) (make-rational (round x) 1))
  )
  (define rational->integer
    (lambda (x) (make-integer (rational->scheme-number x)))
  )

  ; raise
  (put-coercion 'integer 'raise
                integer->rational)
  (put-coercion 'scheme-number 'raise
                scheme-number->complex)
  (put-coercion 'rational 'raise
                rational->scheme-number)
  (put-coercion 'complex 'raise
                complex->polynomial)

  ; project
  (put-coercion 'complex 'project complex->scheme-number)
  (put-coercion 'scheme-number 'project scheme-number->rational)
  (put-coercion 'rational 'project rational->integer)


  (put-level 'integer 'level 1)
  (put-level 'rational 'level 2)
  (put-level 'scheme-number 'level 3)
  (put-level 'complex 'level 4)
  (put-level 'polynomial 'level 5)
                
'install_transform_done)
(install_transform_package)



(define (project x)
  (let ((lower (get-coercion (type-tag x) 'project)))
    (if lower
        (lower x)
        false)))

(define (drop x)
  (let ((low (project x)))
    (if (and low (equ? (raise low) x))
        (drop (project x))
        x
    )
  )
)
(define (raise x)
  (let ((raise (get-coercion (type-tag x) 'raise)))
    (if raise
        (raise x)
        (error 
               "No raise for this types"
               (type-tag x)))))