; Exercise 2.96:

; Implement the procedure pseudoremainder-terms, which is just like remainder-terms except that it multiplies the dividend by the integerizing factor described above before calling div-terms. Modify gcd-terms to use pseudoremainder-terms, and verify that greatest-common-divisor now produces an answer with integer coefficients on the example in Exercise 2.95.
; The GCD now has integer coefficients, but they are larger than those of P1P1. Modify gcd-terms so that it removes common factors from the coefficients of the answer by dividing all the coefficients by their (integer) greatest common divisor.
; (load "/home/soulomoon/Documents/git/SICP/Chapter2/source.scm")
(load "C:/git/SICP/Chapter2/source.scm")
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
    (lambda (x) (make-polynomial 'x (list (make_term 0 (real-part x)))))
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

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
      (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

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

      
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (value n)
  (apply-generic 'value n)
)
 
(define (numer z) 
  (apply-generic 'numer z))
(define (denom z) 
  (apply-generic 'denom z))

(define (install-polynomial-package)

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
    (null? term-list))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (negation-termlist termlist)
    (define (negation-term term)
      (make-term (order term) (negation (coeff term))))
    (if (empty-termlist? termlist)
      termlist
      (adjoin-term (negation-term (first-term termlist)) (negation-termlist (rest-terms termlist)))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negation-termlist L2)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
          (let ((t1 (first-term L1)) 
                (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                    (adjoin-term
                    t1 
                    (add-terms (rest-terms L1) 
                                L2)))
                  ((< (order t1) (order t2))
                    (adjoin-term
                    t2 
                    (add-terms 
                      L1 
                      (rest-terms L2))))
                  (else
                    (adjoin-term
                    (make-term 
                      (order t1)
                      (add (coeff t1) 
                          (coeff t2)))
                    (add-terms 
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms 
          (mul-term-by-all-terms 
            (first-term L1) L2)
          (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term 
              (+ (order t1) (order t2))
              (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms 
              t1 
              (rest-terms L))))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) 
              (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
              ; (display L1)(newline )
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) 
                                (coeff t2)))
                    (new-o (- (order t1) 
                              (order t2))))
                (let ((rest-of-result
                      (div-terms (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2)) L2)))
                  (list (adjoin-term (make-term new-o new-c)
                              (car rest-of-result))
                        (cadr rest-of-result))))))))
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable (list term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))
  (define (same-variable? v1 v2)
    (define (variable? x) (symbol? x))
    (and (variable? v1)
        (variable? v2)
        (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (negation-poly p)
    (make-poly (variable p) (negation-termlist (term-list p)))
  )      
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
          (variable p1)
          (add-terms (term-list p1)
                      (term-list p2)))
        (contents (add_expand p1 p2))))
  (define (max_order term_list) (apply max (map order term_list)))
  
  
  (define (add_expand p1 p2)
    (let (
      (var1 (variable p1))
      (var2 (variable p2))
      (max1 (max_order (term-list p1)))
      (max2 (max_order (term-list p2)))
      (merge (lambda (lp1 lp2)
        (let ((addent (tag lp1))
              (adder (make-polynomial (variable lp1)
                                          (list (make-term 0 (tag lp2))))))
        (make-poly
          (variable lp1)
          (contents (add adder addent)))))))
    (if (> max1 max2)
          (merge p1 p2)
          (merge p2 p1))))

  (define (mul_expand p1 p2)
    (let ((merge (lambda (lp1 lp2) 
                         (let ((t1 (make-term 0 (tag lp1))))
                              (cons (variable lp2) (mul-term-by-all-terms t1 (term-list lp2))))))
          (max1 (max_order (term-list p1)))
          (max2 (max_order (term-list p2))))
         (if (< max1 max2)
             (merge p1 p2)
             (merge p2 p1))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly
          (variable p1)
          (mul-terms (term-list p1)
                      (term-list p2)))
        (mul_expand p1 p2)))
    
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (append
          (make-poly 
            (variable p1)
            (car (div-terms (term-list p1)
                        (term-list p2))))
          (cdr (div-terms (term-list p1)
                      (term-list p2))))
            (error "Polys not in same var: 
                    div-POLY"
                  (list p1 p2))
          ))

    (define (get_remain_list pol)
      (let ((remain (cddr pol)))
           (if (null? remain)
               (list (make-term 0 0))
               (car remain))))

  (define (remainder-terms a b)
    (let (
      (get_remain_list_inner (lambda (termlist_remain) (cadr termlist_remain)))
      (maxa (max_order a))
      (maxb (max_order b)))
      ; (display (get_remain_list_inner (div-terms a b)))(newline)
      (if (< maxa maxb)
          (get_remain_list_inner (div-terms b a))
          (get_remain_list_inner (div-terms a b))
      )
  ))
  (define (max_order_coeff term_list)
    (define (iter m rest)
      (if (empty-termlist? rest)
          (error "max_order_coeff fail!")
          (let ((first_order (order (first-term rest)))
              (first_coeff (coeff (first-term rest))))
              (if (= first_order m)
                  first_coeff
                  (iter m (rest-terms rest))))))
    (let ((max_o (max_order term_list)))
         (iter max_o term_list)))
  
  (define (pseudoremainder-terms a b)
    (let (
      (get_remain_list_inner (lambda (termlist_remain) (cadr termlist_remain)))
      (maxa (max_order a))
      (maxb (max_order b))
      (scale_factor (lambda (x maxp maxm)
                            (let ((base (max_order_coeff x))
                                  (power (- (+ 1 maxp) maxm)))
                                 (make_term 0 (expt base power))))))
      ; (display (get_remain_list_inner (div-terms a b)))(newline)
      (if (< maxa maxb)
          (get_remain_list_inner (div-terms (mul-term-by-all-terms (scale_factor a maxb maxa) b ) a))
          (get_remain_list_inner (div-terms (mul-term-by-all-terms (scale_factor b maxa maxb) a ) b))
      )
  ))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (let ((max_coeff_divisor (apply gcd (map coeff a))))
              (map (lambda (x) (make-term (order x) 
                                          (div (coeff x) 
                                              max_coeff_divisor))) 
                   a))
        (gcd-terms b (pseudoremainder-terms a b))))
  ; (display (div-terms '((4 1) (3 -1) (2 -2) (1 2)) '((3 1) (1 -1))))(newline)
  (define (greatest-common-divisor p1 p2)
    (let ((var1 (variable p1))
          (var2 (variable p2)))
          (if (same-variable? var1 var2)
              (make-poly var1 (gcd-terms (term-list p1) (term-list p2)))
              (error "Polys not in same var: 
                div-POLY"
                (list p1 p2))
          )
    )
    
  )

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? '(polynomial)
    (lambda (x) (empty-termlist? (term-list x))))
  (put 'negation '(polynomial)
    (lambda (x)
      (tag (negation-poly x))))
  (put 'make 'term
    make-term)
  (put 'sub '(polynomial polynomial)
    (lambda (p1 p2) 
      (tag (add-poly p1 (negation-poly p2))))
  )
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (greatest-common-divisor p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (div-poly p1 p2))))
  (put 'get_remain_list '(polynomial)
       (lambda (p) 
         (get_remain_list p)))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  'install-polynomial-package-done)
(install-polynomial-package)


(define (make_term order coeff)
  ((get 'make 'term) order coeff))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (get_remain_list n)
  (apply-generic 'get_remain_list n)
)

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b)
)


(define p1 
  (make-polynomial 
   'x '((2 1) (1 -2) (0 1))))

(define p2 
  (make-polynomial 
   'x '((2 11) (0 7))))

(define p3 
  (make-polynomial 
   'x '((1 13) (0 5))))
; (display p1)
(define Q1 (mul p1 p2))
(define Q2 (mul p1 p3))
(define Q3 (greatest-common-divisor Q1 Q2))
(display p1)(newline)
(display Q2)(newline)
(display Q2)(newline)
(display Q3)(newline)
(display p1)(newline)
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'install_transform_done
; 'rational-package-done
; 'install-polynomial-package-done
; (polynomial x ((2 1) (1 -2) (0 1)))
; (polynomial x ((3 13) (2 -21) (1 3) (0 5)))
; (polynomial x ((3 13) (2 -21) (1 3) (0 5)))
; (polynomial x ((2 1) (1 -2) (0 1)))
; (polynomial x ((2 1) (1 -2) (0 1)))
; > 