; Exercise 2.89: Define procedures that implement the term-list representation described above as appropriate for dense polynomials.

; Exercise 2.88: Extend the polynomial system to include subtraction of polynomials. (Hint: You may find it helpful to define a generic negation operation.)
(load "/home/soulomoon/Documents/git/SICP/Chapter2/source.scm")

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
  (define (make_term_list n)
    (if (= n -1)
        (the-empty-termlist)
        (cons (make_term n 0) (make_term_list (- n 1)))))
  
  (define (add_terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
          (let ((t1 (order (first-term L1))) 
                (t2 (order (first-term L2))))
               (add-terms (make_term_list (max t1 t2))
                          (add-terms L1  L2))))))
  (define (mul_terms L1 L2)
    (add_terms (mul-terms L1 L2) (the-empty-termlist)))

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
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
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
        (add_terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
                ADD-POLY"
              (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
        (variable p1)
        (mul_terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
                MUL-POLY"
              (list p1 p2))))

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
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  'install-polynomial-package-done)
(install-polynomial-package)


(define (make_term order coeff)
  ((get 'make 'term) order coeff))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define a (make_term 1 2))
(define b (make_term 2 2))
(define c (make_term 3 3))
(define e (make_term 4 0))

(define pol0 (make-polynomial 'x (list e)))
(define pol (make-polynomial 'x (list c b a)))
(define d (make_term 1 pol))
(define pol2 (make-polynomial 'x (list e c b d)))
(display (add pol pol2))(newline)
(display (add pol2 pol))(newline)
(display (add pol2 pol2))(newline)
(display (sub pol pol2))(newline)
(display (sub pol2 pol))(newline)
(display (sub pol2 pol2))(newline)

(display (sub 1 pol2))(newline)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'install_transform_done
; 'install-polynomial-package-done
; (polynomial x (3 6) (2 4) (1 (polynomial x (3 3) (2 2) (1 2) (0 2))) (0 0))
; (polynomial x (3 6) (2 4) (1 (polynomial x (3 3) (2 2) (1 2) (0 2))) (0 0))
; (polynomial x (3 6) (2 4) (1 (polynomial x (3 6) (2 4) (1 4) (0 0))) (0 0))
; (polynomial x (1 (polynomial x (3 -3) (2 -2) (1 -2) (0 2))) (0 0))
; (polynomial x (1 (polynomial x (3 3) (2 2) (1 2) (0 -2))) (0 0))
; (polynomial x (1 (polynomial x (3 0) (2 0) (1 0) (0 0))) (0 0))
; (polynomial x (3 -3) (2 -2) (1 (polynomial x (3 -3) (2 -2) (1 -2))) (0 1))
; > 