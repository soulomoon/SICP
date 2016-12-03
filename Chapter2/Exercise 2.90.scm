; Exercise 2.90: Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials. One way to do this is to allow both kinds of term_list representations in our system. The situation is analogous to the complex_number example of 2.4, where we allowed both rectangular and polar representations. To do this we must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.
; Exercise 2.89: Define procedures that implement the term_list representation described above as appropriate for dense polynomials.

(load "/home/soulomoon/Documents/git/SICP/Chapter2/source.scm")

(define (install_term_package)
  (define (make_term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (tag x) (cons 'term x))
  (define (null_term? term)
    (null? term))
  (put 'null_term? '(term) null_term?)
  (put 'make 'term 
    (lambda (x y) (tag (make_term x y))))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
'install_term_package_done)
(install_term_package)
(define make_term
  (get 'make 'term))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))
(define (null_term? term)
  (apply-generic 'null_term? term))
  

(define (install_sparse_polynomial_package)
  (define (first_term term_list)
    (car term_list))
  (define (rest_terms term_list) 
    (cdr term_list))
  (define (empty_termlist? term_list) 
    (null? term_list))
  (define (adjoin_term term termlist)
            ; (newline)(display (termlist))
    (let 
        ((term_order (order term))
        (term_coeff (coeff term)))
      (cond 
        ((and (null_term? term) (= 0 term_coeff)) termlist)
        ((empty_termlist? termlist) (list term))
        (else 
        (let ((firstterm (first_term termlist))
              (rest (rest_terms termlist)))
          (let 
              ((list_order (order firstterm))
                (list_coeff (coeff firstterm)))
            (if (= term_order list_order)
                (cons 
                  (make_term 
                  term_order
                  (add term_coeff list_coeff))
                  rest)
                (cons firstterm (adjoin_term term rest)))))))))
  (define (make_sparse term_list)
    ; (display term_list)
    (define (iter result termlist)
      (if (empty_termlist? termlist)
          result
          (let ((first (first_term termlist))
                (rest (rest_terms termlist)))
            (iter (adjoin_term first result) rest))))
  ; (display term_list)(newline)
  (iter '() term_list))
  (define (tag x) 
    (cons 'sparse x))
  (put 'make 'sparse
    (lambda term_list (tag (make_sparse term_list))))
  (put 'take_terms '(sparse)
    (lambda (x) (cdr x)))
  (put 'empty_termlist? '(sparse)
    (lambda (term_list) (empty_termlist? term_list)))
  (put 'adjoin_term '(sparse)
    (lambda (term termlist) (tag (adjoin_term term termlist))))
  (put 'first_term '(sparse)
    first_term)
  (put 'rest_terms '(sparse)
    (lambda (x) (tag (rest_terms x))))
  'install_sparse_polynomial_package_done)
(install_sparse_polynomial_package)
(define (install_dense_polynomial_package)
  (define (first_term term_list) 
    (define max_order (apply max (map order term_list)))
    (define (iter rest_list)
      (if (= max_order (order (car rest_list)))
          (car rest_list)
          (iter (cdr rest_list))))
    (iter term_list))
  (define (rest_terms term_list) 
    (define first_term_order (order (first_term term_list)))
      (define (iter remain_list)
        (if (= first_term_order (order (car remain_list)))
            (cdr remain_list)
            (cons (car remain_list) (iter (cdr remain_list)))))
    (iter term_list))
  (define (empty_termlist? term_list) 
    (null? term_list))
  (define (adjoin_term term termlist)
    ; (display term)(newline)
    ; (display termlist)(newline)
    (let 
      ((term_order (order term))
      (term_coeff (coeff term)))
      (cond 
        ((null_term? term) termlist)
        ((empty_termlist? termlist) (list term))
        (else 
          (let ((first_term (first_term termlist))
                (rest (rest_terms termlist)))
            (let 
              ((list_order (order first_term))
                (list_coeff (coeff first_term)))
              (if (= term_order list_order)
                  (cons 
                    (make_term 
                      term_order
                      (+ term_coeff list_coeff))
                    rest)
                  (cons first_term (adjoin_term term rest)))))))))
  (define (make_dense termlist)
    (define (get_term term_list n)
      (let ((first (first_term term_list)))
        (if (= (order first) n)
            first
            (make_term n 0))))
    (define (first_term_order_equals_n? term_list n)
      (let ((first_term_order (order (first_term term_list))))
          (= first_term_order n)
      )
    )
    (define (iter result term_list n)
      (if (empty_termlist? term_list)
          result
          (if (first_term_order_equals_n? term_list n)
              (iter (adjoin_term (get_term term_list n) result) (rest_terms term_list) n)
              (iter (adjoin_term (get_term term_list n) result) term_list (- n 1)))))
    ; (display termlist)
    (if (empty_termlist? termlist)
        termlist
        (iter nil termlist (order (first_term termlist)))))
  (define (tag x) (cons 'dense x))
  (put 'make 'dense
    (lambda term_list (tag (make_dense term_list))))
  (put 'take_terms '(dense)
    (lambda (x) (cdr x)))
  (put 'empty_termlist? '(dense)
    (lambda (term_list) (empty_termlist? term_list)))
  (put 'adjoin_term '(dense)
    (lambda (term termlist) (tag (adjoin_term term termlist))))
  (put 'first_term '(dense)
    first_term)
  (put 'rest_terms '(dense)
    (lambda (x) (tag (rest_terms x))))
  'install_dense_polynomial_package_done)
(install_dense_polynomial_package)
(define make_sparse
  (get 'make 'sparse))
(define make_dense
  (get 'make 'dense))
(define take_terms
  (lambda (term term_list) (apply-generic 'take_terms term term_list)))
(define adjoin_term
  (lambda (term term_list) (apply-generic 'adjoin_term term term_list)))
(define first_term
  (lambda (term_list) (apply-generic 'first_term term_list)))
(define rest_terms
  (lambda (term_list) (apply-generic 'rest_terms term_list)))
(define (empty_termlist? term_list) (apply-generic 'empty_termlist? term_list))

(define (install_polynomial_package)
  (define (the_empty_termlist)
    (make_dense))
  (define (negation_termlist term_list)
    (define (negation_termlist_inner termlist)
      (define (negation_term term)
        (make_term (order term) (negation (coeff term))))
      (if (empty_termlist? termlist)
        (list)
        (adjoin_term (negation_term (first_term termlist)) (negation_termlist_inner (rest_terms termlist)))))
  (apply (get 'make (type-tag term_list)) (negation_termlist_inner term_list)))
      
  (define (add_terms X Y)
    (define (add_terms_inner L1 L2)
      (cond ((empty_termlist? L1) (cdr L2))
            ((empty_termlist? L2) (cdr L1))
            (else
            (let ((t1 (first_term L1)) 
                  (t2 (first_term L2)))
              (cond ((> (order t1) (order t2))
                      (adjoin_term
                      t1 
                      (add_terms_inner (rest_terms L1) 
                                  L2)))
                    ((< (order t1) (order t2))
                      (adjoin_term
                      t2 
                      (add_terms_inner 
                        L1 
                        (rest_terms L2))))
                    (else
                      (adjoin_term
                      (make_term 
                        (order t1)
                        (add (coeff t1) 
                            (coeff t2)))
                      (add_terms_inner 
                        (rest_terms L1)
                        (rest_terms L2)))))))))
    ; (display (add_terms_inner X Y))(newline)(newline)
    (apply (get 'make (type-tag X)) (add_terms_inner X Y)))
  (define (mul_terms L1 L2)
    (if (empty_termlist? L1)
        (the_empty_termlist)
        (add_terms 
          (apply make_sparse (mul_term_by_all_terms 
            (first_term L1) L2))
          (mul_terms (rest_terms L1) L2))))
  (define (mul_term_by_all_terms t1 L)
    ; (display (empty_termlist? L))(newline)
    (if (empty_termlist? L)
        nil
        (let ((t2 (first_term L)))
          (adjoin_term
          ; (display (rest_terms L))(newline)
          (make_term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
          (mul_term_by_all_terms 
            t1 
            (rest_terms L))))))
  ;; internal procedures
  ;; representation of poly
  (define (make_poly variable term_list)
    (cons variable term_list))
  (define (variable p) (car p))
  (define (term_list p) (cdr p))
  (define (same_variable? v1 v2)
    (define (variable? x) (symbol? x))
    (and (variable? v1)
        (variable? v2)
        (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin_term term term_list)
    ;   (display term)(newline)
    ; (display term_list)(newline)
    (if (=zero? (coeff term))
        term_list
        (cons term term_list)))
  (define (negation_poly p)
    (make_poly (variable p) (negation_termlist (term_list p)))
  )      
  (define (add_poly p1 p2)
    (if (same_variable? (variable p1) 
                        (variable p2))
        (make_poly 
        (variable p1)
        (add_terms (term_list p1)
                    (term_list p2)))
        (error "Polys not in same var: 
                ADD_POLY"
              (list p1 p2))))

  (define (mul_poly p1 p2)
    (if (same_variable? (variable p1) 
                        (variable p2))
        (make_poly 
        (variable p1)
        (mul_terms (term_list p1)
                    (term_list p2)))
        (error "Polys not in same var: 
                MUL_POLY"
              (list p1 p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'negation '(polynomial)
    (lambda (x)
      (tag (negation_poly x))))
  (put 'sub '(polynomial polynomial)
    (lambda (p1 p2) 
     (tag (add_poly p1 (negation_poly p2)))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add_poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul_poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make_poly var terms))))
  'install_polynomial_package_done)
(install_polynomial_package)
(define make-polynomial
  (get 'make 'polynomial))

(define a (make_term 1 2))
(define b (make_term 2 2))
(define c (make_term 3 3))
(define e (make_term 4 0))
(display a)(newline)
(define aterm (make_sparse a b c))
(define bterm (make_dense a b c))
(display aterm)(newline)
(display bterm)(newline)

(define pol (make-polynomial 'x aterm))
(define pol1 (make-polynomial 'x bterm))
(define pol2 (make-polynomial 'x bterm))
(display (add pol pol2))(newline)
(display (add pol2 pol))(newline)
(display (add pol2 pol2))(newline)
(display (sub pol pol2))(newline)
(display (sub pol2 pol))(newline)
(display (sub pol2 pol2))(newline)
(display (mul pol2 pol2))(newline)

(display (add 1 pol2))(newline)
(display (sub 1 pol2))(newline)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'install_term_package_done
; 'install_sparse_polynomial_package_done
; 'install_dense_polynomial_package_done
; 'install_polynomial_package_done
; (term 1 2)
; (sparse (term 1 2) (term 2 2) (term 3 3))
; (dense (term 3 3) (term 2 2) (term 1 2))
; (polynomial x sparse (term 3 6) (term 2 4) (term 1 4))
; (polynomial x dense (term 3 6) (term 2 4) (term 1 4))
; (polynomial x dense (term 3 6) (term 2 4) (term 1 4))
; (polynomial x sparse (term 3 0) (term 2 0))
; (polynomial x dense (term 3 0) (term 2 0))
; (polynomial x dense)
; (polynomial x sparse (term 6 9) (term 5 12) (term 4 16) (term 3 8) (term 2 4))
; (polynomial x sparse (term 3 3) (term 2 2) (term 1 2) (term 0 1))
; (polynomial x sparse (term 3 -3) (term 2 -2) (term 1 -2) (term 0 1))
; > 