(load "/home/soulomoon/Documents/git/SICP/Chapter2/source.scm")
; Exercise 2.82: Show how to generalize apply-generic to handle coercion in the general case of multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on. Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.)



(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
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
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
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
  (define scheme-number->rational 
    (lambda (x) (make-rational x 1))
  )
  (define rational->complex
    (lambda (x) (make-complex-from-real-imag  (div (numer x) (denom x)) 0))
  )
  (define scheme-number->complex
    (lambda (x) (rational->complex (scheme-number->rational x)))
  )
  (put-coercion 'scheme-number 'rational
                scheme-number->rational)
  (put-coercion 'scheme-number 'complex
                scheme-number->complex)
  (put-coercion 'rational 'complex
                rational->complex)
                
'install_transform_done)
(install_transform_package)

(define a (make-complex-from-real-imag 0 0))
(define b (make-complex-from-real-imag 24 5))
(define c (make-rational 0 3))
(define d (make-rational 1 3))
(numer c)
(display (add c 1 d))(newline)
(display (add c d 2))(newline)
(display (add d 1 a))(newline)
(display (add d 1 b))(newline)