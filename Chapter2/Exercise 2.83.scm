(load "/home/soulomoon/Documents/git/SICP/Chapter2/source.scm")
; Exercise 2.83: Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in Figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure that raises objects of that type one level in the tower. Show how to install a generic raise operation that will work for each type (except complex).


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

  (put-coercion 'scheme-number 'raise
                scheme-number->rational)
  (put-coercion 'rational 'raise
                rational->complex)
                
'install_transform_done)
(install_transform_package)


(define (raise x)
  (let ((raise (get-coercion (type-tag x) 'raise)))
    (if raise
        (raise x)
        (error 
               "No raise for this types"
               (type-tag x))
    )
  )
)

(define a (make-complex-from-real-imag 0 0))
(define b (make-complex-from-real-imag 24 5))
(define c (make-rational 0 3))
(define d (make-rational 1 3))
; (raise a)
; (raise b)
(display (raise d))(newline)
(display (raise c))(newline)
(display (raise 1))(newline)
(display (raise (raise 1)))(newline)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'install_transform_done
; (complex rectangular 1/3 . 0)
; (complex rectangular 0 . 0)
; (rational 1 . 1)
; (complex rectangular 1 . 0)
; > 