(load "/home/soulomoon/Documents/git/SICP/Chapter2/source.scm")
; Exercise 2.84: Using the raise operation of Exercise 2.83, modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising, as discussed in this section. You will need to devise a way to test which of two types is higher in the tower. Do this in a manner that is “compatible” with the rest of the system and will not lead to problems in adding new levels to the tower.


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
  (define integer->rational
    (lambda (x) (make-rational (cdr x) 1))
  )
  (define rational->scheme-number
    (lambda (x) (div (numer x) (denom x)))
  )
  (define scheme-number->complex
    (lambda (x) (make-complex-from-real-imag x 0))
  )
  

  (define complex->scheme-number
    (lambda (x) (real-part x))
  )

  (put-coercion 'integer 'rational
                integer->rational)
  (put-coercion 'rational 'scheme-number
                rational->scheme-number)
  (put-coercion 'scheme-number 'complex
                scheme-number->complex)
  

  (put-coercion 'integer 'raise
                integer->rational)
  (put-coercion 'scheme-number 'raise
                scheme-number->complex)
  (put-coercion 'rational 'raise
                rational->scheme-number)

  (put-level 'integer 'level 1)
  (put-level 'rational 'level 2)
  (put-level 'scheme-number 'level 3)
  (put-level 'complex 'level 4)
                
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
; (define (drop x)

; )
(display (make-integer 1.1))(newline)

(define a (make-complex-from-real-imag 0 0))
(define b (make-complex-from-real-imag 24 5))
(define c (make-rational 0 3))
(define d (make-rational 1 3))
(define e (make-integer 3.5))
(define f (make-integer 4))

; (raise a)
; (raise b)
(display (raise d))(newline)
(display (raise (raise c)))(newline)
(display (raise 1))(newline)
(display (raise 1))(newline)
(display (add 1 a))(newline)
(display (add 1 e))(newline)
(display (add c e))(newline)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'install_transform_done
; (integer . 1.0)
; 1/3
; (complex rectangular 0 . 0)
; (complex rectangular 1 . 0)
; (complex rectangular 1 . 0)
; (complex rectangular 1 . 0)
; 5.0
; (rational 4.0 . 1.0)
; > 