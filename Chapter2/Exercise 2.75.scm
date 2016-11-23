; Exercise 2.75: Implement the constructor make-from-mag-ang in message-passing style. This procedure should be analogous to the make-from-real-imag procedure given above.

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part)
            (* x (cos y)))
          ((eq? op 'real-part)
            (* x (sin y)))
          (else
           (error "Unknown op: 
             make-from-mag-ang" op))))
  dispatch)