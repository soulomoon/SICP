; Figure 3.10: Using (define W2 (make-withdraw 100)) to create a second object.
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds")))
(define W2 (make-withdraw 100))
