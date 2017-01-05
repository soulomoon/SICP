; Exercise 4.9: Many languages support a variety of iteration constructs, such as do, for, while, and until. In Scheme, iterative processes can be expressed in terms of ordinary procedure calls, so special iteration constructs provide no essential gain in computational power. On the other hand, such constructs are often convenient. Design some iteration constructs, give examples of their use, and show how to implement them as derived expressions.

(define (do-bindings exp)
  (cadr exp))
(define (do-var exp)
  (car (do-bindings exp)))
(define (do-init exp)
  (cadr (do-bindings exp)))
(define (do-step exp)
  (caddr (do-bindings exp)))


(define (do-clause exp)
  (caddr exp))
(define (do-clause-test exp)
  (car (do-test exp)))
(define (do-clause-squence exp)
  (cddr (do-test exp)))

()
(define (do-body exp)
  (cdddr exp))

(let
  

)

(let
  ((var init))
  (let iter
        ()
        (if test
            (begin 
              (set! var step)
              (sequence->exp body)
              iter
            (expression)
))))

(do ((var init )))


(define (eval-do exp)
  ())