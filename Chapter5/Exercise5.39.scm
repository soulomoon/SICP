;Exercise 5.39: Write a procedure lexical-address-lookup that implements the new lookup operation. It should take two arguments—a lexical address and a run-time environment—and return the value of the variable stored at the specified lexical address. Lexical-address-lookup should signal an error if the value of the variable is the symbol *unassigned*.331 Also write a procedure lexical-address-set! that implements the operation that changes the value of the variable at a specified lexical address.
(load "/Users/soulomoon/git/SICP/material/allcode/ch5-eceval-support.scm")

(define (list-set! lst pos value)
  (define (iter l n)
    (cond
      ((null? l) (error "list-set!: displacement number overflow: " pos))
      ((= 0 n) (set-car! l value))
      (else (iter (cdr l) (- n 1)))))
  (iter lst pos))

(define (get-frame n e)
  (cond
    ((eq? e the-empty-environment) (error "getframe: frame-number overflow"))
    ((= n 0) (first-frame e))
    (else (get-frame (- n 1) (enclosing-environment e)))))

(define (get-value n frame)
    (list-ref (frame-values frame) n))

(define (lexical-address-lookup l-address r-env)
  (let* ((frame-number (car l-address))
        (displacement-number (cdr l-address))
        (frame (get-frame frame-number r-env))
        (val (get-value displacement-number frame)))
      (if (equal? "*unassigned*" val)
          (error "lexical-address-lookup *unassigned*:" l-address)
          val)))

(define (lexical-address-set! l-address r-env value)
  (let* ((frame-number (car l-address))
        (displacement-number (cdr l-address))
        (frame (get-frame frame-number r-env)))
      (list-set! (frame-values frame) displacement-number value)))


;;;;;;;test;;;;;;;;;
(define avariables
  '(a b c d e))
(define avalues
  '(1 2 3 4 5))
(define initial-env
  (extend-environment avariables avalues the-empty-environment))

(define a avariables)
(set! avariables avalues)
(set! avalues a)

(define new-env
  (extend-environment avariables avalues initial-env))

(define (test n)
  (for-each
    (lambda (x) (display x) (newline ))
    (list
      (lexical-address-lookup (cons n n) new-env)
      (lexical-address-lookup (cons n 0) new-env)
      (lexical-address-lookup (cons 0 n) new-env)
      (lexical-address-lookup (cons 0 0) new-env))))
(define (test2 n)
  (for-each
    (lambda (x) (display x) (newline ))
    (list
      (lexical-address-set! (cons n n) new-env "new")
      (lexical-address-set! (cons n 0) new-env "new")
      (lexical-address-set! (cons 0 n) new-env "new")
      (lexical-address-set! (cons 0 0) new-env "new"))))

(test 1)
(test2 1)
(test 1)

(lexical-address-set! (cons 0 100) new-env "new")

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;2
;1
;b
;a
;#<void>
;#<void>
;#<void>
;#<void>
;new
;new
;new
;new
;. . list-set!: displacement number overflow:  100
;>
