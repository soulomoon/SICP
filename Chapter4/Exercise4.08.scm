; Exercise 4.8: “Named let” is a variant of let that has the form

; (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
; The ⟨bindings⟩ and ⟨body⟩ are just as in ordinary let, except that ⟨var⟩ is bound within ⟨body⟩ to a procedure whose body is ⟨body⟩ and whose parameters are the variables in the ⟨bindings⟩. Thus, one can repeatedly execute the ⟨body⟩ by invoking the procedure named ⟨var⟩. For example, the iterative Fibonacci procedure (1.2.2) can be rewritten using named let as follows:

; (define (fib n)
;   (let fib-iter ((a 1) (b 0) (count n))
;     (if (= count 0)
;         b
;         (fib-iter (+ a b) 
;                   a 
;                   (- count 1)))))
; Modify let->combination of Exercise 4.6 to also support named let.

(load "/home/soulomoon/git/SICP/Chapter4/ex4.03.rkt")

(define (let-empty-pairs? exp)
  ; (display (cadr exp))
  (null? (cadr exp)))
  
(define (let-body exp)
  (cddr exp))
(define (let-pa-pairs exp)
  (cadr exp))
(define (let-parameters exp)
  (map car (let-pa-pairs exp)))
(define (let-arguments exp)
  (map cadr (let-pa-pairs exp)))
(define (let-name? exp)
  (symbol? (cadr exp)))

(define (let-name=>nor exp)
(apply make-let (cons (caddr exp) (cdddr exp))))
  
(define (let->combination exp)
  ; (display (make-lambda (let-parameters exp) (let-body exp)))
  (if (let-name? exp)
    (let ((normal-exp (let-name=>nor exp)))
      (sequence->exp
        (list 
          (make-define (cadr exp)
                      (make-lambda 
                        (let-parameters normal-exp) 
                          (let-body normal-exp)))
          (let->combination normal-exp))))
    (if (let-empty-pairs? exp)
      (sequence->exp (let-body exp))
      (cons (make-lambda (let-parameters exp) (let-body exp))
            (let-arguments exp)))))


(define (eval-let exp env)
; (display (let->combination exp))(newline)
; (display (let->combination exp))(newline )
  (eval# (let->combination exp) env))




(define (make-let pairs . body)
  (cons 'let (cons pairs body)))

(define (make-define name . body)
  (cons 'define (cons name body)))
(put-syntax! 'let eval-let)

; (interpret '(let ((i 0)) (begin (define iter (lambda () (if (= i 5) i (begin (set! i (+ i 1)) (+ 1 i) (iter))))) (if (= i 5) i (begin (set! i (+ i 1)) (+ 1 i) (iter))))))

; (interpret (make-let '((x 2) (y 3)) '(if 1 1) '(if x y)))

; (interpret
;       '(let ((n 11)) 
;         (let fib-iter ((a 1) (b 0) (count n))
;           (if (= count 0)
;               b
;               (fib-iter (+ a b) 
;                         a 
; (- count 1))))))

; (interpret
;   '(begin
;     (define (fib n)
;       (let fib-iter ((a 1) (b 0) (count n))
;         (if (= count 0)
;             b
;             (fib-iter (+ a b) 
;                       a 
;                       (- count 1)))))
;     (fib 10)))
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; 3
; 89
; 55
; > 