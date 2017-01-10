; Exercise 4.9: Many languages support a variety of iteration constructs, such as do, for, while, and until. In Scheme, iterative processes can be expressed in terms of ordinary procedure calls, so special iteration constructs provide no essential gain in computational power. On the other hand, such constructs are often convenient. Design some iteration constructs, give examples of their use, and show how to implement them as derived expressions.
(load "/home/soulomoon/git/SICP/Chapter4/Exercise4.08.scm")


(define (do-bindings exp)
  (cadr exp))
(define (do-vars exp)
  (map car (do-bindings exp)))
(define (do-inits exp)
  (map cadr (do-bindings exp)))
(define (do-steps exp)
  (map caddr (do-bindings exp)))

(define (do-clause exp)
  (caddr exp))
(define (do-clause-test exp)
  (car (do-clause exp)))
(define (do-clause-expressions exp)
  (cdr (do-clause exp)))
(define (do-commands exp)
  (cdddr exp))

(define (make-assignment var arg)
  (list 'set! var arg))

(define (make-let-long var pairs . body)
(cons 'let (cons var (cons pairs (sequence->exp body)))))

(define (make-let-whole . args)
  ; (display args)
  (if (symbol? (car args))
    (make-let-long (car args) (cadr args) (cddr args))
    (make-let (car args) (cddr args))))

(define (do=>let exp)
  (let ((vars (do-vars exp))
        (inits (do-inits exp))
        (steps (do-steps exp))
        (expressions (do-clause-expressions exp))
        (commands (do-commands exp)))
        (let ((pairs (map list vars inits)))
              (make-let-whole
                'iter
                pairs
                (make-if
                  (do-clause-test exp)
                  (sequence->exp expressions)
                  (sequence->exp
                    (list 
                      (sequence->exp commands)
                      (cons 'iter steps))))))))

; (do ((<variable1> <init1> <step1>)‌‌syntax 
; ...)
; (<test> <expression> ...)
; <command> ...)

(define (eval-do exp env)
(display (do=>let exp))(newline )
  (eval# (do=>let exp) env))

(define x
      '(do ((i 0 (+ i 1))
            (y 0 (+ y 2)))
        ((> (+ i y) 50) i)
        (+ 1 i)))

(put-syntax! 'do eval-do)

(interpret x)

;   (let iter
;         (pairs)
;         (if test
;             (sequence->exp expressions)
;             (sequence->exp
;               (list
;                 (sequence->exp commands)
;                 (iter steps))
;             ))))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; 'ok
; (let iter ((i 0) (y 0)) (if (> (+ i y) 50) i (begin (+ 1 i) (iter (+ i 1) (+ y 2)))))
; 17
; > 