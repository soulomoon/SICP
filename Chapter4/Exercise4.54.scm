; Exercise 4.54: If we had not realized that require could be implemented as an ordinary procedure that uses amb, to be defined by the user as part of a nondeterministic program, we would have had to implement it as a special form. This would require syntax procedures

; (define (require? exp) 
;   (tagged-list? exp 'require))

; (define (require-predicate exp) 
;   (cadr exp))
; and a new clause in the dispatch in analyze

; ((require? exp) (analyze-require exp))
; as well the procedure analyze-require that handles require expressions. Complete the following definition of analyze-require.

; (define (analyze-require exp)
;   (let ((pproc (analyze 
;                 (require-predicate exp))))
;     (lambda (env succeed fail)
;       (pproc env
;              (lambda (pred-value fail2)
;                (if ⟨??⟩
;                    ⟨??⟩
;                    (succeed 'ok fail2)))
;              fail))))
(load "/Users/soulomoon/git/SICP/Chapter4/zch4-ambeval.scm")
(define (make-interpret)
  (let ((try false))
        (define (interpret-rm input)
          (newline )
          (if (eq? input 'try-again)
              (try)
          (ambeval 
           input
           the-global-environment
           ;; ambeval success
           (lambda (val next-alternative)
             (set! try next-alternative)
             val)
           ;; ambeval failure
           (lambda ()
             (newline)
             (display "fail")
             (newline))))
          )
        interpret-rm))
(define interpret (make-interpret))

(define (try-again) (newline )(interpret 'try-again))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((require? exp) (analyze-require exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))



(define (require? exp) 
  (tagged-list? exp 'require))

(define (require-predicate exp) 
  (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze 
                (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

(interpret 
'(begin

(define (square x) (* x x))
(define (require p)
  (if (not p) (amb)))
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher 
                      miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require 
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
(display (multiple-dwelling))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'AMB-EVALUATOR-LOADED

; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
; > 



)

)