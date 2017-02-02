; Exercise 4.52: Implement a new construct called if-fail that permits the user to catch the failure of an expression. If-fail takes two expressions. It evaluates the first expression as usual and returns as usual if the evaluation succeeds. If the evaluation fails, however, the value of the second expression is returned, as in the following example:

; ;;; Amb-Eval input:
; (if-fail 
;  (let ((x (an-element-of '(1 3 5))))
;    (require (even? x))
;    x)
;  'all-odd)

; ;;; Starting a new problem
; ;;; Amb-Eval value:
; all-odd

; ;;; Amb-Eval input:
; (if-fail
;  (let ((x (an-element-of '(1 3 5 8))))
;    (require (even? x))
;    x)
;  'all-odd)

; ;;; Starting a new problem
; ;;; Amb-Eval value:
; 8

(load "/Users/soulomoon/git/SICP/Chapter4/nlpparser.scm")

(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) 
                   (an-element-of (cdr items)))))
; using the if instead of writing my own, i am lazy.
(define (analyze-if-fail exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating
             succeed
             ;; failure continuation for
             (lambda () (cproc env succeed fail))))))
(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(interpret
 '(define (even? n)
    (= 0 (remainder n 2))))
(interpret 
 '(if-fail 
   (let ((x (an-element-of '(1 3 5))))
     (require (even? x))
     x)
   'all-odd))
(interpret 
 '(if-fail
   (let ((x (an-element-of '(1 3 5 8))))
     (require (even? x))
     x)
   'all-odd))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; 'ok

; 'ok

; 'all-odd

; 8
; > 