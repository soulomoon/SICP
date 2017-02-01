; Exercise 4.50: Implement a new special form ramb that is like amb except that it searches alternatives in a random order, rather than from left to right. Show how this can help with Alyssa’s problem in Exercise 4.49.
(load "/Users/soulomoon/git/SICP/Chapter4/zch4-ambeval.scm")
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))
(define (analyze-ramb exp)
  (let ((cprocs
         (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (cond 
          ((null? choices) (fail))
          ((null? (cdr choices))
           ((cadr choices)
            env
            succeed
            (lambda ()
              (try-next (append (car choices) (cddr choices))))))
          ((= 1 (random 2)) 
           ((cadr choices) 
            env
            succeed
            (lambda ()
              (try-next (append (car choices) (cddr choices))))))
          (else 
           ((car choices) 
            env
            succeed
            (lambda ()
              (try-next (cdr choices)))))))
      (try-next cprocs))))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((ramb? exp) (analyze-ramb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(interpret '(amb 1 2))
(interpret '(amb 1 2))
(interpret '(amb 1 2))
(interpret '(amb 1 2))
(interpret '(amb 1 2))
(interpret '(amb 1 2))
(interpret '(amb 1 2))
(newline )(display 'random)(newline )
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))
(interpret '(ramb 1 2))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'AMB-EVALUATOR-LOADED
; 1
; 1
; 1
; 1
; 1
; 1
; 1

; random
; 1
; 2
; 1
; 2
; 2
; 1
; 1
; 1
; 2
; > 