; Exercise 4.22: Extend the evaluator in this section to support the special form let. (See Exercise 4.6.)
(load "/Users/soulomoon/git/SICP/Chapter4/separating.scm")
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) 
         (analyze-quoted exp))
        ((variable? exp) 
         (analyze-variable exp))
        ((let? exp)
          (display (let->combination exp))(newline ) 
         (analyze (let->combination exp)))
        ((assignment? exp) 
         (analyze-assignment exp))
        ((definition? exp) 
         (analyze-definition exp))
        ((if? exp) 
         (analyze-if exp))
        ((lambda? exp) 
         (analyze-lambda exp))
        ((begin? exp) 
         (analyze-sequence 
          (begin-actions exp)))
        ((cond? exp) 
         (analyze (cond->if exp)))
        ((application? exp) 
         (analyze-application exp))

        (else
         (error "Unknown expression 
                 type: ANALYZE" 
                exp))))

(define (let? exp)
  (tagged-list? exp 'let))


(let ((x 2)
      (y 3))
      (if x y))
(interpret
  '(let ((x 2)
        (y 3))
        (if x y)))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 3
; ((lambda (x y) (if x y)) 2 3)
; 3
; > 