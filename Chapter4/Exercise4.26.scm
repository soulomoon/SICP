; Exercise 4.26: Ben Bitdiddle and Alyssa P. Hacker disagree over the importance of lazy evaluation for implementing things such as unless. Ben points out that it’s possible to implement unless in applicative order as a special form. Alyssa counters that, if one did that, unless would be merely syntax, not a procedure that could be used in conjunction with higher-order procedures. Fill in the details on both sides of the argument. Show how to implement unless as a derived expression (like cond or let), and give an example of a situation where it might be useful to have unless available as a procedure, rather than as a special form.

; as a special form, it unless could be normal order using if.
; if we were to eval it as procedure, we have to use delay to do it
; when you try to use it with map, you have to use it as procedure not special form
(load "/Users/soulomoon/git/SICP/Chapter4/separating.scm")
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) 
         (analyze-quoted exp))
        ((variable? exp) 
         (analyze-variable exp))
        ((unless? exp)
          (display (unless->if exp))(newline ) 
         (analyze (unless->if exp)))
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


(define (unless-conditione exp)
  (cadr exp))
(define (unless-normal exp)
  (caddr exp))
(define (unless-exception exp)
  (cadddr exp))

(define (unless->if exp)
  (make-if (unless-conditione exp)
           (unless-exception exp) 
           (unless-normal exp)))


(define (unless? exp)
  (tagged-list? exp 'unless))

(interpret 
'(unless (= 0 0)
         (/ 1 0)
         10
  )
)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (if (= 0 0) 10 (/ 1 0))
; 10
; > 


