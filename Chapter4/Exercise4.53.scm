; Exercise 4.53: With permanent-set! as described in Exercise 4.51 and if-fail as in Exercise 4.52, what will be the result of evaluating

; (let ((pairs '()))
;   (if-fail 
;    (let ((p (prime-sum-pair 
;              '(1 3 5 8) 
;              '(20 35 110))))
;      (permanent-set! pairs 
;                      (cons p pairs))
;      (amb))
;    pairs))

(load "/Users/soulomoon/git/SICP/Chapter4/Exercise4.51.scm")
; (load "/Users/soulomoon/git/SICP/Chapter4/Exercise4.52.scm")




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
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
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


(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) 
                   (an-element-of (cdr items)))))



(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items)))))

(interpret '(define (smallest-divisor n)
              (find-divisor n 2)))

(interpret '(define (find-divisor n test-divisor)
              (cond ((> (square test-divisor) n) n)
                    ((divides? test-divisor n) test-divisor)
                    (else (find-divisor n (+ test-divisor 1))))))

(interpret '(define (divides? a b)
              (= (remainder b a) 0)))

(interpret '(define (square x)
              (* x x)))

(interpret '(define (prime? n)
              (= n (smallest-divisor n))))

(interpret '(define (prime-sum-pair list1 list2)
              (let ((a (an-element-of list1))
                    (b (an-element-of list2)))
                (require (prime? (+ a b)))
                (list a b))))
(interpret
 '(display
   (let ((pairs '()))
     (if-fail 
      (let ((p (prime-sum-pair 
                '(1 3 5 8) 
                '(20 35 110))))
        (permanent-set! pairs 
                        (cons p pairs))
        (amb))
      pairs))))


;here you get all the prime-pairs since it loop over every pair, and fails 

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; ((8 35) (3 110) (3 20))
; > 