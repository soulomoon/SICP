; Exercise 4.51: Implement a new kind of assignment called permanent-set! that is not undone upon failure. For example, we can choose two distinct elements from a list and count the number of trials required to make a successful choice as follows:

; (define count 0)
; (let ((x (an-element-of '(a b c)))
;       (y (an-element-of '(a b c))))
;   (permanent-set! count (+ count 1))
;   (require (not (eq? x y)))
;   (list x y count))

; ;;; Starting a new problem
; ;;; Amb-Eval value:
; (a b 2)

; ;;; Amb-Eval input:
; try-again

; ;;; Amb-Eval value:
; (a c 3)
; What values would have been displayed if we had used set! here rather than permanent-set!?
(load "/Users/soulomoon/git/SICP/Chapter4/nlpparser.scm")


(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
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

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)    ; *1*
               (let ((old-value
                      (lookup-variable-value 
                       var 
                       env)))
                 (set-variable-value!
                  var 
                  val 
                  env)
                 (succeed 
                  'ok
                  fail2)))
             fail))))
(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) 
                   (an-element-of (cdr items)))))
(interpret '(define count 0))
(interpret '(display (let ((x (an-element-of '(a b c)))
                    (y (an-element-of '(a b c))))
                (permanent-set! count (+ count 1))
                (require (not (eq? x y)))
(list x y count))))

(try-again)
(try-again)

(interpret '(define count2 0))
(interpret '(display (let ((x (an-element-of '(a b c)))
                    (y (an-element-of '(a b c))))
                (set! count2 (+ count2 1))
                (require (not (eq? x y)))
(list x y count2))))

(try-again)
(try-again)


; set! would undo the count

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; 'ok

; 'ok

; (a b 2)

; (a c 3)

; (b a 4)
; 'ok

; (a b 1)

; (a c 1)

; (b a 1)
; > 