(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
;Exercise 5.25: Modify the evaluator so that it uses normal-order evaluation, based on the lazy evaluator of 4.2.

;that I need to steal somthing from 4.2
;in order to use STRUCTURE like a thunk, here is what we need

;constructor: delay-it
(define (delay-it exp env)
  (list 'thunk exp env))
;predict: thunk?, evaluated-thunk?
(define (thunk? obj)
  ;(user-print obj)
  ;(display (tagged-list? obj 'thunk))
  (tagged-list? obj 'thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
;getter
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;operator: (force-it,actual-value) would be implement in the EXPLICIT-CONTROL machine
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj)
                    (thunk-env obj))
      obj))
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value
             (first-operand exps)
             env)
            (list-of-arg-values
             (rest-operands exps)
             env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it
             (first-operand exps)
             env)
            (list-of-delayed-args
             (rest-operands exps)
             env))))
;;Now it is time to implement
;add in the new operations
(set! eceval-operations
  (append
    eceval-operations
    (list (list 'delay-it delay-it)
          (list 'thunk? thunk?)
          (list 'cons cons)
          (list 'display display)
          (list 'equal? equal?)
          (list 'list list)
          (list 'evaluated-thunk? evaluated-thunk?)
          (list 'thunk-exp thunk-exp)
          (list 'thunk-env thunk-env))))


;now the matchine controller
; first implement force-it
;we first need to implement it in print-result
(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(
;test
;(assign env (op get-global-environment))
;
;(assign unev (const (a)))
;(assign argl (const (12)))
;(assign env (op extend-environment)
;            (reg unev) (reg argl) (reg env))
;
;(assign exp (const (a)))
;(assign continue (label here))
;(goto (label list-of-delayed-args))
;
;here
;(assign unev (const (a)))
;(assign argl (reg val))
;(assign env (op extend-environment)
;            (reg unev) (reg argl) (reg env))
;
;(assign exp (const (a)))
;(assign continue (label end))
;(goto (label list-of-arg-values))

;;SECTION 5.4.4
read-eval-print-loop
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
;;**following instruction optional -- if use it, need monitored stack
  ;(perform (op print-stack-statistics))
  ;(perform
  ; (op announce-output) (const ";;; EC-Eval value:"))
  ;for lazy evaluation
  (test (op equal?) (reg val) (const ok))
  (branch (label end))
  (assign exp (reg val))
  (assign env (op get-global-environment))
  (assign continue (label print-result-after))
  (goto (label actual-value-pre))
print-result-after
  (perform (op user-print) (reg val))
  (goto (label end))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label end))

;getting back to where is it from
force-it-done
  (restore env)
  (restore proc)
  (restore argl)
  (restore unev)
  (restore continue)
  (goto (reg continue))


;;;force-it
force-it
  (test (op thunk?) (reg val))
  (branch (label force-it-thunk))
  (goto (label force-it-done))
force-it-thunk
  (assign env (op thunk-env) (reg val))
  (assign exp (op thunk-exp) (reg val))
  (goto (label actual-value))
actual-value-pre
  (save continue)
  (save unev)
  (save argl)
  (save proc)
  (save env)
;;;actual-value
actual-value
  (assign continue (label force-it))
  (assign val (reg exp))
  (test (op thunk?) (reg val))
  (branch (label force-it))
  (goto (label eval-dispatch))
;;;list-of-arg-values
list-of-arg-values
  (save unev)
  (save continue)
  (assign continue (label list-of-arg-values-done))
  (save continue)
list-of-arg-values-loop
  (test (op no-operands?) (reg exp))
  (branch (label list-of-arg-values-base))
  (save env)
  ;stash the rest
  (assign unev (op rest-operands) (reg exp))
  ;save exp for dispatch
  (assign exp (op first-operand) (reg exp))
  (save exp)
  ;set exp to be the rest and loop
  (assign exp (reg unev))

  ;prepare for after
  (assign continue (label list-of-arg-values-after))
  (save continue)
  (goto (label list-of-arg-values-loop))

list-of-arg-values-after
  (restore exp)
  (restore env)
  ;then it come back with value
  (save val)
  (assign continue (label list-of-arg-values-cons))
  (goto (label actual-value-pre))
  ;prepare for cons
list-of-arg-values-cons
  (assign exp (reg val))
  (restore val)
  (restore continue)
  (assign val (op cons) (reg exp) (reg val))
  (goto (reg continue))
;base
list-of-arg-values-base
;empty-list is ()
  (assign val (const ()))
  (restore continue)
  (goto (reg continue))
list-of-arg-values-done
  (restore continue)
  (restore unev)
  (goto (reg continue))

;;;list-of-delayed-args
list-of-delayed-args
  (save unev)
  (save continue)
  (assign continue (label list-of-delayed-args-done))
  (save continue)
list-of-delayed-args-loop
  (test (op no-operands?) (reg exp))
  (branch (label list-of-delayed-args-base))
  (save env)
  ;stash the rest
  (assign unev (op rest-operands) (reg exp))
  ;save exp for dispatch
  (assign exp (op first-operand) (reg exp))
  (save exp)
  ;set exp to be the rest and loop
  (assign exp (reg unev))

  ;prepare for after
  (assign continue (label list-of-delayed-args-after))
  (save continue)
  (goto (label list-of-delayed-args-loop))

list-of-delayed-args-after
  (restore exp)
  (restore env)
  ;then it come back with value
  (assign exp (op delay-it) (reg exp) (reg env))
  (restore continue)
  (assign val (op cons) (reg exp) (reg val))
  (goto (reg continue))
;base
list-of-delayed-args-base
;empty-list is ()
  (assign val (const ()))
  (restore continue)
  (goto (reg continue))
list-of-delayed-args-done
  (restore continue)
  (restore unev)
  (goto (reg continue))

;;SECTION 5.4.1
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  ;(perform (op user-print) (reg exp))
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  ;actual-value of the operator
  (assign continue (label apply-dispatch))
  (goto (label actual-value-pre))

;;;Modifing apply-dispatch todo
apply-dispatch
  (restore unev)
  (restore env)
  (assign proc (reg val))
  (assign argl (reg unev))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  ;assign argl
  (assign exp (reg argl))
  ;env is already there, but it is not needed afterwords
  (assign continue (label primitive-apply-after))
  ;save proc for later
  (save proc)
  (goto (label list-of-arg-values))
primitive-apply-after
  (restore proc)
  ;get a list of args from list-of-arg-values
  (assign argl (reg val))
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign exp (reg argl))

  ;(assign exp (op procedure-parameters) (reg proc))
  (assign continue (label compound-apply-after))
  (save proc)
  (goto (label list-of-delayed-args))

compound-apply-after

  (restore proc)
  ;get all para from list-of-delayed-args
  (assign argl (reg val))
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

;;;SECTION 5.4.2
ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;;;SECTION 5.4.3

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label actual-value-pre))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
end

   )))
(i
 '(
 (define (try a b) (if (= a 0) 1 b))
 (try 0 (/ 1 0))
 (begin
   (define count 0)
   (define (id x) (set! count (+ count 1)) x)
   (define w (id (id 10)))
   count
 )
 w
 count
 )

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;
;
;INTERPRETATION:
;λ> 1
;λ> 1
;λ> 10
;λ> 2
;>
 )
