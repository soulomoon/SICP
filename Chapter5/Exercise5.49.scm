;Exercise 5.49: As an alternative to using the explicit-control evaluator’s read-eval-print loop, design a register machine that performs a read-compile-execute-print loop. That is, the machine should run a loop that reads an expression, compiles it, assembles and executes the resulting code, and prints the result. This is easy to run in our simulated setup, since we can arrange to call the procedures compile and assemble as “register-machine operations.”
;
;;;this is rather an easy job, I already implement the compile-and-go into interpreter primitives in 5.48
; I just need to use it every time I recieve expression, but this time I need to use it as one of the  eceval-operations, but I need to get expression from outside since if getting inside would evaluate it
(load "/Users/soulomoon/git/SICP/Chapter5/compile-and-interpret.scm")
(define (compile-and-run)
  (let* ((expression (get-register-contents eceval 'exp))
        (instructions
         (assemble (statements
                    (compile expression 'val 'return))
                   eceval)))
    (set-register-contents! eceval 'val instructions)
    instructions))
(set! eceval-operations
  (append
    eceval-operations
    (list
      (list 'compile-and-run compile-and-run)
      (list 'ev-print ev-print)
      (list 'eq? eq?)
      (list 'null? null?)
      (list 'car car)
      (list 'cdr cdr))))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	;;above from book -- here are some more
  (list 'display display)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ /)
	(list '> >)
	(list '< <)
  (list 'compile-and-run compile-and-run)
        ))
(define the-global-environment (setup-environment))

(define (read-compile-execute-print in-exp)
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'flag false)
    (set-register-contents! eceval 'extra in-exp)
    (start eceval))

(define eceval
  (make-machine
    ;;;;extra to hold the the ones about to be interpret
   '(exp env val proc argl continue unev extra extra-go
	 compapp			;*for compiled to call interpreted
	 )
   eceval-operations
  '(
;;SECTION 5.4.4, as modified in 5.5.7
;;*for compiled to call interpreted (from exercise 5.47)
read-eval-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input) (const ";;; ECompile-Eval input:"))
;;;;if there is nothing go to the end
  (test (op null?) (reg extra))
  (branch (label ev-end))
;;;;;eval the current
  (assign exp (op car) (reg extra))
  (perform (op ev-print) (reg exp))
  (assign extra (op cdr) (reg extra))

  (assign env (op get-global-environment))
  (assign continue (label print-result))

  (assign val (op compile-and-run))

  (goto (reg val))

print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))
ev-end
   )))

(read-compile-execute-print
'(
(define (factorial n)
   (if (= n 1)
       1
       (* (factorial (- n 1)) n)))
(factorial 10)
))

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;(EXPLICIT CONTROL EVALUATOR FOR COMPILER LOADED)
;
;
;;;; ECompile-Eval input:
;λ > (define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))
;
;(total-pushes = 0 maximum-depth = 0)
;;;; EC-Eval value:
;ok
;
;;;; ECompile-Eval input:
;λ > (factorial 10)
;
;(total-pushes = 56 maximum-depth = 29)
;;;; EC-Eval value:
;3628800
;
;;;; ECompile-Eval input:
;'done
;>
