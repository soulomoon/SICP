; hack to evaluat one expression by in, or i to evaluat multible expressions

;;;; LOADS THE EXPLICIT-CONTROL EVALUATOR FROM SECTION 5.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS, WITH
;;;; ALL THE SUPPORTING CODE IT NEEDS IN ORDER TO RUN.

;;;; **NB** The actual "load" calls are implementation dependent.

(load "/Users/soulomoon/git/SICP/material/allcode/ch5-regsim.scm")			;reg machine simulator

;; **NB** next file contains another "load"
(load "/Users/soulomoon/git/SICP/material/allcode/ch5-eceval-support.scm")		;simulation of machine operations

(load "/Users/soulomoon/git/SICP/material/allcode/ch5-eceval.scm")			;eceval itself

;eval machine is:
;eceval
(define (print x) (display x) (newline ))

; the register-name would be:
;(exp env val proc argl continue unev)
(define (in exp)
  (set-register-contents! eceval 'exp exp)
  (start eceval)
  )
(define (i exps)
  (newline )
  (newline )
  (display "INTERPRETATION:")
  (for-each (lambda (exp) (in exp)) exps))
;(in '(+ 1 1))
;(i '(
;  (define a 1)
;  (+ a 1)
;  ))

;(define (i . exp)
;  (for-each (lambda ) i)
;)
