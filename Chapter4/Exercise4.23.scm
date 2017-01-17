; Exercise 4.23: Alyssa P. Hacker doesn’t understand why analyze-sequence needs to be so complicated. All the other analysis procedures are straightforward transformations of the corresponding evaluation procedures (or eval clauses) in 4.1.1. She expected analyze-sequence to look like this:

; (define (analyze-sequence exps)
;   (define (execute-sequence procs env)
;     (cond ((null? (cdr procs)) 
;            ((car procs) env))
;           (else ((car procs) env)
;                 (execute-sequence 
;                  (cdr procs) env))))
;   (let ((procs (map analyze exps)))
;     (if (null? procs)
;         (error "Empty sequence: 
;                 ANALYZE"))
;     (lambda (env) 
;       (execute-sequence procs env))))
; Eva Lu Ator explains to Alyssa that the version in the text does more of the work of evaluating a sequence at analysis time. Alyssa’s sequence-execution procedure, rather than having the calls to the individual execution procedures built in, loops through the procedures in order to call them: In effect, although the individual expressions in the sequence have been analyzed, the sequence itself has not been.

one:
procs = ((+ 1 1))
  new: (lambda (env) (execute-sequence ((lambda (env) (+ 1 1))) env))
  old: (lambda (env) (+ 1 1))
two:
  ((+ 1 1) (- 2 2))
  new: (lambda (env) (execute-sequence ((lambda (env) (+ 1 1)) (lambda (env) (- 2 2))) env))
  old: (lambda (env) (((lambda (env) (+ 1 1)) env) ((lambda (env) (- 2 2)) env)))

the new version have to evaluating execute-sequence which use to unfold the begin list, where as the old one make it already a nested lambda.
