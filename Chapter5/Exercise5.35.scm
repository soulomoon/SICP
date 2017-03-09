;Exercise 5.35: What expression was compiled to produce the code shown in Figure 5.18?
(load "/Users/soulomoon/git/SICP/material/allcode/ch5-compiler.scm")
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
(define a
  (compile
    '(define f (lambda (x) (+ x (g (+ x 2)))))
    'val
    'next))
(set! a (statements a))
(for-each (lambda (x) (user-print x)) a)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;{mcons 'COMPILER {mcons 'LOADED '()}}
;
;λ> (assign val (op make-compiled-procedure) (label entry1) (reg env))
;λ> (goto (label after-lambda2))
;λ> entry1
;λ> (assign env (op compiled-procedure-env) (reg proc))
;λ> (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
;λ> (assign proc (op lookup-variable-value) (const +) (reg env))
;λ> (save continue)
;λ> (save proc)
;λ> (save env)
;λ> (assign proc (op lookup-variable-value) (const g) (reg env))
;λ> (save proc)
;λ> (assign proc (op lookup-variable-value) (const +) (reg env))
;λ> (assign val (const 2))
;λ> (assign argl (op list) (reg val))
;λ> (assign val (op lookup-variable-value) (const x) (reg env))
;λ> (assign argl (op cons) (reg val) (reg argl))
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch3))
;λ> compiled-branch4
;λ> (assign continue (label after-call5))
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> primitive-branch3
;λ> (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> after-call5
;λ> (assign argl (op list) (reg val))
;λ> (restore proc)
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch6))
;λ> compiled-branch7
;λ> (assign continue (label after-call8))
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> primitive-branch6
;λ> (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> after-call8
;λ> (assign argl (op list) (reg val))
;λ> (restore env)
;λ> (assign val (op lookup-variable-value) (const x) (reg env))
;λ> (assign argl (op cons) (reg val) (reg argl))
;λ> (restore proc)
;λ> (restore continue)
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch9))
;λ> compiled-branch10
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> primitive-branch9
;λ> (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> (goto (reg continue))
;λ> after-call11
;λ> after-lambda2
;λ> (perform (op define-variable!) (const f) (reg val) (reg env))
;λ> (assign val (const ok))
;>
