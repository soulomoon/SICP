;Exercise 5.37: One way to understand the compiler’s preserving mechanism for optimizing stack usage is to see what extra operations would be generated if we did not use this idea. Modify preserving so that it always generates the save and restore operations. Compile some simple expressions and identify the unnecessary stack operations that are generated. Compare the code to that generated with the preserving mechanism intact.

;; it is easy to change it to do it very time it preserve
; look at the compilation of begin with two (+ 1 1)

(load "/Users/soulomoon/git/SICP/material/allcode/ch5-compiler.scm")
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")


(define a
  '(begin
    (+ 1 1)
    (+ 1 1)))
(define b (compile a 'val 'next))

(for-each (lambda (x) (user-print x)) (statements b))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2))))
(define a
  '(begin
    (+ 1 1)
    (+ 1 1)))
(define b (compile a 'val 'next))
(newline )
(for-each (lambda (x) (user-print x)) (statements b))


;λ> (save env)
;λ> (assign proc (op lookup-variable-value) (const +) (reg env))
;λ> (assign val (const 1))
;λ> (assign argl (op list) (reg val))
;λ> (assign val (const 1))
;λ> (assign argl (op cons) (reg val) (reg argl))
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch1))
;λ> compiled-branch2
;λ> (assign continue (label after-call3))
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> primitive-branch1
;λ> (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> after-call3
;λ> (restore env)
;λ> (assign proc (op lookup-variable-value) (const +) (reg env))
;λ> (assign val (const 1))
;λ> (assign argl (op list) (reg val))
;λ> (assign val (const 1))
;λ> (assign argl (op cons) (reg val) (reg argl))
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch4))
;λ> compiled-branch5
;λ> (assign continue (label after-call6))
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> primitive-branch4
;λ> (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> after-call6
;
;λ> (save continue)
;λ> (save env)
;λ> (save continue)
;λ> (save env)
;λ> (save continue)
;λ> (assign proc (op lookup-variable-value) (const +) (reg env))
;λ> (restore continue)
;λ> (restore env)
;λ> (restore continue)
;λ> (save continue)
;λ> (save proc)
;λ> (save env)
;λ> (save continue)
;λ> (assign val (const 1))
;λ> (restore continue)
;λ> (assign argl (op list) (reg val))
;λ> (restore env)
;λ> (save argl)
;λ> (save continue)
;λ> (assign val (const 1))
;λ> (restore continue)
;λ> (restore argl)
;λ> (assign argl (op cons) (reg val) (reg argl))
;λ> (restore proc)
;λ> (restore continue)
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch7))
;λ> compiled-branch8
;λ> (assign continue (label after-call9))
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> primitive-branch7
;λ> (save continue)
;λ> (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> (restore continue)
;λ> after-call9
;λ> (restore env)
;λ> (restore continue)
;λ> (save continue)
;λ> (save env)
;λ> (save continue)
;λ> (assign proc (op lookup-variable-value) (const +) (reg env))
;λ> (restore continue)
;λ> (restore env)
;λ> (restore continue)
;λ> (save continue)
;λ> (save proc)
;λ> (save env)
;λ> (save continue)
;λ> (assign val (const 1))
;λ> (restore continue)
;λ> (assign argl (op list) (reg val))
;λ> (restore env)
;λ> (save argl)
;λ> (save continue)
;λ> (assign val (const 1))
;λ> (restore continue)
;λ> (restore argl)
;λ> (assign argl (op cons) (reg val) (reg argl))
;λ> (restore proc)
;λ> (restore continue)
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch10))
;λ> compiled-branch11
;λ> (assign continue (label after-call12))
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> primitive-branch10
;λ> (save continue)
;λ> (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> (restore continue)
;λ> after-call12
;>
