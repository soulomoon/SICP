;Exercise 5.47: This section described how to modify the explicit-control evaluator so that interpreted code can call compiled procedures. Show how to modify the compiler so that compiled procedures can call not only primitive procedures and compiled procedures, but interpreted procedures as well. This requires modifying compile-procedure-call to handle the case of compound (interpreted) procedures. Be sure to handle all the same target and linkage combinations as in compile-proc-appl. To do the actual procedure application, the code needs to jump to the evaluator’s compound-apply entry point. This label cannot be directly referenced in object code (since the assembler requires that all labels referenced by the code it is assembling be defined there), so we will add a register called compapp to the evaluator machine to hold this entry point, and add an instruction to initialize it:
;
;  (assign compapp (label compound-apply))
;  ;; branches if flag is set:
;  (branch (label external-entry))
;read-eval-print-loop …
;To test your code, start by defining a procedure f that calls a procedure g. Use compile-and-go to compile the definition of f and start the evaluator. Now, typing at the evaluator, define g and try to call f.
(load "/Users/soulomoon/git/SICP/Chapter5/compile-and-interpret.scm")

;You have to save continue before go to the compound-apply so you can return to it. because compound-apply don't save continue for you

;first we need to add a test if it is a call to interpreted procedure
(define (compile-procedure-call
         target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next)
               after-call
               linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc)
        '()
        `(;;new branch here
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))
          (test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
     (parallel-instruction-sequences
       (append-instruction-sequences
        compiled-branch
        (compile-proc-appl
         target
         compiled-linkage))
       (parallel-instruction-sequences
         (append-instruction-sequences
           compound-branch
           (compound-proc-appl
            target
            compiled-linkage))
          (append-instruction-sequences
           primitive-branch
           (end-with-linkage
            linkage
            (make-instruction-sequence
             '(proc argl)
             (list target)
             `((assign
                ,target
                (op apply-primitive-procedure)
                (reg proc)
                (reg argl))))))))
         after-call))))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (save continue)
            (assign
             val
             (reg compapp))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return
                (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc)
            all-regs
            `((assign continue
                      (label ,proc-return))
              (save continue)
              (assign
               val
               (reg compapp))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '(
            (save continue)
            (assign val (reg compapp))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage,
                 target not val: COMPILE"
                target))))
(go-two
  '(define (f) (g))
  '((define (g) (+ 1 1))
    (f)))

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;(EXPLICIT CONTROL EVALUATOR FOR COMPILER LOADED)
;
;
;;;; EC-Eval input:
;λ > (define (g) (+ 1 1))
;
;
;;;; EC-Eval input:
;λ > (f)
;
;(total-pushes = 12 maximum-depth = 5)
;;;; EC-Eval value:
;2
;
;;;; EC-Eval input:
;'done
;>
