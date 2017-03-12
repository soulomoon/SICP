;Exercise 5.42: Using find-variable from Exercise 5.41, rewrite compile-variable and compile-assignment to output lexical-address instructions. In cases where find-variable returns not-found (that is, where the variable is not in the compile-time environment), you should have the code generators use the evaluator operations, as before, to search for the binding. (The only place a variable that is not found at compile time can be is in the global environment, which is part of the run-time environment but is not part of the compile-time environment.332 Thus, if you wish, you may have the evaluator operations look directly in the global environment, which can be obtained with the operation (op get-global-environment), instead of having them search the whole run-time environment found in env.) Test the modified compiler on a few simple cases, such as the nested lambda combination at the beginning of this section.
(load "/Users/soulomoon/git/SICP/Chapter5/Exercise5.40.scm")
(load "/Users/soulomoon/git/SICP/Chapter5/Exercise5.41.scm")

(define (compile-variable exp target linkage ct-env)
  (let ((lexical-position (find-variable exp ct-env)))
    (if (eq? lexical-position 'not-found)
        (end-with-linkage linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))))
        (end-with-linkage linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,lexical-position)
                    (reg env))))))))

(define (compile-assignment exp target linkage ct-env)
  (let* ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ct-env))
        (lexical-position (find-variable exp ct-env)))
    (if (eq? lexical-position 'not-found)
        (end-with-linkage linkage
         (preserving '(env)
          get-value-code
          (make-instruction-sequence '(env val) (list target)
           `((perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok))))))
        (end-with-linkage linkage
         (preserving '(env)
          get-value-code
          (make-instruction-sequence '(env val) (list target)
           `((perform (op lexical-address-set!)
                      (const ,lexical-position)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))))))))
;;;;;test;;;;;;
;in order to show ct-env
;(define (compile-lambda-body exp proc-entry ct-env)
;  (let* ((formals (lambda-parameters exp))
;        (ct-env (extend-compile-time-environment formals ct-env)))
;        (print ct-env)
;    (append-instruction-sequences
;     (make-instruction-sequence '(env proc argl) '(env)
;      `(,proc-entry
;        (assign env (op compiled-procedure-env) (reg proc))
;        (assign env
;                (op extend-environment)
;                (const ,formals)
;                (reg argl)
;                (reg env))))
;     (compile-sequence (lambda-body exp) 'val 'return ct-env))))
;
;(define x
;'(((lambda (x y)
;   (lambda (a b c d e)
;     ((lambda (y z) (* x y z))
;      (* a b x)
;      (+ c d x))))
; 3
; 4) 1 1 1 1 1))
;
;(set! eceval-operations
;  (append
;    eceval-operations
;    (list
;      (list 'make-compiled-procedure make-compiled-procedure)
;      (list 'compiled-procedure-env compiled-procedure-env)
;      (list 'compiled-procedure-entry compiled-procedure-entry)
;      (list 'list list)
;      (list 'cons cons)
;      (list 'false? false?)
;      (list 'lexical-address-lookup lexical-address-lookup)
;      )))
;
;
;  (define a
;    (compile
;      x
;      'val
;      'next
;      (empty-compile-time-env)))
;      ;(print-statements a)
;  (set! a
;    (append
;      '((assign env (op get-global-environment)))
;    (statements a)))
;  (set! a (append a '((perform (op user-print) (reg val)))))
;
;  (newline )
;  (define eceval
;    (make-machine
;     '(env val proc argl continue)
;     eceval-operations
;     a
;     ))
;  (start eceval)
;  ((eceval 'stack) 'print-statistics)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;{mcons 'COMPILER {mcons 'LOADED {mcons 'for {mcons 'lexical-address '()}}}}
;((x y))
;((a b c d e) (x y))
;((y z) (a b c d e) (x y))
;
;
;λ> 45'done
;
;(total-pushes = 4 maximum-depth = 3)
;>
