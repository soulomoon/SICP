;Exercise 5.42: Using find-variable from Exercise 5.41, rewrite compile-variable and compile-assignment to output lexical-address instructions. In cases where find-variable returns not-found (that is, where the variable is not in the compile-time environment), you should have the code generators use the evaluator operations, as before, to search for the binding. (The only place a variable that is not found at compile time can be is in the global environment, which is part of the run-time environment but is not part of the compile-time environment.332 Thus, if you wish, you may have the evaluator operations look directly in the global environment, which can be obtained with the operation (op get-global-environment), instead of having them search the whole run-time environment found in env.) Test the modified compiler on a few simple cases, such as the nested lambda combination at the beginning of this section.
(load "/Users/soulomoon/git/SICP/Chapter5/Exercise5.40.scm")
(load "/Users/soulomoon/git/SICP/Chapter5/Exercise5.41.scm")
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ;(user-print (caar insts))
                ;(newline )
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

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

(define x
'(((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) (* x y z))
      (* a b x)
      (+ c d x))))
 3
 4) 1 1 1 1 1)


 )

(set! eceval-operations
  (append
    eceval-operations
    (list
      (list 'make-compiled-procedure make-compiled-procedure)
      (list 'compiled-procedure-env compiled-procedure-env)
      (list 'compiled-procedure-entry compiled-procedure-entry)
      (list 'list list)
      (list 'cons cons)
      (list 'false? false?)
      (list 'lexical-address-lookup lexical-address-lookup)
      )))


  (define a
    (compile
      x
      'val
      'next
      (empty-compile-time-env)))
      ;(print-statements a)
  (set! a
    (append
      '((assign env (op get-global-environment)))
    (statements a)))
  (set! a (append a '((perform (op user-print) (reg val)))))
  (newline )
  (newline )
  (newline )
  (newline )
  (define eceval
    (make-machine
     '(env val proc argl continue)
     eceval-operations
     a
     ))
  (start eceval)
  ((eceval 'stack) 'print-statistics)

;  Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;{mcons 'COMPILER {mcons 'LOADED {mcons 'for {mcons 'lexical-address '()}}}}
;λ> 45'done
;
;(total-pushes = 4 maximum-depth = 3)
;>
