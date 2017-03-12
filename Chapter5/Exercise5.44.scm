;Exercise 5.44: In this section we have focused on the use of the compile-time environment to produce lexical addresses. But there are other uses for compile-time environments. For instance, in Exercise 5.38 we increased the efficiency of compiled code by open-coding primitive procedures. Our implementation treated the names of open-coded procedures as reserved words. If a program were to rebind such a name, the mechanism described in Exercise 5.38 would still open-code it as a primitive, ignoring the new binding. For example, consider the procedure
;
;(lambda (+ * a b x y)
;  (+ (* a x) (* b y)))
;which computes a linear combination of x and y. We might call it with arguments +matrix, *matrix, and four matrices, but the open-coding compiler would still open-code the + and the * in (+ (* a x) (* b y)) as primitive + and *. Modify the open-coding compiler to consult the compile-time environment in order to compile the correct code for expressions involving the names of primitive procedures. (The code will work correctly as long as the program does not define or set! these names.)
;;the open-coded compiler which we need to reimplementing something with compile-time-environment
(load "/Users/soulomoon/git/SICP/Chapter5/Exercise5.38.scm")
;;compile-time environment added here
(load "/Users/soulomoon/git/SICP/Chapter5/Exercise5.43.scm")

;; we just need to implementing a detection in the process of  compile-open-primitive if the corresponding open-coded-primitive operator is in the ct-env
(define (compile exp target linkage ct-env)
  ;(print ct-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage ct-env))
        ((quoted? exp) (compile-quoted exp target linkage ct-env))
        ((variable? exp)
         (compile-variable exp target linkage ct-env))
        ((let? exp)
         (compile (let->combination exp) target linkage ct-env))
        ((assignment? exp)
         (compile-assignment exp target linkage ct-env))
        ((definition? exp)
         (compile-definition exp target linkage ct-env))
        ((if? exp) (compile-if exp target linkage ct-env))
        ((open-coded-primitive? exp)
          (compile-open-primitive exp target linkage ct-env))
        ((lambda? exp) (compile-lambda exp target linkage ct-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage ct-env))
        ((cond? exp) (compile (cond->if exp) target linkage ct-env))
        ((application? exp)
         (compile-application exp target linkage ct-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (spread-arguments operands ct-env)
  (define (iter operands order)
    (if (null? operands)
        (empty-instruction-sequence)
        (let ((code-to-set-current-arg
                (compile (car operands)
                         (make-register-name order)
                          'next
                          ct-env)))
             (preserving
               all-arg
               code-to-set-current-arg
               (iter (cdr operands) (+ order 1))))))
  (iter operands 1))

(define (compile-open-primitive exp target linkage ct-env)
  (let* ((primitive-operator (operator exp))
        (primitive-operands (spread-arguments (operands exp) ct-env))
        (lexical-position (find-variable primitive-operator ct-env)))

        (if (not (eq? lexical-position 'not-found))
            (error "reassignment of open-coded-primitive operator" primitive-operator))
            
        (end-with-linkage linkage
          (append-instruction-sequences
            primitive-operands
            (make-instruction-sequence '(arg1 arg2) (list target)
              `((assign ,target (op ,primitive-operator) (reg arg1) (reg arg2))))))))
;;;;;;;test;;;;;;;;;
(define a
  (compile
    '(begin
      (define (test)
        (define + (lambda (x y) (+ x y)))
        (+ 1 2))
      (test)
      )
      'val
      'next
      (empty-compile-time-env)))

(set! a
  (append
    '((assign env (op get-global-environment)))
  (statements a)))
(set! a (append a '((perform (op user-print) (reg val)))))

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
      (list '- -)
      (list '= =)
      (list '+ +)
      (list '* *)
      (list 'lexical-address-lookup lexical-address-lookup)
      )))

(define eceval
  (make-machine
   '(env val proc argl arg1 arg2 continue)
   eceval-operations
   a
   ))
(start eceval)
;((eceval 'stack) 'print-statistics)
;
;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;. . reassignment of open-coded-primitive operator +
;>
