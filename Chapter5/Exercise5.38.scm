;Exercise 5.38: Our compiler is clever about avoiding unnecessary stack operations, but it is not clever at all when it comes to compiling calls to the primitive procedures of the language in terms of the primitive operations supplied by the machine. For example, consider how much code is compiled to compute (+ a 1): The code sets up an argument list in argl, puts the primitive addition procedure (which it finds by looking up the symbol + in the environment) into proc, and tests whether the procedure is primitive or compound. The compiler always generates code to perform the test, as well as code for primitive and compound branches (only one of which will be executed). We have not shown the part of the controller that implements primitives, but we presume that these instructions make use of primitive arithmetic operations in the machine’s data paths. Consider how much less code would be generated if the compiler could open-code primitives—that is, if it could generate code to directly use these primitive machine operations. The expression (+ a 1) might be compiled into something as simple as328
;
;(assign val (op lookup-variable-value)
;            (const a)
;            (reg env))
;(assign val (op +)
;            (reg val)
;            (const 1))
;In this exercise we will extend our compiler to support open coding of selected primitives. Special-purpose code will be generated for calls to these primitive procedures instead of the general procedure-application code. In order to support this, we will augment our machine with special argument registers arg1 and arg2. The primitive arithmetic operations of the machine will take their inputs from arg1 and arg2. The results may be put into val, arg1, or arg2.
;
;The compiler must be able to recognize the application of an open-coded primitive in the source program. We will augment the dispatch in the compile procedure to recognize the names of these primitives in addition to the reserved words (the special forms) it currently recognizes.329 For each special form our compiler has a code generator. In this exercise we will construct a family of code generators for the open-coded primitives.
;
;The open-coded primitives, unlike the special forms, all need their operands evaluated. Write a code generator spread-arguments for use by all the open-coding code generators. Spread-arguments should take an operand list and compile the given operands targeted to successive argument registers. Note that an operand may contain a call to an open-coded primitive, so argument registers will have to be preserved during operand evaluation.
;For each of the primitive procedures =, *, -, and +, write a code generator that takes a combination with that operator, together with a target and a linkage descriptor, and produces code to spread the arguments into the registers and then perform the operation targeted to the given target with the given linkage. You need only handle expressions with two operands. Make compile dispatch to these code generators.
;Try your new compiler on the factorial example. Compare the resulting code with the result produced without open coding.
;Extend your code generators for + and * so that they can handle expressions with arbitrary numbers of operands. An expression with more than two operands will have to be compiled into a sequence of operations, each with only two inputs.
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
(load "/Users/soulomoon/git/SICP/material/allcode/ch5-compiler.scm")
;; 1 you do not need to preserve env too because although evaluation of arguments should not change the env, but compilation already-save that for us

;; 2 you need arg1 and arg2 and you change target here in compilation of  open-coded primitive application

;; 3 it actually getting a much smaller compiled code, it don't even need to save anthing

;;4 I would modifies the result on top of the previous one with marco
;(+ 1 1 1) to be (+ 1 (+ 1 (+ 1 (+))))

;;generator for register
(define (print a)
  (newline )(display a))

(define (make-register-name order)
  (string->symbol
    (string-append (symbol->string 'arg)
                   (number->string order))))
(define all-arg '(arg1 rag2))
;;;1
;;it would be arg1 arg2 to infinit..
(define (spread-arguments operands)
  (define (iter operands order)
    (if (null? operands)
        (empty-instruction-sequence)
        (let ((code-to-set-current-arg
                (compile (car operands)
                         (make-register-name order)
                          'next)))
             (preserving
               all-arg
               code-to-set-current-arg
               (iter (cdr operands) (+ order 1))))))
  (iter operands 1))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((open-coded-primitive? exp)
          (compile-open-primitive exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define open-coded-primitives-list
  '(= * - +))

(define (open-coded-primitive? exp)
  (if (pair? exp)
      (member (car exp) open-coded-primitives-list)
      false))
;; 2 you need arg1 and arg2 and you change target here in compilation of  open-coded primitive application
(define (compile-open-primitive exp target linkage)
  (print exp)
  (let ((primitive-operator (operator exp))
        (primitive-operands (spread-arguments (operands exp))))
        (end-with-linkage linkage
          (append-instruction-sequences
            primitive-operands
            (make-instruction-sequence '(arg1 arg2) (list target)
              `((assign ,target (op ,primitive-operator) (reg arg1) (reg arg2))))))))

(define (print-statements s)
  (for-each (lambda (x) (user-print x))
    (statements s)))

;; 3 it actually getting a much smaller compiled code, it don't even need to save anthing
;(print-statements
;  (compile
;    '(define (factorial-alt n)
;      (if (= n 1)
;          1
;          (* (factorial-alt (- n 1)) n)))
;
;    'val 'next))
;(newline )
;(define a
;  (compile
;    '(begin
;        (define (factorial-alt n)
;          (if (= n 1)
;              1
;              (* (factorial-alt (- n 1)) n)))
;
;        (factorial-alt 101)
;      ) 'val 'next))
;
;(set! a
;  (append
;    '((assign env (op get-global-environment)))
;  (statements a)))
;(set! a (append a '((perform (op user-print) (reg val)))))
;(set! eceval-operations
;  (append
;    eceval-operations
;    (list
;      (list 'make-compiled-procedure make-compiled-procedure)
;      (list 'compiled-procedure-env compiled-procedure-env)
;      (list 'compiled-procedure-entry compiled-procedure-entry)
;      (list 'list list)
;      (list 'cons cons)
;      (list '- -)
;      (list '= =)
;      (list 'false? false?)
;      (list '* *))))
;(define eceval
;  (make-machine
;   '(env val proc argl arg1 arg2 continue)
;   eceval-operations
;   a
;   ))
;(start eceval)
;((eceval 'stack) 'print-statistics)
;(start eceval)
;((eceval 'stack) 'print-statistics)

; 4 This is the syntax transferation for no more than two operands
(define (open-coded-trans exp)
  (let ((op (operator exp))
        (args (operands exp)))
    (define (iter arlist)
      (if (null? arlist)
          `(,op)
          (cons op (list (car arlist) (iter (cdr arlist))))))
    (iter args)))

;(display (open-coded-trans '(+ 1 1 1 1)))

;λ> (assign val (op make-compiled-procedure) (label entry1) (reg env))
;λ> (goto (label after-lambda2))
;λ> entry1
;λ> (assign env (op compiled-procedure-env) (reg proc))
;λ> (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;λ> (assign arg1 (op lookup-variable-value) (const n) (reg env))
;λ> (assign arg2 (const 1))
;λ> (assign val (op =) (reg arg1) (reg arg2))
;λ> (test (op false?) (reg val))
;λ> (branch (label false-branch4))
;λ> true-branch3
;λ> (assign val (const 1))
;λ> (goto (reg continue))
;λ> false-branch4
;λ> (save continue)
;λ> (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
;λ> (assign arg1 (op lookup-variable-value) (const n) (reg env))
;λ> (assign arg2 (const 1))
;λ> (assign val (op -) (reg arg1) (reg arg2))
;λ> (assign argl (op list) (reg val))
;λ> (test (op primitive-procedure?) (reg proc))
;λ> (branch (label primitive-branch6))
;λ> compiled-branch7
;λ> (assign continue (label proc-return9))
;λ> (assign val (op compiled-procedure-entry) (reg proc))
;λ> (goto (reg val))
;λ> proc-return9
;λ> (assign arg1 (reg val))
;λ> (goto (label after-call8))
;λ> primitive-branch6
;λ> (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
;λ> after-call8
;λ> (assign arg2 (op lookup-variable-value) (const n) (reg env))
;λ> (assign val (op *) (reg arg1) (reg arg2))
;λ> (restore continue)
;λ> (goto (reg continue))
;λ> after-if5
;λ> after-lambda2
;λ> (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
;λ> (assign val (const ok))
