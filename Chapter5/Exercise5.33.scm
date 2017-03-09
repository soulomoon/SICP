;Exercise 5.33: Consider the following definition of a factorial procedure, which is slightly different from the one given above:
;
;(define (factorial-alt n)
;  (if (= n 1)
;      1
;      (* n (factorial-alt (- n 1)))))
;Compile this procedure and compare the resulting code with that produced for factorial. Explain any differences you find. Does either program execute more efficiently than the other?

;the only different is from (* n (factorial (- n 1)))
;  to (* (factorial (- n 1)) n)
;
;mostly the same, when compiling arglist see below,
;the different is that when construct-arglist is called, the last arg is a call to itself or just n,
;if the the last arg is n, since it would not chang env, so it would not save env here, but when it the last is another call, env would be saved
;but when code-to-get-last-arg is called, the saving and not saving is reversed,
;just in another register which is argl, since a call to factorial would modifies it and n won't
; so they have the same stack deep and operation.
(load "/Users/soulomoon/git/SICP/material/allcode/ch5-compiler.scm")
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
(define (print a)
  (newline )(display a))
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (count 0))
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
                ;(print (car (car insts)))
                (set! count (+ count 1))
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
              ((eq? message 'count) count)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
(define a
  (compile
    '(begin
        (define (factorial-alt n)
          (if (= n 1)
              1
              (* (factorial-alt (- n 1)) n)))
      (factorial-alt 10))
    'val
    'next))
(define b
  (compile
    '(begin
        (define (factorial n)
          (if (= n 1)
              1
              (* n (factorial (- n 1)))))
      (factorial 10))
    'val
    'next))
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
      )))
(set! a
  (append
    '((assign env (op get-global-environment)))
  (statements a)))
(set! a (append a '((perform (op user-print) (reg val)))))

(set! b
  (append
    '((assign env (op get-global-environment)))
  (statements b)))
(set! b (append b '((perform (op user-print) (reg val)))))

;(for-each (lambda (x) (print x)) a)
;(newline )
(define (user-print a)
  (if (not (equal? a 'ok))
    (begin
      (newline )
      (display "λ> ")
      (display )
      (display a))))

(define eceval
  (make-machine
   '(env val proc argl continue)
   eceval-operations
   a
   ))
(start eceval)
(eceval 'count)
((eceval 'stack) 'print-statistics)
(define eceval2
  (make-machine
   '(env val proc argl continue)
   eceval-operations
   b
   ))
(start eceval2)
(eceval2 'count)
((eceval2 'stack) 'print-statistics)


(define (construct-arglist operand-codes)
  (let ((operand-codes
         (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence
         '()
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val)
                 '(argl)
                 '((assign argl
                           (op list)
                           (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))
