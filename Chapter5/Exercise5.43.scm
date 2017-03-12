;Exercise 5.43: We argued in 4.1.6 that internal definitions for block structure should not be considered “real” defines. Rather, a procedure body should be interpreted as if the internal variables being defined were installed as ordinary lambda variables initialized to their correct values using set!. 4.1.6 and Exercise 4.16 showed how to modify the metacircular interpreter to accomplish this by scanning out internal definitions. Modify the compiler to perform the same transformation before it compiles a procedure body.

;; in Exercise4.16, implementing this feature is done by modifying the make-procedure. Here we should finde the equivalent procedure,
;make-compiled-procedure is just a way to combine the entry with he environment,

;so we look at compile-lambda, the only places we need to make a procedure in compilation (when define need to make a procedure it just turn its body into lambda)

;here we can see compile-lambda-body is the actually place where is make procedures in machine code

;we did what we did in 4.16, do a scan-out-defines in the lambda-body, using the scan-out-defines in 4.16, which transformation the expression into a let expression which is actually a lambda expression with calls that take the definition as variable and then sign val to it

;using scan-out-defines, it would transform it into a let expression if there is definition, so here we need to make a little change that , we have to transform the let into a lambda, since it is not implementing in the compiler

(load "/Users/soulomoon/git/SICP/Chapter4/Exercise4.16.scm")
;(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
;(load "/Users/soulomoon/git/SICP/material/allcode/ch5-compiler.scm")
(load "/Users/soulomoon/git/SICP/Chapter5/Exercise5.42.scm")

(define (scan-out-defines body)
  ;(print "scan-out-defines is on")
  (define (notdefinition? exp) (not (definition? exp)))
  (define (filter l predict?)
    (let ((returns '()))
         (define (iter l)
            (if (null? l)
                returns
                (if (predict? (car l))
                    (begin
                      (set! returns (cons (car l) returns))                   (iter (cdr l)) )
                    (iter (cdr l)))))
          (iter l)))
  (define (make-the-let-body defines notdefines)
    (if defines
        (make-the-let-body
          (cdr defines)
          (cons
            (list 'set!
                  (definition-variable (car defines))
                  (definition-value (car defines)))
            notdefines))
        notdefines))
  (define (defines) (filter body definition?))
  (define (notdefines) (filter body notdefinition?))
  ;(display (defines))(newline )
  (if (defines)
      body
      (let->combination
        (make-let;new
          (map
            (lambda (exp)
                    (list (definition-variable exp) '*unassigned*))
            (defines))
            (make-the-let-body (defines) (notdefines))))))

(define (compile-lambda-body exp proc-entry ct-env)
  (let* ((formals (lambda-parameters exp))
        (ct-env (extend-compile-time-environment formals ct-env)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return ct-env))))


;(define x
;'(begin
;  (define (test)
;    (define b 2)
;    (define (c) 3)
;    (+ b (c)))
;(test)))
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
;      )))
;(define a
;  (compile
;    x
;    'val
;    'next
;    (empty-compile-time-env)
;    ))
;
;(set! a
;  (append
;    '((assign env (op get-global-environment)))
;  (statements a)))
;(set! a (append a '((perform (op user-print) (reg val)))))
;
;(define eceval
;    (make-machine
;   '(env val proc argl continue)
;   eceval-operations
;   a
;   ))
;(start eceval)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;{mcons 'COMPILER {mcons 'LOADED '()}}
;scan-out-defines is on
;scan-out-defines is on
;
;λ> 5'done
