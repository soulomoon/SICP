;Exercise 5.36: What order of evaluation does our compiler produce for operands of a combination? Is it left-to-right, right-to-left, or some other order? Where in the compiler is this order determined? Modify the compiler so that it produces some other order of evaluation. (See the discussion of order of evaluation for the explicit-control evaluator in 5.4.1.) How does changing the order of operand evaluation affect the efficiency of the code that constructs the argument list?
;our compiler is right to left, evaluate the last first then the previous one, and cons the previous with it.and so on.
;construct-arglist and code-to-get-rest-args determined the order of it
;to modifies it we
;1 do not need to reverse it
;2 must use append rather than cons, which implement adjoin-arg.

;result:
;1 it is still tail-recursive
;2 since it use append instead of cons, i may be slower, but since it does not need reverse, compiler woule be slower
(load "/Users/soulomoon/git/SICP/material/allcode/ch5-compiler.scm")
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
(define (construct-arglist operand-codes)
    (if (null? operand-codes)
        (make-instruction-sequence
         '()
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-first-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val)
                 '(argl)
                 '((assign argl
                           (op list)
                           (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-first-arg
              (preserving
               '(env)
               code-to-get-first-arg
               (code-to-get-rest-args
                (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl (op adjoin-arg) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define a
  (compile
    '(begin
        (define (factorial-alt n)
          (if (= n 1)
              1
              (* (factorial-alt (- n 1)) n)))
      (factorial-alt 100))
    'val
    'next))
(define b
  (compile
    '(begin
      (define (factorial n)
        (define (iter product counter)
          (if (> counter n)
              product
              (iter (* counter product)
                    (+ counter 1))))
        (iter 1 1))
      (factorial 100))
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


(define eceval
  (make-machine
   '(env val proc argl continue)
   eceval-operations
   a
   ))
(start eceval)
((eceval 'stack) 'print-statistics)
(define eceval2
  (make-machine
   '(env val proc argl continue)
   eceval-operations
   b
   ))
(start eceval2)
((eceval2 'stack) 'print-statistics)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;{mcons 'COMPILER {mcons 'LOADED '()}}
;
;λ> 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000'done
;
;(total-pushes = 596 maximum-depth = 299)
;λ> 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000'done
;
;(total-pushes = 602 maximum-depth = 3)
;>
