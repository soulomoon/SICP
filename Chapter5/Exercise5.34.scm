;Exercise 5.34: Compile the iterative factorial procedure
;
;(define (factorial n)
;  (define (iter product counter)
;    (if (> counter n)
;        product
;        (iter (* counter product)
;              (+ counter 1))))
;  (iter 1 1))
;Annotate the resulting code, showing the essential difference between the code for iterative and recursive versions of factorial that makes one process build up stack space and the other run in constant stack space.
(load "/Users/soulomoon/git/SICP/material/allcode/ch5-compiler.scm")
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")


;for recursive construct-arglist would save env, or code-to-get-rest-args would save argl every it called itself, but for iterative, since there are just these called it self, would release those stack in the begging of every call.
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
