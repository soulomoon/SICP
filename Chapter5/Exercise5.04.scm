;Exercise 5.4: Specify register machines that implement each of the following procedures. For each machine, write a controller instruction sequence and draw a diagram showing the data paths.
;
;Recursive exponentiation:
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;Iterative exponentiation:
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))

(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")

(define (print x)
  (newline )
  (display x)
  (newline ))

;Recursive exponentiation:
(define expt-machine
(make-machine
  '(b n val continue)
  (list (list '* *) (list '- -) (list 'print print) (list '= =))
  '(
    (assign continue (label expt-done))
  expt-begin
    (save continue)
    (test (op =) (reg n) (const 0))
    (branch (label expt-base))
    (assign n (op -) (reg n) (const 1))
    (assign continue (label expt-after))
    (goto (label expt-begin))

  expt-base
    (restore continue)
    (assign val (const 1))
    (goto (reg continue))

  expt-after
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))

  expt-done
    (perform (op print) (reg val))
)))
;Iterative exponentiation:
(define expt-machine2
  (make-machine
    '(b n c p continue)
    (list (list '* *) (list '- -) (list 'print print) (list '= =))
    '(
      (assign c (reg n))
      (assign p (const 1))
    expt-iter
      (test (op =) (reg c) (const 0))
      (branch (label expt-done))
      (assign c (op -) (reg c) (const 1))
      (assign p (op *) (reg b) (reg p))
      (goto (label expt-iter))
    expt-done
      (perform (op print) (reg p))
    )))


(set-register-contents! expt-machine 'b 5)
(set-register-contents! expt-machine 'n 2)
(start expt-machine)
(set-register-contents! expt-machine2 'b 5)
(set-register-contents! expt-machine2 'n 2)
(start expt-machine2)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;'done
;'done
;
;25
;'done
;'done
;'done
;
;25
;'done
;>
